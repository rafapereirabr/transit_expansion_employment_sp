# Adapted from https://github.com/baarthur/spatialops/blob/main/R/dist_nearest.R

#' Distance to the nearest feature with DuckDB Spatial
#'
#' For each feature in `x`, calculate the distance to the nearest feature in `y`. Pairwise
#' distances and nearest-feature selection are computed inside DuckDB.
#'
#' @param x,y Two `sf` objects, or two table names registered in `conn`.
#' @param name Optional name for the distance column, as a bare name or string. Defaults to
#'   `"distance"`.
#' @param id_feature Should an `id_closest` column identifying the nearest feature in `y` be
#'   included?
#' @param id_y Optional column in `y`, as a bare name or string, whose values should populate
#'   `id_closest`. When omitted, `id_closest` contains the one-based row index of `y`.
#' @param geometry Optional geometry-column name, as a bare name or string. By default, the active
#'   geometry column of `x` is used.
#' @param drop_geometry Should the geometry column be removed from the result?
#' @param drop_units For `sf` input, should units be removed from the distance column? Lazy
#'   DuckDB results always store distance as a numeric column.
#' @param conn Optional DuckDB connection created by `duckspatial::ddbs_create_conn()`. A
#'   connection is required when `x` and `y` are table names. For `sf` inputs, the function
#'   creates and closes an in-memory connection when `conn` is omitted.
#' @param dist_type Distance implementation passed to `duckspatial::ddbs_distance()`: `NULL`
#'   (automatic), `"planar"`, `"geos"`, `"haversine"`, or `"spheroid"`.
#' @param max_distance Optional maximum distance. When supplied, only features in `x` whose
#'   nearest feature in `y` is within this cutoff are returned. Units are metres for
#'   `"haversine"` and `"spheroid"`, and CRS units for planar distances.
#' @param candidate_crs Projected CRS used only to generate candidates efficiently when
#'   `max_distance` is supplied with a geographic distance type. For Sao Paulo, use EPSG:31983.
#' @param candidate_margin Multiplicative safety margin applied to `max_distance` during
#'   projected candidate generation. The exact distance cutoff is applied afterwards.
#'
#' @details
#' DuckDB Spatial does not currently provide an exact equivalent of
#' `sf::st_nearest_feature()`. This function computes all `n_x * n_y` distances in DuckDB and
#' ranks them there, so it is best suited to cases where `y` is relatively small. Ties are
#' resolved by the original row order of `y`, independently of `id_y`.
#'
#' Use `dist_type = "spheroid"` for ellipsoidal distances between WGS84 points. Geographic
#' distances are returned in metres; planar distances use the coordinate reference system units.
#'
#' With table-name inputs, the returned object remains lazy and backed by a temporary DuckDB table.
#' Use `duckspatial::ddbs_collect()` to materialize a result that retains geometry, or
#' `dplyr::collect()` when `drop_geometry = TRUE`.
#'
#' When `max_distance` is supplied, candidate generation uses `ST_DWithin()`. Features in `x`
#' without any feature in `y` inside the exact cutoff are omitted from the result.
#'
#' @returns For `sf` inputs, the original `x` with the requested columns. For table-name inputs,
#'   a lazy `duckspatial_df`, or a lazy `tbl_duckdb_connection` when geometry is dropped.
#'
#' @export
dist_nearest <- function(
	x,
	y,
	name,
	id_feature = FALSE,
	id_y = NULL,
	geometry = NULL,
	drop_geometry = FALSE,
	drop_units = TRUE,
	conn = NULL,
	dist_type = NULL,
	max_distance = NULL,
	candidate_crs = NULL,
	candidate_margin = 1.1
) {
	x_is_sf <- inherits(x, "sf")
	y_is_sf <- inherits(y, "sf")
	x_is_table <- is.character(x) && length(x) == 1L && !is.na(x)
	y_is_table <- is.character(y) && length(y) == 1L && !is.na(y)
	sf_mode <- x_is_sf && y_is_sf
	table_mode <- x_is_table && y_is_table

	if (!sf_mode && !table_mode) {
		stop(
			"`x` and `y` must both be sf objects or both be table names in `conn`.",
			call. = FALSE
		)
	}
	if (table_mode && is.null(conn)) {
		stop("`conn` is required when `x` and `y` are table names.", call. = FALSE)
	}
	if (!is.logical(id_feature) || length(id_feature) != 1L || is.na(id_feature)) {
		stop("`id_feature` must be TRUE or FALSE.", call. = FALSE)
	}
	if (!is.logical(drop_geometry) || length(drop_geometry) != 1L || is.na(drop_geometry)) {
		stop("`drop_geometry` must be TRUE or FALSE.", call. = FALSE)
	}
	if (!is.logical(drop_units) || length(drop_units) != 1L || is.na(drop_units)) {
		stop("`drop_units` must be TRUE or FALSE.", call. = FALSE)
	}
	if (
		!is.null(max_distance) &&
			(!is.numeric(max_distance) ||
				length(max_distance) != 1L ||
				is.na(max_distance) ||
				!is.finite(max_distance) ||
				max_distance < 0)
	) {
		stop("`max_distance` must be NULL or one non-negative finite number.", call. = FALSE)
	}
	if (
		!is.numeric(candidate_margin) ||
			length(candidate_margin) != 1L ||
			is.na(candidate_margin) ||
			!is.finite(candidate_margin) ||
			candidate_margin < 1
	) {
		stop("`candidate_margin` must be one finite number greater than or equal to 1.", call. = FALSE)
	}
	if (!is.null(max_distance) && is.null(dist_type)) {
		stop("`dist_type` must be explicit when `max_distance` is supplied.", call. = FALSE)
	}
	if (
		!is.null(max_distance) &&
			dist_type %in% c("haversine", "spheroid") &&
			is.null(candidate_crs)
	) {
		stop(
			"`candidate_crs` is required for geographic distance filtering.",
			call. = FALSE
		)
	}

	distance_name <- if (missing(name)) {
		"distance"
	} else {
		rlang::as_name(rlang::ensym(name))
	}
	id_y_name <- if (missing(id_y) || rlang::quo_is_null(rlang::enquo(id_y))) {
		NULL
	} else {
		rlang::as_name(rlang::ensym(id_y))
	}

	owns_connection <- is.null(conn)
	if (owns_connection) {
		conn <- duckspatial::ddbs_create_conn()
		on.exit(duckspatial::ddbs_stop_conn(conn), add = TRUE)
	}
	if (!DBI::dbIsValid(conn)) {
		stop("`conn` must be a valid DuckDB connection.", call. = FALSE)
	}

	if (sf_mode) {
		return(dist_nearest_sf(
			x = x,
			y = y,
			distance_name = distance_name,
			id_feature = id_feature,
			id_y_name = id_y_name,
			geometry = rlang::enquo(geometry),
			drop_geometry = drop_geometry,
			drop_units = drop_units,
			conn = conn,
			dist_type = dist_type,
			max_distance = max_distance,
			candidate_crs = candidate_crs,
			candidate_margin = candidate_margin
		))
	}

	dist_nearest_table(
		x = x,
		y = y,
		distance_name = distance_name,
		id_feature = id_feature,
		id_y_name = id_y_name,
		geometry = rlang::enquo(geometry),
		drop_geometry = drop_geometry,
		conn = conn,
		dist_type = dist_type,
		max_distance = max_distance,
		candidate_crs = candidate_crs,
		candidate_margin = candidate_margin
	)
}

dist_nearest_sf <- function(
	x,
	y,
	distance_name,
	id_feature,
	id_y_name,
	geometry,
	drop_geometry,
	drop_units,
	conn,
	dist_type,
	max_distance,
	candidate_crs,
	candidate_margin
) {
	if (nrow(y) == 0L) {
		stop("`y` must contain at least one feature.", call. = FALSE)
	}
	if (!is.null(id_y_name) && !id_y_name %in% names(y)) {
		stop(sprintf("Column `%s` is not present in `y`.", id_y_name), call. = FALSE)
	}

	geometry_name <- if (rlang::quo_is_null(geometry)) {
		attr(x, "sf_column")
	} else {
		rlang::as_name(rlang::quo_get_expr(geometry))
	}
	if (!geometry_name %in% names(x)) {
		stop("The selected geometry column is not present in `x`.", call. = FALSE)
	}

	distance_unit <- if (
		identical(dist_type, "haversine") ||
			identical(dist_type, "spheroid") ||
			(is.null(dist_type) && isTRUE(sf::st_is_longlat(x)))
	) {
		"m"
	} else {
		sf::st_crs(x)$units_gdal
	}

	if (nrow(x) == 0L) {
		distance <- numeric()
		if (!drop_units && !is.null(distance_unit) && !is.na(distance_unit)) {
			distance <- units::set_units(distance, distance_unit, mode = "standard")
		}
		x[[distance_name]] <- distance
		x <- dplyr::relocate(
			x,
			dplyr::all_of(distance_name),
			.before = dplyr::all_of(geometry_name)
		)
		if (id_feature) {
			x[["id_closest"]] <- if (is.null(id_y_name)) integer() else y[[id_y_name]][0]
		}
		return(if (drop_geometry) sf::st_drop_geometry(x) else x)
	}
	if (!is.null(max_distance)) {
		return(dist_nearest_sf_threshold(
			x = x,
			y = y,
			distance_name = distance_name,
			id_feature = id_feature,
			id_y_name = id_y_name,
			geometry_name = geometry_name,
			drop_geometry = drop_geometry,
			drop_units = drop_units,
			distance_unit = distance_unit,
			conn = conn,
			dist_type = dist_type,
			max_distance = max_distance,
			candidate_crs = candidate_crs,
			candidate_margin = candidate_margin
		))
	}

	distance_pairs <- duckspatial::ddbs_distance(
		x = x,
		y = y,
		dist_type = dist_type,
		conn = conn,
		quiet = TRUE
	)
	distance_table <- dbplyr::remote_name(distance_pairs)
	quoted_table <- as.character(DBI::dbQuoteIdentifier(conn, distance_table))
	on.exit(
		if (DBI::dbIsValid(conn)) {
			DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quoted_table))
		},
		add = TRUE
	)

	nearest <- DBI::dbGetQuery(conn, dist_nearest_rank_sql(quoted_table))
	if (
		nrow(nearest) != nrow(x) ||
			!identical(nearest$id_x, as.numeric(seq_len(nrow(x))))
	) {
		stop("DuckDB did not return exactly one nearest feature for every row of `x`.", call. = FALSE)
	}

	distance <- nearest$distance
	if (!drop_units && !is.null(distance_unit) && !is.na(distance_unit)) {
		distance <- units::set_units(distance, distance_unit, mode = "standard")
	}
	x[[distance_name]] <- distance
	x <- dplyr::relocate(
		x,
		dplyr::all_of(distance_name),
		.before = dplyr::all_of(geometry_name)
	)
	if (id_feature) {
		x[["id_closest"]] <- if (is.null(id_y_name)) {
			as.integer(nearest$id_y)
		} else {
			y[[id_y_name]][as.integer(nearest$id_y)]
		}
	}

	if (drop_geometry) sf::st_drop_geometry(x) else x
}

dist_nearest_sf_threshold <- function(
	x,
	y,
	distance_name,
	id_feature,
	id_y_name,
	geometry_name,
	drop_geometry,
	drop_units,
	distance_unit,
	conn,
	dist_type,
	max_distance,
	candidate_crs,
	candidate_margin
) {
	unique_suffix <- gsub("-", "_", uuid::UUIDgenerate())
	x_table <- paste0("dist_nearest_input_x_", unique_suffix)
	y_table <- paste0("dist_nearest_input_y_", unique_suffix)
	quote_id <- function(value) as.character(DBI::dbQuoteIdentifier(conn, value))

	duckspatial::ddbs_write_table(conn, x, x_table, quiet = TRUE)
	on.exit(
		if (DBI::dbIsValid(conn)) {
			DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quote_id(x_table)))
		},
		add = TRUE
	)
	duckspatial::ddbs_write_table(conn, y, y_table, quiet = TRUE)
	on.exit(
		if (DBI::dbIsValid(conn)) {
			DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quote_id(y_table)))
		},
		add = TRUE
	)

	result_lazy <- dist_nearest_table(
		x = x_table,
		y = y_table,
		distance_name = distance_name,
		id_feature = id_feature,
		id_y_name = id_y_name,
		geometry = rlang::new_quosure(rlang::sym(geometry_name), rlang::empty_env()),
		drop_geometry = drop_geometry,
		conn = conn,
		dist_type = dist_type,
		max_distance = max_distance,
		candidate_crs = candidate_crs,
		candidate_margin = candidate_margin
	)
	result_table <- dbplyr::remote_name(result_lazy)
	on.exit(
		if (DBI::dbIsValid(conn)) {
			DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quote_id(result_table)))
		},
		add = TRUE
	)

	result <- if (drop_geometry) {
		as.data.frame(dplyr::collect(result_lazy))
	} else {
		duckspatial::ddbs_collect(result_lazy)
	}
	if (!drop_geometry) {
		sf::st_crs(result) <- sf::st_crs(x)
	}
	if (!drop_units && !is.null(distance_unit) && !is.na(distance_unit)) {
		result[[distance_name]] <- units::set_units(
			result[[distance_name]],
			distance_unit,
			mode = "standard"
		)
	}
	result
}

dist_nearest_table <- function(
	x,
	y,
	distance_name,
	id_feature,
	id_y_name,
	geometry,
	drop_geometry,
	conn,
	dist_type,
	max_distance,
	candidate_crs,
	candidate_margin
) {
	x_lazy <- duckspatial::as_duckspatial_df(x, conn = conn)
	y_lazy <- duckspatial::as_duckspatial_df(y, conn = conn)
	x_columns <- colnames(x_lazy)
	y_columns <- colnames(y_lazy)
	geometry_name <- if (rlang::quo_is_null(geometry)) {
		duckspatial::ddbs_geom_col(x_lazy)
	} else {
		rlang::as_name(rlang::quo_get_expr(geometry))
	}
	if (!geometry_name %in% x_columns) {
		stop("The selected geometry column is not present in `x`.", call. = FALSE)
	}
	y_geometry_name <- duckspatial::ddbs_geom_col(y_lazy)
	if (!is.null(id_y_name) && !id_y_name %in% y_columns) {
		stop(sprintf("Column `%s` is not present in `y`.", id_y_name), call. = FALSE)
	}
	x_crs <- sf::st_crs(x_lazy)
	y_crs <- sf::st_crs(y_lazy)
	if (is.na(x_crs) || is.na(y_crs) || !isTRUE(x_crs == y_crs)) {
		stop("`x` and `y` must have the same known CRS.", call. = FALSE)
	}
	if (
		!is.null(max_distance) &&
			dist_type %in% c("haversine", "spheroid") &&
			(!identical(x_crs$epsg, 4326L) || !identical(y_crs$epsg, 4326L))
	) {
		stop("Geographic distance filtering requires `x` and `y` in EPSG:4326.", call. = FALSE)
	}

	unique_suffix <- gsub("-", "_", uuid::UUIDgenerate())
	x_indexed <- paste0("dist_nearest_x_", unique_suffix)
	y_indexed <- paste0("dist_nearest_y_", unique_suffix)
	result_table <- paste0("dist_nearest_result_", unique_suffix)
	id_x_internal <- "__dist_nearest_id_x"
	id_y_internal <- "__dist_nearest_id_y"
	quote_id <- function(value) as.character(DBI::dbQuoteIdentifier(conn, value))

	quoted_x <- quote_id(x)
	quoted_y <- quote_id(y)
	quoted_x_indexed <- quote_id(x_indexed)
	quoted_y_indexed <- quote_id(y_indexed)
	quoted_result <- quote_id(result_table)
	quoted_id_x <- quote_id(id_x_internal)
	quoted_id_y <- quote_id(id_y_internal)

	DBI::dbExecute(
		conn,
		paste(
			"CREATE TEMP TABLE",
			quoted_x_indexed,
			"AS",
			"SELECT ROW_NUMBER() OVER () AS",
			quoted_id_x,
			", * FROM",
			quoted_x
		)
	)
	on.exit(
		if (DBI::dbIsValid(conn)) {
			DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quoted_x_indexed))
		},
		add = TRUE
	)
	DBI::dbExecute(
		conn,
		paste(
			"CREATE TEMP TABLE",
			quoted_y_indexed,
			"AS",
			"SELECT ROW_NUMBER() OVER () AS",
			quoted_id_y,
			", * FROM",
			quoted_y
		)
	)
	on.exit(
		if (DBI::dbIsValid(conn)) {
			DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quoted_y_indexed))
		},
		add = TRUE
	)

	ny <- DBI::dbGetQuery(
		conn,
		paste("SELECT COUNT(*) AS n FROM", quoted_y_indexed)
	)[["n"]][[1]]
	if (ny == 0L) {
		stop("`y` must contain at least one feature.", call. = FALSE)
	}
	nx <- DBI::dbGetQuery(
		conn,
		paste("SELECT COUNT(*) AS n FROM", quoted_x_indexed)
	)[["n"]][[1]]

	excluded_x <- c(
		id_x_internal,
		geometry_name,
		intersect(distance_name, x_columns),
		if (id_feature) intersect("id_closest", x_columns)
	)
	result_columns <- setdiff(x_columns, excluded_x)
	base_select_columns <- paste0(
		"x.",
		vapply(result_columns, quote_id, character(1))
	)
	if (nx == 0L) {
		select_columns <- c(
			base_select_columns,
			paste0("CAST(NULL AS DOUBLE) AS ", quote_id(distance_name))
		)
		if (!drop_geometry) {
			select_columns <- c(select_columns, paste0("x.", quote_id(geometry_name)))
		}
		if (id_feature) {
			id_expression <- if (is.null(id_y_name)) {
				"CAST(NULL AS INTEGER)"
			} else {
				paste0(
					"(SELECT ",
					quote_id(id_y_name),
					" FROM ",
					quoted_y_indexed,
					" LIMIT 1)"
				)
			}
			select_columns <- c(
				select_columns,
				paste0(id_expression, " AS ", quote_id("id_closest"))
			)
		}
		DBI::dbExecute(
			conn,
			paste(
				"CREATE TEMP TABLE",
				quoted_result,
				"AS",
				"SELECT",
				paste(select_columns, collapse = ", "),
				"FROM",
				quoted_x_indexed,
				"x",
				paste0("ORDER BY x.", quoted_id_x)
			)
		)
		result <- dplyr::tbl(conn, result_table)
		return(
			if (drop_geometry) {
				result
			} else {
				duckspatial::as_duckspatial_df(
					result,
					conn = conn,
					crs = x_crs,
					geom_col = geometry_name
				)
			}
		)
	}

	if (is.null(max_distance)) {
		distance_pairs <- duckspatial::ddbs_distance(
			x = x_indexed,
			y = y_indexed,
			dist_type = dist_type,
			conn = conn,
			id_x = id_x_internal,
			id_y = id_y_internal,
			quiet = TRUE
		)
		distance_table <- dbplyr::remote_name(distance_pairs)
		quoted_distance <- quote_id(distance_table)
	} else {
		candidate_x <- x_indexed
		candidate_y <- y_indexed
		candidate_x_geometry <- geometry_name
		candidate_y_geometry <- y_geometry_name

		if (!is.null(candidate_crs)) {
			candidate_crs <- sf::st_crs(candidate_crs)
			if (is.na(candidate_crs) || !identical(candidate_crs$units_gdal, "metre")) {
				stop("`candidate_crs` must be a projected CRS with metre units.", call. = FALSE)
			}
			# x and y already share a known CRS; reuse indexed tables when no reprojection is needed.
			if (!isTRUE(x_crs == candidate_crs)) {
				candidate_x <- paste0("dist_nearest_candidate_x_", unique_suffix)
				candidate_y <- paste0("dist_nearest_candidate_y_", unique_suffix)
				duckspatial::ddbs_transform(
					x = x_indexed,
					y = candidate_crs,
					conn = conn,
					name = candidate_x,
					quiet = TRUE
				)
				on.exit(
					if (DBI::dbIsValid(conn)) {
						DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quote_id(candidate_x)))
					},
					add = TRUE
				)
				duckspatial::ddbs_transform(
					x = y_indexed,
					y = candidate_crs,
					conn = conn,
					name = candidate_y,
					quiet = TRUE
				)
				on.exit(
					if (DBI::dbIsValid(conn)) {
						DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quote_id(candidate_y)))
					},
					add = TRUE
				)
			}
		}

		candidate_table <- paste0("dist_nearest_pairs_", unique_suffix)
		quoted_candidate <- quote_id(candidate_table)
		candidate_cutoff <- as.character(
			DBI::dbQuoteLiteral(conn, max_distance * candidate_margin)
		)
		DBI::dbExecute(
			conn,
			paste(
				"CREATE TEMP TABLE",
				quoted_candidate,
				"AS SELECT",
				paste0("cx.", quoted_id_x),
				"AS id_x,",
				paste0("cy.", quoted_id_y),
				"AS id_y",
				"FROM",
				quote_id(candidate_x),
				"cx JOIN",
				quote_id(candidate_y),
				"cy ON ST_DWithin(",
				paste0("cx.", quote_id(candidate_x_geometry)),
				",",
				paste0("cy.", quote_id(candidate_y_geometry)),
				",",
				candidate_cutoff,
				")"
			)
		)
		on.exit(
			if (DBI::dbIsValid(conn)) {
				DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quoted_candidate))
			},
			add = TRUE
		)

		distance_table <- paste0("dist_nearest_distances_", unique_suffix)
		quoted_distance <- quote_id(distance_table)
		exact_cutoff <- as.character(DBI::dbQuoteLiteral(conn, max_distance))
		distance_expression <- dist_nearest_distance_sql(
			dist_type = dist_type,
			x_geometry = paste0("x.", quote_id(geometry_name)),
			y_geometry = paste0("y.", quote_id(y_geometry_name))
		)
		DBI::dbExecute(
			conn,
			paste(
				"CREATE TEMP TABLE",
				quoted_distance,
				"AS WITH exact_distances AS (SELECT",
				"p.id_x, p.id_y,",
				distance_expression,
				"AS distance FROM",
				quoted_candidate,
				"p JOIN",
				quoted_x_indexed,
				paste0("x ON x.", quoted_id_x, " = p.id_x"),
				"JOIN",
				quoted_y_indexed,
				paste0("y ON y.", quoted_id_y, " = p.id_y"),
				") SELECT * FROM exact_distances WHERE distance <=",
				exact_cutoff
			)
		)
	}
	on.exit(
		if (DBI::dbIsValid(conn)) {
			DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quoted_distance))
		},
		add = TRUE
	)

	select_columns <- c(
		base_select_columns,
		paste0("n.distance AS ", quote_id(distance_name))
	)
	if (!drop_geometry) {
		select_columns <- c(select_columns, paste0("x.", quote_id(geometry_name)))
	}
	if (id_feature) {
		id_expression <- if (is.null(id_y_name)) {
			"CAST(n.id_y AS INTEGER)"
		} else {
			paste0("y.", quote_id(id_y_name))
		}
		select_columns <- c(
			select_columns,
			paste0(id_expression, " AS ", quote_id("id_closest"))
		)
	}

	y_join <- if (id_feature && !is.null(id_y_name)) {
		paste(
			"JOIN",
			quoted_y_indexed,
			"y",
			paste0("ON y.", quoted_id_y, " = n.id_y")
		)
	} else {
		""
	}
	DBI::dbExecute(
		conn,
		paste(
			"CREATE TEMP TABLE",
			quoted_result,
			"AS",
			"WITH nearest AS (",
			dist_nearest_rank_sql(quoted_distance),
			")",
			"SELECT",
			paste(select_columns, collapse = ", "),
			"FROM",
			quoted_x_indexed,
			"x",
			paste0("JOIN nearest n ON x.", quoted_id_x, " = n.id_x"),
			y_join,
			paste0("ORDER BY x.", quoted_id_x)
		)
	)

	n_result <- DBI::dbGetQuery(
		conn,
		paste("SELECT COUNT(*) AS n FROM", quoted_result)
	)[["n"]][[1]]
	cardinality_failed <- if (is.null(max_distance)) {
		n_result != nx
	} else {
		n_result > nx
	}
	if (cardinality_failed) {
		DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", quoted_result))
		stop("DuckDB did not return exactly one nearest feature for every row of `x`.", call. = FALSE)
	}

	result <- dplyr::tbl(conn, result_table)
	if (drop_geometry) {
		result
	} else {
		duckspatial::as_duckspatial_df(
			result,
			conn = conn,
			crs = x_crs,
			geom_col = geometry_name
		)
	}
}

dist_nearest_rank_sql <- function(quoted_table) {
	paste(
		"SELECT id_x, id_y, distance",
		"FROM (",
		"SELECT id_x, id_y, distance,",
		"ROW_NUMBER() OVER (",
		"PARTITION BY id_x ORDER BY distance ASC NULLS LAST, id_y ASC",
		") AS nearest_rank",
		"FROM",
		quoted_table,
		") ranked",
		"WHERE nearest_rank = 1",
		"ORDER BY id_x"
	)
}

dist_nearest_distance_sql <- function(dist_type, x_geometry, y_geometry) {
	switch(
		dist_type,
		planar = paste0("ST_Distance(", x_geometry, ", ", y_geometry, ")"),
		geos = paste0("ST_Distance_GEOS(", x_geometry, ", ", y_geometry, ")"),
		haversine = paste0(
			"ST_Distance_Sphere(",
			"ST_Point(ST_Y(",
			x_geometry,
			"), ST_X(",
			x_geometry,
			")), ",
			"ST_Point(ST_Y(",
			y_geometry,
			"), ST_X(",
			y_geometry,
			")))"
		),
		spheroid = paste0(
			"ST_Distance_Spheroid(",
			"ST_Point(ST_Y(",
			x_geometry,
			"), ST_X(",
			x_geometry,
			")), ",
			"ST_Point(ST_Y(",
			y_geometry,
			"), ST_X(",
			y_geometry,
			")))"
		),
		stop("Unsupported `dist_type`.", call. = FALSE)
	)
}
