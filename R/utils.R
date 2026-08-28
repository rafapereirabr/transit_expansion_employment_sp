# utility functions

# write_parquet_target ------------------------------------------------------------------------

write_parquet_target <- function(data, data_path = NULL, overwrite, ...) {
  #' Write a parquet inside a target
  #'
  #' @description `write_parquet_target()` scales a common routine in targets with `file` format:
  #' checking if a file exists, saving `data` in a `parquet` file in a given `data_path`, and
  #' returning that path as the target result.
  #'
  #' @param data A `data.frame`, [RecordBatch][arrow::class-RecordBatch], or
  #'  [Table][arrow::class-Table] passed to [arrow::write_parquet()]. If `NULL`, the function won't
  #'  write anything at all and simply return `data`.
  #' @param data_path Formally, the `sink` argument of [arrow::write_parquet()]

  if (is.null(data_path)) {
    return(data)
  } else {
    if (!file.exists(data_path) | overwrite) {
      arrow::write_parquet(x = data, sink = data_path, ...)
    }
    return(data_path)
  }
}


# sf_from_parquet -----------------------------------------------------------------------------

sf_from_parquet <- function(data_path, drop_geometry = F) {
  #' Open a `geoarrow` parquet inside a target
  #'
  #' @description since `targets` is allergic to geoarrow, this wrapper reads it from a path and
  #' optionally drops the geometry column if wanted. Just another wrapper of a common routine.
  #'
  #' @param data_path Formally, the `sources` argument of [arrow::open_dataset()]
  #' @param drop_geometry When `TRUE`, drops the geometry column.

  data <- arrow::open_dataset(data_path) |>
    sf::st_as_sf()
  if (drop_geometry) {
    data <- sf::st_drop_geometry(data)
  }
  return(data)
}


# h3_from_sf ---------------------------------------------------------------------------------

# tar_load(munis_sf)
# data <- munis_sf
# col_name <- "h3_address"
# res <- 6
# drop <- TRUE

# tar_load(cadunico_ind, branches = c(1:2))
# h3 <- arrow::open_dataset(cadunico_ind) |>
#   select(h3_09) |>
#   pull(as_vector = T)
# col_name <- "h3_address"

h3_from_sf <- function(data, col_name = "h3_address", res = 9, drop = TRUE) {
  col_sym <- rlang::sym(col_name)
  if (inherits(data, "sf")) {
    input_sf <- TRUE
    h3 <- sf::st_geometry(data) |>
      h3o::sfc_to_cells(res) |>
      h3o::flatten_h3()

    data_h3 <- tibble::tibble(
      {{ col_sym }} := as.character(h3),
      geometry = sf::st_as_sfc(h3)
    )
  } else {
    input_sf <- FALSE
    h3 <- distinct(data, {{ col_sym }}) |>
      pull({{ col_sym }}, as_vector = T)

    h3_o <- h3o::h3_from_strings(h3)

    data_h3 <- tibble::tibble(
      {{ col_sym }} := h3,
      geometry = sf::st_as_sfc(h3_o)
    )
  }

  data_h3 <- sf::st_as_sf(data_h3)

  if (!drop) {
    if (inherits(data, "ArrowObject")) {
      data_df <- collect(data)
      data_h3 <- inner_join(data_df, data_h3, by = col_name)
    }
    if (inherits(data, "sf")) {
      data_df <- sf::st_drop_geometry(data)
      data_h3 <- cross_join(data_df, data_h3)
    }
  }

  return(data_h3)
}
