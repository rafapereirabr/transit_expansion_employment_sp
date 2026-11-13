# Read only explicitly whitelisted aggregate audits, never records/person-year microdata.
read_labor_audit_bundle <- function(files, prefix, years) {
	files <- unique(unlist(files, use.names = FALSE))
	pattern <- paste0("^", prefix, "_[0-9]{4}[.]parquet$")
	files <- files[grepl(pattern, basename(files))]
	file_year <- as.integer(sub(".*_([0-9]{4})[.]parquet$", "\\1", basename(files)))
	if (!length(files) || anyDuplicated(file_year) ||
		!setequal(file_year, unique(years)) || !all(file.exists(files))) {
		stop("Incomplete/duplicate tracked audit bundle: ", prefix)
	}
	data <- dplyr::bind_rows(lapply(files, arrow::read_parquet))
	return(data)
}

# Counts use distinct names: overwriting n_links/n_people inside summarise would
# corrupt subsequent weighted summaries. Ratios retain the unit of their denominator.
summarise_labor_audits <- function(
	rais_files, workplace_files, risk_sets, person_files,
	source_years, baseline_years, g_lag
) {
	read <- read_labor_audit_bundle
	manifest <- read(rais_files, "rais_manifest", source_years)
	person <- read(rais_files, "rais_person_audit", source_years)
	link <- read(rais_files, "rais_link_audit", source_years)
	geo <- read(workplace_files, "rais_workplace_audit", source_years)
	lookup <- read(workplace_files, "rais_workplace_lookup_audit", source_years)
	risk <- read(risk_sets, "risk_set_audit", baseline_years + g_lag)
	cad <- read(person_files, "cad_individ_audit", baseline_years)
	ratio <- function(x, n) 100 * x / ifelse(n > 0, n, NA_real_)
	people <- person |>
		dplyr::group_by(year) |>
		dplyr::summarise(
			n_people_total = sum(n_people),
			n_positive_people = sum(n_people[employment_status == "observed_celetista"]),
			n_uncertain_people = sum(n_people[employment_status == "unclassifiable_links"]),
			n_unconfirmed_people = sum(n_people[employment_status == "source_coverage_unconfirmed"]),
			n_no_match_people = sum(n_people[employment_status == "no_match"]),
			n_no_active_people = sum(n_people[employment_status == "no_active_celetista"]),
			.groups = "drop"
		)
	links <- link |>
		dplyr::group_by(year) |>
		dplyr::summarise(
			n_links_total = sum(n_links),
			n_active_links = sum(n_links[rais_active]),
			n_active_celetista = sum(n_links[rais_active & rais_celetista]),
			n_unknown_type = sum(n_links[!rais_type_known]),
			n_unknown_status = sum(n_links[!rais_active & !rais_inactive]),
			n_wage_positive_hours = sum(n_links[rais_active & rais_celetista & salary_positive & hours_positive]),
			n_monthly_verified = sum(n_links[rais_active & rais_celetista & salary_monthly_verified]),
			.groups = "drop"
		)
	geocodes <- geo |>
		dplyr::group_by(year) |>
		dplyr::summarise(
			n_establishments_total = sum(n_establishments),
			n_usable_establishments = sum(n_establishments[work_geo_reason == "usable"]),
			n_invalid_coordinate_establishments = sum(n_establishments[work_geo_reason == "invalid_coordinates"]),
			n_invalid_deviation_establishments = sum(n_establishments[work_geo_reason == "invalid_deviation"]),
			n_unknown_deviation_establishments = sum(n_establishments[work_geo_reason == "unknown_deviation"]),
			.groups = "drop"
		)
	annual <- manifest |>
		dplyr::left_join(people, by = "year", relationship = "one-to-one") |>
		dplyr::left_join(links, by = "year", relationship = "one-to-one") |>
		dplyr::left_join(geocodes, by = "year", relationship = "one-to-one") |>
		dplyr::left_join(lookup, by = "year", relationship = "one-to-one") |>
		dplyr::mutate(
			pct_positive_people = ratio(n_positive_people, n_roster),
			pct_unconfirmed_people = ratio(n_unconfirmed_people, n_roster),
			pct_wage_positive_hours = ratio(n_wage_positive_hours, n_active_celetista),
			pct_monthly_verified_links = ratio(n_monthly_verified, n_active_celetista),
			pct_usable_establishments = ratio(n_usable_establishments, n_establishments_total),
			pct_exact_link_duplicates = ratio(n_exact_duplicates_removed.x, n_matched_raw)
		) |>
		dplyr::arrange(year)
	# Both extraction and geocoder record exact duplicates; disambiguate those metrics.
	names(annual)[names(annual) == "n_exact_duplicates_removed.x"] <- "n_exact_link_duplicates_removed"
	names(annual)[names(annual) == "n_exact_duplicates_removed.y"] <- "n_exact_geocode_duplicates_removed"
	annual$delta_usable_establishment_pp <- c(NA_real_, diff(annual$pct_usable_establishments))
	if (anyDuplicated(manifest$year) || anyDuplicated(lookup$year)) stop("Duplicate annual audit keys.")
	risk_funnel <- dplyr::arrange(risk, event_year, step) |>
		dplyr::group_by(event_year) |>
		dplyr::mutate(
			n_initial = sum(n_records[reason == "initial"]),
			pct_initial = ratio(n_records, n_initial),
			n_before = n_initial - cumsum(dplyr::lag(
				ifelse(reason %in% c("initial", "retained"), 0, n_records), default = 0
			)),
			pct_stage = ratio(n_records, n_before)
		) |>
		dplyr::ungroup()
	# Audit order is explicit; downstream reconciliation does not rely on implicit row order.
	risk_funnel <- dplyr::arrange(risk_funnel, event_year, step)
	risk_totals <- risk |>
		dplyr::group_by(event_year, baseline_year) |>
		dplyr::summarise(
			n_initial = sum(n_records[reason == "initial"]),
			n_retained = sum(n_records[reason == "retained"]),
			n_excluded = sum(n_records[!reason %in% c("initial", "retained")]),
			.groups = "drop"
		) |>
		dplyr::mutate(pct_retained = ratio(n_retained, n_initial))
	cad_totals <- cad |>
		dplyr::group_by(year) |>
		dplyr::summarise(
			n_records_total = sum(n_records),
			n_retained = sum(n_records[reason == "retained"]),
			n_invalid_cpf = sum(n_records[reason == "invalid_cpf"]),
			n_duplicate_cpf = sum(n_records[reason == "duplicate_cpf"]),
			n_missing_birthdate = sum(n_records[reason == "missing_birthdate"]),
			.groups = "drop"
		) |>
		dplyr::arrange(year)
	salary_units <- link |>
		dplyr::filter(rais_active, rais_celetista) |>
		dplyr::group_by(year, salary_type_rais) |>
		dplyr::summarise(n_link_years = sum(n_links), .groups = "drop") |>
		dplyr::group_by(year) |>
		dplyr::mutate(pct_active_celetista_links = ratio(n_link_years, sum(n_link_years))) |>
		dplyr::ungroup()
	labels <- c(
		"1" = "Mensal", "2" = "Quinzenal", "3" = "Semanal", "4" = "Di\u00e1rio",
		"5" = "Hor\u00e1rio", "6" = "Tarefa", "7" = "Outros"
	)
	salary_units$unit_label <- unname(labels[as.character(salary_units$salary_type_rais)])
	salary_units$unit_label[is.na(salary_units$unit_label)] <- "Ausente/c\u00f3digo n\u00e3o reconhecido"
	geo_detail <- geo |>
		dplyr::left_join(dplyr::select(geocodes, year, n_establishments_total), by = "year") |>
		dplyr::mutate(pct_establishments = ratio(n_establishments, n_establishments_total))
	checks <- list()
	add_check <- function(name, year, passed, detail, severity = "error") {
		checks[[length(checks) + 1L]] <<- tibble::tibble(
			check = name, year = as.integer(year), passed = as.logical(passed),
			severity = severity, detail = detail
		)
	}
	add_check("person_roster_reconciliation", annual$year, annual$n_people_total == annual$n_roster,
		"Pessoas classificadas = roster anual.")
	add_check("unique_link_reconciliation", annual$year, annual$n_links_total == annual$n_matched_unique,
		"Audit de v\u00ednculos = v\u00ednculos \u00fanicos do manifest.")
	add_check("active_link_reconciliation", annual$year, annual$n_active_links == annual$n_active_retained,
		"Ativos no audit = ativos retidos.")
	add_check("exact_duplicate_reconciliation", annual$year,
		annual$n_matched_raw - annual$n_matched_unique == annual$n_exact_link_duplicates_removed,
		"Brutos - \u00fanicos = duplicatas removidas.")
	add_check("lookup_key_reconciliation", annual$year, annual$n_requested_keys == annual$n_output_keys,
		"Geocodifica\u00e7\u00e3o preserva chaves.")
	add_check("lookup_quality_reconciliation", annual$year, annual$n_output_keys == annual$n_establishments_total,
		"Chaves do lookup = estabelecimentos auditados.")
	add_check("lookup_duplicate_reconciliation", annual$year,
		annual$n_geocoder_rows - annual$n_distinct_geocoder_records == annual$n_exact_geocode_duplicates_removed,
		"Duplicatas exatas de geoc\u00f3digos reconciliam.")
	add_check("risk_set_funnel_reconciliation", risk_totals$event_year,
		risk_totals$n_initial == risk_totals$n_retained + risk_totals$n_excluded,
		"Inicial = exclus\u00f5es sequenciais + retidos.")
	add_check("extraction_completed", annual$year, annual$extraction_completed,
		"Conclus\u00e3o t\u00e9cnica da extra\u00e7\u00e3o registrada.")
	add_check("source_coverage_confirmed", annual$year, annual$source_coverage_confirmed,
		"Confirma\u00e7\u00e3o da fonte necess\u00e1ria antes de codificar aus\u00eancia como zero.", "gate")
	add_check("unknown_classification", annual$year,
		annual$n_unknown_status == 0 & annual$n_unknown_type == 0,
		"Auditar c\u00f3digos ou status n\u00e3o reconhecidos.", "warning")
	add_check("salary_type_available", annual$year, annual$salary_type_available,
		"Unidade contratual dispon\u00edvel para normaliza\u00e7\u00e3o.", "gate")
	checks <- dplyr::bind_rows(checks)
	inventory <- tibble::tibble(
		kind = c("rais_manifest", "rais_person_audit", "rais_link_audit",
			"rais_workplace_audit", "rais_workplace_lookup_audit",
			"risk_set_audit", "cad_individ_audit"),
		expected_years = c(rep(length(unique(source_years)), 5L),
			length(unique(baseline_years)), length(unique(baseline_years)))
	)
	return(list(
		annual = annual, checks = checks, risk_totals = risk_totals, risk_funnel = risk_funnel,
		cad_totals = cad_totals, cad_detail = cad, salary_units = salary_units,
		geocoding = geo_detail, inventory = inventory,
		scope = "Aggregate source audits only; no individual records or causal estimates.",
		limitations = c(
			"Roster \u00e9 a uni\u00e3o dos risk sets; n\u00e3o representa uma popula\u00e7\u00e3o de idade fixa em todos os anos.",
			"Pessoas do roster, v\u00ednculos e estabelecimentos t\u00eam denominadores diferentes.",
			"Geoc\u00f3digos utiliz\u00e1veis s\u00e3o uma propor\u00e7\u00e3o de estabelecimentos, n\u00e3o de trabalhadores ou horas.",
			"Sal\u00e1rio/horas positivos n\u00e3o garantem unidade mensal, consist\u00eancia temporal ou cobertura completa por pessoa.",
			"Audits agregados n\u00e3o identificam quem mudou de classifica\u00e7\u00e3o salarial entre vers\u00f5es.",
			"Audits atuais n\u00e3o fornecem suporte por tratamento/controle, status_num, covari\u00e1veis ou H3-7.",
			"Extra\u00e7\u00e3o conclu\u00edda n\u00e3o comprova cobertura da fonte; n\u00e3o converter automaticamente NA em zero."
		)
	))
}

labor_audit_html_escape <- function(x) {
	x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
	x <- gsub("<", "&lt;", x, fixed = TRUE)
	x <- gsub(">", "&gt;", x, fixed = TRUE)
	return(x)
}

labor_audit_html_table <- function(data) {
	if (!nrow(data)) return("<p>Sem observa\u00e7\u00f5es.</p>")
	header <- paste0("<th>", labor_audit_html_escape(names(data)), "</th>", collapse = "")
	rows <- vapply(seq_len(nrow(data)), function(i) {
		values <- vapply(data, function(column) {
			value <- column[i]
			if (is.na(value)) return("NA")
			if (is.numeric(value)) return(format(value, digits = 7, big.mark = ".", decimal.mark = ",", trim = TRUE))
			return(as.character(value))
		}, character(1))
		paste0("<tr>", paste0("<td>", labor_audit_html_escape(values), "</td>", collapse = ""), "</tr>")
	}, character(1))
	return(paste0("<div class='scroll'><table><thead><tr>", header,
		"</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"))
}

build_labor_audit_review <- function(
	rais_files, workplace_files, risk_sets, person_files,
	source_years, baseline_years, g_lag, save_dir
) {
	summary <- summarise_labor_audits(
		rais_files, workplace_files, risk_sets, person_files,
		source_years, baseline_years, g_lag
	)
	# Detailed administrative-data diagnostics stay in the same restricted storage.
	if (!dir.exists(save_dir)) stop("Restricted output directory does not exist.")
	paths <- c(
		report = file.path(save_dir, "labor_audit_review.html"),
		summary = file.path(save_dir, "labor_audit_review.rds")
	)
	select <- dplyr::select
	annual_people <- select(summary$annual, year, n_roster, n_positive_people,
		pct_positive_people, n_uncertain_people, n_unconfirmed_people,
		source_coverage_confirmed)
	annual_wages <- select(summary$annual, year, n_active_celetista,
		pct_wage_positive_hours, pct_monthly_verified_links,
		salary_type_column, monthly_salary_verified)
	annual_geo <- select(summary$annual, year, n_establishments_total,
		pct_usable_establishments, delta_usable_establishment_pp,
		n_invalid_coordinate_establishments, n_invalid_deviation_establishments,
		n_exact_geocode_duplicates_removed)
	section <- function(title, data) paste0("<h2>", title, "</h2>", labor_audit_html_table(data))
	issues <- dplyr::filter(summary$checks, is.na(passed) | !passed)
	html <- paste0(
		"<!doctype html><html lang='pt-BR'><meta charset='utf-8'>",
		"<meta name='viewport' content='width=device-width, initial-scale=1'>",
		"<title>Audits Cad\u00danico\u2013RAIS</title><style>",
		"body{font-family:system-ui,sans-serif;max-width:1200px;margin:2rem auto;padding:0 1rem;color:#172b3a}",
		"h1,h2{color:#123e54}.scroll{overflow:auto;max-height:32rem}table{border-collapse:collapse;font-size:.88rem;width:100%}",
		"th,td{padding:.45rem;border:1px solid #d4dde2;text-align:right}th{position:sticky;top:0;background:#e5eef2}",
		"td:first-child,th:first-child{text-align:left}tr:nth-child(even){background:#f6f8fa}",
		".note{padding:1rem;background:#fff3d8}a{color:#075b84}</style><body>",
		"<h1>Diagn\u00f3stico dos audits Cad\u00danico\u2013RAIS</h1>",
		"<p>Uso interno. Dados administrativos derivados; circula\u00e7\u00e3o sujeita ao acordo de dados.</p>",
		"<p>Snapshot dos arquivos salvos no store de targets. A geracao deste relatorio nao ",
		"reconstroi extracoes nem certifica que suas dependencias estejam atualizadas em relacao ao codigo.</p>",
		"<p>Gerado em ", format(Sys.time(), tz = "America/Sao_Paulo", usetz = TRUE), ".</p>",
		"<div class='note'><strong>Leitura:</strong> este relat\u00f3rio reconcilia extra\u00e7\u00e3o e qualidade de mensura\u00e7\u00e3o. ",
		"N\u00e3o estima efeitos nem valida as hip\u00f3teses DiD. As taxas abaixo usam o denominador indicado no campo.</div>",
		"<nav><a href='#checks'>Checagens</a> \u00b7 <a href='#people'>Pessoas</a> \u00b7 ",
		"<a href='#wages'>Sal\u00e1rio</a> \u00b7 <a href='#geo'>Workplaces</a> \u00b7 <a href='#risk'>Risk sets</a></nav>",
		"<h2>Limites de interpreta\u00e7\u00e3o e pr\u00f3ximos dados necess\u00e1rios</h2><ul>",
		paste0("<li>", labor_audit_html_escape(summary$limitations), "</li>", collapse = ""), "</ul>",
		"<div id='checks'>", section("Checagens pendentes ou reprovadas", issues), "</div>",
		section("Todas as reconcilia\u00e7\u00f5es", summary$checks),
		"<div id='people'>", section("Pessoas do roster por ano", annual_people), "</div>",
		"<p>O percentual positivo \u00e9 emprego celetista observado no roster-union. ",
		"Os demais casos n\u00e3o s\u00e3o zeros certificados enquanto a cobertura estiver n\u00e3o confirmada.</p>",
		"<div id='wages'>", section("Qualidade salarial por v\u00ednculo ativo celetista", annual_wages), "</div>",
		section("Unidades salariais por ano \u2014 contagens de v\u00ednculos", summary$salary_units),
		"<div id='geo'>", section("Workplaces \u2014 estabelecimentos, sem pondera\u00e7\u00e3o por trabalhadores", annual_geo), "</div>",
		section("Motivos de indisponibilidade geogr\u00e1fica", summary$geocoding),
		"<div id='risk'>", section("Reten\u00e7\u00e3o por risk set", summary$risk_totals), "</div>",
		section("Exclus\u00f5es sequenciais por risk set", summary$risk_funnel),
		section("Extra\u00e7\u00e3o Cad\u00danico \u2014 somente snapshots relevantes", summary$cad_totals),
		section("Audit detalhado Cad\u00danico", summary$cad_detail),
		section("Invent\u00e1rio esperado", summary$inventory),
		"</body></html>"
	)
	saveRDS(summary, paths[["summary"]])
	writeLines(html, paths[["report"]], useBytes = TRUE)
	return(paths)
}
