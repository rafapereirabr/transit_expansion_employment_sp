cad <- tar_read(cadunico_ind) |>
	arrow::open_dataset() |>
	filter(year == 2024) |>
	compute()

cpfs <- pull(cad, cpf)

# cad_fam <- tar_read(cadunico_fam) |>
#   arrow::open_dataset()

bcd <- ipeadatalake::ler_bcadastro(202503, "cpf")

# bcd <- file.path(Sys.getenv(bcadastro_dir, "parquet/20250326/20250326_cpf_parquet")) |>
#   arrow::open_dataset()

bcd <- bcd |>
	select(cpf, codmun_dom, cep, bairro, tipoLogradouro, logradouro, nroLogradouro) |>
	mutate(cpf = as.character(cpf)) |>
	filter(cpf %in% cpfs & codmun_dom == 3550308) |>
	compute()

bcd_geo <- bcd |>
	mutate(
		addr = paste(tipoLogradouro, logradouro),
		uf = stringr::str_sub(as.character(codmun_dom), 1, 2)
	) |>
	select(-tipoLogradouro) |>
	collect() |>
	geocodebr::geocode(
		campos = geocodebr::definir_campos("uf", "codmun_dom", "addr", "nroLogradouro", "cep", "bairro"),
		resolver_empates = T,
		h3_res = 9
	)

cad_bcd <- bcd_geo |>
	arrow::as_arrow_table() |>
	select(cpf, h3_09_bcd = h3_09, code_muni_bcd = codmun_dom) |>
	left_join(select(cad, cpf, h3_09_cad, code_muni_cad)) |>
	compute()

dist_df <- cad_bcd |>
	select(code_muni_cad, code_muni_bcd, starts_with("h3_")) |>
	mutate(
		across(starts_with("h3_"), h3o::h3_from_strings),
		dist = h3o::grid_distance(h3_09_cad, h3_09_bcd)
	)

dist_df <- dist_df |>
	mutate(
		uf_cad = floor(code_muni_cad / 1e5),
		uf_bcd = floor(code_muni_bcd / 1e5),
		same_h3 = h3_09_bcd == h3_09_cad,
		same_muni = code_muni_cad == code_muni_bcd,
		same_uf = uf_cad == uf_bcd
	)

dist_df |> filter(same_muni) |> pull(dist) |> summary()
dist_df |> filter(same_muni) |> pull(dist) |> quantile(0.95)

library(ggplot2)
dist_df |>
	filter(dist < 51) |>
	ggplot() +
	# stat_density(aes(x = dist)) +
	stat_bin(aes(x = dist)) +
	# stat_ecdf(aes(x = dist)) +
	# xlim(c(0, 100))
	theme_minimal()
