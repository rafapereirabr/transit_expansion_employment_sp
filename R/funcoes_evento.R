# Funções comuns da especificação final por evento (data exata de serviço pleno; 24/11/2026). Reúne o que os scripts
# soltos repetiam: índices de mês, emprego no fim do mês (máscaras do R/89), salário-hora real do vínculo principal,
# marcador de criança de 0 a 5 anos na família e distâncias em linha reta entre hexágonos H3-9.
# Usado por R/118, R/119, R/123 e R/124 (carregar depois de R/00_config.R e R/funcoes_desenhos.R, com data.table e a
# função msg definida).

## tipos de vínculo celetista (regra do Arthur) e índice de mês (1 = jan/2011)
eleg <- c(10L, 15L, 20L, 25L, 50L, 55L, 60L, 65L, 70L, 75L, 90L)
mes_idx <- function(d) (year(d) - 2011L) * 12L + month(d)
ano_de <- function(m) 2011L + (m - 1L) %/% 12L
mes_de <- function(m) (m - 1L) %% 12L + 1L

## emprego no fim do mês m (qualquer vínculo celetista; máscaras emp_mask_<ano> do R/89, juntadas com junta_pa)
emp_mes <- function(x, m) {
	a <- ano_de(m)
	if (a > 2025L || a < 2011L) return(rep(NA, nrow(x)))
	bitwAnd(x[[paste0("emp_mask_", a)]], bitwShiftL(1L, mes_de(m) - 1L)) > 0
}

## IPCA de dezembro (R/05) e fator para reais de dez/2023
IPC <- fread("outputs/ipca_dezembro.csv")
IPC[, fator := ipca_dez[ano == 2023] / ipca_dez]
fator_ano <- function(a) IPC$fator[match(a, IPC$ano)]

## salário-hora real do vínculo principal no fim do mês (salário contratual com a correção e a faixa válida do R/05)
sal_mes_dt <- function(salario, tipo, horas) {
	fcase(is.na(salario) | salario <= 0 | is.na(tipo), NA_real_, tipo == 1, salario, tipo == 2, salario * 2, tipo == 3, salario * 30 / 7,
		tipo == 4, salario * 30, tipo == 5, salario * 5 * horas, tipo %in% c(6, 7), salario, default = NA_real_)
}
RS <- list()
rais_sal <- function(a) {
	kk <- as.character(a)
	if (is.null(RS[[kk]])) {
		f <- file.path(dir_rais, sprintf("rais_vinc_%d.parquet", a))
		cols <- intersect(c("cpf_num", "tipo_vinculo", "emp_31dez", "data_adm", "data_deslig", "mes_deslig", "salario", "tipo_salario", "horas_contr",
			"rem_med_r", "ind_trab_intermitente"), names(arrow::open_dataset(f)$schema))
		v <- as.data.table(arrow::read_parquet(f, col_select = cols))
		if (!"ind_trab_intermitente" %in% names(v)) v[, ind_trab_intermitente := NA_integer_]
		v <- v[as.integer(tipo_vinculo) %in% eleg]
		sm_hora <- salario_minimo_dez[[as.character(a)]] / ((52 / 12) * 44)
		v[, `:=`(adm = as.integer(as.IDate(data_adm)),
			m_sep = fifelse(emp_31dez %in% 1L, 13L, fifelse(!is.na(data_deslig) & year(data_deslig) == a, month(data_deslig),
				fifelse(as.integer(mes_deslig) %in% 1:12, as.integer(mes_deslig), NA_integer_))),
			sal = sal_mes_dt(as.numeric(salario), as.integer(tipo_salario), as.numeric(horas_contr)),
			horas_ok = !is.na(horas_contr) & as.numeric(horas_contr) > 0 & !(as.integer(ind_trab_intermitente) %in% 1L))]
		hr <- v$sal / ((52 / 12) * as.numeric(v$horas_contr))
		razao <- as.numeric(v$rem_med_r) / v$sal
		x100 <- !is.na(hr) & v$horas_ok & (hr < 0.5 * sm_hora | hr > 100 * sm_hora) & !is.na(razao) & razao >= 50 & razao <= 200
		v[x100, sal := sal * 100]
		hr <- v$sal / ((52 / 12) * as.numeric(v$horas_contr))
		v[, lsalh := fifelse(horas_ok & !is.na(hr) & hr >= 0.5 * sm_hora & hr <= 100 * sm_hora, log(hr * IPC$fator[IPC$ano == a]), NA_real_)]
		RS[[kk]] <<- v[!is.na(m_sep), .(cpf_num, adm, m_sep, rem = fcoalesce(as.numeric(rem_med_r), 0), lsalh)]
		rm(v)
		gc()
	}
	RS[[kk]]
}
lsalh_mes <- function(cpf, m) {
	a <- ano_de(m)
	if (a > 2025L) return(rep(NA_real_, length(cpf)))
	mm <- mes_de(m)
	fim_mes <- as.integer(as.IDate(sprintf("%d-%02d-01", a + (mm == 12L), mm %% 12L + 1L))) - 1L
	v <- rais_sal(a)[cpf_num %in% cpf & adm <= fim_mes & m_sep > mm]
	v <- v[order(-rem)][!duplicated(cpf_num)]
	v$lsalh[match(cpf, v$cpf_num)]
}

## criança de 0 a 5 anos na família no ano-base do endereço: a família da pessoa vem do CadÚnico do Arthur (só leitura;
## só cpf e id_family do ano-base b) e a presença de criança vem do CadÚnico bruto harmonizado de dezembro de b, com
## todos os membros (data/base/explora/criancas/fam_crianca_<b>.rds, feito pelo R/117). Mesma definição em todos os anos.
dir_art_cad <- file.path(dir_arthur, "temp")
CADF <- list()
flag_crianca_ano <- function(b) {
	kk <- as.character(b)
	if (is.null(CADF[[kk]])) {
		res <- data.table(cpf_num = numeric(0), crianca = logical(0))
		fk <- sprintf("data/base/explora/criancas/fam_crianca_%d.rds", b)
		f <- file.path(dir_art_cad, sprintf("cad_individ_%d.parquet", b))
		if (file.exists(fk) && file.exists(f)) {
			tb <- arrow::read_parquet(f, col_select = c("cpf", "id_family"), as_data_frame = FALSE)
			d <- data.table(cpf_num = as.numeric(as.vector(tb$cpf)), fam = as.vector(tb$id_family$cast(arrow::float64())))
			rm(tb)
			kid <- readRDS(fk)
			res <- unique(d[!is.na(cpf_num), .(cpf_num, crianca = fam %in% kid)], by = "cpf_num")
			msg("ano-base ", b, ": ", round(100 * mean(res$crianca), 1), "% das pessoas em família com criança de 0 a 5 anos")
			rm(d, kid)
			gc()
		} else msg("ano-base ", b, ": sem arquivo de famílias com criança")
		CADF[[kk]] <<- res
	}
	CADF[[kk]]
}
marca_crianca <- function(x, g, k) {
	if (is.null(x)) return(x)
	x[, crianca := NA]
	b <- if (k == 0L) g - x$k_base else rep(g - k, nrow(x))
	for (bb in unique(b)) {
		f <- flag_crianca_ano(bb)
		ix <- which(b == bb)
		set(x, ix, "crianca", f$crianca[match(x$cpf_num[ix], f$cpf_num)])
	}
	msg("coorte ", g, ": marcador de criança ausente para ", sum(is.na(x$crianca)), " de ", nrow(x), " pessoas")
	x
}

## centroides dos hexágonos H3-9 e distância em linha reta (km) entre dois hexágonos
CEN <- data.table(cell = character(0), lon = numeric(0), lat = numeric(0))
centro <- function(cells) {
	novos <- setdiff(unique(cells[!is.na(cells)]), CEN$cell)
	if (length(novos)) {
		cc <- sf::st_coordinates(h3jsr::cell_to_point(novos, simple = TRUE))
		CEN <<- rbind(CEN, data.table(cell = novos, lon = cc[, 1], lat = cc[, 2]))
	}
	CEN[match(cells, cell), .(lon, lat)]
}
dist_h3 <- function(a, b) {
	p <- centro(a)
	q <- centro(b)
	rad <- pi / 180
	dlat <- (q$lat - p$lat) * rad
	dlon <- (q$lon - p$lon) * rad
	h <- sin(dlat / 2)^2 + cos(p$lat * rad) * cos(q$lat * rad) * sin(dlon / 2)^2
	2 * 6371 * asin(pmin(1, sqrt(h)))
}

## célula-mãe H3 em blocos (o h3jsr falha com milhões de células de uma vez: erro de JSON no V8; 24/11/2026)
pai_h3 <- function(x, res) {
	u <- unique(x[!is.na(x)])
	p <- unlist(lapply(split(u, ceiling(seq_along(u) / 50000)), function(z) h3jsr::get_parent(z, res = res)), use.names = FALSE)
	p[match(x, u)]
}
