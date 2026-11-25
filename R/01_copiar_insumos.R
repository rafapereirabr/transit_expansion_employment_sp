# Copia os insumos atuais da pasta do Arthur (somente leitura na origem) e grava um manifesto
# com data de modificação, tamanho e md5 de cada arquivo, para saber qual versão foi usada.

source("R/00_config.R")

arquivos <- c(
	file.path("temp", "cad_fams.parquet"),
	file.path("temp", sprintf("cad_individ_%d.parquet", 2011:2023)),
	file.path("temp", sprintf("cad_individ_audit_%d.parquet", 2011:2023)),
	"transit_stations.parquet",
	"transit_lines.parquet",
	"munis_rmsp.parquet",
	"footprint_sf.parquet",
	"station_openings.xlsx"
)

origem <- file.path(dir_arthur, arquivos)
stopifnot(all(file.exists(origem)))
info_antes <- file.info(origem)[, c("size", "mtime")]

destino <- file.path(dir_insumos, basename(arquivos))
ok <- file.copy(origem, destino, overwrite = TRUE, copy.date = TRUE)
stopifnot(all(ok))

# a origem não pode ter mudado durante a cópia
info_depois <- file.info(origem)[, c("size", "mtime")]
stopifnot(identical(info_antes, info_depois))

manifesto <- data.frame(
	arquivo = basename(arquivos),
	origem = origem,
	mtime_origem = format(info_antes$mtime, "%Y-%m-%d %H:%M:%S"),
	bytes = info_antes$size,
	md5_origem = unname(tools::md5sum(origem)),
	md5_copia = unname(tools::md5sum(destino)),
	copiado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
stopifnot(identical(manifesto$md5_origem, manifesto$md5_copia))
write.csv(manifesto, file.path(dir_insumos, "_manifesto_insumos.csv"), row.names = FALSE)
message("Copiados ", nrow(manifesto), " arquivos (", round(sum(manifesto$bytes) / 1e9, 2), " GB).")
