# Ambiente para montar GTFS e rotear com r5r neste servidor: R 4.6 com a biblioteca renv e o Java 21
# do projeto do Arthur, usados só para leitura (nada é escrito na pasta dele). Carregar antes de
# qualquer library() e rodar com o Rscript do R 4.6.
#
# Versão do r5r: padrão 2.4.0 (a do renv do projeto, R5 v7.5.1). Com a variável de ambiente
# R5R_VERSAO=2.5.0, usa o r5r 2.5.0 (R5 v7.6; argumento seed), instalado em 14/11/2026 numa
# biblioteca de usuário separada, com cache próprio do motor R5: o r5r apaga, ao carregar, caches de
# motor de outras versões, então as duas versões não podem dividir o mesmo cache.

## caminhos desta máquina (raiz_arthur etc.): arquivo local, fora do git; o modelo é R/00_config_local_exemplo.R
if (!file.exists("R/00_config_local.R")) stop("falta R/00_config_local.R: copie R/00_config_local_exemplo.R e preencha os caminhos desta máquina")
source("R/00_config_local.R")
if (getRversion() < "4.6.0") stop("rodar com o R 4.6 (biblioteca renv do projeto é de R-4.6)")
.libPaths(c(file.path(raiz_arthur, "renv/library/windows/R-4.6/x86_64-w64-mingw32"), .libPaths()))
Sys.setenv(JAVA_HOME = file.path(raiz_arthur, "rjavaenv/windows/x64/21/21"))

versao_r5r <- Sys.getenv("R5R_VERSAO", "2.4.0")
if (versao_r5r == "2.5.0") {
	usuario <- Sys.getenv("LOCALAPPDATA")
	.libPaths(c(file.path(usuario, "R/win-library/4.6-r5r250"), .libPaths()))
	Sys.setenv(R_USER_CACHE_DIR = file.path(usuario, "R/cache-r5r250"))
} else if (versao_r5r != "2.4.0") {
	stop("R5R_VERSAO deve ser 2.4.0 ou 2.5.0")
}
