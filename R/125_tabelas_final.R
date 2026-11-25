# Tabelas da especificação final (24/11/2026): reúne, por bloco, as saídas das estimações em outputs/final/ e grava um
# resumo dos desfechos principais. Só lê e grava agregados (nenhum dado por pessoa).
# Os arquivos de cada bloco vêm do mapa de resultados (R/pipeline_final.R), de modo que uma especificação nova na grade
# entra nas tabelas sem mexer aqui. Arquivos que ainda não existem (por exemplo os pendentes R4 e R5) são pulados.
# Uso: Rscript R/125_tabelas_final.R
# Saídas: outputs/final/tabelas/bloco_<letra>_<tabela>.csv e outputs/final/tabelas/resumo_principais.md
source("R/00_ambiente_r5.R")
source("R/00_config.R")
source("R/pipeline_final.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(data.table))
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
dir_tab <- "outputs/final/tabelas"
dir.create(dir_tab, recursive = TRUE, showWarnings = FALSE)
M <- as.data.table(mapa_resultados(grade_final(pendentes = TRUE)))
le <- function(f) {
	x <- fread(f, encoding = "UTF-8")
	x[, arquivo := basename(f)]
	x
}
empilha <- function(arqs) {
	arqs <- unique(arqs)
	falta <- arqs[!file.exists(arqs)]
	if (length(falta)) msg("pulados (não existem): ", paste(basename(falta), collapse = ", "))
	arqs <- setdiff(arqs, falta)
	if (!length(arqs)) return(data.table())
	rbindlist(lapply(arqs, le), fill = TRUE)
}
grava <- function(x, bloco, tabela) {
	f <- file.path(dir_tab, sprintf("bloco_%s_%s.csv", bloco, tabela))
	## arquivo sempre gravado (com cabeçalho mínimo se ainda não há resultados), para o alvo do pipeline ter saída fixa
	if (!nrow(x)) x <- data.table(arquivo = character(0))
	fwrite(x, f)
	msg(basename(f), ": ", nrow(x), " linhas")
	f
}
destino <- function(x) if ("outcome" %in% names(x)) grepl("^ped_", x$outcome) else rep(FALSE, nrow(x))

## A: acessibilidade a partir de casa
for (tb in c("segmentos", "contrastes")) grava(empilha(M[bloco == "A" & tabela == tb, arquivo]), "A", tb)
## B e C saem dos mesmos arquivos: B = desligamentos e permanência; C = destinos de quem pediu demissão e a descrição
for (tb in c("segmentos", "contrastes")) {
	x <- empilha(M[bloco %in% c("B", "C") & tabela == tb, arquivo])
	grava(x[!destino(x)], "B", tb)
	grava(x[destino(x)], "C", tb)
}
grava(empilha(M[bloco == "C" & tabela == "descricao", arquivo]), "C", "descricao")
## D: IPTW (área e tempo de viagem na mesma tabela; a coluna arquivo distingue)
for (tb in c("segmentos", "contrastes", "balanco")) grava(empilha(M[bloco == "D" & tabela == tb, arquivo]), "D", tb)
## E: robustez (R1, R2, R3 e R6 nos arquivos robustez_*; R7, R4 e R5 nos arquivos de segmentos)
grava(empilha(M[bloco == "E" & tabela == "robustez", arquivo]), "E", "robustez")
grava(empilha(M[bloco == "E" & tabela == "segmentos", arquivo]), "E", "segmentos")
## F: variações de efeito fixo, ao lado da especificação principal (mesmos segmentos, doses e modelo)
for (tb in c("segmentos", "contrastes")) grava(empilha(M[bloco == "F" & tabela == tb, arquivo]), "F", tb)

## resumo dos desfechos principais (pós e placebo): todos, mulheres e homens ------------------------------------------------
fmt <- function(b, ep, p) sprintf("%.2f (%.2f) p=%.3f", b, ep, p)
L <- c("# Especificação final: desfechos principais", "",
	"Gerado por R/125_tabelas_final.R a partir de outputs/final/. Efeito (EP de cluster por célula de 5 km) e p por permutação (199 sorteios).",
	"Unidade H3-9 (equivale ao painel individual); efeito fixo principal. Em pontos percentuais.", "")
A <- empilha(M[bloco == "A" & tabela == "segmentos" & grepl("h39", arquivo), arquivo])
if (nrow(A)) {
	A <- A[nivel == "h39" & fe == "casa7" & segmento %in% c("todos", "mulheres", "homens") & outcome %in% c("emprego_12", "entrada_12", "empregado_12") &
		dose %in% ROT_DOSE[c("c60", "grav")]]
	L <- c(L, "## A. Acessibilidade a partir de casa (12 meses)", "", "| Dose | Segmento | Desfecho | Efeito (pós) | Placebo (pré) |", "|---|---|---|---|---|")
	for (i in seq_len(nrow(A[janela == "pos"]))) {
		a <- A[janela == "pos"][i]
		b <- A[janela == "pre" & dose == a$dose & segmento == a$segmento & outcome == a$outcome]
		L <- c(L, sprintf("| %s | %s | %s | %s | %s |", a$dose, a$segmento, a$outcome, fmt(a$efeito, a$ep, a$p_perm), if (nrow(b)) fmt(b$efeito[1], b$ep[1], b$p_perm[1]) else ""))
	}
	L <- c(L, "")
}
B <- empilha(M[bloco == "B" & tabela == "segmentos" & grepl("h39", arquivo), arquivo])
if (nrow(B)) {
	B <- B[nivel == "h39" & fe == "casa7trab7" & modelo == "A" & segmento %in% c("todos", "mulheres", "homens") & outcome %in% c("demissao_6", "pedido_6", "empregado_6") &
		termo == "trajeto, até 90"]
	L <- c(L, "## B. Tempo de viagem ao próprio emprego (modelo A, trajeto até 90 min, por 10 min; 6 meses)", "", "| Segmento | Desfecho | Efeito (pós) | Placebo (pré) |",
		"|---|---|---|---|")
	for (i in seq_len(nrow(B[janela == "pos"]))) {
		a <- B[janela == "pos"][i]
		b <- B[janela == "pre" & segmento == a$segmento & outcome == a$outcome]
		L <- c(L, sprintf("| %s | %s | %s | %s |", a$segmento, a$outcome, fmt(a$efeito, a$ep, a$p_perm), if (nrow(b)) fmt(b$efeito[1], b$ep[1], b$p_perm[1]) else ""))
	}
	L <- c(L, "")
}
L <- c(L, "Onde está cada resultado: notas/mapa_resultados.md e outputs/final/mapa_resultados.csv.")
con <- file(file.path(dir_tab, "resumo_principais.md"), open = "wb", encoding = "UTF-8")
writeLines(enc2utf8(L), con, useBytes = TRUE)
close(con)
msg("fim tabelas")
