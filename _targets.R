# Pipeline do projeto (24/11/2026): especificação final, com a DATA EXATA DE SERVIÇO PLENO.
# Cada data de serviço pleno é um evento (19 eventos, 2014–2022); referência = mês anterior ao pleno; horizontes de 6, 12,
# 18 e 24 meses; placebo 25 meses antes da primeira abertura; B&H com 199 permutações.
# Onde está cada resultado: notas/mapa_resultados.md (gerado pelo próprio pipeline).
#
# Estágios: 1 insumos externos (acompanhados) -> 2 montante (R/01, R/02, R/04, R/05, R/42, R/89, R/90, R/107, R/109–R/112, R/116)
#   -> 3 preparação (R/117) -> 4 bases por evento (R/118, R/119) -> 5 estimações (R/120–R/124) -> 6 tabelas, checagem e
#   mapa de resultados (R/125, R/pipeline_final.R).
#
# Uso (R 4.6, na raiz do projeto; antes, copiar R/00_config_local_exemplo.R para R/00_config_local.R e preencher):
#   Rscript -e "source('R/00_ambiente_r5.R'); targets::tar_manifest()"   # o que existe (não roda nada)
#   Rscript -e "source('R/00_ambiente_r5.R'); targets::tar_outdated()"   # o que rodaria
#   Rscript -e "source('R/00_ambiente_r5.R'); targets::tar_make()"       # roda o que estiver desatualizado
# Variáveis de ambiente (todas opcionais):
#   TSP_MONTANTE   vazio (padrão): as etapas a montante só acompanham os arquivos já prontos, nada roda;
#                  leve: roda as que não roteiam; tudo: roda também o roteamento r5r (dias de máquina).
#   TSP_PENDENTES  1: inclui a robustez pendente R4 (curva mês a mês) e R5 (endereço-base g−1 a g−4), com bases próprias.
#   TSP_ADOTAR     1: na primeira rodada, registra as saídas que já existem (rodadas feitas com os scripts soltos) sem rodar.
#   TSP_PESADOS    número de estimações pesadas ao mesmo tempo (padrão 3); TSP_LEVES: alvos leves (padrão 4).
#   TSP_RESERVA_GB memória livre que deve sobrar para os outros usuários (padrão 60); TSP_ESPERA_S: intervalo mínimo entre
#                  largadas pesadas (padrão 120 s).
# Logs de cada alvo: outputs/logs_pipeline/<alvo>.txt.

source("R/00_ambiente_r5.R")
## os processos do crew herdam as bibliotecas (targets e crew estão na biblioteca renv do projeto do Arthur, só leitura)
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
library(targets)
source("R/pipeline_final.R", encoding = "UTF-8")

n_pesados <- as.integer(Sys.getenv("TSP_PESADOS", "3"))
n_leves <- as.integer(Sys.getenv("TSP_LEVES", "4"))
tar_option_set(
	controller = crew::crew_controller_group(
		crew::crew_controller_local(name = "leve", workers = n_leves, seconds_idle = 300),
		crew::crew_controller_local(name = "pesado", workers = n_pesados, seconds_idle = 300)
	),
	resources = tar_resources(crew = tar_resources_crew(controller = "leve")),
	error = "continue",
	memory = "transient",
	garbage_collection = TRUE
)

## scripts e funções: mudar um deles refaz o que depende dele ---------------------------------------------------------------
scripts <- c(
	scr_ambiente = "R/00_ambiente_r5.R", scr_config = "R/00_config.R", scr_funcoes = "R/funcoes_desenhos.R",
	scr_fun_evento = "R/funcoes_evento.R", scr_fun_bh = "R/funcoes_bh_evento.R", scr_fun_iptw = "R/funcoes_iptw_evento.R", scr_monta_par = "R/evento_par_monta.R",
	scr_r01 = "R/01_copiar_insumos.R", scr_r02 = "R/02_estacoes.R", scr_r04 = "R/04_rais_extrair.R", scr_r05 = "R/05_rais_pessoa_ano.R",
	scr_r42 = "R/42_rotear_familia_a.R", scr_r89 = "R/89_base_pessoa_ano.R", scr_r90 = "R/90_datas_insumos.R",
	scr_r107 = "R/107_ahlfeldt_previsao.R", scr_r109 = "R/109_pares_longos_t3.R", scr_r110 = "R/110_rotear_pares_t3.R", scr_r111 = "R/111_matriz_a23_240.R",
	scr_r112 = "R/112_acesso_240_familia_a.R", scr_r116 = "R/116_ganhos_gravitacional.R", scr_r117 = "R/117_criancas_cadunico.R",
	scr_r118 = "R/118_base_evento_area.R", scr_r119 = "R/119_base_evento_par.R", scr_r120 = "R/120_bh_evento_area.R", scr_r121 = "R/121_bh_evento_par.R",
	scr_r122 = "R/122_bh_evento_robustez.R", scr_r123 = "R/123_iptw_evento_area.R", scr_r124 = "R/124_iptw_evento_par.R", scr_r125 = "R/125_tabelas_final.R",
	scr_r127 = "R/127_futuros_evento.R", scr_pipeline = "R/pipeline_final.R",
	scr_fun_relatorio = "R/funcoes_relatorio.R", scr_relatorio_qmd = "relatorio/relatorio_desenhos.qmd", scr_relatorio_css = "relatorio/relatorio.css"
)
alvos_scripts <- lapply(names(scripts), function(n) tar_target_raw(n, substitute(c(ARQ), list(ARQ = scripts[[n]])), format = "file", deployment = "main"))

## grade: etapas a montante (acompanhadas por padrão) e especificação final ----------------------------------------------------
grade <- rbind(grade_montante(dir_cad_arthur = file.path(raiz_arthur, "data/temp"), dir_cadunico_bruto = dir_cadunico_bruto), grade_final())
simbolos <- function(x) as.call(c(as.symbol("list"), lapply(x, as.symbol)))
alvo_da_grade <- function(g) {
	deps <- separa(g$deps)
	saidas <- separa(g$saidas)
	if (g$modo == "acompanha") {
		## arquivos já prontos: acompanhados por data e tamanho (file_fast), no processo principal; nenhum script roda
		cmd <- substitute(acompanha(saidas = SAI, deps = DEPS, origem = ORI), list(SAI = saidas, DEPS = simbolos(deps), ORI = g$descricao))
		return(tar_target_raw(g$alvo, cmd, format = "file_fast", deployment = "main", priority = 1))
	}
	deps <- unique(c(deps, "scr_ambiente", "scr_config", "scr_funcoes"))
	cmd <- substitute(roda_script(script = SCR, args = ARGS, saidas = SAI, log = LOG, deps = DEPS, mem_gb = MEM, exige_nova = NOVA, r5r = R5R), list(
		SCR = as.symbol(g$script), ARGS = if (nzchar(g$args)) strsplit(g$args, " ", fixed = TRUE)[[1]] else character(0), SAI = saidas,
		LOG = file.path("outputs/logs_pipeline", paste0(g$alvo, ".txt")), DEPS = simbolos(deps), MEM = g$mem_gb, NOVA = g$exige_nova, R5R = g$r5r))
	## bases e matrizes grandes: acompanhadas por data e tamanho; resultados (csv pequenos): por conteúdo
	grande <- any(grepl("[.](parquet|rds|dat)$", saidas))
	tar_target_raw(g$alvo, cmd, format = if (grande) "file_fast" else "file", priority = 1 - g$prioridade / 10,
		resources = tar_resources(crew = tar_resources_crew(controller = g$controlador)),
		## o roteamento só roda com TSP_MONTANTE=tudo; mesmo assim, nunca por causa de mudança em código ou insumo
		cue = if (g$r5r) tar_cue(mode = "never") else tar_cue(mode = "thorough"))
}
alvos_grade <- lapply(seq_len(nrow(grade)), function(i) alvo_da_grade(grade[i, ]))

## tabelas, checagem e mapa de resultados --------------------------------------------------------------------------------------
est <- grade$alvo[grade$estagio == "5 estimações"]
bases <- grade$alvo[grade$estagio == "4 bases por evento"]
tabelas <- file.path("outputs/final/tabelas", c(sprintf("bloco_%s.csv", c("A_segmentos", "A_contrastes", "B_segmentos", "B_contrastes", "C_segmentos", "C_contrastes",
	"C_descricao", "D_segmentos", "D_contrastes", "D_balanco", "E_robustez", "E_segmentos", "F_segmentos", "F_contrastes")), "resumo_principais.md"))
alvos_final <- list(
	tar_target_raw("tab_final", substitute(roda_script(script = scr_r125, args = character(0), saidas = SAI, log = "outputs/logs_pipeline/tab_final.txt",
		deps = DEPS), list(SAI = tabelas, DEPS = simbolos(c(est, "scr_pipeline", "scr_ambiente", "scr_config")))), format = "file", priority = 0),
	tar_target_raw("checagem", substitute({
		r <- do.call(rbind, lapply(unlist(TODAS), checa_saida))
		dir.create("outputs/final", showWarnings = FALSE, recursive = TRUE)
		utils::write.csv(r, "outputs/final/checagem_pipeline.csv", row.names = FALSE, fileEncoding = "UTF-8")
		"outputs/final/checagem_pipeline.csv"
	}, list(TODAS = simbolos(c(bases, est)))), format = "file", priority = 0),
	## o mapa depende só da grade (R/pipeline_final.R): fica pronto antes de qualquer estimação
	tar_target_raw("mapa_resultados", quote({
		scr_pipeline
		grava_mapa(grade_final(pendentes = TRUE))
	}), format = "file", deployment = "main", priority = 1),
	## relatório HTML dos desenhos (dados, mapa das linhas, resumo e um bloco por desenho): refeito quando qualquer estimação,
	## o texto ou as funções do relatório mudam
	tar_target_raw("relatorio_html", substitute(renderiza_relatorio(deps = DEPS), list(DEPS = simbolos(c(est, "mapa_resultados", "scr_fun_relatorio",
		"scr_relatorio_qmd", "scr_relatorio_css")))), format = "file", deployment = "main", priority = 0)
)

c(alvos_scripts, alvos_grade, alvos_final)
