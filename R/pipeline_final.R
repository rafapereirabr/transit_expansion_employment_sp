# Funções e grade de especificações do pipeline da especificação final (_targets.R; 24/11/2026).
# Plano e justificativas: notas/plano_targets_2026-11-24.md. Tudo aqui usa só R base, para que a grade e o mapa de
# resultados possam ser gerados sem carregar o targets nem os dados.
#
# Conteúdo:
#   1. roda_script() e acompanha(): como um alvo roda um script do projeto ou só acompanha arquivos já prontos;
#   2. espera_memoria(): portão de memória para as estimações pesadas (máquina de 512 GB, compartilhada);
#   3. grade_montante(): etapas a montante (R/01, R/02, R/04, R/05, R/42, R/89, R/90, R/107, R/109–R/112, R/116);
#   4. grade_final(): preparação, bases por evento, B&H, IPTW, robustez, efeitos fixos e pendentes (R4, R5);
#   5. mapa_resultados() e grava_mapa(): de cada especificação ao arquivo e às colunas do resultado;
#   6. checa_saida(): checagem das saídas.

# 1. rodar e acompanhar ------------------------------------------------------------------------------------------------

## roda um script do projeto com Rscript e confere que as saídas foram gravadas nesta rodada.
## - exige_nova = FALSE para scripts retomáveis (pulam o que já existe): basta as saídas existirem.
## - mem_gb: pico de memória declarado; antes de começar espera haver essa folga (espera_memoria).
## - r5r = TRUE: roda com R5R_VERSAO=2.5.0 (roteamento).
## - TSP_ADOTAR=1 (variável de ambiente): se todas as saídas já existem, não roda e as registra como estão. Serve só
##   para a primeira rodada do pipeline, para adotar os resultados das rodadas feitas com os scripts soltos.
roda_script <- function(script, args, saidas, log, deps = NULL, mem_gb = 0, exige_nova = TRUE, r5r = FALSE) {
	force(deps)
	dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
	## adoção sem rodar: só saídas conferidas uma a uma (log da rodada sem erro e com o marcador de fim) e listadas em
	## outputs/final/concluidas.txt; um arquivo existente pode ser parcial, porque os scripts gravam a cada segmento
	if (identical(Sys.getenv("TSP_ADOTAR"), "1") && all(file.exists(saidas))) {
		conf <- if (file.exists("outputs/final/concluidas.txt")) readLines("outputs/final/concluidas.txt", warn = FALSE) else character(0)
		if (all(saidas %in% conf)) {
			cat(format(Sys.time()), "saídas conferidas adotadas sem rodar (TSP_ADOTAR=1):", paste(saidas, collapse = ", "), "\n", file = log, append = TRUE)
			return(saidas)
		}
	}
	rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
	for (d in unique(dirname(saidas))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
	espera_memoria(mem_gb)
	if (r5r) Sys.setenv(R5R_VERSAO = "2.5.0") else Sys.unsetenv("R5R_VERSAO")
	inicio <- Sys.time() - 1
	st <- system2(rscript, c(shQuote(script), args), stdout = log, stderr = log)
	Sys.unsetenv("R5R_VERSAO")
	if (!identical(as.integer(st), 0L)) stop("o script falhou (código ", st, "): ", script, " ", paste(args, collapse = " "), "; ver ", log)
	## neste servidor o Rscript pode sair com código 0 depois de um erro (24/11/2026): confere o log também
	erros <- grep("^Erro|Error in|Execução interrompida|terminate called|cannot allocate", readLines(log, warn = FALSE), value = TRUE, useBytes = TRUE)
	if (length(erros)) stop("o script terminou com erro no log (", utils::head(erros, 1), "): ", script, " ", paste(args, collapse = " "), "; ver ", log)
	faltam <-saidas[!file.exists(saidas) | (exige_nova & file.mtime(saidas) < inicio)]
	if (length(faltam)) stop("saída não gravada nesta rodada: ", paste(faltam, collapse = ", "), "; ver ", log)
	saidas
}

## só acompanha arquivos que já existem (insumos externos e etapas a montante no modo padrão): não roda nada
acompanha <- function(saidas, deps = NULL, origem = "") {
	force(deps)
	faltam <- saidas[!file.exists(saidas)]
	if (length(faltam)) stop("faltam ", length(faltam), " arquivo(s) acompanhado(s)", if (nzchar(origem)) paste0(" (", origem, ")") else "", ": ",
		paste(utils::head(faltam, 5), collapse = ", "), if (length(faltam) > 5) " ..." else "")
	saidas
}

# 2. memória -----------------------------------------------------------------------------------------------------------

## memória física livre, em GB (Windows: wmic ou PowerShell; NA se não der para medir)
mem_livre_gb <- function() {
	if (.Platform$OS.type != "windows") return(NA_real_)
	kb <- suppressWarnings(tryCatch(system2("wmic", c("OS", "get", "FreePhysicalMemory"), stdout = TRUE, stderr = FALSE), error = function(e) character(0)))
	kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", kb)))
	kb <- kb[!is.na(kb)]
	if (!length(kb)) {
		kb <- suppressWarnings(tryCatch(system2("powershell", c("-NoProfile", "-Command", shQuote("(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory")),
			stdout = TRUE, stderr = FALSE), error = function(e) character(0)))
		kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", kb)))
		kb <- kb[!is.na(kb)]
	}
	if (!length(kb)) NA_real_ else kb[1] / 1048576
}

## espera haver mem_gb + reserva de memória livre e pelo menos TSP_ESPERA_S segundos desde a última largada pesada
## (os processos demoram a ocupar a memória; sem o intervalo, dois alvos veriam a mesma folga). Reserva para os outros
## usuários da máquina: TSP_RESERVA_GB (padrão 60). Se não der para medir a memória, segue sem esperar.
espera_memoria <- function(mem_gb, marcador = "outputs/logs_pipeline/.ultima_largada") {
	if (is.null(mem_gb) || is.na(mem_gb) || mem_gb <= 0) return(invisible(NULL))
	reserva <- as.numeric(Sys.getenv("TSP_RESERVA_GB", "60"))
	espera_s <- as.numeric(Sys.getenv("TSP_ESPERA_S", "120"))
	dir.create(dirname(marcador), recursive = TRUE, showWarnings = FALSE)
	repeat {
		livre <- mem_livre_gb()
		if (is.na(livre)) break
		recente <- file.exists(marcador) && as.numeric(difftime(Sys.time(), file.mtime(marcador), units = "secs")) < espera_s
		if (livre >= mem_gb + reserva && !recente) break
		Sys.sleep(30 + stats::runif(1, 0, 30))
	}
	writeLines(format(Sys.time()), marcador)
	invisible(NULL)
}

# 3. grade a montante ----------------------------------------------------------------------------------------------------

REDES_A <- sprintf("A%02d", 0:26)
SEGMENTOS <- c("todos", "mulheres", "homens", "18 a 29", "30 a 44", "45 a 60", "com criança", "sem criança", "mulheres com criança",
	"mulheres sem criança", "homens com criança", "homens sem criança", "vínculo até 12 meses", "vínculo mais de 12 meses")
ROT_DOSE <- c(c30 = "cumulativa 30 min (+100 mil empregos)", c45 = "cumulativa 45 min (+100 mil empregos)", c60 = "cumulativa 60 min (+100 mil empregos)",
	c90 = "cumulativa 90 min (+100 mil empregos)", grav = "gravitacional (+0,1 no log)",
	c30p = "cumulativa 30 min (+10% da acessibilidade de antes)", c45p = "cumulativa 45 min (+10% da acessibilidade de antes)",
	c60p = "cumulativa 60 min (+10% da acessibilidade de antes)", c90p = "cumulativa 90 min (+10% da acessibilidade de antes)",
	gravabs = "gravitacional (+100 mil empregos ponderados)")

junta <- function(x) paste(x, collapse = "|")
separa <- function(x) if (is.na(x) || !nzchar(x)) character(0) else strsplit(x, "|", fixed = TRUE)[[1]]
linha <- function(...) {
	l <- list(...)
	padrao <- list(alvo = NA_character_, estagio = "", bloco = "", script = "", args = "", saidas = "", deps = "", controlador = "leve", mem_gb = 0,
		prioridade = 5, exige_nova = TRUE, r5r = FALSE, modo = "roda", pendente = FALSE, descricao = "")
	stopifnot(all(names(l) %in% names(padrao)))
	padrao[names(l)] <- l
	as.data.frame(padrao, stringsAsFactors = FALSE)
}

## Etapas a montante: uma linha por etapa, com o script, os argumentos, as saídas e as dependências.
## O modo de cada linha sai da variável de ambiente TSP_MONTANTE:
##   (vazio, padrão)  "acompanha": o alvo só acompanha as saídas que já existem; nenhum script a montante roda;
##   leve             roda os scripts que não roteiam (R/01–R/05, R/43, R/89, R/90, R/107, R/109, R/116);
##   tudo             roda também o roteamento com o r5r (R/42, R/110, R/111, R/112), que leva dias.
## Os caminhos externos vêm de R/00_config_local.R (dir_cad_arthur = pasta temp do Arthur; dir_cadunico_bruto).
grade_montante <- function(dir_cad_arthur, dir_cadunico_bruto, modo_montante = Sys.getenv("TSP_MONTANTE", "")) {
	stopifnot(modo_montante %in% c("", "leve", "tudo"))
	G <- list()
	ad <- function(...) G[[length(G) + 1L]] <<- linha(estagio = "1 insumos externos", modo = "acompanha", ...)
	mo <- function(..., r5r = FALSE) G[[length(G) + 1L]] <<- linha(estagio = "2 montante", r5r = r5r,
		modo = if (modo_montante == "tudo" || (modo_montante == "leve" && !r5r)) "roda" else "acompanha", ...)
	## insumos externos (nunca rodados aqui) ---------------------------------------------------------------------------
	ad(alvo = "ins_arthur_cad", saidas = junta(file.path(dir_cad_arthur, sprintf("cad_individ_%d.parquet", 2011:2023))),
		descricao = "CadÚnico processado pelo pipeline do Arthur (pasta dele, só leitura)")
	ad(alvo = "ins_arthur_trabalho", saidas = junta(file.path(dir_cad_arthur, sprintf("rais_workplaces_%d.parquet", 2011:2023))),
		descricao = "locais de trabalho geocodificados da RAIS (pasta do Arthur, só leitura)")
	ad(alvo = "ins_cadunico_bruto", saidas = junta(file.path(dir_cadunico_bruto, sprintf("cadunico_pes_%d12_harmonizado.parquet", 2011:2022))),
		descricao = "CadÚnico bruto harmonizado de dezembro (servidor de dados restritos)")
	ad(alvo = "ins_redes", saidas = junta(c("data/redes/catalogo_familia_a.csv", "data/redes/estacoes_familia_a.csv", "data/redes/checagens/pre_roteamento.csv",
		file.path("data/redes/gtfs", sprintf("trilho_%s.zip", REDES_A)))),
		descricao = "catálogo das 27 redes, GTFS de trilhos e checagens (R/34, R/35 e R/39, fora do DAG)")
	ad(alvo = "ins_apoio", saidas = junta(c("data/base/empregos_h3_2015.parquet", "data/base/celetista_pessoa_ano.parquet", "data/base/censo_h3_8.parquet",
		"data/base/acesso_cortes/hex_se.csv")),
		descricao = "empregos por hexágono, celetistas (R/85), Censo 2010 por H3-8 e hexágonos da Sé (R/61); scripts antigos fora do DAG")
	## R/01–R/05 ---------------------------------------------------------------------------------------------------------
	mo(alvo = "mon_insumos", script = "scr_r01", saidas = "data/insumos/_manifesto_insumos.csv", deps = "ins_arthur_cad",
		descricao = "R/01: copia os insumos do Arthur e grava o manifesto (md5)")
	mo(alvo = "mon_estacoes", script = "scr_r02", saidas = "data/base/estacoes.parquet", deps = "mon_insumos", descricao = "R/02: estações e datas de abertura")
	mo(alvo = "mon_rais_vinc", script = "scr_r04", saidas = junta(file.path("data/base/rais_vinc", sprintf("rais_vinc_%d.parquet", 2011:2025))),
		deps = "mon_insumos", exige_nova = FALSE, descricao = "R/04: vínculos RAIS dos CPFs do CadÚnico (lê a RAIS restrita; retomável)")
	mo(alvo = "mon_rais_pessoa_ano", script = "scr_r05", saidas = junta(c("outputs/ipca_dezembro.csv",
		file.path("data/base/rais_pessoa_ano", sprintf("year=%d", 2011:2025), "part-0.parquet"))), deps = "mon_rais_vinc",
		descricao = "R/05: desfechos RAIS por CPF e ano e o IPCA de dezembro")
	## R/89 ----------------------------------------------------------------------------------------------------------------
	mo(alvo = "mon_pessoa_ano", script = "scr_r89", args = "etapa=outcomes", saidas = junta(file.path("data/base/pessoa_ano", sprintf("pa_%d.parquet", 2011:2025))),
		deps = junta(c("mon_rais_vinc", "ins_apoio")), descricao = "R/89 etapa=outcomes: emprego mensal (máscaras) por pessoa e ano")
	mo(alvo = "mon_pop_base", script = "scr_r89", args = "etapa=populacoes", saidas = junta(file.path("data/base/pop_base", sprintf("pop_%d.parquet", 2011:2021))),
		deps = "ins_arthur_cad", descricao = "R/89 etapa=populacoes: população do CadÚnico por ano-base")
	## R/42 e R/43 (roteamento das 27 redes e ganhos) ---------------------------------------------------------------------------
	mo(alvo = "mon_r5_redes", script = "scr_r42", args = "etapa=montar", saidas = junta(file.path("data/redes/familia_a", REDES_A, "PRONTO.txt")),
		deps = junta(c("ins_redes", "mon_insumos")), exige_nova = FALSE, r5r = TRUE, descricao = "R/42 etapa=montar: redes R5 das 27 redes (r5r)")
	mo(alvo = "mon_r5_ttm", script = "scr_r42", args = "etapa=rotear", saidas = junta(file.path("data/redes/familia_a", sprintf("ttm_%s.parquet", REDES_A))),
		deps = junta(c("mon_r5_redes", "ins_apoio")), exige_nova = FALSE, r5r = TRUE, descricao = "R/42 etapa=rotear: tempos até 90 min nas 27 redes (r5r)")
	ad(alvo = "ins_acesso_sem_se", saidas = junta(c(file.path("data/base/acesso_sem_se", sprintf("acesso_%s.parquet", REDES_A)),
		file.path("data/base/acesso_sem_se_30", sprintf("acesso_%s.parquet", REDES_A)))), deps = "mon_r5_ttm",
		descricao = "acessibilidade cumulativa por rede sem as sedes da Sé (R/66 e R/87, fora do DAG; feitos das matrizes do R/42)")
	## R/90 ----------------------------------------------------------------------------------------------------------------
	mo(alvo = "mon_datas", script = "scr_r90", saidas = junta(file.path("data/base/datas", c("niveis.csv", "sorteios_pleno.parquet", "ganhos_c30.parquet",
		"ganhos_c45.parquet", "ganhos_c60.parquet", "ganhos_c90.parquet", "hex_meta.parquet"))),
		deps = junta(c("mon_estacoes", "ins_redes", "ins_acesso_sem_se", "ins_apoio")),
		descricao = "R/90: níveis e datas, sorteios das datas de serviço pleno, ganhos por nível e metadados dos hexágonos")
	## R/109–R/112, R/107 e R/116 --------------------------------------------------------------------------------------------
	mo(alvo = "mon_pares_longos", script = "scr_r109", saidas = "data/base/t3_pares_acima90.parquet",
		deps = junta(c("mon_datas", "mon_pop_base", "mon_rais_vinc", "mon_r5_ttm", "ins_arthur_trabalho")), descricao = "R/109: pares casa × trabalho acima de 90 min")
	mo(alvo = "mon_r5_longos", script = "scr_r110", saidas = junta(file.path("data/base/ttm_longos_t3", sprintf("ttm_longos_%s.parquet", REDES_A))),
		deps = junta(c("mon_pares_longos", "mon_r5_redes")), exige_nova = FALSE, r5r = TRUE, descricao = "R/110: tempos até 240 min dos pares longos (r5r)")
	mo(alvo = "mon_r5_a23_240", script = "scr_r111", saidas = junta(file.path("data/base/acesso_grav", c("j_min_A23_240.parquet", "t_pares2012_A23_240.parquet"))),
		deps = junta(c("mon_r5_redes", "mon_pop_base", "mon_pessoa_ano", "ins_apoio", "ins_arthur_trabalho")), exige_nova = FALSE, r5r = TRUE,
		descricao = "R/111: matriz completa da rede de 2012 até 240 min (r5r)")
	mo(alvo = "mon_r5_acesso_240", script = "scr_r112", saidas = junta(file.path("data/base/acesso_240", sprintf("jmin_%s.parquet", REDES_A))),
		deps = junta(c("mon_r5_redes", "mon_r5_a23_240", "ins_apoio")), exige_nova = FALSE, r5r = TRUE,
		descricao = "R/112: empregos alcançáveis por minuto até 240 min nas 27 redes (r5r)")
	mo(alvo = "mon_decaimento", script = "scr_r107", args = "limite=240 acesso=completo", saidas = "outputs/ahlfeldt_decaimento_240.csv",
		deps = junta(c("mon_r5_a23_240", "mon_r5_acesso_240", "mon_r5_ttm", "ins_apoio", "ins_arthur_trabalho")),
		descricao = "R/107 limite=240: decaimento da acessibilidade gravitacional (a parte C do script ainda lê o painel anual do R/92)")
	mo(alvo = "mon_ganhos_gcal", script = "scr_r116", saidas = "data/base/datas/ganhos_cgcal.parquet",
		deps = junta(c("mon_decaimento", "mon_r5_acesso_240", "mon_r5_a23_240", "mon_datas", "mon_pop_base", "mon_pessoa_ano", "ins_arthur_trabalho")),
		descricao = "R/116: ganhos de acessibilidade gravitacional calibrada por nível")
	do.call(rbind, G)
}

# 4. grade da especificação final ----------------------------------------------------------------------------------------

## há contraste gravado quando o processo estima os dois lados de algum par (ou os três grupos de idade)
PARES_CONTRASTE <- list(c(2, 3), c(5, 4), c(6, 4), c(7, 8), c(9, 10), c(11, 12), c(13, 14))
contrastes_de <- function(seg) {
	seg <- as.integer(seg)
	out <- character(0)
	for (p in PARES_CONTRASTE) if (all(p %in% seg)) out <- c(out, paste(SEGMENTOS[p[1]], "−", SEGMENTOS[p[2]]))
	if (all(9:12 %in% seg)) out <- c(out, "(mulheres com − sem criança) − (homens com − sem criança)")
	if (all(4:6 %in% seg)) out <- c(out, "idade: teste conjunto de igualdade (3 grupos)")
	out
}
saidas_area <- function(nivel, sufixo, seg = 1:12) sprintf("outputs/final/area_%s%s_%s.csv", nivel, sufixo, c("segmentos", if (length(contrastes_de(seg))) "contrastes"))
saidas_par <- function(nivel, sufixo, seg, descricao = TRUE) sprintf("outputs/final/par_%s%s_%s.csv", nivel, sufixo,
	c("segmentos", if (descricao) "descricao", if (length(contrastes_de(seg))) "contrastes"))
saidas_iptw <- function(desenho, nivel, sufixo) sprintf("outputs/final/iptw_%s_%s%s_%s.csv", desenho, nivel, sufixo, c("segmentos", "contrastes", "balanco"))

## Uma linha por processo (alvo). Os nomes de saída e os argumentos são os das rodadas feitas com os scripts soltos
## (trabalhos_final_*.txt de 24/11/2026), para que aquelas saídas já sejam os resultados esperados.
## As colunas dose/modelo/seg/fe/nivel descrevem a especificação e alimentam o mapa de resultados.
grade_final <- function(pendentes = identical(Sys.getenv("TSP_PENDENTES"), "1")) {
	G <- list()
	E <- list()
	ad <- function(alvo, ..., esp = list()) {
		G[[length(G) + 1L]] <<- linha(alvo = alvo, ...)
		E[[alvo]] <<- esp
	}
	comuns_area <- c("mon_datas", "mon_pop_base", "mon_pessoa_ano", "mon_rais_vinc", "mon_rais_pessoa_ano", "ins_arthur_cad", "prep_criancas", "scr_fun_evento")
	comuns_par <- c(comuns_area, "mon_r5_ttm", "mon_r5_longos", "mon_pares_longos", "ins_arthur_trabalho", "scr_monta_par")
	cache <- "data/base/explora/ev_cache"
	## preparação -------------------------------------------------------------------------------------------------------
	ad("prep_criancas", estagio = "3 preparação", script = "scr_r117", saidas = junta(sprintf("data/base/explora/criancas/fam_crianca_%d.rds", 2011:2022)),
		deps = "ins_cadunico_bruto", exige_nova = FALSE, mem_gb = 20, prioridade = 1, descricao = "famílias com criança de 0 a 5 anos (CadÚnico bruto)")
	## bases por evento -------------------------------------------------------------------------------------------------
	ad("base_area", estagio = "4 bases por evento", script = "scr_r118", saidas = junta(file.path(cache, c("area_celulas.parquet", "eventos_area.csv"))),
		deps = junta(comuns_area), controlador = "pesado", mem_gb = 60, prioridade = 1, descricao = "base por evento da área (células H3-9 × grupo × evento)")
	ad("base_par", estagio = "4 bases por evento", script = "scr_r119", saidas = junta(file.path(cache, c("par_celulas.parquet", "par_pares.parquet", "eventos.csv",
		"par_checagem.csv"))), deps = junta(comuns_par), controlador = "pesado", mem_gb = 100, prioridade = 1,
		descricao = "base por evento do tempo de viagem (pares casa × trabalho × grupo × evento)")
	## B&H de área (bloco A) ----------------------------------------------------------------------------------------------
	dep_bh_area <- junta(c("base_area", "mon_datas", "mon_ganhos_gcal", "scr_fun_bh"))
	bh_area <- function(alvo, nivel, doses, sufixo, prio, mem) ad(alvo, estagio = "5 estimações", bloco = "A", script = "scr_r120",
		args = paste0("nivel=", nivel, " doses=", paste(doses, collapse = ","), " perm=199", if (nzchar(sufixo)) paste0(" sufixo=", sufixo) else ""),
		saidas = junta(saidas_area(nivel, sufixo)), deps = dep_bh_area, controlador = "pesado", mem_gb = mem, prioridade = prio,
		descricao = sprintf("B&H de área, %s, doses %s", nivel, paste(doses, collapse = ", ")),
		esp = list(tipo = "bh_area", nivel = nivel, doses = doses, seg = 1:12, fe = "casa7"))
	bh_area("bh_area_h39_a", "h39", c("c60", "grav"), "_a", 2, 40)
	bh_area("bh_area_h39_b", "h39", c("c30", "c45", "c90"), "_b", 3, 40)
	bh_area("bh_area_h38", "h38", c("c30", "c45", "c60", "c90", "grav"), "", 3, 30)
	## a mesma acessibilidade na outra unidade: cumulativas em % e gravitacional absoluta (24/11/2026)
	bh_area("bh_area_h39_pct", "h39", c("c30p", "c45p", "c60p", "c90p"), "_pct", 3, 40)
	bh_area("bh_area_h38_pct", "h38", c("c30p", "c45p", "c60p", "c90p"), "_pct", 3, 30)
	bh_area("bh_area_h39_gravabs", "h39", "gravabs", "_gravabs", 3, 40)
	bh_area("bh_area_h38_gravabs", "h38", "gravabs", "_gravabs", 3, 30)
	## B&H de tempo de viagem (blocos B e C) --------------------------------------------------------------------------------
	dep_bh_par <- junta(c("base_par", "mon_datas", "mon_ganhos_gcal", "scr_fun_bh"))
	bh_par <- function(alvo, nivel, seg, sufixo, prio, mem, modelos = c("A", "B"), extra = "", bloco = "B e C", fe = "casa7trab7", tmin = 0, estagio = "5 estimações")
		ad(alvo, estagio = estagio, bloco = bloco, script = "scr_r121",
			args = paste0("nivel=", nivel, " seg=", paste(seg, collapse = ";"), if (!identical(modelos, c("A", "B"))) paste0(" modelos=", paste(modelos, collapse = ",")) else "",
				" perm=199", extra, " sufixo=", sufixo),
			saidas = junta(saidas_par(nivel, sufixo, seg)), deps = dep_bh_par, controlador = "pesado", mem_gb = mem, prioridade = prio,
			descricao = sprintf("B&H de tempo de viagem, %s, modelos %s, segmentos %s", nivel, paste(modelos, collapse = " e "), paste(SEGMENTOS[seg], collapse = "; ")),
			esp = list(tipo = "bh_par", nivel = nivel, modelos = modelos, seg = seg, fe = fe, tmin = tmin))
	## "todos" em dois processos, um por modelo: com os dois modelos juntos a rodada esgota a memória (24/11/2026)
	bh_par("bh_par_h39_todos_A", "h39", 1, "_todos_A", 2, 150, modelos = "A")
	bh_par("bh_par_h39_todos_B", "h39", 1, "_todos_B", 2, 150, modelos = "B")
	bh_par("bh_par_h39_sexo", "h39", 2:3, "_sexo", 2, 80)
	bh_par("bh_par_h39_crsexo", "h39", 9:12, "_crsexo", 2, 80)
	bh_par("bh_par_h39_idade", "h39", 4:6, "_idade", 3, 80)
	bh_par("bh_par_h39_cr", "h39", 7:8, "_cr", 3, 80)
	bh_par("bh_par_h39_vinc", "h39", 13:14, "_vinc", 3, 80)
	bh_par("bh_par_h38_a", "h38", c(1:3, 9:12), "_a", 4, 60)
	bh_par("bh_par_h38_b", "h38", c(4:8, 13:14), "_b", 4, 60)
	## todos os trajetos juntos, sem a divisão em 90 min: em minutos (U), em % do tempo antes (P) e trajeto + acesso (UB),
	## em quatro processos por memória (24/11/2026)
	for (u in list(list("todos", 1), list("sexo", 2:3), list("crsexo", 9:12), list("resto", c(4:8, 13:14))))
		bh_par(paste0("bh_par_h39_unico_", u[[1]]), "h39", u[[2]], paste0("_unico_", u[[1]]), 3, 90, modelos = c("U", "P", "UB"), bloco = "B e C")
	## robustez (bloco E) -----------------------------------------------------------------------------------------------------
	ad("rob_area", estagio = "5 estimações", bloco = "E", script = "scr_r122", args = "parte=area perm=199", saidas = "outputs/final/robustez_area.csv",
		deps = dep_bh_area, controlador = "pesado", mem_gb = 40, prioridade = 5, descricao = "robustez da área: R1, R2, R3 e R6",
		esp = list(tipo = "rob", parte = "area"))
	ad("rob_par", estagio = "5 estimações", bloco = "E", script = "scr_r122", args = "parte=par perm=199", saidas = "outputs/final/robustez_par.csv",
		deps = dep_bh_par, controlador = "pesado", mem_gb = 170, prioridade = 5, descricao = "robustez do tempo de viagem: R1, R2, R3, R6 e R8",
		esp = list(tipo = "rob", parte = "par"))
	bh_par("rob_par_tempo0", "h39", c(1, 2, 9), "_tempo0", 5, 80, modelos = "A", extra = " tmin=-1", bloco = "E", tmin = -1)
	## variações de efeito fixo (bloco F) -------------------------------------------------------------------------------------
	for (fe in c("ev", "casa8")) ad(paste0("fe_area_", fe), estagio = "5 estimações", bloco = "F", script = "scr_r120",
		args = sprintf("nivel=h39 doses=c60,grav perm=199 fe=%s sufixo=_fe_%s", fe, fe), saidas = junta(saidas_area("h39", paste0("_fe_", fe))), deps = dep_bh_area,
		controlador = "pesado", mem_gb = 40, prioridade = 6, descricao = sprintf("B&H de área com efeito fixo %s", fe),
		esp = list(tipo = "bh_area", nivel = "h39", doses = c("c60", "grav"), seg = 1:12, fe = fe))
	for (fe in c("ev", "casa7", "casa8trab8")) bh_par(paste0("fe_par_", fe), "h39", c(1:3, 9:12), paste0("_fe_", fe), 6, 80, modelos = "A",
		extra = paste0(" fe=", fe), bloco = "F", fe = fe)
	## IPTW (bloco D) ---------------------------------------------------------------------------------------------------------
	dep_iptw_area <- junta(c(comuns_area, "mon_ganhos_gcal", "scr_fun_iptw"))
	## todos os cortes num processo por nível e janela (a base de pessoas é a mesma; 24/11/2026)
	for (nv in c("ind", "h38")) for (jn in c("pos", "pre")) for (cs in list(c("30", "45", "60", "90", "grav"))) {
		suf <- paste0("_", jn, "_todos")
		ad(paste0("iptw_area_", nv, suf), estagio = "5 estimações", bloco = "D", script = "scr_r123",
			args = sprintf("nivel=%s janelas=%s cortes=%s sufixo=%s", nv, jn, paste(cs, collapse = ","), suf), saidas = junta(saidas_iptw("area", nv, suf)),
			deps = dep_iptw_area, controlador = "pesado", mem_gb = if (nv == "ind") 100 else 80, prioridade = 4,
			descricao = sprintf("IPTW de área, %s, janela %s, cortes %s", nv, jn, paste(cs, collapse = ", ")),
			esp = list(tipo = "iptw_area", nivel = nv, janela = jn, cortes = cs))
	}
	for (nv in c("ind", "h38")) ad(paste0("iptw_par_", nv), estagio = "5 estimações", bloco = "D", script = "scr_r124",
		args = sprintf("nivel=%s janelas=pos,pre", nv), saidas = junta(saidas_iptw("par", nv, "")), deps = junta(c(comuns_par, "mon_ganhos_gcal", "scr_fun_iptw")),
		controlador = "pesado", mem_gb = 100, prioridade = 4, descricao = sprintf("IPTW de tempo de viagem, %s", nv),
		esp = list(tipo = "iptw_par", nivel = nv))
	## controles futuros na mesma faixa (bloco G) -------------------------------------------------------------------------------
	for (pt in c("area", "par")) for (nv in c("h39", "h38")) ad(paste0("futuros_", pt, "_", nv), estagio = "5 estimações", bloco = "G", script = "scr_r127",
		args = sprintf("parte=%s nivel=%s%s", pt, nv, if (pt == "area") " cortes=c30,c45,c60,c90,grav" else ""),
		saidas = junta(sprintf("outputs/final/futuros_%s_%s_%s.csv", pt, nv, c("segmentos", "contrastes", "amostra"))),
		deps = if (pt == "area") dep_bh_area else dep_bh_par, controlador = "pesado", mem_gb = if (pt == "area") 30 else 60, prioridade = 4,
		descricao = sprintf("controles futuros na mesma faixa, %s, %s", if (pt == "area") "área" else "tempo de viagem", nv),
		esp = list(tipo = "futuros", parte = pt, nivel = nv))
	## pendentes: R4 (curva mês a mês) e R5 (endereço-base g−1 a g−4), com bases próprias. Código pronto, ainda não rodado.
	## Só entram no DAG com TSP_PENDENTES=1; no mapa de resultados aparecem sempre, marcados como pendentes.
	h24 <- paste(1:24, collapse = ",")
	var <- c(list(list(rot = "r4", base = "_mensal", arg = paste0("horizontes=", h24), txt = "curva mês a mês (h = 1 a 24)")),
		lapply(1:4, function(k) list(rot = "r5", base = paste0("_lag", k), arg = paste0("lag=", k), txt = sprintf("endereço-base g−%d", k))))
	for (v in var) {
		ba <- paste0("base_area", v$base)
		bp <- paste0("base_par", v$base)
		hz <- if (v$rot == "r4") paste0(" horizontes=", h24) else ""
		ad(ba, estagio = "4 bases por evento", bloco = "E", script = "scr_r118", args = paste0(v$arg, " base=", v$base),
			saidas = junta(file.path(cache, sprintf(c("area_celulas%s.parquet", "eventos_area%s.csv"), v$base))), deps = junta(comuns_area), controlador = "pesado",
			mem_gb = 60, prioridade = 8, pendente = TRUE, descricao = paste("base da área,", v$txt))
		ad(bp, estagio = "4 bases por evento", bloco = "E", script = "scr_r119", args = paste0(v$arg, " base=", v$base),
			saidas = junta(file.path(cache, sprintf(c("par_celulas%s.parquet", "par_pares%s.parquet", "eventos%s.csv", "par_checagem%s.csv"), v$base))),
			deps = junta(comuns_par), controlador = "pesado", mem_gb = 100, prioridade = 8, pendente = TRUE, descricao = paste("base do tempo de viagem,", v$txt))
		suf <- paste0("_", v$rot, v$base)
		ad(paste0(v$rot, "_area", v$base), estagio = "5 estimações", bloco = "E", script = "scr_r120",
			args = paste0("nivel=h39 doses=c60,grav perm=199 seg=1;2;9 conjuntos=emprego,entrada base=", v$base, hz, " sufixo=", suf),
			saidas = junta(saidas_area("h39", suf, c(1, 2, 9))), deps = junta(c(ba, "mon_datas", "mon_ganhos_gcal", "scr_fun_bh")), controlador = "pesado", mem_gb = 40,
			prioridade = 9, pendente = TRUE, descricao = paste("B&H de área,", v$txt),
			esp = list(tipo = "bh_area", nivel = "h39", doses = c("c60", "grav"), seg = c(1, 2, 9), fe = "casa7", variante = v$txt))
		ad(paste0(v$rot, "_par", v$base), estagio = "5 estimações", bloco = "E", script = "scr_r121",
			args = paste0("nivel=h39 seg=1;2;9 modelos=A perm=199 desfechos=demissao,pedido base=", v$base, hz, " sufixo=", suf),
			saidas = junta(saidas_par("h39", suf, c(1, 2, 9), descricao = FALSE)), deps = junta(c(bp, "mon_datas", "mon_ganhos_gcal", "scr_fun_bh")),
			controlador = "pesado", mem_gb = 80, prioridade = 9, pendente = TRUE, descricao = paste("B&H de tempo de viagem,", v$txt),
			esp = list(tipo = "bh_par", nivel = "h39", modelos = "A", seg = c(1, 2, 9), fe = "casa7trab7", tmin = 0, variante = v$txt, desfechos = c("demissao", "pedido")))
	}
	G <- do.call(rbind, G)
	attr(G, "esp") <- E
	if (!pendentes) {
		fica <- !G$pendente
		E <- attr(G, "esp")
		G <- G[fica, , drop = FALSE]
		attr(G, "esp") <- E
	}
	G
}

# 5. mapa de resultados ----------------------------------------------------------------------------------------------------

COLS <- list(
	bh_area_seg = "efeito, ep, p_perm, p_normal, efeito_1dp, dose_media, dose_dp, media_y, pessoas, unidades, perm",
	bh_area_ct = "diferenca, ep, p_normal, p_perm (teste conjunto da idade: wald, gl, p_normal)",
	bh_par_seg = "efeito, ep, p_perm, p_normal, media_y, vinculos, unidades, perm",
	bh_par_ct = "diferenca, ep, p_normal, p_perm (teste conjunto da idade: wald, gl, p_normal)",
	bh_par_desc = "vinculos, traj_realizado_min, traj_recentrado_min, acesso_realizado_log, acesso_recentrado_log",
	iptw_seg = "att, ep, p, tratados, controles, media_tratados",
	iptw_ct = "diferenca, ep, p",
	iptw_bal = "tratados, controles, ess_controles, smd_max_antes, smd_max_depois, eventos",
	rob = "efeito, ep, p_perm, p_normal, unidades",
	fut_seg = "efeito, ep, p, tratados, controles, media_tratados, media_controles",
	fut_ct = "diferenca, ep, p (teste conjunto da idade: wald, gl, p)",
	fut_am = "unidades (hexagonos ou pares), pessoas ou vinculos, ganho_medio, por evento × faixa × status (T = 1 tratado, 0 controle futuro)")
OC_AREA <- "emprego_h, entrada_h, empregado_h e meses_h (permanência), salario_h; h = 6, 12, 18, 24"
OC_B <- "qualquer_h, demissao_h, sem_justa_h, justa_h, pedido_h, empregado_h, meses_h; h = 6, 12, 18, 24"
OC_C <- "ped_curto_h, ped_naocurto_h, ped_semloc_h, ped_semnovo_h, ped_salmaior_h, ped_salnaomaior_h, ped_novo3_h, ped_novo4a12_h; h = 6, 12, 18, 24"
ROT_FE <- c(casa7 = "casa H3-7 × evento (principal)", casa8 = "casa H3-8 × evento", ev = "só evento", casa7trab7 = "casa e trabalho H3-7 × evento (principal)",
	casa8trab8 = "casa e trabalho H3-8 × evento")
ROT_NIVEL <- c(h39 = "H3-9 (equivale ao painel individual)", h38 = "agregado H3-8", ind = "individual")
ROT_BLOCO <- c(A = "A. Acessibilidade a partir de casa (B&H)", B = "B. Tempo de viagem ao próprio emprego (B&H)", C = "C. Quem pediu demissão: destinos e descrição dos ganhos",
	D = "D. IPTW com a data exata", E = "E. Robustez", F = "F. Variações de efeito fixo", G = "G. Controles futuros na mesma faixa (DiD empilhado)")
TESTES_ROB <- list(
	area = c("principal", "R1 outras aberturas", "R2 efeito fixo H3-8", "R3 sem um evento", "R6 faixas de ganho em % (abaixo de 1% = zero)"),
	par = c("principal (modelo A)", "R1 outras aberturas", "R2 efeitos fixos H3-8", "R3 sem um evento", "R6 faixas de ganho em minutos",
		"R6 faixas de ganho em % (abaixo de 1% = zero)", "R6 quintis do ganho", "R6 interação com o tempo antes", "R6 dose percentual",
		"R8 dose única em minutos (sem divisão em 90 min)", "R8 dose única percentual (sem divisão em 90 min)"))

## Uma linha por especificação × tabela: bloco, desenho, estimador, unidade, efeito fixo, dose/modelo, segmento ou
## contraste, arquivo, filtro (expressão sobre as colunas do arquivo), colunas do resultado, desfechos e alvo.
## Em todos os arquivos, janela == "pos" é o efeito e janela == "pre" é o placebo.
mapa_resultados <- function(G = grade_final(pendentes = TRUE)) {
	E <- attr(G, "esp")
	M <- list()
	ad <- function(g, bloco, desenho, estimador, unidade, fe, chave, quem, tabela, arquivo, filtro, colunas, desfechos) M[[length(M) + 1L]] <<- data.frame(
		bloco = bloco, desenho = desenho, estimador = estimador, unidade = unidade, efeito_fixo = fe, dose_ou_modelo = chave, segmento_ou_contraste = quem,
		tabela = tabela, arquivo = arquivo, filtro = filtro, colunas_resultado = colunas, desfechos = desfechos, alvo = g$alvo,
		estado = if (g$pendente) "pendente (TSP_PENDENTES=1)" else "padrão", stringsAsFactors = FALSE)
	arq <- function(g, parte) grep(paste0("_", parte, "[.]csv$"), separa(g$saidas), value = TRUE)
	for (i in seq_len(nrow(G))) {
		g <- G[i, ]
		e <- E[[g$alvo]]
		if (is.null(e) || !length(e)) next
		var <- if (is.null(e$variante)) "" else paste0("; ", e$variante)
		if (e$tipo == "bh_area") {
			bl <- if (g$bloco == "A") "A" else g$bloco
			for (d in e$doses) {
				for (s in e$seg) ad(g, bl, "área", "B&H", paste0(ROT_NIVEL[[e$nivel]], var), ROT_FE[[e$fe]], ROT_DOSE[[d]], SEGMENTOS[s], "segmentos", arq(g, "segmentos"),
					sprintf("dose == \"%s\" & segmento == \"%s\" & janela == \"pos\" (placebo: \"pre\")", ROT_DOSE[[d]], SEGMENTOS[s]), COLS$bh_area_seg,
					if (is.null(e$variante)) OC_AREA else "emprego_h e entrada_h")
				for (ct in contrastes_de(e$seg)) ad(g, bl, "área", "B&H", paste0(ROT_NIVEL[[e$nivel]], var), ROT_FE[[e$fe]], ROT_DOSE[[d]], ct, "contrastes", arq(g, "contrastes"),
					sprintf("dose == \"%s\" & contraste == \"%s\" & janela == \"pos\" (placebo: \"pre\")", ROT_DOSE[[d]], ct), COLS$bh_area_ct, OC_AREA)
			}
		}
		if (e$tipo == "bh_par") {
			amostra <- if (e$tmin < 0) "; inclui pares com tempo zero (R7)" else ""
			blocos <- if (g$bloco == "B e C") c("B", "C") else g$bloco
			for (bl in blocos) for (md in e$modelos) {
				if (bl == "C" && !md %in% c("B", "UB")) next
				chave <- switch(md, A = "modelo A (só trajeto)", B = "modelo B (trajeto e acesso)", U = "modelo U (trajeto sem divisão em 90 min, por 10 min)",
					P = "modelo P (trajeto sem divisão em 90 min, por 10% do tempo)", UB = "modelo UB (trajeto e acesso sem divisão em 90 min)")
				termos <- switch(md, A = "termo: \"trajeto, até 90\", \"trajeto, acima de 90\" ou \"trajeto: até 90 − acima de 90\"",
					B = "termo: \"trajeto, até 90\", \"trajeto, acima de 90\", \"acesso, até 90\", \"acesso, acima de 90\" ou \"trajeto: até 90 − acima de 90\"",
					U = "termo: \"trajeto, sem divisão (por 10 min)\"", P = "termo: \"trajeto, sem divisão (por 10% do tempo)\"",
					UB = "termo: \"trajeto, sem divisão (por 10 min)\" ou \"acesso, sem divisão\"")
				oc <- if (!is.null(e$desfechos)) paste0(paste0(e$desfechos, "_h", collapse = ", "), if (is.null(e$variante)) "" else paste0(" (", e$variante, ")")) else
					if (bl == "C") OC_C else if (bl == "B") OC_B else paste(OC_B, OC_C, sep = "; ")
				for (s in e$seg) ad(g, bl, "tempo de viagem", "B&H", paste0(ROT_NIVEL[[e$nivel]], amostra, var), ROT_FE[[e$fe]], chave, SEGMENTOS[s], "segmentos", arq(g, "segmentos"),
					sprintf("modelo == \"%s\" & segmento == \"%s\" & janela == \"pos\" (placebo: \"pre\"); %s", md, SEGMENTOS[s], termos), COLS$bh_par_seg, oc)
				for (ct in contrastes_de(e$seg)) ad(g, bl, "tempo de viagem", "B&H", paste0(ROT_NIVEL[[e$nivel]], amostra, var), ROT_FE[[e$fe]], chave, ct, "contrastes",
					arq(g, "contrastes"), sprintf("modelo == \"%s\" & contraste == \"%s\" & janela == \"pos\" (placebo: \"pre\"); termo como nos segmentos", md, ct),
					COLS$bh_par_ct, oc)
			}
			if (length(arq(g, "descricao")) && "C" %in% blocos) for (s in e$seg) ad(g, "C", "tempo de viagem", "descrição dos ganhos", ROT_NIVEL[[e$nivel]], "", "ganhos realizados e recentrados",
				SEGMENTOS[s], "descricao", arq(g, "descricao"), sprintf("segmento == \"%s\" & janela == \"pos\"; h; trajeto (\"até 90\"/\"acima de 90\"); grupo (pedido, nao_pediu ou destino ped_*)",
					SEGMENTOS[s]), COLS$bh_par_desc, "ganho de quem pediu demissão até h, por destino, contra quem não pediu")
		}
		if (e$tipo == "rob") for (tt in TESTES_ROB[[e$parte]]) ad(g, "E", if (e$parte == "area") "área" else "tempo de viagem", "B&H", ROT_NIVEL[["h39"]],
			if (grepl("^R2", tt)) "H3-8 no lugar de H3-7" else if (e$parte == "area") ROT_FE[["casa7"]] else ROT_FE[["casa7trab7"]],
			if (e$parte == "area") "cumulativa 60 min e gravitacional (coluna dose: c60, grav)" else "modelo A (só trajeto)", "todos; mulheres; mulheres com criança", "robustez",
			g$saidas, sprintf("teste == \"%s\" & janela == \"pos\" (placebo: \"pre\"); segmento; termo%s", tt, if (grepl("^R3", tt)) "; evento_fora" else ""), COLS$rob,
			if (e$parte == "area") "emprego_h e entrada_h; h = 6, 12, 18, 24" else "demissao_h, sem_justa_h, pedido_h, empregado_h; h = 6, 12, 18, 24")
		if (e$tipo == "iptw_area") for (cc in e$cortes) {
			rot <- if (cc == "grav") "gravitacional calibrada" else paste("cumulativa", cc, "min")
			filtro <- sprintf("corte == \"%s\" & janela == \"%s\"; faixa (f1a5, f5a20, f20mais = ganho de 1–5%%, 5–20%%, 20%% ou mais contra menos de 1%%)", cc, e$janela)
			jan <- if (e$janela == "pos") "efeito (pós)" else "placebo (pré)"
			ad(g, "D", "área", "IPTW duplamente robusto", ROT_NIVEL[[e$nivel]], "efeitos de evento", paste0(rot, "; ", jan), "12 segmentos (coluna segmento)", "segmentos",
				arq(g, "segmentos"), filtro, COLS$iptw_seg, OC_AREA)
			ad(g, "D", "área", "IPTW duplamente robusto", ROT_NIVEL[[e$nivel]], "efeitos de evento", paste0(rot, "; ", jan), "6 contrastes entre segmentos (coluna contraste)",
				"contrastes", arq(g, "contrastes"), filtro, COLS$iptw_ct, OC_AREA)
			ad(g, "D", "área", "balanço do escore", ROT_NIVEL[[e$nivel]], "", paste0(rot, "; ", jan), if (e$nivel == "ind") "amostra toda" else "por segmento", "balanco",
				arq(g, "balanco"), filtro, COLS$iptw_bal, "diferença padronizada máxima antes e depois dos pesos")
		}
		if (e$tipo == "iptw_par") {
			filtro <- "janela (\"pos\" efeito, \"pre\" placebo); trajeto (\"até 90\"/\"acima de 90\"); faixa (f1a5, f5a10, f10mais = ganho de 1–5, 5–10, 10 min ou mais contra menos de 1 min)"
			ad(g, "D", "tempo de viagem", "IPTW duplamente robusto", ROT_NIVEL[[e$nivel]], "efeitos de evento", "faixas de ganho de tempo", "14 segmentos (coluna segmento)",
				"segmentos", arq(g, "segmentos"), filtro, COLS$iptw_seg, OC_B)
			ad(g, "D", "tempo de viagem", "IPTW duplamente robusto", ROT_NIVEL[[e$nivel]], "efeitos de evento", "faixas de ganho de tempo",
				"7 contrastes entre segmentos (coluna contraste)", "contrastes", arq(g, "contrastes"), filtro, COLS$iptw_ct, OC_B)
			ad(g, "D", "tempo de viagem", "balanço do escore", ROT_NIVEL[[e$nivel]], "", "faixas de ganho de tempo", if (e$nivel == "ind") "amostra toda" else "por segmento",
				"balanco", arq(g, "balanco"), filtro, COLS$iptw_bal, "diferença padronizada máxima antes e depois dos pesos")
		}
		if (e$tipo == "futuros") {
			area <- e$parte == "area"
			des <- if (area) "área" else "tempo de viagem"
			chave <- if (area) "faixas de ganho de acessibilidade (cortes 30, 45, 60, 90 e gravitacional; coluna corte)" else "faixas de ganho de tempo (coluna trajeto: até 90 / acima de 90)"
			fx <- if (area) "faixa (todas, f1a5, f5a20, f20mais)" else "faixa (todas, f1a5, f5a10, f10mais)"
			filtro <- sprintf("esquema (todos, sexo, idade, crianca, sexo_crianca%s) & segmento & %s & janela == \"pos\" (placebo: \"pre\")%s", if (area) "" else ", vinculo", fx,
				if (area) "; corte; conjunto" else "; trajeto")
			oc <- if (area) OC_AREA else OC_B
			ad(g, "G", des, "DiD empilhado, controles futuros na mesma faixa", ROT_NIVEL[[e$nivel]], "evento × faixa (× segmento)", chave, "segmentos de cada esquema", "segmentos",
				arq(g, "segmentos"), filtro, COLS$fut_seg, oc)
			ad(g, "G", des, "DiD empilhado, controles futuros na mesma faixa", ROT_NIVEL[[e$nivel]], "evento × faixa (× segmento)", chave, "contrastes dentro de cada esquema",
				"contrastes", arq(g, "contrastes"), sub("segmento & ", "contraste & ", filtro), COLS$fut_ct, oc)
			ad(g, "G", des, "amostra do DiD empilhado", ROT_NIVEL[[e$nivel]], "", chave, "tratados e controles futuros por evento e faixa", "amostra", arq(g, "amostra"),
				"ev; faixa; T; janela", COLS$fut_am, "contagens e ganho médio")
		}
	}
	do.call(rbind, M)
}

## grava outputs/final/mapa_resultados.csv (uma linha por especificação × tabela) e notas/mapa_resultados.md (por bloco,
## uma linha por arquivo). Não depende de nenhum dado: só da grade.
grava_mapa <- function(G = grade_final(pendentes = TRUE), arq_csv = "outputs/final/mapa_resultados.csv", arq_md = "notas/mapa_resultados.md") {
	M <- mapa_resultados(G)
	dir.create(dirname(arq_csv), recursive = TRUE, showWarnings = FALSE)
	utils::write.csv(M, arq_csv, row.names = FALSE, fileEncoding = "UTF-8")
	une <- function(x) paste(unique(x), collapse = "; ")
	L <- c("# Mapa de resultados da especificação final", "",
		"Gerado automaticamente a partir da grade de especificações (`grade_final()` em `R/pipeline_final.R`) pelo alvo `mapa_resultados` do pipeline;",
		"não editar à mão. Para regerar sem rodar o pipeline:", "",
		"```", "Rscript -e \"source('R/pipeline_final.R', encoding = 'UTF-8'); grava_mapa()\"", "```", "",
		sprintf("A versão completa, com uma linha por especificação (%d linhas), está em `outputs/final/mapa_resultados.csv`.", nrow(M)), "",
		"Como ler:", "",
		"- Todas as estimativas usam a data exata de serviço pleno: cada data é um evento (19 eventos), a referência é o mês anterior ao serviço pleno e os",
		"  horizontes são de 6, 12, 18 e 24 meses (sufixo `_h` dos desfechos).",
		"- Em todos os arquivos, `janela == \"pos\"` é o efeito e `janela == \"pre\"` é o placebo (referência 25 meses antes da primeira abertura).",
		"- B&H: `efeito` e `ep` em pontos percentuais (salário: 100 × log); `p_perm` = p por permutação (199 sorteios); `p_normal` = p com o EP de cluster",
		"  (célula de 5 km). Nos contrastes, o p de referência é o `p_normal` (EP conjunto); o `p_perm` testa a nula de nenhum efeito nos dois grupos.",
		"- IPTW: `att`, `ep` e `p` (normal, EP de cluster por célula de 5 km); o balanço está nos arquivos `_balanco.csv`.",
		"- Unidade H3-9 = células H3-9 × grupo × evento com pesos = pessoas ou vínculos (mesmo coeficiente, EP e p do painel individual).",
		"- Segmentos: todos; mulheres; homens; 18 a 29; 30 a 44; 45 a 60; com criança; sem criança; mulheres com criança; mulheres sem criança; homens com",
		"  criança; homens sem criança; e, no tempo de viagem, vínculo até 12 meses e vínculo mais de 12 meses.",
		"- Os arquivos ficam em `outputs/final/` (fora do git). As linhas marcadas como pendentes só são produzidas com `TSP_PENDENTES=1`.", "")
	chave <- paste(M$arquivo, M$tabela, M$bloco)
	for (bl in names(ROT_BLOCO)) {
		Mb <- M[M$bloco == bl, ]
		if (!nrow(Mb)) next
		L <- c(L, paste("##", ROT_BLOCO[[bl]]), "",
			"| Arquivo | Tabela | Desenho e estimador | Unidade | Efeito fixo | Dose ou modelo | Segmentos ou contrastes | Colunas do resultado | Desfechos | Alvo | Estado |",
			"|---|---|---|---|---|---|---|---|---|---|---|")
		for (kk in unique(chave[M$bloco == bl])) {
			x <- Mb[paste(Mb$arquivo, Mb$tabela, Mb$bloco) == kk, ]
			L <- c(L, sprintf("| `%s` | %s | %s, %s | %s | %s | %s | %s | %s | %s | `%s` | %s |", sub("^outputs/final/", "", x$arquivo[1]), x$tabela[1], x$desenho[1], x$estimador[1],
				une(x$unidade), une(x$efeito_fixo), une(x$dose_ou_modelo), une(x$segmento_ou_contraste), une(x$colunas_resultado), une(x$desfechos), x$alvo[1], x$estado[1]))
		}
		filtros <- Mb$filtro
		for (cc in c("segmento", "contraste", "dose", "teste", "corte")) filtros <- sub(sprintf("%s == \"[^\"]*\"", cc), sprintf("%s == \"<%s>\"", cc, cc), filtros)
		filtros <- unique(filtros)
		L <- c(L, "", "Como filtrar (colunas-chave):", "", paste0("- `", utils::head(filtros, 8), "`"), "")
	}
	L <- c(L, "## Tabelas reunidas por bloco", "",
		"O alvo `tab_final` (R/125) empilha os arquivos de cada bloco, com a coluna `arquivo` de origem, em `outputs/final/tabelas/bloco_<letra>_*.csv`, e grava",
		"`outputs/final/tabelas/resumo_principais.md` com os desfechos principais. A checagem de todas as saídas fica em `outputs/final/checagem_pipeline.csv`.", "")
	dir.create(dirname(arq_md), recursive = TRUE, showWarnings = FALSE)
	con <- file(arq_md, open = "wb", encoding = "UTF-8")
	writeLines(enc2utf8(L), con, useBytes = TRUE)
	close(con)
	c(arq_csv, arq_md)
}

# 6. checagem ------------------------------------------------------------------------------------------------------------

## checagem de uma saída: arquivo existe, tem linhas e os efeitos estimados não são todos ausentes
checa_saida <- function(arq) {
	if (!file.exists(arq)) return(data.frame(arquivo = arq, linhas = NA_integer_, efeitos_ausentes = NA_integer_, status = "falta o arquivo"))
	if (!grepl("[.]csv$", arq)) return(data.frame(arquivo = arq, linhas = NA_integer_, efeitos_ausentes = NA_integer_, status = "ok (base)"))
	x <- utils::read.csv(arq, encoding = "UTF-8")
	col <- intersect(c("efeito", "att", "diferenca"), names(x))[1]
	na <- if (is.na(col)) NA_integer_ else sum(!is.finite(x[[col]]))
	st <- if (nrow(x) == 0) "vazio" else if (!is.na(na) && na == nrow(x)) "todos os efeitos ausentes" else if (!is.na(na) && na > 0) "alguns efeitos ausentes" else "ok"
	data.frame(arquivo = arq, linhas = nrow(x), efeitos_ausentes = na, status = st)
}

# 6. relatório HTML dos desenhos -------------------------------------------------------------------------------------------
## renderiza relatorio/relatorio_desenhos.qmd com o Quarto (o R e as bibliotecas do processo atual) e devolve o HTML;
## falha se o log do Quarto não indicar o arquivo gerado
renderiza_relatorio <- function(deps = NULL, qmd = "relatorio/relatorio_desenhos.qmd", saida = "outputs/relatorio/relatorio_desenhos.html",
	log = "outputs/logs_pipeline/relatorio_html.txt") {
	force(deps)
	dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
	dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
	raiz <- normalizePath(".")
	velho <- Sys.getenv(c("QUARTO_R", "R_LIBS"), unset = NA)
	on.exit({
		for (v in names(velho)) if (is.na(velho[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, stats::setNames(list(velho[[v]]), v))
		setwd(raiz)
	}, add = TRUE)
	Sys.setenv(QUARTO_R = R.home("bin"), R_LIBS = paste(unique(c(.libPaths(), normalizePath("lib_r46_extra", mustWork = FALSE))), collapse = .Platform$path.sep))
	setwd(dirname(qmd))
	st <- system2("quarto", c("render", basename(qmd), "--output-dir", file.path(raiz, dirname(saida))), stdout = file.path(raiz, log), stderr = file.path(raiz, log))
	setwd(raiz)
	ok <- any(grepl("Output created", readLines(log, warn = FALSE)))
	if (!identical(as.integer(st), 0L) || !ok || !file.exists(saida)) stop("o relatório não foi gerado; ver ", log)
	saida
}
