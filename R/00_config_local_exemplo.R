# Configuração local (modelo). Copie este arquivo para R/00_config_local.R e preencha os caminhos desta máquina.
# O arquivo R/00_config_local.R NÃO é versionado (.gitignore): caminhos absolutos da máquina nunca entram no git.
# Todos os outros caminhos do projeto são relativos à raiz do repositório (data/, outputs/, lib_r46_extra/).

## raiz do projeto do Arthur (SOMENTE LEITURA: nunca gravar lá, nunca rodar o pipeline dele, nunca instalar pacotes na
## renv dele). De lá vêm a biblioteca renv (targets, crew, arrow, r5r...), o Java, o CadÚnico processado e os locais de
## trabalho geocodificados da RAIS.
raiz_arthur <- "<unidade>:/<pasta>/transit_expansion_employment_sp"

## CadÚnico bruto harmonizado (restrito): pasta com cadunico_pes_<aaaa>12_harmonizado.parquet (R/117)
dir_cadunico_bruto <- "//<servidor>/<pasta>/CADASTRO_UNICO/parquet/harmonizado"

## RAIS geocodificada (restrito): pasta com rais_geolocalizada_<aaaa>.parquet (R/04)
dir_rais_geocode <- "//<servidor>/<pasta>/RAIS/geocode"

## CAGED identificado (restrito): pasta parquet (R/95)
dir_caged <- "//<servidor>/<pasta>/CAGED_ID/parquet"

## pasta da auditoria interna (opcional; conferências do R/61). Sem ela, usa outputs/auditoria
dir_auditoria <- "<pasta da auditoria>"

## Rtools para compilar o demean em C (opcional; também lido de RTOOLS45_HOME/RTOOLS44_HOME)
dirs_rtools <- c("<unidade>:/rtools45", "<unidade>:/rtools44")
