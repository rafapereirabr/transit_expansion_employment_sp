# Revisão DiD downstream — 2026-11-11

## Resultado e escopo

Scripts revisados sem executar R, targets, bootstrap, Air ou testes, conforme solicitado.
Nenhum dado administrativo foi lido. Alterações anteriores em define_treatment.R,
read_cadunico.R, .Rprofile, mapas e metadados targets foram preservadas. A seção de
estimação do DAG mudou, mas a estimação continua comentada.

## Estratégia

- Unidade da análise: pessoa pertencente ao risk set anual focal, residência e
  elegibilidade congeladas no baseline. Aberturas do mesmo ano entram na coorte anual.
- Serviço antigo não é tratamento absorvente por si só. No risk set atual, status 2
  versus 1 forma o estrato additional; status 3 versus 4/5/6 forma first.
  Restrições temporais posteriores continuam necessárias em ambos os estratos.
- O ano prospectivo de primeira cobertura usa TODAS as aberturas incluídas no endereço
  de baseline daquele risk set. Não usar a abertura da estação mais próxima:
  uma estação mais distante pode abrir antes, e a estação de additional é antiga.
- A atribuição anterior entre risk sets é um campo separado; pessoas já atribuídas
  anteriormente são excluídas. O helper exige essa informação, não presume sua ausência.
- O controle L6/L17 exige flag independente dos status mutuamente exclusivos, preserva
  todos os tratados focais e mantém o mesmo estrato de serviço de baseline.
  Se não houver controles adicionais elegíveis, o modelo falha em vez de trocar de população.
- O baseline residencial g - 4 não substitui a referência DiD g - anticipation - 1.
  A spec usa base universal e anticipation = 0 como hipótese inicial explícita.
  Antecipação positiva continua configurável.
- A spec padrão usa DR com intercepto apenas: o contraste é incondicional, sem ajuste
  efetivo por covariáveis. Covariáveis futuras devem ser medidas no baseline e permanecer
  constantes por pessoa dentro de cada risk set. Overlap e tendências ainda precisam de auditoria.
- Somente ATT(g,t) focal sai de cada fit. Coortes futuras auxiliares classificam quando
  controles deixam de ser elegíveis; não são reportadas como se tivessem o baseline focal delas.
- Employment e formal_above têm população de coorte. Falta de cobertura/outcome impede
  estimação, em vez de balancear silenciosamente o painel ou converter NA em zero.
- Salário e distância são contrastes da população selecionada observada, separados do ITT.
  Não há combined desses outcomes nesta implementação.
- Combined usa contagens de tratados dos estratos. Ano de abertura é separado dos anos
  completos; a média exige coortes e horizonte comum explícitos. A soma dos pesos entre
  coortes é número de memberships, não número de pessoas únicas.

## Spillovers e inferência

Spillovers permanecem suspensos, sem ativação automática em 16/11 ou qualquer outra data.
A spec aceita none, exclude_2_4km e exclude_2_4km_and_gain; os dois últimos exigem flags
de controle fornecidas e auditadas upstream. Não se interpreta o controle atual como
livre de efeitos de rede.

O bootstrap sorteia pessoas da união dos risk sets e replica cada pessoa, com novo ID
por sorteio, em todas as suas aparições, anos e modelos. Reestima estratos e pesos do
combined no mesmo sorteio. Não há bootstrap aninhado do did. A seed é explícita e o
estado RNG do chamador é restaurado.

A distribuição é condicional ao roster, à exposição e à definição dos outcomes
(incluindo o limiar salarial fixado no baseline); não propaga incerteza desses objetos.
Os intervalos normais pontuais e bandas simultâneas cobrem a família de contrastes
solicitada. Falhas, suporte alterado ou resultados não finitos em qualquer réplica
impedem a publicação automática dos intervalos. Dependência espacial/corredores não
é resolvida por clustering de pessoa e permanece pendente.

## Arquivos

- R/did_spec.R: grid e configuração de inferência.
- R/did_panel.R: adaptação dos status e junções com cardinalidade explícita.
- R/did.R: amostra, estimação, extração focal, combined e horizonte.
- R/did_bootstrap.R: grid conjunto, reamostragem e auditoria.
- R/read_rais.R: remove descarte antecipado de vínculos sem id_estab, deduplica e
  torna explícita a junção por CPF; a falta de estabelecimento não define formal = 0.
- _targets.R: spec inteira em um objeto, configuração separada e wiring futuro corrigido.

## Bloqueios antes de estimar

1. Materializar a tabela de exposição pessoa × risk set e atribuições anteriores;
   implementar flags de corredor independentes. O adapter não inventa esses campos.
2. Resolver divergências entre decisões registradas e builder atual:
   idade 18–60 em janeiro versus 22–60 na data de abertura, distância planar versus
   geodésica, população municipal versus RMSP e limite espacial de 15 km.
   Auditar ainda exclusões por atualização cadastral/geocodificação.
3. Corrigir/auditar o conjunto stations_nyet: o setdiff atual pode incluir anos
   anteriores ao focal fora do intervalo forbidden. Nearest station não recupera
   a cronologia prospectiva mesmo depois dessa correção.
4. Construir outcomes anuais, cobertura de extração, formalização, salário real,
   limiar comum no RMSP e distância ao workplace. Não calcular o limiar depois
   de restringir os controles ao corredor ou ao suporte espacial.
5. Confirmar renv/API did, definir covariáveis/support, horizonte, inferência espacial
   e família de testes para o produto final.

## Verificação e próximo passo

Somente leitura e revisão textual de código/diffs; sem alegação de validação funcional.
O wrapper invoke_repo_patch.ps1 falhou por múltiplos executáveis Codex e passagem de
argumentos multilinha; arquivos foram escritos diretamente sem mudar o wrapper.

Na próxima sessão autorizada a executar: Air, parse e testes sintéticos dos contratos,
controles futuros, estratos vazios, replicação conjunta e falhas; depois benchmark de
um risk set sem bootstrap, antes de qualquer rodada completa.

Referência da API consultada: https://bcallaway11.github.io/did/reference/att_gt.html.

