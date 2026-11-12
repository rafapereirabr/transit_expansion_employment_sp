# Divergências resolvidas — 2026-11-12

Usuário confirmou: município de São Paulo, distância planar em EPSG:31983,
suporte de 15 km e idade 18–60 em anos completos em 1º de janeiro de g.
Spillovers seguem suspensos. Datas de abertura não definem mais a referência etária.

O limite de 15 km preserva a implementação existente: distância a qualquer estação
nas categorias do risk set, não apenas às aberturas focais. A residência territorial
é verificada pelo código municipal no snapshot g - 4. Históricos familiares fora
atingem a extração, mas não estabelecem elegibilidade no baseline.

Alterações: assumptions explícitas; exclusão precoce de menores retirada de
read_cadunico; idade definida por limites de nascimento exatos; filtro municipal
com exclusões auditadas; dependency munis_sf no risk set; nome do arquivo municipal
corrigido; stations_nyet restrito a anos futuros; MEMORY/BACKLOG atualizados.
A distância ao workplace continua geodésica; a decisão planar aplica-se ao tratamento.

CRS: a validação local já rejeita CRS distintos ou desconhecidos. O warning de CRS
iguais está na função ddbs_transform da dependência, indicando transformação redundante.
O helper passa agora a reutilizar as tabelas quando candidate_crs coincide com o CRS
compartilhado. Não suprime warnings genericamente. Diagnóstico por inspeção local e
fonte upstream, sem reprodução na versão instalada:
https://github.com/Cidree/duckspatial/blob/main/R/ddbs_crs.R

Verificação: revisão textual e git diff --check. Não executados R, Air, parse,
testes, bootstrap ou targets por orientação do usuário. Rebuild obrigatório de
person_files/records/audit, risk_sets e mapas antes de usar os resultados existentes.
Testar limites de idade (18, 60, 61), mudanças de município e CRS coincidente/distinto.
Outras pendências: atribuição prospectiva e absorvente, outcomes, inferência espacial,
auditoria das exclusões por geocodificação e atualização cadastral.
