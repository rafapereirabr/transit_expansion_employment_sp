# Plan: workflow agnóstico para agentes

**Data:** 2026-08-28
**Status:** COMPLETED — approved by user on 2026-08-28

## Objetivo

Criar uma camada de colaboração com agentes que funcione tanto no Codex quanto no Claude Code,
sem duplicar a verdade do projeto e sem importar como fatos os templates do repositório de
Pedro H. C. Sant'Anna.

## Diagnóstico inicial

- `MEMORY.md` e `BACKLOG.md` contêm o estado real do trabalho de GTFS/acessibilidade e têm
  alterações locais não commitadas; devem ser preservados.
- `AGENTS.md`, `MEMORY_2.md` e `CHANGELOG.md` foram importados de `aoplanduse` e descrevem outro
  projeto. Servem como exemplos de estrutura, não como fontes factuais.
- O repositório ainda não contém `.agents/`, `.claude/`, `templates/` ou `quality_reports/` além
  deste plano.
- O workplan do CAF confirma o objetivo substantivo: estudar os efeitos da expansão do transporte
  rápido de São Paulo sobre resultados no mercado de trabalho, com CadÚnico como fonte principal,
  linkage com RAIS e atenção explícita a spillovers, tratamento escalonado e COVID-19.
- O upstream contém cerca de 60 skills e muitas regras voltadas a slides, Quarto, periódicos e
  desenvolvimento de pacotes. Copiá-las integralmente criaria ruído e manutenção desnecessária.

## Arquitetura proposta

1. **Constituição compartilhada:** `AGENTS.md` curto e factual como fonte canônica para objetivo,
   estrutura, comandos, segurança dos dados e protocolo de trabalho.
2. **Ponte para Claude:** `CLAUDE.md` mínimo, apontando para `AGENTS.md` e documentando apenas
   diferenças realmente específicas do Claude Code.
3. **Conhecimento compartilhado:** `.agents/` para regras, referências e skills agnósticas em
   Markdown; instruções não devem depender de nomes de ferramentas exclusivos de um fornecedor.
4. **Adaptadores finos:** `.claude/` e `.codex/` somente quando cada produto exigir descoberta,
   permissões, hooks ou configuração próprios. Evitar cópias independentes das mesmas regras.
5. **Quatro registros com funções distintas:**
   - `AGENTS.md`: contexto estável e regras de operação;
   - `MEMORY.md`: decisões, achados e correções duráveis;
   - `BACKLOG.md`: trabalho ainda não concluído, com horizonte e prioridade;
   - `CHANGELOG.md`: mudanças da infraestrutura de agentes, não resultados da pesquisa.
6. **Histórico de execução:** planos e logs em `quality_reports/`; git continua sendo o histórico
   de código, e os logs registram principalmente decisões e alternativas rejeitadas.

## Implementação em fases

### Fase 1 — fundação

- Reescrever `AGENTS.md` para este projeto e remover todas as afirmações de `aoplanduse`.
- Criar `CLAUDE.md` como ponte de compatibilidade.
- Mesclar em `MEMORY.md` apenas fatos confirmados pelo usuário, pelo pipeline e pelo workplan do
  CAF; incorporar de `MEMORY_2.md` somente o padrão `[LEARN:categoria]`.
- Reorganizar `BACKLOG.md` por entregável imediato, relatório CAF e pesquisa de longo prazo,
  preservando cada item técnico existente.
- Reiniciar `CHANGELOG.md` com a migração real deste repositório.
- Arquivar ou remover `MEMORY_2.md` após a fusão para que não reste uma segunda fonte de verdade.
- Criar `templates/session-log.md`, `templates/requirements-spec.md` e um guia rápido do workflow.

### Fase 2 — primeiro lote de regras e skills

Adaptar, sem copiar cegamente:

- regras: plan-first, verificação do DAG `targets`, sessão/handoff, higiene do repo, código R e
  protocolo de dados confidenciais (CadÚnico/RAIS);
- skills: `review-r`, `diagnose`, `interview-me`, `capture-environment`, `checkpoint` e `commit`;
- excluir por enquanto skills de Beamer/Quarto, ensino, TikZ, revisão editorial e pacotes R.

Cada skill deverá usar linguagem de capacidade (ler, editar, executar, revisar) em vez de nomes
de ferramentas Claude/Codex. Onde a invocação nativa exigir metadados distintos, usar adaptador.

### Fase 3 — validação cruzada

- Procurar referências residuais a `aoplanduse`, `.claude` hardcoded, caminhos pessoais e
  comandos inexistentes.
- Validar links relativos e a estrutura das skills.
- Fazer um smoke test no Codex e fornecer um checklist curto para um colega testar no Claude.
- Não instalar hooks nem mudar permissões automaticamente nesta fase; primeiro revisar sua
  portabilidade e o risco com dados restritos.

## Verificação

- `rg` não encontra fatos ou caminhos de `aoplanduse` nos arquivos ativos.
- Toda instrução menciona comandos e caminhos que existem neste repositório.
- O diff preserva integralmente as mudanças locais pré-existentes em `MEMORY.md` e `BACKLOG.md`.
- `AGENTS.md`, `MEMORY.md`, `BACKLOG.md` e `CHANGELOG.md` não disputam a mesma responsabilidade.
- As skills do primeiro lote não dependem de nomes de ferramentas exclusivos de um fornecedor.
- Mudanças apenas de workflow não disparam a pipeline; qualquer mudança futura em `_targets.R`
  ou `R/` exige `targets::tar_make()` no escopo afetado e confirmação dos outputs.

## Decisões aprovadas

1. Usar `AGENTS.md` como constituição canônica e `CLAUDE.md` como ponte fina.
2. Excluir `MEMORY_2.md` após extrair o padrão útil, conforme autorização posterior do usuário.
3. Adotar apenas o primeiro lote de seis skills e cinco regras; ampliar conforme o uso real.
4. Manter hooks e configurações de permissão fora desta primeira implementação.

## Arquivos previstos

- Modificar: `AGENTS.md`, `MEMORY.md`, `BACKLOG.md`, `CHANGELOG.md`.
- Mover/arquivar: `MEMORY_2.md`.
- Criar: `CLAUDE.md`, `.agents/WORKFLOW_QUICK_REF.md`, regras e skills selecionadas,
  `templates/`, `quality_reports/session_logs/` e adaptadores mínimos necessários.

## Resultado

- Fundação, regras, seis skills e templates criados.
- Conteúdo incorreto de `aoplanduse` removido dos arquivos ativos, mantendo apenas uma correção
  histórica em `MEMORY.md` e o registro desta migração.
- Hooks e permissões específicos de produto permanecem deliberadamente fora do escopo inicial.
