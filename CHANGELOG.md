# Changelog

Notable changes to this project's Claude Code / Agents workflow configuration (`AGENTS.md`, `.agents/`, `templates/`). This tracks the *workflow config*, not the pipeline's data/analytical outputs.

---

## v0.1.0 — 2026-08-24

Adopted and adapted the Claude Code academic workflow (forked from `pedrohcgs/claude-code-my-workflow`) for `aoplanduse`.

### Fixed
- `.agents/settings.json` and its hooks hardcoded `.claude/hooks/...` / `.claude/scripts/...` paths that didn't exist in this repo (the folder is `.agents/`) — every hook was silently no-op-ing. Repointed to `.agents/`.

### Removed
- Skills, agents, rules, references, and templates built for a Beamer/Quarto lecture-slides + journal-peer-review workflow (12 skills, 4 agents, 12 rules, 3 references, 5 templates) — none apply to this R/`targets` geocoding pipeline.
- `MEMORY.md` and this file's prior content, which documented the upstream template's own v1.8/v1.9 development history, not this project.

### Changed
- `AGENTS.md` fully rewritten for aoplanduse: real folder structure, real commands (`renv`, `targets`, `air format`), advisory quality gates (no `quality_score.py`/pre-commit hook exists here), pruned skills list, and a pipeline-domains table (jobs/RAIS and schools implemented; CNES health and CRAS welfare not yet implemented).
- Kept rules/references retargeted to this repo: `r-code-conventions.md` (path scoping + actual map/figure conventions from `figures/maps_br.R`), `orchestrator-research.md` and `quality-gates.md` (targets-pipeline-specific verification/thresholds), `replication-protocol.md` (pipeline-rerun reproducibility instead of paper replication), `model-routing.md`/`agent-fleet.md` (trimmed to the 3 retained agents), `verifier.md` agent (pipeline verification instead of slide compile/render), `deep-audit` skill (retargeted to this repo's actual surfaces — no `guide/`, `docs/`, or integrity-check scripts exist here).
- Swept remaining `.claude`/`CLAUDE.md` path references in kept files to `.agents`/`AGENTS.md`, preserving genuine references to the real Claude Code CLI's global `~/.claude/` directory.
