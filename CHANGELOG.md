# Workflow changelog

Notable changes to the repository's agent/workflow infrastructure. Research results and pipeline
outputs belong in git history, `MEMORY.md`, or the relevant report—not here.

## Unreleased

### Changed

- Replaced imported `aoplanduse` instructions with project-specific, model-agnostic guidance.
- Established distinct responsibilities for `AGENTS.md`, `MEMORY.md`, `BACKLOG.md`, and this file.
- Added a minimal `CLAUDE.md` bridge so Claude Code follows the same constitution as Codex.
- Reorganized the backlog around the executive box, CAF report, longer-term research design, and
  accessibility-pipeline dependencies.

### Added

- Shared workflow material under `.agents/`.
- Initial rules for planning, pipeline verification, session handoff, R code, and confidential data.
- Initial skills for R review, diagnosis, research specification, environment capture, checkpoints,
  and explicitly authorized commits.
- Templates for requirements specifications and session logs.

### Removed

- `MEMORY_2.md`, after retaining only its useful `[LEARN:category]` convention. Its substantive
  contents described a different repository.
