# Shared agent workflow — quick reference

This repository uses one factual layer for both Codex and Claude Code.

## Start a session

1. Read `AGENTS.md`.
2. Inspect `git status --short` and preserve pre-existing work.
3. Read the relevant sections of `MEMORY.md` and `BACKLOG.md`.
4. For non-trivial work, create or resume a plan in `quality_reports/plans/`.

Codex reads `AGENTS.md` directly. Claude Code starts from `CLAUDE.md`, which points to the same
constitution. Shared procedures live here under `.agents/`; product-specific settings should be
thin adapters and must not duplicate project facts.

## Normal loop

`scope → plan → implement → verify → document → hand off`

- Plans describe the intended work and verification.
- `targets` verifies pipeline changes; file existence alone is insufficient when a target should
  have rebuilt.
- `MEMORY.md` receives durable decisions and corrections.
- `BACKLOG.md` receives unfinished work.
- Session logs explain why an approach changed or what remains blocked.
- Commits require an explicit request.

## Shared skill invocation

Ask in natural language or name the skill, for example:

- “Use `review-r` on `R/routing.R`.”
- “Run `diagnose` for `ttm_transit_all`.”
- “Use `interview-me` to formulate the executive box.”
- “Create a `checkpoint` before we stop.”

Both agents should open `.agents/skills/<name>/SKILL.md`. Skills describe capabilities, not
vendor-specific tool names.

## What was intentionally not imported

The upstream workflow's Beamer, Quarto, TikZ, teaching, journal-submission, Stata, simulation, and
R-package-development modules are not active. Add a skill only after the same multi-step workflow
has recurred or a stable quality check is clearly needed.

Hooks and permission files are also deferred. They are product-specific and must be reviewed
against the restricted-data protocol before installation.
