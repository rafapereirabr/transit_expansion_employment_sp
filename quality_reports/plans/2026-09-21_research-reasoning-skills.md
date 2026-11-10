# Research-reasoning skills

**Date:** 2026-09-21
**Status:** COMPLETED

## Objective

Adapt four skills from `pedrohcgs/claude-code-my-workflow` to improve discussion of research
questions, identification strategies, relevant literature, and the credibility of causal claims.
Keep the skills method-agnostic and useful before implementation choices are fixed.

## Scope

- Add canonical, vendor-neutral skills under `.agents/skills/`:
  `research-ideation`, `devils-advocate`, `lit-review`, and `credible-claims`.
- Add thin Claude discovery adapters under `.claude/skills/`.
- Adapt rather than copy the upstream procedures: remove lecture-specific, manuscript-specific,
  tool-specific, and subagent-specific assumptions.
- Encode the project's three distinct products and its restrictions on confidential data.
- Update the workflow quick reference, `AGENTS.md`, and `CHANGELOG.md` skill inventories.

## Design

1. `research-ideation` maps a substantive topic into candidate questions, estimands, identifying
   variation, assumptions, threats, data requirements, and discriminating next evidence.
2. `devils-advocate` performs a read-only adversarial examination of a proposed research design,
   separating fatal identification failures from resolvable limitations and presentational gaps.
3. `lit-review` synthesizes literature by question, estimand, variation, assumptions, and external
   validity rather than producing an annotated bibliography.
4. `credible-claims` maps evidence to the strongest defensible descriptive, associational, design-
   based, or causal claim and identifies what additional evidence would strengthen it.

## Verification

- The skill-creator Python initializer and validator could not run because neither `python` nor
  the Windows `py` launcher is installed in the execution environment.
- PowerShell checks confirmed valid names and descriptions, one-to-one canonical/adapter parity,
  and a single correct canonical pointer in every adapter.
- A repository search found no upstream-only paths, tool names, or unsupported dependencies in the
  canonical skills.
- `git diff --check` passed. The final status retained pre-existing R, pipeline, lockfile, plan,
  and sidequest changes without modifying them.

## Non-scope

- No estimator-specific implementation, R functions, targets, simulations, hooks, or agents.
- No change to the empirical design itself.
- No commit, push, or external upload.
