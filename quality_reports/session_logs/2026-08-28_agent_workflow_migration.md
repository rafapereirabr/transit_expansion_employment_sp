# Session log: 2026-08-28 — model-agnostic agent workflow

**Status:** COMPLETED

## Objective

Replace imported workflow templates from another repository with a shared Codex/Claude workflow
grounded in this project's actual research objective and pipeline.

## Decisions and rationale

| Decision | Alternatives | Reason |
|---|---|---|
| `AGENTS.md` is canonical; `CLAUDE.md` imports it | Maintain two constitutions | Prevent factual drift between agents |
| Shared procedures live in `.agents/` | Copy the full upstream `.claude/` tree | Most upstream modules do not fit this project |
| Start with six skills and five rules | Import roughly 60 upstream skills | Progressive adoption keeps routing and maintenance tractable |
| Delete `MEMORY_2.md` | Keep or archive it | Its facts describe `aoplanduse`; only the `[LEARN]` convention was useful |
| Defer hooks and permissions | Install upstream settings immediately | These are vendor-specific and need restricted-data review |

## Changes

| File or target | Change | Why |
|---|---|---|
| `AGENTS.md` | Rewritten for this project | Establish factual shared constitution |
| `CLAUDE.md` | Added thin import bridge | Give Claude the same instructions as Codex |
| `MEMORY.md` | Added research horizons and correction log | Preserve durable context and learnings |
| `BACKLOG.md` | Added deliverable-oriented sections | Separate immediate, report, and research work |
| `CHANGELOG.md` | Reset to actual workflow history | Remove claims inherited from another repository |
| `.agents/` | Added guide, rules, and six skills | Supply vendor-neutral reusable procedures |
| `templates/` | Added spec and session-log templates | Standardize planning and handoff |

## Verification

| Check | Result | Status |
|---|---|---|
| Stale-project and path scan | No stale project claims or personal paths in active instructions; historical references are explicitly labeled | PASS |
| Shared-skill structure scan | Six `SKILL.md` files with valid minimal frontmatter; five shared rules present | PASS |
| Claude/Codex bridge | `CLAUDE.md` imports canonical `AGENTS.md`; shared skill paths exist | PASS |
| Pipeline execution | No `_targets.R` or `R/` change from this task | NOT APPLICABLE |

## Blockers and next actions

- [ ] Have a Claude-using collaborator smoke-test `CLAUDE.md` import and one shared skill.
- [ ] Consider hooks only after agreeing on restricted-data and team permission policy.
