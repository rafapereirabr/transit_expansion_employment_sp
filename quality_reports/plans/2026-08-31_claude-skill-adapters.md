# Claude skill discovery adapters

## Objective

Make the canonical skills under `.agents/skills/` discoverable by Claude Code without duplicating
their procedures, and record the routing-review recommendations for later triage.

## Scope

1. Add one thin `.claude/skills/<name>/SKILL.md` adapter for each existing shared skill.
2. Update the workflow quick reference and changelog to describe the adapters.
3. Add a dated, non-duplicative follow-up section to `BACKLOG.md` for the review findings.

## Verification

- Confirm every `.agents/skills/*/SKILL.md` has a corresponding Claude adapter.
- Confirm each adapter points to the canonical shared skill and duplicates no procedure body.
- Inspect the focused diff and scan the new files for absolute machine paths.

