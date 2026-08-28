---
name: checkpoint
description: Create a concise, durable cross-session handoff with decisions, verification, blockers, and the exact next action.
---

# Checkpoint

1. Read the active plan, `git status`, relevant diff, and recent target/test results.
2. Create or update `quality_reports/session_logs/YYYY-MM-DD_description.md` from
   `templates/session-log.md`.
3. Record why key choices were made, what was verified, what remains uncertain, and the exact next
   command or decision.
4. Move durable corrections to `MEMORY.md` and unfinished tasks to `BACKLOG.md`; link them rather
   than duplicating long text.
5. Never include secrets, confidential values, identifiers, or machine-specific restricted paths.

A checkpoint does not commit or push.
