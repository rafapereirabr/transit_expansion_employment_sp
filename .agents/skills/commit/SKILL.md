---
name: commit
description: Review, verify, stage, and commit an approved change. Use only when the user explicitly requests a commit; pushing, PR creation, and merging require their own explicit authorization.
---

# Commit

1. Confirm explicit commit intent and inspect `git status`, diff, and recent log.
2. Separate pre-existing/user changes from this task. Never stage unrelated work implicitly.
3. Run proportionate verification. For pipeline changes, follow
   `.agents/rules/targets-verification.md`; for documentation/workflow changes, validate links,
   paths, stale claims, and referenced commands.
4. Inspect staged candidates for secrets, restricted data, identifiers, absolute machine paths, and
   generated artifacts. Stage named files only; do not use broad staging shortcuts.
5. Show or summarize the exact staged scope, then commit with a message describing the resulting
   truth and why it matters.
6. Report commit hash and verification. Do not push, open a PR, merge, rebase, or rewrite history
   unless explicitly requested.

Never bypass hooks or use destructive git commands to make a commit succeed.
