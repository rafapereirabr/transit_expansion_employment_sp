---
name: diagnose
description: Diagnose a failed, stale, slow, or substantively wrong targets target and identify the root cause. Read-only unless the user asks for a fix.
---

# Diagnose a target

1. Record the target name, expected behavior, observed behavior, and most recent relevant change.
2. Read `AGENTS.md`, relevant `MEMORY.md` cautions, the target command, upstream functions, and
   downstream expectations.
3. Inspect target metadata, progress, error/workspace, logs, and `tar_outdated()` where safe.
4. Reproduce with the smallest diagnostic scope. Do not start a multi-hour R5 build merely to obtain
   an error already present in metadata.
5. Separate root cause from symptoms and contributing conditions. Test competing hypotheses with
   read-only or temporary diagnostics.
6. Report evidence, confidence, affected scope, and a proposed fix/verification plan.

Stop after diagnosis unless implementation was requested.
