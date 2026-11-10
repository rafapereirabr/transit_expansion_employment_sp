# `targets` verification rule

The DAG is canonical. After changing `_targets.R` or `R/*.R`:

1. Map the changed function or parameter to its target and downstream consumers.
2. Inspect `targets::tar_outdated()` when feasible.
3. Run the narrowest meaningful `targets::tar_make(names = ...)` scope.
4. Check the target state, schema, row/unit invariants, and expected outputs.
5. Escalate to downstream or full builds when interfaces or assumptions changed.

Never silently substitute a sidequest result for a target. Never claim a long run was completed if
only a smoke test ran. Record the exact scope and any verification deferred for runtime, memory,
Java/R5, or restricted-data reasons.
