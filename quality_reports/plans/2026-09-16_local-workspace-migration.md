# Local workspace migration

## Objective

Move the active Git worktree, R environment, `targets` store, and working outputs from the network
share to the institutional machine's local disk. Keep the shared storage as the authoritative source
for raw inputs and as durable backup. The immediate test is whether removing network filesystem
latency restores reliable Positron submission, completion, Git watching, and `targets` metadata
operations.

## Preconditions

- Confirm whether restricted CadUnico/RAIS intermediates may reside on the protected local disk.
- Confirm adequate local free space for the active `data/` outputs and `_targets/` store.
- Preserve the dirty worktree: modified files and untracked exploratory files must be transferred
  without altering the shared copy.
- Do not expose machine-local restricted-storage paths in versioned configuration or documentation.

## Migration design

1. Create a new local clone from `origin` on the current branch.
2. Transfer the current tracked diff and untracked project files to the local clone, then verify that
   `git status --short` matches the shared worktree.
3. Keep `renv` activation and the project library local to the clone; do not restore or upgrade
   packages during the filesystem test unless the local environment cannot start.
4. Use a distinct local `_targets/` store. Never run the local and shared worktrees against the same
   store concurrently.
5. Copy expensive reusable artifacts selectively, especially R5 networks and completed travel-time
   matrices. Validate paths and target status before deciding whether to copy the complete existing
   `_targets/` store.
6. Keep authoritative raw inputs on shared storage. Represent their root through machine-local,
   unversioned configuration and stage the inputs needed for an active run to local disk when policy
   permits.
7. Keep active derived outputs local. Sync only durable outputs and cleared deliverables back to
   shared storage through an explicit, reviewable step.

## Verification

- Open only the local clone in Positron and confirm that Git resolves the local repository.
- Start R normally and verify project `renv` activation.
- Compare several direct console submissions and several editor `Ctrl+Enter` submissions, both with
  and without selecting a complete expression.
- Run `targets::tar_outdated()` and inspect unexpected invalidation without starting long R5 jobs.
- Run a small, data-safe target or smoke test and confirm that `_targets/` and derived files are
  written locally.
- Confirm that required raw inputs remain readable from shared storage or from the approved local
  staging area.

## Rollback

The shared worktree remains untouched and usable throughout the test. If the local copy fails, close
it and reopen the shared worktree. Do not delete either store until the local setup has been used
successfully and durable outputs have been reconciled.
