# Reconcile local and storage repositories

## Goal

Consolidate work from the storage6 checkout into the local checkout without losing either
checkout's uncommitted changes.

## Findings

- Both histories fork from `6317831`.
- The local checkout uniquely contains `cbe75b1` (four-period panel and local-workspace changes).
- The storage6 checkout uniquely contains `f7af1b0` (research-design skills).
- The local versions of `R/pilot_study.R`, `R/routing.R`, `_targets.R`, `renv.lock`, and
  `sidequests/panel shenanigans.R` already contain or supersede the staged storage6 changes.
- The storage6 research-design plan and report files are not present locally.

## Procedure

1. Record recovery refs for both pre-reconciliation heads.
2. Bundle the storage6-only commit and fetch it into the local repository.
3. Merge the storage6 commit into the local worktree without committing.
4. Copy only the storage6-only research-design plan and report files.
5. Preserve the local `.Rprofile`, `renv.lock`, panel script, and newer pipeline/code versions.
6. Verify the merged ancestry, worktree status, copied-file hashes, and absence of conflict markers.

## Boundaries

- Do not commit, push, reset, clean, or delete files.
- Do not copy restricted data or generated target stores.
- Leave the storage6 worktree unchanged except for this reconciliation plan.
