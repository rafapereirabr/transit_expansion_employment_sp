# Positron / renv startup investigation - 2026-09-11

## Current state

The current hang is recovered. The user confirmed `1 + 1` responded in the Positron console.
Publisher is disabled. Sandbox remains enabled; no packages or project profiles were changed.
Permanent recurrence prevention is pending in BACKLOG.md. Do not resume process cleanup or ask
for further approvals unless a new incident or implementation request warrants it.

## Initial symptoms and hypotheses

Opening Positron without entering commands produced a busy console. Two Rterm helpers querying
`renv::paths$lockfile()` (PIDs 37444 and 31004) and the console consumed approximately one core each.
Publisher 2.12.0 contains that query with a 15-second timeout; attributing those specific old
processes to Publisher remained an inference because the full ancestry was unavailable.
After Publisher was disabled and Positron reopened at 16:52, no new helper query appeared, but
new console kernel PID 36600 remained CPU-busy. The old helpers survived from earlier sessions.

## Diagnosis and evidence

Environment tested: Windows, R 4.6.1, renv 1.2.4.

- Rscript --vanilla reached a diagnostic script and exited in 1.4 seconds.
- Explicit source("renv/activate.R") exceeded 20 seconds. Rprof attributed 93.72% of sampled CPU
  time to renv_lock_acquire inside sandbox activation. This is distinct from renv.lock.
- Sandbox lock directory e93724f8.lock dated September 10, 14:32:58 recorded owner PID 29088.
  CIM and Get-Process did not list it, but renv_process_exists returned TRUE. Its imported psnice
  returned 0 for that PID and NA for deliberately nonexistent PID 2000000000.
- Direct Windows APIs resolved the discrepancy: OpenProcess succeeded for 29088, exit code was 0,
  and WaitForSingleObject(handle, 0) returned WAIT_OBJECT_0 (0). The process had terminated even
  though its process object remained accessible. Live console PID 36600 returned STILL_ACTIVE
  (259) and WAIT_TIMEOUT (258). The nonexistent PID failed OpenProcess with error 87.
- The installed renv liveness check uses !is.na(psnice(pid)); it therefore misclassified this
  terminated owner as alive. Lock acquisition retries immediately on FALSE and sleeps only on
  errors, explaining the continuous CPU consumption.

Windows semantics: https://learn.microsoft.com/en-us/windows/win32/procthread/terminating-a-process
We did not identify which process retained the old process object or why the owner left its lock.

## Recovery and verification

Followed quality_reports/plans/2026-09-11_renv-orphan-lock.md. Immediately before recovery, re-read
owner PID 29088 and reconfirmed termination via Windows APIs. Renamed only the exact orphaned lock
to sibling e93724f8.orphan-evidence-b668a99a03454b2992337600e2859832, preserving its contents in the
machine-local sandbox cache. Machine-specific absolute paths are intentionally omitted.

Console PID 36600 acquired the lock afterward; a later check found it released. Old helpers
37444 and 31004 exited on their own. The console consumed zero CPU seconds in a three-second sample.
Rscript --no-save --no-restore, using the normal project profile and sandbox ENABLED, reached
PROJECT_STARTUP_DONE, reported renv 1.2.4, and exited successfully in 47.67 seconds.

Separate sandbox-disabled diagnostic children exceeded limits of 20 and 55 seconds, but profiles
advanced through installed package enumeration and broken-link checks in the synchronization
report. This is additional startup latency, not the original lock wait. These are separate runs,
not a controlled estimate of sandbox overhead. C.UTF-8 locale warnings occurred even in baseline
startup and did not prevent successful startup; no locale setting was changed.

The user subsequently confirmed console responsiveness and reported an earlier warning:
"renv took longer than expected (1300 seconds) to activate the sandbox."
This is consistent with the old session's wait before release. It does not establish recurrence.
Do not disable the sandbox solely because that old warning suggests it as a generic workaround.

Only newly created diagnostic processes were stopped at their time limits. No original user
process was terminated by the assistant. No packages, renv.lock, persistent environment settings,
analytical code, or targets were changed/run. Temporary diagnostic scripts/profiles stayed local.

## Handoff and exact next action

No immediate user action is needed; continue working with Publisher disabled. If symptoms recur,
inspect the actual local sandbox lock and prove its owner's termination before any recovery.
Do not reuse this machine's PID or sandbox hash on another machine. A missing PID in process
lists alone is insufficient; check Windows termination state and host identity as appropriate.

A permanent fix would address Windows liveness detection and the abandoned-lock origin. Existing
recovery alone does not guarantee non-recurrence. Investigate remaining startup latency separately
if it impedes work. Do not update packages or disable isolation automatically.

The user prefers clear distinction between current recovery and permanent prevention. Repeated
permission prompts were confusing; batch read-only diagnostics and request only necessary approvals.

## Records

MEMORY.md holds the durable correction; BACKLOG.md holds recurrence and latency work. The checkpoint
skill's referenced session-log template was not found under .agents; this uses a compact layout.
No commit or push was requested or performed.
