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

## 2026-09-15 autocomplete follow-up

The remaining slowdown is generic Ark completion (`mea` -> `mean` as well as `libr` ->
`library`), observed between roughly 10 and 47 seconds. Package enumeration itself took 0.56
seconds, so the project renv library does not explain this completion delay. Workspace
watcher/search exclusions and temporarily disabling R diagnostics did not produce a clear
improvement; single timings must not be treated as causal because warm-up and prior requests vary.

Temporary workspace trace/debug settings were enabled to collect the evidence below and were
removed after collection.

Ark PID 22860, in the user's Windows SessionId 2, repeatedly logged 5--15 second LSP main-loop
stalls with outstanding Salsa database holds. Reloads reset the LSP/DAP connections, and the UI
appeared to restart R after about 10 seconds, but the same Ark process remained alive. Its parent
is the live `kcserver.exe` PID 15348, so it is not an orphan at the operating-system level. The
more precise hypothesis is stale or broken Ark/LSP state surviving window/session reconstruction.
The Ark/Rterm pair in SessionId 3 belongs to another logged-in session and must not be touched.

A later full exit produced a new Ark but the first completion still waited for minutes, ruling out
the old Ark as the cause of the remaining delay. The new log showed that completion had not reached
Ark. Adding `files.exclude` then reduced the observed delay to seconds.

### Automatic `tar_renv()` discovery

After adding `files.exclude`, four completion timings were approximately 5, 17, 7, and 9
seconds. Disabling Oak source fetching gave 4, 12, 6, and 10 seconds, which was not a clear
improvement; that temporary setting was removed. With the renv autoloader temporarily disabled,
timings were 3, 9, 4, and 7 seconds, also overlapping the prior range. The large improvement from
minutes to seconds remains associated with adding `files.exclude`; the remaining UI delay occurs
outside Ark, whose logged completion work took about 0.35--0.53 seconds.

A separate startup cause was found in the Windows user profile `Documents/.Rprofile`: every
interactive R session in a directory containing `_targets.R` called `targets::tar_renv()`. This
explains why startup parsed `R/pilot_study.R` and emitted a known syntax error. The automatic block
was removed on 2026-09-15 after creating a machine-local backup beside the profile. The llmcoder
configuration was preserved. `tar_renv()` should be run manually only when project dependencies
change; `_wizard.R` already contains a commented reminder.

The temporary workspace setting that disabled the renv autoloader was then removed. Current
intended state: normal project renv activation, no automatic `tar_renv()`, R diagnostics still
disabled, and `files.exclude`/watcher/search exclusions retained for heavy generated/data folders.

### Controlled Positron profile comparison and recovery

A clean temporary `--user-data-dir` with installed extensions disabled produced sub-2-second R
completion after the first warm-up command, and a second `1 + 1` was immediate. Reopening the same
temporary profile with the normal extension directory kept both `mea`/`libr` near or below one
second and both `1 + 1` calls immediate. Copying the normal user `settings.json` into that temporary
profile did not slow it down. This rules out the installed extension set and user settings by
themselves; the remaining difference is persisted UI/profile state and whether the long Codex
conversation is loaded.

The normal profile's workspace state for this project contained two 79.4 MB `state.vscdb` files
(151.4 MB total), versus two 94 KB files in the fast temporary profile. The workspace state was
reset, and the new active database remained small (about 43 KB), but normal-profile commands still
waited before reaching Ark. The sibling `.stale-20260915` backup was automatically removed by
Positron on restart, presumably as an invalid workspaceStorage entry; it is no longer available.
Project files, settings, and extensions were unaffected.

UI caches were then moved outside the profile to a reversible machine-local backup. `Cache` fell
from 190.8 MB to 4.8 MB, `GPUCache` from 5.6 MB to 0.5 MB, and `WebStorage` from 52.1 MB to 13.6 MB
after reopening. Completion improved to roughly 2--4 seconds, but `1 + 1` still waited in the UI.
Supervisor logs consistently show execute requests completing within the same second once they
arrive; Ark logs after the resets contain no Salsa watchdog errors. The frontend delay remains.

The current long Codex conversation is now a specific remaining confound: after UI caches were
cleared, WebStorage grew to 13.6 MB almost immediately when this conversation reopened, while the
fast temporary profile where Codex was not opened held only about 0.09 MB. Earlier closing the chat
panel did not eliminate delays, but may not unload its persisted webview state.

Exact next action: start a new Codex conversation in the normal Positron profile, refer to this
checkpoint, wait until the R prompt and renv activation are fully complete, and test two `1 + 1`
commands followed by `mea`, `libr`, `mea`, `libr`. If the new conversation remains slow, migrate
the normal Positron user-data profile to a clean profile, copying only settings/keybindings/snippets
and keeping the old profile as an external backup. Do not restore the old UI caches during the
test.
### Codex extension-host isolation

A new Codex conversation in the normal profile did not remove the delay: the six-command test
remained around 2--3 seconds. The profile was then migrated reversibly to a clean profile, copying
only settings, keybindings, and snippets and preserving the old profile in a machine-local backup.
The clean profile did not help: the first `mea` completion took about 25 seconds, both `1 + 1`
submissions were delayed, and later `libr`/`mea` completions took about 5 seconds.

Logs from that controlled run showed that `libr` took only 338 ms in Ark and completion resolution
took another 218 ms. At the same time, the Codex extension log repeatedly reported
`git-repo-watcher` failures while watching `.git`, `refs`, `refs/heads`, and `info` through the
project's network/UNC path. The installed Codex extension was pre-release version
`26.5730.61309`; Positron offered "Switch to Release Version", but the marketplace reported that no
release version exists.

Disabling only `openai.chatgpt` for this workspace was the decisive controlled test: both `1 + 1`
commands became immediate and R completions fell to about one second. Codex enabled in the shared
extension host had also taken about seven seconds after submission before `thinking` appeared.

The installed Positron build supports `extensions.experimental.affinity`. The user authorized
adding the following machine-local user setting, with an adjacent backup of the prior
`settings.json`:

```json
"extensions.experimental.affinity": {
  "openai.chatgpt": 1
}
```

After reloading with Codex enabled in its own extension host, both `1 + 1` commands were immediate,
`mea`/`libr` completions were below one second, and Codex reached `thinking` in about two seconds.
This confirms extension-host contention as the immediate cause of the R UI delays. The repeating
Codex Git watcher failures on the network repository are the strongest observed contributing
condition; affinity isolates their impact but does not fix the watcher itself.

Current intended state: keep the affinity setting, normal renv activation, no automatic
`tar_renv()`, R diagnostics disabled, and the existing workspace exclusions. The clean Positron
profile and the original profile backup remain machine-local. Do not delete the backup until the
user confirms no missing UI or extension state. If delays recur, first verify that Codex still runs
in a separate extension host and inspect its current Git watcher log.
