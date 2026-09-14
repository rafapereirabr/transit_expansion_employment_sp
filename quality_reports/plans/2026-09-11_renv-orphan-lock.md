# Recover orphaned renv sandbox lock

Diagnosis: direct Windows APIs confirm owner PID 29088 has exited (exit code 0, signaled process
object), while renv 1.2.4 considers it alive through psnice. Sandbox acquisition spins on FALSE.

1. Recheck the exact sandbox lock owner and process exit state immediately before mutation.
2. Rename only that orphaned lock directory to a unique sibling evidence name; preserve contents.
   Do not terminate user processes or change packages, project profiles, or sandbox configuration.
3. Sample waiting helpers and the console after release; check whether the active lock disappears.
4. Run bounded normal project startup. Separately measure synchronization-report runtime.
5. Record outcome and recurrence risk. No pipeline or analytical code changes are involved.
