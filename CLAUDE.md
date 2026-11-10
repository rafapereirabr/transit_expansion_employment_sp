# Claude Code project instructions

@AGENTS.md

The imported file above is the canonical project constitution. Also read the relevant shared rule
or skill under `.agents/` for the current task.

Do not duplicate project facts here. Claude-specific permissions, hooks, or discovery adapters may
live under `.claude/`, but they must not replace or contradict `AGENTS.md`.

Discovery adapters under `.claude/skills/` point to the canonical shared procedures. When a user
invokes a shared skill by name, follow `.agents/skills/<name>/SKILL.md` in full.
