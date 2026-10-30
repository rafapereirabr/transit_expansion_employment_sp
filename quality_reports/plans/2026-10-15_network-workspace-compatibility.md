# Network workspace compatibility

## Objective

Make Codex file operations reliable for the mapped-drive checkout without recording or exposing
the backing network path.

## Scope

1. Add a repository-relative PowerShell adapter for the official Codex patch interface.
2. Normalize line endings and UTF-8/base64 input before applying a patch.
3. Document that Codex should use the mapped drive and request sandbox escalation on the first
   repository shell call because the Windows restricted-token sandbox cannot handle this checkout.
4. Keep all repository paths relative and do not store the backing UNC path.

## Verification

- Use the adapter itself to update this plan after bootstrap.
- Confirm that an invalid patch is rejected before invoking Codex.
- Run `git diff --check` and inspect the scoped diff.

## Status

Implemented and verified on the mapped-drive checkout.

## Verification results

- Text and UTF-8/base64 patch modes both applied changes successfully.
- Invalid patch input was rejected before the Codex patch interface ran.
- `git diff --check` and the scoped diff were reviewed after implementation.
