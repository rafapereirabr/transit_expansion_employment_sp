# Restricted-data rule

CadÚnico, linked RAIS records, addresses, and personal identifiers are confidential unless an
applicable agreement explicitly says otherwise.

1. Never commit or upload raw/row-level restricted data or identifiers.
2. Never expose restricted values through web search, external connectors, prompts, logs, tests, or
   examples. Use synthetic or schema-only material.
3. Keep machine-specific restricted-data paths outside committed files.
4. Treat fine-geography maps, small cells, extrema, and linked administrative outputs as potentially
   disclosive until formally cleared.
5. Inspect the staged file list before every commit. Commit code and approved aggregate artifacts,
   not confidential inputs.

This rule does not invent disclosure thresholds. Record and apply the actual DUA/provider rules
once confirmed by the project team.
