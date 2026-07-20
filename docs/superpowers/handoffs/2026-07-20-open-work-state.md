# Handoff — Deflating WIOD Tables / Growth Decomposition

**Date:** 2026-07-20
**Repo:** `/home/gpetrini/Documents/Deflating_WIOD_Tables`
**Branch:** `master` (working tree clean)

## Context

Fork of a WIOD (2016 release) deflation project, now decomposing real-GDP growth into
domestic and import-content contributions, per country, 1995–2020. Code is R. Full
project orientation lives in `README.md` — read it first; do not re-derive it here.

## Critical blocker (shared by everything below)

Stage B and all three open plans depend on `etc/Modelo/NIOTs_resultados.rdata` (output of
Stage A), which is **not in the checkout**. Without it, only Tier-A verification (parse)
runs; Tier-B (numeric/runtime) is impossible. This data object must be supplied
out-of-band before any runtime verification.

## Open work (all pending — nothing started)

Reference the source docs rather than the summaries below; they hold the full, checkboxed steps.

1. **Pre-meeting reporting improvements** — 8 tasks, none done.
   Plan: `docs/superpowers/plans/2026-07-10-pre-meeting-reporting-improvements.md`
   Spec: `docs/superpowers/specs/2026-07-10-pre-meeting-reporting-improvements-design.md`
   Items map: 7→Task1, 3→Task2, 4→Task3, 5→Task4, 6→Task5, 8→Task6, 1→Task7; **Item 2 deferred (do not implement)**.
   All edits confined to `code/support_functions.R` and `code/global_variables.R`.
   Tier-A verify command is in the plan; Item-3 finding must be written to project memory (Task 8 Step 3).

2. **`growthdecomp` R package** — plan complete, implementation not begun.
   Plan: `docs/superpowers/plans/2026-07-08-growthdecomp-package.org`
   Spec: `docs/superpowers/specs/2026-07-08-growthdecomp-package-design.org`
   Decisions: `docs/adr/0001`–`0010`.

3. **Exchange-rate GDP growth-gap diagnosis** — script `code/diagnose_xr_gap.R` to be created.
   Plan: `docs/superpowers/plans/2026-07-08-exchange-rate-growth-gap-diagnosis.org`
   Spec: `docs/superpowers/specs/2026-07-08-exchange-rate-growth-gap-diagnosis.org`
   Numeric checks gated on the NIOTs object plus exchange-rate/deflator inputs read by
   `get_Erate()`/`get_deflator()` in `code/tmp.R`.

4. **In-code FIXMEs** — 17 markers indexed in `TODOs.org` (§FIXME List), mostly in
   `code/support_functions.R`. Notable: CDD computed as residual (343, 403); identity test
   `CDD + C == Total` (378); potentially problematic method (374); possible input-data /
   area-plot errors (497). Do not treat as part of the three plans above unless asked.

## What can be done now, without the data

- Tier-A parse checks on all `code/*.R` edits.
- Any package scaffolding for `growthdecomp` that is not data-gated (structure, DESCRIPTION,
  NAMESPACE, pure-function migration, structural tests).
- Writing the diagnostic function bodies (verified structurally until inputs arrive).

## Notes

- `.remember/now.md` is empty — no prior session state to recover.
- Recent commits added the specs/plans/ADRs and `pdf_extract.sh`; a CRLF-vs-LF conflict
  warning was recorded (commit 8580ddb).
- CLAUDE.md conventions apply: native pipe `|>`, `ggplot2` → PDF in `figs/`, tables via
  `knitr::kable(booktabs = TRUE, caption=...)`, scripts run from inside `code/`.
- **Do NOT prefix commands with `rtk`** (project CLAUDE.md).

## Suggested skills for the next session

- **superpowers:executing-plans** or **superpowers:subagent-driven-development** — both
  plans explicitly name these as the required sub-skill to execute their checkboxed tasks.
- **superpowers:systematic-debugging** — if resuming the exchange-rate gap diagnosis or the
  Item-3 CDX sign investigation.
- **superpowers:verification-before-completion** — before claiming any task done; note the
  Tier-A vs Tier-B distinction (parse-clean is necessary, not sufficient).
