# Handoff — Zero and diagnose the IO-vs-official GDP growth gap

**Date:** 2026-07-21
**Repo:** `/home/gpetrini/Documents/Deflating_WIOD_Tables`
**Branch:** `diagnosis/xr-gap-debug` (based on `master`; pushed to `origin` through
commit `466b318`; local commits after that — through `48283c6` — are **not yet pushed**).
**Data:** present and gitignored. Work in the main checkout on this branch, **not** a
fresh worktree (a new worktree lacks the gitignored data).

## The goal

Drive the gap `Gap = g^IO − g^off` to zero across the base and explain what causes
it. Every non-trivial gap is a defect (the user's standard: "o gap deveria ser bem
próximo de zero para toda a base"). Work each hypothesis to a **95% confidence**
bar: state the hypothesis, state the specific evidence that would raise it to 95%,
gather it, record the finding. Use `/grill-with-docs` and `/diagnose` to align
before large steps.

- `g^IO` = log-difference of the GDP level built by `prepare_data()` from the
  deflated tables.
- `g^off` = log-difference of the World Bank constant series `NY.GDP.MKTP.KD`.

## Repository structure you must hold in mind

Read `docs/deflation-pipeline-onboarding.org` first — it is the authoritative map.
The essentials:

- **Data source is OECD ICIO, not WIOD.** The analysed tables are the OECD
  Inter-Country Input-Output tables in `inputs/NATIODOMIMP/` (1995–2020, 76
  countries). The repo *name* and upstream code say "WIOD" — that is **heritage**
  (the repo is a fork of Quentin Perrier's WIOD-deflation project). WIOD lives only
  in `results/` (n=43), `code/price_deflator.R`, `code/checks.R`, and as the
  licensing-permitted **package sample** (`niot_wiod`, ADR-0003). **Never call the
  analysed volumes "WIOD."** The variable `WIOD_countries` in `global_variables.R`
  is misnamed; it holds OECD codes.
- **Two pipeline versions:**
  - *Original* `etc/Modelo/base/*.R`, run by `etc/Modelo/compilacao.R` — runnable,
    produced the loaded `etc/Modelo/NIOTs_resultados.rdata` (n=75). This is Stage A.
  - *Refactor* `code/*.R` — `support_functions.R` (analysis) is **complete and in
    use**; `deflate_tables.R` (Stage-A deflation) is **stubbed** (FIXME l.82-85,
    deflator commented l.94/103), so it would emit undeflated tables. `tmp.R` is an
    older draft (syntax error l.391).
- **Deflation (Stage A):** raw OECD cells are current USD; each cell is multiplied
  by one uniform scalar `deflator_geral = exchange_rate(LCU/USD) / deflator_GDP(base 1995)`,
  giving constant-1995 LCU. **No component-specific deflators.**
- **Final outputs:** `reports/*.pdf` come from `code/generate_reports.R` →
  `global_variables.R` (loads OECD `NIOTs`) + `support_functions.R`
  (`prepare_data → decompose_growth → group_plots`, writes at `support_functions.R:1601`).
  Same codebase and same `prepare_data` object as the diagnosis.
- **Growth-convention mismatch (unreconciled):** the reports use *simple* growth
  `diff(GDP)/lag(GDP)` (`support_functions.R:202`); the diagnosis and `g^off` use
  *log* growth. Same GDP level, diverge second-order (~6.8 p.p. for UKR, huge across
  breaks). The gap you report is a **log** gap.
- **Inputs:** `inputs/*.xlsx` are byte-identical (md5) to the `etc/Modelo/*.xlsx`
  the deflation used. `results/NIOTs_resultados.rdata` (n=43) is legacy WIOD — never
  load it. Command rule: **do not prefix anything with `rtk`**; run native binaries.

## Diagnosis state (do not re-derive — see the findings doc)

Full record: `docs/superpowers/findings/2026-07-21-xr-gap-diagnosis-findings.org`.

- **Verdict:** the gap is **not** exchange rate and **not** inflation; the supplied
  series is **real** (`β_π≈0`, `β_e≈0` in the robust discriminating fit). This
  refutes the original conjecture.
- **Taxonomy (75 countries):** 56 clean, 12 volatile (mean≈0, sd 3–7 p.p.), 7 spike
  (one-time breaks), 0 persistent offset.
- **Spike regime is exogenous and deprioritized** (user's call): a one-time
  exchange-rate re-expression injected through the deflation's `exchange_rate` term
  (verified: MMR rate 5.44→640.65 in 2012). 6 of 7 are clean single-break spikes;
  MMR is a hybrid (spike + volatile pre-2012). Splice deferred.
- **Pervasive gap (68 non-spike countries):** unbiased (mean −0.0009), sd 0.021,
  mean-reverting, concentrated by country. Ruled out (source-agnostic tests, stay
  ruled out): import adjustment, index-number formula, component dispersion, base
  year. It is **source data quality** in the OECD volumes/deflators — the pipeline
  is validated by the well-measured economies.
- **Gap decomposition (exact, `decompose_gap()`/`attribute_gap()` in
  `code/diagnose_xr_gap.R`):** the gap = **45% nominal-aggregate mismatch** (OECD
  final demand vs WB nominal GDP) + **55% deflator mismatch** (applied deflator vs
  official implicit deflator), by covariance-with-Gap attribution. The cumulative
  **drifters are deflator-driven**: UKR 90%, BRN 98%, KAZ 89%, VNM 66%, EGY 68%;
  year-to-year dispersion is nominal-aggregate (BGR, ROU, RUS, TUN, MAR, NZL).

## Open hypotheses and the 95%-confidence criteria

See the findings doc "Hypothesis review after the provenance correction" for the
full statements. In brief:

- **H-F (deflator mismatch).** 95% needs: form the official implicit deflator and
  show the applied OECD/WB deflator diverges from it, correlated with the gap, for
  the deflator-driven countries.
- **H-G (price-concept mismatch), elevated.** Three price bases — OECD basic prices,
  an applied deflator labelled consumer prices, WB market-price GDP. 95% needs:
  re-deflate one high-dispersion country with the market-price GDP deflator and show
  the gap contracts.
- **H-vintage (new).** OECD ICIO 2023 SNA benchmark vs the WB GDP vintage may differ;
  this would show as persistent per-country drift (the drifter signature). 95% needs:
  separate a vintage/benchmark step from a genuine deflator error before claiming
  either.
- **Exchange-rate source (not fully dead).** The deflation multiplies by the rate, so
  a rate-source discrepancy (OECD/WB rate vs the rate implicit in the official
  series) is bundled inside `deflator_mm`; separate it with the local-currency series.
- **H-nominal.** OECD ICIO final demand (LCU) may not track official nominal GDP.

## Immediate next step

Run the **targeted deflator test on UKR and BRN** (the clearest deflator-driven
drifters). Replace the applied OECD/WB GDP deflator with the official national
market-price GDP deflator and check whether `deflator_mm` collapses (H-F/H-G), while
watching for a vintage step (H-vintage).

**Blocker / additional info required:** cleanly separating the deflator mismatch from
the exchange-rate-source discrepancy needs the World Bank **local-currency** GDP
series `NY.GDP.MKTP.CN` (current LCU) and `NY.GDP.MKTP.KN` (constant LCU), or the WB
GDP-deflator indicator. **These are not on disk** (only `NY.GDP.MKTP.KD` and
`NY.GDP.MKTP.CD`, both USD, are present). Fetch them from the World Bank, or
reconstruct via the exchange rate and flag the residual contamination.

## Reproduce quickly

From `code/`: `source("diagnose_xr_gap.R")`, then `build_gap_panel()`,
`decompose_gap()`, `attribute_gap()`. Panel persisted at
`results/diagnostics/gap_panel.rds`. Tier-B (numeric) runs immediately on this branch.

## Housekeeping

- Commit convention: `[TAG] subject`, trailer
  `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- Docs are Org-mode; `.gitignore` whitelists `.org` but not `.md` — handoffs need
  `git add -f`.
- Durable project memory:
  `~/.claude/projects/-home-gpetrini-Documents-Deflating-WIOD-Tables/memory/open-work-state.md`.
- Do **not** "fix" the WIOD references in ADR-0003 or the growthdecomp plan/spec —
  the shipped WIOD sample is a licensing-driven fixture and is correct.

## Suggested skills

- `superpowers:systematic-debugging` — root-cause the deflator vs vintage question
  before any fix.
- `grill-with-docs` — align on the H-F/H-G/H-vintage test design and the WB
  local-currency data decision before coding.
- `superpowers:verification-before-completion` — Tier-B evidence (the decomposition
  identity holds to machine precision) before claiming a term is resolved.
- `superpowers:test-driven-development` — pin the decomposition identity and the
  deflator-driven country shares with a regression check.
