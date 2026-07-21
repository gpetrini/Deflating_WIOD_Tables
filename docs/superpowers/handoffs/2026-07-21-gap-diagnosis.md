# Handoff — IO-vs-official GDP growth gap

**Date:** 2026-07-21 (supersedes and replaces all earlier handoffs, which were deleted)
**Repo:** `/home/gpetrini/Documents/Deflating_WIOD_Tables`
**Branch:** `diagnosis/xr-gap-debug`
**Data:** present and gitignored. Work in the main checkout on this branch, **not**
a fresh worktree (a new worktree lacks the gitignored data).

## Standing rules from the user

- No hypothesis is decided below **95% confidence**. Below the bar, state what
  evidence would raise it and stop.
- Never act on a diagnostic assumption without stating it first.
- Do not prefix commands with `rtk`.

## The object

`Gap = g^IO − g^off` per country-year.

- `g^IO` — real GDP growth from the deflated OECD ICIO tables, built as
  `Σ_j (1−m_j)·F_j` over C, I, G, X, E (`support_functions.R:157-161`).
- `g^off` — World Bank constant-dollar `NY.GDP.MKTP.KD`.

**Repository layout (ADR-0012).** `code/` is the working codebase — all new work
goes there. `etc/Modelo/` is **read-only reference**: the co-author's Stage A,
never executed on this machine, kept for the refactor. Its `compilacao.R` mixes
two working directories and comments out the line writing
`NIOTs_resultados.rdata` — **these are not defects to repair**. `inputs/` is the
canonical data tree; the ~90 MB duplicated inside `etc/Modelo/` (15 byte-identical
spreadsheets plus `NATIODOMIMP` and `NATIOTTL`) is accepted debt. Regenerating the
NIOTs object means restoring the deflation in `code/deflate_tables.R`, never
running `etc/Modelo/`.

Source is **OECD ICIO** (`inputs/NATIODOMIMP/`, 1995–2020, 76 countries). The repo
name and upstream code say "WIOD"; that is heritage. WIOD survives only in
`results/` (n=43), `code/price_deflator.R`, `code/checks.R`, and the licensing
sample (ADR-0003). `WIOD_countries` in `global_variables.R` is misnamed — it holds
OECD codes. Never call the analysed volumes "WIOD". Do not "fix" the WIOD
references in ADR-0003 or the growthdecomp plan; that fixture is correct.

Read `CONTEXT.org` for the glossary and `docs/deflation-pipeline-onboarding.org`
for the pipeline map before touching anything.

## What is settled

- **The panels reconcile.** `test/g_GDP.xlsx` (74 countries, 1996–2020, simple
  growth in pp) and `results/diagnostics/gap_panel.rds` measure the same `g^IO`:
  median absolute difference 0.013 pp, consistent with the sheet's 2-decimal
  rounding. The `dif_g_GDP` identity holds exactly. An **earlier revision of that
  xlsx was erroneous** (median 1.3 pp off) and was corrected by the user; nothing
  was concluded from it.
- **The FX/inflation conjecture is refuted** (`β_e≈0`, `β_π≈0`); the supplied
  series is real. Source-agnostic refutations also stand: index-number formula,
  base year, component dispersion, import adjustment (it *reduces* the gap).
- **Deflator provenance.** OECD side is Economic Outlook 116, *GDP, market prices,
  deflator*, rebased 1995=1, covering 47 countries. WB fallback is
  `NY.GDP.DEFL.ZS`, in `inputs/deflator_GDP_WB.xls`, rebased 1995=1, covering all
  266 entities 1995–2020.

## Corrections to the previous record — do not reintroduce these

- The comment at `etc/Modelo/base/deflator.R:5` calls the OECD series a
  *consumer-price* measure. **It is wrong** — the file header says market prices.
  H-G was elevated on that comment and is largely refuted.
- The previous handoff claimed the WB GDP-deflator indicator was **not on disk**.
  **It is on disk.** No World Bank fetch is required to run H-F. Fetching
  `NY.GDP.MKTP.CN`/`KN` adds nothing to source discrimination, since
  `NY.GDP.DEFL.ZS ≡ CN/KN`; it would only isolate the exchange-rate channel.
- H-D ("the residual is source data quality") was restated as a settled verdict.
  **It is at ~90% and is open.** The phrase also conflates two things now
  separated in `CONTEXT.org`: an *inherited defect* (unfixable here) versus an
  *applied-deflator choice* (fixable here). Its evidence cannot distinguish them.

## STATE: the spec has been executed; the plan carries the hypotheses

`docs/superpowers/specs/2026-07-21-gap-diagnosis-revised.org` has been run except
for Task 6. Results are in the findings doc, section "Execution on the current
benchmark". **The hypothesis state, each confidence level, and the route to 95%
for each, are in `docs/superpowers/plans/2026-07-21-gap-causal-diagnosis.org` —
read that before proposing any next step.**

**The feedback loop is an identity, not a test.** Stage A applies one scalar
`e/P` per country-year and GDP is a linear combination of components all carrying
it, so `g^IO = Δln(ICIO nominal USD) + Δln e − π`. Any exchange rate or deflator
can be substituted analytically without re-running Stage A — which matters
because `deflate_tables.R` is stubbed. The ICIO nominal aggregate is the one term
that cannot be substituted, so H1 is unreachable by this loop.

**Next test, cheapest and unrun: substitute the exchange rate series.** `Δln e`
enters `g^IO` with coefficient one; the old `β_e≈0` regression answers a
different question and does not exclude an error in the rate's level or dating.
Both series are on disk.

Withheld at 90%, do not record as settled: the deflator swap (Task 5) lands
inside the pre-registered refutation region (+9% mean, +1% median, threshold 20%)
but the pooled mean spans −32% (BRA) to +303% (COL), so the group verdict does
not follow from it.

## Historical: the benchmark was stale and the panel ended in 2016

`inputs/world_bank/GDP_constant/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv` carries
`Last Updated Date` **2018-03-01** and holds data only to **2016**. The same is
true of the current-dollar file used by `decompose_gap()`. Therefore
`results/diagnostics/gap_panel.rds` spans **1996–2016**, and 2017–2020 have never
been examined even though the tables cover them. Any statement that the analysis
window is 1995–2020 describes the tables, not the diagnosis.

The maintainer confirms `g_GDP_WB` is the same indicator, `NY.GDP.MKTP.KD`,
downloaded 2026-07-14. Same indicator, two releases ~8 years apart — so the
benchmark difference (mean −0.019 pp, **sd 1.07 pp**, against a gap of median
0.395 pp) is a **vintage difference**. Above 95%. It concentrates by country —
CIV 18 of 25 years, LUX 16, MLT 14, ROU 12, KHM 11, SAU 11, BRN 9 — with
correlation 0.014 against the gap, so it moves individual country-years without
creating gap systematically.

**DONE.** `get_gOff(vintage=)` now defaults to `test/g_GDP.xlsx` sheet `g_GDP_WB`
and keeps the 2018 release behind `vintage="repo2018"`. Panel: 1850 country-years,
74 countries, 1996–2020. Myanmar is the one country lost, and it is the hybrid
spike case. The revision explains >90% of the former gap for SAU/LVA/LUX but
moves the aggregate only 1.243 → 1.134 pp. An exhaustive search
established it is the only source of official GDP on disk reaching 2020 — the WB
CSVs and every UN SNA file stop at 2016; `ICESHRE.csv` is OECD import content;
the `*_original` WB extracts hold the deflator only; `tmp/` and `outputs/` do not
exist despite being referenced in `code/*.R`. **Do not download anything.**

Re-estimate the taxonomy, the decomposition and the country attributions on the
full window before attributing any of the gap to deflators. Until then an unknown
share of the measured gap for the concentrated countries is benchmark revision.

## Next test, designed and pre-committed but NOT run

Paired deflator swap on the **47 OECD-covered countries**, which hold two
independent LCU GDP deflators on the same base and window (EO 116 vs
`NY.GDP.DEFL.ZS`). No new data needed.

**Pre-registered decision rule** (fixed before running, do not renegotiate after
seeing results): change in mean|Gap| **< ~20%** refutes the applied-deflator
channel for these countries; **> ~50%** establishes it; in between, no claim.

Known limits, to be stated with any result: the test conflates provider and
vintage and cannot separate them; and it shares a provider with the benchmark, so
part of any contraction is mechanical rather than evidence of correctness.

**Blocking constraint:** every drifter (UKR, BRN, KAZ, VNM, EGY) and every spike
country is a WB-fallback country, absent from the OECD deflator. Deflator
provenance is collinear with drift, so H-F **cannot** be tested by swapping
sources within the drifter set. A genuine third source is required there. On disk:
`inputs/price_deflators/unsd_snaAma_GDP_ImplicitPriceDeflator_USD.txt` (UN SNA
AMA, 245 countries, covers all drifters) — but it ends in **2016** and is
**USD-based**, so it re-injects the exchange rate. IMF WEO would be LCU-based and
cover 1995–2020, at the cost of an external fetch the user has not authorised.

## Still open

- **H-nominal / H-F** jointly explain the gap (45%/55% by covariance attribution
  on the reconciled panel). Deflator mismatch drives the cumulative drifters
  (UKR 90%, BRN 98%, KAZ 89%, VNM 66%, EGY 68%); nominal-aggregate mismatch drives
  the year-to-year dispersion (BGR, ROU, RUS, TUN, MAR, NZL).
- **H-vintage.** Three vintages sit in one identity: EO 116 deflator, ICIO 2023
  volumes, a WB GDP series. Separate a benchmark step from a genuine deflator
  error before claiming either.
- **Spike regime** (7 countries) — deferred by the user; exogenous re-expression
  entering through `exchange_rate` (MMR verified: 5.44 → 640.65 in 2012). 6 are
  clean single breaks; MMR is a hybrid.
- **Growth-convention mismatch.** Reports use simple growth
  (`support_functions.R:202`); the diagnosis uses log. Same GDP level, diverging
  at second order. Unreconciled.

## Reproduce

From `code/`: `source("diagnose_xr_gap.R")`, then `build_gap_panel()`,
`decompose_gap()`, `attribute_gap()`. Panel at `results/diagnostics/gap_panel.rds`.

## Housekeeping

- Full record: `docs/superpowers/findings/2026-07-21-xr-gap-diagnosis-findings.org`.
  Its "Panel reconciliation and provenance corrections" section supersedes earlier
  sections where they conflict.
- Commits: `[TAG] subject`, trailer
  `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- Docs are Org-mode; `.gitignore` whitelists `.org` but not `.md` — handoffs need
  `git add -f`.
