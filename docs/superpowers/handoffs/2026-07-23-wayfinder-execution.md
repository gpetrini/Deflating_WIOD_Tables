# Handoff — Wayfinder execution of the gap diagnosis (2026-07-23)

**Supersedes** `2026-07-21-gap-diagnosis.md` for the *execution* state (that file
still holds the deeper provenance narrative; read it if a term is unclear).
**Repo:** `/home/gpetrini/Documents/Deflating_WIOD_Tables` — **Branch:** `diagnosis/xr-gap-debug`
**Data:** present and gitignored. Work in the main checkout, not a fresh worktree.

## ACTIVE EFFORT (2026-07-23): wayfinder map #13 — RESUME HERE

Maps #1 (located the gap) and #8 (verified the pipeline) are CLOSED; the Phase B
write-up #7 is DELIVERED. The active effort is **map #13** "characterize the ICIO
nominal-aggregate divergence on-disk, then decide the fetch." Read the map body for its
Destination, Notes (seams + rules), fog, and out-of-scope; it is self-sufficient to work
a ticket cold.

**Issue snapshot (gpetrini/PrivateClaude), 2026-07-23:**

| # | Type | State | Gist |
|---|------|-------|------|
| 1  | map | CLOSED | Located the majority gap in the ICIO nominal aggregate (H1, 95%). |
| 2–6 | — | CLOSED | Threshold 0.5pp; quarantine set; H4 rejected; H2 rejected; H1 located. |
| 7  | spec | **DONE** (35a3793) | Phase B write-up delivered (branch a). `docs/superpowers/writeups/2026-07-23-gap-diagnosis-writeup.org` + `code/writeup_numbers.R`. |
| 8  | map | CLOSED | Pipeline-error audit; destination reached (identity closes to 9e-15 lp). |
| 9  | task | CLOSED (ce05e71) | `prepare_data()` verified, 17/17. `code/verify_prepare_data.R`. |
| 10 | research | CLOSED | Extraction recipe recovered. Findings md 2026-07-23. |
| 11 | task | CLOSED (dfba45d) | Identity closure, 4/4. `code/retest_h1_identity_closure.R`. |
| 12 | grilling | CLOSED | H1 confirmed at 95%, verified-pipeline basis; split left open. |
| 13 | **map** | **OPEN (ACTIVE)** | Characterize the nominal divergence on-disk, then decide the fetch. |
| 14 | task | **OPEN (frontier)** | **2a** income-side vs expenditure-side GDP per raw table → internal or external divergence. |
| 15 | task | **OPEN (frontier)** | **2b** regress `nominal_mm` on structural covariates (import share, openness, size, INVNT). |
| 16 | task | **OPEN (frontier)** | **2c** bound the nominal/deflator split across on-disk benchmark vintages (`get_gOff` repo2018 vs current). |

**Frontier = #14, #15, #16** (all `wayfinder:task`, on-disk, unclaimed, independent/parallel;
no fetch). Fog: the fetch decision 1a/1b (grilling, graduates after 2a–2c); outcome-dependent
localization if 2a finds an internal inconsistency; the minority extension. **Uncommitted/
unpushed:** all this session's commits and GitHub actions are local on `diagnosis/xr-gap-debug`.
The completed-stage record for maps #1/#8 and every seam and verdict is below.

## Standing rules (do not violate)

- No hypothesis decided below **95% confidence**; below the bar, state the evidence
  that would raise it and stop.
- Never act on a diagnostic assumption without stating it first.
- Do **not** prefix commands with `rtk`. Do **not** download data (no fetch authorised).
- `etc/Modelo/` is READ-ONLY reference, never executed. `code/` is the working codebase.
- Commits `[TAG] subject`, trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- `.md` docs are gitignored; commit with `git add -f`.
- One sentence per line in Org docs; no em-dashes; native R pipe `|>`; figures PDF in `figs/`.

## The object

`Gap = g^IO − g^off` per country-year, in log-growth (multiply by 100 for pp).
- `g^IO` — real GDP growth from the deflated OECD ICIO tables, `Σ_j (1−m_j)·F_j`.
- `g^off` — World Bank `NY.GDP.MKTP.KD`, current download, via `test/g_GDP.xlsx` sheet `g_GDP_WB`.

**The feedback loop is an identity, not a test:**
`g^IO = Δln(ICIO nominal, USD) + Δln e − π`. Stage A applies one scalar `e/P` per
country-year, so `e`, `π`, `g^off` are substitutable analytically (no Stage-A re-run,
which is impossible: `deflate_tables.R` is stubbed). The ICIO nominal aggregate is the
one non-substitutable term, reachable only by external comparison (that is H1 / #6).
**Method rule:** an identity does not attribute cause; a large term says *where* the
gap sits, not *what* produced it. Every verdict rests on a substitution or an external
comparison, never on a term's size.

## Wayfinder map and ticket state (gpetrini/PrivateClaude)

Map **#1** "gap-to-zero for the majority, 95% causal verdict on H1/H4". Execution is
in-scope. Destination: a 95% causal account for the **majority** (drive its gap toward
zero), quarantining the minority; every verdict carries the vintage/inter-year caution.

| # | Type | State | Verdict |
|---|------|-------|---------|
| 2 | grilling | **DONE** (692ca34) | Operational gap threshold = **0.5 pp** at the coverage knee. |
| 3 | task | **DONE** (b54729b) | Canonical quarantine set fixed in code + glossary. |
| 4 | task | **DONE** (0ea6103) | H4 (exchange rate) **rejected at 95%** for the majority. |
| 5 | task | **DONE** (b9943ae) | H2 (applied deflator, OECD-covered) **rejected at 95%**. |
| 6 | task | **DONE** (2dba2c4) | H1 **confirmed at 95%** as the *location* of the majority gap: the ICIO nominal aggregate. |

**ROUTE COMPLETE (2026-07-23).** All six tickets closed; the frontier is empty. The two
substitutable channels are rejected for the majority (H4 exchange, H2 deflator) and the
residual is located in the ICIO nominal aggregate at 95% (H1). The majority gap is already
small (median 0.36 pp/yr, ~2% cumulative in level) and bounded by the IO tables, not
removable by any deflation/exchange choice on disk. The map's destination is reached for
the majority. **A second stage is now open (see below).** Also deferred: volatile-country
remediation (COL/TUR/IDN/ARG and the 12 volatile), and a current-vintage current-USD fetch
to split inherited-defect from vintage in H1.

## SECOND STAGE (opened 2026-07-23): pipeline-error audit + write-up

The first map's H1 verdict rests on an **untested assumption**: that
`g^IO = Δln(ICIO nominal, USD) + Δln e − π` is an *exact* property of the Stage-A
pipeline. It was only ever verified to hold *algebraically by construction* inside
`decompose_gap()`, never that `g^IO` is computed correctly from the tables. A Stage-A or
aggregation bug would masquerade as H1. So H1-at-95% is conditional on the pipeline being
correct, and that is now audited before any write-up asserts it.

**Phase B — write-up spec = issue #7 — DONE (2026-07-23, commit 35a3793).** Org-mode
falsification-chain write-up delivered on branch (a) (pipeline confirmed → H1 stands at
95%): `docs/superpowers/writeups/2026-07-23-gap-diagnosis-writeup.org`, with all numbers
generated by the new reproducible layer `code/writeup_numbers.R` (sources the audited
diagnosis + retest scripts, returns each cited table; no transcription; all 8 Org chunks
emit booktabs LaTeX). Prose carries no absolute values (house style); the two existing
figures are reused unrenamed. NOT rendered to PDF here (no Emacs/LaTeX toolchain).

**Phase A — pipeline-error audit = wayfinder map #8.** Destination: reclassify H1
(confirmed at 95%, or defect found + corrected gap), which unblocks #7. Execution is
in-scope. Tickets:

| # | Type | State | What |
|---|------|-------|------|
| 9  | task | **DONE** (ce05e71) | `prepare_data()` correct in isolation from deflation: **17/17 PASS** (`code/verify_prepare_data.R`). `GDP ≡ Ft_total − M_total` (max rel err 4e-16 over 75×26), `M ≡ M_F + M_I`, `M_I = Am·Z·Fn` orientation, `m` scalar-invariant, GDP linear in scalar, `gY` lag/index aligned. No error in the assembly seam; a defect (if any) lives in the raw-table extraction → #11. |
| 10 | research | **DONE** (2026-07-23) | Extraction recipe recovered: raw `*dom.csv` = nominal current-USD-M ICIO NIOT; `Ft_total − M_total = Fn_total − M_I_total` (Fm cancels); exact reconstruction = `sum(Fn) − sum(Am·Z·Fn)` per CSV, caveats (7-sector agg, `OUTPUT`-row GO, `ginv`+zerofill, currency basis). Recipe: `docs/superpowers/findings/2026-07-23-nominal-aggregate-extraction-recipe.md`. |
| 11 | task | **DONE** (dfba45d) | **DECISIVE TEST, 4/4 PASS.** Reconstructed the nominal-USD aggregate independently from raw `inputs/NATIODOMIMP/` for all 1950 c-y; `g^IO = Δln(NomUSD_raw) + Δln e − π` closes cell-for-cell (max resid **8.8e-15 lp** « 1e-8), deflation uniform (4e-16), aggregation composes (4e-15), scalar==e/P exactly on 1196 OCDE c-y. No pipeline artifact; **H1 stands**. Suite: `code/retest_h1_identity_closure.R`. |
| 12 | grilling | **DONE** (2026-07-23) | Terminal decision via `/grilling`: **H1 confirmed at 95%**, basis upgraded elimination→verified-pipeline (not raised). Inherited-defect-vs-vintage split left open (fetch named, OOS). Majority-scoped verdict; pipeline correctness universal. 3 typed falsifiers. Majority account survives unchanged. Findings §"H1 reclassified". **Map #8 destination reached; Phase B #7 unblocked on branch (a).** |

**Key enabling fact:** the raw nominal ICIO tables ARE on disk (`inputs/NATIODOMIMP/`,
1976 files ≈ 76×26), so the decisive identity closure (#11) needs **no fetch**.
**Seam:** `code/support_functions.R::prepare_data()` — the single function turning the
supplied tables into `g^IO` (`GDP = Σ(1−m_j)F_j`, which reduces to `Ft_total − M_total`;
`m = M/Ft`; `M = M_F + M_I`; `M_I = Am·Z·Fn`). No `testthat` in the repo; verify via
standalone scripts like `code/retest_*.R`. `code/tests.R` is an exploratory script, not a
test suite.
**Out of scope on the map:** executing/fixing Stage-A in `etc/Modelo/` (read-only,
ADR-0012), any fetch, the volatile remediation, and the write-up itself (#7).
**Work-through:** one ticket per session, claim (assign) before work, clear context
between. Frontier now: **EMPTY** — #9, #10, #11, #12 all closed 2026-07-23. **Map #8's
destination is reached:** the pipeline is verified end-to-end (identity closes to 9e-15
log points), H1 is confirmed at 95% on a verified-pipeline basis, and the majority-gap
account survives unchanged. **Next effort (not this map): Phase B write-up spec #7**,
now unblocked on its pipeline-confirmed branch (a); start it in its own session via the
spec. Deferred beyond both maps: the current-vintage current-USD fetch (splits
inherited-defect from vintage) and the volatile-country remediation.

### Single sources of truth (in `code/diagnose_xr_gap.R`)

- **Quarantine (H5):** `QUARANTINE_SPIKES` = MMR, NGA, MLT, CYP, BLR, CIV, PAK;
  `QUARANTINE_DRIFTERS` = UKR, BRN, KAZ, VNM, EGY; `QUARANTINE_ISO` (union);
  `QUARANTINE_YEARS` = 2020L. `majority_panel()` / `quarantine_panel()` = exact partition.
  Majority = **63 countries, 1996-2019, 1512 country-years** (MMR absent from panel).
  Every pooled majority statistic MUST call `majority_panel()`.
- **Threshold (#2):** `GAP_THRESHOLD_PP` = 0.5; `gap_coverage()` sweeps it; curve at
  `figs/gap_coverage_curve.pdf`. At 0.5 pp: 62% of majority country-years, 70% of
  countries (by median year) clear the gap. Carries the H6 ~1 pp vintage band.
- **Substitution loop:** replace a term through the identity, recompute the gap, no
  Stage-A re-run. `Gap_new = Gap + (Δln e_new − Δln e_old)` for exchange;
  `Gap_new = Gap − (π_new − π_old)` for deflator.
- **Decomposition:** `decompose_gap()` reads the independent WB current-USD CD file
  and computes `nominal_mm = (gY_io + dln_e + pi) − dln_cd`,
  `deflator_mm = dln_cd − gY_off − dln_e − pi`; `nominal_mm + deflator_mm ≡ Gap`
  (verified 2e-16). `attribute_gap()` = covariance shares.

### Reproducibility scripts (in `code/`)

- `reconstruct_and_retest.R` — rebuilds both panels, re-runs the discriminating fit.
- `retest_h4_xr_swap.R` — H4 exchange-rate substitution (#4).
- `retest_h2_deflator_swap.R` — H2 deflator substitution, country level (#5).
- Panels persisted: `results/diagnostics/gap_panel.rds` (current, 1850 obs, 1996-2020),
  `gap_panel_repo2018.rds` (legacy, 1996-2016).

### Verdicts in one line each (for citation)

- **H4 rejected (95%):** WB `PA.NUS.FCRF` swapped through the identity. Clean moved
  subset (15 euro-adoption reconversion break-years removed a priori) mean +17.1% /
  median +12.6%, both < 20% pre-registered bound; the swap *raises* the gap, so the
  applied rate is not the error. EMU substitutes need the euro break-year filter.
- **H2 rejected (95%):** WB `NY.GDP.DEFL.ZS` swapped on OECD-covered majority (41
  moved). Only 2 improve >20% (BRA, IRL), 35 unchanged, 4 worsen (COL, TUR, IDN, ARG,
  the highest-inflation countries with annual timing disagreement; COL identical mean
  π yet +781%). The 4 flagged for volatile-country remediation, not a fix.
- **Consequence:** both substitutable channels (exchange, deflator) rejected for the
  majority, so by elimination the surviving majority gap sits in the **ICIO nominal
  aggregate** — exactly what #6 tests directly.

## Ticket #6 — the concrete plan (execute this)

**Question:** does the surviving majority gap track the difference between the ICIO
nominal-dollar aggregate and official nominal GDP *in levels*, per country-year?

**CRITICAL TRAP (verified 2026-07-23):** do NOT reconstruct `dln_cd` (the official
side) from the pipeline's own P and e — that makes `deflator_mm → 0` and `nominal_mm ≡
Gap` mechanically (circular). Use the **independent** WB current-USD file
`inputs/world_bank/GDP_current/API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv` (2018 vintage,
data to **2016**). `decompose_gap()` already does this correctly: `(gY_io + dln_e + pi)`
recovers the *raw* ICIO nominal aggregate (independent of the deflation choice), and
`dln_cd` is the independent official nominal. That comparison is the legitimate H1 test.

**Steps:**
1. `d <- decompose_gap(majority_panel())` then restrict to `Year <= 2016` (CD window).
   Confirm `nominal_mm + deflator_mm ≡ Gap` (identity check, expect ~2e-16).
2. **Levels:** per country, build two normalized nominal-USD level indices to a common
   base year: ICIO-nominal = `cumprod(exp(gY_io + dln_e + pi))`, official = the WB CD
   level from the file. Show the two trajectories diverge and that the cumulative
   divergence equals cumulative `nominal_mm`.
3. **Direct per-country-year test:** show `Gap ≈ nominal_mm` on the majority ∩ 1996-2016
   (i.e. `deflator_mm` is small there, consistent with H4/H2 having cleared e and P).
   Report the share of majority country-years where |deflator_mm| < |nominal_mm|, and
   the country-level magnitude of the nominal-aggregate divergence.
4. **Verdict framing:** if the residual tracks the ICIO-vs-official nominal difference,
   H1 is confirmed as *where* the gap sits — but distinguish an **inherited defect**
   (the balanced ICIO tables ≠ transcribed official nominal, unfixable here) from a
   **vintage** artifact (WB CD 2018 vs ICIO 2023 volumes, H6). The level test alone
   cannot fully separate these; state that limit. A 95% verdict that the residual *is*
   the nominal aggregate is reachable; a 95% verdict on inherited-defect-vs-vintage may
   not be, given the CD file is a single 2018 vintage. Say so explicitly.

**Pre-register before running** (write it in the script): the criterion for "the
residual tracks the nominal aggregate" (e.g. median |Gap − nominal_mm| below some pp,
and the share of country-years where the nominal term dominates the deflator term).

**On completion:** post resolution comment on #6, close it, append one line to map #1
Decisions-so-far, update `open-work-state.md` memory, update this handoff's #6 row.
Write findings to `docs/superpowers/findings/2026-07-21-xr-gap-diagnosis-findings.org`.
When #6 closes, the frontier is empty and the map's destination is reached for the
majority — hand off to `/to-spec` only if a paper write-up is then wanted.

## Full record

`docs/superpowers/findings/2026-07-21-xr-gap-diagnosis-findings.org` (H4 and H2 sections
appended 2026-07-23). Plan: `docs/superpowers/plans/2026-07-21-gap-causal-diagnosis.org`.
Glossary: `CONTEXT.org` (terms: Operational gap threshold, Quarantine set, and others).
Memory: `open-work-state.md`.
