# Handoff — Spike-regime correction (XR gap diagnosis)

**Date:** 2026-07-21
**Repo:** `/home/gpetrini/Documents/Deflating_WIOD_Tables`
**Branch:** `diagnosis/xr-gap-debug` (pushed to `origin`; HEAD `eff7bf9`)
**Data:** present — `etc/Modelo/NIOTs_resultados.rdata` (75 countries) + `inputs/` are in
place (gitignored). Tier-B runs. Work on this branch in the main checkout, **not** a
worktree (a fresh worktree lacks the gitignored data).

## Goal for the new session

Correct the **spike regime**: detect and splice the one-time GDP-level discontinuities
that inflate `g^IO` in isolated country-years. Start with MMR and NGA. Target: the
break-year `|Gap|` drops into the clean band (< 2 p.p.) without disturbing the other years.

## Context — read these, do not re-derive

- Verdict + full taxonomy: `docs/superpowers/findings/2026-07-21-xr-gap-diagnosis-findings.org`.
  (Gap is NOT exchange rate / NOT inflation; series is real; 56 clean / 12 volatile / 7 spike / 0 offset.)
- Plan (Tasks 1-9, with Task 3 revised per ADR-0011): `docs/superpowers/plans/2026-07-08-exchange-rate-growth-gap-diagnosis.org`.
- Why the diagnosis reads currency inputs directly: `docs/adr/0011-diagnosis-reads-inputs-directly.org`.
- Code: `code/diagnose_xr_gap.R` (`get_gIO`, `get_gOff`, `get_xr_infl`, `build_gap_panel`,
  `classify_series`, `fit_gap_model`, `exclusion_checks`). Panel: `results/diagnostics/gap_panel.rds`.

## The 7 spike countries and their break years

| ISO | Break | Gap | dln_e | Matches −dln_e? |
|-----|------:|----:|------:|-----------------|
| MMR | 2012 | +4.733 | −4.768 | yes (Myanmar FX unification) |
| NGA | 1999 | +1.329 | −1.440 | yes (naira devaluation) |
| MLT | 2008 | +0.860 | −0.780 | yes (euro adoption) |
| CYP | 2008 | +0.538 | −0.467 | yes (euro adoption) |
| BLR | 1996 | −0.300 | −0.138 | **no** — different origin |
| CIV | 1996 | −0.409 | −0.025 | **no** — different origin |
| PAK | 2000 | −0.485 | −0.080 | **no** — different origin |

**Do not assume all 7 are exchange-rate breaks.** Four match `−dln_e` (a currency
re-expression in the raw table at an FX-regime change). Three (BLR, CIV, PAK) are level
discontinuities `dln_e` does **not** explain — likely redenomination or a data-vintage
splice in the raw national tables; root-cause them individually before splicing.

## Where the break lives in code

`g^IO` is the log-difference of the GDP level built in `prepare_data()`
(`code/support_functions.R:~155-168`):
`GDP = (1-C_m)*C_Ft + (1-I_m)*I_Ft + (1-E_m)*E_Ft + (1-G_m)*G_Ft + (1-X_m)*X_Ft`,
then `GDP <- xts(...)`. The discontinuity is a one-time jump in the **raw `Ft`
components** of the supplied NIOT (e.g. MMR: GDP level 20,387 in 2011 → 2,485,941 in 2012,
factor ≈122). The pipeline conversion at `deflate_tables.R:94` is commented out, so the
jump is inherited from the raw data, not introduced by the pipeline.

## First decision the session must make (brainstorm before coding)

**Where to splice.** Two options, genuine trade-off:
1. **Upstream** — correct the raw NIOT `Ft` level at the break. Cleanest conceptually, but
   Stage A is WIP and ADR-0010 excludes its helpers; risks scope creep.
2. **Post-hoc in the diagnosis** — correct the affected `g^IO` observations (e.g. drop the
   single break-year growth and treat the level as spliced across the break). Self-contained
   on this branch; recommended for a first tractable pass.

**Splice method.** For an FX re-expression break, the pre- and post-break level segments are
in different currency bases; splicing means rescaling one segment so the break-year growth
reflects real change, not the unit switch. For the non-FX breaks (BLR/CIV/PAK) the correct
rescale factor is unknown until root-caused.

## Verification

After any splice: re-source `code/diagnose_xr_gap.R`, rebuild `build_gap_panel()`, and check
that each spike country's break-year `|Gap|` falls below ~0.02, that the neighbouring years
are unchanged, and that the robust `fit_gap_model` on the trimmed panel stays ≈0 for both
coefficients. Add a regression check that pins the 7 break-years.

## Suggested skills

- **superpowers:systematic-debugging** — root-cause each raw-data break (especially BLR/CIV/PAK) before fixing.
- **superpowers:brainstorming** — resolve the where-to-splice / splice-method decision before touching code.
- **superpowers:test-driven-development** — write the break-year regression check first, then make it pass.
- **superpowers:verification-before-completion** — Tier-B evidence before claiming any spike fixed.

## Housekeeping

- Do **not** prefix commands with `rtk` (project CLAUDE.md). Run native binaries.
- Commit convention: `[TAG] subject`, trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- Docs in Org-mode; `.gitignore` whitelists `.org` (tracked) but not `.md` (needs `git add -f`).
- Project memory: `~/.claude/projects/-home-gpetrini-Documents-Deflating-WIOD-Tables/memory/open-work-state.md`.
