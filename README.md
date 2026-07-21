# Growth Decomposition from Deflated Input-Output Tables

This repository decomposes the growth of real GDP demand components into domestic and import-content contributions across countries.
It is a fork of a project that deflates the World Input-Output Database (WIOD, 2016 release) tables, and it retains that deflation code as its origin.

The tables it analyses are the OECD Inter-Country Input-Output (ICIO) tables in `inputs/NATIODOMIMP/`, 1995 to 2020, aggregated to seven sectors.
WIOD is heritage: it survives in `results/`, in the heritage deflation scripts, and as the redistributable package sample, and it is not the source of the analysed volumes.
For each demand component (private consumption, investment, government, exports, inventories) the growth contribution is computed under four accounting methods: Net Exports, Attribution, Import Content, and Average Import Content.
The methods are then compared through per-country figures, tables, and distance metrics.

The code is written in R.

## Pipeline

The reporting stage is driven by a single entry point that sources the other scripts.
All scripts assume the working directory is `code/`, since every path is relative to it.

### Stage B — Growth decomposition and reporting (main pipeline)

Run `generate_reports.R`.
It is the one script that composes the pipeline: it sources `global_variables.R` and `support_functions.R`, then runs `prepare_data()`, `decompose_growth()`, and `group_plots()` in sequence.
Figures are written to `figs/` and tables to `tabs/`.
This stage requires the precomputed object `etc/Modelo/NIOTs_resultados.rdata`, produced by Stage A.

```r
setwd("code")
source("generate_reports.R")
```

### Stage A — Build the national IO tables (data construction)

Stage A does not run in this repository.
The supplied object `etc/Modelo/NIOTs_resultados.rdata` was built by the co-author's project in `etc/Modelo/`, which is kept here as read-only reference and has never been executed on this machine (ADR-0012).
Each raw current-dollar cell is multiplied by one uniform scalar per country-year, `exchange_rate(LCU/USD) / deflator_GDP(1995 = 1)`, yielding constant-1995 local currency.
There are no component-specific deflators.

`deflate_tables.R` is the reimplementation of that stage inside `code/`.
Its deflation is not restored — a FIXME at lines 82 to 85 with the deflator commented at 94 and 103 — so it currently emits undeflated tables and cannot reproduce the supplied object.
Restoring it is the only route to regenerating `NIOTs_resultados.rdata`; running `etc/Modelo/` is not.

### Paper assembly

`Figs_Tabs.org` collects the figures and tables from `figs/` and `tabs/` into the paper, exported to LaTeX from Emacs Org-mode.

### Original WIOD deflation (heritage)

This is the deflation the repository was forked from; it is retained for reference and produces the deflated WIOD tables in `results/`.

1. Copy the WIOD tables, in R format, to `inputs/WIOD_Nov16/R_Files`.
   The tables are at http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip.
2. Run `price_deflator.R` to build the sectoral price deflators, then the deflation routine.
3. Run `checks.R` to validate the current-versus-constant price identities, then knit `documentation/documentation.Rmd`.

## R files

| File | Run directly | Role |
|------|:---:|------|
| `generate_reports.R` | yes | Stage-B entry point; sources the pipeline and produces all figures and tables. |
| `deflate_tables.R` | yes | Stage-A reimplementation; deflation not yet restored, so it emits undeflated tables. |
| `diagnose_xr_gap.R` | yes | Diagnosis of the gap between IO-computed and official real GDP growth. |
| `tmp.R` | no | Earlier draft; does not parse (syntax error at line 391). Excluded by ADR-0010 and ADR-0011; do not source. |
| `price_deflator.R` | yes | Heritage: builds WIOD sectoral price deflators. |
| `checks.R` | yes | Heritage: validates the deflated WIOD tables. |
| `global_variables.R` | sourced | Loads `NIOTs_resultados.rdata`; defines countries, years, and country groups. |
| `support_functions.R` | sourced | Core library: `prepare_data`, `decompose_growth`, `group_plots`, plotting and metrics. |
| `GRAS.R` | sourced | Generalized RAS matrix balancing (Temurshoev et al. 2013). |
| `tests.R` | scratch | Exercises the metric functions outside `group_plots`; not a formal test suite. |
| `deprecated.R` | none | Superseded per-country report generator; do not extend. |

## Required data

The following inputs are not committed and must be provided locally.

- `etc/Modelo/NIOTs_resultados.rdata` — output of Stage A, supplied by the co-author; blocks Stage B until present.
- `inputs/NATIODOMIMP/` — OECD ICIO national tables; input to Stage A.
- `inputs/WIOD_Nov16/R_Files/` — the `WIOT{2008..2014}_October16_ROW.RData` files for the heritage deflation.
- `test/g_GDP.xlsx` — official World Bank growth (`NY.GDP.MKTP.KD`, 1996 to 2020) alongside the pipeline's own, used as the diagnosis benchmark.

## Repository layout

| Path | Role |
|------|------|
| `code/` | The working codebase. All new work goes here; every path is relative to it. |
| `inputs/` | Canonical data tree, and a superset of the spreadsheets duplicated inside `etc/Modelo/`. |
| `etc/Modelo/` | Read-only reference: the co-author's Stage A, never run here (ADR-0012). |
| `results/`, `objs/`, `figs/`, `tabs/`, `reports/` | Products. `results/` also holds the heritage WIOD output. |
| `docs/` | `adr/` for decisions, `superpowers/` for plans, findings and handoffs. |
| `test/` | Benchmark data, not a test suite; the exercises live in `code/tests.R`. |

Note that `.gitignore` excludes `*.*` and whitelists only `.org`, `.tex`, `.R`, `.txt` and `.sh`, so data, PDFs and Markdown are untracked; committing a `.md` requires `git add -f`.

## Known caveat

The World Bank benchmark committed under `inputs/world_bank/` is the 2018-03-01 release and ends in 2016.
`get_gOff()` therefore defaults to the current benchmark in `test/g_GDP.xlsx`, and reaches the 2018 release only through `get_gOff("repo2018")`, which is retained so that the revision between the two releases stays measurable.
The gap decomposition in `decompose_gap()` still reads the World Bank current-dollar file and so remains capped at 2016.
