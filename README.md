# Growth Decomposition from Deflated Input-Output Tables

This repository decomposes the growth of real GDP demand components into domestic and import-content contributions across countries.
It is a fork of a project that deflates the World Input-Output Database (WIOD, 2016 release) tables, and it retains that deflation code as its origin.

The decomposition operates on national input-output tables aggregated to seven sectors over 1995 to 2020.
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

Run `deflate_tables.R` together with `tmp.R`.
They read the OECD national domestic and import tables from `inputs/NATIODOMIMP/`, deflate and convert them to constant local prices, aggregate to seven sectors, and build the nested list `NIOTs` serialized to `etc/Modelo/NIOTs_resultados.rdata`.
This stage is work in progress.

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
| `deflate_tables.R` | yes | Stage-A construction of the `NIOTs` object (work in progress). |
| `tmp.R` | yes | Stage-A helpers still being promoted into the pipeline. |
| `price_deflator.R` | yes | Heritage: builds WIOD sectoral price deflators. |
| `checks.R` | yes | Heritage: validates the deflated WIOD tables. |
| `global_variables.R` | sourced | Loads `NIOTs_resultados.rdata`; defines countries, years, and country groups. |
| `support_functions.R` | sourced | Core library: `prepare_data`, `decompose_growth`, `group_plots`, plotting and metrics. |
| `GRAS.R` | sourced | Generalized RAS matrix balancing (Temurshoev et al. 2013). |
| `tests.R` | scratch | Exercises the metric functions outside `group_plots`; not a formal test suite. |
| `deprecated.R` | none | Superseded per-country report generator; do not extend. |

## Required data

The following inputs are not committed and must be provided locally.

- `etc/Modelo/NIOTs_resultados.rdata` — output of Stage A; blocks Stage B until present.
- `inputs/NATIODOMIMP/` — OECD national IO tables; input to Stage A.
- `inputs/WIOD_Nov16/R_Files/` — the `WIOT{2008..2014}_October16_ROW.RData` files for the heritage deflation.
