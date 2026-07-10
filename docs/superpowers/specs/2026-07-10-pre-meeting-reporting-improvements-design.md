# Pre-meeting reporting improvements (Stage B)

Date: 2026-07-10
Status: Approved (design)
Scope: reporting/plotting stage only (`code/support_functions.R`, `code/global_variables.R`).
No Stage-A data rebuild; `etc/Modelo/NIOTs_resultados.rdata` is consumed unchanged.

## Purpose

A batch of independent refinements to the growth-decomposition reports, to be completed before the next meeting.
Eight items were raised; one is documented but deferred, the rest are implemented.
Each item is small and self-contained; they share only the reporting stage.

## Vocabulary (grounding)

The decomposition (`decompose_growth`) produces, per country and per method, contributions that are fractions of GDP (a weight times a growth rate).
Multiplied by 100 they read as percentage points of GDP growth.

- `CDD` — domestic content contribution.
- `CDX` — external-sector contribution.
- `CDI` — import-content term (defined precisely in Item 7).
- Under *Import Content*: `CDX = CDI + (1 - imp_X) * lag(wei_X) * grw_X`, with `CDI = Σ_v [ -lag(imp_v) * lag(wei_v) * gms_v ]` summed over all demand components.
- Under *Average Import Content*: `CDX = Σ_v [ -lag(m̄) * lag(wei_v) * gM ] + (1 - m̄) * lag(wei_X) * grw_X`, with `m̄` the aggregate import share and `gM` its growth.

The two methods are not algebraically equivalent: *Import Content* uses component-specific import shares and import-growth rates, *Average Import Content* uses a single aggregate import share and its growth.

## Items

### Item 7 — Annotate the CDI definition (do first)

CDI is referenced throughout but never defined in the code.
It grounds Items 3, 6, and 8, so it is done first.

Add a comment block at each CDI definition site in `decompose_growth`:

- Import Content branch (`support_functions.R` ~line 268): `CDI = Σ_v [ -lag(imp_v) * lag(wei_v) * gms_v ]` — the negative aggregate import-content leakage across all demand components; negative because imports subtract from domestic value added.
- Average Import Content branch (`support_functions.R` ~line 358): `CDI = -lag(m̄) * lag(DA) * gM` — the same leakage built from the aggregate import share `m̄`, domestic-absorption ratio `DA`, and aggregate import-share growth `gM`.

No behaviour change.

Verify: comments present at both sites; `source("generate_reports.R")` still runs unchanged.

### Item 3 — External-sector sign divergence (investigate, do not force a fix)

Premise correction accepted: because the two methods weight import content differently (component-level vs aggregate), their `CDX` terms can legitimately differ in sign when import growth is heterogeneous across demand components.
"Same sign always" is an empirical regularity, not a theoretical guarantee.
Observed divergences: USA 2012, MEX 2014.

Action: a throwaway diagnostic (kept in `tests.R` / scratch, not wired into the pipeline) that, for USA-2012 and MEX-2014, prints per demand component:

- Import Content inputs: `lag(imp_v)`, `lag(wei_v)`, `gms_v`, and the resulting per-component contribution to the aggregate import term.
- Average Import Content inputs: `m̄`, `gM`, and the resulting aggregate import term.

Determine whether the sign flip is driven by one component's import growth (a genuine composition effect) or by a data/code error.
Record the conclusion as a short note near the FIXME at `support_functions.R:360` and in project memory.
Patch code only if the diagnostic proves a defect.

Verify: diagnostic output produced for both country-years; written conclusion stating "composition effect" or "defect (with location)".

### Item 4 — Grouped-period mean-growth table (new function)

Configurable crisis window, four period blocks, 2020 excluded everywhere.

`global_variables.R`: add `crisis_start <- 2007` and `crisis_end <- 2009` (defaults; changeable in one place).
Derive the four blocks from these and from `years`:

- pre-crisis: `first_year` .. `crisis_start - 1` (default 1995–2006)
- crisis: `crisis_start` .. `crisis_end` (default 2007–2009)
- post-crisis: `crisis_end + 1` .. last non-2020 year (default 2010–2019)
- total: `first_year` .. last non-2020 year, crisis years included (default 1995–2019)

2020 is dropped from all blocks.

New function `tabulate_period_means(decomp, method, ...)` in `support_functions.R`:

- Rows: country. Columns grouped by block; within each block, mean GDP growth and the mean contribution of each demand component (and `CDD`, `CDX`).
- One method per table (default the project's primary method; parameterised).
- Export with `knitr::kable(booktabs = TRUE, caption = ...)` to `tabs/`, following the repo's table conventions (LaTeX; conditional formatting optional, consistent with existing tables).
- Wire the call into `group_plots()` alongside the other `tabulate_*` calls.

Verify: table written to `tabs/` for at least one method; columns show four blocks; no 2020; a hand-checked country mean matches a manual `mean()` over the block's years.

### Item 5 — Distribution-plot units (verify and label)

The density plots in `plot_differenteces` show the *difference* in contribution between a method and the target method.
Expected unit: difference in contribution-to-GDP-growth (percentage points), not shares of 100%.
The axis is currently unlabelled (`x = NULL`), which is the ambiguity to resolve.

Action: confirm the unit from the data path, then add an explicit axis label stating it (e.g. "Difference in contribution to GDP growth (p.p.)").
If the check contradicts the expectation, correct the label to the true unit and note the finding.

Verify: axis label present and matching the verified unit; a spot value on the axis reconciles with a raw contribution difference times 100.

### Item 6 — Contribution-over-time normalisation

Two plots.

1. `plot_decomp` stacked-column contribution-over-time: normalise each contribution by that year's GDP growth (`contribution / gGDP`), expressing share of growth.
   Values are unbounded (may exceed 100% or be negative); label the axis accordingly.
   Handle `gGDP == 0` / near-zero years explicitly (leave `NA`, do not plot spurious spikes).
2. Bump chart `ImpCoef_TimeSeries`: reinterpreted (import coefficients are ratios, so normalising by GDP growth is undefined).
   Instead, plot all demand components' import coefficients together over time on one shared scale, so their differing import content over time is directly comparable.

Verify: `plot_decomp` output shows share-of-growth values with a matching axis label and no spurious spikes at zero-growth years; the bump chart shows all components on one shared scale.

### Item 8 — CDX detail per country (new plot)

Per-country stacked plot over time decomposing `CDX` into:

- domestic export content: `(1 - imp_X) * lag(wei_X) * grw_X`
- per-component import-content leakage: `-lag(imp_v) * lag(wei_v) * gms_v` for each demand component `v`

Stacked area or column over time; one panel per country.
The stack sums to `CDX` (Import Content method).

Verify: for a sample country-year the stacked components sum to the `CDX` value from `decompose_growth`.

### Item 1 — Shared y-axes across facets

Policy: fix the y-scale where facets differ only by country/method (comparable quantities); this is the general default.
Where a facet dimension mixes quantities of different magnitude (e.g. faceting by `Variable`), a shared scale can compress small series.

Action: sweep the faceted plots and set shared y where it aids comparison.
For any plot where a shared scale visibly harms readability, keep `free_y` and record that plot in the implementation notes for review, rather than degrading the figure.

Affected plots (to sweep): `plot_external_contrib` (External_Contrib), `plot_decomp` (Growth_Decomp), `report_import_coeff` (ImpCoef_BoxPlot, ImpCoef_TimeSeries), `plot_differenteces` (differences + densities), and the new Item 8 plot.

Verify: each swept plot renders with the intended scale; any exceptions are listed with a one-line reason.

### Item 2 — Merge CDI / imports (DEFERRED)

`CDI` and the imports term carry overlapping information; consolidating them is worth planning but is explicitly postponed.
Recorded here as future work only.
Not implemented in this batch.
When taken up, it should be scoped as its own spec (it touches the decomposition identities, not just presentation).

## Out of scope

- Any Stage-A change or data rebuild.
- The design refactor of the three main functions tracked in `TODOs.org`.
- Item 2 implementation.

## Execution order

7 (annotate) → 3 (diagnostic) → 4 (table) → 5 (units) → 6 (normalisation) → 8 (CDX detail) → 1 (axis sweep, last, since it touches plots the earlier items add/modify).
