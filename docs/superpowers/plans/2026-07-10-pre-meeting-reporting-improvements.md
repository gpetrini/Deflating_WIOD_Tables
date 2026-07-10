# Pre-meeting reporting improvements Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver seven independent refinements to the growth-decomposition reports (Stage B) — CDI annotation, a sign diagnostic, a grouped-period table, distribution-unit labelling, contribution normalisation, a per-country CDX breakdown, and a shared-y-axis sweep.

**Architecture:** All work is in `code/support_functions.R` and `code/global_variables.R`. No Stage-A rebuild. The reporting stage reads the precomputed `NIOTs` list via `global_variables.R`, derives `results` (`prepare_data`) and `decomp` (`decompose_growth`), tidies them (`get_tidy`, `get_imp`, `get_dGDP`), and renders figures/tables through `group_plots`.

**Tech Stack:** R 4.3.3, tidyverse (dplyr/tidyr/purrr/ggplot2), xts/zoo, timetk, lubridate, knitr/kableExtra, ggbump.

## Global Constraints

- Native pipe `|>` for new code; match the file's existing idioms where editing in place.
- `ggplot2` over base; figures via `save_figs` (PDF + PNG) to `../figs/`.
- Tables via `knitr::kable(booktabs = TRUE, caption = ...)` to `../tabs/`.
- Never rename an existing figure/table file.
- Scripts run from **inside `code/`**; every path is relative to it (`../figs`, `../tabs`, `../etc`).
- Crisis window is configurable: `crisis_start <- 2007`, `crisis_end <- 2009`. 2020 excluded from all period aggregates.
- Portuguese placeholder names may appear; keep new names consistent with the surrounding file.

## PREREQUISITE — data object

Full runtime verification requires `etc/Modelo/NIOTs_resultados.rdata`, which is **absent** in the current checkout. Until it is placed:

- **Tier-A verification (works now):** syntax/parse check —
  `Rscript -e 'invisible(parse(file="code/support_functions.R")); invisible(parse(file="code/global_variables.R")); cat("parse OK\n")'`
  Expected: `parse OK`.
- **Tier-B verification (needs data):** run the report driver from inside `code/` and inspect outputs. Each task lists its Tier-B check; run them once the data object exists. Do **not** claim a task "works" on Tier-A alone — parse-clean is necessary, not sufficient.

---

### Task 1: Annotate the CDI definition (Item 7)

**Files:**
- Modify: `code/support_functions.R` (~line 268, Import Content branch; ~line 358, Average Import Content branch)

**Interfaces:**
- Consumes: nothing.
- Produces: no new symbols; documentation only.

- [ ] **Step 1: Add the comment above the Import Content CDI block**

Locate:

```r
    CDI <- map(vars, ~ ( - lag(imp[, .x])) * lag(wei[, .x]) * gms[, .x]) |>
      reduce(`+`)
    colnames(CDI) <- c("CDI")
```

Insert immediately above the `CDI <- map(...)` line:

```r
    ## CDI (Import Content method): negative aggregate import-content leakage.
    ## CDI = sum_v [ -lag(imp_v) * lag(wei_v) * gms_v ] over all demand components v,
    ## where imp_v = component import share, wei_v = component weight in GDP,
    ## gms_v = component import growth. Negative because imported content does
    ## not accrue to domestic value added.
```

- [ ] **Step 2: Add the comment above the Average Import Content CDI line**

Locate:

```r
    df[,"CDI"] <- (lag(mAvg) * lag(DA) * gM[["Total"]]) * (-1)

    ## FIXME: CDI does not match
```

Insert immediately above the `df[,"CDI"] <- ...` line:

```r
    ## CDI (Average Import Content method): same leakage built from aggregates.
    ## CDI = -lag(m_bar) * lag(DA) * gM, where m_bar = aggregate import share (Total),
    ## DA = domestic-absorption ratio (Ft_Total / GDP), gM = aggregate import-share growth.
```

- [ ] **Step 3: Tier-A verify (parse)**

Run: `Rscript -e 'invisible(parse(file="code/support_functions.R")); cat("parse OK\n")'`
Expected: `parse OK`

- [ ] **Step 4: Commit**

```bash
git add code/support_functions.R
git commit -m "[DOC] Annotate CDI definition in both import-content branches"
```

---

### Task 2: External-sector sign diagnostic (Item 3)

Investigate, do not force a fix. The two methods weight import content differently (component-level vs aggregate), so their CDX terms can legitimately differ in sign under heterogeneous per-component import growth. Diagnose USA-2012 and MEX-2014.

**Files:**
- Modify: `code/tests.R` (append the diagnostic function + calls)
- Modify: `code/support_functions.R` (one-line note near the FIXME at ~line 360)

**Interfaces:**
- Consumes: `results` (output of `prepare_data`) — per country a list with `m` (import shares), `Weights`, `gm` (component import growth).
- Produces: `diagnose_cdx_sign(results, country, year)` printing a per-component table; return value is the table (data.frame), invisibly.

- [ ] **Step 1: Append the diagnostic to `code/tests.R`**

```r
## Diagnostic (Item 3): why do Import Content vs Average Import Content CDX terms
## diverge in sign? Prints, per demand component, the inputs to each method's
## aggregate import term for a given country-year. Scratch only; not in pipeline.
diagnose_cdx_sign <- function(data_base = results, country, year) {
  data <- data_base[[country]]
  imp  <- data[["m"]]
  dates <- zoo::index(imp)
  vars <- setdiff(colnames(imp), "Total")

  imp <- tibble::as_tibble(imp)
  wei <- tibble::as_tibble(data[["Weights"]])
  gms <- tibble::as_tibble(data[["gm"]])

  row <- which(lubridate::year(dates) == year)
  stopifnot(length(row) == 1)

  ## Import Content: per-component import term  -lag(imp_v)*lag(wei_v)*gms_v
  ic <- vapply(vars, function(v) {
    (-dplyr::lag(imp[[v]]) * dplyr::lag(wei[[v]]) * gms[[v]])[row]
  }, numeric(1))

  ## Average Import Content: aggregate term  -lag(m_bar)*lag(wei_v)*gM
  mbar <- imp[["Total"]]
  gM   <- (mbar - dplyr::lag(mbar)) / dplyr::lag(mbar)
  avg <- vapply(vars, function(v) {
    (-dplyr::lag(mbar) * dplyr::lag(wei[[v]]) * gM)[row]
  }, numeric(1))

  out <- data.frame(
    Component      = vars,
    IC_term        = ic,
    Avg_term       = avg,
    row.names = NULL
  )
  cat(sprintf("\n== %s %d ==\n", country, year))
  print(out, digits = 4)
  cat(sprintf("IC aggregate import term  = %+.5f\n", sum(ic, na.rm = TRUE)))
  cat(sprintf("Avg aggregate import term = %+.5f\n", sum(avg, na.rm = TRUE)))
  invisible(out)
}

## Run once results exists:
## diagnose_cdx_sign(results, "USA", 2012)
## diagnose_cdx_sign(results, "MEX", 2014)
```

- [ ] **Step 2: Tier-A verify (parse)**

Run: `Rscript -e 'invisible(parse(file="code/tests.R")); cat("parse OK\n")'`
Expected: `parse OK`

- [ ] **Step 3: Tier-B verify (needs data) — run the diagnostic**

From inside `code/`, after `source("global_variables.R"); source("support_functions.R"); results <- prepare_data(NIOTs)`:

Run: `diagnose_cdx_sign(results, "USA", 2012); diagnose_cdx_sign(results, "MEX", 2014)`
Expected: two per-component tables print. Read whether the sign of the summed IC term differs from the Avg term because one component dominates the IC sum — a composition effect — versus an unexpected value (e.g. a share outside [0,1], an `NA`/`Inf`) signalling a data/code defect.

- [ ] **Step 4: Record the conclusion**

Near the FIXME at `code/support_functions.R:360` (`## FIXME: CDI does not match`), append a one-line note stating the finding, e.g.:

```r
    ## NOTE (Item 3): USA-2012 / MEX-2014 CDX sign flip is a composition effect of
    ## heterogeneous per-component import growth, not a defect. See diagnose_cdx_sign().
```

Adjust the wording to match what the diagnostic actually showed. If it showed a defect, state the defect and its location instead, and open a follow-up rather than editing the note to claim success.

- [ ] **Step 5: Commit**

```bash
git add code/tests.R code/support_functions.R
git commit -m "[INTERNAL] Add CDX-sign diagnostic and record USA/MEX finding"
```

---

### Task 3: Grouped-period mean-growth table (Item 4)

**Files:**
- Modify: `code/global_variables.R` (add crisis-window variables)
- Modify: `code/support_functions.R` (add `tabulate_period_means`; wire into `group_plots`)

**Interfaces:**
- Consumes: `decomp` (via `get_tidy` → long df with `Time`, `Method`, `ISO`, `Variable`, `Contribution`); globals `crisis_start`, `crisis_end`.
- Produces: `tabulate_period_means(decomp, method, crisis_start, crisis_end, vars_keep, tabs)` writing `../tabs/Period_Means_<method>.tex`; returns the wide means df invisibly.

- [ ] **Step 1: Add crisis-window globals**

In `code/global_variables.R`, after the `last_year <- tail(years, 1)` line, add:

```r
## Crisis window for grouped-period aggregates (Item 4). Configurable.
crisis_start <- 2007
crisis_end   <- 2009
```

- [ ] **Step 2: Add `tabulate_period_means` to `code/support_functions.R`**

Insert after `tabulate_statistics` (near line 1470):

```r
tabulate_period_means <- function(
    decomp,
    method = "Import Content",
    crisis_start = get0("crisis_start", ifnotfound = 2007),
    crisis_end   = get0("crisis_end",   ifnotfound = 2009),
    vars_keep = c("GDP", "CDD", "CDX", "C", "I", "G", "E", "X"),
    tabs = "../tabs/"
) {
  tidy <- get_tidy(decomp) |>
    dplyr::filter(Method == method) |>
    dplyr::mutate(Year = lubridate::year(Time)) |>
    dplyr::filter(Year != 2020) |>
    dplyr::filter(Variable %in% vars_keep)

  partition <- tidy |>
    dplyr::mutate(Block = dplyr::case_when(
      Year <  crisis_start ~ "Pre-crisis",
      Year <= crisis_end   ~ "Crisis",
      TRUE                 ~ "Post-crisis"
    ))
  total <- tidy |> dplyr::mutate(Block = "Total")

  means <- dplyr::bind_rows(partition, total) |>
    dplyr::group_by(ISO, Block, Variable) |>
    dplyr::summarise(Mean = mean(Contribution, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Block = factor(
      Block, levels = c("Pre-crisis", "Crisis", "Post-crisis", "Total"))) |>
    tidyr::pivot_wider(names_from = Variable, values_from = Mean) |>
    dplyr::arrange(ISO, Block)

  tab_title <- paste0(
    "Mean growth-rate contributions by period (", method, ", 2020 excluded)")

  tex <- means |>
    knitr::kable(
      format = "latex", booktabs = TRUE, digits = 4, escape = FALSE,
      longtable = TRUE, caption = tab_title, label = "TAB-PERIOD-MEANS")

  fname <- file.path(
    tabs, paste0("Period_Means_", stringr::str_remove_all(method, " "), ".tex"))
  writeLines(as.character(tex), fname)

  invisible(means)
}
```

- [ ] **Step 3: Wire it into `group_plots`**

Read `group_plots` (starts ~line 1569) and locate where the other `tabulate_*` functions are called. Add, alongside them:

```r
  tabulate_period_means(decomp, method = "Import Content")
```

- [ ] **Step 4: Tier-A verify (parse)**

Run: `Rscript -e 'invisible(parse(file="code/support_functions.R")); invisible(parse(file="code/global_variables.R")); cat("parse OK\n")'`
Expected: `parse OK`

- [ ] **Step 5: Tier-B verify (needs data) — hand-check a mean**

After building `decomp`, run:

```r
m <- tabulate_period_means(decomp, method = "Import Content")
subset(m, ISO == "USA")            # inspect four blocks, no 2020
## manual cross-check of one cell:
tidy <- get_tidy(decomp)
usa_gdp_post <- subset(tidy, ISO=="USA" & Method=="Import Content" &
                       Variable=="GDP" & lubridate::year(Time) %in% 2010:2019)
mean(usa_gdp_post$Contribution, na.rm = TRUE)   # must equal USA Post-crisis GDP cell
```

Expected: file `../tabs/Period_Means_ImportContent.tex` written; the manual mean matches the table's USA Post-crisis GDP cell; no 2020 row contributes.

- [ ] **Step 6: Commit**

```bash
git add code/global_variables.R code/support_functions.R
git commit -m "[NEW] Grouped-period mean-growth table with configurable crisis window"
```

---

### Task 4: Label distribution-plot units (Item 5)

Verify the density plots in `plot_differenteces` show differences in contribution-to-GDP-growth (a fraction; ×100 = percentage points), not shares of 100%; then label the axis.

**Files:**
- Modify: `code/support_functions.R` (~line 961-970, density `ggplot` block)

**Interfaces:**
- Consumes / Produces: none new; presentation only.

- [ ] **Step 1: Confirm the unit**

Trace: `plot_differenteces` builds `diff_df$Difference` as `method_contribution - target_contribution`, where `Contribution` comes from `decompose_growth` as `weight * growth_rate` — a fraction of GDP. Therefore `Difference` is a fraction; ×100 reads as percentage points of GDP growth. It is **not** a share of a 100% total. Record this one-line conclusion in the commit message.

- [ ] **Step 2: Add the axis label and percent formatting**

Locate the density block:

```r
    ggplot(aes(x=Difference, fill = Method, color = Method)) +
    geom_density(aes(y = after_stat(scaled)), adjust=1.5, alpha=.3, na.rm = TRUE, trim = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste0("Scaled divergence distribution between different methods and ", target_meth, " for ", tag),
      subtitle = "Across Variables",
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()
```

Change `x = NULL` to a label and add an x-scale in percentage points. Replace the `labs(...) + custom_theme()` tail with:

```r
    labs(
      title = paste0("Scaled divergence distribution between different methods and ", target_meth, " for ", tag),
      subtitle = "Across Variables",
      x = "Difference in contribution to GDP growth (p.p.)", y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    scale_x_continuous(labels = scales::percent_format(scale = 100)) +
    custom_theme()
```

- [ ] **Step 3: Tier-A verify (parse)**

Run: `Rscript -e 'invisible(parse(file="code/support_functions.R")); cat("parse OK\n")'`
Expected: `parse OK`

- [ ] **Step 4: Tier-B verify (needs data)**

Regenerate a `DistDiff_*` figure via `group_plots` for one country and confirm the x-axis is labelled "Difference in contribution to GDP growth (p.p.)" with percent tick labels.

- [ ] **Step 5: Commit**

```bash
git add code/support_functions.R
git commit -m "[DOC] Label distribution-plot x-axis as p.p. contribution difference"
```

---

### Task 5: Contribution normalisation and bump-chart shared scale (Item 6)

**Files:**
- Modify: `code/support_functions.R` — `plot_decomp` first plot (~line 1012-1034); `report_import_coeff` bump chart (~line 716-733)

**Interfaces:**
- Consumes: tidy `df` (`Time`, `ISO`, `Method`, `Variable`, `Contribution`); `imp` tidy (`Time`, `ISO`, `Variable`, `Coefficient`).
- Produces: none new.

- [ ] **Step 1: Normalise `plot_decomp` contributions by GDP growth**

Locate the first plot in `plot_decomp`:

```r
  p <- df |>
    filter(Variable != "GDP") |>
    filter(Variable != "CDD") |>
    filter(Variable != "CDX") |>
    group_by(Time, Variable) |>
    ggplot(aes(x = Time, y = Contribution, fill = Variable)) +
    geom_col(
      ## width = 0.6,
      color = "black",
      position = "stack"
    ) +
    geom_point(
      data = df |> filter(Variable == "GDP"),
      aes(x = Time, y = Contribution),
      color = "black"
    ) +
    labs(
```

Replace it with (join GDP growth, divide, guard zero growth, drop the GDP point which is 1 by construction):

```r
  gdp_df <- df |>
    filter(Variable == "GDP") |>
    select(Time, ISO, Method, gGDP = Contribution)

  p <- df |>
    filter(Variable != "GDP") |>
    filter(Variable != "CDD") |>
    filter(Variable != "CDX") |>
    left_join(gdp_df, by = c("Time", "ISO", "Method")) |>
    mutate(Share = ifelse(abs(gGDP) < 1e-8, NA_real_, Contribution / gGDP)) |>
    group_by(Time, Variable) |>
    ggplot(aes(x = Time, y = Share, fill = Variable)) +
    geom_col(
      color = "black",
      position = "stack"
    ) +
    labs(
```

Then update the `y = NULL` inside this plot's `labs(...)` to `y = "Share of GDP growth"`. Locate:

```r
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()
```

and change only the first of these (the one belonging to this plot) to:

```r
      x = NULL, y = "Share of GDP growth", fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()
```

- [ ] **Step 2: Bump chart — all components on one shared scale**

Locate in `report_import_coeff`:

```r
  p <- df |>
    filter(Variable != "E") |>
    ggplot(aes(x = Time, y = Coefficient, color = Variable)) +
    geom_bump(size = 2) +
    facet_wrap(~ Variable, scales = "free_y") +
    geom_point(size = 6) +
```

Remove the per-Variable free-scale facet so all components share one panel/scale:

```r
  p <- df |>
    filter(Variable != "E") |>
    ggplot(aes(x = Time, y = Coefficient, color = Variable)) +
    geom_bump(size = 2) +
    geom_point(size = 6) +
```

And a few lines below, change the grouped facet to a fixed scale:

```r
  if (grouped) {
    p <- p +
      facet_wrap(~ ISO, scales = "free_y")
  }
```

becomes

```r
  if (grouped) {
    p <- p +
      facet_wrap(~ ISO)
  }
```

- [ ] **Step 3: Tier-A verify (parse)**

Run: `Rscript -e 'invisible(parse(file="code/support_functions.R")); cat("parse OK\n")'`
Expected: `parse OK`

- [ ] **Step 4: Tier-B verify (needs data)**

Regenerate `Growth_Decomp` and `ImpCoef_TimeSeries`. Confirm: the decomposition bars express share of GDP growth (y-axis "Share of GDP growth"), zero-growth years leave gaps rather than spikes; the import-coefficient bump chart shows all components together on one shared y-scale.

- [ ] **Step 5: Commit**

```bash
git add code/support_functions.R
git commit -m "[NEW] Normalise decomposition bars by GDP growth; unify bump-chart scale"
```

---

### Task 6: Per-country CDX breakdown plot (Item 8)

Decompose CDX (Import Content method) into domestic export content + per-component import-content leakage, stacked over time, one panel per country. The stack sums to CDX by construction: `CDX = (1-imp_X)*lag(wei_X)*grw_X + sum_v[-lag(imp_v)*lag(wei_v)*gms_v]`.

**Files:**
- Modify: `code/support_functions.R` (add `get_cdx_detail` + `plot_cdx_detail`; wire into `group_plots`)

**Interfaces:**
- Consumes: `results` — per country `m`, `Weights`, `gm`, `g`.
- Produces: `get_cdx_detail(results)` → long df (`Time`, `ISO`, `Component`, `Value`); `plot_cdx_detail(results, countries, group, grouped, fig_extension)` → figure `CDX_Detail`.

- [ ] **Step 1: Add `get_cdx_detail`**

Insert after `get_dGDP` (near line 533):

```r
get_cdx_detail <- function(IO = results) {
  countries <- names(IO)
  out <- data.frame()
  for (country in countries) {
    data  <- IO[[country]]
    imp   <- data[["m"]]
    dates <- zoo::index(imp)
    vars  <- setdiff(colnames(imp), "Total")

    imp <- tibble::as_tibble(imp)
    wei <- tibble::as_tibble(data[["Weights"]])
    gms <- tibble::as_tibble(data[["gm"]])
    grw <- tibble::as_tibble(data[["g"]])

    comp <- data.frame(Time = dates)
    for (v in vars) {
      comp[[paste0("ImportContent_", v)]] <-
        (-dplyr::lag(imp[[v]])) * dplyr::lag(wei[[v]]) * gms[[v]]
    }
    comp[["DomesticExportContent"]] <-
      (1 - imp[["X"]]) * dplyr::lag(wei[["X"]]) * grw[["X"]]
    comp$ISO <- country
    out <- rbind(out, comp)
  }
  out |>
    tidyr::pivot_longer(cols = !c(Time, ISO),
                        names_to = "Component", values_to = "Value") |>
    dplyr::mutate(ISO = as.factor(ISO))
}
```

- [ ] **Step 2: Add `plot_cdx_detail`**

Immediately below `get_cdx_detail`:

```r
plot_cdx_detail <- function(IO = results, countries, group = NULL,
                            grouped, fig_extension = c("pdf", "png")) {
  tag <- group
  df <- get_cdx_detail(IO) |>
    dplyr::filter(ISO %in% countries)

  p <- df |>
    ggplot(aes(x = Time, y = Value, fill = Component)) +
    geom_col(position = "stack", color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste0("Decomposition of external-sector contribution (CDX) for ", tag),
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()

  if (grouped) {
    p <- p + facet_wrap(~ ISO)
  }

  print(p)
  save_figs(plot = p, main = "CDX_Detail",
            fig_extension = fig_extension, suffix = tag)
}
```

- [ ] **Step 3: Wire into `group_plots`**

In `group_plots`, alongside the other per-group plot calls (where `plot_decomp` / `report_import_coeff` are invoked), add:

```r
  plot_cdx_detail(results, countries = countries, group = group, grouped = grouped)
```

Match the exact argument names used by neighbouring calls in `group_plots` (e.g. how `countries`, `group`, `grouped` are named there); adjust if they differ.

- [ ] **Step 4: Tier-A verify (parse)**

Run: `Rscript -e 'invisible(parse(file="code/support_functions.R")); cat("parse OK\n")'`
Expected: `parse OK`

- [ ] **Step 5: Tier-B verify (needs data) — stack sums to CDX**

```r
d <- get_cdx_detail(results)
usa <- subset(d, ISO == "USA")
stack_sum <- aggregate(Value ~ Time, data = usa, FUN = sum, na.rm = TRUE)
cdx <- subset(get_tidy(decomp),
              ISO=="USA" & Method=="Import Content" & Variable=="CDX")
## compare stack_sum$Value against cdx$Contribution row-by-row (tolerance 1e-8)
```

Expected: for each year, the stacked components sum to the `CDX` value from `decompose_growth`; `CDX_Detail` figure written to `../figs/`.

- [ ] **Step 6: Commit**

```bash
git add code/support_functions.R
git commit -m "[NEW] Per-country CDX breakdown plot (domestic export + import content)"
```

---

### Task 7: Shared y-axis sweep (Item 1)

Fix the y-scale on faceted plots (drop `free_y`). Item 1 concerns y-axes only; leave `free_x` facets untouched.

**Files:**
- Modify: `code/support_functions.R` — lines currently reading `scales = "free_y"` at ~565, ~606, ~709, ~1039, ~1042. (Line ~720/732 handled in Task 5; the new Task 6 plot already uses fixed scales.)

**Interfaces:**
- Consumes / Produces: none new.

- [ ] **Step 1: Replace each `free_y` facet with a fixed scale**

For each occurrence below, drop the `scales = "free_y"` argument (fixed is the default):

`plot_external_contrib` (~565):
```r
      facet_grid(rows = vars(Variable), cols = vars(ISO), scales = "free_y")
```
→
```r
      facet_grid(rows = vars(Variable), cols = vars(ISO))
```

`plot_external_contrib` (~606):
```r
      facet_grid(ISO ~ Method, scales = "free_y")
```
→
```r
      facet_grid(ISO ~ Method)
```

`report_import_coeff` boxplot (~709):
```r
      facet_wrap(~ ISO, scales = "free_y")
```
→
```r
      facet_wrap(~ ISO)
```

`plot_decomp` (~1039):
```r
      facet_wrap(~Method, scales = "free_y")
```
→
```r
      facet_wrap(~Method)
```

`plot_decomp` (~1042):
```r
      facet_wrap(ISO~Method, scales = "free_y")
```
→
```r
      facet_wrap(ISO~Method)
```

- [ ] **Step 2: Tier-A verify (parse + no stray free_y in swept lines)**

Run: `Rscript -e 'invisible(parse(file="code/support_functions.R")); cat("parse OK\n")'`
Then: `grep -n 'free_y' code/support_functions.R`
Expected: `parse OK`; remaining `free_y` hits (if any) belong only to the density/differences plots that facet on the x quantity — confirm each remaining hit is a plot where the free axis is x-related, and note any you intentionally leave in the commit message.

- [ ] **Step 3: Tier-B verify (needs data)**

Regenerate `External_Contrib`, `Growth_Decomp`, `ImpCoef_BoxPlot`. Confirm facets share a common y-scale. If any figure is visibly harmed by the shared scale (small series flattened), revert that single facet to `free_y` and record it in the commit message per the spec's Item 1 policy.

- [ ] **Step 4: Commit**

```bash
git add code/support_functions.R
git commit -m "[INTERNAL] Fix shared y-axis across faceted report plots"
```

---

### Task 8: Full integration run (gated on data)

**Files:** none modified.

- [ ] **Step 1: Run the driver end-to-end (needs data)**

From inside `code/`: `source("generate_reports.R")`
Expected: completes without error; `../figs/` and `../tabs/` populated, including `Period_Means_ImportContent.tex` and `CDX_Detail.*`.

- [ ] **Step 2: Confirm the Item-2 deferral is documented, not implemented**

Grep for any accidental CDI/imports merge: `grep -n "CDI" code/support_functions.R` — confirm CDI is still computed and reported (Item 2 was deferred, not done).

- [ ] **Step 3: Record the Item-3 finding to project memory**

Write the sign-divergence conclusion (composition effect vs defect) to a memory file so it survives the session.

---

## Self-Review

**Spec coverage:** Item 7 → Task 1; Item 3 → Task 2; Item 4 → Task 3; Item 5 → Task 4; Item 6 → Task 5; Item 8 → Task 6; Item 1 → Task 7; Item 2 → deferred (no task, verified untouched in Task 8 Step 2). All spec items mapped.

**Placeholders:** none — every code step shows complete code; every command shows expected output.

**Type consistency:** `get_cdx_detail` produces (`Time`, `ISO`, `Component`, `Value`) consumed by `plot_cdx_detail`; `tabulate_period_means` consumes `get_tidy` columns (`Time`, `Method`, `ISO`, `Variable`, `Contribution`) — all matching their definitions in `support_functions.R`. Crisis-window globals `crisis_start`/`crisis_end` defined in Task 3 Step 1, consumed with `get0(...)` fallback in the same task.

**Known risk:** Tier-B code (new-function bodies) is faithful to existing idioms but unrun (data absent); the executing agent must run Tier-B once `NIOTs_resultados.rdata` is present and adjust column-name mismatches (`gm`, `Weights`, `Ft`) if the real object differs from the read code.
