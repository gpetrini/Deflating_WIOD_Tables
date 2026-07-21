## Exchange-rate GDP growth-gap diagnosis.
## Plan: docs/superpowers/plans/2026-07-08-exchange-rate-growth-gap-diagnosis.org
## Spec: docs/superpowers/specs/2026-07-08-exchange-rate-growth-gap-diagnosis.org
## Decision: ADR-0011 (reads currency inputs directly, does not source tmp.R).
## Run from inside code/; every path is relative to it.

suppressMessages({
  library(tidyverse)
  library(xts)
})

## --- Task 1: reproduce the IO growth series g^IO ---------------------------

## Sources the pipeline once and caches the prepared results.
.io_cache <- new.env()

load_io <- function() {
  if (is.null(.io_cache$results)) {
    suppressMessages(suppressWarnings(invisible(capture.output({
      source("global_variables.R")   # loads NIOTs, defines WIOD_countries, years
      source("support_functions.R")
      .io_cache$results <- prepare_data()
    }))))
  }
  .io_cache$results
}

get_gIO <- function(results = load_io()) {
  map(names(results), function(iso) {
    lvl <- results[[iso]][["GDP"]][, "GDP"]
    yrs <- format(index(lvl), "%Y")
    g <- diff(log(as.numeric(lvl)))
    tibble(ISO = iso, Year = as.integer(yrs[-1]), gY_io = g)
  }) |>
    list_rbind()
}

## --- Task 2: official growth g^off from World Bank constant dollars ---------

get_gOff <- function(
    path = "../inputs/world_bank/GDP_constant/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv"
) {
  raw <- read_csv(path, skip = 4, show_col_types = FALSE)
  raw |>
    rename(ISO = `Country Code`) |>
    select(ISO, matches("^[0-9]{4}$")) |>
    pivot_longer(-ISO, names_to = "Year", values_to = "gdp_kd") |>
    mutate(Year = as.integer(Year)) |>
    filter(!is.na(gdp_kd)) |>
    arrange(ISO, Year) |>
    group_by(ISO) |>
    mutate(gY_off = c(NA, diff(log(gdp_kd)))) |>
    ungroup() |>
    filter(!is.na(gY_off)) |>
    select(ISO, Year, gY_off)
}

## --- Task 3: exchange-rate and inflation log-changes (ADR-0011) -------------

## Read cleaned currency inputs directly; do NOT source tmp.R (ADR-0010/0011).

## English-name to ISO map, French suffix stripped as the Stage-A code did.
.iso_map <- function() {
  readxl::read_excel("../inputs/titulos.xlsx", sheet = "country") |>
    transmute(ISO = Code, Country = sub(" / .*", "", Country))
}

## Wide OCDE file (Country x Year) -> long ISO/Year/Value, provenance "OCDE".
read_ocde_wide <- function(file) {
  readxl::read_excel(file) |>
    mutate(Country = sub(" / .*", "", Country)) |>
    left_join(.iso_map(), by = "Country") |>
    filter(!is.na(ISO)) |>
    select(ISO, matches("^[0-9]{4}$")) |>
    pivot_longer(-ISO, names_to = "Year", values_to = "Value") |>
    mutate(Year = as.integer(Year), Source = "OCDE") |>
    filter(!is.na(Value))
}

## Wide World Bank file (Country Code = ISO) -> long, provenance "WB".
read_wb_wide <- function(file) {
  readxl::read_excel(file) |>
    rename(ISO = `Country Code`) |>
    select(ISO, matches("^[0-9]{4}$")) |>
    pivot_longer(-ISO, names_to = "Year", values_to = "Value") |>
    mutate(Year = as.integer(Year), Source = "WB") |>
    filter(!is.na(Value))
}

## OCDE primary; World Bank fills ISO/Year absent from OCDE.
read_currency_series <- function(ocde_file, wb_file) {
  ocde <- read_ocde_wide(ocde_file)
  wb   <- read_wb_wide(wb_file)
  bind_rows(ocde, anti_join(wb, ocde, by = c("ISO", "Year")))
}

get_xr_infl <- function() {
  ## Exchange rate stored as local currency per US dollar (US = 1); invert to
  ## US dollars per unit of local currency so depreciation lowers e (ADR-0011).
  e <- read_currency_series("../inputs/exchange_rate_OCDE.xlsx",
                            "../inputs/exchange_rate_WB.xls") |>
    transmute(ISO, Year, e = 1 / Value, e_src = Source)
  P <- read_currency_series("../inputs/deflator_GDP_OCDE.xlsx",
                            "../inputs/deflator_GDP_WB.xls") |>
    transmute(ISO, Year, P = Value, P_src = Source)

  e |>
    inner_join(P, by = c("ISO", "Year")) |>
    arrange(ISO, Year) |>
    group_by(ISO) |>
    mutate(dln_e = c(NA, diff(log(e))),
           pi    = c(NA, diff(log(P)))) |>
    ungroup() |>
    filter(!is.na(dln_e), !is.na(pi)) |>
    select(ISO, Year, dln_e, pi, e_src, P_src)
}

## --- Task 4: assemble the Gap panel and classify the series (H0) ------------

build_gap_panel <- function() {
  get_gIO() |>
    inner_join(get_gOff(),    by = c("ISO", "Year")) |>
    inner_join(get_xr_infl(), by = c("ISO", "Year")) |>
    mutate(Gap = gY_io - gY_off)
}

## First-order classification from simple correlations of the gap.
classify_series <- function(panel) {
  ce <- cor(panel$Gap, panel$dln_e, use = "complete.obs")
  cp <- cor(panel$Gap, panel$pi,    use = "complete.obs")
  dplyr::case_when(
    ce > 0.5 & cp < 0.3 ~ "real",
    cp > 0.5 & ce < 0.3 ~ "nominal_lcu",
    ce > 0.5 & cp > 0.5 ~ "nominal_usd",
    TRUE                ~ "ambiguous"
  )
}

## --- Task 5: two-regressor discriminating fit (H0 vs H1) --------------------

fit_gap_model <- function(panel) {
  m <- lm(Gap ~ dln_e + pi + factor(ISO), data = panel)
  co <- coef(m)
  ## within R2: variance explained by dln_e and pi beyond the fixed effects
  m_fe <- lm(Gap ~ factor(ISO), data = panel)
  within_r2 <- 1 - sum(resid(m)^2) / sum(resid(m_fe)^2)
  list(
    beta_e    = unname(co["dln_e"]),
    beta_pi   = unname(co["pi"]),
    within_r2 = within_r2,
    resid     = residuals(m)
  )
}

## --- Gap decomposition: nominal-aggregate vs deflator mismatch --------------

## Exact additive split of Gap in USD space (onboarding doc, gap decomposition):
##   nominal_mm  = dln(WIOD nominal final demand, USD) - dln(WB nominal GDP, USD)
##   deflator_mm = dln(official USD deflator) - dln(pipeline deflation factor)
## Reconstruction is exact: nominal_USD_io = GDP_real_io * deflator_GDP / e, so
## dln(nominal_USD_io) = gY_io + dln_e + pi. Needs WB current-USD GDP only.
decompose_gap <- function(
    panel = build_gap_panel(),
    cd_path = "../inputs/world_bank/GDP_current/API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv"
) {
  cd <- read_csv(cd_path, skip = 4, show_col_types = FALSE) |>
    rename(ISO = `Country Code`) |>
    select(ISO, matches("^[0-9]{4}$")) |>
    pivot_longer(-ISO, names_to = "Year", values_to = "cd") |>
    mutate(Year = as.integer(Year)) |>
    filter(!is.na(cd)) |>
    arrange(ISO, Year) |>
    group_by(ISO) |>
    mutate(dln_cd = c(NA, diff(log(cd)))) |>
    ungroup() |>
    select(ISO, Year, dln_cd)

  panel |>
    inner_join(cd, by = c("ISO", "Year")) |>
    filter(!is.na(dln_cd)) |>
    mutate(
      nominal_mm  = (gY_io + dln_e + pi) - dln_cd,
      deflator_mm = dln_cd - gY_off - dln_e - pi
    )
}

## Covariance-with-Gap attribution (shares sum to 1, robust to the mechanical
## negative covariance between the two terms).
attribute_gap <- function(d) {
  list(
    share_nominal  = cov(d$Gap, d$nominal_mm)  / var(d$Gap),
    share_deflator = cov(d$Gap, d$deflator_mm) / var(d$Gap)
  )
}

## --- Task 8: exclusion checks (H6, base-year invariance) --------------------

## H6 (base year) cannot move a growth rate: fixed-base scaling is
## growth-invariant. The check confirms the invariant numerically on the
## first country's GDP level.
exclusion_checks <- function() {
  results <- load_io()
  iso <- names(results)[1]
  lvl <- as.numeric(results[[iso]][["GDP"]][, "GDP"])
  g_base1 <- diff(log(lvl))
  g_base2 <- diff(log(lvl * 100))     # rescale as a base-year change proxy
  tibble(
    hypothesis = "H6_base_year",
    invariant  = "growth invariant to constant rescaling",
    holds      = isTRUE(all.equal(g_base1, g_base2))
  )
}
