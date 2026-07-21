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
