## H1: direct level test, ICIO nominal-USD aggregate vs official nominal GDP
## (map #1, ticket #6). Both substitutable channels (exchange rate H4, deflator
## H2) were rejected for the majority, so by elimination the surviving gap should
## sit in the ICIO nominal aggregate. This tests that directly, in levels, against
## the INDEPENDENT World Bank current-USD series (NY.GDP.MKTP.CD, 2018 vintage, to
## 2016). It does NOT reconstruct the official side from the pipeline's own P and e
## (that cancellation is exact and circular, verified 2026-07-23). Run from code/.

source("diagnose_xr_gap.R")

## --- Pre-registered criteria (fixed before running) ------------------------
## H1 is confirmed as WHERE the majority gap sits if, on the majority x 1996-2016:
##  (a) the nominal-aggregate term dominates the deflator term in the majority of
##      country-years: share(|nominal_mm| > |deflator_mm|) > 0.5, and
##  (b) the gap is well approximated by the nominal term:
##      median|Gap - nominal_mm| < 0.5 pp (the operational threshold).
## This locates the gap; it does NOT by itself separate an inherited-table defect
## from the 2018-vs-2023 vintage mismatch (stated with the verdict).
cat("PRE-REGISTERED: (a) share(|nominal_mm|>|deflator_mm|)>0.5 and\n")
cat("                (b) median|Gap-nominal_mm| < 0.5 pp  => H1 locates the gap.\n\n")

## --- Decomposition on the independent CD file, majority x CD window ---------
d <- decompose_gap(majority_panel()) |>
  dplyr::filter(Year <= 2016)
cat(sprintf("Majority x 1996-2016: %d country-years, %d countries\n",
            nrow(d), length(unique(d$ISO))))
cat(sprintf("Identity check max|nominal_mm + deflator_mm - Gap| = %.2e\n\n",
            max(abs(d$nominal_mm + d$deflator_mm - d$Gap))))

## --- (a) Which term dominates, per country-year ----------------------------
d <- d |> dplyr::mutate(nom_dom = abs(nominal_mm) > abs(deflator_mm))
cat("DOMINANCE (per country-year)\n")
cat(sprintf("  nominal term dominates : %.1f%%\n", 100*mean(d$nom_dom)))
cat(sprintf("  deflator term dominates: %.1f%%\n\n", 100*mean(!d$nom_dom)))

## --- (b) Gap approximated by the nominal term ------------------------------
pp <- function(x) 100*x
cat("APPROXIMATION (pp)\n")
cat(sprintf("  median|Gap|                 = %.3f\n", median(abs(pp(d$Gap)))))
cat(sprintf("  median|nominal_mm|          = %.3f\n", median(abs(pp(d$nominal_mm)))))
cat(sprintf("  median|deflator_mm|         = %.3f\n", median(abs(pp(d$deflator_mm)))))
cat(sprintf("  median|Gap - nominal_mm|    = %.3f  (= median|deflator_mm|)\n", median(abs(pp(d$Gap - d$nominal_mm)))))
cat(sprintf("  covariance share nominal    = %.3f\n\n", attribute_gap(d)$share_nominal))

## --- Levels: normalized nominal-USD index per country ----------------------
## ICIO nominal recovers the raw input: dln = gY_io + dln_e + pi. Cumulate to a
## level index, base = first year in-window. Official = WB CD level, same base.
cd <- read_csv("../inputs/world_bank/GDP_current/API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv",
               skip = 4, show_col_types = FALSE) |>
  rename(ISO = `Country Code`) |>
  select(ISO, matches("^[0-9]{4}$")) |>
  pivot_longer(-ISO, names_to = "Year", values_to = "cd") |>
  mutate(Year = as.integer(Year)) |>
  filter(!is.na(cd))

lvl <- d |>
  arrange(ISO, Year) |>
  group_by(ISO) |>
  mutate(io_nom_idx = cumprod(exp(gY_io + dln_e + pi))) |>   # index, base ~ first year
  ungroup() |>
  inner_join(cd, by = c("ISO", "Year")) |>
  group_by(ISO) |>
  mutate(io_nom_idx = io_nom_idx / first(io_nom_idx),
         cd_idx     = cd / first(cd),
         lvl_ratio  = io_nom_idx / cd_idx) |>          # 1 = ICIO nominal tracks official
  ungroup()

cat("LEVELS: terminal ICIO-nominal / official-nominal index ratio per country\n")
cat("(1 = the two nominal-USD levels track; away from 1 = nominal-aggregate divergence)\n")
term <- lvl |>
  group_by(ISO) |>
  summarise(years = dplyr::n(),
            end_ratio = dplyr::last(lvl_ratio), .groups = "drop") |>
  mutate(abs_dev = abs(log(end_ratio))) |>
  arrange(desc(abs_dev))
cat(sprintf("  median terminal ratio = %.3f ; median |ln ratio| = %.3f\n",
            median(term$end_ratio), median(term$abs_dev)))
cat(sprintf("  countries within +/-5%% of 1: %.1f%%\n\n",
            100*mean(term$abs_dev < log(1.05))))
cat("Largest level divergences (top 12):\n")
print(as.data.frame(head(term, 12)), digits = 3)
