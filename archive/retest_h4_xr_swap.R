## H4: exchange-rate substitution through the identity (map #1, ticket #4).
## The applied rate is OECD-primary with a World Bank fallback. Substituting the
## World Bank rate for the whole series changes only Dln e in the identity
##   g^IO = Dln(ICIO nominal USD) + Dln e - pi,
## so the gap moves by exactly (Dln e_WB - Dln e_applied). The test asks whether
## the MAJORITY gap (quarantine excluded) is left unchanged. Run from inside archive/.

source("diagnose_xr_gap.R")

## --- Pre-registered decision rule (fixed before running) -------------------
## Evaluated on the rows the swap actually moves (applied rate = OECD). Change in
## mean|Gap| < 20% => H4 rejected (the exchange-rate series is not the source of
## the majority gap); > 50% => established; in between, no claim. The median must
## corroborate the mean for a verdict. Caution: this shares no provider with the
## KD benchmark, but WB PA.NUS.FCRF and OECD EO 116 may embed different vintages.
cat("PRE-REGISTERED: <20% refutes H4, >50% establishes, median must corroborate.\n\n")

## --- WB-only exchange rate, same convention (US dollars per unit LCU) -------
dln_e_wb <- read_wb_wide("../inputs/exchange_rate_WB.xls") |>
  transmute(ISO, Year, e_wb = 1 / Value) |>
  arrange(ISO, Year) |>
  group_by(ISO) |>
  mutate(dln_e_wb = c(NA, diff(log(e_wb)))) |>
  ungroup() |>
  filter(!is.na(dln_e_wb)) |>
  select(ISO, Year, dln_e_wb)

## --- Apply the substitution through the identity on the majority -----------
mp <- majority_panel(build_gap_panel())
sw <- mp |>
  left_join(dln_e_wb, by = c("ISO", "Year")) |>
  mutate(
    substitutable = !is.na(dln_e_wb),
    dln_e_new     = ifelse(substitutable, dln_e_wb, dln_e),   # keep applied where WB absent
    Gap_wb        = Gap + (dln_e_new - dln_e),
    moved         = abs(dln_e_new - dln_e) > 1e-8
  )

## --- Coverage --------------------------------------------------------------
cat("MAJORITY coverage\n")
cat(sprintf("  rows                         : %d (%d countries)\n",
            nrow(sw), length(unique(sw$ISO))))
cat(sprintf("  no WB rate (kept applied)    : %d\n", sum(!sw$substitutable)))
cat(sprintf("  applied rate = OECD          : %d\n", sum(sw$e_src == "OCDE")))
cat(sprintf("  rows the swap actually moves : %d (%d countries)\n\n",
            sum(sw$moved), length(unique(sw$ISO[sw$moved]))))

## --- Verdict statistics ----------------------------------------------------
report <- function(d, label) {
  before <- mean(abs(d$Gap));    after  <- mean(abs(d$Gap_wb))
  mbef   <- median(abs(d$Gap));  maft   <- median(abs(d$Gap_wb))
  cat(sprintf("%-22s n=%-4d mean|Gap| %.4f -> %.4f (%+.1f%%) | median %.4f -> %.4f (%+.1f%%)\n",
              label, nrow(d), before, after, 100*(after/before - 1),
              mbef, maft, 100*(maft/mbef - 1)))
}
cat("EFFECT ON |Gap|\n")
report(sw,                     "full majority")
report(dplyr::filter(sw, moved), "moved subset (OECD)")
report(dplyr::filter(sw, e_src == "OCDE"), "OECD-sourced rows")

## --- Redenomination filter -------------------------------------------------
## A one-year log change above 0.5 in a substitute rate for these countries is a
## currency reconversion (euro adoption: ITA/ESP/PRT 1999, GRC 2001, SVN 2007,
## SVK 2009, EST 2011, LVA 2014, LTU 2015), not exchange-rate movement. The WB
## series carries the pre-euro national currency; the OECD applied rate back-casts
## the euro. These are the exogenous re-expression H5 quarantines, here in the
## substitute, so they are removed before the verdict.
redenom <- dplyr::filter(sw, moved, abs(dln_e_new - dln_e) > 0.5)
cat(sprintf("\nRedenomination break-years removed: %d (%s)\n",
            nrow(redenom), paste(unique(redenom$ISO), collapse = " ")))
clean <- dplyr::filter(sw, moved, abs(dln_e_new - dln_e) <= 0.5)
cat("EFFECT ON |Gap|, redenomination breaks removed\n")
report(clean, "moved subset, clean")

## --- Country-level movement on the moved subset ----------------------------
cat("\nLargest country-level movement (moved subset), mean|Gap| before -> after\n")
sw |>
  dplyr::filter(moved) |>
  group_by(ISO) |>
  summarise(n = dplyr::n(),
            before = mean(abs(Gap)), after = mean(abs(Gap_wb)),
            .groups = "drop") |>
  mutate(pct = 100 * (after / before - 1)) |>
  arrange(desc(abs(pct))) |>
  head(12) |>
  as.data.frame() |>
  print(digits = 3)
