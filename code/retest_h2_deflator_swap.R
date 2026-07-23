## H2: restate the deflator-swap rejection at the country level (map #1, ticket #5).
## The applied deflator for OECD-covered countries is Economic Outlook 116; the
## substitute is the World Bank NY.GDP.DEFL.ZS (=inputs/deflator_GDP_WB.xls). Only
## pi = Dln P moves in the identity g^IO = Dln(ICIO nominal USD) + Dln e - pi, so
## the gap moves by Gap_new = Gap - (pi_wb - pi_applied), no Stage-A re-run.
## The pooled mean was near the refutation bound (+9%); this asks whether the
## rejection holds country by country. Run from inside code/.

source("diagnose_xr_gap.R")

cat("PRE-REGISTERED (country level): a country whose mean|Gap| falls >20% under\n")
cat("the swap is 'improved'. If few improve, the applied-deflator rejection holds\n")
cat("for the group, not merely for the pooled mean.\n\n")

## --- WB-only deflator log-change pi_wb --------------------------------------
pi_wb <- read_wb_wide("../inputs/deflator_GDP_WB.xls") |>
  transmute(ISO, Year, P_wb = Value) |>
  arrange(ISO, Year) |>
  group_by(ISO) |>
  mutate(pi_wb = c(NA, diff(log(P_wb)))) |>
  ungroup() |>
  filter(!is.na(pi_wb)) |>
  select(ISO, Year, pi_wb)

## --- Substitute on OECD-covered majority country-years ----------------------
oecd <- build_gap_panel() |>
  majority_panel() |>
  filter(P_src == "OCDE") |>
  left_join(pi_wb, by = c("ISO", "Year")) |>
  mutate(
    substitutable = !is.na(pi_wb),
    pi_new        = ifelse(substitutable, pi_wb, pi),
    Gap_wb        = Gap - (pi_new - pi),
    moved         = abs(pi_new - pi) > 1e-8
  )

cat(sprintf("OECD-covered majority: %d country-years, %d countries; swap moves %d (%d countries)\n",
            nrow(oecd), length(unique(oecd$ISO)),
            sum(oecd$moved), length(unique(oecd$ISO[oecd$moved]))))

## --- Pooled reproduction ----------------------------------------------------
pooled <- function(d) sprintf("mean|Gap| %.4f -> %.4f (%+.1f%%) | median %.4f -> %.4f (%+.1f%%)",
  mean(abs(d$Gap)), mean(abs(d$Gap_wb)), 100*(mean(abs(d$Gap_wb))/mean(abs(d$Gap))-1),
  median(abs(d$Gap)), median(abs(d$Gap_wb)), 100*(median(abs(d$Gap_wb))/median(abs(d$Gap))-1))
cat("POOLED  ", pooled(dplyr::filter(oecd, moved)), "\n\n")

## --- Country-level movement -------------------------------------------------
cl <- oecd |>
  dplyr::filter(moved) |>
  group_by(ISO) |>
  summarise(n = dplyr::n(),
            before = mean(abs(Gap)), after = mean(abs(Gap_wb)), .groups = "drop") |>
  mutate(pct = 100 * (after / before - 1)) |>
  arrange(pct)

nc <- nrow(cl)
cat(sprintf("COUNTRY-LEVEL over %d moved countries:\n", nc))
cat(sprintf("  improve >20%% (|Gap| falls): %d (%.0f%%)\n", sum(cl$pct < -20), 100*mean(cl$pct < -20)))
cat(sprintf("  ~unchanged (|20%%|)        : %d (%.0f%%)\n", sum(abs(cl$pct) <= 20), 100*mean(abs(cl$pct) <= 20)))
cat(sprintf("  worsen >20%%              : %d (%.0f%%)\n\n", sum(cl$pct > 20), 100*mean(cl$pct > 20)))

cat("Best improvements (top 6):\n"); print(as.data.frame(head(cl, 6)), digits = 3)
cat("\nWorst deteriorations (top 6):\n"); print(as.data.frame(head(cl[order(-cl$pct), ], 6)), digits = 3)

## --- The four the ticket names: why the swap worsens them --------------------
cat("\nNamed deteriorations, applied vs WB deflator inflation (mean pi, pp/yr):\n")
oecd |>
  dplyr::filter(ISO %in% c("COL", "TUR", "IDN", "ARG"), moved) |>
  group_by(ISO) |>
  summarise(n = dplyr::n(),
            pi_oecd = 100*mean(pi), pi_wb = 100*mean(pi_wb),
            gap_bef = mean(abs(Gap)), gap_aft = mean(abs(Gap_wb)), .groups = "drop") |>
  mutate(pct = 100*(gap_aft/gap_bef - 1)) |>
  as.data.frame() |> print(digits = 3)
