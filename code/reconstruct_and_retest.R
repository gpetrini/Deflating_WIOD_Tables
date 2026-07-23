## Reconstruction + re-test wave (2026-07-23).
## Rebuilds the gap panel on the CURRENT benchmark (1996-2020) and re-runs the
## hypotheses that were previously estimated only on the legacy 1575-obs panel
## (1996-2016), reporting the two side by side. Run from inside code/.

source("diagnose_xr_gap.R")

sep <- function(s) cat("\n========== ", s, " ==========\n")

## --- 1. Rebuild both panels ------------------------------------------------
sep("PANEL DIMENSIONS")
pc <- build_gap_panel("current")
pr <- build_gap_panel("repo2018")
dims <- function(p) sprintf("nrow=%d  years=%d-%d  countries=%d",
                            nrow(p), min(p$Year), max(p$Year), length(unique(p$ISO)))
cat("current :", dims(pc), "\n")
cat("repo2018:", dims(pr), "\n")

## --- 2. Re-test the discriminating fit on both panels ----------------------
## Full, trimmed |Gap|<=0.5, and robust rlm; the three rows the findings report.
fit_row <- function(panel, label) {
  full <- fit_gap_model(panel)
  trim <- fit_gap_model(dplyr::filter(panel, abs(Gap) <= 0.5))
  rob  <- MASS::rlm(Gap ~ dln_e + pi + factor(ISO), data = panel, maxit = 100)
  rc   <- coef(rob)
  cat(sprintf("%-9s  full: be=%+.3f bpi=%+.3f wR2=%.3f | trim: be=%+.3f bpi=%+.3f wR2=%.3f | rlm: be=%+.3f bpi=%+.3f\n",
              label,
              full$beta_e, full$beta_pi, full$within_r2,
              trim$beta_e, trim$beta_pi, trim$within_r2,
              unname(rc["dln_e"]), unname(rc["pi"])))
}
sep("DISCRIMINATING FIT  Gap ~ dln_e + pi + factor(ISO)")
cat("(legacy findings, current-panel target): be~0, bpi~0 under trim/robust\n")
fit_row(pr, "repo2018")
fit_row(pc, "current")

sep("CORRELATIONS cor(Gap, dln_e) and cor(Gap, pi)")
cc <- function(p, lab) cat(sprintf("%-9s  cor(Gap,dln_e)=%+.3f  cor(Gap,pi)=%+.3f  | trimmed |Gap|<=0.5: %+.3f  %+.3f\n",
  lab, cor(p$Gap,p$dln_e), cor(p$Gap,p$pi),
  { q<-dplyr::filter(p,abs(Gap)<=0.5); cor(q$Gap,q$dln_e) },
  { q<-dplyr::filter(p,abs(Gap)<=0.5); cor(q$Gap,q$pi) }))
cc(pr,"repo2018"); cc(pc,"current")

## --- 3. Exclusion checks (base-year invariance) ----------------------------
sep("EXCLUSION CHECKS")
print(exclusion_checks())

## --- 4. Decomposition / attribution + the cancellation test ----------------
## decompose_gap reads the independent WB current-USD series (2018 vintage, to
## 2016). Test whether reconstructing dln_cd from the pipeline's own P and e
## makes deflator_mm collapse to zero (mechanical cancellation).
sep("DECOMPOSITION on independent CD file (auto-restricts to CD window)")
dc <- decompose_gap(pc)
dr <- decompose_gap(pr)
cat("current : CD-window years", min(dc$Year), "-", max(dc$Year), " nrow", nrow(dc), "\n")
cat("repo2018: CD-window years", min(dr$Year), "-", max(dr$Year), " nrow", nrow(dr), "\n")
att <- function(d, lab, nonspike = FALSE) {
  a <- attribute_gap(d)
  cat(sprintf("%-9s%s  share_nominal=%.3f  share_deflator=%.3f  identity_max_err=%.2e\n",
      lab, if(nonspike) " (non-spike)" else "", a$share_nominal, a$share_deflator,
      max(abs(d$nominal_mm + d$deflator_mm - d$Gap))))
}
att(dc,"current"); att(dr,"repo2018")

sep("CANCELLATION TEST: reconstruct dln_cd from pipeline's own P and e")
## Reconstructed official nominal-USD growth using the SAME deflator/exchange
## the pipeline applies: dln_cd_hat = gY_off + pi + dln_e.
recon <- pc |>
  dplyr::mutate(
    dln_cd_hat  = gY_off + pi + dln_e,
    nominal_hat = (gY_io + dln_e + pi) - dln_cd_hat,
    deflator_hat = dln_cd_hat - gY_off - dln_e - pi
  )
cat(sprintf("max|deflator_mm_hat| = %.3e   (0 => full mechanical cancellation)\n",
            max(abs(recon$deflator_hat))))
cat(sprintf("max|nominal_hat - Gap| = %.3e   (0 => nominal term absorbs all gap)\n",
            max(abs(recon$nominal_hat - recon$Gap))))

## --- 5. Persist the current panel ------------------------------------------
sep("PERSIST")
saveRDS(pc, "../results/diagnostics/gap_panel.rds")
saveRDS(pr, "../results/diagnostics/gap_panel_repo2018.rds")
cat("wrote gap_panel.rds (current) and gap_panel_repo2018.rds\n")
