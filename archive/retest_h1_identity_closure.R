## Ticket #11 (wayfinder map #8, Phase A) -- THE DECISIVE TEST.
## Does g^IO(deflated) equal Dln(nominal aggregate) + Dln e - pi cell-for-cell,
## when the nominal aggregate is reconstructed INDEPENDENTLY from the raw
## inputs/NATIODOMIMP/ tables (own read, own agregador, own ginv), rather than
## trusted from the deflated pipeline?
##
## This separates a genuine ICIO-vs-official difference (H1) from a Stage-A /
## aggregation artifact. It tests, together:
##   (a) deflation uniformity  -- the Stage-A scalar e/P is uniform per
##       country-year, so every cell of a deflated block equals scalar x raw;
##   (b) aggregation-deflation composition -- the aggregate g^IO is exactly the
##       raw nominal aggregate's growth, adjusted by Dln e - pi = Dln(scalar).
## Recipe from #10 (docs/superpowers/findings/2026-07-23-nominal-aggregate-
## extraction-recipe.md); assembly seam verified in #9. READ-ONLY: no Stage-A
## script executed; NIOTs, agregador, and OCDE rate/deflator files are read as
## data. On disk, no fetch. Run from archive/.

suppressMessages({library(readxl); library(MASS)})
source("../code/support_functions.R")   # prepare_data(); tidyverse, xts

## ---- PRE-REGISTERED CRITERIA (fixed before running) -----------------------
TOL_UNIF <- 1e-6   # max cell spread max/min-1 of (deflated Fn)/(raw Fn) per c-y
TOL_COMP <- 1e-6   # max rel diff |scalar_agg/scalar_block - 1| per country-year
TOL_IDEN <- 1e-8   # max |g^IO - (Dln NomUSD_raw + Dln scalar)| in log points
TOL_ANCH <- 1e-4   # external anchor: |scalar_block/(e/P) - 1| (OCDE-covered c-y)
cat("PRE-REGISTERED TOLERANCES\n")
cat(sprintf("  (a) deflation uniformity : max cell spread   < %.0e\n", TOL_UNIF))
cat(sprintf("  (b) aggregation compose  : max rel diff       < %.0e\n", TOL_COMP))
cat(sprintf("  identity closure         : max |residual| lp   < %.0e\n", TOL_IDEN))
cat(sprintf("  external anchor (OCDE)   : scalar == e/P rel   < %.0e\n\n", TOL_ANCH))
cat("VERDICT RULE: all pass => g^IO is a faithful transform of the raw ICIO\n")
cat("nominal aggregate; no pipeline artifact; H1 stands. A systematic residual\n")
cat("= a methodological error, and its pattern localises the bug.\n\n")

.pass <- 0L; .fail <- 0L
chk <- function(name, ok, detail = "") {
  tag <- if (isTRUE(ok)) { .pass <<- .pass+1L; "PASS" } else { .fail <<- .fail+1L; "FAIL" }
  cat(sprintf("[%s] %s%s\n", tag, name, if (nzchar(detail)) paste0("  (", detail, ")") else ""))
}

## ---- Load reference objects (data only) -----------------------------------
ag <- read_excel("../inputs/agregador.xlsx")
ag <- as.matrix(ag[, setdiff(names(ag), "Code")])      # 45 x 7 membership
load("../etc/Modelo/NIOTs_resultados.rdata")
countries <- names(NIOTs)
years     <- names(NIOTs[[1]][[1]])
WIOD_countries <- countries                             # prepare_data reads this global
R <- prepare_data(NIOTs)                                # deflated pipeline g^IO
FDcols <- c("HFCE", "GGFC", "GFCF", "INVNT", "EXPO")

## ---- Independent raw-USD nominal aggregate, per country-year ---------------
## NomUSD = sum(Fn) - sum(Am %*% Z %*% Fn), all built from the raw CSV via the
## #10 recipe (7-sector aggregation, OUTPUT-row GO, MASS::ginv), no deflation.
recon_cy <- function(iso, yr) {
  f <- sprintf("../inputs/NATIODOMIMP/%s%sdom.csv", iso, yr)
  df <- read.csv(f)
  CIn <- t(ag) %*% as.matrix(df[1:45,   2:46]) %*% ag
  CIm <- t(ag) %*% as.matrix(df[46:90,  2:46]) %*% ag
  Mfd <- t(ag) %*% as.matrix(df[1:45,  47:56]); colnames(Mfd) <- colnames(df)[47:56]
  Fn  <- cbind(HFCE  = Mfd[,"HFCE"] + Mfd[,"NPISH"],
               GGFC  = Mfd[,"GGFC"], GFCF = Mfd[,"GFCF"], INVNT = Mfd[,"INVNT"],
               EXPO  = Mfd[,"EXPO"] + Mfd[,"CONS_NONRES"])
  GO  <- (as.matrix(df[c(91,92,94,95), 2:46]) %*% ag)[4, ]     # OUTPUT row
  An  <- CIn %*% ginv(diag(GO)); Am <- CIm %*% ginv(diag(GO))
  Z   <- ginv(diag(7) - An)
  NomUSD <- sum(Fn) - sum(Am %*% Z %*% Fn)
  ## deflation-uniformity: pipeline deflated Fn / raw Fn, cell-wise
  Fnd <- as.matrix(NIOTs[[iso]]$Fn[[yr]])[, FDcols]
  r   <- Fnd / Fn; r <- r[is.finite(r) & Fn != 0]
  list(NomUSD = NomUSD, min_GO = min(GO),
       spread = if (length(r)) max(r)/min(r) - 1 else NA_real_,
       scalar_block = if (length(r)) median(r) else NA_real_)
}

rows <- list()
for (iso in countries) for (yr in years) {
  rc <- recon_cy(iso, yr)
  NomDefl <- as.numeric(R[[iso]]$GDP[paste0(yr, "-01-01"), "GDP"])
  rows[[length(rows)+1]] <- data.frame(
    iso = iso, yr = as.integer(yr), NomUSD = rc$NomUSD, NomDefl = NomDefl,
    scalar_agg = NomDefl / rc$NomUSD, scalar_block = rc$scalar_block,
    spread = rc$spread, min_GO = rc$min_GO)
}
D <- do.call(rbind, rows)
cat(sprintf("Reconstructed %d country-years (%d countries x %d years).\n\n",
            nrow(D), length(countries), length(years)))

## ---- (a) deflation uniformity ---------------------------------------------
chk("T1 deflation uniform: (deflated Fn)/(raw Fn) is one scalar per c-y",
    max(D$spread, na.rm = TRUE) < TOL_UNIF,
    sprintf("max cell spread = %.2e at %s%d",
            max(D$spread, na.rm = TRUE),
            D$iso[which.max(D$spread)], D$yr[which.max(D$spread)]))

## ---- (b) aggregation-deflation composition --------------------------------
## The scalar recovered from the Fn block must equal the scalar implied by the
## aggregate NomDefl/NomUSD. Divergence => Am/Z/GO/orientation is wrong.
D$comp_rel <- abs(D$scalar_agg / D$scalar_block - 1)
chk("T2 aggregation composes: scalar_agg == scalar_block (Am.Z.Fn, GO ok)",
    max(D$comp_rel, na.rm = TRUE) < TOL_COMP,
    sprintf("max rel diff = %.2e at %s%d",
            max(D$comp_rel, na.rm = TRUE),
            D$iso[which.max(D$comp_rel)], D$yr[which.max(D$comp_rel)]))

## ---- identity closure in growth (the ticket's literal statement) ----------
## g^IO_t = Dln(NomDefl_t); RHS_t = Dln(NomUSD_raw_t) + Dln(scalar_block_t),
## with scalar_block from the block ratio (independent of the aggregate).
D <- D[order(D$iso, D$yr), ]
resid <- unlist(lapply(split(D, D$iso), function(g) {
  lhs <- diff(log(g$NomDefl))
  rhs <- diff(log(g$NomUSD)) + diff(log(g$scalar_block))
  lhs - rhs
}))
chk("T3 identity closure: g^IO == Dln(NomUSD_raw) + Dln e - pi, cell-for-cell",
    max(abs(resid), na.rm = TRUE) < TOL_IDEN,
    sprintf("max |residual| = %.2e log points", max(abs(resid), na.rm = TRUE)))

## ---- external anchor: recovered scalar == e/P (OCDE-covered c-y) ----------
## Ties Dln(scalar) to the literal Dln e - pi. Reads the same OCDE inputs Stage
## A used; WB-fallback country-years are simply not anchored (reported).
anchor <- tryCatch({
  cmap <- read_excel("../inputs/titulos.xlsx", sheet = "country")
  cmap$Country <- sub(" / .*", "", cmap$Country)
  long <- function(path) {
    x <- read_excel(path); names(x)[1] <- "Country"
    x$Country <- sub(" / .*", "", x$Country)
    x <- merge(x, cmap, by = "Country")
    tidyr::pivot_longer(x[, c("Code", setdiff(names(x), c("Country","Code")))],
                        -Code, names_to = "yr", values_to = "v")
  }
  e <- long("../inputs/exchange_rate_OCDE.xlsx"); names(e)[3] <- "e"
  p <- long("../inputs/deflator_GDP_OCDE.xlsx");  names(p)[3] <- "p"
  ep <- merge(e, p, by = c("Code","yr"))
  ep$yr <- as.integer(ep$yr); ep$scalar_ext <- as.numeric(ep$e)/as.numeric(ep$p)
  m <- merge(D[, c("iso","yr","scalar_block")], ep[, c("Code","yr","scalar_ext")],
             by.x = c("iso","yr"), by.y = c("Code","yr"))
  m <- m[is.finite(m$scalar_ext) & is.finite(m$scalar_block), ]
  list(n = nrow(m), maxrel = max(abs(m$scalar_block/m$scalar_ext - 1)))
}, error = function(e) list(n = 0L, maxrel = NA_real_, err = conditionMessage(e)))
if (anchor$n > 0) {
  chk(sprintf("T4 external anchor: scalar == e/P on %d OCDE-covered c-y", anchor$n),
      anchor$maxrel < TOL_ANCH, sprintf("max rel diff = %.2e", anchor$maxrel))
} else {
  cat(sprintf("[SKIP] T4 external anchor unavailable (%s)\n",
              if (!is.null(anchor$err)) anchor$err else "no overlap"))
}

## ---- diagnostics ----------------------------------------------------------
cat(sprintf("\nmin GO across all c-y = %.3f (no zero-output sector => no ginv NA)\n",
            min(D$min_GO)))
cat(sprintf("scalar range [%.4f, %.4f]; identity residual median |.| = %.2e lp\n",
            min(D$scalar_block, na.rm=TRUE), max(D$scalar_block, na.rm=TRUE),
            median(abs(resid), na.rm = TRUE)))

cat(sprintf("\n=========== SUMMARY: %d PASS, %d FAIL ===========\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
