## Ticket #9 (wayfinder map #8, Phase A pipeline-error audit).
## Verifies that prepare_data() (support_functions.R) computes g^IO correctly
## from the supplied ICIO tables, in ISOLATION from the deflation. Two parts:
##   A. Synthetic hand-computable fixture: assert GDP, m, gY match hand calc,
##      and that a uniform scalar on (Ft,Fn,Fm) leaves m invariant (deflation
##      cancels) while GDP scales linearly.
##   B. Internal consistency on the real NIOTs: GDP == Ft_total - M_total per
##      country-year; M == M_F + M_I with M_I = Am %*% Z %*% Fn in the correct
##      orientation; m deflation-invariant on real inputs; index/lag alignment.
## READ-ONLY audit: no Stage-A script executed; NIOTs is loaded as a data object
## (same as global_variables.R / diagnose_xr_gap.R do). Run from code/.

source("support_functions.R")   # defines prepare_data(); loads tidyverse, xts

## --- tiny assertion harness ------------------------------------------------
.pass <- 0L; .fail <- 0L
chk <- function(name, ok, detail = "") {
  tag <- if (isTRUE(ok)) { .pass <<- .pass + 1L; "PASS" } else { .fail <<- .fail + 1L; "FAIL" }
  cat(sprintf("[%s] %s%s\n", tag, name,
              if (nzchar(detail)) paste0("  (", detail, ")") else ""))
}
near <- function(a, b, tol = 1e-9) all(abs(as.numeric(a) - as.numeric(b)) <= tol)

FD <- c("HFCE", "GGFC", "GFCF", "INVNT", "EXPO")  # column schema of Ft/Fn/Fm

cat("\n=========== PART A: SYNTHETIC HAND-COMPUTABLE FIXTURE ===========\n\n")

## Two sectors, two years, one country. Year 2001 = 1.1 * year 2000 on the
## final-demand blocks, with Am/Z held fixed (they are scale-free ratios), so
## the uniform-scalar invariance of m and linear scaling of GDP are both tested.
mkdf <- function(m) { d <- as.data.frame(m); colnames(d) <- FD; d }

Fn0 <- mkdf(rbind(c(10, 2, 4, 1, 3),
                  c(20, 0, 6, 2, 5)))          # domestic final demand (2x5)
Fm0 <- mkdf(rbind(c( 1, 0, 1, 0, 0),
                  c( 2, 0, 1, 0, 0)))          # imported final demand (2x5)
Ft0 <- mkdf(as.matrix(Fn0) + as.matrix(Fm0))   # Ft = Fn + Fm (Ut.R identity)
Am0 <- matrix(c(0.1, 0.0,
                0.0, 0.2), 2, 2, byrow = TRUE) # imported tech coefficients
Z0  <- diag(2)                                 # Leontief inverse (identity)
s   <- 1.1

data_base <- list(TST = list(
  Ft = list("2000" = Ft0,        "2001" = mkdf(s * as.matrix(Ft0))),
  Fn = list("2000" = Fn0,        "2001" = mkdf(s * as.matrix(Fn0))),
  Fm = list("2000" = Fm0,        "2001" = mkdf(s * as.matrix(Fm0))),
  lista_Am = list("2000" = Am0,  "2001" = Am0),
  lista_Z  = list("2000" = Z0,   "2001" = Z0)
))

## Install the globals prepare_data() reads from its enclosing scope, then run.
WIOD_countries <- "TST"; years <- c("2000", "2001")
res <- prepare_data(data_base)
M   <- res$TST$M; Ft <- res$TST$Ft; m <- res$TST$m
GDP <- as.numeric(res$TST$GDP[, "GDP"]); gY <- as.numeric(res$TST$g[, "GDP"])

## Hand calculation (component order C=HFCE, I=GFCF, G=GGFC, X=EXPO, E=INVNT).
FtT  <- colSums(Ft0)                              # by FD column
M_F  <- colSums(Fm0)                              # imported final demand
M_Ie <- colSums(Am0 %*% Z0 %*% as.matrix(Fn0))    # embodied imported interm.
names(M_Ie) <- FD
Mtot_by_fd <- M_F + M_Ie
GDP_hand   <- sum(FtT) - sum(Mtot_by_fd)          # == Ft_total - M_total

chk("A1 GDP(2000) matches hand calc", near(GDP[1], GDP_hand),
    sprintf("got %.6f, expect %.6f", GDP[1], GDP_hand))
chk("A2 GDP == Ft_total - M_total (synthetic)",
    near(GDP[1], sum(FtT) - sum(Mtot_by_fd)))
chk("A3 GDP(2001) == 1.1 * GDP(2000) (linear in scalar)",
    near(GDP[2], s * GDP[1]))
chk("A4 gY(2001) == 0.1 exactly", near(gY[2], 0.1))
chk("A5 gY(2000) is NA (no prior year to lag)", is.na(gY[1]))

## m column order must align with Ft column order (C,I,G,X,E,Total) -- a
## positional xts division; distinct component values catch a misalignment.
m0  <- as.numeric(m[1, c("C", "I", "G", "X", "E")])
m_hand <- c(C = Mtot_by_fd["HFCE"] / FtT["HFCE"],
            I = Mtot_by_fd["GFCF"] / FtT["GFCF"],
            G = Mtot_by_fd["GGFC"] / FtT["GGFC"],
            X = Mtot_by_fd["EXPO"] / FtT["EXPO"],
            E = Mtot_by_fd["INVNT"] / FtT["INVNT"])
chk("A6 import shares m match hand calc (component alignment)",
    near(m0, m_hand), paste0("m=", paste(round(m0, 4), collapse = ",")))
chk("A7 m invariant to uniform scalar (deflation cancels)",
    near(as.numeric(m[1, c("C","I","G","X","E")]),
         as.numeric(m[2, c("C","I","G","X","E")])))

## Orientation of M_I: Am %*% Z %*% Fn (not a transpose / wrong product).
MI_res <- as.numeric(res$TST$M_I["2000", c("M_C","M_I","M_G","M_X","M_E")])
MI_hand <- c(M_Ie["HFCE"], M_Ie["GFCF"], M_Ie["GGFC"], M_Ie["EXPO"], M_Ie["INVNT"])
chk("A8 M_I == Am %*% Z %*% Fn, correct orientation", near(MI_res, MI_hand))
chk("A9 M == M_F + M_I (synthetic)",
    near(as.numeric(M["2000", "Total"]), sum(M_F) + sum(M_Ie)))

cat("\n=========== PART B: INTERNAL CONSISTENCY ON REAL NIOTs ===========\n\n")

load("../etc/Modelo/NIOTs_resultados.rdata")   # data object, not a script
WIOD_countries <- names(NIOTs)
years <- names(NIOTs[[1]][[1]])
cat(sprintf("Loaded %d countries, %d years (%s..%s)\n\n",
            length(WIOD_countries), length(years), years[1], tail(years, 1)))

R <- prepare_data(NIOTs)

## B0: country codes and index not shifted.
chk("B0a names(results) == names(NIOTs) (no country shift)",
    identical(names(R), names(NIOTs)))
idx_ok <- all(vapply(R, function(x)
  identical(format(zoo::index(x$GDP), "%Y"), years), logical(1)))
chk("B0b GDP index years == NIOTs years for every country", idx_ok)

## B1: GDP == Ft_total - M_total per country-year (the core aggregation identity).
err_id <- vapply(names(R), function(iso) {
  g   <- as.numeric(R[[iso]]$GDP[, "GDP"])
  ftm <- as.numeric(R[[iso]]$Ft[, "Total"]) - as.numeric(R[[iso]]$M[, "Total"])
  max(abs(g - ftm) / pmax(abs(g), 1e-8))
}, numeric(1))
chk("B1 GDP == Ft_total - M_total (all country-years, rel.)",
    max(err_id) < 1e-10, sprintf("max rel err = %.2e (%s)",
                                 max(err_id), names(which.max(err_id))))

## B2: M == M_F + M_I on real data.
err_m <- vapply(names(R), function(iso) {
  d <- as.numeric(R[[iso]]$M[, "Total"]) -
       (as.numeric(R[[iso]]$M_F[, "Total"]) + as.numeric(R[[iso]]$M_I[, "Total"]))
  max(abs(d))
}, numeric(1))
chk("B2 M == M_F + M_I (all country-years)", max(err_m) < 1e-8,
    sprintf("max abs err = %.2e", max(err_m)))

## B3: M_I = Am %*% Z %*% Fn orientation, recomputed independently on real data.
##     Spot 5 countries x their first year; rebuild from stored Am, Z, Fn.
spot <- head(names(NIOTs), 5)
err_mi <- vapply(spot, function(iso) {
  yr <- years[1]
  Am <- NIOTs[[iso]]$lista_Am[[yr]]; Z <- NIOTs[[iso]]$lista_Z[[yr]]
  Fn <- NIOTs[[iso]]$Fn[[yr]][, FD] |> as.matrix()   # sectors x 5 (HFCE..EXPO)
  mi_indep <- sum(Am %*% Z %*% Fn)
  mi_res   <- as.numeric(R[[iso]]$M_I[yr, "Total"])
  abs(mi_indep - mi_res)
}, numeric(1))
chk("B3 M_I == Am %*% Z %*% Fn recomputed (5 spot countries)",
    max(err_mi) < 1e-6, sprintf("max abs err = %.2e", max(err_mi)))

## B4: m deflation-invariant on REAL inputs. Scale (Ft,Fn,Fm) of one country by
##     a uniform scalar, hold Am/Z, re-run; m must be unchanged, GDP scaled.
iso <- names(NIOTs)[1]; sc <- 3.7
nb <- NIOTs[iso]
for (yr in years) {
  nb[[iso]]$Ft[[yr]][, FD] <- sc * nb[[iso]]$Ft[[yr]][, FD]
  nb[[iso]]$Fn[[yr]][, FD] <- sc * nb[[iso]]$Fn[[yr]][, FD]
  nb[[iso]]$Fm[[yr]][, FD] <- sc * nb[[iso]]$Fm[[yr]][, FD]
}
WIOD_countries <- iso
Rs <- prepare_data(nb)
WIOD_countries <- names(NIOTs)
m_diff <- max(abs(as.numeric(Rs[[iso]]$m[, c("C","I","G","X","E")]) -
                  as.numeric(R[[iso]]$m[,  c("C","I","G","X","E")])), na.rm = TRUE)
g_ratio <- as.numeric(Rs[[iso]]$GDP[, "GDP"]) / as.numeric(R[[iso]]$GDP[, "GDP"])
chk(sprintf("B4a m invariant to x%.1f scalar on real inputs (%s)", sc, iso),
    m_diff < 1e-10, sprintf("max |dm| = %.2e", m_diff))
chk("B4b GDP scales by the same scalar", near(g_ratio, sc, tol = 1e-8),
    sprintf("ratio range [%.6f, %.6f]", min(g_ratio), max(g_ratio)))

## B5: g^IO alignment -- gY == diff(GDP)/lag(GDP), first year NA, rest defined.
al_ok <- vapply(names(R), function(iso) {
  g   <- as.numeric(R[[iso]]$GDP[, "GDP"])
  gy  <- as.numeric(R[[iso]]$g[, "GDP"])
  ref <- c(NA, diff(g) / head(g, -1))              # (G_t - G_{t-1}) / G_{t-1}
  is.na(gy[1]) && all(is.finite(gy[-1])) &&
    max(abs(gy[-1] - ref[-1]), na.rm = TRUE) < 1e-10
}, logical(1))
chk("B5 gY == diff(GDP)/lag(GDP), first year NA (no shift)", all(al_ok),
    sprintf("%d/%d countries", sum(al_ok), length(al_ok)))

cat(sprintf("\n=========== SUMMARY: %d PASS, %d FAIL ===========\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
