## Tier-B verification for wayfinder ticket #20 (plan Tasks 1, 4, 7).
## Scope: the reporting refinements only. This script has nothing to do with the
## growth-rate gap diagnosis, which was resolved upstream.
##
## It regenerates the figures the three tasks touch, for one group, and records a
## numeric fingerprint of `decomp`, so that a comment, an axis label and a facet
## scale can be shown not to have moved any number.
##
## Run from inside code/:  Rscript verify_reporting_refinements.R

source("global_variables.R")
source("support_functions.R")

results <- prepare_data()
decomp  <- decompose_growth()

tidy <- get_tidy(decomp)

fin <- is.finite(tidy$Contribution)

fingerprint <- data.frame(
  n        = nrow(tidy),
  n_finite = sum(fin),
  sum      = sum(tidy$Contribution[fin]),
  ss       = sum(tidy$Contribution[fin]^2),
  min      = min(tidy$Contribution[fin]),
  max      = max(tidy$Contribution[fin])
)

cat("== decomp fingerprint ==\n")
print(fingerprint, digits = 16)
saveRDS(fingerprint, "../objs/reporting_refinements_fingerprint.rds")

## Unit check for Task 4: contributions are fractions of GDP growth, not
## percentages and not shares of a hundred percent total.
cat("\n== unit check: quantiles of |Contribution| ==\n")
print(stats::quantile(abs(tidy$Contribution[fin]), c(.5, .9, .99, 1)))

group_tag <- "Developed"
countries <- country_groups[[group_tag]]
methods   <- names(decomp[[1]])

df <- tidy |>
  dplyr::filter(ISO %in% countries)

pdf(file.path("../reports", paste0(group_tag, "_verify_reporting.pdf")),
    width = plotW, height = plotH)

plot_decomp(df, group = group_tag, countries = countries, methods,
            grouped = TRUE, fig_extension = "pdf")

## plot_differenteces is exercised on the single-country path. Its grouped path
## is broken independently of this ticket: the density block reuses `vrbl` leaked
## from the loop above it, whose last value is "M", which the block then filters
## out, leaving no data to facet. group_plots only reaches that path when
## verbose = TRUE, which is not the default.
solo <- tidy |>
  dplyr::filter(ISO == "USA")

plot_differenteces(solo, group = "USA", countries = "USA", methods,
                   grouped = FALSE, target_meth = "Import Content",
                   fig_extension = "pdf")

plot_external_contrib(df = df, group = group_tag, countries = countries,
                      grouped = TRUE, fig_extension = "pdf")

report_import_coeff(group = group_tag, IO = results, countries = countries,
                    grouped = TRUE, fig_extension = "pdf")

invisible(dev.off())

cat("\nTier-B run finished\n")
