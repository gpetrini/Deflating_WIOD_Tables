source("./global_variables.R")
source("./support_functions.R")

results <- prepare_data()


decomp <- decompose_growth()

  df <- get_tidy(data = decomp) |>
    filter(ISO == "BRA") |>
    mutate(Contribution = as.numeric(Contribution))


## calculate_metrics <- function(
##                               data,
##                               probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
##                               digits = 4L,
##                               ...
##                               ) {

##   accuracy <- 10^(-digits)

##   # 1. Prepare clean data
##   clean_data <- data |>
##     get_tidy() |>
##     filter(ISO == "BRA") |>
##     mutate(Contribution = as.numeric(Contribution)) |>
##     group_by(Variable, Method) |>
##     summarise(Contribution = list(na.omit(Contribution)), .groups = "drop")

##   # 2. Get unique methods and variables
##   methods <- unique(clean_data$Method)
##   variables <- unique(clean_data$Variable)

##   # 3. Build nested results structure
##   results <- map(variables, function(var) {
##     var_data <- filter(clean_data, Variable == var)

##     map(methods, function(ref_method) {
##       ref_series <- var_data |>
##         filter(Method == ref_method) |>
##         pull(Contribution) |>
##         pluck(1) |>
##         unlist(use.names = FALSE) |>
##         na.omit() |>
##         as.vector()


##       alt_methods <- setdiff(methods, ref_method)

##       # Calculate all metrics for each alternative method
##       metrics <- map(alt_methods, function(alt_method) {
##         alt_series <- var_data |>
##           filter(Method == alt_method) |>
##           pull(Contribution) |>
##           pluck(1) |>
##           unlist(use.names = FALSE) |>
##           na.omit() |>
##           as.vector()


##         quantile_ref <- quantile(ref_series, probs = probs, na.rm = TRUE)

##         quantile_alt <- quantile(alt_series, probs = probs, na.rm = TRUE)


##         quantile_res <- mean(abs(quantile_ref - quantile_alt))


##         # Ensure series are comparable
##         if (length(ref_series) != length(alt_series)) {
##           return(tibble(
##             ## Difference Metrics
##             !!sym("Euclidean Distance") := NA_real_,
##             MAE = NA_real_,
##             MAD = NA_real_,
##             MAPE = NA_real_,
##             ## Directional Discrepancies
##             !!sym("Cross-Correlation Distance") := NA_real_,
##             !!sym("Sign Divergence Rate") := NA_real_,
##             ## Volatity Comparison
##             !!sym("Standard Deviation Percent Rate") := NA_real_,
##             !!sym("Difference in Autocorrelation") := NA_real_,
##             ## Distribution Comparison
##             !!sym("Mean Absolute Quantile Differences") := NA_real_
##           ))
##         }

##         # Calculate metrics
##         tibble(
##           ## Difference Metrics
##           !!sym("Euclidean Distance") := TSdist::EuclideanDistance(ref_series, alt_series),
##           MAE = mean(abs(ref_series - alt_series)),
##           MAD = max(abs(ref_series - alt_series)),
##           MAPE = mean(abs(ref_series - alt_series)/abs(ref_series)),
##           ## Directional Discrepancies
##           !!sym("Cross-Correlation Distance") := TSdist::CCorDistance(ref_series, alt_series),
##           !!sym("Sign Divergence Rate") := mean(
##             sign(ref_series) != sign(alt_series),
##             na.rm = TRUE
##           ),
##           ## Volatity Comparison
##           !!sym("Standard Deviation Percent Rate") := (sd(alt_series) / sd(ref_series)) - 1,
##           !!sym("Difference in Autocorrelation") := TSdist::ACFDistance(ref_series, alt_series),
##           ## Distribution Comparison
##           !!sym("Mean Absolute Quantile Differences") := quantile_res |> round(digits = digits)
##         )
##       }) |> set_names(alt_methods)

##       # Convert to matrix
##       metric_names <- names(metrics[[1]])
##       matrix(
##         data = unlist(metrics),
##         nrow = length(metric_names),
##         byrow = FALSE,
##         dimnames = list(metric_names, alt_methods)
##       )
##     }) |> set_names(methods)
##   }) |> set_names(variables)

##   return(results)
## }
