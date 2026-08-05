### Validator for the Stage-B input contract (ticket #26).
###
### Checks a candidate object against what the reporting code actually reads,
### not against what deflate_tables.R happens to produce. The contract itself is
### documented, requirement by requirement with line references, in
### docs/superpowers/specs/2026-08-05-stage-b-input-contract.org.
###
### Usage:
###   source("validate_niots_contract.R")
###   validate_niots(NIOTs)            # returns invisibly on success
###
### Reports the FIRST violation and stops looking, because a candidate that
### fails a structural check makes every later check meaningless.
### No side effects: nothing is loaded, written or printed except the verdict.

NIOTS_CONTRACT <- list(
  ## Slots prepare_data() reads by name. The other nine slots of the current
  ## object are never touched by Stage B and are therefore outside the contract.
  final_demand_slots = c("Ft", "Fn", "Fm"),
  matrix_slots       = c("lista_Am", "lista_Z"),
  ## The producer's column names, and the vocabulary Stage B renames them to.
  demand_columns     = c(C = "HFCE", I = "GFCF", G = "GGFC", X = "EXPO", E = "INVNT"),
  ## Countries country_groups names in global_variables.R. Their absence does not
  ## raise an error: it produces empty figures.
  required_countries = c("USA", "DEU", "NLD", "JPN", "BRA", "MEX", "CHN", "IND")
)

validate_niots <- function(obj, strict = TRUE) {

  C <- NIOTS_CONTRACT
  slots <- c(C$final_demand_slots, C$matrix_slots)
  fail <- function(...) paste0(...)

  report <- function(msg) {
    if (is.null(msg)) {
      message("PASS: the candidate satisfies the Stage-B input contract.")
      return(invisible(TRUE))
    }
    message("FAIL: ", msg)
    invisible(FALSE)
  }

  ## --- 1. The object itself -------------------------------------------------
  ## global_variables.R:4-9 derives every downstream symbol from this shape.

  if (!is.list(obj) || is.data.frame(obj))
    return(report(fail("the object is not a list (global_variables.R:4)")))
  if (length(obj) == 0L)
    return(report(fail("the object is empty; WIOD_countries would be character(0)")))

  countries <- names(obj)
  if (is.null(countries) || any(!nzchar(countries)))
    return(report(fail("countries are unnamed; WIOD_countries requires names(NIOTs)")))
  if (anyDuplicated(countries))
    return(report(fail("duplicated country names: ",
                       paste(unique(countries[duplicated(countries)]), collapse = ", "))))
  bad_iso <- countries[!grepl("^[A-Z]{3}$", countries)]
  if (length(bad_iso))
    return(report(fail("names are not ISO-3 codes: ", paste(bad_iso, collapse = ", "),
                       "; get_tidy() uses them as the ISO factor (support_functions.R:500)")))

  ## --- 2. Slots, per country -----------------------------------------------
  ## tbls is read off the first country alone (global_variables.R:6), so an
  ## inconsistent country elsewhere is exactly the silent failure to catch.

  tbls <- names(obj[[1]])
  for (ct in countries) {
    if (!identical(names(obj[[ct]]), tbls))
      return(report(fail(ct, " has different slots from ", countries[1],
                         "; tbls is derived from the first country only (global_variables.R:6)")))
    missing <- setdiff(slots, names(obj[[ct]]))
    if (length(missing))
      return(report(fail(ct, " lacks the slot(s) ", paste(missing, collapse = ", "),
                         " that prepare_data() reads (support_functions.R:28-105)")))
  }

  ## --- 3. Years -------------------------------------------------------------
  ## years comes from the FIRST slot of the FIRST country (global_variables.R:7)
  ## and is then applied to every country and slot, so all must agree, in order:
  ## lapply(years, ...) |> bind_rows() is positional against order.by = dates.

  years <- names(obj[[1]][[1]])
  if (is.null(years) || length(years) == 0L)
    return(report(fail("the first slot of ", countries[1],
                       " is not a list keyed by year (global_variables.R:7)")))
  dates <- suppressWarnings(as.Date(paste0(years, "-01-01")))
  if (anyNA(dates))
    return(report(fail("year keys do not parse as dates: ",
                       paste(years[is.na(dates)], collapse = ", "),
                       " (support_functions.R:19)")))

  for (ct in countries) for (sl in slots) {
    if (!identical(names(obj[[ct]][[sl]]), years))
      return(report(fail(ct, "$", sl, " has year keys differing from ", countries[1],
                         "'s first slot; xts() would misalign them (support_functions.R:44)")))
  }

  ## --- 4. Class and shape, per country-year --------------------------------

  n_sectors <- NULL
  for (ct in countries) for (yr in years) {

    for (sl in C$final_demand_slots) {
      x <- obj[[ct]][[sl]][[yr]]
      ## dplyr::rename() and select() are called on it, so a matrix fails.
      if (!is.data.frame(x))
        return(report(fail(ct, "$", sl, "[[", yr, "]] is ", class(x)[1],
                           ", not a data.frame; rename() requires one (support_functions.R:29)")))
      absent <- setdiff(C$demand_columns, colnames(x))
      if (length(absent))
        return(report(fail(ct, "$", sl, "[[", yr, "]] lacks the column(s) ",
                           paste(absent, collapse = ", "),
                           " (support_functions.R:30-34)")))
      if (!all(vapply(x[, C$demand_columns, drop = FALSE], is.numeric, logical(1))))
        return(report(fail(ct, "$", sl, "[[", yr, "]] has non-numeric demand columns; ",
                           "colSums() would error (support_functions.R:37)")))
    }

    for (sl in C$matrix_slots) {
      m <- obj[[ct]][[sl]][[yr]]
      if (!(is.matrix(m) || is.data.frame(m)) || !is.numeric(as.matrix(m)))
        return(report(fail(ct, "$", sl, "[[", yr, "]] is not a numeric matrix; ",
                           "Am %*% Z %*% gastos requires one (support_functions.R:116)")))
      if (nrow(m) != ncol(m))
        return(report(fail(ct, "$", sl, "[[", yr, "]] is ", nrow(m), "x", ncol(m),
                           ", not square")))
      if (is.null(n_sectors)) n_sectors <- nrow(m)
      if (nrow(m) != n_sectors)
        return(report(fail(ct, "$", sl, "[[", yr, "]] is ", nrow(m), "x", ncol(m),
                           " against ", n_sectors, " elsewhere; the product would not conform")))
    }

    ## Fn supplies the right factor of Am %*% Z %*% gastos, so its row count is
    ## fixed by the matrices. Nothing checks that the ROWS are the same sectors
    ## in the same order: the product is positional and would be silently wrong.
    if (nrow(obj[[ct]][["Fn"]][[yr]]) != n_sectors)
      return(report(fail(ct, "$Fn[[", yr, "]] has ", nrow(obj[[ct]][["Fn"]][[yr]]),
                         " rows against ", n_sectors,
                         " sectors; the matrix product would not conform (support_functions.R:116)")))
  }

  ## --- 5. Hazards that satisfy the structure and corrupt the results -------
  ## Reported as violations under strict = TRUE. They are not type errors: the
  ## pipeline runs to completion and writes figures.

  if (strict) {

    absent <- setdiff(C$required_countries, countries)
    if (length(absent))
      return(report(fail("country_groups of global_variables.R:25-39 names ",
                         paste(absent, collapse = ", "),
                         ", absent here; group_plots() would write empty figures")))

    for (ct in countries) for (yr in years) {
      ft <- colSums(obj[[ct]][["Ft"]][[yr]][, C$demand_columns, drop = FALSE])
      if (any(ft == 0))
        return(report(fail(ct, " ", yr, ": Ft is zero for ",
                           paste(names(C$demand_columns)[ft == 0], collapse = ", "),
                           "; m = M / Ft is not finite (support_functions.R:139)")))
      fm <- colSums(obj[[ct]][["Fm"]][[yr]][, C$demand_columns, drop = FALSE])
      share <- fm / ft
      if (any(share < 0 | share > 1))
        return(report(fail(ct, " ", yr, ": the final-demand import share leaves [0,1] for ",
                           paste(names(C$demand_columns)[share < 0 | share > 1], collapse = ", "),
                           "; this is the defect of issue #34 (support_functions.R:139)")))
    }
  }

  report(NULL)
}
