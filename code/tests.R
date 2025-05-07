source("./global_variables.R")
source("./support_functions.R")

results <- prepare_data()


decomp <- decompose_growth()

## Inhirited by the main function

df <- get_tidy(data = decomp) |>
  ## filter(ISO == "BRA") |>
  mutate(Contribution = as.numeric(Contribution))


target_var <- "CDX"
target_ref <- "Import Content"
norm_meth <- "Average Import Content"

## That should be on the scope of the function

all_countries <- df |> select(ISO) |> unique() |> unlist(use.names = FALSE) |> as.character()

## FIXME: Ensure the normalization by norm_meth
metrics_df <- calculate_metrics(data = df, countries = all_countries, grouped = FALSE) |>
  filter(Variable == target_var) |>
  filter(Method != target_ref) |>
  filter(Reference == target_ref) |>
  select(!Reference) |>
  select(!Variable)

wider_df <- metrics_df |>
  group_by(Measure, ISO, Method) |>
  pivot_wider(names_from = "Method", values_from = "Differences") |>
  mutate(across(where(is.numeric), ~ (. / !!sym(norm_meth)) - 1, .names = "{.col} Normalized")) |>
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.infinite(.), NA_real_, .)
  )) |>
  select(!c(!!sym(paste0(norm_meth, " Normalized")))) |>
  select(ISO, everything()) |>
  arrange(ISO) |>
  ungroup()

tab_title <- paste0(
  "Comparative dissimilarity metrics in respect to ",
  target_ref,
  " method for ",
  target_var,
  " variable (whole period)"
)

## FIXME: Set the number of normalized and unormalized programatically
custom_cols <- wider_df |>
  select(!ISO) |>
  colnames() |>
  stringr::str_replace_all("Average", "Avg.") |>
  stringr::str_replace_all("Normalized", "Norm.")

not_norm <- sum(
  !grepl("Measure", custom_cols) &
  !grepl("Norm.", custom_cols)
)

norm_col <- sum(
  !grepl("Measure", custom_cols) &
  grepl("Norm.", custom_cols)
)

tex_table <- wider_df |>
  select(!ISO) |>
  kableExtra::kable(
                format = "latex",       # For PDF output
                booktabs = TRUE,        # Professional styling
                longtable = TRUE,       # Span multiple pages
                caption = tab_title,
                col.names = custom_cols,
                digits = 3
              ) |>
  kableExtra::kable_styling(
                latex_options = c("hold_position", "repeat_header"),  # Repeat headers on each page
                font_size = 9
              ) |>
  kableExtra::add_header_above(               # Add a header above Method columns
                c(
                  " ", "Method" = not_norm,
                  "Normalized Methods" = norm_col
                  ),
                bold = TRUE
              ) |>
  kableExtra::pack_rows(
    index = table(wider_df$ISO),
    label = "\\textbf{%s}",  # Bold group labels
    italic = FALSE,
    latex_gap_space = "0.5em",
    hline_after = TRUE  # Line after each group
  ) |>
  kableExtra::footnote(
    general = paste0(
      "MAE: Mean Absolute Error; MAD: Maximum Absolute Difference; ",
      "MAPE: Mean Absolute Percentage Error; ",
      "MAQD: Mean Absolute Quantile Differences. ",
      "Note: — indicates values that were infinite due to division by zero."
    ),
    general_title = "Legend:",
    footnote_as_chunk = FALSE,
    threeparttable = TRUE
    ) |>
  kableExtra::landscape()

tex_table <- gsub("NaN", "\\\\textendash", tex_table)
tex_table <- gsub("NA", "\\\\textendash", tex_table)

fname <- paste0("../tabs/All_Metrics.tex")
writeLines(tex_table, fname)
