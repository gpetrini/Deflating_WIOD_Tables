
single_plots <- function(
                           data = decomp,
                           IO = results
                           ) {

  countries <- names(data)

  methods <- names(data[[1]])

  for (country in countries) {
    pdf(
      paste0("../reports/", country, ".pdf"),
      width = plotW, height = plotH
    )

    df <- get_tidy(data = data) |>
      filter(ISO == country) |>
      select(!ISO)



    plot_decomp(df, group = NULL, countries = country, methods)

    ## Contribution of external sector and exports

    plot_external_contrib(df = df, group = NULL, countries = country)


    plot_differenteces(df, group = NULL, countries = country, methods = methods)


    ## Distribution of import coeficients



    report_import_coeff(group = NULL, IO = IO, countries = country)




    dev.off()
  }



}
