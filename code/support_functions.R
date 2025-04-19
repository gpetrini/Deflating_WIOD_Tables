
library(tidyverse)
## @DOUG: xts substitui lag do r-base
library(xts)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggbump)
library(patchwork)
library(ggh4x)

prepare_data <- function(data_base = NIOTs) {

  results <- list()

  for (country in WIOD_countries) {

    data <- data_base[[country]]
    dates <- as.Date(paste0(years, "-01-01"))


    Meepo <- list()

                                        # Computes GDP by summing Value Added (net of taxes and subsidies)

                                        # Componentes da demanda total - Ft #
    Ft <- lapply(years, function(cur_year) {
      data[["Ft"]][[cur_year]] |>
        rename(
          C = HFCE,
          G = GGFC,
          I = GFCF,
          X = EXPO,
          E = INVNT
        ) |>
        select(C, I, G, X, E) |>
        colSums()
    }) |>
      bind_rows() # Combina todos os resultados em um único dataframe

    Ft <- Ft |>
      mutate(Total = rowSums(Ft))

    Ft <- xts::xts(x = Ft, order.by = dates)

    Meepo[["Ft"]] <- Ft

                                        # Componentes da demanda nacional - Fn #

    Fn = lapply(years, function(cur_year) {

      data[["Fn"]][[cur_year]] |>
        rename(
          C = HFCE,
          G = GGFC,
          I = GFCF,
          X = EXPO,
          E = INVNT
        ) |>
        select(C,I,G,X,E) |>
        colSums()
    }) |>
      bind_rows() # Combina todos os resultados em um único dataframe

    Fn <- Fn |>
      mutate(Total = rowSums(Fn))

    Fn <- xts::xts(x = Fn, order.by = dates)

    Meepo[["Fn"]] <- Fn

                                          # Demanda final importada por componentes da demanda agregada - M_F #

    M_F <- lapply(years, function(cur_year) {

      data[["Fm"]][[cur_year]] |>
        rename(
          M_C = HFCE,
          M_G = GGFC,
          M_I = GFCF,
          M_X = EXPO,
          M_E = INVNT
        ) |>
        select(M_C,M_I,M_G,M_X,M_E) |>
        colSums()

    }) |>
      bind_rows() # Combina todos os resultados em um único dataframe


    M_F <- M_F |>
      mutate(Total = rowSums(M_F))

    M_F <- xts::xts(x = M_F, order.by = dates)

    Meepo[["M_F"]] <- M_F

                                          # Consumo intermediario importado relacionado aos componentes da demanda final - M_I #

    M_I = lapply(years, function(cur_year){

      Am <- data[["lista_Am"]][[cur_year]] ## NOTE: coeficientes tecnicos importados
      Z <- data[["lista_Z"]][[cur_year]] ## NOTE: matriz inversa de Leontief

      gastos <- data[["Fn"]][[cur_year]] |> ## NOTE: vetor com os componentes da demanda final nacional
        rename(
          C = HFCE,
          G = GGFC,
          I = GFCF,
          X = EXPO,
          E = INVNT
        ) |>
        select(C,I,G,X,E) |>
        as.matrix()

      df <- Am %*% Z %*% gastos |>
        colSums() |>
        t() |>
        as.data.frame() |>
        rename_with(~ paste0("M_", .))

      return(df)

    }) |>
      bind_rows() # Combina todos os resultados em um único dataframe

    M_I <- M_I |>
      mutate(Total = rowSums(across(everything())))
    M_I <- xts::xts(x = M_I, order.by = dates)

    Meepo[["M_I"]] <- M_I

                                          # Conteudo importado total por componente da demanda agregada - M #

    M <- M_F + M_I
    Meepo[["M"]] <- M


    m <- (M / Ft) |>
      as.data.frame() |>
      rename_with(~ sub("^M_", "", .))

                                        # Parcela de conteudo importado por componente da demanda agregada - m #
    m <- xts::xts(x = m, order.by = dates)

    Meepo[["m"]] <- m


    Ft_df <- Ft |>
      as_tibble(rownames = "date")

    m_df <- m |>
      as_tibble(rownames = "date")

    GDP_df <- Ft_df |>
      inner_join(m_df, by = "date", suffix = c("_Ft", "_m")) |>
      mutate(GDP = (1 - C_m) * C_Ft +
               (1 - I_m) * I_Ft +
               (1 - E_m) * E_Ft +
               (1 - G_m) * G_Ft +
               (1 - X_m) * X_Ft) |>
      select(date, GDP)

    GDP <- xts::xts(GDP_df$GDP, order.by = as.Date(GDP_df$date))
    colnames(GDP) <- "GDP"


    Meepo[["GDP"]] <- GDP

    # Compute weights
    WEI <- sweep(Ft, 1, GDP[, "GDP"], `/`)
    WEI <- cbind(WEI, M = setNames(M[, "Total"] / GDP[, "GDP"], NULL))



    Meepo[["Weights"]] <- WEI

    M_WEI <- sweep(M, 1, GDP[, "GDP"], `/`) |>
      as.data.frame() |>
      rename_with(~ sub("^M_", "", .))

    Meepo[["Import Weights"]] <- M_WEI

    M_GRW <- (diff(M) / lag(M)) |>
      as.data.frame() |>
      rename_with(~ sub("^M_", "", .))


    Meepo[["Import Growth"]] <- M_GRW

    check <- rowSums(WEI[, !(colnames(WEI) %in% c("Total", "M"))]) - WEI[, "M"]
    colnames(check) <- c("Total Weights")
    Meepo[["Weights Check"]] <- check


    GRW <- diff(Ft) / lag(Ft)
    gM <- diff(M[, "Total"]) / lag(M[, "Total"])
    colnames(gM) <- "M"
    GRW <- merge(GRW, gM)


    gY <- diff(GDP[, "GDP"]) / lag(GDP[, "GDP"])
    colnames(gY) <- "GDP"
    GRW <- merge(GRW, gY)


    Meepo[["g"]] <- GRW

    mGRW <- diff(m) / lag(m)

    Meepo[["gm"]] <- mGRW
    # Compute growth rates


    results[[country]] <- Meepo
  }

  return(results)
}

decompose_growth <- function(data_base = results) {

  tmp <- list()

  countries <- names(data_base)

  for (country in countries) {


    data <- data_base[[country]]

    imp <- data[["m"]]
    dates <- index(imp)

    vars <- setdiff(colnames(imp), "Total")
    vars_dom <- setdiff(vars, "X")


    imp <- imp |> as_tibble()
    grw <- data[["g"]]

    df_gY <- df <- grw[, "GDP"]

    grw <- grw |> as_tibble()
    gms <- data[["gm"]] |> as_tibble()
    wei <- data[["Weights"]] |> as_tibble()
    m_wei <- data[["Import Weights"]] |> as_tibble()
    m_grw <- data[["Import Growth"]] |> as_tibble()



                                        # Import content method


    CDA <- map(vars, ~ (1 - imp[, .x]) * lag(wei[, .x]) * grw[, .x]) |>
      reduce(`+`)
    colnames(CDA) <- c("CDA")
    CDA <- xts::xts(CDA, order.by = dates)

    df <- merge(df, CDA)


    CDI <- map(vars, ~ ( - lag(imp[, .x])) * lag(wei[, .x]) * gms[, .x]) |>
      reduce(`+`)
    colnames(CDI) <- c("CDI")
    CDI <- xts::xts(CDI, order.by = dates)


    df <- merge(df, CDI)



    IMP_CONTENT <- CDA + CDI
    colnames(IMP_CONTENT) <- c("CDA_CDI")
    IMP_CONTENT <- xts::xts(IMP_CONTENT, order.by = dates)

    df <- merge(df, IMP_CONTENT)


    CDD <- map(vars_dom, ~ (1 - imp[, .x]) * lag(wei[, .x]) * grw[, .x]) |>
      reduce(`+`)

    for (v in vars) {
      foo <- (1 - imp[, v]) * lag(wei[, v]) * grw[, v]
      foo <- xts::xts(foo, order.by = dates)
      df <- merge(df, foo)
    }

    colnames(CDD) <- c("CDD")
    CDD <- xts::xts(CDD, order.by = dates)

    df <- merge(df, CDD)


    CDX <- (CDI |> as_tibble()) +
      (1 - imp[, "X"]) * lag(wei[, "X"]) * grw[, "X"]
    colnames(CDX) <- c("CDX")
    CDX <- xts::xts(CDX, order.by = dates)

    df <- merge(df, CDX)



    total <- CDD + CDX
    colnames(total) <- c("CDD_CDX")

    df <- merge(df, total)

    tmp[[country]][["Import Content"]] <- df



                                        # Average Import Content


    ## mAvg <- (data[["M"]] / data[["Ft"]])[,"Total"] |>
    mAvg <- (data[["M"]][, "Total"] / (data[["Ft"]][,"Total"]))
    gM <- (diff(mAvg) / lag(mAvg)) |>
      as_tibble()

    mAvg <- mAvg |>
      as_tibble()
    colnames(mAvg) <- c("m")

    DA <- ((data[["Ft"]][, "Total"]) / data[["GDP"]]) |>
      as_tibble()

    colnames(DA) <- c("DA")


    df <- df_gY |>
      as_tibble()

    tmp_CDX <- 0
    for (v in vars) {
      df[,v] <- (1 - mAvg) * lag(wei[[v]]) * grw[[v]]
      tmp_CDX <- tmp_CDX + (-mAvg * lag(wei[[v]]) * grw[[v]])
    }

    df[,"M"] <- (lag(mAvg) * lag(DA) * gM[["Total"]]) * (-1)

    ## @DOUG: FIXME Faz sentido essa ser a contriguição do setor externo?
    df[,"CDX"] <- tmp_CDX + df[, "M"]
    ## NOTE: As a workaround


    df <- df |>
      mutate(Total = rowSums(across(all_of(c(vars, "M"))))) |>
      mutate(CDD = Total - CDX) ## NOTE: as a workaround


    df <- xts(df, order.by = dates)


    tmp[[country]][["Average Import Content"]] <- df

    df <- NULL

    # Attribution method

    df <- df_gY |>
      as_tibble()


    tmp_CDX <- 0
    for (v in vars) {
      df[, v] <- lag(wei[[v]]) * grw[[v]] - lag(m_wei[[v]]) * m_grw[[v]]
      tmp_CDX <- tmp_CDX + (- lag(m_wei[[v]]) * m_grw[[v]])
    }

    ## @DOUG: FIXME Faz sentido essa ser a contriguição do setor externo?
    ## |- Por alguma razão, está gerando resultados idêndicos ao NX
    df[, "CDX"] <- lag(wei[["X"]]) * grw[["X"]] + tmp_CDX


    df <- df |>
      mutate(Total = rowSums(across(all_of(vars)))) |>
      mutate(CDD = Total - CDX) ## NOTE: as a workaround

    df <- xts(df, order.by = dates)

    tmp[[country]][["Attribution"]] <- df

    # Net Exports Method


    df <- df_gY |>
      as_tibble()

    for (v in c(vars, "M")) {
      if (v != "M") {
        df[,v] <- lag(wei[[v]]) * grw[[v]]
      } else {
        df[,v] <- lag(wei[[v]]) * grw[[v]] * (-1)
      }
    }


    df <- df |>
      mutate(Total = rowSums(across(all_of(c(vars, "M"))))) |>
      mutate(CDX = X + M) |> ## NOTE: M already in negative terms
      mutate(CDD = Total - CDX) ## NOTE: as a workaround

    df <- xts(df, order.by = dates)

    tmp[[country]][["Net Exports"]] <- df

  }

  return(tmp)

}

single_plots <- function(
                           data = decomp,
                           IO = results
                           ) {

  countries <- names(data)

  methods <- names(data[[1]])

  expenditures_dom <- c("C", "I", "G", "E")
  expenditures <- c(expenditures_dom, "X", "CDX", "CDD")
  vars <- c(expenditures, "GDP")

  plotW     <- 16     # plot window width
  plotH     <- 12     # plot window height

  for (country in countries) {
    pdf(
      paste0("../reports/", country, ".pdf"),
      width = plotW, height = plotH
    )
    sngl <- data[[country]]
    df <- data.frame()

    ## NOTE: Making it tiddy
    for (meth in methods) {
      tmp <- sngl[[meth]] |>
        timetk::tk_tbl(rename_index = "Time") |>
        rownames_to_column()
      tmp <- tmp |>
        select(Time, all_of(vars)) |>
        mutate(Method = as.factor(meth))
      df <- rbind(df, tmp)
    }

    df <- df |>
      pivot_longer(cols = !c(Time, Method), names_to = "Variable", values_to = "Contribution") |>
      mutate(Variable = as.factor(Variable))

    ## Across methods
    p <- df |>
      filter(Variable != "GDP") |>
      group_by(Time, Variable) |>
      ggplot(aes(x = Time, y = Contribution, fill = Variable)) +
      geom_col(
        ## width = 0.6,
        color = "black",
        position = "dodge"
      ) +
      geom_point(
        data = df |> filter(Variable == "GDP"),
        aes(x = Time, y = Contribution),
        color = "black"
      ) +
      facet_wrap(~Method, scales = "free_y") +
      labs(
        title = paste0("Growth decompostion across different method for ", country),
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
      ) +
      scale_color_ipsum() +
      scale_fill_ipsum() +
      theme_ipsum(grid="XY", base_family = "sans")

    print(p)


    ## Contribution of external sector and exports

    ## FIXME: For the attribution method, use exports instead of CDX
    p <- df |>
      filter(Variable %in% c("CDX", "X")) |>
      group_by(Time, Variable) |>
      ggplot(aes(x = Time, y = Contribution, color = Method)) +
      geom_line(linewidth = 1.2) +
      facet_wrap(~ Variable, ncol = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(
        title = paste0("Contribution of external sector across different method for ", country),
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
      ) +
      scale_color_ipsum() +
      scale_fill_ipsum() +
      theme_ipsum(grid="XY", base_family = "sans")

    print(p)

    ## TODO Stacked bar plot accross methods (Plot)


    p <- df |>
      filter(Variable %in% c("CDD", "CDX")) |>
      ggplot(aes(x = Time, y = Contribution, fill = Variable)) +
      geom_area(alpha=0.6 , size=.5, colour="black", position = "stack") +
      ## FIXME: Add GDP growth?
      facet_wrap(~Method, ncol = 1) +
      labs(
        title = paste0("Domestic (CDD) and external (CDX) demand contribution across different methods for ", country),
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
      ) +
      scale_color_ipsum() +
      scale_fill_ipsum() +
      theme_ipsum(grid = "XY", base_family = "sans")

    print(p)

    ## TODO Accross methods (Table)

    ## Accross variables (Plot)

    p <- df |>
      filter(Variable != "GDP") |>
      group_by(Time, Variable) |>
      ggplot(aes(x = Time, y = Contribution, fill = Method)) +
      ## FIXME: Increase the space between groups
      geom_col(color = "black", position = "dodge") +
      facet_wrap(~Variable, scales = "free_y") +
      labs(
        title = paste0("Growth decompostion across different methods for ", country),
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
      ) +
      scale_color_ipsum() +
      scale_fill_ipsum() +
      theme_ipsum(grid="XY", base_family = "sans")

    print(p)
    ## TODO Accross variables (Table)
    ## Lollipop plot and line diff

    for (meth in methods) {
      others <- setdiff(methods, meth)

      base_df <- df |>
        filter(Method == meth) |>
        select(Contribution)

      for (alt in others) {
        alt_df <- df |>
          filter(Method %in% c(meth, alt)) |>
          filter(Variable != "GDP") |>
          filter(Variable != "CDD") |>
          mutate(Year = forcats::fct_rev(as.factor(lubridate::year(Time)))) |>
          select(!Time)

        p <- alt_df |>
          ggplot(aes(x=Contribution,y=Year)) +
          geom_line(aes(group=Year), color="#E7E7E7", linewidth=3.5) +
          geom_point(aes(color=Method), size=3) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
          facet_wrap(~Variable, scales = "free_x") +
          labs(
            title = paste0("Difference between ", alt, " and ", meth, " methods for ", country),
            x = NULL, y = NULL, fill = NULL,
            caption = "Authors' own elaboration",
            ) +
          scale_x_continuous(labels = scales::percent_format(scale = 100)) +
          scale_color_ipsum() +
          scale_fill_ipsum() +
          theme_ipsum(grid="XY", base_family = "sans")

        print(p)


        alt_df <- df |>
          filter(Method %in% c(meth, alt)) |>
          filter(Variable != "GDP") |>
          filter(Variable != "CDD") |>
          drop_na() |>
          pivot_wider(names_from = Method, values_from = Contribution) |>
          rename(Base = !!sym(meth)) |>
          rename(Alt = !!sym(alt))

        cols_list <- c("palegreen3", "indianred1")
        val_list <- setNames(cols_list, c(meth, alt))

        p <- alt_df |>
          ggplot(aes(x = Time)) +
          geom_line(aes(y = Base, color = meth)) +
          geom_line(aes(y = Alt, color = alt)) +
          stat_difference(aes(ymin = Base, ymax = Alt), alpha = 0.3, levels = c("(+)", "(-)"),) +
          scale_fill_manual(
            values = c(
              "(+)" = "palegreen3",     # now green
              "(-)" = "indianred1"     # now red
            )
          ) +
          scale_color_manual(values = val_list) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
          facet_wrap(~Variable, scales = "free_y") +
          labs(
            title = paste0("Difference between ", alt, " and ", meth, " methods for ", country),
            subtitle = ("(+) denotes overestimation, (-) denotes underestimation"),
            x = NULL, y = NULL, fill = NULL,
            caption = "Authors' own elaboration",
            ) +
          scale_y_continuous(labels = scales::percent_format(scale = 100)) +
          ## scale_color_ipsum() +
          ## scale_fill_ipsum() +
          theme_ipsum(grid="XY", base_family = "sans")

        print(p)

      }
    }

    ## Distribution of import coeficients


    imp <- IO[[country]][["m"]] |>
      as_tibble() |>
      rename(Average = Total) |>
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Coefficient")



    ## p <- imp |>
    ##   ggplot(aes(x=Coefficient, y=Variable, fill=Variable)) +
    ##   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    ##   labs(
    ##     title = paste0("Import coefficient distribution across different expenditures for ", country),
    ##     x = NULL, y = NULL, fill = NULL,
    ##     caption = "Authors' own elaboration",
    ##     ) +
    ##   scale_color_ipsum() +
    ##   scale_fill_ipsum() +
    ##   theme_ipsum(grid="XY", base_family = "sans")

    ## print(p)


    p <- imp |>
      mutate( type=ifelse(Variable =="Average","Average","Individual")) |>
      ggplot(aes(y=Coefficient, x=Variable, fill=type)) +
      geom_boxplot() +
      scale_fill_manual(values=c("#69b3a2", "grey")) +
      scale_alpha_manual(values=c(1,0.1)) +
      coord_cartesian(ylim = c(0, 0.425)) +  ## NOTE: Because inventories is very volatile
      labs(
        title = paste0("Import coefficient boxplot across different expenditures for ", country),
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
        ) +
      theme_ipsum(grid="XY", base_family = "sans")

    print(p)


    bump <- IO[[country]][["m"]] |>
      timetk::tk_tbl(rename_index = "Time") |>
      rename(Average = Total) |>
      pivot_longer(cols = !c(Time), names_to = "Variable", values_to = "Coefficient")

    p <- bump |>
      ggplot(aes(x = Time, y = Coefficient, color = Variable)) +
      geom_bump(size = 2) +
      facet_wrap(~ Variable, scales = "free_y") +
      geom_point(size = 6) +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = paste0("Import coefficient time series across different expenditures for ", country),
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
        ) +
      theme_ipsum(grid="XY", base_family = "sans")

    print(p)

    ## TODO Table of import coeficients?

    tex_imp <- IO[[country]][["m"]] |>
      as.data.frame() |>
      select(!Total) |>
      rownames_to_column(var = "Year") |>
      mutate(Year = forcats::fct_rev(as.factor(lubridate::year(Year))))

    grob_imp <- gridExtra::tableGrob(
                             formatC(tex_imp |> as.matrix(),
                                     digits = 3, format = "g"
                                     )
                           )


    tab_title <- paste0("Import coefficients for ", country)

  tabs <- wrap_plots(grob_imp) +
    plot_annotation(title = tab_title)


    print(tabs)

    tex_tbl <- tex_imp |>
      kableExtra::kable(
                    digits = 3,
                    format = "latex", booktabs = TRUE, escape = FALSE, longtable = TRUE,
                    caption = tab_title,
                    label = "TAB-IMP-MCOEFF"
                  )

    fname <- paste0("../tabs/", country, "_Import_Coefficients.tex")
    writeLines(tex_tbl, fname)



    dev.off()
  }



}
