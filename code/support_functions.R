library(tidyverse)
library(xts)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggbump)
library(patchwork)
library(ggh4x)
library(TSdist)
library(gt)

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

    Fn <- lapply(years, function(cur_year) {

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


    ## mAvg <- (data[["M"]][, "Total"] / (data[["Ft"]][,"Total"]))
    mAvg <- imp[, "Total"]

    mAvg <- xts(mAvg, order.by = dates)

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
    tmp_CDD <- 0
    for (v in vars) {
      df[,v] <- (1 - mAvg) * lag(wei[[v]]) * grw[[v]]
      tmp_CDX <- tmp_CDX + (-lag(mAvg) * lag(wei[[v]]) * gM) + (v == "X") * df[,v]
      tmp_CDD <- tmp_CDD + df[,v] * (v != "X")
    }




    df[, "CDD"] <- tmp_CDD
    df[, "CDX"] <- tmp_CDX

    total <- tmp_CDD + tmp_CDX
    colnames(total) <- c("CDD_CDX")

    df[, "CDD_CDX"] <- total

    df[,"M"] <- (lag(mAvg) * lag(DA) * gM[["Total"]]) * (-1)

    err_sqr <- (df[, "CDD_CDX"] - df[, "GDP"])^2 |>
      unlist(use.names = FALSE) |>
      mean(na.rm = TRUE)


    df <- df |>
      mutate(Total = rowSums(across(all_of(c(vars, "M")))))


    df <- xts(df, order.by = dates)


    tmp[[country]][["Average Import Content"]] <- df


                                        # Attribution method

    df <- df_gY |>
      as_tibble()


    tmp_CDD <- 0
    tmp_CDX <- 0

    ## STOP HERE
    for (v in vars) {
      df[, v] <- lag(wei[[v]]) * grw[[v]] - lag(m_wei[[v]]) * m_grw[[v]]
      tmp_CDD <- tmp_CDD + df[,v] * (v != "X")
      tmp_CDX <- tmp_CDX + df[,v] * (v == "X")
    }

    df[, "CDD"] <-  tmp_CDD
    df[, "CDX"] <-  tmp_CDX

    total <- tmp_CDD + tmp_CDX
    colnames(total) <- c("CDD_CDX")

    df[, "CDD_CDX"] <- total


    err_sqr <- (df[, "CDD_CDX"] - df[, "GDP"])^2 |>
      unlist(use.names = FALSE) |>
      mean(na.rm = TRUE)

    df <- df |>
      mutate(Total = rowSums(across(all_of(vars))))

    df <- xts(df, order.by = dates)

    tmp[[country]][["Attribution"]] <- df

                                        # Net Exports Method

    df <- df_gY |>
      as_tibble()

    tmp_CDD <- 0
    tmp_CDX <- 0
    for (v in c(vars, "M")) {
      var_sign <- ifelse(v == "M", -1, 1)
      df[,v] <- lag(wei[[v]]) * grw[[v]]*var_sign
      if (v != "M" && v != "X") {
        tmp_CDD <- tmp_CDD + df[, v]
      }
      if (v == "X" || v == "M") {
        tmp_CDX <- tmp_CDX + df[, v]
      }

    }


    df[, "CDD"] <-  tmp_CDD
    df[, "CDX"] <-  tmp_CDX

    total <- tmp_CDD + tmp_CDX
    colnames(total) <- c("CDD_CDX")

    df[, "CDD_CDX"] <- total


    err_sqr <- (df[, "CDD_CDX"] - df[, "GDP"])^2 |>
      unlist(use.names = FALSE) |>
      mean(na.rm = TRUE)

    df <- df |>
      mutate(Total = rowSums(across(all_of(c(vars, "M")))))

    df <- xts(df, order.by = dates)

    tmp[[country]][["Net Exports"]] <- df

  }

  return(tmp)

}

get_tidy <- function(data = decomp) {
  countries <- names(data)

  methods <- names(data[[1]])

  df <- data.frame()
  for (country in countries) {
    sngl <- data[[country]]

    ## NOTE: Making it tiddy
    for (meth in methods) {
      tmp <- sngl[[meth]] |>
        timetk::tk_tbl(rename_index = "Time") |>
        rownames_to_column()
      tmp <- tmp |>
        select(Time, all_of(vars)) |>
        mutate(Method = as.factor(meth)) |>
        mutate(ISO = country) |>
        mutate(ISO = as.factor(ISO))
      df <- rbind(df, tmp)
    }
  }

  df <- df |>
    pivot_longer(cols = !c(Time, Method, ISO), names_to = "Variable", values_to = "Contribution") |>
    mutate(Variable = as.factor(Variable))

  return(df)
}

get_imp <- function(IO = results) {

  countries <- names(IO)


  df <- data.frame()

  for (country in countries) {
    tmp <- IO[[country]][["m"]] |>
      timetk::tk_tbl(rename_index = "Time") |>
      rename(Average = Total) |>
      pivot_longer(cols = !c(Time), names_to = "Variable", values_to = "Coefficient") |>
      mutate(ISO = country) |>
      mutate(ISO = as.factor(ISO))

    df <- rbind(df, tmp)
  }
  return(df)
}


get_dGDP <- function(IO = results) {

  countries <- names(IO)


  df <- data.frame()

  for (country in countries) {
    tmp <- IO[[country]][["g"]] |>
      timetk::tk_tbl(rename_index = "Time") |>
      select(Time, GDP) |>
      mutate(ISO = country) |>
      mutate(ISO = as.factor(ISO))

    df <- rbind(df, tmp)
  }
  return(df)
}

plot_external_contrib <- function(
                                  df,
                                  group = NULL,
                                  countries = NULL,
                                  figs = "../figs/",
                                  tabs = "../tabs/",
                                  fig_extension = c("pdf", "png"),
                                  tab_extension = c("tex", "docx"),
                                  grouped,
                                  ...
                                  ) {

  ## FIXME: There might be some errors with the input data and with the area plot as well

  tag <- group

  p <- df |>
    filter(Variable %in% c("CDX", "X")) |>
    group_by(Time, Variable) |>
    ggplot(aes(x = Time, y = Contribution, color = Method)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste0("Contribution of external sector (CDX and X) across different method for ", tag),
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()

  if (grouped) {
    p <- p +
      facet_grid(rows = vars(Variable), cols = vars(ISO), scales = "free_y")
  } else {
    p <- p +
      facet_wrap(~ Variable, ncol = 1)
  }

  print(p)


  save_figs(plot = p, main = "External_Contrib_CDX_X", fig_extension = fig_extension, suffix = tag)

  if (grouped) {
    diffs <- "countries"
  } else {
    diffs <- "methods"
  }

  df <- df |>
    mutate(Time = lubridate::year(Time))

  p <- df |>
    filter(Variable %in% c("CDD", "CDX")) |>
    ggplot(aes(x = Time, y = Contribution, fill = Variable)) +
    geom_col(position = "stack", stat = "identity", width = 1, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(
      data = df |> filter(Variable == "GDP"),
      aes(x = Time, y = Contribution),
      color = "black",
      inherit.aes = FALSE,
      size = 1.2
    ) +
    labs(
      title = paste0("Domestic (CDD) and external (CDX) demand contribution across different methods for ", tag),
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()

  if (grouped) {
    p <- p +
      facet_grid(ISO ~ Method, scales = "free_y")
  } else {
    p <- p +
      facet_wrap(~Method, ncol = 1)
  }

  print(p)

  save_figs(plot = p, main = "Contrib_CDD_CDX", fig_extension = fig_extension, suffix = tag)
}

report_import_coeff <- function(
                                countries,
                                group = FALSE,
                                IO = results,
                                grouped,
                                figs = "../figs/",
                                tabs = "../tabs/",
                                fig_extension = c("pdf", "png"),
                                tab_extension = c("tex", "docx"),
                                ...
                                ) {


  imp <- get_imp(IO)

  dGDP <- get_dGDP(IO) |>
    drop_na() |>
    filter(ISO %in% countries)

  ## browser()

  if (!is.null(group)) {
    tag <- group
  } else {
    tag <- countries
  }


  df_path <- imp |>
    pivot_wider(names_from = "Variable", values_from = "Coefficient") |>
    left_join(dGDP, by = join_by(Time, ISO)) |>
    drop_na()

  df_path <- df_path |>
    pivot_longer(
      cols = -c(Time, GDP, ISO),  # Keep GDP and Year as columns
      names_to = "Variable",
      values_to = "Value"
    )



    p <- df_path |>
      filter(Variable != "E") |>
      ggplot(aes(y=Value, x=GDP, color = ISO)) +
      geom_path(
        lineend = "round",   # Smoother line ends
        linejoin = "round",  # Smoother path joints
        arrow = arrow(
          type = "closed",   # Solid arrowhead
          length = unit(0.2, "inches"),
          ends = "last"
          )
        ) +
      geom_point(size = 1.5) +
      facet_wrap(~ Variable, scales = "free_y") +
      labs(
        title = paste0("Import coefficient vs GDP growth ", tag),
        subtitle = "Arrow indicates the direction of time",
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
        ) +
      custom_theme()

    print(p)

  save_figs(plot = p, main = "Cycle_ImpCoef_dGDP", fig_extension = fig_extension, suffix = tag)




  df <- imp |>
    filter(ISO %in% countries) |>
    mutate( type=ifelse(Variable =="Average","Average","Individual"))


  p <- df |>
    filter(Variable != "E") |>
    select(!Time) |>
    ggplot(aes(y=Coefficient, x=Variable, fill=type)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#69b3a2", "grey")) +
    scale_alpha_manual(values=c(1,0.1)) +
    labs(
      title = paste0("Import coefficient boxplot across different expenditures for ", tag),
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()

  if (grouped) {
    p <- p +
      facet_wrap(~ ISO, scales = "free_y")
  }

  print(p)

  save_figs(plot = p, main = "ImpCoef_BoxPlot", fig_extension = fig_extension, suffix = tag)

  p <- df |>
    filter(Variable != "E") |>
    ggplot(aes(x = Time, y = Coefficient, color = Variable)) +
    geom_bump(size = 2) +
    facet_wrap(~ Variable, scales = "free_y") +
    geom_point(size = 6) +
    scale_color_brewer(palette = "Paired") +
    labs(
      title = paste0("Import coefficient time series across different expenditures for ", tag),
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()

  if (grouped) {
    p <- p +
      facet_wrap(~ ISO, scales = "free_y")
  }

  print(p)

  save_figs(plot = p, main = "ImpCoef_TimeSeries", fig_extension = fig_extension, suffix = tag)


  ## FIXME Table of import coeficients?

  if (is.null(group)) {
    tex_imp <- IO[[countries]][["m"]] |>
      as.data.frame() |>
      select(!Total) |>
      rownames_to_column(var = "Year") |>
      mutate(Year = forcats::fct_rev(as.factor(lubridate::year(Year))))

    grob_imp <- gridExtra::tableGrob(
                             formatC(tex_imp |> as.matrix(),
                                     digits = 3, format = "g"
                                     )
                           )


    tab_title <- paste0("Import coefficients for ", tag)

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

    fname <- paste0("../tabs/", countries, "_Import_Coefficients.tex")

    writeLines(tex_tbl, fname)
  }
}

plot_differenteces <- function(
                               df,
                               group = NULL,
                               countries = NULL,
                               methods,
                               grouped,
                               figs = "../figs/",
                               tabs = "../tabs/",
                               fig_extension = c("pdf", "png"),
                               tab_extension = c("tex", "docx"),
                               target_meth = "Import Content",
                               ...
                               ) {




  tag <- group

  for (meth in methods) {
    others <- setdiff(methods, meth)

    base_df <- df |>
      filter(Method == meth) |>
      select(Contribution)

    tmp_vars <- df |>
      filter(Variable != "GDP") |>
      ## filter(Variable != "CDD") |>
      select(Variable) |>
      unique() |>
      unlist(use.names = FALSE) |>
      as.character()

    for (alt in others) {

      for (vrbl in tmp_vars) {

        if (!grouped) {
          vrbl <- tmp_vars
          subtitle <- "Considering all variables "
        } else {
          subtitle <- paste0("Considering only ", vrbl, " ")
        }

        alt_df <- df |>
          filter(Method %in% c(meth, alt)) |>
          filter(Variable != "GDP") |>
          mutate(Year = forcats::fct_rev(as.factor(lubridate::year(Time)))) |>
          select(!Time)


        p <- alt_df |>
          filter(Variable %in% vrbl) |>
          filter(Variable != "E") |>
          ggplot(aes(x=Contribution,y=Year)) +
          geom_line(aes(group=Year), color="#E7E7E7", linewidth=3.5) +
          geom_point(aes(color=Method), size=3) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
          labs(
            title = paste0("Difference between ", alt, " and ", meth, " methods for ", tag),
            subtitle = subtitle,
            x = NULL, y = NULL, fill = NULL,
            caption = "Authors' own elaboration",
            ) +
          scale_x_continuous(labels = scales::percent_format(scale = 100)) +
          custom_theme()

        if (grouped) {
          p <- p +
            facet_wrap(~ISO, scales = "free_x")
        } else {
          p <- p +
            facet_wrap(~Variable, scales = "free_x")
        }

        print(p)

        tmp_main <- paste0("DotDiff_", alt, "_", meth)
        tmp_main <- tmp_main |>
          stringr::str_remove_all(" ")

        if (meth == target_meth) {
          save_figs(plot = p, main = tmp_main, fig_extension = fig_extension, suffix = tag)
        }

        alt_df <- df |>
          filter(Method %in% c(meth, alt)) |>
          filter(Variable != "GDP") |>
          filter(Variable %in% vrbl) |>
          drop_na() |>
          pivot_wider(names_from = Method, values_from = Contribution) |>
          rename(Base = !!sym(meth)) |>
          rename(Alt = !!sym(alt))

        cols_list <- c("palegreen3", "indianred1")
        val_list <- setNames(cols_list, c(meth, alt))



        p <- alt_df |>
          filter(Variable != "E") |>
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
          labs(
            title = paste0("Difference between ", alt, " and ", meth, " methods for ", tag),
            subtitle = paste0(subtitle, "(+) denotes overestimation, (-) denotes underestimation"),
            x = NULL, y = NULL, fill = NULL,
            caption = "Authors' own elaboration",
            ) +
          scale_y_continuous(labels = scales::percent_format(scale = 100)) +
          custom_theme()


        if (grouped) {
          p <- p +
            facet_wrap(~ISO, scales = "free_x")
        } else {
          p <- p +
            facet_wrap(~Variable, scales = "free_x")
        }

        print(p)

        tmp_main <- paste0("LineDiff_", alt, "_", meth)
        tmp_main <- tmp_main |>
          stringr::str_remove_all(" ")

        if (meth == target_meth) {
          save_figs(plot = p, main = tmp_main, fig_extension = fig_extension, suffix = tag)
        }

        if (!grouped) {
          break
        }





      }




    }


  }

  others <- setdiff(methods, target_meth)

  diff_df <- df |>
    pivot_wider(names_from = "Method", values_from = "Contribution") %>%
    mutate(across(
      .cols = all_of(others),
      .fns = ~ . - !!sym(target_meth),
      .names = "{.col}"
    )) |>
    pivot_longer(cols = !c(Time, ISO, Variable), names_to = "Method", values_to = "Difference") |>
    filter(Method != target_meth) |>
    drop_na()


  p <- diff_df |>
    filter(Variable %in% vrbl) |>
    filter(Variable != "E") |>
    ggplot(aes(x=Difference, fill = Method, color = Method)) +
    geom_density(aes(y = after_stat(scaled)), adjust=1.5, alpha=.3, na.rm = TRUE, trim = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste0("Scaled divergence distribution between different methods and ", target_meth, " for ", tag),
      subtitle = "Across Variables",
      x = NULL, y = NULL, fill = NULL,
      caption = "Authors' own elaboration",
      ) +
    custom_theme()

  if (grouped) {
    p <- p +
      facet_grid(ISO ~ Variable, scales = "free_x")
  } else {
    p <- p +
      facet_wrap(~Variable, scales = "free_x")
  }

  print(p)

  tmp_main <- paste0("DistDiff_", target_meth)
  tmp_main <- tmp_main |>
    stringr::str_remove_all(" ")

  if (!grouped) {
    save_figs(plot = p, main = tmp_main, fig_extension = fig_extension, suffix = tag)
  }
}

plot_decomp <- function(
                        df,
                        group = NULL,
                        countries = NULL,
                        methods,
                        figs = "../figs/",
                        tabs = "../tabs/",
                        fig_extension = c("pdf", "png"),
                        tab_extension = c("tex", "docx"),
                        grouped,
                        ...
                        ) {

  tag <- group

  for (meth in methods) {
    if (!grouped) {
      meth <- methods
      title <- paste0("Growth decompostion across different method for ", tag)
      subtitle <- ""
    } else {
      title <- paste0("Growth decompostion across different countries of ", tag)
      subtitle <- paste0("Using ", meth, " method")
    }

    ## Across methods



    p <- df |>
      filter(Variable != "GDP") |>
      filter(Variable != "E") |>
      filter(Method %in% meth) |>
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
      labs(
        title = title,
        subtitle = subtitle,
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
        ) +
      custom_theme()


    if (!grouped) {
      p <- p +
        facet_wrap(~Method, scales = "free_y")
    } else {
      p <- p +
        facet_wrap(~ISO, scales = "free_y")
    }

    print(p)

    if (!grouped) {
      save_figs(plot = p, main = "Growth_Decomp_Variable", fig_extension = fig_extension, suffix = tag)
      break
    }
  }


  tmp_vars <- df |>
    filter(Variable != "GDP") |>
    filter(Variable != "E") |>
    select(Variable) |>
    unique() |>
    unlist(use.names = FALSE) |>
    as.character()

  for (vrbl in tmp_vars) {

    if (!grouped) {
      vrbl <- tmp_vars
      subtitle <- "Considering all variables"
    } else {
      subtitle <- paste0("Considering only ", vrbl)
    }

    ## Accross variables (Plot)

    p <- df |>
      filter(Variable %in% vrbl) |>
      group_by(Time, Variable) |>
      ggplot(aes(x = Time, y = Contribution, fill = Method)) +
      ## FIXME: Increase the space between groups
      geom_col(color = "black", position = "dodge") +
      labs(
        title = paste0("Growth decompostion across different methods for ", tag),
        subtitle = subtitle,
        x = NULL, y = NULL, fill = NULL,
        caption = "Authors' own elaboration",
        ) +
      custom_theme()

    if (!grouped) {
      p <- p +
        facet_wrap(~Variable, scales = "free_y")
    } else {
      p <- p +
        facet_wrap(~ISO, scales = "free_y")
    }

    print(p)

    save_figs(plot = p, main = "Growth_Decomp_Method", fig_extension = fig_extension, suffix = tag)

    if(!grouped) {

      break
    }

  }




}


calculate_metrics <- function(
                              data,
                              countries,
                              probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
                              digits = 4L,
                              ...
                              ) {
  accuracy <- 10^(-digits)

  res_df <- data.frame()

  for (country in countries) {
                                        # 1. Prepare clean data (same as before)
  clean_data <- data |>
    filter(ISO == country) |>
    mutate(Contribution = as.numeric(Contribution)) |>
    group_by(Variable, Method) |>
    summarise(Contribution = list(na.omit(Contribution)), .groups = "drop")

                                        # 2. Get unique methods and variables
  methods <- unique(clean_data$Method)
  variables <- unique(clean_data$Variable)

                                        # 3. Build nested structure with FLIPPED hierarchy
  results <- map(methods, function(ref_method) {  # First level: methods
    method_data <- filter(clean_data, Method == ref_method)

    map(variables, function(var) {  # Second level: variables
      var_data <- filter(method_data, Variable == var)

      ref_series <- var_data |>
        pull(Contribution) |>
        pluck(1) |>
        unlist(use.names = FALSE) |>
        na.omit() |>
        as.vector()

      alt_methods <- setdiff(methods, ref_method)

                                        # Calculate metrics for each alternative method
      metrics <- map(alt_methods, function(alt_method) {
        alt_series <- clean_data |>
          filter(Variable == var, Method == alt_method) |>
          pull(Contribution) |>
          pluck(1) |>
          unlist(use.names = FALSE) |>
          na.omit() |>
          as.vector()

        quantile_ref <- quantile(ref_series, probs = probs, na.rm = TRUE)
        quantile_alt <- quantile(alt_series, probs = probs, na.rm = TRUE)
        quantile_res <- mean(abs(quantile_ref - quantile_alt))


        tmp <- tibble(
          !!sym("Mean") := mean(alt_series, na.rm = TRUE) - mean(ref_series, na.rm = TRUE),
          !!sym("Standard Deviation Ratio") := (sd(alt_series) / sd(ref_series)),
          !!sym("Euclidean Distance") := TSdist::EuclideanDistance(ref_series, alt_series),
          MAE = mean(abs(ref_series - alt_series)),
          MAD = max(abs(ref_series - alt_series)),
          MAPE = mean(abs(ref_series - alt_series)/abs(ref_series)),
          !!sym("Cross-Correlation Distance") := TSdist::CCorDistance(ref_series, alt_series),
          !!sym("Sign Divergence Rate") := mean(sign(ref_series) != sign(alt_series), na.rm = TRUE),
          !!sym("Difference in Autocorrelation") := TSdist::ACFDistance(ref_series, alt_series),
          !!sym("Mean Absolute Quantile Differences") := round(quantile_res, digits = digits)
        )
        return(tmp)
      }) |> set_names(alt_methods)

                                        # Convert to matrix
      metric_names <- names(metrics[[1]])
      matrix(
        unlist(metrics),
        nrow = length(metric_names),
        byrow = FALSE,
        dimnames = list(metric_names, alt_methods)
      )
    }) |> set_names(variables)
  }) |> set_names(methods)



  pivot_tbl <- imap_dfr(results, function(second_level, reference_name) {
    imap_dfr(second_level, function(df, variable_name) {

      df |>
        as.data.frame() |>
        tibble::rownames_to_column("Measure") |>
        pivot_longer(-Measure, names_to = "Method", values_to = "Differences") |>
        mutate(Variable = variable_name, Reference = reference_name)
    }
    )
  }) |>
    relocate(Measure, Method, Reference, Variable, Differences) |>
    mutate(ISO = country) |>
    mutate(across(where(is.character), as.factor))

    res_df <- bind_rows(res_df, pivot_tbl)
  }

  return(res_df)
}

tabulate_metrics <- function(
                             df,
                             ft_size = 20,
                             inline_print = TRUE,
                             target_var = "CDX",
                             target_ref = "Import Content",
                             extension = c(
                               ".docx", ".tex",
                               ".pdf", ".png"
                             ),
                             norm_meth = "Average Import Content",
                             ## FIXME: Add as another table
                             tabs = "../tabs/",
                             figs = "../figs/",
                             ...
                             ) {

  all_countries <- df |>
    select(ISO) |>
    unique() |>
    unlist(use.names = FALSE) |>
    as.vector()

  if (is.null(target_ref)) {
    all_refs <- target_ref <- df |>
      select(Reference) |>
      unique() |>
      unlist(use.names = FALSE) |>
      as.vector()
  } else {
    all_refs <- target_ref
  }

  if (is.null(target_var)) {

    ## NOTE: If not specified, use all
    target_var <- df |>
      select(Variable) |>
      unique() |>
      unlist(use.names = FALSE) |>
      as.vector()
  }

  for (country in all_countries) {
    for (ref in all_refs) {
      for (cur_var in target_var) {

        fname <- paste0(country, "_", ref, "_", cur_var)
        tmp_title <- paste0(
          "Dissimilarity measures in respect to ",
          ref, " method"
        )

      tmp_df <- df |>
        filter(ISO == country) |>
        filter(Reference == ref) |>
        filter(Variable == cur_var) |>
        select(!c(Variable, ISO, Reference)) |>
        pivot_wider(
          names_from = Method,
          values_from = Differences
        ) |>
        mutate(across(-Measure,
                      ~ . / !!sym(norm_meth),
                      .names = "{.col} Normalized")) %>%
        select(Measure,
               !ends_with("Normalized"),  # Select all non-normalized columns
               ends_with("Normalized"),    # Then select normalized columns
               everything())

        gt_obj <- tmp_df |>
          gt::gt() |>
          gt::tab_header(
            title = tmp_title,
            subtitle = "Lower values indicates lower dissimilarity to the reference"
          ) |>
          gt::tab_footnote(
            footnote = paste0(
              "MAE: Mean Absolute Error; MAD: Maximum Absolute Difference; ",
              "MAPE: Mean Absolute Percentage Error."
            )
          ) |>
          gt::tab_options(
            table.font.size = ft_size
          ) |>
          ## FIXME: Format the numbers
          fmt_number() |>
          gt::tab_spanner(
                label = paste0("Divergence in respect to ", target_ref),
                columns = !contains(c("Normalized", "Measure")),
                id = "unorm"
              ) |>
          gt::tab_spanner(
                label = paste0("Divergence norm. by ", norm_meth),
                ## FIXME: Check if there is a way to post remove the Normalize
                columns = contains("Normalized"),
                id = "norm"
              ) |>
          gt::tab_spanner(
                label = paste0(country),
                spanners = c("unorm", "norm"),
                id = "ISO"
              ) |>
          cols_label(
            .list = lapply(names(tmp_df), function(col) {
              gsub(" Normalized", "", col)  # Removes "_Normalized"
                                        # Alternative: gsub("Normalized", "", col) for any position
            }) %>%
              setNames(names(tmp_df))
          )
        ## FIXME: Define a grouping scheme if countries > 1

        if (inline_print) {
          gt_obj |>
            gt::as_gtable(plot = TRUE)
        }

        if (".docx" %in% extension) {

          tmp_name <- paste0(
            tabs,
            "/", ## NOTE: For some reason, it treats docx/tex files differently
            fname,
            ".docx"
          ) |>
            stringr::str_remove_all(" ")

          gt_obj |>
            gt::gtsave(
                  filename = tmp_name, path = tabs
                )
        }


        if (".tex" %in% extension) {
          tmp_name <- paste0(
            tabs,
            "/", ## NOTE: For some reason, it treats docx/tex files differently
            fname,
            ".tex"
          ) |>
            stringr::str_remove_all(" ")

          body_name <- paste0(
            tabs,
            "/", ## NOTE: For some reason, it treats docx/tex files differently
            "Body_",
            fname,
            ".tex"
          ) |>
            stringr::str_remove_all(" ")

          ## ## FIXME: Possible export the body only
          ## gt_obj |>
          ##   gt::gtsave(
          ##         filename = tmp_name, path = tabs
          ##       )
          ## tmp_body <- gt_obj |>
          ##   gt::extract_body(output = "latex")

        }

        ## if ("png" %in% extension) {

        ##   tmp_name <- paste0(
        ##     fname,
        ##     ".png"
        ##   ) |>
        ##     stringr::str_remove_all(" ")

        ##   gt_obj |>
        ##     gt::gtsave(
        ##           filename = tmp_name, path = figs,
        ##           vwidth = 1080,
        ##           quiet = TRUE,
        ##           selector = "table",
        ##           expand = c(1,20,1,5)
        ##           )
        ## }

        ## if (".pdf" %in% extension) {
        ##   tmp_name <- paste0(
        ##     fname,
        ##     ".pdf"
        ##   ) |>
        ##     stringr::str_remove_all(" ")
        ##   gt_obj |>
        ##     gt::gtsave(filename = tmp_name, path = figs)
        ## }

      }

    }
  }

}

tabulate_statistics <- function(
                             data,
                             ft_size = 20,
                             inline_print = TRUE,
                             extension = c(
                               ".docx", ".tex"
                               ## ".pdf", ".png"
                             ),
                             tabs = "../tabs",
                             figs = "../figs",
                             ...
                             ) {

  ## browser()

  foo <- df

}


save_figs <- function(
                      plot,
                      figs = "../figs/",
                      main = NA_character_,
                      fig_extension = c("pdf", "png"),
                      suffix = NA_character_,
                      width = plotW,
                      height = plotH,
                      units = "in"
                      ) {

  for (ext in fig_extension) {
    fname <- paste0(
      figs,
      main,
      "_",
      suffix,
      ".",
      ext
    )
    ggsave(
      fname,
      plot,
      device = ext,
      width = width,
      height = height,
      units = units
    )
  }
}


custom_theme <- function() {
  p <- tryCatch(last_plot(), error = function(e) NULL)
  if (is.null(p)) return(theme_ipsum()) # Fallback if no plot exists

  # Initialize components
  components <- list()

  # 1. Check if color/fill scales exist by inspecting plot object
  existing_scales <- sapply(p$scales$scales, function(s) s$aesthetics[1])

  # 2. Add color scale only if:
  #    - color aesthetic exists in mapping/layers AND
  #    - no color scale exists already
  has_color_aes <- !is.null(p$mapping$colour) ||
                   any(sapply(p$layers, function(l) !is.null(l$mapping$colour)))

  if (has_color_aes && !"colour" %in% existing_scales) {
    components <- c(components, list(scale_color_ipsum()))
  }

  # 3. Same for fill scale
  has_fill_aes <- !is.null(p$mapping$fill) ||
                  any(sapply(p$layers, function(l) !is.null(l$mapping$fill)))

  if (has_fill_aes && !"fill" %in% existing_scales) {
    components <- c(components, list(scale_fill_ipsum()))
  }

  # 4. Date handling (unchanged)
  time_x <- FALSE
  if (!is.null(p$mapping$x)) {
    x_var <- rlang::as_name(p$mapping$x)
    if (inherits(p$data[[x_var]], "Date")) {
      components <- c(components, list(
        scale_x_date(
          breaks = scales::breaks_width("5 years"),
          minor_breaks = scales::breaks_width("1 year"),
          labels = scales::label_date("%Y"),
          expand = c(0, 0)  # No padding
        )
      ))
      time_x <- TRUE
    }
  }
  is_bar_plot <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomBar")))


  # 5. Theme components (unchanged)
  theme_comp <- theme_ipsum(grid = "XY", base_family = "sans") +
    theme(
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 20),
      axis.text.x = if (time_x) element_text(angle = 45, hjust = 1) else element_text(),
      axis.ticks.x.minor = if (time_x) element_line(color = "gray95", linewidth = 0.3) else element_line(),
      panel.grid.minor.x = if (time_x) element_line(color = "grey95", linewidth = 0.2) else element_line()
    )

  c(components, list(theme_comp))
}

group_plots <- function(
                        data = decomp,
                        IO = results,
                        groups = country_groups,
                        verbose = FALSE,
                        figs = "../figs/",
                        tabs = "../tabs/",
                        fig_extension = c("pdf", "png"),
                        tab_extension = c("tex", "docx"),
                        target_meth = "Import Content",
                        ...
                        ) {

  methods <- names(data[[1]])

  all_data <- get_tidy(data = data)

  for (group in names(groups)) {

    group_tag <- group
    tmp_msg <- paste0(
      "\nProducing results for ",
      group_tag
    )
    cat(tmp_msg)
    countries <- groups[[group]]
    if (length(countries) > 1) {
      grouped <- TRUE
    } else {
      grouped <- FALSE
    }
    pdf(
      paste0("../reports/", group_tag, ".pdf"),
      width = plotW, height = plotH
    )
    df <- all_data |>
      filter(ISO %in% countries)

    cat(" ...")
    plot_decomp(df, group = group, countries = countries, methods, grouped = grouped)

    if ((grouped && verbose) || !(grouped)) {
      cat(" ...")
      plot_differenteces(df, group = group, countries = countries, methods, grouped = grouped, target_meth = target_meth)
    }

    cat(" ...")
    plot_external_contrib(df = df, group = group, countries = countries, grouped = grouped)

    cat(" ...")
    report_import_coeff(group = group, IO = IO, countries = countries, grouped = grouped)


    ## FIXME: Aparently, it is not working
    ## cat(" ...")
    ## tabulate_statistics(data = df, countries = countries, grouped = grouped)

    cat(" ...")
    metrics_df <- calculate_metrics(data = df, countries = countries, grouped = grouped)

    cat(" ...")
    tabulate_metrics(metrics_df)

    dev.off()
    cat(" done!")

  }
}



