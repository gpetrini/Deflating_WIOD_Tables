get_common <- function(
                       inputs_folder = "../inputs/",
                       ftitle = "titulos.xlsx",
                       fagg = "agregador.xslx",
                       download = FALSE,
                       ...
                       ) {

  res_obj <- list()
  ftitle <- paste0(inputs_folder, ftitle)

  OCDE_country <- read_excel(ftitle, sheet = "country")

                                        # Limpa os dados - tira nome em frances
  OCDE_country$Country <- sub(" / .*", "", OCDE_country$Country)

  res_obj[["OCDE_country"]] <- OCDE_country

  ## Titulo dos setores ##
  titulo <- read_excel(ftitle, sheet = "sector") |>
    select(Code) |>
    unlist()

  res_obj[["titulo"]] <- titulo

  ## Titulo da demanda final ##
  titulo_fd <- read_excel(ftitle, sheet = "fd") |>
    select(Code) |>
    unlist()

  res_obj[["titulo_fd"]] <- titulo_fd

  ## Titulo da demanda final ##
  titulo_vbp <- read_excel(ftitle, sheet = "vbp") |>
    select(Code) |>
    unlist()


  res_obj[["titulo_vbp"]] <- titulo_vbp
                                        # Agregador de setores

  agregador <- read_excel(fagg) |>
    select(-Code) |>
    as.matrix()

  rownames(agregador) <- titulo

  res_obj[["agregador"]] <- agregador

                                        # titulo curto

  titulo_agregador <- colnames(agregador)

  res_obj[["titulo_agregador"]] <- titulo_agregador

  return(res_obj)
}

get_deflator <- function(
                         common_obj = NULL,
                         inputs_folder = "../inputs/",
                         fdefl1 = "deflator_GDP_OCDE.xlsx",
                         fdefl2 = "deflator_GDP_WB.xls",
                         download = FALSE,
                         ...
                         ) {
  if (is.null(common_obj)) {
    dot_args <- list(...)
    common_obj <- get_common(dot_args)
  }

  fdefl1 <- paste0(inputs_folder, fdefl1)
  fdefl2 <- paste0(inputs_folder, fdefl2)

  deflator_OCDE <- read_excel(fdefl)

  tmp_vec <- deflator_OCDE[,1] |>
    arrange(Country) |>
    unlist()

  df <- deflator_OCDE |>
    arrange(Country) |>
    left_join(OCDE_country,
              by = "Country") |>
    select(-Country) |>
    select(Code,everything())

  rownames(df) <- tmp_vec

                                        # Reorganizando o deflator

  deflator_OCDE <- df |>
    pivot_longer(
      cols = -c(Code),  # Mantém a coluna "Country" fixa
      names_to = "Year",
      values_to = "Value"
    )

  ## Cria o deflator do GDP unificado - OCDE e WB ##

  deflator <- left_join(OCDE_country,
                        deflator_OCDE,
                        by = "Code")

                                        # seleciona os paises faltantes
  paises_faltantes <- deflator |>
    filter(if_any(where(is.numeric), is.na)) |>
    select(-c(Year,Value))


### Deflator GDP WB ###

  df <- read_excel(fdefl2) |>
    rename(Code = "Country Code") |>
    select(-c("Country Name"))

  deflator_WB <- left_join(paises_faltantes,
                           df,
                           by = "Code") |>
    pivot_longer(
      cols = -c(Code,Country),  # Mantém a coluna "Country" fixa
      names_to = "Year",
      values_to = "Value"
    )

  ## Combina os dois dataframes ##

  deflator_GDP <- rbind(deflator,deflator_WB) |>
    arrange(Code) %>%
    filter(!is.na(Value))

  return(deflator_GDP)

}

get_Erate <- function(
                      common_obj = NULL,
                      inputs_folder = "../inputs/",
                      fexchange1 = "exchange_rate_OCDE.xlsx",
                      fexchange2 = "exchange_rate_WB.xls",
                      download = FALSE,
                      ...
                      ) {

  if (is.null(common_obj)) {
    dot_args <- list(...)
    common_obj <- get_common(dot_args)
  }

  fexchange1 <- paste0(inputs_folder, fexchange1)
  fexchange2 <- paste0(inputs_folder, fexchange2)

  exchange_rate_OCDE <- read_excel(fexchange1)

  tmp_vec <- exchange_rate_OCDE[,1] |>
    arrange(Country) |>
    unlist()

  df <- exchange_rate_OCDE |>
    arrange(Country) |>
    left_join(common_obj[["OCDE_country"]],
              by = "Country") |>
    select(-Country) |>
    select(Code,everything())

  rownames(df) <- tmp_vec

                                        # Reorganizando o deflator

  exchange_rate_OCDE <- df |>
    pivot_longer(
      cols = -c(Code),  # Mantém a coluna "Country" fixa
      names_to = "Year",
      values_to = "Value"
    )

  ## Cria o deflator do GDP unificado - OCDE e WB ##

  exchange_rate <- left_join(common_obj[["OCDE_country"]],
                             exchange_rate_OCDE,
                             by = "Code")

  paises_faltantes <- exchange_rate |>
    filter(if_any(where(is.numeric), is.na)) |>
    select(-c(Year,Value))


### Deflator GDP WB ###

  df <- read_excel(fexchange2) |>
    rename(Code = "Country Code") |>
    select(-c("Country Name"))

  exchange_rate_WB <- left_join(paises_faltantes,
                                df,
                                by = "Code") |>
    pivot_longer(
      cols = -c(Code,Country),
      names_to = "Year",
      values_to = "Value"
    )

                                        # seleciona os paises faltantes
  paises_faltantes <- exchange_rate_WB |>
    filter(if_any(where(is.numeric), is.na)) |>
    select(-c(Year,Value))


## Combina os dois dataframes ##

  exchange_rate_GDP <- rbind(exchange_rate,exchange_rate_WB) |>
    arrange(Code) |>
    filter(!is.na(Value))

  return(exchange_rate_GDP)

}

consolidate_deflator <- function(exchange = NULL, deflator = NULL, ...) {

  dot_args <- list(...)

  if (is.null(exchange)) {
    exchange <- get_Erate(dot_args)
  }

  if (is.null(deflator)) {
    deflator <- get_deflator()
  }

  deflator_geral <- exchange_rate_GDP |>
    rename(exchange_rate = Value) |>
    left_join(deflator_GDP,
              by = c("Code","Country","Year")) |>
    rename(deflator = Value) |>
    mutate(Value = exchange_rate / deflator)

  return(deflator_geral)
}

get_DOM <- function(
                    inputs_folder = "../inputs/",
                    outputs_folder = "../outputs/",
                    inputs_subfolder = "NATIODOMIMP",
                    to_exclude = "TWN",
                    verbose = FALSE,
                    ...
                    ) {

  full_path <- paste0(inputs_folder, inputs_subfolder)
  csv_files <- list.files(
    path = full_path, pattern = "\\.csv$", full.names = TRUE
  )

  csv_files <- csv_files[!grepl(to_exclude, csv_files)]

  OCDE_countries <- unique(substr(basename(csv_files), 1, 3))


  dados_DOM <- lapply(OCDE_countries, function(pais) {

                                        # Filtra arquivos deste país
    arquivos_pais <- csv_files[grep(paste0("^", pais), basename(csv_files))]

                                        # Carrega todos os arquivos do país
    Meepo <- lapply(arquivos_pais, function(arquivo) {

      dados <- read.csv(arquivo)

      return(dados)
    })

    names(Meepo) <- year

    return(Meepo)

  }) |> setNames(OCDE_countries)

  if (verbose) {
    fout <- paste0(outputs_folder, "Un_example.csv")
    readr::write_csv(dados_DOM[["ARG"]][["1995"]], file = fout)
  }

  return(Meepo)

}

get_indicators <- function(res_obj, countries_list = NULL, ...) {
  if (is.null(countries_list)) {
### FIXME: Creates a wrapper that gets from the csv subbolfolder
  }

  NIOTs <- res_obj ## NOTE: To make fewer changes to the original code

  for (pais in OCDE_countries) {
    lista_BL <- list()
    lista_FL <- list()

    for (ano in year) {
                                        # Indicadores sinteticos - BL

      Z = NIOTs[[pais]][["lista_Z"]][[ano]]

      BL <- colSums(Z)

      Lkmedio <- sum(BL) / n_setores^2

      media_BL <- colMeans(Z)
      PD <- media_BL / Lkmedio
      DP <- apply(Z, 2, sd, na.rm = TRUE)
      CV <- DP / media_BL
      Ord_BL <- rank(desc(BL))
      Ord_CV <- rank(desc(CV))

      Meepo <- rbind(BL, media_BL, PD, DP, CV, Ord_BL, Ord_CV)
      Meepo[is.na(Meepo)] <- 0

      lista_BL[[ano]] <- Meepo

                                        # Indicadores sinteticos - FL

      G <- NIOTs[[pais]][["Ghosh"]][[ano]]

      FL <- rowSums(G)

      Lkmedio <- sum(FL) / n_setores^2

      media_FL <- rowMeans(G)
      SD <- media_FL / Lkmedio
      DP <- apply(G, 2, sd, na.rm = TRUE)
      CV <- DP / media_FL
      Ord_FL <- rank(desc(FL))
      Ord_CV <- rank(desc(CV))

      Meepo <- rbind(FL, media_FL, SD, DP, CV, Ord_FL, Ord_CV)
      Meepo[is.na(Meepo)] <- 0

      lista_FL[[ano]] <- Meepo
    }

    names(lista_BL) <- year
    names(lista_FL) <- year

    NIOTs[[pais]][["lista_BL"]] <- lista_BL
    NIOTs[[pais]][["lista_FL"]] <- lista_FL
  }

  return(NIOTs)
}

get_impact <- function(
                       common_obj = NULL,
                       res_obj = NULL,
                       n_sectors = 7,
                       countries_list = NULL,
                       ...
                       ) {

  dot_args <- list(...)

  NIOTs <- res_obj

  if (is.null(countries_list)) {
### FIXME: Creates a wrapper that gets from the csv subbolfolder
  }

  Ident <- diag(n_sectors)

  for (pais in countries_list) {

    lista_Z <- list()
    Ghosh <- list()

    for (ano in year) {

                                        # Encadeamentos para tras - Leontief #

      An <- NIOTs[[ pais ]][["lista_An"]][[ ano ]]

      Z <- ginv(Ident - An)
      Z[is.na(Z)] <- 0

      colnames(Z) <- common_obj[["titulo_agregador"]]
      rownames(Z) <- common_obj[["titulo_agregador"]]

      lista_Z[[ ano ]] <- Z

                                        # Encadeamentos para frente - Ghosh #

      vec_VBP <- rowSums(NIOTs[[pais]][["CIn"]][[ano]]) +
        rowSums(NIOTs[[pais]][["Fn"]]][[ano]]) # x_i oferta da producao setorial

    inversa <- ginv( diag( vec_VBP ) )

    G <- inversa %*% Z %*% diag( vec_VBP )

    colnames(G) <- common_obj[["titulo_agregador"]]
    rownames(G) <- common_obj[["titulo_agregador"]]

    Ghosh[[ ano ]] <- G

  }

  names(lista_Z) <- year
  names(Ghosh) <- year

  NIOTs[[pais]][["lista_Z"]] <- lista_Z
  NIOTs[[pais]][["Ghosh"]] <- Ghosh

  return(NIOTs)

}

compile_base <- function(
                         inputs_folder = "../inputs/",
                         ftitle = "titulos.xlsx",
                         fagg = "agregador.xslx",
                         download = FALSE,
                         inputs_folder = "../inputs/",
                         outputs_folder = "../inputs/",
                         fdefl1 = "deflator_GDP_OCDE.xlsx",
                         fdefl2 = "deflator_GDP_WB.xls",
                         fexchange1 = "exchange_rate_OCDE.xlsx",
                         fexchange2 = "exchange_rate_WB.xls",
                         inputs_subfolder = "NATIODOMIMP",
                         to_exclude = "TWN",
                         verbose = FALSE,
                         ...
                         ) {

  dot_args <- list(...)

  common_obj <- get_common(
    inputs_folder = inputs_folder,
    ftitle = ftitle,
    fagg = fagg,
    download = download,
    dot_args
  )

  deflator_GDP <- get_deflator(
    common_obj = common_obj,
    inputs_folder = inputs_folder,
    fdefl1 = fdefl1,
    fdefl2 = fdefl2,
    download = download,
    dot_args
  )



  exchange_rate_GDP <- get_Erate(
    common_obj = common_obj,
    inputs_folder = inputs_folder,
    fexchange1 = fexchange1,
    fexchange2 = fexchange2,
    download = download,
    dot_args
  )
  deflator_geral <- consolidate_deflator(
    exchange = exchange_rate_GDP,
    deflator = deflator_GDP,
    dot_args
  )

  Meepo <- get_DOM(
    inputs_folder = inputs_folder,
    outputs_folder = outputs_folder,
    inputs_subfolder = inputs_subfolder,
    to_exclude = to_exclude,
    verbose = verbose,
    dot_args
  )

  NIOTs <- get_impact(
    common_obj = common_obj,
    res_obj = NIOTs,
    countries_list = NULL,
    dot_args
  )

  NIOTs <- get_indicators(
    res_obj = NIOTs,
    countries_list = NULL,
    dot_args
  )

  return(NIOTs)
}
