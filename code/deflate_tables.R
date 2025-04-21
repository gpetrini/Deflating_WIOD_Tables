library(MASS)
library(tidyverse)
library(openxlsx)
library(readxl)
library(purrr)


year <- paste0(seq(1995,2020)) # intervalo contemplado pelas matrizes
n_setores <- 7 # numero de setores

                                        # Titulos


## Nome dos paises ##
OCDE_country <- read_excel("../inputs/titulos.xlsx", sheet = "country")

                                        # Limpa os dados - tira nome em frances
OCDE_country$Country <- sub(" / .*", "", OCDE_country$Country)

## Titulo dos setores ##
titulo <- read_excel("../inputs/titulos.xlsx", sheet = "sector") %>%
  select(Code) |>
  unlist()

## Titulo da demanda final ##
titulo_fd <- read_excel("../inputs/titulos.xlsx", sheet = "fd") %>%
  select(Code) |>
  unlist()

## Titulo da demanda final ##
titulo_vbp <- read_excel("../inputs/titulos.xlsx", sheet = "vbp") %>%
  select(Code) |>
  unlist()

                                        # Agregador de setores

agregador <- read_excel("../inputs/agregador.xlsx") %>%
  select(-Code) |>
  as.matrix()

rownames(agregador) <- titulo

                                        # titulo curto

titulo_agregador <- colnames(agregador)


                                        # Deflator


                                        # Deflator e moedas nacionais #

## Deflator do GDP ##

### Deflator do GDP a precos do consumidor - OCDE ###

deflator_OCDE <- read_excel("../inputs/deflator_GDP_OCDE.xlsx")

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

                                        # teste para ver quantidade de paises
teste <- deflator_OCDE |>
  group_by(Code) |>
  count()

## Cria o deflator do GDP unificado - OCDE e WB ##

deflator <- left_join(OCDE_country,
                      deflator_OCDE,
                      by = "Code")

                                        # seleciona os paises faltantes
paises_faltantes <- deflator |>
  filter(if_any(where(is.numeric), is.na)) |>
  select(-c(Year,Value))


### Deflator GDP WB ###

df <- read_excel("../inputs/deflator_GDP_WB.xls") |>
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

teste <- deflator_GDP |>
  group_by(Code) |>
  count() # falta apenas TWN



## Taxa de cambio de moeda nacional por unidade de dolar ##

exchange_rate_OCDE <- read_excel("../inputs/exchange_rate_OCDE.xlsx")

tmp_vec <- exchange_rate_OCDE[,1] |>
  arrange(Country) |>
  unlist()

df <- exchange_rate_OCDE |>
  arrange(Country) |>
  left_join(OCDE_country,
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

                                        # teste para ver quantidade de paises
teste <- exchange_rate_OCDE |>
  group_by(Code) |>
  count()

## Cria o deflator do GDP unificado - OCDE e WB ##

exchange_rate <- left_join(OCDE_country,
                           exchange_rate_OCDE,
                           by = "Code")

                                        # seleciona os paises faltantes
paises_faltantes <- exchange_rate |>
  filter(if_any(where(is.numeric), is.na)) |>
  select(-c(Year,Value))


### Deflator GDP WB ###

df <- read_excel("../inputs/exchange_rate_WB.xls") |>
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

teste <- exchange_rate_WB |>
  group_by(Code) |>
  count()

                                        # seleciona os paises faltantes
paises_faltantes <- exchange_rate_WB |>
  filter(if_any(where(is.numeric), is.na)) |>
  select(-c(Year,Value))


## Combina os dois dataframes ##

exchange_rate_GDP <- rbind(exchange_rate,exchange_rate_WB) |>
  arrange(Code) |>
  filter(!is.na(Value))

teste <- exchange_rate_GDP |>
  group_by(Code) |>
  count()


                                        # Prepara a base de dados a ser utilizada #

                                        # Consolidacao do deflator #

deflator_geral <- exchange_rate_GDP |>
  rename(exchange_rate = Value) |>
  left_join(deflator_GDP,
            by = c("Code","Country","Year")) |>
  rename(deflator = Value) |>
  mutate(Value = exchange_rate / deflator)

                                        # Seleciona caminho para extracao das matrizes e combina com os deflatores #

## Matrizes DOM - Un e Um ##

                                        # Definir o diretório onde estão os arquivos CSV
caminho_da_pasta <- "../inputs/NATIODOMIMP"  # substitua pelo caminho real

                                        # Listar todos os arquivos CSV na pasta
arquivos_csv <- list.files(path = caminho_da_pasta,
                           pattern = "\\.csv$",
                           full.names = TRUE)

                                        # Filtrar a lista, excluindo paths que contenham "TWN" - Taiwan #
arquivos_csv <- arquivos_csv[!grepl("TWN", arquivos_csv)]

                                        # Extrair códigos de países únicos (3 primeiras letras dos nomes dos arquivos)
OCDE_countries <- unique(substr(basename(arquivos_csv), 1, 3))

## Carregar dados das matrizes DOM - Un e Um ##
dados_DOM <- lapply(OCDE_countries, function(pais) {

                                        # Filtra arquivos deste país
  arquivos_pais <- arquivos_csv[grep(paste0("^", pais), basename(arquivos_csv))]

                                        # Carrega todos os arquivos do país
  Meepo <- lapply(arquivos_pais, function(arquivo) {

    dados <- read.csv(arquivo)

    return(dados)
  })

  names(Meepo) <- year

  return(Meepo)

}) |> setNames(OCDE_countries)

                                        # Salva um exemplos
readr::write_csv(dados_DOM[["ARG"]][["1995"]], file = "../outputs/Un_exemplo.csv")




## Geracao das Matrizes Nacionais deflacionadas a preços locais e constantes de 1995

NIOTs <- list()

for (pais in OCDE_countries) {

  CIn <- list()
  Fn <- list()
  CIm <- list()
  Fm <- list()
  CI <- list()
  Ft <- list()
  Recurso <- list()

  for (ano in year) {

                                        # Seleciona o deflator utilizado para o ano e pais correspondente

    tmp_vec <- deflator_geral |>
      filter(Code == pais && Year == ano)

    print(tmp_vec)

    deflator <- vec$Value

                                        # Un #
    
    ## FIXME
    ## source("../Modelo/base/Un.R") # Uso nacional
    ## source("../Modelo/base/Um.R") # Uso importado
    ## source("../Modelo/base/Ut.R") # Uso total


                                        # Ut

                                        # CI
    Meepo <- CIn[[ ano ]] + CIm [[ ano ]]  # Extrai consumo intermediário

                                        # Aplica o deflator e taxa de cambio
    Jull <- Meepo # * deflator

                                        # Salva o CI
    CI[[ ano ]] <- Jull

                                        # Ft
    Meepo <- Fn[[ ano ]] + Fm[[ ano ]]  # Extrai demanda final

                                        # Aplica o deflator e taxa de cambio
    Jull <- Meepo # * deflator

                                        # Salva o CI
    Ft[[ ano ]] <- Jull

    ## source("../Modelo/base/Recurso.R") # Dados do lado da oferta das NIOT

                                        # Recurso

                                        # Producao
    Meepo <- dados_DOM[[ pais ]][[ ano]][c(91,92,94,95), 2:46]  # Extrai consumo intermediário

    Meepo <- Meepo |> as.matrix()

                                        # agrega os setores

    Meepo = Meepo %*% agregador

    Meepo = Meepo |>
      as.data.frame()

                                        # Soma os impostos
    Pudge <- rbind(
      Meepo[1,] + Meepo[2,],
      Meepo[3:4,]
    )
    rownames(Pudge) <- c("II","VA","GO")

                                        # Aplica o deflator e taxa de cambio
    Jull <- Pudge * deflator

                                        # Salva o Recursos
    Recurso[[ ano ]] <- Jull

  }

  names(CIn) <- year
  names(Fn) <- year
  names(CIm) <- year
  names(Fm) <- year
  names(CI) <- year
  names(Ft) <- year
  names(Recurso) <- year

  NIOTs[[pais]][["CIn"]] <- CIn
  NIOTs[[pais]][["Fn"]] <- Fn

  NIOTs[[pais]][["CIm"]] <- CIm
  NIOTs[[pais]][["Fm"]] <- Fm

  NIOTs[[pais]][["CI"]] <- CI
  NIOTs[[pais]][["Ft"]] <- Ft

  NIOTs[[pais]][["Recurso"]] <- Recurso

}




                                        # Coeficientes tecnicos #

lista_An <- list()
lista_Am <- list()
lista_A <- list()

for (pais in OCDE_countries) {

  for (ano in year) {

                                        # Seleciona a producao bruta setorial e calcula a inversa #

    vec_VBP <- NIOTs[[pais]][["Recurso"]][[ano]]["GO", ] |>
      unlist()

    inversa <- ginv( diag( vec_VBP ) )

                                        # Coeficientes tecnicos nacionais - An #

    Meepo <- NIOTs[[pais]][["CIn"]][[ano]] |>
      as.matrix()

    An <- Meepo %*% inversa

    An[is.na(An)]

    colnames(An) <- titulo_agregador

    lista_An[[ ano ]] <- An

                                        # Coeficientes tecnicos importados - Am #

    Meepo <- NIOTs[[ pais ]][["CIm"]][[ ano ]] |>
      as.matrix()

    Am <- Meepo %*% inversa

    Am[is.na(Am)] <- 0

    colnames(Am) <- titulo_agregador

    lista_Am[[ ano ]] <- Am

                                        # Coeficientes tecnicos totais - A #

    Meepo <- NIOTs[[pais]][["CI"]][[ano]] |>
      as.matrix()

    A <- Meepo %*% inversa

    A[is.na(A)] <- 0

    colnames(A) <- titulo_agregador

    lista_A[[ ano ]] <- A

  }

  names(lista_An) <- year
  names(lista_Am) <- year
  names(lista_A) <- year

  NIOTs[[pais]][["lista_An"]] <- lista_An
  NIOTs[[pais]][["lista_Am"]] <- lista_Am
  NIOTs[[pais]][["lista_A"]] <- lista_A

}




                                        # Matriz de impacto

Ident <- diag(n_setores) # matriz diagonal

for (pais in OCDE_countries) {

  lista_Z <- list()
  Ghosh <- list()

  for (ano in year) {

                                        # Encadeamentos para tras - Leontief #

    An <- NIOTs[[ pais ]][["lista_An"]][[ ano ]]

    Z <- ginv(Ident - An)
    Z[is.na(Z)] <- 0

    colnames(Z) <- titulo_agregador
    rownames(Z) <- titulo_agregador

    lista_Z[[ ano ]] <- Z

                                        # Encadeamentos para frente - Ghosh #

    vec_VBP <- rowSums(NIOTs[[pais]][["CIn"]][[ano]]) +
      rowSums(NIOTs[[pais]][["Fn"]]][[ano]]) # x_i oferta da producao setorial

  inversa <- ginv( diag( vec_VBP ) )

  G <- inversa %*% Z %*% diag( vec_VBP )

  colnames(G) <- titulo_agregador
  rownames(G) <- titulo_agregador

  Ghosh[[ ano ]] <- G

}

names(lista_Z) <- year
names(Ghosh) <- year

NIOTs[[pais]][["lista_Z"]] <- lista_Z
NIOTs[[pais]][["Ghosh"]] <- Ghosh

}





# Indicadores sinteticos - BL

for (pais in OCDE_countries) {

  lista_BL <- list()
  lista_FL <- list()

  for (ano in year) {

    # Indicadores sinteticos - BL

    Z = NIOTs[[pais]][["lista_Z"]][[ano]]

    BL <- colSums(Z)

    Lkmedio <- sum(BL) / n_setores ^ 2

    media_BL <- colMeans(Z)
    PD <- media_BL / Lkmedio
    DP <- apply(Z, 2, sd, na.rm = TRUE)
    CV <- DP / media_BL
    Ord_BL <- rank(desc(BL))
    Ord_CV <- rank(desc(CV))

    Meepo <- rbind(BL,media_BL,PD,DP,CV,Ord_BL,Ord_CV)
    Meepo[is.na(Meepo)] <- 0

    lista_BL[[ ano ]] <- Meepo

    # Indicadores sinteticos - FL

    G <- NIOTs[[pais]][["Ghosh"]][[ano]]

    FL <- rowSums(G)

    Lkmedio <- sum(FL) / n_setores ^ 2

    media_FL <- rowMeans(G)
    SD <- media_FL / Lkmedio
    DP <- apply(G, 2, sd, na.rm = TRUE)
    CV <- DP / media_FL
    Ord_FL <- rank(desc(FL))
    Ord_CV <- rank(desc(CV))

    Meepo <- rbind(FL,media_FL,SD,DP,CV,Ord_FL,Ord_CV)
    Meepo[is.na(Meepo)] <- 0

    lista_FL[[ ano ]] <- Meepo

  }

  names(lista_BL) <- year
  names(lista_FL) <- year

  NIOTs[[ pais ]][["lista_BL"] <- lista_BL
  NIOTs[[ pais ]][["lista_FL"] <- lista_FL
}
