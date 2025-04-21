library(MASS)
library(tidyverse)
library(openxlsx)
library(readxl)
library(purrr)


year <- paste0(seq(1995,2020)) # intervalo contemplado pelas matrizes
n_setores <- 7 # numero de setores

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

