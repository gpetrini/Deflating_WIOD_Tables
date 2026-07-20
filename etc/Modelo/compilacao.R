## Compilação das NIOT ### 

# Pacotes 

library(MASS)
library(tidyverse)
library(openxlsx)
library(readxl)
library(purrr)

# Dados basicos #  

year = paste0(seq(1995,2020)) # intervalo contemplado pelas matrizes
n_setores = 7 # numero de setores

# Compilando dados

source("./base/titulos.R") # titulos usados na construcao da base de dados
source("./base/deflator.R") # deflator do GDP a precos do consumidor
source("./base/exchange_rate.R") # taxa de cambio
source("./base/base.R") # prepara base de dados
source("./base/matrizes.R") # gera matrizes deflacionadas
source("./base/coeficientes_tecnicos.R") # estima os coeficientes tecnicos (A, An e Am)
source("./base/matriz_impacto.R") # estima a matriz de impacto - Leontief e Ghosh
source("./base/indicadores_sinteticos.R") # estima os indicadores sinteticos

## Salvando os dados ##

## save(NIOTs, file = "../Modelo/NIOTs_resultados.rdata")
save(NIOTs, file = "./Modelo/Test.rdata")
