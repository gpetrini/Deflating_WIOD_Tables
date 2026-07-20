## Teste Brasil ##

# Pacotes 

library(MASS)
library(tidyverse)
library(openxlsx)
library(readxl)

# Dados basicos #  

year = seq(1995,2020) # intervalo contemplado pelas matrizes
n_setores = 7 # numero de setores

# Compilando dados

source("../Modelo/base/titulos.R") # titulos usados na construcao da base de dados
source("../Modelo/base/deflator.R") # deflator do GDP a precos do consumidor 
source("../Modelo/base/exchange_rate.R") # taxa de cambio

source("../Modelo/base/Un.R") # Uso nacional
source("../Modelo/base/Um.R") # Uso importado
source("../Modelo/base/Ut.R") # Uso total
####################################################################################################

teste = exchange_rate_GDP %>% 
  rename(exchange_rate = Value) %>% 
  left_join(deflator_GDP,
            by = c("Code","Country","Year")) %>% 
  rename(deflator = Value) %>% 
  filter(Code == "BRA") %>% 
  mutate(Value = exchange_rate / deflator)

# Definir o diretório onde estão os arquivos CSV
caminho_da_pasta <- "../Modelo/NATIODOMIMP"  # substitua pelo caminho real

# Listar todos os arquivos CSV na pasta
arquivos_csv <- list.files(path = caminho_da_pasta, 
                           pattern = "\\.csv$", 
                           full.names = TRUE)

# Filtrar a lista paths que contenham "BRA" - Brasil #
arquivos_csv <- arquivos_csv[grepl("BRA", arquivos_csv)]

# Ler todos os arquivos e armazenar em uma lista
lista_de_dados <- lapply(arquivos_csv, read.csv)  # ou read.csv2 para vírgula como separador decimal

# Nomeia os elementos da lista com os nomes dos arquivos
nomes <- gsub("\\.csv$|dom", "", basename(arquivos_csv))
names(lista_de_dados) <- nomes

# Separar consumo intermediario e demanda final nacional 

CIn = list()
Fn = list()

for (i in 1:length(lista_de_dados)) {
  
  # Seleciona o deflator e a taxa de cambio
  deflator = teste$Value[i] # seleciona a taxa de cambio
  
  # CIn
  Meepo <- lista_de_dados[[i]][1:45, 2:46]  # Extrai consumo intermediário
  rownames(Meepo) <- titulo    # Nomeia as linhas
  
  Meepo = Meepo %>% as.matrix()
  
  # agrega os setores 
  
  Meepo = t(agregador) %*% Meepo %*% agregador
  
  # Aplica o deflator e taxa de cambio
  Jull = Meepo * deflator
  
  # Salva o CI
  CIn[[i]] = Jull
  
  # Fn
  Meepo = lista_de_dados[[i]][1:45,47:56]
  rownames(Meepo) = titulo
  
  Meepo = Meepo %>% as.matrix()
  
  # agrega os setores 
  
  Meepo = t(agregador) %*% Meepo 
  
  Meepo = Meepo %>% as.data.frame()
  
  # Ajustando componentes da demanda final
  Pudge = Meepo %>% 
    mutate(HFCE = HFCE + NPISH, # soma consumo das familias e empresas sem fins lucrativos 
           EXPO = EXPO + CONS_NONRES) %>% # soma exportacao e compras diretas de nao residentes (exportacao)
    select(-c(NPISH,CONS_NONRES,DPABR,IMPO,TOTAL))
  
  # Aplica o deflator e taxa de cambio
  Jull = Pudge * deflator
  
  # Salva o CI
  Fn[[i]] = Jull
  
}

names(CIn) = nomes
names(Fn) = nomes
