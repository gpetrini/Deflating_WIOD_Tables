### Compilação das NIOT ### 

# Pacotes 

library(MASS)
library(tidyverse)
library(openxlsx)
library(purrr)

# Dados basicos #  

load("NIOTs_resultados.rdata") # carregando os dados 

variaveis = names(NIOTs) # vetor com o nome das variaveis da base
year = paste(seq(1995,2020)) # intervalo contemplado pelas matrizes

# source("../Modelo/base/preparar_base.R") # prepara a base de dados

dados_escalares = list()

OCDE_country = names(NIOTs)

for (country in OCDE_country) {
  
  dados = NIOTs[[country]] # dados do pais
  
  Meepo = list() # salva dados do pais
  
  ## Gerando a base de dados ##
  
  # Componentes da demanda total - Ft #
  
  Ft = lapply(year, function(ano) {
    
    dados$Ft[[ano]] %>% 
      rename(C = HFCE,
             G = GGFC,
             I = GFCF,
             X = EXPO,
             E = INVNT) %>% 
      select(C,I,G,X,E) %>% 
      colSums() 
    
  }) %>% 
    bind_rows() # Combina todos os resultados em um único dataframe
  
  rownames(Ft) = year
  
  # Componentes da demanda nacional - Fn #
  
  Fn = lapply(year, function(ano) {
    
    dados$Fn[[ano]] %>% 
      rename(C = HFCE,
             G = GGFC,
             I = GFCF,
             X = EXPO,
             E = INVNT) %>% 
      select(C,I,G,X,E) %>% 
      colSums() 

  }) %>% 
    bind_rows() # Combina todos os resultados em um único dataframe
  
  rownames(Fn) = year
  
  # Demanda final importada por componentes da demanda agregada - M_F #
  
  M_F = lapply(year, function(ano) {
    
    dados$Fm[[ano]] %>% 
      rename(M_C = HFCE,
             M_G = GGFC,
             M_I = GFCF,
             M_X = EXPO,
             M_E = INVNT) %>% 
      select(M_C,M_I,M_G,M_X,M_E) %>% 
      colSums() 
    
  }) %>% 
    bind_rows() # Combina todos os resultados em um único dataframe
  
  rownames(M_F) = year
  
  # Consumo intermediario importado relacionado aos componentes da demanda final - M_I #
  
  M_I = lapply(year, function(ano){
    
    Am = dados$lista_Am[[ano]] # coeficientes tecnicos importados
    Z = dados$lista_Z[[ano]] # matriz inversa de Leontief
    
    gastos = dados$Fn[[ano]] %>% 
      rename(C = HFCE,
             G = GGFC,
             I = GFCF,
             X = EXPO,
             E = INVNT) %>% 
      select(C,I,G,X,E) %>% 
      as.matrix()
    
    df = Am %*% Z %*% gastos %>% 
      colSums() %>% 
      t() %>% 
      as.data.frame()
    
    colnames(df) = c("M_C",
                     "M_G",
                     "M_I",
                     "M_X",
                     "M_E")
    
    return(df)
    
  }) %>% 
    bind_rows() # Combina todos os resultados em um único dataframe
  
  rownames(M_I) = year
  
  # Conteudo importado total por componente da demanda agregada - M #
  
  M = M_F + M_I
  
  # Parcela de conteudo importado por componente da demanda agregada - m #
  
  m = M / Ft
  
  # Salvando dados
  
  Meepo$Ft = Ft
  Meepo$Fn = Fn
  Meepo$M = M
  Meepo$M_F = M_F
  Meepo$M_I = M_I
  Meepo$m = m
  
  dados_escalares[[country]] = Meepo
  
  print(country)
  
}

save(dados_escalares, file = "../Modelo/dados_escalares.rdata")
