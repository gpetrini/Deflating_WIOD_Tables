## Geracao das Matrizes Nacionais deflacionadas a preços locais e constantes de 1995

NIOTs = list()

for (pais in OCDE_countries) {
  
  CIn = list()
  Fn = list()
  CIm = list()
  Fm = list()
  CI = list()
  Ft = list()
  Recurso = list()
  
  for (ano in year) {
    
    # Seleciona o deflator utilizado para o ano e pais correspondente
    
    vec = deflator_geral %>% 
      filter(Code == pais & Year == ano)
    
    print(vec)
    
    deflator = vec$Value
    
    # Un #
    
    source("../Modelo/base/Un.R") # Uso nacional
    source("../Modelo/base/Um.R") # Uso importado
    source("../Modelo/base/Ut.R") # Uso total
    source("../Modelo/base/Recurso.R") # Dados do lado da oferta das NIOT
    
  }
  
  names(CIn) = year
  names(Fn) = year
  names(CIm) = year
  names(Fm) = year
  names(CI) = year
  names(Ft) = year
  names(Recurso) = year
  
  NIOTs[[ pais ]]$CIn = CIn
  NIOTs[[ pais ]]$Fn = Fn
  
  NIOTs[[ pais ]]$CIm = CIm
  NIOTs[[ pais ]]$Fm = Fm
  
  NIOTs[[ pais ]]$CI = CI
  NIOTs[[ pais ]]$Ft = Ft
  
  NIOTs[[ pais ]]$Recurso = Recurso
  
}



