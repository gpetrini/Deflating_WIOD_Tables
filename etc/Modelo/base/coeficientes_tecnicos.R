# Coeficientes tecnicos #

lista_An = list()
lista_Am = list()
lista_A = list()

for (pais in OCDE_countries) {
  
  for (ano in year) {
    
    # Seleciona a producao bruta setorial e calcula a inversa #
    
    vec_VBP = NIOTs[[ pais ]]$Recurso[[ ano ]]["GO",] %>% unlist()
    
    inversa = ginv( diag( vec_VBP ) )
    
    # Coeficientes tecnicos nacionais - An #
    
    Meepo = NIOTs[[ pais ]]$CIn[[ ano ]] %>% as.matrix()
    
    An = Meepo %*% inversa
    
    An[is.na(An)] 
    
    colnames(An) = titulo_agregador
    
    lista_An[[ ano ]] = An
    
    # Coeficientes tecnicos importados - Am #
    
    Meepo = NIOTs[[ pais ]]$CIm[[ ano ]] %>% as.matrix()
    
    Am = Meepo %*% inversa
    
    Am[is.na(Am)] = 0
    
    colnames(Am) = titulo_agregador
    
    lista_Am[[ ano ]] = Am
    
    # Coeficientes tecnicos totais - A #
    
    Meepo = NIOTs[[ pais ]]$CI[[ ano ]] %>% as.matrix()
    
    A = Meepo %*% inversa
    
    A[is.na(A)] = 0
    
    colnames(A) = titulo_agregador
    
    lista_A[[ ano ]] = A
    
  }
  
  names(lista_An) = year
  names(lista_Am) = year
  names(lista_A) = year
  
  NIOTs[[ pais ]]$lista_An = lista_An
  NIOTs[[ pais ]]$lista_Am = lista_Am
  NIOTs[[ pais ]]$lista_A = lista_A
  
}



