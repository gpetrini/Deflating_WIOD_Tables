# Matriz de impacto

Ident = diag(n_setores) # matriz diagonal

for (pais in OCDE_countries) {
  
  lista_Z = list()
  Ghosh = list()
  
  for (ano in year) {
  
    # Encadeamentos para tras - Leontief #
    
    An = NIOTs[[ pais ]]$lista_An[[ ano ]]
    
    Z = ginv(Ident - An)
    Z[is.na(Z)] = 0
    
    colnames(Z) = titulo_agregador
    rownames(Z) = titulo_agregador
    
    lista_Z[[ ano ]] = Z
    
    # Encadeamentos para frente - Ghosh #
    
    vec_VBP = rowSums( NIOTs[[ pais ]]$CIn[[ ano ]] ) + rowSums( NIOTs[[ pais ]]$Fn[[ ano ]] ) # x_i oferta da producao setorial
    
    inversa = ginv( diag( vec_VBP ) )
    
    G = inversa %*% Z %*% diag( vec_VBP )
    
    colnames(G) = titulo_agregador
    rownames(G) = titulo_agregador
    
    Ghosh[[ ano ]] = G
  
  }
  
  names(lista_Z) = year
  names(Ghosh) = year
  
  NIOTs[[ pais ]]$lista_Z = lista_Z
  NIOTs[[ pais ]]$Ghosh = Ghosh
  
}




