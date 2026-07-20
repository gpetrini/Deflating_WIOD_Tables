# Indicadores sinteticos - BL

for (pais in OCDE_countries) {
  
  lista_BL = list()
  lista_FL = list()
  
  for (ano in year) {
  
    # Indicadores sinteticos - BL
    
    Z = NIOTs[[ pais ]]$lista_Z[[ ano ]]
    
    BL = colSums(Z)
    
    Lkmedio <- sum(BL) / n_setores ^ 2
    
    media_BL = colMeans(Z)
    PD = media_BL / Lkmedio
    DP = apply(Z, 2, sd, na.rm = TRUE)
    CV = DP / media_BL
    Ord_BL <- rank(desc(BL))
    Ord_CV <- rank(desc(CV))
    
    Meepo = rbind(BL,media_BL,PD,DP,CV,Ord_BL,Ord_CV)
    Meepo[is.na(Meepo)] = 0
    
    lista_BL[[ ano ]] = Meepo
    
    # Indicadores sinteticos - FL
    
    G = NIOTs[[ pais ]]$Ghosh[[ ano ]]
    
    FL = rowSums(G)
    
    Lkmedio <- sum(FL) / n_setores ^ 2
    
    media_FL = rowMeans(G)
    SD = media_FL / Lkmedio
    DP = apply(G, 2, sd, na.rm = TRUE)
    CV = DP / media_FL
    Ord_FL <- rank(desc(FL))
    Ord_CV <- rank(desc(CV))
    
    Meepo = rbind(FL,media_FL,SD,DP,CV,Ord_FL,Ord_CV)
    Meepo[is.na(Meepo)] = 0
    
    lista_FL[[ ano ]] = Meepo
  
  }
  
  names(lista_BL) = year
  names(lista_FL) = year
  
  NIOTs[[ pais ]]$lista_BL = lista_BL
  NIOTs[[ pais ]]$lista_FL = lista_FL
}



