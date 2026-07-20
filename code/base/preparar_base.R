# Extrair as três primeiras letras
OCDE_countries <- substr(names(NIOTs$CI), 1, 3) %>% unique()

# Criar lista agregada por país
NIOTs <- lapply(OCDE_countries, function(pais) {
  
  # Filtra elementos que começam com o código do país
  lapply(NIOTs, function(variavel) {
    
    Meepo = variavel[grep(paste0("^", pais), names(variavel))]
    
    names(Meepo) = year
    
    return(Meepo)
  })
  
  
})

names(NIOTs) = OCDE_countries

