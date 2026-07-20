# Prepara a base de dados a ser utilizada #

# Consolidacao do deflator #

deflator_geral = exchange_rate_GDP %>% 
  rename(exchange_rate = Value) %>% 
  left_join(deflator_GDP,
            by = c("Code","Country","Year")) %>% 
  rename(deflator = Value) %>% 
  mutate(Value = exchange_rate / deflator)

# Seleciona caminho para extracao das matrizes e combina com os deflatores #

## Matrizes DOM - Un e Um ## 

# Definir o diretório onde estão os arquivos CSV
caminho_da_pasta <- "../Modelo/NATIODOMIMP"  # substitua pelo caminho real

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
  Meepo = lapply(arquivos_pais, function(arquivo) {
    
    dados <- read.csv(arquivo)
    
    return(dados)
  })
  
  names(Meepo) = year
  
  return(Meepo)
  
}) %>% setNames(OCDE_countries)

# Salva um exemplos
write.xlsx(dados_DOM$ARG$"1995",file = "Un_exemplo.xlsx", firstRow = T, firstCol = T)

