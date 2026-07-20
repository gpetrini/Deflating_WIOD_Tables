# Titulos #

## Nome dos paises ##
OCDE_country <- read_excel("titulos.xlsx", sheet = "country")

# Limpa os dados - tira nome em frances
OCDE_country$Country <- sub(" / .*", "", OCDE_country$Country)

## Titulo dos setores ##
titulo <- read_excel("titulos.xlsx", sheet = "sector") %>% 
  select(Code) %>% 
  unlist()

## Titulo da demanda final ##
titulo_fd <- read_excel("titulos.xlsx", sheet = "fd") %>% 
  select(Code) %>% 
  unlist()

## Titulo da demanda final ##
titulo_vbp <- read_excel("titulos.xlsx", sheet = "vbp") %>% 
  select(Code) %>% 
  unlist()

# Agregador de setores

agregador <- read_excel("agregador.xlsx") %>% 
  select(-Code) %>% 
  as.matrix()

rownames(agregador) = titulo

# titulo curto

titulo_agregador = colnames(agregador)
