## Taxa de cambio de moeda nacional por unidade de dolar ##

exchange_rate_OCDE <- read_excel("exchange_rate_OCDE.xlsx")

vec = exchange_rate_OCDE[,1] %>% 
  arrange(Country) %>% 
  unlist()

df = exchange_rate_OCDE %>% 
  arrange(Country) %>% 
  left_join(OCDE_country,
            by = "Country") %>% 
  select(-Country) %>% 
  select(Code,everything())

rownames(df) = vec

# Reorganizando o deflator

exchange_rate_OCDE <- df %>%
  pivot_longer(
    cols = -c(Code),  # Mantém a coluna "Country" fixa
    names_to = "Year",
    values_to = "Value"
  ) 

# teste para ver quantidade de paises
teste = exchange_rate_OCDE %>% 
  group_by(Code) %>% 
  count()

## Cria o deflator do GDP unificado - OCDE e WB ##

exchange_rate = left_join(OCDE_country,
                          exchange_rate_OCDE,
                          by = "Code")

# seleciona os paises faltantes 
paises_faltantes <- exchange_rate %>%
  filter(if_any(where(is.numeric), is.na)) %>% 
  select(-c(Year,Value))


### Deflator GDP WB ###

df <- read_excel("exchange_rate_WB.xls") %>% 
  rename(Code = "Country Code") %>% 
  select(-c("Country Name"))

exchange_rate_WB = left_join(paises_faltantes,
                             df,
                             by = "Code") %>% 
  pivot_longer(
    cols = -c(Code,Country),  # Mantém a coluna "Country" fixa
    names_to = "Year",
    values_to = "Value"
  )

teste = exchange_rate_WB %>% 
  group_by(Code) %>% 
  count() # falta apenas TWN

# seleciona os paises faltantes 
paises_faltantes <- exchange_rate_WB %>%
  filter(if_any(where(is.numeric), is.na)) %>% 
  select(-c(Year,Value))


## Combina os dois dataframes ##

exchange_rate_GDP = rbind(exchange_rate,exchange_rate_WB) %>% 
  arrange(Code) %>% 
  filter(!is.na(Value))

teste = exchange_rate_GDP %>% 
  group_by(Code) %>% 
  count() # falta apenas TWN
