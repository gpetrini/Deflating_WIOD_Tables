# Deflator e moedas nacionais #

## Deflator do GDP ##

### Deflator do GDP a precos do consumidor - OCDE ###

deflator_OCDE <- read_excel("deflator_GDP_OCDE.xlsx")

vec = deflator_OCDE[,1] %>% 
  arrange(Country) %>% 
  unlist()

df = deflator_OCDE %>% 
  arrange(Country) %>% 
  left_join(OCDE_country,
            by = "Country") %>% 
  select(-Country) %>% 
  select(Code,everything())
  
rownames(df) = vec

# Reorganizando o deflator

deflator_OCDE <- df %>%
  pivot_longer(
    cols = -c(Code),  # Mantém a coluna "Country" fixa
    names_to = "Year",
    values_to = "Value"
  ) 

# teste para ver quantidade de paises
teste = deflator_OCDE %>% 
  group_by(Code) %>% 
  count()

## Cria o deflator do GDP unificado - OCDE e WB ##

deflator = left_join(OCDE_country,
                     deflator_OCDE,
                     by = "Code")

# seleciona os paises faltantes 
paises_faltantes <- deflator %>%
  filter(if_any(where(is.numeric), is.na)) %>% 
  select(-c(Year,Value))


### Deflator GDP WB ###

df <- read_excel("deflator_GDP_WB.xls") %>% 
  rename(Code = "Country Code") %>% 
  select(-c("Country Name"))

deflator_WB = left_join(paises_faltantes,
                        df,
                        by = "Code") %>% 
  pivot_longer(
    cols = -c(Code,Country),  # Mantém a coluna "Country" fixa
    names_to = "Year",
    values_to = "Value"
  )

## Combina os dois dataframes ##

deflator_GDP = rbind(deflator,deflator_WB) %>% 
  arrange(Code) %>% 
  filter(!is.na(Value))

teste = deflator_GDP %>% 
  group_by(Code) %>% 
  count() # falta apenas TWN

               