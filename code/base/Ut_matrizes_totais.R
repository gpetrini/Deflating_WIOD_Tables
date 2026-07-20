# Ut

# Definir o diretório onde estão os arquivos CSV
caminho_da_pasta <- "../Modelo/NATIOTTL"  # substitua pelo caminho real

# Listar todos os arquivos CSV na pasta
arquivos_csv <- list.files(path = caminho_da_pasta, 
                           pattern = "\\.csv$", 
                           full.names = TRUE)

# Filtrar a lista, excluindo paths que contenham "TWN" - Taiwan #
arquivos_csv <- arquivos_csv[!grepl("TWN", arquivos_csv)]

# Ler todos os arquivos e armazenar em uma lista
lista_de_dados <- lapply(arquivos_csv, read.csv)  # ou read.csv2 para vírgula como separador decimal

# Nomeia os elementos da lista com os nomes dos arquivos
nomes <- gsub("\\.csv$|ttl", "", basename(arquivos_csv))
names(lista_de_dados) <- nomes

# Salva um exemplo
write.xlsx(lista_de_dados$ARG1995,file = "Ut_exemplo.xlsx", firstRow = T, firstCol = T)

# Separar consumo intermediario e demanda final total

CI = list()
Ft = list()

for (i in 1:length(lista_de_dados)) {
  
  # Seleciona o deflator e a taxa de cambio
  exchange_rate = exchange_rate_GDP$Value[i] #  
  deflator = deflator_GDP$Value[i] # seleciona a taxa de cambio
  
  # CIn
  Meepo <- lista_de_dados[[i]][1:45, 2:46]  # Extrai consumo intermediário
  rownames(Meepo) <- titulo    # Nomeia as linhas
  
  Meepo = Meepo %>% as.matrix()
  
  # agrega os setores 
  
  Meepo = t(agregador) %*% Meepo %*% agregador
  
  # Aplica o deflator e taxa de cambio
  Jull = Meepo * exchange_rate / deflator
  
  # Salva o CI
  CI[[i]] = Jull
  
  # Fn
  Meepo = lista_de_dados[[i]][1:45,47:55]
  rownames(Meepo) = titulo
  
  Meepo = Meepo %>% as.matrix()
  
  # agrega os setores 
  
  Meepo = t(agregador) %*% Meepo 
  
  Meepo = Meepo %>% as.data.frame()
  
  # Ajustando componentes da demanda final
  Pudge = Meepo %>% 
    mutate(HFCE = HFCE + NPISH + DPABR, # soma consumo das familias e empresas sem fins lucrativos + consumo direto de residentes
           EXPO = EXPO + CONS_NONRES) %>% # soma exportacao e compras diretas de nao residentes (exportacao)
    select(-c(NPISH,CONS_NONRES,DPABR,IMPO))
  
  # Aplica o deflator e taxa de cambio
  Jull = Pudge * exchange_rate / deflator
  
  # Salva o CI
  Ft[[i]] = Jull
  
}

names(CI) = nomes
names(Ft) = nomes


