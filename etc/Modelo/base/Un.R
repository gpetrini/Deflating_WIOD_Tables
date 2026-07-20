## CIn ## 

Meepo = dados_DOM[[ pais ]][[ ano ]][1:45, 2:46]  # Extrai consumo intermediário
rownames(Meepo) <- titulo # Nomeia as linhas

Meepo = Meepo %>% as.matrix()

# agrega os setores 

Meepo = t(agregador) %*% Meepo %*% agregador

# Aplica o deflator e taxa de cambio

Jull = Meepo * deflator

# Salva o CI
CIn[[ ano ]] = Jull

## Fn ##
Meepo = dados_DOM[[ pais ]][[ ano ]][1:45,47:56]
rownames(Meepo) = titulo

Meepo = Meepo %>% as.matrix()

# agrega os setores 

Meepo = t(agregador) %*% Meepo 

Meepo = Meepo %>% as.data.frame()

# Ajustando componentes da demanda final
Pudge = Meepo %>% 
  mutate(HFCE = HFCE + NPISH, # soma consumo das familias e empresas sem fins lucrativos 
         EXPO = EXPO + CONS_NONRES) %>% # soma exportacao e compras diretas de nao residentes (exportacao)
  select(-c(NPISH,CONS_NONRES,DPABR,IMPO,TOTAL))

# Aplica o deflator e taxa de cambio
Jull = Pudge * deflator

# Salva o CI
Fn[[ ano ]] = Jull
