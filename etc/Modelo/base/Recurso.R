# Recurso

# Producao
Meepo <- dados_DOM[[ pais ]][[ ano]][c(91,92,94,95), 2:46]  # Extrai consumo intermediário

Meepo = Meepo %>% as.matrix()

# agrega os setores 

Meepo = Meepo %*% agregador

Meepo = Meepo %>% as.data.frame()

# Soma os impostos
Pudge = rbind(
  Meepo[1,] + Meepo[2,],
  Meepo[3:4,]
)
rownames(Pudge) = c("II","VA","GO")

# Aplica o deflator e taxa de cambio
Jull = Pudge * deflator

# Salva o Recursos
Recurso[[ ano ]] = Jull
