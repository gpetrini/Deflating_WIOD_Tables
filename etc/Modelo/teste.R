
dados = dados_escalares$BRA

df = rowSums(dados$Ft) - rowSums(dados$M)

tx_cresc = 100 * (df[-1] - df[-26]) / df[-26]

tx_cresc

V0 = df["2018"]

# Função para calcular CAGR
tx_geometrica <- function(Vn, V0, n) {
  
  taxa <- (Vn / V0)^(1/n) - 1
  
  return(taxa * 100)  # Retorna em %
  
}

# Exemplo de uso

## 2009-2018 ##
cagr_resultado <- tx_geometrica(df["2018"], df["2009"], 9) 
print(paste0("Taxa média de crescimento: ", round(cagr_resultado, 2), "%"))

## 1995-2007 ##
cagr_resultado <- tx_geometrica(df["2007"], df["1995"], 12)
print(paste0("Taxa média de crescimento: ", round(cagr_resultado, 2), "%"))
