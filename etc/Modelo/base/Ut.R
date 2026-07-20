# Ut

# CI
Meepo <- CIn[[ ano ]] + CIm [[ ano ]]  # Extrai consumo intermediário

# Aplica o deflator e taxa de cambio
Jull = Meepo # * deflator

# Salva o CI
CI[[ ano ]] = Jull

# Ft
Meepo <- Fn[[ ano ]] + Fm[[ ano ]]  # Extrai demanda final

# Aplica o deflator e taxa de cambio
Jull = Meepo # * deflator

# Salva o CI
Ft[[ ano ]] = Jull


