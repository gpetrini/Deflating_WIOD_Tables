
load("../etc/Modelo/NIOTs_resultados.rdata") # carregando os dados

WIOD_countries <- NIOTs |> names()
country <- NIOTs[[WIOD_countries[1]]]
tbls <- country |> names()
years <- country[[1]] |> names()
first_year <- years[1]
last_year <- tail(years, 1)

country <- NULL
