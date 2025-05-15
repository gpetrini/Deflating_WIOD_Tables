
load("../etc/Modelo/NIOTs_resultados.rdata") # carregando os dados

WIOD_countries <- NIOTs |> names()
country <- NIOTs[[WIOD_countries[1]]]
tbls <- country |> names()
years <- country[[1]] |> names()
first_year <- years[1]
last_year <- tail(years, 1)

country <- NULL

expenditures_dom <- c("C", "I", "G", "E")
expenditures <- c(expenditures_dom, "X", "CDX", "CDD", "CDI", "M")
vars <- c(expenditures, "GDP")

plotW     <- 28     # plot window width
plotH     <- 16     # plot window height

## NOTE: Some generic groups
country_groups <- list(

  "Developed" = c(
    "USA", "DEU", "NLD", "JPN"
  ),
  "Developing" = c(
    "BRA", "MEX", "CHN", "IND"
  )
  ## BRICS = c("BRA", "RUS", "IND", "CHN", "ZAF"),
  ## G7 = c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
  ## MINT = c("MEX", "IDN", "NGA", "TUR"),
  ## ASEAN = c("IDN", "THA", "MYS", "PHL", "SGP", "VNM"),
  ## FragileFive = c("BRA", "IND", "IDN", "TUR", "ZAF"),
  ## Nordic = c("DNK", "FIN", "ISL", "NOR", "SWE")
)

saveRDS(country_groups, "../objs/country_groups.rds")

# Append using single countries as the function should work the same way
all_groups <- c(
  country_groups,
  setNames(as.list(WIOD_countries), WIOD_countries)
)
