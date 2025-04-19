### Compilação das NIOT ### 
### FIXME: Adjust the call to compile the databse

source("./global_variables.R")
source("./support_functions.R")

results <- prepare_data()


decomp <- decompose_growth()


group_plots(
  data = decomp,
  IO = results,
  groups = all_groups,
  verbose = FALSE
)
