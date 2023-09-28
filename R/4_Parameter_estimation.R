# 4 Parameter estimation

# Config
if (!require(here)) install.packages("here")
here::i_am("R/4_Parameter_estimation.R")
source(here::here("R/0_config.R"))

# Reading data
data <- read_csv2(here("data/data.csv"))
dict <- loadWorkbook(here("data/dictionary.xlsx"))
dict_equations <- read_excel(here("data/dictionary.xlsx"), sheet = "Equations")
dict_equations_og <- dict_equations
param_equations <- read_excel(here("data/dictionary.xlsx"), sheet = "Equations") %>%
  filter(!is.na(Param_equation)) %>%
  pull(Param_equation)
dict_parameters_og <- read_excel(here("data/dictionary.xlsx"), sheet = "Parameters")

# 1. Editing equations --------------------------------------------------------

# n - next
# `equation` - enter equation
# t - transform
# r - reset
# q - quit

# restimate()

# 2. Parameter estimation -------------------------------------------------
cat("\n")
logger::log_info("Parameter estimation")

estimate()

# 3. Saving data -----------------------------------------------------
dict_equations$
  Param_equation[which(!is.na(dict_equations$Param_equation) == T)] <-
  param_equations
if (!identical(dict_equations, dict_equations_og)) {
  writeData(dict, sheet = "Equations", dict_equations)
  saveWorkbook(dict, here("data/dictionary.xlsx"), overwrite = T)
}

dict_parameters <- tibble(
  Name = parameters$coeff_str,
  Parameter = parameters$coeff,
  Explanatory_variable = rownames(parameters),
  Comment = NA
)
if (!isTRUE(all.equal(dict_parameters, dict_parameters_og))) {
  writeData(dict, sheet = "Parameters", dict_parameters)
  saveWorkbook(dict, here("data/dictionary.xlsx"), overwrite = T)
}
