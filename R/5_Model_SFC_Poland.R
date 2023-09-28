# Config
here::i_am("R/5_Model_SFC_Poland.R")
source(here::here("R/0_config.R"))

# Reading data
data <- read_csv2(here("data/data.csv"))
seasonality <- read_csv2(here("data/seasonality.csv"))
param <- read_csv2(here("data/parameters.csv"))
equations <- read_excel(here("data/dictionary.xlsx"), sheet = "Equations") %>%
  pull(Equation)
param <- read_excel(here("data/dictionary.xlsx"), sheet = "Parameters") %>%
  select(Name, Parameter) %>%
  pivot_wider(names_from = Name, values_from = Parameter)


# 1. Model initialization--------------------------------------------------
model <- create_model(name = "Poland")

# Variables
for (i in 2:ncol(data)) {
  model <- model %>% add_variable(colnames(data[i]), data[[i]])
}

# Parametry
for (i in 1:ncol(param)) {
  model <- model %>% add_variable(colnames(param[i]), param[[i]])
}

# Equations
for (i in equations) {
  model <- model %>% add_equation(i)
}

# 2. Simulation  ------------------------------------------------------------
model <- simulate_scenario(
  model,
  start_date = data$time[1], periods = nrow(data)
)

# 3. Review of results -----------------------------------------------------
# Adding seasonality
data_out <- model$baseline$result
data_out_seas <- add_seasonality(data_out, seasonality)
data_seas <- add_seasonality(data, seasonality)

# Plot
see_fit("Y", data, data_out)
see_fit("Y", data_seas, data_out_seas)

# 4. Scenarios - shocks --------------------------------------------------
# 4.1. Increase in interest rates
shock_1 <- create_shock() %>%
  add_shock(
    c("chi", "r_A_H", "r_N", "r_L_H", "r_G"),
    rate = 5, start = "2015-01-01"
  )
model <- model %>%
  add_scenario("rates", shock = shock_1) %>%
  simulate_scenario()

plot_simulation(model, c("baseline", "rates"), expressions = "I")

# 4.2. Growth of taxes
shock_2 <- create_shock() %>%
  add_shock(c("par_T_N_1", "par_T_H_1"),
            rate = 0.1,
            start = "2015-01-01", end = "2016-10-01"
  )
model <- model %>%
  add_scenario("taxes", shock = shock_2) %>%
  simulate_scenario()

plot_simulation(model, c("baseline", "taxes"), expressions = "C")

# 4.3. Growth of government spending
shock_3 <- create_shock() %>%
  add_shock("G",
            rate = 0.1,
            start = "2015-01-01", end = "2015-10-01"
  )
model <- model %>%
  add_scenario("government_spending", shock = shock_3) %>%
  simulate_scenario()

plot_simulation(
  model, c("baseline", "government_spending"),
  expressions = "C"
)

# 4.4. Wage increase
shock_4 <- create_shock() %>%
  add_shock("par_W_1", rate = 0.1, start = "2015-01-01")
model <- model %>%
  add_scenario("wages", shock = shock_4) %>%
  simulate_scenario()

plot_simulation(model, c("baseline", "wages"), expressions = "C")

# 4.5. Increase of unemployment
shock_5 <- create_shock() %>%
  add_shock("par_N_1", rate = -0.1, start = "2015-01-01")
model <- model %>%
  add_scenario("unemployment", shock = shock_5) %>%
  simulate_scenario()

plot_simulation(model, c("baseline", "unemployment"), expressions = "C")

# 6. Sensitivity ----------------------------------------------------------
# Research on the sensitivity of the household taxation parameter
model_sen <- model %>%
  create_sensitivity(
    variable = "par_T_H_1", lower = 0.02, upper = 0.08, step = 0.01
  ) %>%
  simulate_scenario(start_date = data$time[1], periods = nrow(data))

plot_simulation(
  model_sen,
  scenario = "sensitivity", take_all = T, expressions = "C"
)

# 6. Saving the model and data -------------------------------------------
saveRDS(model, file = here("data/model.Rds"))
write_csv2(data_out, here("data/data_out.csv"), na = "")
write_csv2(data_out_seas, here("data/data_out_seas.csv"), na = "")
