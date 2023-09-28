# Config
here::i_am("R/3_Data_transformation.R")
source(here::here("R/0_config.R"))

time_lower <- "2010-01-01"
time_upper <- "2022-04-01"

# Reading data
data_raw <- read_csv2(here("data/data_raw.csv"))

# 1. Variable transformations -------------------------------------------------
cat("\n")
logger::log_info("Variable transformations ...")

# Discard observations below the selected year
data <- data_raw %>% filter(
  time >= time_lower,
  time <= time_upper
)

# Imputation of missing data
data <- ts(data[, colnames(data) != "time"],
  start = c(year(min(data$time)), quarter(min(data$time))),
  frequency = 4
) %>%
  imputeTS::na_seadec(algorithm = "kalman", model = "StructTS") %>%
  tsbox::ts_df() %>%
  tsbox::ts_wide()

# Seasonal decomposition of variables
j <- ncol(data)
seas_info <- data.frame(name = character(0), is_seas = logical(0))

for (i in seq(2, ncol(data))) {
  message("\r", i, "/", j, appendLF = F)
  varname <- colnames(data)[i]

  # Seasonality tests
  if (seastests::isSeasonal(ts(data[, i]), freq = 4)) {
    variable <- data %>%
      select(time, !!varname) %>%
      drop_na()
    variable_ts <- ts(
      variable[[varname]],
      start = c(year(min(variable$time)), quarter(min(variable$time))),
      frequency = 4
    )

    # Decomposition
    decomp <- decompose(variable_ts)
    seas <- decomp$seasonal
    deseas <- variable_ts - seas

    varname_og <- paste0(varname, "_og")
    varname_seas <- paste0(varname, "_seas")

    data <- data %>%
      rename(!!varname_og := !!varname)

    variable <- tibble(
      time = zoo::as.Date(time(deseas)),
      !!varname := deseas,
      !!varname_seas := seas
    )

    data <- full_join(data, variable, by = "time")

    seas_info <- rbind(seas_info, data.frame(name = varname, is_seas = T))
  } else {
    seas_info <- rbind(seas_info, data.frame(name = varname, is_seas = F))
  }
}

# Auxiliary variables
data <- data %>% mutate(
  dummy_covid = ifelse(time == "2020-04-01", 1, 1e-10),
  dummy_taxes = ifelse(time >= "2017-01-01", 1, 1e-10)
)

# Writing data after transformations
data <- data %>% select(time, order(colnames(data)))

write_csv2(data, here("data/data.csv"), na = "")
write_csv2(seas_info, here("data/seasonality.csv"), na = "")
