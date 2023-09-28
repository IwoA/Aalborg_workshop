# Config
here::i_am("R/1_Data_download.R")
source(here::here("R/0_config.R"))

time_lower_raw <- "2000-01-01"

# Reading the variables dictionary
dict_raw <- read_excel(here("data/dictionary.xlsx"), sheet = "Variables")

# Defining columns of the dictionary outside of Eurostat
cols_user <- c(
  "Id", "Name", "Sector_name", "Type", "Description", "Link_to_source", "freq_td",
  "Start_series", "End_series", "Calc", "Comment"
)

# 1. Downloading Eurostat tables -----------------------
cat("\n")
logger::log_info("Downloading the Eurostat tables ...")

tables <- unique(dict_raw$online_data_code)
tables <- tables[!is.na(tables)]

for (table_id in tables) {
  message(
    which(tables == table_id), "/",
    length(tables)
  )

  if (exists(table_id)) rm(list = table_id)

  # Manual filtering of big tables (bop_c6_q)
  if (table_id == "bop_c6_q") {
    get_bop_c6_q()
    message("Downloaded with filters")
    next
  }

  filters <- dict_raw %>%
    filter(online_data_code == table_id) %>%
    select(-all_of(c(cols_user, "online_data_code"))) %>%
    select_if(function(col) all(is.na(col)) == F) %>%
    as.list() %>%
    lapply(unique)

  filters <- filters[sapply(filters, function(x) !any(is.na(x)))]

  tryCatch(
    {
      assign(table_id, eurostat::get_eurostat(table_id, filters = filters))
    },
    error = function(e) message("Downloading with filters failed")
  )

  if (exists(table_id)) {
    message("Downloaded with filters")
  } else {
    message("Downloading full table ...")
    assign(table_id, eurostat::get_eurostat(table_id))
  }
}

# 2. Creating raw variables from the Eurostat tables --------------------------
cat("\n")
logger::log_info("Creating raw variables from the Eurostat tables ...")

dict_estat <- dict_raw %>% filter(is.na(Calc))

data_estat <- data.frame(time = as.Date(integer(0)))

for (i in seq(1, nrow(dict_estat))) {
  message("\r", i, "/", nrow(dict_estat), appendLF = F)
  table_estat <- get(dict_estat$online_data_code[i])
  variable <- merge(table_estat, dict_estat[i, ])
  variable <- variable %>%
    transmute(time, !!pull(dict_estat[i, ], Name) := values)
  data_estat <- full_join(data_estat, variable, by = "time") %>%
    arrange(time)
}

# Transformation of annual time series to quarterly time series
for (col in filter(dict_estat, freq == "A")$Name) {
  freq_td <- dict_estat[dict_estat$Name == col, ]$freq_td
  variable <- data_estat %>%
    select(time, !!col) %>%
    drop_na()
  variable <- ts(
    variable[[col]],
    start = year(min(variable$time)), frequency = 1
  )
  variable <- predict(td(variable ~ 1,
    conversion = freq_td, to = "quarter",
    method = "denton-cholette"
  ))
  variable <- tibble(time = zoo::as.Date(time(variable)), !!col := variable)
  data_estat <- data_estat %>%
    select(-!!col) %>%
    full_join(variable, by = "time") %>%
    arrange(time)
}

# 3. Own calculations -------------------------------------------------------
dict_cust <- dict_raw %>% filter(!is.na(Calc))

# Own calculations based on the raw Eurostat variables
cat("\n")
logger::log_info("Performing own calculations based on Eurostat data ...")

dict_cust_estat <- dict_cust %>% filter(!is.na(online_data_code))

data_cust <- data.frame(time = as.Date(integer(0)))

for (i in 1:nrow(dict_cust_estat)) {
  message("\r", i, "/", nrow(dict_cust_estat), appendLF = F)
  variable <- custom_variable(dict_cust_estat[i, ])
  data_cust <- full_join(data_cust, variable, by = "time") %>%
    arrange(time)
}

# Transformation of annual series into quarterly series
for (col in filter(dict_cust_estat, freq == "A")$Name) {
  freq_td <- dict_cust_estat[dict_cust_estat$Name == col, ]$freq_td
  variable <- data_cust %>%
    select(time, !!col) %>%
    drop_na()
  variable <- ts(
    variable[[col]],
    start = year(min(variable$time)), frequency = 1
  )
  variable <- predict(td(variable ~ 1,
    conversion = freq_td, to = "quarter",
    method = "denton-cholette"
  ))
  variable <- tibble(time = zoo::as.Date(time(variable)), !!col := variable)
  data_cust <- data_cust %>%
    select(-!!col) %>%
    full_join(variable, by = "time") %>%
    arrange(time)
}

# Own calculations based on created variables
cat("\n")
logger::log_info(
  "Performing own calculations based on created variables ..."
)

dict_cust_other <- dict_cust %>%
  filter(is.na(online_data_code)) %>%
  select(Name, Calc)

data_other <- full_join(data_estat, data_cust, by = "time")

i <- 1
j <- nrow(dict_cust_other)

while (nrow(dict_cust_other) > 0) {
  exprs_str <- dict_cust_other$Calc[1]
  if (all(setdiff(get_variables(exprs_str), "NA") %in% colnames(data_other))) {
    message("\r", i, "/", j, appendLF = F)
    variable <- data_other %>%
      transmute(
        time,
        !!dict_cust_other$Name[1] := !!rlang::parse_expr(exprs_str)
      )
    data_other <- full_join(data_other, variable, by = "time")
    data_cust <- full_join(data_cust, variable, by = "time") %>%
      arrange(time)

    dict_cust_other <- dict_cust_other[-1, ]
    i <- i + 1
  } else {
    dict_cust_other[nrow(dict_cust_other) + 1, ] <- dict_cust_other[1, ]
    dict_cust_other <- dict_cust_other[-1, ]
  }
}

data_cust[sapply(data_cust, is.infinite)] <- NA

# 4. Create and save a complete dataframe ------------------------------
cat("\n")
logger::log_info("Writing dataframe")

data_raw <- full_join(data_estat, data_cust) %>%
  arrange(time) %>%
  filter(time >= time_lower_raw)
data_raw <- data_raw %>% select(time, order(colnames(data_raw)))

write_csv2(data_raw, here("data/data_raw.csv"), na = "")

