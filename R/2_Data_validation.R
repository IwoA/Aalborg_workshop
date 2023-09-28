# Config
here::i_am("R/2_Data_validation.R")
source(here::here("R/0_config.R"))

# Reading data
data_raw <- read_csv2(here("data/data_raw.csv"))
equations <- read_excel(here("data/dictionary.xlsx"), sheet = "Equations") %>%
  filter(is.na(Param_equation)) %>%
  pull(Equation)

data_raw <- data_raw %>% filter(
  time >= "2010-01-01",
  time <= "2022-04-01"
)

# Overview of raw series
miss_rates <- sapply(data_raw, function(x) sum(is.na(x)) / length(x)) %>%
  data.frame("miss_rate" = .) %>%
  rownames_to_column("name")

miss_rates_1 <- miss_rates %>% filter(miss_rate == 1)

if (nrow(miss_rates_1) > 0) {
  stop(paste0(
    "Te zmienne są puste w badanym przedziale czasowym: ",
    paste0(miss_rates_1$name, collapse = ", "),
    "\nSprawdź słownik zmiennych i pobieranie danych"
  ))
}

miss_rates_0.05 <- miss_rates %>%
  filter(miss_rate >= 0.05) %>%
  arrange(desc(miss_rate))

miss_rates_0.05
# miss <- sapply(data_raw, function(x) sum(is.na(x)) / length(x))
# for (i in names(which(miss > 0.05))) {
#   print(plot(data_raw$time, data_raw[, i][[1]], type = "l"))
#   title(paste(i, miss[i]))
# }



# Transaction matrix
checks_tr <- data_raw %>%
  transmute(
    time,
    investment    = I - I_N - I_F - I_G - I_H,
    taxes         = -T_N - T_F + T_G - T_H - T_W,
    grossop       = B_2_N - B_2 + B_2_F + B_2_G + B_2_H,
    wages         = -WB_N + WB_H + WB_W,
    capincome     = CAP_N + CAP_F + CAP_G + CAP_H + CAP_W,
    transfers     = STR_N + STR_F + STR_G + STR_H + STR_W,
    captransf     = KTR_N + KTR_F + KTR_G + KTR_H + KTR_W,
    acquisitions  = NP_N + NP_F + NP_G + NP_H + NP_W,
    netlending    = NL_N + NL_F + NL_G + NL_H + NL_W
  ) %>%
  mutate(across(everything(), round))

# Balance matrix
checks_bs <- data_raw %>%
  transmute(
    time,
    ib            = IBA_FH - IBL_FH + IBA_H - IBL_H,
    nib           = NIB_N + NIB_F + NIB_G + NIB_W,
    neq           = NEQ_N + NEQ_F + EQA_H + NEQ_W,
    fnw           = FNW_N + FNW_F + FNW_H + FNW_W,
    k             = K_N + K_F + K_G + K_H
  ) %>%
  mutate(across(everything(), round))

# Validation of model equations
validate <- function(e = NA) {
  if (!is.na(e)) {
    dict_diff <- sapply(equations, function(x) get_variables(x)[1])
    equations <- equations[which(dict_diff == e)[[1]]]
  } else {
    diffs <<- data.frame(Equation = character(0), mape = numeric(0), miss = numeric(0))
  }

  d <- function(x) c(NA, diff(x))

  for (i in 1:length(equations)) {
    message("\r", i, "/", length(equations), appendLF = F)

    eqn <- equations[i]
    eqn_sp <- str_split(eqn, "=")
    var <- str_replace_all(eqn_sp[[1]][1], " ", "")
    var_teo <- paste0(str_replace_all(var, " ", ""), "_teo")

    exprs_str <- eqn_sp[[1]][2]
    vars <- get_variables(exprs_str)

    for (j in 1:4) {
      exprs_str <- gsub(
        paste0(
          "(?<![[:alnum:]]|\\.|\\_)(", paste0(vars, collapse = "|"), ")(?=\\[-", j, "\\])"
        ),
        paste0("lag(\\1, ", j, ")"), exprs_str,
        perl = T
      )
      exprs_str <- gsub(paste0("\\[-", j, "\\]"), "", exprs_str)
    }

    data_teo <- data_raw %>%
      transmute(!!var_teo := !!rlang::parse_expr(exprs_str)) %>%
      pull()

    data_real <- pull(data_raw, var)

    mape <- round(mean(abs((data_real - data_teo) / data_real), na.rm = T), 3)
    miss <- round(1 - (sum(!is.na(data_teo)) / sum(!is.na(data_real))), 3)

    if (!is.na(e)) {
      print(
        ggplot() +
          geom_line(aes(x = data_raw$time, y = data_real)) +
          geom_line(aes(x = data_raw$time, y = data_teo), col = "#8494FF") +
          scale_x_date(date_breaks = "1 year") +
          labs(title = paste0(eqn, "   MAPE: ", mape, " Missing rate: ", miss)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    } else {
      diffs[i, ] <<- list(Equation = eqn, mape = mape, miss = miss)
    }
  }
  cat("\n")
}

validate()
