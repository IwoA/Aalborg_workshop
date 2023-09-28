get_bop_c6_q <- function() {
  assign(table_id, eurostat::get_eurostat(table_id, filters = list(
    bop_item = c("CA", "D1", "D4P__F", "D2"), currency = "MIO_NAC",
    partner = "WRL_REST", geo = "PL", sector10 = "S1", sectpart = "S1",
    stk_flow = "BAL", freq = "Q"
  )), pos = 1)
}

get_variables <- function(exprs_str) {
  functions <- stringr::str_replace_all(exprs_str, "[!%^&*)\\-+=\\[\\]|`~<,>/]", " ") %>%
    stringr::str_replace_all("[(]", "( ") %>%
    stringr::str_squish() %>%
    stringr::str_split(" ", simplify = T) %>%
    grep("\\(", ., value = T) %>%
    stringr::str_replace_all("[(]", "")
  variables_eqs <- stringr::str_replace_all(exprs_str, "[!%^&*()\\-+=\\[\\]|`~<,>/]", " ") %>%
    stringr::str_squish() %>%
    stringr::str_split(" ", simplify = T) %>%
    vecsets::vsetdiff(functions) %>%
    unique()
  variables_eqs <- variables_eqs[suppressWarnings(is.na(as.numeric(variables_eqs)))]
  return(variables_eqs)
}

custom_variable <- function(vardict_item) {
  varname <- vardict_item[["Name"]]
  exprs_str <- vardict_item[["Calc"]]
  exprs <- parse(text = exprs_str)

  table_id <- dict_raw[dict_raw$Name == varname, ]$online_data_code
  table <- get(table_id)

  variable <- dict_raw[dict_raw$Name == varname, ]
  columns_na <- colnames(variable[, colnames(variable)[is.na(variable)]])
  variable <- merge(table, variable[, !(colnames(dict_raw) %in% columns_na)])
  columns_cal <- intersect(columns_na, colnames(table))

  variables_eqs <- get_variables(exprs_str)

  variable <- variable %>%
    select(all_of(c("time", "values", columns_cal))) %>%
    pivot_wider(names_from = all_of(columns_cal), values_from = values)

  for (i in variables_eqs) {
    if (i %in% colnames(variable)) next
    variable <- variable %>% mutate(
      !!as.symbol(i) := 0
    )
  }

  variable[is.na(variable)] <- 0
  variable <- variable %>% transmute(time, !!as.symbol(varname) := eval(exprs))
  variable[variable == 0] <- NA

  return(variable)
}

see_fit <- function(variable, data_real, data_fit) {
  ggplot() +
    geom_line(data = data_real, aes(x = time, y = !!as.symbol(variable))) +
    geom_line(
      data = data_fit, aes(x = time, y = !!as.symbol(variable)),
      col = "#8494FF"
    ) +
    scale_x_date(breaks = seq.Date(min(data_real$time), by = "year", max(data_real$time))) +
    labs(
      title = paste(
        variable,
        "        MAE: ",
        round(mean(abs(data_real[[variable]] - data_fit[[variable]])), 2),
        "  MAPE: ",
        round(mean(abs((data_real[[variable]] - data_fit[[variable]]) /
          data_real[[variable]])), 2)
      )
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# temporary update of the  eurostat package function  
assignInNamespace(
  "eurostat_json_url",
  function(id, filters, lang) {
    filters2 <- as.list(unlist(filters))
    names(filters2) <- rep(names(filters), lapply(filters, length))
    url_list <- list(
      scheme = "https", hostname = "ec.europa.eu",
      path = file.path(
        "eurostat/api/dissemination/statistics/1.0/data",
        id
      ), query = filters2
    )
    class(url_list) <- "url"
    url <- httr::build_url(url_list)
    url
  },
  asNamespace("eurostat")
)

assignInNamespace(
  "eurotime2date",
  function(x, last = FALSE) {
    if (!is.factor(x)) {
      x <- factor(x)
    }
    times <- levels(x)
    times <- gsub("-", "", times)
    year <- substr(times, 1, 4)
    subyear <- substr(times, 5, 7)
    tcode <- substr(subyear[1], 1, 1)
    if (tcode != "_" && nchar(times[1]) > 7) {
      days <- substr(times, 8, 10)
      tcode <- substr(days[1], 1, 1)
    }
    if (tcode == "") {
      tcode <- "Y"
    }
    day <- "01"
    if (tcode == "Y") {
      period <- "01"
    } else if (tcode == "S") {
      lookup <- c(S1 = "01", S2 = "07")
      period <- lookup[subyear]
    } else if (tcode == "Q") {
      lookup <- c(Q1 = "01", Q2 = "04", Q3 = "07", Q4 = "10")
      period <- lookup[subyear]
    } else if (tcode == "M") {
      period <- gsub("M", "", subyear)
    } else if (tcode == "D") {
      period <- gsub("M", "", subyear)
      day <- gsub("D", "", days)
    } else if (tcode == "_") {
      warning("Time format is a year interval. No date conversion was made.")
      return(x)
    } else {
      warning(
        "Unknown time code, ", tcode, ". No date conversion was made.\n\n            Please fill bug report at ",
        "https://github.com/rOpenGov/eurostat/issues."
      )
      return(x)
    }
    levels(x) <- paste0(year, "-", period, "-", day)
    if (last == TRUE) {
      shift <- c(Y = 367, S = 186, Q = 96, M = 32, D = 0)[tcode]
      levels(x) <- lubridate::ymd(cut(lubridate::ymd(levels(x)) +
        shift, "month")) - 1
    }
    y <- lubridate::ymd(x)
    y
  },
  asNamespace("eurostat")
)

estimate <- function(eqn_n = NULL, y_tf = "", new_eqn = NULL) {
  data_ts <- ts(data[, colnames(data) != "time"],
                start = c(year(min(data$time)), quarter(min(data$time))),
                frequency = 4
  )
  
  parameters <- data.frame()
  
  if (!is.null(eqn_n)) param_equations <- param_equations[eqn_n]
  if (!is.null(new_eqn)) param_equations <- new_eqn
  
  for (i in 1:length(param_equations)) {
    if (length(param_equations) > 1) message("\n", i, "/", length(param_equations))
    
    # Feature selection
    variables <- get_variables(param_equations[i])
    data_fs <- select(data, all_of(variables))
    
    for (j in variables) {
      j_log <- paste0(j, "_log")
      exprs_str_log <- paste0("log(", j, ")")
      variable <- data_fs %>%
        transmute(!!j_log := !!rlang::parse_expr(exprs_str_log))
      data_fs <- cbind(data_fs, variable)
    }
    for (j in colnames(data_fs)) {
      for (l in 1:4) {
        j_l <- paste0(j, "_l", l)
        exprs_str_l <- paste0("lag(", j, ", ", l, ")")
        variable <- data_fs %>%
          transmute(!!j_l := !!rlang::parse_expr(exprs_str_l))
        data_fs <- cbind(data_fs, variable)
      }
      for (d in 1:4) {
        j_d <- paste0(j, "_d", d)
        exprs_str_d <- paste0("c(rep(NA, ", d, "), ", "diff(", j, ", ", d, "))")
        variable <- data_fs %>%
          transmute(!!j_d := !!rlang::parse_expr(exprs_str_d))
        data_fs <- cbind(data_fs, variable)
      }
      for (ld in 1:4) {
        j_ld <- paste0(j, "_ld", ld)
        exprs_str_ld <- paste0("lag(", "c(NA, ", "diff(", j, ")), ", ld, ")")
        variable <- data_fs %>%
          mutate(!!j_ld := !!rlang::parse_expr(exprs_str_ld), .keep = "none")
        data_fs <- cbind(data_fs, variable)
      }
    }
    
    corr <<- data_fs %>%
      cor(use = "complete") %>%
      as.data.frame.matrix() %>%
      rownames_to_column() %>%
      pivot_longer(-rowname, names_to = "name", values_to = "corr") %>%
      filter(rowname == paste0(variables[1], y_tf)) %>%
      arrange(desc(abs(corr)))
    
    print(corr)
    
    ## Model
    eqn <- param_equations[i]
    model <- dynlm::dynlm(as.formula(eqn), data_ts)
    
    ## Stationarity tests
    cat("\n")
    for (j in 1:ncol(model$model)) {
      suppressWarnings(adf_p <- tseries::adf.test(model$model[[j]])$p.value)
      is_stat <- ifelse(adf_p < 0.05, T, F)
      cat(colnames(model$model[j]), " Stationarity: ", is_stat, "\n")
    }
    
    ## Summary
    print(summary(model))
    cat(eqn, "\n", sep = "")
    clipr::write_clip(eqn)
    
    ## Prediction vs reality plot
    print(autoplot(ts.union(model$fitted.values, model$model[1]), main = eqn))
    
    # Adding parameters
    coeff_str <- get_variables(deparse(model$terms[[2]]))
    if (length(model$coefficients) == 1) {
      coeff_str <- paste0("par_", coeff_str)
    } else {
      coeff_str <- paste0("par_", coeff_str, "_", 1:length(model$coefficients))
    }
    parameters <- rbind(
      parameters,
      data.frame(coeff = model$coefficients, coeff_str)
    )
  }
  parameters <<- parameters
}

restimate <- function() {
  print(param_equations)
  param_equations_old <- param_equations
  
  i <- as.numeric(readline("Which equation do you want to edit?: "))
  y_tf <- ""
  status <- NULL
  new_eqn <- NULL
  
  while (!identical(status, "q")) {
    message(i, "/", length(param_equations))
    
    is_estim <- FALSE
    tryCatch(
      {
        estimate(eqn_n = i, y_tf = y_tf, new_eqn = new_eqn)
        is_estim <- TRUE
      },
      error = function(e) message("Estimation error, check the input expression.")
    )
    
    if (!is_estim) {
      tryCatch(
        {
          eval(parse(text = new_eqn))
        },
        error = function(e) message("Evaluation error, check the entered expression.")
      )
    }
    
    status <- as.character(readline("What you want to do? (n/ equation/ t/ r/ q): "))
    
    if (status == "n") {
      if (!is.null(new_eqn)) param_equations[i] <<- new_eqn
      i <- i + 1
      if (i == (length(param_equations) + 1)) i <- 1
      y_tf <- ""
      new_eqn <- NULL
    } else if (status == "t") {
      y_tf <- as.character(readline("Enter the transformation: "))
    } else if (status == "r") {
      y_tf <- ""
      new_eqn <- NULL
    } else {
      new_eqn <- status
    }
  }
  
  param_equations_diff <- c()
  change <- FALSE
  for (i in 1:length(param_equations)) {
    if (param_equations_old[i] != param_equations[i]) {
      param_equations_diff[i] <- paste0(
        "change: ", param_equations_old[i], "  ---->  ", param_equations[i]
      )
      change <- TRUE
    } else {
      param_equations_diff[i] <- param_equations[i]
    }
  }
  
  if (change) {
    print(param_equations_diff)
    status <- as.character(readline("Do you want to keep changes? (y/n) "))
    if (status == "n") param_equations <- param_equations_old
  }
}

add_seasonality <- function(data, seas_info){
  for (i in 1:nrow(seas_info)) {
    if (seas_info[i, ]$is_seas) {
      name <- seas_info[i, ]$name
      name_og <- paste0(name, "_og")
      name_seas <- paste0(name, "_seas")
      exprs_str <- paste0(name, " + ", name_seas)
      
      data <- data %>%
        mutate(!!name := !!rlang::parse_expr(exprs_str)) %>%
        select(-all_of(c(name_og, name_seas)))
      
    }
  }
  return(data)
}
