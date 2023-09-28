# timeout
options(timeout = 120)

# localisation
if (!require(here)) install.packages("here")
here::i_am("R/0_config.R")

# libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  clipr,
  dplyr,
  dynlm,
  eurostat,
  ggfortify,
  here,
  imputeTS,
  logger,
  lubridate,
  readr,
  readxl,
  rlang,
  seastests,
  stringr,
  tempdisagg,
  tibble,
  tidyr,
  tidyselect,
  tsbox,
  tseries,
  zoo,
  vecsets,
  openxlsx
)

# godley library
if (!require(devtools)) install.packages("devtools")
if (!require(godley)) devtools::install_github("gamrot/godley")
library(godley)

# functions
source(here::here("R/0_functions.R"))
