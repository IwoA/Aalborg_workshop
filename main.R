# Config
if (!require(here)) install.packages("here")
here::i_am("main.R")
source(here::here("R/0_config.R"))

# 1. Downloading data
source(here("R/1_Data_download.R"), encoding = "UTF-8")

# 2. Data validation
source(here("R/2_Data_validation.R"), encoding = "UTF-8")

# 3. Data transformations
source(here("R/3_Data_transformation.R"), encoding = "UTF-8")

# 4. Estimation of parameters
source(here("R/4_Parameter_estimation.R"), encoding = "UTF-8")

# 5. Model SFC
source(here("R/5_Model_SFC_Poland.R"), encoding = "UTF-8")
