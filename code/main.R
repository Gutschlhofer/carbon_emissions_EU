# Packages and shapefile -------------------------------------------------------
# we will use NUTS3, we add the shapefile directly for all non-EUROSTAT stuff
source("code/00_libraries_functions.R")

# Download data ----------------------------------------------------------------
# after downloading, the data will be pre-processed to have a common standard
# in particular, a long tibble/data.frame with column $indicator
# variable names are lower case letters, the actual value is saved in $value
source("code/01_download_EDGAR.R") # output: data_edgar
source("code/02_download_eurostat.R") # output: data_eurostat
source("code/03_download_temp_heat_cool.R") # output: data_heating_cooling, no temperature used

# Combine data -----------------------------------------------------------------
source("code/04_combine_data.R") # output: data

# Provide and visualise input data ---------------------------------------------
source("code/05_visualise_input.R")

# Run OLS ----------------------------------------------------------------------
source("code/06_model_OLS.R")

# Prepare and run GWR ----------------------------------------------------------
source("code/07_model_GWR.R")
# source("code/08_model_GWR_ext.R") # no extensions yet 


# RMarkdown stuff  -------------------------------------------------------------

data <- readRDS("input/data.rds")
data_coef <- readRDS("input/data_coef.rds")
gwr <- readRDS("input/gwr.rds")

# knit the short presentation  -------------------------------------------------
file_name <- "./code/P1_short_presentation"
rmarkdown::render(paste0(file_name,".Rmd"))

# knit the final presentation  -------------------------------------------------
file_name <- "./code/P2_final_presentation"
rmarkdown::render(paste0(file_name,".Rmd"))
