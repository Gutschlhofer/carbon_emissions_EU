# Packages and shapefile -------------------------------------------------------
## NUTS3, excluded oversea regions/countries, calculate area, add Kosovo and Bosnia
source("code/00_libraries_functions.R")


# Download and prepare data ----------------------------------------------------
## common standard: long tibble/data.frame, with year|nuts3_id|value|indicator

## EDGAR: CO2 emissions excl. short cycle C
source("code/01_download_EDGAR.R")

## Eurostat: Population, GDP/capita, GWA of mining and industry
source("code/02_download_eurostat.R") 

## AGRI4CAST: Heating and cooling days
source("code/03_download_temp_heat_cool.R") 


# Combine data -----------------------------------------------------------------
## merge data, exclude NAs, deal with outliers, create correlation tables, 
## aggregate on NUTS2 for MAUP
source("code/04_combine_data.R")


# Provide and visualise input data ---------------------------------------------
source("code/05_visualise_input.R")


# Run OLS ----------------------------------------------------------------------
source("code/06_model_OLS.R")


# Prepare and run GWR ----------------------------------------------------------
source("code/07_model_GWR.R")



# RMarkdown stuff  -------------------------------------------------------------
# data <- readRDS("input/data.rds")
# data_coef <- readRDS("input/data_coef.rds")
# gwr <- readRDS("input/gwr.rds")
# 
# # knit the short presentation  -------------------------------------------------
# file_name <- "./code/P1_short_presentation"
# rmarkdown::render(paste0(file_name,".Rmd"))
# 
# # knit the final presentation  -------------------------------------------------
# file_name <- "./code/P2_final_presentation"
# rmarkdown::render(paste0(file_name,".Rmd"))

