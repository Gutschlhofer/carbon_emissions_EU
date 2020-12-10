# Download data ----------------------------------------------------------------
# we will use NUTS3, we add the shapefile directly for all non-EUROSTAT stuff
# after downloading, the data will be pre-processed to have a common standard
# in particular, a long tibble/data.frame with column $indicator
# variable names are lower case letters, the actual value is saved in $value

# shape_nuts3 is our base-shapefile
source("code/00_libraries_functions.R")

source("code/01_download_EDGAR.R") # output: data_edgar
# output: data_eurostat
# output: data_temperature

# Combine data -----------------------------------------------------------------

# output: data

# Provide and visualise input data ---------------------------------------------

# Run OLS ----------------------------------------------------------------------

# Prepare and run GWR ----------------------------------------------------------

# Extensions  ------------------------------------------------------------------

