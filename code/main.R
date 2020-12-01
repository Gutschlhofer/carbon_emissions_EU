# Load libraries
library(tidyverse)
library(eurostat)
library(stringr)
library(sf)
library(raster)
library(rgdal)
library(ncdf4) # we need this package but i think we need to ask someone to install the package on the server


# Download data ----------------------------------------------------------------
# we will use NUTS3, we add the shapefile directly for all non-EUROSTAT stuff
# after downloading, the data will be pre-processed to have a common standard
# in particular, a long tibble/data.frame with column $indicator
# variable names are lower case letters, the actual value is saved in $value

# shape_nuts3 is our base-shapefile
source("code/00_download_shapefile.R")

source("code/01_download_EDGAR.R") # output: data_edgar
# output: data_eurostat
# output: data_temperature

# Combine data -----------------------------------------------------------------

# output: data

# Provide and visualise input data ---------------------------------------------

# Run OLS ----------------------------------------------------------------------

# Prepare and run GWR ----------------------------------------------------------

# Extensions  ------------------------------------------------------------------

