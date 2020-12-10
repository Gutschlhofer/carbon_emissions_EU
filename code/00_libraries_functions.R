# Load libraries
library(tidyverse)
library(eurostat)
library(stringr)
library(sf)
library(raster)
library(rgdal)
library(ncdf4)
library(foreach)
library(doMC)

## eurostat NUTS 3 shapefile----------------------------------------------------

# this is the shapefile for 2021, we use the eurostat package which has the year 2016
getShapefile <- function(replace = FALSE){

  if(file.exists("./input/shapefile/nuts3.shp") && !replace) {
  
    ## load the shapefile
    shape_nuts3 <- sf::st_read("./input/shapefile", "nuts3")
    
    return(shape_nuts3)
  } else {
  
    # create a directory
    dir.create("input/shapefile")
    
    shape_nuts3 <- eurostat::get_eurostat_geospatial(output_class="sf", resolution="1", nuts_level=3, year=2016) %>% 
      dplyr::rename("nuts3_id" = "NUTS_ID")
    names(shape_nuts3) <- tolower(names(shape_nuts3))
    # plot(sf::st_geometry(shape_nuts3))
    
    # is valid?
    any(!st_is_valid(shape_nuts3)) # no
    # now we buffer
    any(!st_is_valid(st_buffer(shape_nuts3, 0)))
    shape_nuts3 <- st_buffer(shape_nuts3, 0)
    
    # filter overseas territories
    overseas <- c("FRY10", "FRY20", "FRY30", "FRY40", "FRY50", "FRM01", "FRM02", 
                  "PT200", "PT300", "ES630", "ES640", "ES703", "ES704", "ES705", 
                  "ES706", "ES707", "ES708", "ES709", "ES531", "ES532", "ES533", 
                  "IS001", "IS002", "ITG1", "ITG25", "ITG26", "ITG27", "ITG28", 
                  "ITG29", "ITG2A", "ITG2B", "ITG2C", "EL431", "EL432", "EL433", 
                  "EL434", "EL411", "EL412", "EL413", "EL421", "EL422", "CY000")
    
    # shape_nuts3 <- shape_nuts3 %>% 
    #   dplyr::filter(!(substr(nuts3_id, 1, 4) %in% overseas))
    shape_nuts3 <- shape_nuts3 %>% 
      dplyr::filter(!nuts3_id %in% overseas)
    
    plot(sf::st_geometry(shape_nuts3))

    unlink("./input/shapefile/nuts3.shp")
    sf::st_write(shape_nuts3, "./input/shapefile/nuts3.shp")
    
    
    # re-project into an azimuthal projection (keeping area constant, hence using true area representation)
    shape_equalarea <- st_transform(shape_nuts3, "epsg:3035")
    shape_equalarea$area <- as.numeric(st_area(shape_equalarea))
    # st_crs(shape_equalarea)
    # area is in m^2
    
    # geometry needs to be dropped so that we can join the attributes to the original shapefile
    shape_equalarea <- st_drop_geometry(shape_equalarea)
    
    shape_nuts3 <- left_join(shape_nuts3, shape_equalarea)
    
    # # download the zip file with all zip files of the shapefiles
    # download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip",
    #               "input/shapefile/NUTS_2021_01M.zip")
    # 
    # # unzip the zip file with all the zipped shapefiles
    # unzip("input/shapefile/NUTS_2021_01M.zip", files = "NUTS_RG_01M_2021_4326_LEVL_3.shp.zip",
    #       exdir = "./input/shapefile")
    # 
    # # unzip the shapefile of interest with the following specifications
    # # RG = regions (multipolygons)
    # # 4326 = EPSG:4326 (WGS84, coordinates in decimal degrees)
    # # LEVL_3 = NUTS level 3
    # unzip("input/shapefile/NUTS_RG_01M_2021_4326_LEVL_3.shp.zip", exdir = "input/shapefile")
    # 
    # # load the shapefile and remove the zip file with all the zipped shapefiles
    # shape_nuts3 <- st_read("./input/shapefile", "NUTS_RG_01M_2021_4326_LEVL_3")
    # file.remove("input/shapefile/NUTS_2021_01M.zip")
  
    return(shape_nuts3)
  }
}

# getShapefile(replace = TRUE)
