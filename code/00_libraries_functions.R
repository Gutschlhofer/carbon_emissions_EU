# Load libraries
# install.packages("pacman")
library(pacman)

p_load(tidyverse)
p_load(eurostat)
p_load(stringr)
p_load(sf)
p_load(raster)
p_load(rgdal)
p_load(ncdf4)
p_load(foreach)
p_load(doMC)
p_load(ggplot2)
p_load(scales)
p_load(viridis)
p_load(spdep)
p_load(cartography)
p_load(summarytools)
p_load(stargazer)
p_load(ggpattern)
p_load(spgwr)
p_load(nngeo)
p_load(gridExtra)
p_load(cowplot)
# add new packages with p_load(packagename)


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
    
    shape_nuts3 <- shape_nuts3 %>% 
      dplyr::filter(!nuts3_id %in% overseas)
    
    # re-project into an azimuthal projection (keeping area constant, hence using true area representation)
    shape_equalarea <- st_transform(shape_nuts3, "epsg:3035")
    shape_equalarea$area <- as.numeric(st_area(shape_equalarea))
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
  
    unlink("./input/shapefile/nuts3.shp")
    sf::st_write(shape_nuts3, "./input/shapefile/nuts3.shp")
    
    return(shape_nuts3)
  }
}


# we need a nuts0 shapefile so that we can add country borders to the plot
# to exclude regions overseas, exclude them from nuts1 level 
shape_nuts1 <- eurostat::get_eurostat_geospatial(output_class="sf", resolution="1", nuts_level=1, year=2016) 
names(shape_nuts1) <- tolower(names(shape_nuts1))

# then aggregate based on ctnr_code (and exclude turkey, iceland and cyprus for nicer and more centred plots)
shape_nuts0 <- shape_nuts1 %>% 
  filter(!nuts_id %in% c("FRY", "PT2", "PT3", "ES7") & (cntr_code != "TR" & cntr_code != "IS" & cntr_code != "CY") ) %>% 
  count(cntr_code)

# this is needed for st_pattern 
shape_nuts0 <- st_remove_holes(shape_nuts0)


# add kosovo and bosnia to the shapefile
ba <- readRDS("./input/shapefile_ba_xk/gadm36_BIH_0_sf.rds") %>%
  rename(cntr_code = "GID_0",
         n = "NAME_0",
         geom = "geometry")
ba$cntr_code <- "BA"
ba$n <- 1

xk <- readRDS("./input/shapefile_ba_xk/gadm36_XKO_0_sf.rds") %>%
  rename(cntr_code = "GID_0",
         n = "NAME_0",
         geom = "geometry")
xk$cntr_code <- "XK"
xk$n <- 1

shape_nuts0 <- rbind(shape_nuts0, ba)
shape_nuts0 <- rbind(shape_nuts0, xk)

shape_nuts0 <- st_buffer(shape_nuts0, dist = 0)


