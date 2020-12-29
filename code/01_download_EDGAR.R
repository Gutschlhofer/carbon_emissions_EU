## this file downloads the edgar data-------------------------------------------
# we decided to use "CO2_excl_short-cycle_org_C"

if(! file.exists("./input/data_edgar.rds")) {
  
  source("./code/00_libraries_functions.R")
  
  dir.create("input/EDGAR")
  
  download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=187465",
                "input/EDGAR/CO2_excl_short-cycle_org_C")
  
  unzip("input/EDGAR/CO2_excl_short-cycle_org_C", exdir = "./input/EDGAR")
  
  fs <- list.files("input/EDGAR", pattern = ".txt$", full.names = T)
  
  # only for 2000-2018
  fs <- fs[31:49]
  
  shape_nuts3 <- getShapefile()

  r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  
  # check if .nc files have the same projection as shapefile
  if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  
  
  data_edgar <- data.frame()
  
  registerDoMC(20)
  data_edgar <- foreach(f = fs) %dopar% {
    
    r <- read.delim(file = f, header = T, sep = ";", dec = ".", skip = 2) %>%
      dplyr::select(lon, lat, value = starts_with("emission")) %>%
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
    
    er <- raster::extract(r, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE)
    
    d <- er@data %>%
      dplyr::select(nuts3_id, value) %>%
      mutate(year = as.numeric(substr(f, 44, 47)), indicator = c("edgar")) %>%
      dplyr::select(year, nuts3_id, value, indicator)
    
    return(d)
  }
  
  data_edgar <- do.call(rbind, data_edgar)
  saveRDS(data_edgar, "input/data_edgar.rds")
  
}