## this file downloads the edgar data-------------------------------------------
# we decided to use "CO2_excl_short-cycle_org_C"

if(! file.exists("./input/data_edgar.rds")) {
  
  # source("./code/00_libraries_functions.R")
  
  dir.create("input/edgar")
  
  # CO2------------------------------------------------------------------------
  
  dir.create("input/edgar/co2")
  
  download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=187465",
                "input/edgar/co2/v50_CO2_excl_short-cycle_org_C_TOTALS")
  
  unzip("input/edgar/co2/v50_CO2_excl_short-cycle_org_C_TOTALS", exdir = "./input/edgar/co2")
  
  fs <- list.files("input/edgar/co2", pattern = ".txt$", full.names = T)
  
  # only for 2000-2018
  fs <- fs[31:49]
  
  shape_nuts3 <- getShapefile()

  r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  
  # check if .nc files have the same projection as shapefile
  if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  
  
  data_edgar_co2 <- data.frame()
  
  registerDoMC(20)
  data_edgar_co2 <- foreach(f = fs) %dopar% {
    
    r <- read.delim(file = f, header = T, sep = ";", dec = ".", skip = 2) %>%
      dplyr::select(lon, lat, value = starts_with("emission")) %>%
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
    
    er <- raster::extract(r, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE)
    
    d <- er@data %>%
      dplyr::select(nuts3_id, value) %>%
      mutate(year = as.numeric(substr(f, 48, 51)), indicator = c("edgar_co2")) %>%
      dplyr::select(year, nuts3_id, value, indicator)
    
    return(d)
  }
  
  data_edgar_co2 <- do.call(rbind, data_edgar_co2)
  #saveRDS(data_edgar_co2, "input/data_edgar_co2.rds")
  
  
  
  # CH4------------------------------------------------------------------------
  
  dir.create("input/edgar/ch4")
  
  download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=185316",
                "input/edgar/ch4/v50_CH4_TOTALS")
  
  unzip("input/edgar/ch4/v50_CH4_TOTALS", exdir = "./input/edgar/ch4")
  
  fs <- list.files("input/edgar/ch4", pattern = ".txt$", full.names = T)
  
  # only for 2000-2015
  fs <- fs[31:46]
  
  r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  
  # check if .nc files have the same projection as shapefile
  if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  
  
  data_edgar_ch4 <- data.frame()
  
  registerDoMC(20)
  data_edgar_ch4 <- foreach(f = fs) %dopar% {
    
    r <- read.delim(file = f, header = T, sep = ";", dec = ".", skip = 2) %>%
      dplyr::select(lon, lat, value = starts_with("emission")) %>%
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
    
    er <- raster::extract(r, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE)
    
    d <- er@data %>%
      dplyr::select(nuts3_id, value) %>%
      mutate(year = as.numeric(substr(f, 25, 28)), indicator = c("edgar_ch4")) %>%
      dplyr::select(year, nuts3_id, value, indicator)
    
    return(d)
  }
  
  data_edgar_ch4 <- do.call(rbind, data_edgar_ch4)
  #saveRDS(data_edgar_ch4 , "input/data_edgar_ch4.rds")
  
  
  
  # N2O------------------------------------------------------------------------
  
  dir.create("input/edgar/n2o")
  
  download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=183085",
                "input/edgar/n2o/v50_N2O_TOTALS")
  
  unzip("input/edgar/n2o/v50_N2O_TOTALS", exdir = "./input/edgar/n2o")
  
  fs <- list.files("input/edgar/n2o", pattern = ".txt$", full.names = T)
  
  # only for 2000-2015
  fs <- fs[31:46]
  
  r <- read.delim(file = fs[1], header = T, sep = ";", dec = ".", skip = 2) %>%
    dplyr::select(lon, lat, value = starts_with("emission")) %>%
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") #not sure about the datum but projection should be correct
  
  # check if .nc files have the same projection as shapefile
  if(!compareCRS(shape_nuts3, r)) print("Warning: shape and nc file have different CRS!")
  
  
  data_edgar_n2o <- data.frame()
  
  registerDoMC(20)
  data_edgar_n2o <- foreach(f = fs) %dopar% {
    
    r <- read.delim(file = f, header = T, sep = ";", dec = ".", skip = 2) %>%
      dplyr::select(lon, lat, value = starts_with("emission")) %>%
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
    
    er <- raster::extract(r, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE)
    
    d <- er@data %>%
      dplyr::select(nuts3_id, value) %>%
      mutate(year = as.numeric(substr(f, 25, 28)), indicator = c("edgar_n2o")) %>%
      dplyr::select(year, nuts3_id, value, indicator)
    
    return(d)
  }
  
  data_edgar_n2o <- do.call(rbind, data_edgar_n2o)
  #saveRDS(data_edgar_n2o , "input/data_edgar_n2o.rds")
  
  
  # rbind all
  data_edgar <- rbind(data_edgar_co2, data_edgar_ch4, data_edgar_n2o)
  saveRDS(data_edgar , "input/data_edgar.rds")
}

