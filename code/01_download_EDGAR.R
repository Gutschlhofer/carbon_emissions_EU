## this file downloads the edgar data-------------------------------------------
# we decided to use "CO2_excl_short-cycle_org_C"

if(! file.exists("./input/data_edgar.rds")) {
  
  source("./code/00_libraries_functions.R")
  
  dir.create("input/EDGAR")
  
  download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=187464",
                "input/EDGAR/CO2_excl_short-cycle_org_C")
  
  unzip("input/EDGAR/CO2_excl_short-cycle_org_C", exdir = "./input/EDGAR")
  
  fs <- list.files("input/EDGAR", pattern = ".nc$", full.names = T)
  
  # only for 2000-2018
  fs <- fs[31:49]
  
  shape_nuts3 <- getShapefile()
  #check if .nc files have the same projection as shapefile
  crs(shape_nuts3)
  crs(brick(fs[1])) # looks good
  
  data_edgar <- data.frame()
  
  registerDoMC(20)
  data_edgar <- foreach(f = fs) %dopar% {
    
    b <- brick(f)
    p <- b$layer 
    ep <- raster::extract(p, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE)
    
    d <- ep@data %>%
      dplyr::select(nuts3_id, value = layer) %>%
      mutate(year = as.numeric(substr(f, 44, 47)), indicator = c("edgar")) %>%
      dplyr::select(year, nuts3_id, value, indicator)
    
    return(d)
  }
  
  data_edgar <- do.call(rbind, data_edgar)
  saveRDS(data_edgar, "input/data_edgar.rds")
  
}

