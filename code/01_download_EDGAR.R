## this file downloads the edgar data-------------------------------------------
# we decided to use "CO2_excl_short-cycle_org_C"
# we still need to discuss what excl short cycle means


if(! file.exists("./input/data_edgar.rds")) {
  
  dir.create("input/EDGAR")
  
  download.file("https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=187464",
                "input/EDGAR/CO2_excl_short-cycle_org_C")
  
  unzip("input/EDGAR/CO2_excl_short-cycle_org_C", exdir = "./input/EDGAR")
  
  fs <- list.files("input/EDGAR", pattern = ".nc$", full.names = T)
  
  # only from 2000-2018
  # fs <- fs[31:49]
  
  registerDoMC(49)
  
  shape_nuts3 <- getShapefile()
  data_edgar <- data.frame()
  data_edgar <- foreach(f = fs) %dopar% {
    
    b <- brick(f)
    p <- b$layer 
    ep <- raster::extract(p, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE)
    
    d <- ep@data %>%
      select(nuts3_id, value = layer) %>%
      mutate(year = as.numeric(substr(f, 44, 47)), indicator = c("edgar")) %>%
      select(year, nuts3_id, value, indicator)
    
    return(d)
  }
  
  data_edgar <- do.call(rbind, data_edgar)
  saveRDS(data_edgar, "input/data_edgar.rds")
  
}

