# sample code for data extraction

# library(raster)
# x <- raster::extract(input data, shape file, method='simple', fun=sum, na.rm=T, sp=TRUE, df=TRUE)

source("./code/00_libraries_functions.R")

# 4x4 temperature grid
if(! file.exists("./input/temperature4x4/TerraClimate_aet_1958.nc")) {
  
  dir.create("input/temperature4x4")
  
  variables <- c(
    "Actual Evapotranspiration" = "aet",
    "Climate Water Deficit" = "def",
    "Potential evapotranspiration" = "pet",
    "Precipitation" = "ppt",
    "Runoff" = "q",
    "Soil Moisture" = "soil",
    "Downward surface shortwave radiation" = "srad",
    "Snow water equivalent - at end of month" = "swe",
    "Max Temperature" = "tmax",
    "Min Temperature" = "tmin",
    "Vapor pressure" = "vap",
    "Wind speed" = "ws",
    "Vapor Pressure Deficit" = "vpd",
    "Palmer Drought Severity Index" = "PDSI"
  )
  
  years <- c(1958:2019)
  
  for(v in variables){
    for(y in years){
      file_name <- paste0("TerraClimate_",v,"_",y,".nc")
      
      download.file(paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/", file_name),
                    paste0("input/temperature4x4/",file_name))
    }
  }
}

if(! file.exists("./input/data_temperature.rds")) {
  
  shape_nuts3 <- getShapefile()
  
  registerDoMC(62/2)
  
  vs <- c("tmin", "tmax")
  ys <- c(2016,2018) # c(2000:2019)
  
  for(v in vs) {
    fs <- paste0("input/temperature4x4/TerraClimate_",v,"_",ys,".nc")
    
    data_temp <- data.frame()
    data_temp <- foreach(f = fs) %dopar% {
      
      b <- brick(f)
      p <- b # b$layer 
      ep <- raster::extract(p, shape_nuts3, method = "simple", fun = mean, na.rm = TRUE, df = TRUE, sp = TRUE)
      
      d <- ep@data %>%
        select(nuts3_id, value = layer) %>%
        mutate(year = as.numeric(substr(f, 44, 47)), indicator = c("edgar")) %>%
        select(year, nuts3_id, value, indicator)
      
      return(d)
    }
    
    data_temp <- do.call(rbind, data_temp)
    saveRDS(data_temp, sprintf("input/data_temperature_%s.rds", v))
  }
  rm(data_temp)
  
}

if(! file.exists("./input/data_heating_cooling.rds")) {
  # heating and cooling days
  data_heating_cooling <- read.csv("input/temp_heating_cooling/test_ver2020-1_0_18494_868222495.csv", sep = ";") %>% 
    dplyr::filter(nchar(NUTS_CODE) == 5) %>% 
    pivot_longer(cols = c("CDD", "HDD"), names_to = "indicator") %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::rename(nuts3_id = nuts_code) %>% 
    dplyr::group_by(nuts3_id, year, indicator) %>% 
    dplyr::summarize(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  # save
  saveRDS(data_heating_cooling, "./input/data_heating_cooling.rds")
}