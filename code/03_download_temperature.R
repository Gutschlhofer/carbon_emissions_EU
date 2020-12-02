# sample code for data extraction

# library(raster)
# x <- raster::extract(input data, shape file, method='simple', fun=sum, na.rm=T, sp=TRUE, df=TRUE)

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