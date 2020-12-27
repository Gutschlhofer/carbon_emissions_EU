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
  # check if .nc files have the same projection as shapefile
  if(!compareCRS(shape_nuts3, brick(fs[1]))) print("Warning: shape and nc file have different CRS!")
  
  x <- brick(fs[19])$layer
  raster::spplot(brick(fs[1])$layer)
  plot(brick(fs[1])$layer)#, xlim=c(-9.507305e-10, 360),ylim=c(-90, 90))
  
  # r <- raster("nc/oisst-sst.nc")
  library(leaflet)
  pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(x),
                      na.color = "transparent")
  
  leaflet() %>% addTiles() %>%
    addRasterImage(x, colors = pal, opacity = 0.8) %>%
    addLegend(pal = pal, values = values(x),
              title = "EDGAR stuffz")
  # issue: data only from 0-meridian onwards
  # therefore we lose UK, IE, parts of FR and ES, PT
  
  library(raster)
  library(fasterize)
  shp_raster <- st_transform(shp, old_crs)
  shp_raster$random <- runif(nrow(shp_raster))
  r <- raster(ncol=600, nrow=600)
  ras <- fasterize(shp_raster, r, field = shp$random, fun = "sum")
  plot(ras, xlim=c(-30,50),ylim=c(20,80))
  
  
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

