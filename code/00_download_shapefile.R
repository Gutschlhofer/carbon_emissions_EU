## eurostat NUTS 3 shapefile----------------------------------------------------

if(file.exists("./input/shapefile/NUTS_RG_01M_2021_4326_LEVL_3.shp")) {
  
  ## load the shapefile
  shape_nuts3 <- st_read("./input/shapefile", "NUTS_RG_01M_2021_4326_LEVL_3")
  
} else {
  
  # create a directory
  dir.create("input/shapefile")
  
  # download the zip file with all zip files of the shapefiles
  download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip",
                "input/shapefile/NUTS_2021_01M.zip")
  
  # unzip the zip file with all the zipped shapefiles
  unzip("input/shapefile/NUTS_2021_01M.zip", files = "NUTS_RG_01M_2021_4326_LEVL_3.shp.zip",
        exdir = "./input/shapefile")
  
  # unzip the shapefile of interest with the following specifications
  # RG = regions (multipolygons)
  # 4326 = EPSG:4326 (WGS84, coordinates in decimal degrees)
  # LEVL_3 = NUTS level 3
  unzip("input/shapefile/NUTS_RG_01M_2021_4326_LEVL_3.shp.zip", exdir = "input/shapefile")
  
  # load the shapefile and remove the zip file with all the zipped shapefiles
  shape_nuts3 <- st_read("./input/shapefile", "NUTS_RG_01M_2021_4326_LEVL_3")
  file.remove("input/shapefile/NUTS_2021_01M.zip")
  
}


