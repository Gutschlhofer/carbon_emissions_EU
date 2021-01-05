# Libraries
library(spgwr)

# # alternative
# library(GWmodel)
# bw.ggwr
# bw.gwr
# gwr.basic
# gwr.robust

source("code/00_libraries_functions.R")
source("code/06_model_OLS.R")

# data <- st_cast(data, "POLYGON")
data_coords <- st_coordinates(st_point_on_surface(data$geometry))
# colnames(data_coords) <- c("Latitude", "Longitud")

# model_base
# data <- data %>% dplyr::select(edgar, pop, density, gdppc, gwa_share_BE, )

# points from scratch
# coords = cbind(x, y)
# sp = SpatialPoints(data_coords)
# make spatial data frame
spdf = SpatialPointsDataFrame(data_coords, 
                              st_drop_geometry(data), 
                              proj4string = CRS("+proj=longlat +datum=WGS84"))

# NOTE: maybe WGS84 is not ideal?

# find best neighbour specification
# select bandwidth, here Videras (2014) uses >>gwr.bisquare<<
bwBisq <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                  gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE) #, adapt = T)# verbose = TRUE, 
bwGauss <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                   gweight = gwr.Gauss, longlat = TRUE, verbose = FALSE) #, adapt = T)# verbose = TRUE, 
bwTri <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                 gweight = gwr.tricube, longlat = TRUE, verbose = FALSE) #, adapt = T)# verbose = TRUE, 

bwG2 <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE, adapt = T)# verbose = TRUE, 

# our bw is 969, which basically includes all observations (969/1122)
# bw <- 285 # or 150 or 450
bw <- bwGauss
bw <- 150

gwr <- gwr(model_base, data = data, coords = data_coords, 
           bandwidth = bw, gweight = gwr.bisquare) #, hatmatrix = TRUE) #, se.fit = TRUE)

gwr

coef <- st_as_sf(gwrG$SDF)
coef$id <- data$nuts3_id
st_crs(coef) <- st_crs(data)

data_coef <- cbind(data,st_drop_geometry(coef))

ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.gdppc.`))

# lw = nb2listw()
gwr.morantest(gwrG1v2, lw = lw)

