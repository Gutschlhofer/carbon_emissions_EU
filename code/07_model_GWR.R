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
data_coords <- st_coordinates(st_point_on_surface(data$geometry)) # TODO: CENTROID??
# colnames(data_coords) <- c("Latitude", "Longitud")

data_coords <- st_coordinates(st_centroid(data$geometry))

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
                  gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE)#,
                  # method = "aic") #, adapt = T)# verbose = TRUE, 
bwGauss <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                   gweight = gwr.Gauss, longlat = TRUE, verbose = FALSE) #, adapt = T)# verbose = TRUE, 
bwTri <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                 gweight = gwr.tricube, longlat = TRUE, verbose = FALSE) #, adapt = T)# verbose = TRUE, 

bwG2 <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE, adapt = T)# verbose = TRUE, 

# our bisq bw is 969, which basically includes all observations (969/1122)
# bw <- 285 # or 150 or 450
bw <- bwGauss

bw <- 150
# bw <- 10
bw <- bwBisq

# change gwr.Gauss/.bisquare etc
# why data and not spdf??
gwr <- gwr(model_base, data = spdf, # data = data, coords = data_coords, 
           bandwidth = bw, gweight = gwr.Gauss,
           longlat = TRUE) #, hatmatrix = TRUE) #, se.fit = TRUE)

gwr

coef <- st_as_sf(gwr$SDF)
summary(coef)

coef$id <- data$nuts3_id
st_crs(coef) <- st_crs(data)

data_coef <- cbind(data,st_drop_geometry(coef))

ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.gdppc.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.density.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.gwa_share_BE.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.hdd.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.cdd_fix.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`localR2`))


# lw = nb2listw()
gwr.morantest(gwrG1v2, lw = lw)


data(columbus, package="spData")
bw <- gwr.sel(CRIME ~ INC + HOVAL, data=columbus, coords=coords)
col0 <- gwr(CRIME ~ INC + HOVAL, data=columbus, coords=coords,
            bandwidth=bw, hatmatrix=TRUE)
gwr.morantest(col0, nb2listw(col.gal.nb))

library(GWmodel)

bw <- GWmodel::bw.gwr(formula = model_base, data = spdf,
                      approach = "AIC", longlat=T)


