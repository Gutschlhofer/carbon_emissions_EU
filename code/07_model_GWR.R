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

# 1. Prepare data for GWR ------------------------------------------------------

# data_coords <- st_coordinates(st_point_on_surface(data$geometry))
data_coords <- st_coordinates(st_centroid(data$geometry))

# make spatial data frame
spdf = SpatialPointsDataFrame(data_coords, 
                              st_drop_geometry(data), 
                              proj4string = CRS("+proj=longlat +datum=WGS84"))

# NOTE: maybe WGS84 is not ideal?

# 2. bandwidth function, run GWR -----------------------------------------------
bw_fct <- c("bisq","gauss","tricube")[1]

if(bw_fct == "bisq"){
  # find best neighbour specification
  # select bandwidth, here Videras (2014) uses >>gwr.bisquare<<
  bw <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE,
                method = "aic")
  
  gwr <- gwr(model_base, data = spdf, # data = data, coords = data_coords, 
             bandwidth = bw, gweight = gwr.bisquare,
             longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
  
} else if(bw_fct == "gauss"){
  bw <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                gweight = gwr.Gauss, longlat = TRUE, verbose = FALSE,
                method = "aic")
  
  gwr <- gwr(model_base, data = spdf, # data = data, coords = data_coords, 
             bandwidth = bw, gweight = gwr.Gauss,
             longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
} else {
  bw <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                gweight = gwr.tricube, longlat = TRUE, verbose = FALSE,
                method = "aic")
  
  gwr <- gwr(model_base, data = spdf, # data = data, coords = data_coords, 
             bandwidth = bw, gweight = gwr.tricube,
             longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
}

# our bisq bw is 969, which basically includes all observations (969/1122)
# bw <- 285 # or 150 or 450
# bw <- bwGauss
# bw <- 150

# 3. Calculate p-values for estimators -----------------------------------------

gwr_output <- gwr$SDF %>% as.data.frame()

vars <- names(gwr_output)[substr(names(gwr_output), nchar(names(gwr_output))-2, nchar(names(gwr_output))) == "_se"]

p_vec <- c()
sig_vec <- c()

for(v in vars){
  se <- v
  x <- substr(v, 1, nchar(v)-3)
  p <- paste0(x, "_p")
  p_vec <- c(p_vec, p)
  
  t.stat<-(gwr_output[,x]-0)/gwr_output[,se] # calculate t-statistic (substract H0 beta)
  pval.t<-2*pt(-abs(t.stat), df=9) #calculate pvalue (assume normal residuals)
  # print(pval.t)
  gwr_output[,p] <- pval.t
  
  gwr_output[,paste0(p,"_sig")] <- ifelse(pval.t < 0.05, "p < 0.05", "n.sig")
}

sig_vec <- paste0(p_vec, "_sig")


# 4. Visualise -----------------------------------------------------------------
coef <- st_as_sf(gwr$SDF)
summary(coef)

coef$id <- data$nuts3_id
st_crs(coef) <- st_crs(data)

data_coef <- cbind(data,st_drop_geometry(coef),gwr_output[,all_of(c(p_vec, sig_vec))])

ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.gdppc.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`I.log.gdppc..2.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.density.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.gwa_share_BE.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.hdd.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.cdd_fix.`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`localR2`))

boxplot(data_coef$localR2)

ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.gdppc._p`))


breaks <- c( min(data_coef$log.gdppc._p), 0.001, 0.1, 0.5, max(data_coef$log.gdppc._p) )
data_coef$gdppc_bins <- cut(data_coef$log.gdppc._p, 
                            breaks = breaks, 
                            #  labels = c("<0.001", "0.001-0.1", "0.1-0.5", ">0.5"),
                            include.lowest = TRUE , 
                            include.highest = TRUE , 
                            right = TRUE )
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=gdppc_bins))


ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`log.gdppc._p_sig`))
ggplot() + geom_sf(data=data_coef, fill=NA) + geom_sf(data=data_coef, aes(fill=`I.log.gdppc..2._p_sig`))

# 5. Tests ---------------------------------------------------------------------

# bw neighbours
lw <- knearneigh(data_coords, k=round(bw)) %>% 
  knn2nb() %>% 
  nb2listw()
gwr.morantest(gwr, lw = lw)
# 100 neighbours
lw <- knearneigh(data_coords, k=100) %>% 
  knn2nb() %>% 
  nb2listw()
gwr.morantest(gwr, lw = lw)
# 20 neighbours
lw <- knearneigh(data_coords, k=20) %>% 
  knn2nb() %>% 
  nb2listw()
gwr.morantest(gwr, lw = lw)

# library(GWmodel)
# 
# bw <- GWmodel::bw.gwr(formula = model_base, data = spdf,
#                       approach = "AIC", longlat=T)


