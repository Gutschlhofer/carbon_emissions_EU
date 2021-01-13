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

# choose one of the methods
# either set adapt manually (no gwr.sel is run or via code (NULL))
run_gwr <- function(method = c("bisq","gauss","tricube"),
                    adapt = NULL,
                    file_name_add = ""){
  # 2. bandwidth function, run GWR -----------------------------------------------
  # bw_fct <- c("bisq","gauss","tricube")[1]
  bw_fct <- method
  
  if(bw_fct == "bisq"){
    # bw_old <- gwr.sel(model_base, data = spdf,# coords = data_coords,
    #                   gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE)#,
    # #method = "aic") # AIC does not work for bisq
    # gwr_old <- gwr(model_base, data = spdf, # data = data, coords = data_coords,
    #                bandwidth = bw_old, gweight = gwr.bisquare,
    #                longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
    
    # find best neighbour specification
    # select bandwidth, here Videras (2014) uses >>gwr.bisquare<<
    bw <- ifelse(is.null(adapt),
                 gwr.sel(model_base, data = spdf,# coords = data_coords, 
                          gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE, adapt = TRUE
                          ,method = "aic"),
                 adapt)
    # 0.203187
    
    gwr <- gwr(model_base, data = spdf, # data = data, coords = data_coords, 
               adapt = bw, gweight = gwr.bisquare,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
    
  } else if(bw_fct == "gauss"){
    # TODO adjust to function
    bw <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                  gweight = gwr.Gauss, longlat = TRUE, verbose = FALSE,
                  method = "aic")
    
    gwr <- gwr(model_base, data = spdf, # data = data, coords = data_coords, 
               bandwidth = bw, gweight = gwr.Gauss,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
  } else {
    # TODO adjust to function
    bw <- gwr.sel(model_base, data = spdf,# coords = data_coords, 
                  gweight = gwr.tricube, longlat = TRUE, verbose = FALSE)#,
    #method = "aic") # AIC does not work for tricube
    
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
    
    n <- round(gwr$adapt * nrow(data))
    k <- 8 # 7 x + 1 intercept
    
    t.stat<-(gwr_output[,x]-0)/gwr_output[,se] # calculate t-statistic (substract H0 beta)
    pval.t<-2*pt(-abs(t.stat), df=(n-k)) #calculate pvalue (assume normal residuals)
    ## ERROR: actually n-k df
    # print(pval.t)
    gwr_output[,p] <- pval.t
    
    gwr_output[,paste0(p,"_sig")] <- ifelse(gwr_output[,x] > 0 & pval.t < 0.05, "pos, p < 0.05", 
                                            ifelse(gwr_output[,x] < 0 & pval.t < 0.05, "neg, p < 0.05",
                                                   "n.sig"))
  }
  
  sig_vec <- paste0(p_vec, "_sig")
  
  coef <- st_as_sf(gwr$SDF)
  summary(coef)
  
  coef$id <- data$nuts3_id
  st_crs(coef) <- st_crs(data)
  
  data_coef <- cbind(data,st_drop_geometry(coef),gwr_output[,all_of(c(p_vec, sig_vec))])
  
  file_name_gwr <- paste0("input/gwr",file_name_add, ".rds")
  file_name <- paste0("input/data_coef",file_name_add, ".rds")
  
  saveRDS(gwr, file_name_gwr)
  saveRDS(data_coef, file_name)
  # data_coef <- readRDS(file_name)
}

# run_gwr(method = "bisq")
run_gwr(method = "bisq", adapt = 0.203187) # result of bisq AIC
run_gwr(method = "bisq", adapt = .1, file_name_add = "_lessneighbours")
run_gwr(method = "bisq", adapt = .3, file_name_add = "_moreneighbours")

# 4. Visualise -----------------------------------------------------------------

theme_set(theme_minimal())

# log.gdppc.
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.gdppc.), color = "white", size=0.01) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# I.log.gdppc..2.
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = I.log.gdppc..2.), color = "white", size=0.01) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.density.
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.density.), color = "white", size=0.01) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# gwa_share_BE
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = gwa_share_BE), color = "white", size=0.01) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.hdd.
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.hdd.), color = "white", size=0.01) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.cdd_fix.
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.cdd_fix.), color = "white", size=0.01) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)


boxplot(data_coef$localR2)

# localR2
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = localR2), color = "white", size=0.01) + 
  scale_fill_viridis_c(option = "magma", direction = -1, labels = scales::percent_format(accuracy = 1)) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)



# significance plots -----------------------------------------------------------
# log.pop._p_sig
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.pop._p_sig), color = "white", size=0.01) + 
  scale_fill_manual(values = rev(magma(4)[2:4])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.density._p_sig
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.density._p_sig), color = "white", size=0.01) + 
  scale_fill_manual(values = rev(magma(4)[2:4])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.gdppc._p_sig
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.gdppc._p_sig), color = "white", size=0.01) + 
  scale_fill_manual(values = rev(magma(4)[2:4])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# I.log.gdppc..2._p_sig
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = I.log.gdppc..2._p_sig), color = "white", size=0.01) + 
  scale_fill_manual(values = rev(magma(4)[2:4])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.gwa_share_BE._p_sig
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.gwa_share_BE._p_sig), color = "white", size=0.01) + 
  scale_fill_manual(values = rev(magma(4)[2:4])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.hdd._p_sig
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.hdd._p_sig), color = "white", size=0.01) + 
  scale_fill_manual(values = rev(magma(4)[2:4])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# log.cdd_fix._p_sig
ggplot(data = data_coef) +
  geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = log.cdd_fix._p_sig), color = "white", size=0.01) + 
  scale_fill_manual(values = rev(magma(4)[2:4])) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)



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


