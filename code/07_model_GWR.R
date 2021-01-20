# # alternative
# library(GWmodel)
# bw.ggwr
# bw.gwr
# gwr.basic
# gwr.robust

# source("code/00_libraries_functions.R")
# source("code/06_model_OLS.R")

# 1. Prepare data for GWR ------------------------------------------------------

# data <- data_fix_outlier

# choose one of the methods
# either set adapt manually (no gwr.sel is run or via code (NULL))
run_gwr <- function(data,
                    method = c("bisq","gauss","tricube"),
                    adapt = NULL,
                    file_name_add = "",
                    model = model_base){
  
  # data_coords <- st_coordinates(st_point_on_surface(data$geometry))
  data_coords <- st_coordinates(st_centroid(data$geometry))
  
  # make spatial data frame
  spdf = SpatialPointsDataFrame(data_coords, 
                                st_drop_geometry(data), 
                                proj4string = CRS("+proj=longlat +datum=WGS84"))
  
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
                 gwr.sel(model, data = spdf,# coords = data_coords, 
                          gweight = gwr.bisquare, longlat = TRUE, verbose = FALSE, adapt = TRUE
                          ,method = "aic"),
                 adapt)
    # 0.203187
    # 0.23975778 for data_fix_outlier
    
    gwr <- gwr(model, data = spdf,
               adapt = bw, gweight = gwr.bisquare,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
    
  } else if(bw_fct == "gauss"){
    bw <- gwr.sel(model, data = spdf,
                  gweight = gwr.Gauss, longlat = TRUE, verbose = FALSE,
                  adapt = TRUE, method = "aic")
    
    gwr <- gwr(model, data = spdf,
               adapt = bw, gweight = gwr.Gauss,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
  } else {
    bw <- gwr.sel(model, data = spdf, 
                  gweight = gwr.tricube, longlat = TRUE, verbose = FALSE,
                  adapt = TRUE, method = "aic")
    
    gwr <- gwr(model, data = spdf,
               adapt = bw, gweight = gwr.tricube,
               longlat = TRUE, se.fit = TRUE, hatmatrix = TRUE)
  }
  
  print(paste("adapt Bandwidth:",bw))
  
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
    pval.t<-2*pt(-abs(t.stat), df=(n-k)) #calculate p-value (assume normal residuals)
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
  
  turning_point <- exp(-data_coef$log.gdppc./(2*data_coef$I.log.gdppc..2.))
  
  cat(summary(turning_point), file = paste0("output/tables/turning_point",file_name_add,".txt"))
  
  # save the files
  file_name_gwr <- paste0("input/gwr",file_name_add, ".rds")
  file_name <- paste0("input/data_coef",file_name_add, ".rds")
  saveRDS(gwr, file_name_gwr)
  saveRDS(data_coef, file_name)
  
  # 4. Visualise -----------------------------------------------------------------
  
  plot_path <- "output/plots/"
  plot_template <- paste0("gwr_%s",file_name_add,".png")
  
  theme_set(theme_minimal())
  
  # log.pop
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.pop.), color = "white", size=0.01) + 
    scale_fill_viridis_c(option = "magma", direction = -1) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_Pop"))
  
  # log.gdppc.
  gwr_gdp <- ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.gdppc.), color = "white", size=0.01) + 
    scale_fill_viridis_c(option = "magma", direction = -1) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc"))
  
  # I.log.gdppc..2.
  gwr_gdp2 <- ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = I.log.gdppc..2.), color = "white", size=0.01) + 
    scale_fill_viridis_c(option = "magma", direction = -1) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc2"))
  
  GDP_GDP2 <- plot_grid(gwr_gdp, gwr_gdp2, ncol = 2)
  if(s) ggsave(plot = GDP_GDP2, path = plot_path, filename = sprintf(plot_template, "log_GDPpc_GDPpc2")) 
  # ggsave(plot = GDP_GDP2, path = plot_path, filename = "gwr_log_GDPpc_GDPpc2.png") 

  # log.density.
  if(!is.null(data_coef$log.density.)){
    ggplot(data = data_coef) +
      geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                      pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                      pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
      geom_sf(aes(fill = log.density.), color = "white", size=0.01) + 
      scale_fill_viridis_c(option = "magma", direction = -1) +  
      theme(legend.title = element_blank()) +
      geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
    ggsave(path = plot_path, filename = sprintf(plot_template, "log_Density"))
  }
  
  # gwa_share_BE
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = gwa_share_BE), color = "white", size=0.01) + 
    scale_fill_viridis_c(option = "magma", direction = -1) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "GWA_share"))
  
  # log.hdd.
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.hdd.), color = "white", size=0.01) + 
    scale_fill_viridis_c(option = "magma", direction = -1) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_HDD"))
  
  # log.cdd_fix.
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.cdd_fix.), color = "white", size=0.01) + 
    scale_fill_viridis_c(option = "magma", direction = -1) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_CDD_fix"))
  
  # localR2
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = localR2), color = "white", size=0.01) + 
    scale_fill_viridis_c(option = "magma", direction = -1, labels = scales::percent_format(accuracy = 1)) +  
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "local_R2"))
  
  
  # significance plots -----------------------------------------------------------
  plot_template <- paste0("gwr_%s",file_name_add,"_signif.png")
  
  # log.pop._p_sig
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.pop._p_sig), color = "white", size=0.01) + 
    scale_fill_manual(values = rev(magma(4)[2:4])) +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_Pop"))
  
  # log.density._p_sig
  if(!is.null(data_coef$log.density._p_sig)){
    ggplot(data = data_coef) +
      geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                      pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                      pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
      geom_sf(aes(fill = log.density._p_sig), color = "white", size=0.01) + 
      scale_fill_manual(values = rev(magma(4)[2:4])) +
      theme(legend.title = element_blank()) +
      geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
    ggsave(path = plot_path, filename = sprintf(plot_template, "log_Density"))
  }
    
  # log.gdppc._p_sig
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.gdppc._p_sig), color = "white", size=0.01) + 
    scale_fill_manual(values = rev(magma(4)[2:4])) +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc"))
  
  # I.log.gdppc..2._p_sig
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = I.log.gdppc..2._p_sig), color = "white", size=0.01) + 
    scale_fill_manual(values = rev(magma(4)[2:4])) +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_GDPpc2"))
  
  # log.gwa_share_BE._p_sig
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = gwa_share_BE_p_sig), color = "white", size=0.01) + 
    scale_fill_manual(values = rev(magma(4)[2:4])) +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "GWA_share"))
  
  # log.hdd._p_sig
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.hdd._p_sig), color = "white", size=0.01) + 
    scale_fill_manual(values = rev(magma(4)[2:4])) +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_HDD"))
  
  # log.cdd_fix._p_sig
  ggplot(data = data_coef) +
    geom_sf_pattern(data = shape_nuts0, colour = 'black', fill = 'white', pattern = 'stripe',                    
                    pattern_size = 0.5, pattern_linetype = 1, pattern_spacing = 0.008,                    
                    pattern_fill = "white", pattern_density = 0.1, pattern_alpha = 0.7) + 
    geom_sf(aes(fill = log.cdd_fix._p_sig), color = "white", size=0.01) + 
    scale_fill_manual(values = rev(magma(4)[2:4])) +
    theme(legend.title = element_blank()) +
    geom_sf(data=shape_nuts0, color='#000000', fill=NA, size=0.1)
  ggsave(path = plot_path, filename = sprintf(plot_template, "log_CDD_fix"))
  
  
  # 5. Tests ---------------------------------------------------------------------
  
  ### knn
  k <- round(bw * nrow(data)) # use amount of neighbours determined by gwr_sel
  lw_knn <- knearneigh(data_coords, k=k) %>% 
    knn2nb()
  
  ### inverse distance (based on knn)
  dlist <- nbdists(lw_knn, data_coords, longlat = TRUE)
  idlist <- lapply(dlist, function(x) 1/x)
  lw_inversedist <- nb2listw(lw_knn, glist=idlist, style="W")
  m <- listw2mat(lw_inversedist)
  
  ### inverse distance (for all)
  lw_d <- dnearneigh(data_coords, 0, Inf, longlat = TRUE)
  dlist <- nbdists(lw_d, data_coords, longlat = TRUE)
  idlist <- lapply(dlist, function(x) 1/x)
  lw_inversedist_all <- nb2listw(lw_d, glist=idlist, style="W")
  m_all <- listw2mat(lw_inversedist_all)
  
  ### convert all to lw objects
  lw_knn <- lw_knn %>% nb2listw()
  lw_d <- lw_d %>% nb2listw()

  moran_knn <- gwr.morantest(gwr, lw = lw_knn)
  
  moran_id <- gwr.morantest(gwr, lw = lw_inversedist)
  
  moran_id_all <- gwr.morantest(gwr, lw = lw_inversedist_all)
  
  print(gwr)
  print(moran_knn)
  print(moran_id)
  print(moran_id_all)
}

run_gwr(data, method = "bisq", adapt = 0.203186957155516) # result of bisq AIC
run_gwr(data, method = "bisq", adapt = .1, file_name_add = "_lessneighbours")
run_gwr(data, method = "bisq", adapt = .3, file_name_add = "_moreneighbours")

# treat outliers
run_gwr(data_fix_outlier, method = "bisq", adapt = 0.188065182679875, file_name_add = "_fixoutlier")
run_gwr(data_filter_outlier, method = "bisq", adapt = 0.188077476917333, file_name_add = "_filteroutlier")

# try without density
run_gwr(data, method = "bisq", model = model_base_no_density, adapt = 0.0793237242334166, file_name_add = "_nodensity")
run_gwr(data_fix_outlier, method = "bisq", model = model_base_no_density, adapt = 0.0909189589985161, file_name_add = "_nodensity_fixoutlier")

# try NUTS2 (MAUP)
run_gwr(data_nuts2, method = "bisq", file_name_add = "_nuts2")

gwr <- readRDS("input/gwr.rds")
gwr_lessnb <- readRDS("input/gwr_lessneighbours.rds")
gwr_morenb <- readRDS("input/gwr_moreneighbours.rds")

data_coef <- readRDS("input/data_coef.rds")
# data_coef <- readRDS("input/data_coef_lessneighbours.rds")
# data_coef <- readRDS("input/data_coef_moreneighbours.rds")


# library(GWmodel)
# 
# bw <- GWmodel::bw.gwr(formula = model_base, data = spdf,
#                       approach = "AIC", longlat=T)

