# load data
# data <- readRDS("input/data.rds")

# I=PAT
model_base <- log(edgar) ~ log(pop) + log(density) + log(gdppc) + 
  I(log(gdppc)^2) + gwa_share_BE +
  log(hdd) + log(cdd_fix)
ols_base <- lm(model_base, data)
summary(ols_base)

model_cntr <- log(edgar) ~ log(pop) + log(density) + log(gdppc) + 
  I(log(gdppc)^2) + gwa_share_BE + 
  log(hdd) + log(cdd_fix) + cntr_code
ols_cntr <- lm(model_cntr, data)
summary(ols_cntr)

# TODO: remove country dummies (cntr_), use nice var names, ...
# using "text" right now for readability of stargazer output 
stargazer::stargazer(ols_base, ols_cntr, digits=2, type = "text", 
          single.row = TRUE, no.space = TRUE, 
          column.sep.width = "3pt", font.size = "small",
          out = "output/tables/OLS.tex") #%>% # flip = TRUE ?

# Moran I test -----------------------------------------------------------------

## 1. Create proper W matrix

data_coords <- st_coordinates(st_centroid(data$geometry))

### knn
k <- round(0.203187 * nrow(data))
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

## 2. Moran's I Test
moran.test(data$edgar, lw_knn)
moran.test(data$edgar, lw_inversedist)
# moran.test(data$edgar, lw_d)
moran.test(data$edgar, lw_inversedist_all)

moran.test(ols_base$residuals, lw_knn)
moran.test(ols_cntr$residuals, lw_knn)

moran.test(ols_base$residuals, lw_inversedist)
moran.test(ols_cntr$residuals, lw_inversedist)

moran.test(ols_base$residuals, lw_inversedist_all)
moran.test(ols_cntr$residuals, lw_inversedist_all)

## 3. Local Moran's I Test

plot_localmoran_sig <- function(test_input, data, lw, subtitle){
  localI <- data.frame(localmoran(test_input, lw))
  data_I <- cbind(data, localI)
  # ggplot(data = data_I) +
  #   geom_sf(aes(fill = Ii), color = "black") +
  #   ggtitle ("Local Moran's I", subtitle = "New Zealand - Median Income")  +
  #   xlab("Longitude") + ylab("Latitude") +
  #   scale_fill_viridis_c(option = "magma", direction = -1) +
  #   theme(legend.title = element_blank())
  
  data_I <- data_I %>% dplyr::mutate(p = `Pr.z...0.`, 
                                     sig=ifelse(p < 0.001, "P < 0.001", 
                                                ifelse(p < 0.05, 
                                                       "P < 0.05", "NS")),
                                     sig=ifelse(sig == "NS",sig,
                                                paste(ifelse(Ii>0,"pos.","neg."),sig)))
  # set as factor to have logical (and not alphabetical) ordering
  data_I$sig <- factor(data_I$sig, 
                       levels = c(unique(data_I$sig)[unique(data_I$sig)!="NS"] %>% 
                                    sort(),
                                  "NS"))

  ggplot(data = data_I) +
    geom_sf(aes(group = sig, fill = sig), color = "black") +
    ggtitle ("Local Moran's I", subtitle = subtitle)  +
    xlab("Longitude") + ylab("Latitude") +
    scale_fill_viridis_d(option = "magma", direction = -1) +
    labs(fill = "Significance") +
    theme_void()
}

plot_localmoran_sig(data$edgar, data, lw = lw_knn, subtitle = "edgar / k-nearest-neighbours")
plot_localmoran_sig(data$edgar, data, lw = lw_inversedist, subtitle = "edgar / KNN-inverse-distance")
plot_localmoran_sig(data$edgar, data, lw = lw_inversedist_all, subtitle = "edgar / inverse-distance")

plot_localmoran_sig(ols_base$residuals, data, lw = lw_knn, subtitle = "OLS residuals / k-nearest-neighbours")
plot_localmoran_sig(ols_cntr$residuals, data, lw = lw_knn, subtitle = "OLS CFE residuals / k-nearest-neighbours")

plot_localmoran_sig(ols_base$residuals, data, lw = lw_inversedist, subtitle = "OLS residuals / inverse distance")
plot_localmoran_sig(ols_cntr$residuals, data, lw = lw_inversedist, subtitle = "OLS CFE residuals / inverse distance")

plot_localmoran_sig(ols_base$residuals, data, lw = lw_inversedist_all, subtitle = "OLS residuals / inverse distance all")
plot_localmoran_sig(ols_cntr$residuals, data, lw = lw_inversedist_all, subtitle = "OLS CFE residuals / inverse distance all")

