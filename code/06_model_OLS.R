# load data
data <- readRDS("input/data.rds")

# data <- data %>% 
#   dplyr::filter(!cntr_code %in% c("UK", "NO",
#                                   "AL", "CH", 
#                                   "MK", "ME", 
#                                   "RS", "LI", 
#                                   "TR"))
# 
# plot(st_geometry(data[is.na(data$HDD),]))

# I=PAT
model_base <- log(edgar) ~ log(pop) + log(density) + log(gdppc) + 
  I(log(gdppc)^2) + gwa_share_BE + # log(gwa_share_GI) +
  # log(heating_or_cooling) +
  # log(HDD) + cdd_log +
  log(hdd) + log(cdd_fix)
ols_base <- lm(model_base, data)
summary(ols_base)

# ols_de <- lm(model_base, data %>% filter(cntr_code == "DE"))
# ols_de <- lm(model_base, data %>% filter(cntr_code %in% c("DE","PL","CZ")))
# summary(ols_de)

model_cntr <- log(edgar) ~ log(pop) + log(density) + log(gdppc) + 
  I(log(gdppc)^2) + gwa_share_BE + # log(gwa_share_GI) +
  # log(heating_or_cooling) +
  # log(HDD) + cdd_log +
  log(hdd) + log(cdd_fix) + cntr_code
ols_cntr <- lm(model_cntr, data)
summary(ols_cntr)


# stargazer(ols_base, ols_cntr, digits=2) # flip = TRUE ? 
# remove country dummies om cntr

data_coords <- st_coordinates(st_centroid(data$geometry))

# bw neighbours
lw <- knearneigh(data_coords, k=100) %>% 
  knn2nb() %>% 
  nb2listw()
moran.test(data$edgar, lw)

moran.test(ols_base$residuals, lw)
moran.test(ols_cntr$residuals, lw)

