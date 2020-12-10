# load data
data <- readRDS("input/data.rds")

# temp fix
data <- data %>% 
  dplyr::mutate(gwa_share_BE = `gwa_share_B-E`,
                gwa_share_GI = `gwa_share_G-I`) %>% 
  dplyr::select(-`gwa_share_B-E`, -`gwa_share_G-I`)

data <- data %>% 
  dplyr::mutate(
    # gdppc_2 = gdppc^2,
    density = pop/area,
    heating_or_cooling = HDD + CDD,
    CDD_fix = CDD+1, # move our scale by 1 to be able to log it
    cdd_log = ifelse(CDD == 0, 0, log(CDD))
  )

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
  I(log(gdppc)^2) + log(gwa_share_BE) + log(gwa_share_GI) +
  # log(heating_or_cooling) +
  # log(HDD) + cdd_log +
  log(HDD) + log(CDD_fix)
ols_base <- lm(model_base, data)
summary(ols_base)
