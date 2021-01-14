# setup ------------------------------------------------------------------------
year_filter <- 2016

# get data ---------------------------------------------------------------------
# shapefile
shape_nuts3 <- getShapefile()
# eurostat
data_eurostat <- readRDS("input/data_eurostat.rds") %>% 
  filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value")
# EDGAR
data_edgar <- readRDS("input/data_edgar.rds") %>%  
  filter(indicator == "edgar_co2") %>%
  filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  dplyr::mutate(year = as.character(year)) %>%
  rename(edgar = "edgar_co2") # only use co2 excl short cycle, we can add other greenhouse gases by summing them
# heating and cooling days
data_heating_cooling <- readRDS("input/data_heating_cooling.rds") %>% 
  filter(year == year_filter) %>% 
  dplyr::mutate(indicator = tolower(indicator)) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  dplyr::mutate(year = as.character(year))

data <- shape_nuts3 %>% 
  left_join(data_eurostat, by = c("nuts3_id")) %>% 
  left_join(data_edgar, by = c("nuts3_id", "year")) %>%
  left_join(data_heating_cooling, by = c("nuts3_id", "year"))

rm(data_eurostat, data_edgar, data_heating_cooling)

summary(data)

data <- data %>% 
  dplyr::mutate(
    # gdppc_2 = gdppc^2,
    density = pop/area, # pop per m2
    heating_or_cooling = hdd + cdd,
    cdd_fix = cdd+1, # move our scale by 1 to be able to log it
    cdd_log = ifelse(cdd == 0, 0, log(cdd))
  )

# exclude so we have gdp, population and GWA share BE
exclude <- c("NO","CH", "TR", "RS", "IE", "UK", "LI")
data <- data[!data$cntr_code%in%exclude,]
# exclude so we have CDD,HDD
exclude <- c("AL","MK","ME")
data <- data[!data$cntr_code%in%exclude,]

# fix outliers
data[data$edgar > quantile(data$edgar,0.99),]$edgar

data_fix_outlier <- data %>% 
  dplyr::mutate(edgar = ifelse(edgar > quantile(edgar,0.99), quantile(edgar,0.99), edgar),
                density = ifelse(density > quantile(density,0.99), quantile(density,0.99), density),
                gdppc = ifelse(gdppc > quantile(gdppc,0.99), quantile(gdppc,0.99), gdppc))

temp <- st_drop_geometry(data_fix_outlier)
temp$gdppc2 <- data$gdppc^2

# correlation of actual values
cor(temp %>% dplyr::select(edgar, pop, gdppc, gdppc2, density, gwa_share_BE, hdd, cdd_fix)) %>% 
  stargazer() %>% 
  cat(.,file="output/tables/cor.tex",sep="\n")
# correlation of log values (care: different gdppc2 specification)
temp_log <- temp %>% mutate(
  edgar = log(edgar),
  pop = log(pop),
  gdppc = log(gdppc),
  gdppc2 = log(gdppc)^2,
  density = log(density),
  gwa_share_BE = log(gwa_share_BE),
  hdd = log(hdd),
  cdd_fix = log(cdd_fix))
cor(temp_log %>% dplyr::select(edgar, pop, gdppc, gdppc2, density, gwa_share_BE, hdd, cdd_fix)) %>% 
  stargazer() %>% 
  cat(.,file="output/tables/cor_log.tex",sep="\n")

summary(data)
saveRDS(data, "input/data.rds")
saveRDS(data_fix_outlier, "input/data_fix_outlier.rds")

# # maybe needed for time dimension
# 
# doesn't change anything anymore
# data_new <- data %>% 
#   filter(!is.na(gdppc),
#          !is.na(edgar),
#          !is.na(cdd),
#          !is.na(hdd))
# 
# summary(data_new)
# 
# 
# check for NAs
# data_na <- data[is.na(data$cdd),]
# data_na <- data[is.na(data$gwa_share_BE),]
# 
# library(leaflet)
## 1. plot all and highlight the NAs
# data %>% 
#   dplyr::mutate(myvar = cdd,
#                 mylabel = nuts3_id) %>% 
#   dplyr::mutate(is_na = is.na(myvar)) %>% 
#   leaflet() %>%
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.5,
#               fillColor = ~colorFactor("YlOrRd", is_na)(is_na),
#               
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE),
#               label = ~mylabel,
#               labelOptions = leaflet::labelOptions(
#                 style = list("font-weight" = "normal", padding = "3px 8px"),
#                 textsize = "15px",
#                 direction = "auto"))
# 
## 2. just plot the NAs
# data_na %>% 
#   dplyr::mutate(myvar = CDD,
#                 mylabel = nuts3_id) %>% 
#   leaflet() %>%
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.5,
#               fillColor = ~colorQuantile("YlOrRd", myvar)(myvar),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE),
#               label = ~mylabel,
#               labelOptions = leaflet::labelOptions(
#                 style = list("font-weight" = "normal", padding = "3px 8px"),
#                 textsize = "15px",
#                 direction = "auto"))
# 
# unique(data_na$cntr_code)
# 
# data_na <- data[rowSums(is.na(data)) > 0,]
# 
# plot(data_na[,c("geometry","CDD")])
# 
# data_na <- data[is.na(data$edgar),]
# plot(data_na[,c("geometry","edgar")])
# 
# #install.packages("sjmisc")
# library(sjmisc)
# 
# x <- left_join(shape_nuts3, data_edgar)
# 
# x$edgar <- group_var(x$edgar, size = 5)
# 
# ggplot(data = x) + 
#   geom_sf(aes(fill = edgar)) +
#   theme_void()

