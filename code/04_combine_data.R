source("code/00_libraries_functions.R")

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
  filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  dplyr::mutate(year = as.character(year))
# heating and cooling days
data_heating_cooling <- readRDS("input/data_heating_cooling.rds") %>% 
  filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  dplyr::mutate(year = as.character(year))

data <- shape_nuts3 %>% 
  left_join(data_eurostat, by = c("nuts3_id")) %>% 
  left_join(data_edgar, by = c("nuts3_id", "year")) %>% 
  left_join(data_heating_cooling, by = c("nuts3_id", "year"))
                                  
rm(data_eurostat, data_edgar, data_heating_cooling)

saveRDS(data, "input/data.rds")

# check for NAs
data_na <- data[is.na(data$CDD),]

data_na <- data[rowSums(is.na(data)) > 0,]

plot(data_na[,c("geometry","CDD")])

data_na <- data[is.na(data$edgar),]
plot(data_na[,c("geometry","edgar")])


