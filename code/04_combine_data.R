year_filter <- 2016

shape_nuts3 <- getShapefile()
data_eurostat <- readRDS("input/data_eurostat.rds") %>% 
  filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value")
# data_edgar <- readRDS("input/data_edgar.rds")
data_heating_cooling <- readRDS("input/data_heating_cooling.rds") %>% 
  filter(year == year_filter) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  dplyr::mutate(year = as.character(year))

data <- shape_nuts3 %>% 
  left_join(data_eurostat, by = c("nuts3_id")) %>% 
  left_join(data_heating_cooling, by = c("nuts3_id", "year"))
                                  
rm(data_eurostat, data_edgar, data_heating_cooling)

saveRDS(data, "input/data.rds")

