## This file is used to download all desired data from eurostat-----------------

# source("code/00_libraries_functions.R")

# Download Data followin Videras:-----------------------------------------------

# 1) Area
# Total Area is chosen (landuse = "TOTAL")
# other option: total land use (landuse = "L0008")

area <- get_eurostat("reg_area3", filters = list(landuse = "TOTAL")) %>% 
  filter(nchar(geo) == 5)  %>% #keep only NUTS 3 regions 
  select(-landuse)
area$time <- format(as.Date(area$time, format="%Y/%m/%d"),"%Y") #convert date to year
area$unit <- "area"

# 2.) Population
# Tatal (men + women) over all agegroups are chosen

pop <- get_eurostat("demo_r_pjangrp3") %>% 
  filter(sex == "T" & age == "TOTAL" & nchar(geo) == 5) %>% 
  select(unit, geo, time, values)
pop$time <- format(as.Date(pop$time, format="%Y/%m/%d"),"%Y") 
pop$unit <- "pop"

# 3.) GDP and GPD/PC
# PPP standards and PPP per capita are chosen here, other alternatives woud be:
#   Million Euros (unit = "MIO_EUR")
#   Million PPS (unit = "MIO_PPS")

gdp <- get_eurostat("nama_10r_3gdp", filters = list(unit = "MIO_PPS")) %>% 
  filter(nchar(geo) == 5) 
gdp$time <- format(as.Date(gdp$time, format="%Y/%m/%d"),"%Y") 
gdp$unit <- "gdp"

gdppc <- get_eurostat("nama_10r_3gdp", filters = list(unit = "PPS_HAB")) %>% 
  filter(nchar(geo) == 5) 
gdppc$time <- format(as.Date(gdppc$time, format="%Y/%m/%d"),"%Y")
gdppc$unit <- "gdppc"


# 4.) Employment per sector
# Indicator chosen:
# V16910 = Persons employed in the population of active enterprises in t - number
# Industries of Interest - available on eurostat; (Videras actually used)
#   H = Transportation and storage; (Transportation and Utilities)
#   B-E = Industry; (Manufacturing)
# Videras also has extraction, this is not available explicit but included in
# Industry B-E since B classifies "Mining and Quarrying"
# see: https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF/dd5443f5-b886-40e4-920d-9df03590ff91
# for a ENACE documentation

 empl <- get_eurostat("bd_enace2_r3") %>%
   filter(indic_sb == "V16910" & nchar(geo) == 5) %>%
   filter(nace_r2 != "B-S_X_K642") %>%  #exclude overlapping NACE
   group_by(geo, time) %>%
   mutate(empshares = round(values/sum(values),3)) #calculate industry shares
 empl$time <- format(as.Date(empl$time, format="%Y/%m/%d"),"%Y")

 #check if this worked
 xy <- filter(empl, geo == "AT111" & time == 2017)
 xy
 sum(xy$empshares) #looks good

 empl <- filter(empl, nace_r2 == "B-E" | nace_r2 == "H") %>%
   select(nace_r2, geo, time, empshares)
 colnames(empl) <- c("unit", "geo", "time","values")
 
 emplind <- empl %>% filter(unit == "B-E")
 emplind$unit <- "emp_share_B-E"

 
 empltrans <- empl %>% filter(unit == "H")
 empltrans$unit <- "emp_share_H"


# 5.) Gross valua added per sector
 # Industries of Interest - available on eurostat; (Videras actually used)
 #   G-I = Wholesale and retail trade, transport, accommodation and food service activities ; (Transportation and Utilities)
 #   B-E = Industry; (Manufacturing)
 # Problem: Transportation (H) is not available explicit
gwa <- get_eurostat("nama_10r_3gva") %>% 
  filter(nchar(geo) == 5 & currency == "MIO_EUR") %>% 
  filter(nace_r2 != "C" & nace_r2 != "G-J" & nace_r2 != "K-N" 
         & nace_r2 != "O-Q" & nace_r2 != "R-U" & nace_r2 != "TOTAL") %>% 
  group_by(geo, time) %>% 
  mutate(gwashare = round(values/sum(values),3)) %>% 
  select(nace_r2, geo, time, gwashare)
gwa$time <- format(as.Date(gwa$time, format="%Y/%m/%d"),"%Y")


xy <- filter(gwa, geo == "AT111" & time == 2017)
xy
sum(xy$gwashare) #looks good

gwaind <- gwa %>% filter(nace_r2 == "B-E")
gwaind$nace_r2 <- "gwa_share_B-E"
colnames(gwaind) <- c("unit", "geo", "time","values")

gwatrans <- gwa %>% filter(nace_r2 == "G-I")
gwatrans$nace_r2 <- "gwa_share_G-I"
colnames(gwatrans) <- c("unit", "geo", "time","values")

  
# Download Data additional data:------------------------------------------------  
# 1.) People at risk of poverty or social exclusion 
# Only available on NUTS2, so not added to main data for now
socexcl <- get_eurostat("ilc_peps11") %>% 
  filter(nchar(geo) == 4)
socexcl$time <- format(as.Date(socexcl$time, format="%Y/%m/%d"),"%Y")
socexcl$unit <- "socexcl"

#2.) Annual road transport data
# The data is available per:
  #region of loading
  #region of unloading
#I used the sum of both for both (unloading and loading) for final data
#maybe the data should be put in relative terms to GDP or something else later on

roadload <- get_eurostat("road_go_na_rl3g") %>% 
  filter(nchar(geo) == 5 & nst07 == "TOTAL") #total goods

roadunload <- get_eurostat("road_go_na_ru3g") %>% 
  filter(nchar(geo) == 5 & nst07 =="TOTAL") 

roadtotal <- roadload %>% 
  left_join(roadunload, by = c("geo", "time", "unit")) %>% 
  mutate(values = values.x + values.y) %>% 
  dplyr::select(unit, geo, time, values)

roadtotal$time <- format(as.Date(roadtotal$time, format="%Y/%m/%d"),"%Y")
roadtotal$unit <- "roadtotal"
colnames(roadtotal) <- c("unit", "geo", "time","values")
  

# Combine Data and save:--------------------------------------------------------

#combine
fineurostat <- rbind(area, pop, gdp, gdppc, emplind, empltrans, gwaind, gwatrans, roadtotal)
colnames(fineurostat) <- c("indicator","nuts3_id", "year", "value")

#save
saveRDS(fineurostat, "./input/data_eurostat.rds")


