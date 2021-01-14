# load data

# data <- readRDS("input/data.rds")

theme_set(theme_minimal())

# save plots under
plot_path <- "output/plots/"
plot_template <- paste0("raw_%s",".png")
# plot_template <- paste0("raw_%s",".rds")


## EDGAR data ------------------------------------------------------------------

# if we just plot the actual edgar data, it looks pretty terrible 
# it all looks the same because we basically just have one outlier in germany that skews the data

# ggplot(data = data) +
#  geom_sf(aes(fill = edgar), color = "black") +
#  ggtitle ("Emission levels across Europe")  +
#  scale_fill_viridis_c(option = "magma", direction = -1)

# so let's rewrite this into quantiles:
no_classes <- 5

quantiles <- quantile(data$edgar, probs = seq(0, 1, length.out = no_classes + 1))

labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 0), 
                             " – ", 
                             round(quantiles[idx + 1], 0)))
}

# remove the last label because that would be something like "100.000 - NA"
labels <- labels[1:length(labels)-1]

data$edgar_quantiles <- cut(data$edgar, 
                            breaks = quantiles, 
                            labels = labels, 
                            include.lowest = T)


ggplot(data = data) +
  geom_sf_pattern(data = shape_nuts1_agg, colour = 'black', fill = 'white') +
  geom_sf(aes(fill = edgar_quantiles), color = "white", size=0.01) +
  scale_fill_viridis(direction = -1, discrete = TRUE) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='black', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
ggsave(path = plot_path, filename = sprintf(plot_template, "Edgar_quantiles"), scale=1)


## GDP p.c. --------------------------------------------------------------------
# same goes for GDP p.c.
# ggplot(data = data) +
#   geom_sf(aes(fill = gdppc), color = "black", size=0.1) +
#   ggtitle ("GDP p.c.") +
#   scale_fill_viridis_c(direction = -1)

quantiles_gdp <- quantile(data$gdppc, 
                      probs = seq(0, 1, length.out = no_classes + 1))


labels <- c()
for(idx in 1:length(quantiles_gdp)){
  labels <- c(labels, paste0(round(quantiles_gdp[idx], 0), 
                             " – ", 
                             round(quantiles_gdp[idx + 1], 0)))
}

# remove the last label because that would be something like "100.000 - NA"
labels <- labels[1:length(labels)-1]

data$gdppc_quantiles <- cut(data$gdppc, 
                            breaks = quantiles_gdp, 
                            labels = labels, 
                            include.lowest = T)

ggplot(data = data) +
  geom_sf_pattern(data = shape_nuts1_agg, colour = 'black', fill = 'white') +
  geom_sf(aes(fill = gdppc_quantiles), color = "white", size=0.01) +
  scale_fill_viridis(direction = -1, discrete = TRUE) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
ggsave(path = plot_path, filename = sprintf(plot_template, "GDPpc_quantiles"))


## employment shares -----------------------------------------------------------
ggplot(data = data) + 
  geom_sf_pattern(data = shape_nuts1_agg, colour = 'black', fill = 'white') +
  geom_sf(aes(fill = gwa_share_BE), color = "white", size=0.01) +
  scale_fill_viridis_c(option = "magma", direction = -1, labels = percent) +  
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
ggsave(path = plot_path, filename = sprintf(plot_template, "GWA_share"))


## CDD -------------------------------------------------------------------------
cdd <- ggplot(data = data) + 
  geom_sf_pattern(data = shape_nuts1_agg, colour = 'black', fill = 'white') +
  #geom_sf(data = shape_nuts1_agg, fill="white") +
  geom_sf(aes(fill = cdd), color = "white", size=0.01) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
cdd
ggsave(path = plot_path, filename = sprintf(plot_template, "CDD")) 


## HDD -------------------------------------------------------------------------
hdd <- ggplot(data = data) + 
  geom_sf_pattern(data = shape_nuts1_agg, colour = 'black', fill = 'white') + 
  geom_sf(aes(fill = hdd), color = "white", size=0.01) +
  scale_fill_viridis_c(direction = -1) +
  theme(legend.title = element_blank()) + 
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) 
  #theme(plot.background = element_rect(fill = NA))
hdd
ggsave(path = plot_path, filename = sprintf(plot_template, "HDD")) 

library(gridExtra)
grid.arrange(cdd, hdd, ncol=2)
ggsave(path = plot_path, filename = sprintf(plot_template, "CDD_HDD")) 

# library(gridExtra)
# grid.arrange(cdd, hdd)


## density ---------------------------------------------------------------------

# ggplot(data = data) +
#   geom_sf(data = shape_nuts1_agg, color="grey") +
#   geom_sf(aes(fill = density), color = "black", size=0.1) +
#   ggtitle ("Population density")  +
#   scale_fill_viridis(direction = -1) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   theme(legend.title = element_blank()) +
#   geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1)

# no_classes <- 5

quantiles_density <- quantile(data$density, 
                      probs = seq(0, 1, length.out = no_classes + 1))

labels <- c()
for(idx in 1:length(quantiles_density)){
  labels <- c(labels, paste0(round(quantiles_density[idx], 2),
                             " – ",
                             round(quantiles_density[idx + 1], 2)))
}


# remove the last label because that would be something like "100.000 - NA"
labels <- labels[1:length(labels)-1]

data$density_quantiles <- cut(data$density, 
                            breaks = quantiles_density, 
                            labels = labels, 
                            include.lowest = T)

ggplot(data = data) +
  geom_sf_pattern(data = shape_nuts1_agg, colour = 'black', fill = 'white') + 
  geom_sf(aes(fill = density_quantiles), color = "white", size=0.01) +
  scale_fill_manual(values = rev(magma(8)[3:8])) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank()) +
  geom_sf(data=shape_nuts1_agg, color='#000000', fill=NA, size=0.1) + 
  # theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) +
  # theme(plot.background = element_rect(fill = NA) )
theme(plot.margin = unit(c(1,1,1,1), "cm"))
ggsave(path = plot_path, filename = sprintf(plot_template, "Density_quantiles"))



# Summary table ----------------------------------------------------------------

sum_data <- st_drop_geometry(data)
sum_data <- sum_data %>% dplyr::select(gdppc, density, gwa_share_BE, edgar, hdd, cdd_fix)

# load summarytools (already included in 00_library_functions)
st_options(descr.transpose = TRUE)

summary1 <- as.data.frame(descr(sum_data, 
                  stats= c("min", "q1", "mean", "med", "q3", "max", "sd"),
                  order = "p"))

rownames(summary1) <- c("GDP p.c.", "Population density", "Empl. share in manufact.", "CO² emission levels", "Heating Days Index", "Cooling Days Index")
summary1 <- round(summary1, 2)
summary1


# equivalent to table 1 from videras
sum_data_log <- log(sum_data) 

summary2 <- cbind(as.data.frame(descr(sum_data, 
                                stats = c("mean", "sd"),
                                order = "p")), 
                    as.data.frame(descr(sum_data_log, 
                                stats = c("mean", "sd"),
                                order = "p")))

rownames(summary2) <- c("GDP p.c.", "Population density", "Empl. share in manufact.", "CO² emission levels", "Heating Days Index", "Cooling Days Index")
colnames(summary2) <- c("Mean (orig. values)", "Std.Dev (orig. values)", "Mean (log values)", "Std.Dev (log values)")
summary2 <- round(summary2, 2)
summary2

# library(car)
# scatterplot(edgar ~ gdppc, data = data)
# 
# plot(log(data$gdppc), log(data$edgar))
# # abline(lm(log(edgar) ~ log(gdppc) + I(log(gdppc)^2), data = data), col = "blue")
# lines(lowess(log(data$gdppc), log(data$edgar)), col = "blue")
# 
# lines(fitted(lm(log(edgar) ~ log(gdppc) + I(log(gdppc)^2), data = data)))
# 
# 
# 
# ggplot(data, aes(x= log(gdppc) , y=  log(edgar) )) + 
#   geom_point() +
#   geom_smooth()




