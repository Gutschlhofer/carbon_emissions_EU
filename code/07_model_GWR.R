# Libraries
library(spgwr)

# # alternative
# library(GWmodel)
# bw.ggwr
# bw.gwr
# gwr.basic
# gwr.robust

# prepare X and y
data <- NY8

# write formula
model <- Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME
model2 <- Cases ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME + offset(log(POP8))

# find best neighbour specification
# select bandwidth, here Videras (2014) uses >>gwr.bisquare<<
bwG <- gwr.sel(model, data = data, gweight = gwr.bisquare, verbose = FALSE)
gwrG <- gwr(model, data = data, bandwidth = bwG,  gweight = gwr.bisquare, hatmatrix = TRUE)
gwrG

# for n_neighbours in 1:n, do
#     make W for each based on n_neighbours, run GWR

# GWR can also be applied in a GLM
# framework, and a provisional implementation permitting this has been added to the
# spgwr package providing both cross-validation bandwidth selection and geographically
# weighted fitting of GLM models.
gbwG <- ggwr.sel(model2, data = data, family = "poisson", gweight = gwr.Gauss, verbose = FALSE)
ggwrG <- ggwr(model2, data = data, family = "poisson", bandwidth = gbwG, gweight = gwr.Gauss)

