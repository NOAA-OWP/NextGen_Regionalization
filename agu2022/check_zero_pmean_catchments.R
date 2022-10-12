rm(list=ls())

library(ncdf4)
library(sf)

f1 <- "data/NextGen_forcing_2007010100_ee.nc"
nc1 <- nc_open(f1)
ids <- ncvar_get(nc1,"ids")
pcp <- ncvar_get(nc1,"RAINRATE")
times <- ncvar_get(nc1,"Time")
nc_close(nc1)

# read HUC-01 geojson into sf
huc01 <- st_read("shapefile/catchment_data_drano.geojson")

# attributes 
attrs <- get(load("output/clim_attr_huc01.Rdata"))

# catchments with p_mean==0
cats0 <- subset(attrs, p_mean == 0)$id
ix1 <- match(cats0, ids)
pcp1 <- pcp[,ix1]