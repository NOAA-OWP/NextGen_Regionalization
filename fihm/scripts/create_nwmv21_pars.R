# temporary script for computing CFE parameters for ngen from NWMv21 parameter grids
# These parameters are already computed in the new hydrofabric (hence no need for this script)

rm(list=ls())

library(zonal)
library(sf)
library(terra)
library(data.table)
library(ncdf4)
library(raster)

pars <- "bexp"

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson")
huc01$area_sqkm <- huc01$toid <- NULL

# NWM 1-km raster mask (to get raster crs, extent, resolution)
m0 <- raster("../data/geogrid_1km_blank.tif")

f1 <- "../data/soil_properties_FullRouting_NWMv2.1.nc"
nc <- nc_open(f1)
for (p1 in pars) {

# read NMW parameter 
v1 <- ncvar_get(nc,p1)
v1 <- v1[,,1]
s1 <- raster(t(v1)[nrow(t(v1)):1,])
#s1 <- do.call(stack,lapply(1:dim(v1)[3], function(x) raster(t(v1[,,x])[nrow(t(v1[,,x])):1,])))
crs(s1) <- crs(m0); extent(s1) <- extent(m0); res(s1) <- res(m0)

# compute zonal 
huc01 <- execute_zonal(rast(s1),geom=huc01,ID="id",FUN="mean",join=TRUE)
huc01$V1 <- round(huc01$V1, 2)
names(huc01)[names(huc01)=="V1"] <- p1

}


