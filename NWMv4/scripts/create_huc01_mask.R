#

rm(list=ls())

library(rgdal)
library(raster)

s1 <- readOGR("shapefile/catchment_data.geojson",layer="OGRGeoJSON")

#prjstr <- "+proj=lcc +lat_1=30 +lat_2=60 +lat_0=40.0000076293945 +lon_0=-97 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs"
#s1 <- spTransform(s1,crs(prjstr))
writeOGR(s1,dsn='../../shapefile',driver='ESRI Shapefile',layer='huc01_catchments', overwrite_layer=TRUE)
