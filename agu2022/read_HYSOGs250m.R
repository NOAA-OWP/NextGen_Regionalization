# derive the hydrologic soil group (HSG) for catchments based on the original 250m data

rm(list=ls())

library(zonal)
library(sf)
library(data.table)

# compute dominant HSG class
huc01 <- st_read("shapefile/catchment_data.geojson")
huc01 <- execute_zonal("../datasets/HYSOGs250m/HYSOGs250m.tif",geom=huc01,ID="id",FUN="mode",join=TRUE)
names(huc01)[names(huc01)=="V1"] <- "hsg"

attrs <- as.data.table(huc01)
attrs <- attrs[,c("id","hsg"),with=F]

# map to letters (A,B,C,D)
attrs$hsg <- factor(attrs$hsg, levels=c(1:4,11:14), labels=c("A","B","C",rep("D",5)))

# save data
save(attrs,file="output/hsg_attr_huc01.Rdata")
