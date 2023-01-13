# get streamflow signatures from existing data computed for NWMv3

rm(list=ls())

library(data.table)
library(sf)

# streamflow signatures calculated in NWM v3 for conus
attrs <- get(load("../datasets/streamflow_signatures_conus_nwmv3/ss_attrs_donors_v3.Rdata"))
attrs1 <- rbind(attrs,get(load("../datasets/streamflow_signatures_conus_nwmv3/ss_attrs_ss_basins.Rdata")))
attrs1$id <- NULL

# get huc-01 streamflow signatures
#f1 <- "../datasets/usgs_stations_all_2021-08-23.txt"
#gage_meta <- read.csv(f1,skip=32,header=TRUE,sep="\t",stringsAsFactors=FALSE,
#    colClasses=c(rep("character",3),rep("numeric",2),rep("character",2),"numeric",rep("character",3),"numeric","numeric"))
#gage_meta$site_no <- gsub(" ", "", gage_meta$site_no)

dt1 <- get(load("output/crosswalk_gage_cat_huc01.Rdata"))
gages0 <- unique(dt1$gages)
attrs <- subset(attrs1, ID %in% gages0)

save(attrs,file="output/ss_attr_huc01.Rdata")

# plot catchments with ss attrs
huc01 <- st_read("shapefile/catchment_data.geojson")
cats <- subset(huc01,id %in% subset(dt1,gages %in% attrs$ID)$id)
sf::sf_use_s2(FALSE)
sf1 <- st_union(huc01)
plot(sf1,main="HUC-01 cathcments with valid streamflow signatures")
plot(st_geometry(cats),border=NA,col="darkgreen",add=T)