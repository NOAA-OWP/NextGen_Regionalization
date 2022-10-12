rm(list=ls())

library(data.table)

# streamflow signatures calculated in NWM v3 for conus
attrs <- get(load("../datasets/streamflow_signatures_conus_nwmv3/ss_attrs_donors_v3.Rdata"))
attrs <- rbind(attrs,get(load("../datasets/streamflow_signatures_conus_nwmv3/ss_attrs_ss_basins.Rdata")))

# get huc-01 streamflow signatures
#f1 <- "../datasets/usgs_stations_all_2021-08-23.txt"
#gage_meta <- read.csv(f1,skip=32,header=TRUE,sep="\t",stringsAsFactors=FALSE,
#    colClasses=c(rep("character",3),rep("numeric",2),rep("character",2),"numeric",rep("character",3),"numeric","numeric"))
#gage_meta$site_no <- gsub(" ", "", gage_meta$site_no)

dt1 <- get(load("output/crosswalk_gage_cat_huc01.Rdata"))
gages0 <- unique(dt1$gages)
attrs <- subset(attrs, ID %in% gages0)
attrs$id <- NULL

save(attrs,file="output/ss_attr_huc01.Rdata")