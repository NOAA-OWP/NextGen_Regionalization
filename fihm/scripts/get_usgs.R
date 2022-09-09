# extract USGS streamflow data for HUC-01 basins from existing archive for CONUS

rm(list=ls())
library(data.table)

load("../data/obsStrData_ALL_GAGES_UV_topOfHour_2007_2019.Rdata")
load("../output/flow_20081001_20140930_camels_noah_owp_CFE_donated.Rdata")
basins <- names(dtFlow)
basins <- basins[basins != "validTime"]
obsDt <- subset(obsStrData,site_no %in% basins)
obsDt <- subset(obsDt, POSIXct %in% dtFlow$validTime)
names(obsDt)[names(obsDt)=="POSIXct"] <- "validTime"
save(obsDt,file="../data/usgs_hourly_flow_20081001_20140930_huc01.Rdata")

