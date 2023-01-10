# extract USGS streamflow data for HUC-01 basins from existing archive for CONUS

rm(list=ls())
library(data.table)

period1 <- "20121001_20160930"
load("../datasets/obsStrData_ALL_GAGES_UV_topOfHour_2007_2019.Rdata")
load(paste0("flows/flow_",period1,"_CFE_hlr_kge_dds.Rdata"))
basins <- names(dtFlow)
basins <- basins[basins != "validTime"]
obsDt <- subset(obsStrData,site_no %in% basins)
obsDt <- subset(obsDt, POSIXct %in% dtFlow$validTime)
names(obsDt)[names(obsDt)=="POSIXct"] <- "validTime"
save(obsDt,file=paste0("../datasets/usgs_hourly_flow_",period1,"_huc01.Rdata"))

