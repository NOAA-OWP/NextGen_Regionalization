# compute daily accumulated precipitation from hourly AORC (required for computing snow fraction)

library(data.table)

rm(list=ls())

# data period
date1 <- "20070101"
date2 <- "20191231"
h1 <- as.POSIXct(date1,format="%Y%m%d",tz="UTC")
h2 <- as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H",tz="UTC")
hours <- seq(h1,h2,by="hour")
dates <- format(hours,format="%Y%m%d",tz="UTC")

# AROC hourly pcp (mm) for HUC01 catchments
aorc <- get(load(paste0("../datasets/AORC/aorc_huc01_",date1,"-",date2,"_hourly.Rdata")))

for (c1 in names(aorc1)) aorc[[c1]] <- aorc1[[c1]]

# loop through catchments to compute daily pcp
aorcDaily <- vector("list",length(aorc))
names(aorcDaily) <- names(aorc)
for (c1 in names(aorc)) {
  message(paste0(match(c1,names(aorc)),"   ",c1))
  dt1 <- data.table(date=dates,pcp=aorc[[c1]])
  dt1 <- dt1[,.(pcp=sum(pcp,na.rm=FALSE)),by=.(date)]
  aorcDaily[[c1]] <- dt1$pcp
}

# save the computed daily data
save(aorcDaily,file=paste0("../datasets/AORC/aorc_huc01_",date1,"-",date2,"_daily.Rdata"))


