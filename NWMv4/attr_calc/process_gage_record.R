# process the streamflow data record for ss basins to determine if the 
# basin has sufficient data for calculating streamflow signatures

rm(list=ls())

library(data.table)

nday_min <- 183
nyear_min <- 3
period1 <- "2008-2021"
date1 <- as.Date("2007-10-01")
date2 <- as.Date("2021-09-30")
dir1 <- "/glade/work/ariz01/data/usgs/daily_streamflow/"

for (region in c("donors_v3","ss_basins")) {

message(region)

# all gages
basins <- get(load(paste0("../../data/meta_",region,".Rdata")))$ID

dtAll <- data.table()
gages1 <- NULL
for (g1 in basins) {

f1 <- paste0(dir1, g1,"_daily_streamflow.Rdata")
if (file.exists(f1)) {
  dtFlow <- get(load(f1))
} else {
  if (region == "donors_v3") {
    obsStrData <- get(load(paste0("/glade/p/cisl/nwc/nwmv30_calibration/OBS/",g1,".Rdata")))
    dtFlow <- obsStrData[,c("POSIXct","obs"), with=FALSE]
    dtFlow[,date:=as.Date(POSIXct)]
    dtFlow$POSIXct <- NULL
    dtFlow <- dtFlow[,.(flow_cms=mean(obs)),by=.(date)]
  } else {
    next
  }
}

#message(g1)
dtFlow[,year:=as.integer(format(date,"%Y"))]
dtFlow[,month:=as.integer(format(date,"%m"))]
dtFlow[,WY:=ifelse(month>=10,year+1,year)]

stop()
dt1 <- subset(dtFlow,!is.na(flow_cms) & flow_cms>=0)
dt1 <- subset(dt1, date>=date1 & date<=date2)
nd1 <- as.data.frame(table(dt1$WY))
nd1 <- subset(nd1,Freq>=nday_min)
ny1 <- length(unique(nd1$Var1))
if (ny1>=nyear_min) {
  gages1 <- c(gages1,g1)
  dt1$site_no <- g1
  dtAll <- rbind(dtAll,dt1)
}}

gages0 <- basins[! basins %in% gages1]
print(paste0("Gages with no data (",length(gages0),"): ", paste(gages0,collapse=" ")))

save(dtAll, file=paste0("../../data/daily_flow_",region,"_",period1,".Rdata"))
}
