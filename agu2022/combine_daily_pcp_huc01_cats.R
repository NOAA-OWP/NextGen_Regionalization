rm(list=ls())
library(data.table)

dir1 <- "/glade/scratch/ariz01/aorc/APCP_daily_huc01/"
years <- 2008:2021

# first combine by year
for (y1 in years) {
dtPcp <- data.table()
date1 <- paste0(y1-1,"1001")
date2 <- paste0(y1,"0930")
dates <- format(seq(as.POSIXct(date1,format="%Y%m%d",tx="UTC"), as.POSIXct(date2,format="%Y%m%d",tx="UTC"),by="day"), "%Y%m%d", tz="UTC")
for (d1 in dates) {
    f1 <- paste0(dir1,d1,".Rdata")
    if (!file.exists(f1)) step(paste0("ERROR: file does not exist - ",f1))
    message(d1)
    load(f1)
    dt_apcp$date <- d1
    dtPcp <- rbind(dtPcp, dt_apcp)
}

save(dtPcp, file=paste0("../data/pcp_daily_huc01_",y1,".Rdata"))
}
gc()

# then combine all years
dtPcpAll <- data.table()
for (y1 in years) 
    dtPcpAll <- rbind(dtPcpAll,get(load(paste0("../data/pcp_daily_huc01_",y1,".Rdata"))))
save(dtPcpAll, file=paste0("../data/pcp_daily_huc01_",years[1],"-",years[2],".Rdata"))