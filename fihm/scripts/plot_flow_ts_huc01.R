# plot streamflow time series for all gages (calibrated and regionalized, in separate files)
rm(list=ls())

library(data.table)
library(ggplot2)

date1 <- "20081001"
date2 <- "20140930"
start_date <- "2009100100"
end_date <- "2014093023"
times <- seq(as.POSIXct(start_date,"%Y%m%d%H",tz="UTC"), as.POSIXct(end_date,"%Y%m%d%H",tz="UTC"),by="hour")
regs <- "camels"
runs <- c("CFE","TOPMODEL","mosaic")
obsDt <- get(load(paste0("data/usgs_hourly_flow_",date1,"_",date2,"_huc01.Rdata"))
basins <- unique(obsDt$site_no)

# get donor basins
df1 <- read.table("../data/headwater_catchments_huc01.txt",header=TRUE,colClasses = "character")
cats1 <- c("cat-15200", "cat-18746", "cat-33305") # not used as donors due to poor performance
calib_basins <- subset(df1, !catchment %in% cats1)$gages

# load the model simulations
modAll <- vector("list", length(regs)*length(runs))
k <- 0
for (reg1 in regs) {
for (run1 in runs) {

    k <- k+1
    scenario <- paste0("noah_owp_",run1)
    outfile <- paste0("../output/flow_",date1,"_",date2,"_",reg1,"_",scenario,"_donated.Rdata")
    dtFlow <- get(load(outfile))
    modAll[[k]] <- dtFlow
    names(modAll)[k] <- scenario
}}

# plot obs vs. model simulation time series
mycolors <- c("black","red","blue","orange")
for (kk in 1:2) {

if (kk==1) {
    basins1 <- calib_basins
    pdf("../figs/flow_ts_all_gages_huc01_donor.pdf",onefile=TRUE,width=10,height=6)
} else {
    basins1 <- basins[!basins %in% calib_basins]
    pdf("../figs/flow_ts_all_gages_huc01_receiver.pdf",onefile=TRUE,width=10,height=6)
}

for (b1 in basins1) {

    message(paste0(match(b1,basins1)," ", b1))

    dtAll <- subset(obsDt,site_no==b1)
    dtAll$site_no <- NULL
    names(dtAll)[2] <- "obs"

    # fill missed obs with NA
    dtAll <- subset(dtAll,validTime>=as.POSIXct(start_date,format="%Y%m%d",tz="UTC"))
    dtAll <- merge(data.table(validTime=times), dtAll, all.x=TRUE)

    for (scenario in names(modAll)) {
        mod1 <- modAll[[scenario]][,c("validTime",b1),with=F]
        names(mod1)[2] <- scenario
        dtAll <- merge(dtAll, mod1,all.x=TRUE)
    }
    dtAll1 <- melt(dtAll,id.vars="validTime")

    gg1 <- ggplot(dtAll1,aes(x=validTime,y=value)) +
           geom_line(aes(group=variable,color=variable,linetype=variable)) +
           scale_color_manual(name="",values=mycolors) +
           scale_linetype_manual(name="",values=1:4) +
           labs(x="Time",y="flow (cms)", title=paste0(b1," Flow Obs vs. Simulations")) +
           theme(text=element_text(size=14), plot.title=element_text(size=20),
              legend.position="top")
         
    #ggsave(file=paste0("figs/flow_ts_",b1,".png"),plot=gg1,width=10,height=5,units="in",dpi=300)

    print(gg1)
}
dev.off()
}

