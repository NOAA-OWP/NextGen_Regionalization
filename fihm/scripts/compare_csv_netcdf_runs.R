# Temporary scrip to compare simulations from using csv vs netcdf forcing readers

rm(list=ls())

library(data.table)
library(ggplot2)

date1 <- "20081001"
date2 <- "20140930"
reg1 <- "hlr" # regionalization scenarios
run1 <- c("CFE+TOPMODEL")
scenario <- paste0("noah_owp_",run1,".",reg1)

f1 <- paste0("../output/flow_",date1,"_",date2,"_",scenario,".Rdata")
f2 <- paste0("../output/flow_",date1,"_",date2,"_",scenario,"_netcdf.Rdata")
flow1 <- get(load(f1))
flow2 <- get(load(f2))

flow1 <- melt(flow1,id.vars="validTime")
flow2 <- melt(flow2,id.vars="validTime")
flows <- merge(flow1,flow2,by=c("validTime","variable"),add.x=TRUE,all.y=TRUE,suffixes=c(".csv",".ncdf"))
flows[,diff := abs(value.csv - value.ncdf)]
names(flows)<- c("time","gage","csv","ncdf","diff")

pdf("../figs/flow_ts_csv_netcdf_comparsion_new.pdf",onefile=TRUE,width=10,height=6)
gages1 <- unique(subset(flows,diff>1000)$gage)
for (g1 in gages1) {

dt1 <- subset(flows, gage==g1)
dt1$gage <- NULL
dt2 <- melt(dt1,id.vars="time")
gg1 <- ggplot(dt2,aes(x=time,y=value)) +
        geom_line(aes(group=variable,color=variable,linetype=variable)) +
        scale_color_manual(name="",values=c("black","blue","red")) +
        scale_linetype_manual(name="",values=1:3) +
        labs(x="Time",y="Flow (cms)", title=paste0(g1," Flow simulations and differences (CSV vs NetCDF)")) +
        theme(text=element_text(size=14), plot.title=element_text(size=20),
        legend.position="top")
print(gg1)
}
dev.off()

