rm(list=ls())

library(data.table)
library(ggplot2)

gage1 <- "01115630"
cat1 <- "cat-34950"; area1 <- 11.927230
cat2 <- "cat-34952"; area2 <- 6.781686

sim_date1 <- "20081001"; sim_date2 <- "20140930"
val_date1 <- "20091001"; val_date2 <- "20140930"
sim_times <- seq(as.POSIXct(sim_date1,"%Y%m%d",tz="UTC"), 
                 as.POSIXct(paste0(sim_date2,"23"),"%Y%m%d%H",tz="UTC"),by="hour")
val_times <- seq(as.POSIXct(val_date1,"%Y%m%d",tz="UTC"), 
                 as.POSIXct(paste0(val_date2,"23"),"%Y%m%d%H",tz="UTC"),by="hour")

obsDt <- get(load(paste0("../datasets/usgs_hourly_flow_",sim_date1,"_",sim_date2,"_huc01.Rdata")))
dtFlow1 <- get(load(paste0("data/flow_",sim_date1,"_",sim_date2,"_CFE_hlr_kge-dds.Rdata")))
dtFlow0 <- get(load(paste0("data/flow_",sim_date1,"_",sim_date2,"_noah_owp_CFE.hlr.Rdata")))

# calculate streamflow from catchment runoff (Q_OUT)
f1 <- paste0("data/",cat1,".csv")
dt1 <- as.data.table(read.csv(f1))
dt1 <- dt1[,c("Time","Q_OUT"),with=F]
dt1$Time <- as.POSIXct(dt1$Time,format="%Y-%m-%d %H:00:00",tz="UTC")
dt1$Q_OUT <- dt1$Q_OUT * area1 * 10^3

f2 <- paste0("data/",cat2,".csv")
dt2 <- as.data.table(read.csv(f2))
dt2 <- dt2[,c("Time","Q_OUT"),with=F]
dt2$Time <- as.POSIXct(dt2$Time,format="%Y-%m-%d %H:00:00",tz="UTC")
dt2$Q_OUT <- dt2$Q_OUT * area2 * 10^3

dtAll <- data.table(time=dt1$Time, ngen=dt1$Q_OUT + dt2$Q_OUT)
dtAll <- subset(dtAll, time>=min(val_times) & time<=max(val_times))

# add observation
obs1 <- subset(obsDt,site_no==gage1)
obs1$site_no <- NULL
names(obs1) <- c("time","obs")
dtAll <- merge(dtAll, obs1,by="time",all.x=TRUE)
dtAll <- dtAll[,c("time","obs","ngen"),with=FALSE]

# calculate streamflow from catchment runoff (Q_OUT) - first run
f1 <- paste0("data/",cat1,"_old.csv")
dt1 <- as.data.table(read.csv(f1))
dt1 <- dt1[,c("Time","Q_OUT"),with=F]
dt1$Time <- as.POSIXct(dt1$Time,format="%Y-%m-%d %H:00:00",tz="UTC")
dt1$Q_OUT <- dt1$Q_OUT * area1 * 10^3

f2 <- paste0("data/",cat2,"_old.csv")
dt2 <- as.data.table(read.csv(f2))
dt2 <- dt2[,c("Time","Q_OUT"),with=F]
dt2$Time <- as.POSIXct(dt2$Time,format="%Y-%m-%d %H:00:00",tz="UTC")
dt2$Q_OUT <- dt2$Q_OUT * area2 * 10^3

tmp <- data.table(time=dt1$Time, ngen_old=dt1$Q_OUT + dt2$Q_OUT)
dtAll <- merge(dtAll,tmp,by="time",all.x=TRUE)
stop()

# add streamflow from t-rout
tmp <- data.table(time=sim_times, trout=dtFlow1[[gage1]])
dtAll <- merge(dtAll, tmp, by="time",all.x=TRUE)

# add streamflow from FIHM
tmp <- data.table(time=sim_times, fihm=dtFlow0[[gage1]])
dtAll <- merge(dtAll, tmp, by="time",all.x=TRUE)

message(paste0("KGE: fihm=",round(KGE(dtAll$fihm,dtAll$obs),2),
                 ", trout=",round(KGE(dtAll$trout,dtAll$obs),2),
                 ", ngen=",round(KGE(dtAll$ngen,dtAll$obs),2)))
message(paste0("NSE: fihm=",round(NSE(dtAll$fihm,dtAll$obs),2),
               ", trout=",round(NSE(dtAll$trout,dtAll$obs),2),
               ", ngen=",round(NSE(dtAll$ngen,dtAll$obs),2)))

mycolors <- c("black","red","blue","orange")
dtAll1 <- melt(dtAll,id.vars = "time")
gg1 <- ggplot(dtAll1,aes(x=time,y=value)) +
  geom_line(aes(group=variable,color=variable,linetype=variable)) +
  scale_color_manual(name="",values=mycolors) +
  scale_linetype_manual(name="",values=1:4) +
  labs(x="Time",y="flow (cms)", title=paste0(gage1," Flow Obs vs. Simulations")) +
  theme(text=element_text(size=14), plot.title=element_text(size=20),
        legend.position="top")

ggsave(file=paste0("figs/flow_ts_",gage1,".png"),plot=gg1,width=10,height=5,units="in",dpi=300)

#plot(dtAll$time,dtAll$obs,type="l",lwd=2)
#lines(dtAll$time, dtAll$flow,lty=2,col="red")
#lines(dtAll$time, dtAll$flow1,lty=2,col="blue")
