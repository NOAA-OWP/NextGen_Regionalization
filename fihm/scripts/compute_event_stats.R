#
rm(list=ls())

library(data.table)
source("matchEvents.R")

period1 <- "20081001_20140930"
period2 <- "20091001_20140930"
date0 <- as.POSIXct("20091001",format="%Y%m%d", tz="UTC") # start date of evaluation period
runs <- c("camels","hlr","default")
models <- c("CFE","TOPMODEL","CFE+TOPMODEL")
prob1 <- 0.90

# first detect observed events
# observed data
f1 <- paste0("../output/usgs_hourly_flow_",period1,"_huc01.Rdata")
load(f1)
obsDt <- dcast(obsDt, validTime ~ site_no, value.var="q_cms")

# observed events
f1 <- paste0("../output/usgs_hourly_flow_",period1,"_huc01_events.csv")
eventsObsAll <- as.data.table(read.csv(f1,colClasses="character"))
eventsObsAll$X <- NULL
eventsObsAll[,start:=as.POSIXct(start,format="%Y-%m-%d %H:%M:%S",tz="UTC")]
eventsObsAll[,end:=as.POSIXct(end,format="%Y-%m-%d %H:%M:%S",tz="UTC")]

# loop through different model scenarios 
dtEventStats <- data.table()
for (m1 in models) {
for (r1 in runs) {

scenario1 <- paste0(m1,".",r1)  

# modeled data
f1 <- paste0("../output/flow_",period1,"_noah_owp_",scenario1,".Rdata")
if (!file.exists(f1)) next
modelDt <- get(load(f1))

# model events
f1 <- paste0("../output/flow_",period1,"_noah_owp_",scenario1,"_events.csv")
eventsModAll <- as.data.table(read.csv(f1,colClasses="character"))
eventsModAll$X <- NULL
eventsModAll[,start:=as.POSIXct(start,format="%Y-%m-%d %H:%M:%S",tz="UTC")]
eventsModAll[,end:=as.POSIXct(end,format="%Y-%m-%d %H:%M:%S",tz="UTC")]

# loop through gages to match observed and model events
gages <- names(obsDt)
gages <- gages[gages != "validTime"]
for (gage1 in gages) {
thresh1 <- quantile(obsDt[[gage1]],prob1,na.rm=TRUE)
data1 <- obsDt[,c("validTime",gage1),with=FALSE]
data1 <- merge(data1,modelDt[,c("validTime",gage1),with=FALSE],all.x=T,all.y=T,by="validTime")
names(data1) <- c("Date", "obs","mod")

eventsObs <- subset(eventsObsAll, usgs_site_code == gage1)
eventsObs$peak <- eventsObs$start
for (i1 in 1:nrow(eventsObs)) {
  tmp1 <- subset(data1, Date>=eventsObs$start[i1] & Date<=eventsObs$end[i1])
  eventsObs$peak[i1] <- tmp1$Date[which.max(tmp1[["obs"]])]
}

eventsMod <- subset(eventsModAll, usgs_site_code == gage1)
if (nrow(eventsMod)==0) next
eventsMod$peak <- eventsMod$start
for (i1 in 1:nrow(eventsMod)) {
  tmp1 <- subset(data1,Date>=eventsMod$start[i1] & Date<=eventsMod$end[i1])
  eventsMod$peak[i1] <- tmp1$Date[which.max(tmp1[["mod"]])]
}

# plot events
# source("plot_events_obs_model.R")
#date1 <- as.POSIXct("20130601",format="%Y%m%d",tz="UTC")
#date2 <- as.POSIXct("20131231",format="%Y%m%d",tz="UTC")
#data2 <- subset(data1,Date>=date1 & Date<=date2)
#eventsObs1 <- subset(eventsObs, start>=date1 & end<=date2)
#eventsMod1 <- subset(eventsMod, start>=date1 & end<=date2)
#plotEventsObsMod(data2,eventsObs1,eventsMod1,gage1,thresh1)

# keep only those observed events above the user defined probability threshold
peaks1 <- data1$obs[match(eventsObs$peak, data1$Date)]
eventsObs1 <- eventsObs[which(peaks1>=thresh1),]

# exclude the first year as warm up
eventsObs1 <- subset(eventsObs1, start>=date0)
eventsMod1 <- subset(eventsMod, start>=date0)

# match observed events with model events
data2 <- as.data.frame(data1)
names(data2)[names(data2)=="Date"] <- "time"
data_obs <- data2[,c("time","obs")]
data_mod <- data2[,c("time","mod")]
names(data_obs) <- c("time","value")
names(data_mod) <- c("time","value")
matchResults <- matchEvents(data_obs,data_mod, eventsObs1, eventsMod1)
events_obs <- matchResults[["events_obs_match"]]
events_mod <- matchResults[["events_mod_match"]]
ne1 <- nrow(events_obs)

message(paste0("+++++++++  ", scenario1, "   ", match(gage1, gages),"   ", gage1," ++++++++++"))
message(paste0("Number of events detected (obs, mod) and matched: ", 
   nrow(eventsObs),", ", nrow(eventsMod), ", ", nrow(events_obs)))
if (ne1==0) next

# compute peak and volume bias
peak_obs <- data_obs$value[match(events_obs$peak,data_obs$time)]
peak_mod <- data_mod$value[match(events_mod$peak,data_mod$time)]
volume_obs <- sapply(1:ne1, function(x) sum(subset(data_obs,time %in% seq(events_obs$start[x],events_obs$end[x],by="hour"))$value))
volume_mod <- sapply(1:ne1, function(x) sum(subset(data_mod,time %in% seq(events_mod$start[x],events_mod$end[x],by="hour"))$value))

peak_bias1 <- (peak_mod-peak_obs)/peak_obs*100
peak_bias_median <- round(quantile(abs(peak_bias1),0.5,na.rm=TRUE),2)
volume_bias1 <- (volume_mod-volume_obs)/volume_obs*100
volume_bias_median <- round(quantile(abs(volume_bias1),0.5,na.rm=TRUE),2)
message (paste0("median abs. peak and volumn biases (%): ", peak_bias_median,", ", volume_bias_median,"\n"))

# event-based objective function
#obj1 <- 0.6*peak_bias_median + 0.4*volume_bias_median
#print(paste0("event-based objective function: ", round(obj1,2)))

dtEventStats <- rbind(dtEventStats,data.table(gage=gage1, scenario=scenario1, peak_bias=peak_bias1, volume_bias=volume_bias1))
}}}

save(dtEventStats, file=paste0("../stat/stat_retro_",period2,"_events.Rdata"))