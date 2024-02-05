# Compute streamflow signatures for SS basins

rm(list=ls())

library(data.table)

period1 <- "2008-2021"
nday_min <- 350 # minimum number of days during a year for the year to be included in streamflow signature calculation

pj4str<-"+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

source("../func/func_calculate_fdc.R")
source("../func/func_fdc_slope.R")
source("../func/func_StreamflowElasticity.R")
source("../func/func_BFI.R")

for (region in c("donors_v3","ss_basins")) {

message(region)

# meta data 
meta <- get(load(paste0("../../data/meta_",region,".Rdata")))

# daily pcp 
dtPcp0 <- get(load(paste0("../../AORC/pcp_daily_",region,"_",period1,".Rdata")))
dtPcp0$ID <- meta$ID[match(dtPcp0$id,meta$domainID)] 
dtPcp0$id <- NULL

# daily streamflow
dtFlowAll <- get(load(paste0("../../data/daily_flow_",region,"_",period1,".Rdata")))

dtSS <- data.table()
gages <- unique(dtPcp0$ID)
for (g1 in gages) {

ix1 <- match(g1,meta$ID)
#area1 <- meta$area_sqkm[ix1]*10^6
area1 <- meta$area_fabr[ix1]*10^6
domainID1 <- meta$domainID[ix1]

if (is.na(area1) | is.na(domainID1)) next

# streamflow time series
dtFlow <- subset(dtFlowAll, site_no == g1)
names(dtFlow)[names(dtFlow)=="flow_cms"] <- "flow"
dtFlow$flow <- dtFlow$flow*24 #covert to daily accumulated flow
dtFlow$flow <- dtFlow$flow*3600*1000/area1 # convert to mm/day

# pcp time series
dtPcp <- subset(dtPcp0, ID==g1)
dtPcp$date <- as.Date(dtPcp$date,format="%Y%m%d")
dtPcp$ID <- NULL

# merge flow and pcp time series
dtAll <- merge(dtFlow, dtPcp)
years <- subset(as.data.frame(table(dtAll$WY)),Freq>=nday_min)$Var1
message(paste0(g1," ",length(years)," years"))

years <- as.integer(as.character(years))
dtAll <- subset(dtAll, WY %in% years)
dtAll <- as.data.frame(dtAll)
if (nrow(dtAll)==0) next

# runoff ratio
runoff_ratio1 <- sum(dtAll$flow)/sum(dtAll$p_mean)

# mean flow
q_mean1 <- mean(dtAll$flow)

# baseflow index
dt1 <- dtAll[,c("date","flow")]
dates <- as.data.frame(seq(min(dt1$date),max(dt1$date),by="day"))
names(dates) <- "date"
dt1 <- merge(dt1,dates,by="date",all.y=TRUE)
dt1 <- dt1[order(dt1$date),]
baseflow_index1 <- BFI(as.vector(dt1$flow))$BFI

# slope of flow duration curve
Fdc <- CalcFdc(dtAll, colName="flow")     #Calculate flow exceedance
flowSpline <- CalcFdcSpline(Fdc, colName = "flow")   #Calculate the spline
slope_fdc1 <- Calc_FDC_Slope(flowSpline)

# streamflow_elasticity
#mP <- mean(dtAll$p_mean)
#mQ <- mean(dtAll$flow)
#stream_elas1 <- quantile((dtAll$flow-mQ)/(dtAll$p_mean-mP)*mP/mQ,prob=0.5)
stream_elas1 <- StreamflowElasticity(dtAll)

# mean half-flow date
hfd_mean1 <- 0
for (y1 in years) {
  df1 <- subset(dtAll, WY==y1)
  df1 <- df1[order(df1$date),]
  df1 <- na.omit(df1)
  accum <- cumsum(df1$flow)
  hfd_mean1 <- hfd_mean1 + min(which(accum>=accum[length(accum)]/2))
}
hfd_mean1 <- hfd_mean1/length(years)

# Q5 and Q95
q5 <- quantile(dtAll$flow,prob=0.05)
q95 <- quantile(dtAll$flow,prob=0.95)

# frequency of high/low flow days
q_median <- quantile(dtAll$flow,prob=0.5)
high_q_freq1 <- sum(dtAll$flow > q_median*9)/length(years)
low_q_freq1 <- sum(dtAll$flow < q_mean1*0.2)/length(years)

# duration of high/low flow events
nd1 <- NULL; nd2 <- 0
for (j1 in 1:nrow(dtAll)) {
  if(dtAll$flow[j1] > q_median*9) {
    nd2 <- nd2+1
  } else {
    if (nd2>0) {nd1 <- c(nd1,nd2); nd2 <- 0}
  }
}
high_q_dur1 <- ifelse(is.null(nd1),0,mean(nd1))

nd1 <- NULL; nd2 <- 0
for (j1 in 1:nrow(dtAll)) {
  if(dtAll$flow[j1] < q_mean1*0.2) {
    nd2 <- nd2+1
  } else {
    if (nd2>0) {nd1 <- c(nd1,nd2); nd2 <- 0}
  }
}
low_q_dur1 <- ifelse(is.null(nd1),0,mean(nd1))

# frequency of days with Q=0
zero_q_freq1 <- round(sum(dtAll$flow<0.0001)/length(dtAll$flow)*100)

dtSS <- rbind(dtSS, data.table(
   ID=g1,domainID=domainID1,data_years=length(years),q_mean=q_mean1,
   runoff_ratio=runoff_ratio1, stream_elas=stream_elas1,
   slope_fdc=slope_fdc1, baseflow_index=baseflow_index1,
   hfd_mean=hfd_mean1, Q5=q5, Q95=q95,
   high_q_freq=high_q_freq1, high_q_dur=high_q_dur1,
   low_q_freq=low_q_freq1, low_q_dur=low_q_dur1,
   zero_q_freq=zero_q_freq1))
}

names(dtSS)[names(dtSS) == "domainID"] <- "id"
save(dtSS, file=paste0("../../output/ss_attrs_",region,".Rdata"))

}
