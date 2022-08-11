# Compute precip frequence related parameters 

rm(list=ls())

library(data.table)

# function for computing high/low precip duration
myfunc <- function(x) {
  y <- nchar(strsplit(paste(x,collapse=""),split="0")[[1]])
  y <- mean(y[y>0])
}

# load daily pcp for huc01 catchments
aorcDaily <- get(load("../datasets/AORC/aorc_huc01_20070101-20191231_daily.Rdata"))
dates0 <- seq(as.POSIXct("20070101",format="%Y%m%d",tz="UTC"), as.POSIXct("20191231",format="%Y%m%d",tz="UTC"),by="day")

# compute high/low precip fresquency and duration
dtPcpFreq <- data.table()
for (c1 in names(aorcDaily)) {

  message(paste0(match(c1,names(aorcDaily)),"   ",c1))
  pcp <- aorcDaily[[c1]]
  if (length(pcp)!=length(dates0)) stop(paste0("Error: length of pcp data not consistent with dates0 for",c1))
  
  dtPcpDaily <- data.table(date=dates0,pcp=pcp)
  dtPcpDaily <- subset(dtPcpDaily,date>=date1 & date<=date2)
  p_mean <- mean(dtPcpDaily$pcp,na.rm=TRUE)
  dtPcpDaily[,high:=ifelse(pcp>=5*p_mean,1,0)]
  dtPcpDaily[,low:=ifelse(pcp<1,1,0)]
  
  dtPcpDaily[,year:=substr(date,1,4)]
  hpf <- dtPcpDaily[,.(hpf=sum(high)),by=.(year)]
  hpf1 <- mean(hpf$hpf)
  lpf <- dtPcpDaily[,.(lpf=sum(low)),by=.(year)]
  lpf1 <- mean(lpf$lpf)
  
  hpd1 <- myfunc(dtPcpDaily$high)
  lpd1 <- myfunc(dtPcpDaily$low)
  
  dtPcpFreq <- rbind(dtPcpFreq, data.table(cat=c1,hpf=hpf1,lpf=lpf1,hpd=hpd1,lpd=lpd1))
}

save(dtPcpFreq,file="../datasets/AORC/aorc_pcp_freq_duration_huc01_2008-2019.Rdata")

