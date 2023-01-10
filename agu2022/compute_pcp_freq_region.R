# Compute precip frequence related parameters 

rm(list=ls())

library(data.table)

for (ver1 in c("v1.2","v0")) {
  
  if (ver1 == "v1.2") {
    date1 <- "20121001"; date2 <- "20210831"; years <- 2013:2020
  } else if (ver1 == "v0") {
    date1 <- "20070101"; date2 <- "20191231"; years <- 2008:2019
  }
  
hours1 <- seq(as.POSIXct(paste0(years[1]-1,"1001"),format="%Y%m%d",tz="UTC"),
              as.POSIXct(paste0(years[length(years)],"093023"),format="%Y%m%d%H",tz="UTC"),by="hour")
hours0 <- seq(as.POSIXct(date1,format="%Y%m%d",tz="UTC"),
              as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H",tz="UTC"),by="hour")
ix1 <- match(hours1,hours0)
if (sum(is.na(ix1))>0) stop("ERROR: dataset is not complete!")
dates1 <- sort(unique(format(hours1,"%Y%m%d",tz="UTC")))


# function for computing high/low precip duration
myfunc <- function(x) {
  y <- nchar(strsplit(paste(x,collapse=""),split="0")[[1]])
  y <- mean(y[y>0])
}

# hourly precip data
aorc <- get(load(paste0("../datasets/AORC_hydrofabric_",ver1,"/aorc_RAINRATE_huc01_",date1,"-",date2,"_hourly.Rdata")))

cats0 <- names(aorc)
dtPcpFreq <- data.table()
outfile <- paste0("output/",ver1,"/aorc_pcp_freq_duration_huc01_",years[1],"-",years[length(years)],".Rdata")
if (file.exists(outfile)) load(outfile)
cats1 <- cats0[!cats0 %in% dtPcpFreq$cat]

for (c1 in cats1) {

  n1 <- match(c1,cats1)
  message(paste0(n1,"   ",c1))
  
  pcp <- aorc[[c1]][ix1] #hourly pcp
  pcp <- colSums(matrix(pcp,nrow=24)) #daily pcp
  if (length(pcp) != length(dates1)) stop(paste0("ERROR: number of days with data not consistent"))
  
  dtPcpDaily <- data.table(date=dates1,pcp=pcp)
  p_mean <- mean(dtPcpDaily$pcp,na.rm=TRUE)
  dtPcpDaily[,high:=ifelse(pcp>=5*p_mean,1,0)]
  dtPcpDaily[,low:=ifelse(pcp<1,1,0)]
  
  dtPcpDaily[,year:=substr(date,1,4)]
  dtPcpDaily[,month:=substr(date,5,6)]
  dtPcpDaily[,year:=ifelse(as.numeric(month)>=10,as.numeric(year)+1,year)]
  hpf <- dtPcpDaily[,.(hpf=sum(high)),by=.(year)]
  hpf1 <- mean(hpf$hpf)
  lpf <- dtPcpDaily[,.(lpf=sum(low)),by=.(year)]
  lpf1 <- mean(lpf$lpf)
  
  hpd1 <- myfunc(dtPcpDaily$high)
  lpd1 <- myfunc(dtPcpDaily$low)
  
  dtPcpFreq <- rbind(dtPcpFreq, data.table(cat=c1,hpf=hpf1,lpf=lpf1,hpd=hpd1,lpd=lpd1))
  if (n1 %% 200 == 0) save(dtPcpFreq,file=outfile)
}

save(dtPcpFreq,file=outfile)
}
