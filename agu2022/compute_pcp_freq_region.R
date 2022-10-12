# Compute precip frequence related parameters 

rm(list=ls())

.libPaths("../R_lib")

library(data.table)
library(cluster)
library(doParallel)
library(foreach)

# function for computing high/low precip duration
myfunc <- function(x) {
  y <- nchar(strsplit(paste(x,collapse=""),split="0")[[1]])
  y <- mean(y[y>0])
}

# load daily pcp for huc01 catchments
dtPcpAll <- get(load("data/pcp_daily_huc01_2008-2021.Rdata"))
dates0 <- format(seq(as.POSIXct("20071001",format="%Y%m%d",tz="UTC"), 
              as.POSIXct("20210930",format="%Y%m%d",tz="UTC"),by="day"),"%Y%m%d", tzz="UTC")

# all catchments
cats0 <- unique(dtPcpAll$id)

# compute high/low precip fresquency and duration
dtPcpFreq <- data.table()
outfile <- "data/aorc_pcp_freq_duration_huc01_2008-2021.Rdata"
if (file.exists(outfile)) load(outfile)
cats1 <- cats0[!cats0 %in% dtPcpFreq$cat]

ncores <- 32 # set ncores to 1 to forgo parallelization
cl <- makeForkCluster(ncores)
registerDoParallel(cl)
dtAll <- foreach (i1=1:length(cats1),.combine=rbind) %dopar% {

#for (c1 in cats1) {

  #n1 <- match(c1,cats1)
  c1 <- cats1[i1]
  message(paste0(i1,"   ",c1))
  pcp <- subset(dtPcpAll, id==c1)
  pcp$id <- NULL

  dtPcpDaily <- data.table(date=dates0)
  dtPcpDaily <- merge(dtPcpDaily, pcp, by="date",all.x=TRUE)
  names(dtPcpDaily) <- c("date","pcp")
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
  
  #dtPcpFreq <- rbind(dtPcpFreq, data.table(cat=c1,hpf=hpf1,lpf=lpf1,hpd=hpd1,lpd=lpd1))
  #if (n1 %% 20 == 0 | c1==length(cats1)) save(dtPcpFreq,file=outfile)
  data.frame(cat=c1,hpf=hpf1,lpf=lpf1,hpd=hpd1,lpd=lpd1)
}
stopCluster(cl)

dtPcpFreq <- rbind(dtAll, as.data.table(dtAll))
save(dtPcpFreq,file=outfile)
