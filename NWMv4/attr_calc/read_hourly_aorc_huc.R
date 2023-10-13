# read the hourly AORC data for each catchment from csv files into Rdata format

rm(list=ls())

library(data.table)

ver1 <- "1.2" #hydrofabric version
huc1 <- "12"

# hourly data for all forcing variables in individual csv files
dir1 <- "/apd_common/For_Liu_lumped_AORC"
files <- list.files(dir1,full.names=TRUE)

date1 <- "2012100100"; date2 <- "2021083123"
h1 <- as.POSIXct(date1,format="%Y%m%d%H")
h2 <- as.POSIXct(date2,format="%Y%m%d%H")
hours <- data.table(Time=seq(h1,h2,by="hour"))

vars <- c("APCP","Q2D","T2D","U2D","V2D","LWDOWN","SWDOWN","PSFC")

for (f1 in files) {

  cat1 <- gsub(".csv","",basename(f1))
  outfile <- paste0("../../datasets/AORC_hydrofabric_v",ver1,"/huc",huc1,"/aorc_hourly_",format(h1,"%Y%m%d%H",tz="UTC"),"_",format(h2,"%Y%m%d%H",tz="UTC"),"_",cat1,".Rdata")
  if (file.exists(outfile)) next
  if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile),recursive=TRUE)

  message(match(f1,files))
 
  aorc <- as.data.table(read.csv(f1,header=TRUE))
  aorc$Time <- as.POSIXct(aorc$Time, format="%Y-%m-%d %H:00:00",tz="UTC") 
  aorc <- merge(hours,aorc,by="Time",all.x=TRUE)
  aorc[, APCP:=round(RAINRATE * 3600,3)] # convert pcp from mm/s to mm
  for (var1 in c("LWDOWN","SWDOWN","T2D")) aorc[[var1]] <- round(aorc[[var1]],1)
  aorc[, Q2D:=round(Q2D,6)]
  for (var1 in c("U2D","V2D")) aorc[[var1]] <- round(aorc[[var1]],3)
  aorc[,PSFC:=round(PSFC)]

  aorc <- aorc[,vars,with=F]
  save(aorc,file=outfile)

}

