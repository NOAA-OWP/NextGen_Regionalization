# convert AROC in Rdata format to csv format for ngen simulations

library(data.table)

ver1 <- "2.0"; h1 <- "01" # hydrofabric version & huc region

date1 <- "2012100100"
date2 <- "2016093023"
date1 <- as.POSIXct(date1,format="%Y%m%d%H",tz="UTC")
date2 <- as.POSIXct(date2,format="%Y%m%d%H",tz="UTC")
dates <- format(seq(date1,date2,by="hour"),"%Y-%m-%d %H:%M:%S", tz="UTC")

date1 <- "2012100100"
date2 <- "2021083123"
date1 <- as.POSIXct(date1,format="%Y%m%d%H",tz="UTC")
date2 <- as.POSIXct(date2,format="%Y%m%d%H",tz="UTC")
dates0 <- format(seq(date1,date2,by="hour"),"%Y-%m-%d %H:%M:%S", tz="UTC")

dir1 <- paste0("../../datasets/AORC_hydrofabric_v",ver1,"/huc",h1)
files <- list.files(dir1,pattern=".Rdata",full.names=TRUE)
#dir2 <- "/local/ngen/data/huc01/huc01_v2.0_2012_2021_AORC_forcings/csv_files/"

for (f1 in files) {    
    aorc <- get(load(f1))
    outfile <- paste0("csv_files/",gsub("Rdata","csv",strsplit(basename(f1),split="_")[[1]][5]))
    if (file.exists(outfile)) next
    message(basename(f1))
    aorc$Time <- dates0
    aorc <- subset(aorc, Time %in% dates)
    aorc <- aorc[order(Time)]
    aorc[,RAINRATE:=APCP/3600]
    aorc <- aorc[,c("Time","RAINRATE","LWDOWN","SWDOWN","PSFC","Q2D","T2D","U2D","V2D"), with=FALSE]
    write.csv(aorc,file=outfile,quote=FALSE,row.names=FALSE)
}