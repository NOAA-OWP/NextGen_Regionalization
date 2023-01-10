# Read AORC 1km data and calcualte annual PET from
# temperature and longwave/shortwave radiation using the Hargreaves method
#
# IMPORTANT: the calculated PET is annual accumulation in MJ/m2,
# multiply by 0.408 to get mm
#
# Unit conversion notes:
# 1 Watt/m2 = 0.0036 MJ/m2
# 1 MJ/m2/day = 0.408 mm/day

rm(list=ls())

#library(doParallel)
#library(foreach)
library(data.table)

ver1 <- "v1.2"; years <- 2013:2020  # v1.2 aorc data is only up to 08/31/2021, i.e., 2021 is not complete
date1 <- "20121001"; date2 <- "20210831"

ver1 <- "v0"; years <- 2008:2019
date1 <- "20070101"; date2 <- "20191231"

hours1 <- seq(as.POSIXct(paste0(years[1]-1,"1001"),format="%Y%m%d",tz="UTC"),
              as.POSIXct(paste0(years[length(years)],"093023"),format="%Y%m%d%H",tz="UTC"),by="hour")
hours0 <- seq(as.POSIXct(date1,format="%Y%m%d",tz="UTC"),
              as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H",tz="UTC"),by="hour")
ix1 <- match(hours1,hours0)
if (sum(is.na(ix1))>0) stop("ERROR: dataset is not complete!")

t0 <- get(load(paste0("../datasets/AORC_hydrofabric_",ver1,"/aorc_T2D_huc01_",date1,"-",date2,"_hourly.Rdata")))
sw0 <- get(load(paste0("../datasets/AORC_hydrofabric_",ver1,"/aorc_SWDOWN_huc01_",date1,"-",date2,"_hourly.Rdata")))
lw0 <- get(load(paste0("../datasets/AORC_hydrofabric_",ver1,"/aorc_LWDOWN_huc01_",date1,"-",date2,"_hourly.Rdata")))
cats <- names(t0)

outfile <- paste0("output/",ver1,"/aorc_pet_huc01.Rdata")
if (file.exists(outfile)) {
  pet <- get(load(outfile))
} else {
  pet <- data.table()
}
cats <- cats[!cats %in% pet$cat]

#ncores <- 36
#cl <- parallel::makeForkCluster(ncores)
#doParallel::registerDoParallel(cl)
#pet <- foreach (i1=1:length(cats),.combine=rbind) %dopar% {

for (i1 in 1:length(cats)) {
  
  c1 <- cats[i1]
  message(paste0(i1," ", c1))
  
  # get hourly temperature data for catchment
  t1 <- data.table(date=hours1, value=t0[[c1]][ix1])
  t1[,yr:=format(date,"%Y")]
  t1[,mn:=format(date,"%m")]
  t1[,yr:=ifelse(mn>=10,as.numeric(yr)+1,yr)]
  t1[,day:=format(date,"%d")]
  # compute daily mean, min, and max
  t2 <- t1[,.(mean=mean(value),min=min(value),max=max(value)),by=.(yr,mn,day)]
  # average over month
  t2 <- t2[,.(mean=mean(mean)-273.15,min=mean(min)-273.15,max=mean(max)-273.15),by=.(yr,mn)]
  
  # hourly downward shortwave
  sw1 <- data.table(date=hours1, value=sw0[[c1]][ix1])
  sw1[,yr:=format(date,"%Y")]
  sw1[,mn:=format(date,"%m")]
  sw1[,yr:=ifelse(mn>=10,as.numeric(yr)+1,yr)]
  # accumulate monthly
  sw2 <- sw1[,.(sum=sum(value)*0.0036),by=.(yr,mn)]
  
  # hourly downward shortwave
  lw1 <- data.table(date=hours1, value=lw0[[c1]][ix1])
  lw1[,yr:=format(date,"%Y")]
  lw1[,mn:=format(date,"%m")]
  lw1[,yr:=ifelse(mn>=10,as.numeric(yr)+1,yr)]
  # accumulate monthly
  lw2 <- lw1[,.(sum=sum(value)*0.0036),by=.(yr,mn)]
  
  # Hargreaves method for PET (monthly)
  dt1 <- merge(t2,sw2,by=c("yr","mn"),all=TRUE)
  dt1 <- merge(dt1,lw2,by=c("yr","mn"),all=TRUE)
  dt1[,pet:=0.0023*(sum.x+sum.y)*(mean+17.8)*(max-min)^0.5]
  dt2 <- dt1[,.(pet=sum(pet)),by=.(yr)]
  
  #data.frame(cat=c1, pet=round(mean(dt2$pet)))
  pet <- rbind(pet,data.table(cat=c1, pet=round(mean(dt2$pet))))
  if ((i1 %% 500)==0) save(pet,file=outfile)
  
}
#parallel::stopCluster(cl)

save(pet, file=outfile)
