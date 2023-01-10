# compare the mean annual pcp for HUC01 catchments calculated from NWMv3 data and NextGen data
# (all based on AORC)
rm(list=ls())

library(data.table)
library(sf)
library(zonal)
library(terra)
library(raster)
library(magrittr)
library(ggplot2)

# huc01 catchments
huc01 <- st_read("shapefile/catchment_data.geojson")

# nwm 1km raster (blank)
r1 <- raster("../datasets/nwm_geogrid_1km_blank.tif")

# nwm mean annual pcp
apcp <- get(load("../datasets/AORC_hydrofabric_v1.2/mean_annual_APCP_2013-2021_AORC.Rdata"))
apcp <- raster(t(apcp)[nrow(t(apcp)):1,])
crs(apcp) <- crs(r1); extent(apcp) <- extent(r1); res(apcp) <- res(r1)

pet <- get(load("../datasets/AORC_hydrofabric_v1.2/mean_annual_snow_frac_2013-2021.Rdata"))
pet <- raster(t(pet)[nrow(t(pet)):1,])
crs(pet) <- crs(r1); extent(pet) <- extent(r1); res(pet) <- res(r1)
pet1 <- execute_zonal(pet,huc01,ID="id",join=FALSE)
pet1 <- as.data.table(pet1)
names(pet1) <- c("id","pet")

# compute catchment mean pcp
apcp1 <- execute_zonal(apcp,huc01,ID="id",join=FALSE)
apcp1 <- as.data.table(apcp1)
names(apcp1) <- c("id","pcp")

# hourly apcp from NextGen data
aorc <- get(load("../datasets/AORC_hydrofabric_v1.2/aorc_huc01_20121001-20210831_hourly.Rdata"))
apcp2 <- sapply(aorc, function(x) sum(x)/9) #compute annual mean
apcp2 <- data.table(id=names(apcp2),pcp=apcp2)  

dt1 <- merge(apcp2,apcp1,by="id")
dt1[,dif:=pcp.x-pcp.y]
dt1[,dif_prc:=round(dif/pcp.x*100)]

range(abs(dt1$dif_prc),na.rm=T)

pcp <- execute_zonal("data/201904260800.RAINRATE.nc",geom=huc01,ID="id",join=FALSE)
pcp <- as.data.table(pcp)
names(pcp) <- c("id","pcp")
pcp$pcp <- pcp$pcp *3600

date1 <- "20121001"
date2 <- "20210831"
h1 <- as.POSIXct(date1,format="%Y%m%d")
h2 <- as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H")
hours <- data.frame(Time=seq(h1,h2,by="hour"))
ix1 <- which(hours$Time==as.POSIXct("2019042608",format="%Y%m%d%H",tz="UTC"))

pcp1 <- aorc %>% sapply("[",ix1)
pcp1 <- data.table(id=names(pcp1),pcp=pcp1)

pcpAll <- merge(pcp,pcp1,by="id",all=TRUE)
pcpAll[,dif:=abs(pcp.x-pcp.y)]

huc01 <- merge(huc01,pcpAll[,c("id","dif")])
huc <- subset(huc01,dif>0.1)
sf::sf_use_s2(FALSE)
sf1 <- st_union(huc01)

png("figs/cats_pcp_diff.png",width=6.5,height=5,units="in",res=300)
plot(sf1,border="grey",main="Catchments with different pcp \n at 2019-04-26 08:00:00")
plot(huc["dif"],border=NA,add=T)
dev.off()

