rm(list=ls())
library(data.table)
library(ggplot2)
library(sf)
library(terrain)
library(raster)
library(zonal)

huc01 <- st_read("shapefile/catchment_data.geojson")
cat1 <- subset(huc01,id=="cat-78")
cat1 <- subset(huc01,id=="cat-18694")

r1 <- raster("../datasets/nwm_geogrid_1km_blank.tif")
cat1 <- st_transform(cat1,crs(r1))

pcp <- execute_zonal("data/201804260800.RAINRATE.nc",geom=cat1,ID="id",join=FALSE)
pcp <- execute_zonal("data/201804260800.RAINRATE.nc",geom=huc01,ID="id",join=FALSE)

#dir0 <- "/local/ngen/data/huc01/huc_01/forcing/csv/"
#dir1 <- "/local/ngen/data/huc01/huc01_v1.2_2012_2021_AORC_forcings/csv_files"

cat_old <- 3739; cat_new <- 78
#cat_old <- 3882; cat_new <- 83
cat_old <- 34952; cat_new <- 16793
dt1 <- as.data.table(read.csv(paste0("data/cat-",cat_old,"_old.csv"),header=TRUE))
dt1[,Time:=as.POSIXct(Time, format="%Y-%m-%d %H:00:00",tz="UTC")]

dt2 <- as.data.table(read.csv(paste0("data/cat-",cat_new,".csv"),header=TRUE))
dt2[,Time:=as.POSIXct(Time, format="%Y-%m-%d %H:00:00",tz="UTC")]

dtAll <- merge(dt1,dt2,by="Time",suffixes=c(".old",".new"))
dtAll[,dif:=(RAINRATE.old - RAINRATE.new)*3600]

dt1 <- dtAll[,c("Time","RAINRATE.old","RAINRATE.new"),with=F]
names(dt1) <- c("Time","pcp.old","pcp.new")
dt1[,dif:=pcp.old - pcp.new]
dt1$dif <- dt1$dif * 3600
gg1 <- ggplot(dt1, aes(Time, dif)) +
    #geom_bar(stat="identity", na.rm = TRUE,color="blue") +
    geom_line(na.rm=TRUE,color="blue") +
    ggtitle(paste0("Difference (old-new) in hourly precipitation for cat-",cat_new)) +
    xlab("time") + ylab("Precipitation difference (mm)") +
    #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 16)) +
    theme(text = element_text(size=14))

ggsave(paste0("figs/pcp_ts_cat-",cat_new,".png"),width=10.5,height=5.5,units="in",dpi=300)