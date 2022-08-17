rm(list=ls())

library(sf)
library(tidyr)
library(ggplot2)
library(data.table)

# HUC-01 boundary
#huc01 <- st_read("../shapefile/catchment_data.geojson")
#sf::sf_use_s2(FALSE)
#sf1 <- st_union(huc01)
#st_write(sf1,"../shapefile/huc01_boundary.shp")
huc01 <- st_read("../shapefile/huc01_boundary.shp")

# usgs gages shapefile
usgs <- st_read("../shapefile/usgs_gages_huc01.shp")

# load stats 
load("../stat/stat_retro_20091001_20140930.Rdata")

# QC stats
stats <- subset(stats_str, QMEAN>0 & t_n>1000)
stats[,nNSE:=1/(2-NSE)]
stat_names <- c("KGE","NSE","nNSE","CORR","PBIAS")
stat_names <- "PBIAS"
dt1 <- stats[, c("site_no","scenario",stat_names), with=FALSE]
dt1 <- na.omit(dt1)
dt1$PBIAS <- abs(dt1$PBIAS)

# simplify name of runs
runs <- c("CFE_default","CFE_donated","TOPMODEL_default","TOPMODEL_donated","mosaic_donated")
dt1$scenario <- factor(dt1$scenario)
levels(dt1$scenario) <- gsub("noah_owp_","",levels(dt1$scenario))
dt1$scenario <- factor(dt1$scenario,levels=runs)

usgs <- subset(usgs, gage %in% dt1$site_no)

cuts <- list(
          CORR=c(-0.2,0,0.2,0.4,0.5,0.6,0.7,0.8,1.0),
          NSE=c(-Inf,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
          nNSE=c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0),
          KGE=c(-Inf,-0.5,-0.2,0,0.2,0.4,0.6,0.8,1.0),
          PBIAS=c(0,20,40,60,80,100,200,300,Inf))

#myColors <- c("darkred","red","violetred1","violet","pink","lightgrey","lightblue","skyblue3", "royalblue","royalblue4","darkblue")
myColors <- c("darkred","red","violetred1","pink","skyblue3", "royalblue","royalblue4","darkblue")

for (s1 in stat_names) {
# add stats to usgs shapefile for plotting
dt2 <- dt1[,c("site_no","scenario",s1), with=FALSE]
dt2[[s1]] <- cut(dt2[[s1]],cuts[[s1]])
usgs1 <- merge(usgs, dt2, by.x="gage",by.y="site_no",all.x=TRUE)
names(usgs1)[names(usgs1)==s1] <- "stat"

colors1 <- myColors
if (s1=="PBIAS") colors1 <- rev(myColors)

gg1 <- ggplot() +
  #geom_sf(data = huc01,mapping = aes(fill=distAttr),color="grey") +
  geom_sf(data = usgs1, mapping=aes(color=stat), shape=20, size=2) +
  geom_sf(data = huc01,color="black",fill=NA) +
  scale_color_manual(name=s1,values=colors1) +
  scale_x_continuous(breaks = seq(-74, -67, by = 2)) +
  #theme_void() + #theme(strip.text = element_text(size = 20, color="white"))
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
     axis.ticks.y = element_blank(), axis.text.y = element_blank(),
     strip.text.x = element_text(size = 14),
     legend.text = element_text(size=12)) +
  facet_wrap(~scenario,ncol=5)
ggsave(paste0("../figs/map_huc01_",s1,".png"), gg1, width=12.5,height=5,units="in",dpi=300)
}