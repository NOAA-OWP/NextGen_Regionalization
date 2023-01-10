# plot the stats from different simulations on spatial map for intercomparison

rm(list=ls())

library(sf)
library(tidyr)
library(ggplot2)
library(data.table)

sf::sf_use_s2(FALSE)

gage_calib <- get(load("output/v0/calib_gages_screened_kge_nse.Rdata"))

# calibration catchments
ver_donor <- "v1.2"
shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
sf1 <- st_union(shps_don)

# gage meta
gage_meta <- get(load("../datasets/obsStrMeta_gages_retro.Rdata"))

# load stats 
load("stat/stat_retro_20131001_20160930.Rdata")

# QC stats
stats <- subset(stats_str, QMEAN>0 & t_n>1000)
stats[,nNSE:=1/(2-NSE)]
stat_names <- c("KGE","NSE","nNSE","CORR","PBIAS")
dt1 <- stats[, c("site_no","scenario",stat_names), with=FALSE]
dt1 <- na.omit(dt1)
dt1$PBIAS <- abs(dt1$PBIAS)
gages_all <- unique(dt1$site_no)

dt1$scenario <- gsub("CFE[+]TOPMODEL","Mixed",dt1$scenario)
scenarios <- c("CFE_camels_kge","CFE_camels_nse","Mixed_camels_nse")
dt1 <- subset(dt1, scenario %in% scenarios)
dt1$scenario <- factor(dt1$scenario,levels=scenarios)
levels(dt1$scenario) <- c("CFE_kge_camels","CFE_nse_camels","Mixed_nse_camels")

gage_meta <- gage_meta[,c("site_no","dec_long_va","dec_lat_va")]
shp_gages <- st_as_sf(subset(gage_meta,site_no %in% gages_all),
         coords=c("dec_long_va","dec_lat_va"),crs=st_crs(shps_don))

cuts <- list(
          CORR=c(-0.2,0,0.2,0.4,0.5,0.6,0.7,0.8,1.0),
          NSE=c(-Inf,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
          nNSE=c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0),
          KGE=c(-Inf,-0.5,-0.2,0,0.2,0.4,0.6,0.8,1.0),
          PBIAS=c(0,20,40,60,80,100,200,300,Inf))

#myColors <- c("darkred","red","violetred1","violet","pink","lightgrey","lightblue","skyblue3", "royalblue","royalblue4","darkblue")
myColors <- c("darkred","red","violetred1","pink","skyblue3", "royalblue","royalblue4","darkblue")

#for (s1 in stat_names) {
for (s1 in "KGE") {
    
# add stats to usgs shapefile for plotting
dt2 <- dt1[,c("site_no","scenario",s1), with=FALSE]
dt2[[s1]] <- cut(dt2[[s1]],cuts[[s1]])
usgs1 <- merge(shp_gages, dt2, by="site_no",all.x=TRUE)
names(usgs1)[names(usgs1)==s1] <- "stat"

ns1 <- length(unique(usgs1$scenario))
colors1 <- myColors
if (s1=="PBIAS") colors1 <- rev(myColors)

gg1 <- ggplot() +
  geom_sf(data = sf1,color="black",fill=NA) +
  geom_sf(data = subset(usgs1,!site_no %in% gages_calib), mapping=aes(color=stat), shape=20, size=3) +
  geom_sf(data = subset(usgs1,site_no %in% gages_calib), mapping=aes(color=stat), shape=2, size=3,stroke=1.2) +
  #geom_sf(data = subset(usgs1, site_no=="01013500"),mapping=aes(color=stat), shape=3, size=3,stroke=1.2) +
  #geom_sf(data = subset(usgs1,site_no %in% gages_calib), shape=2, color="green",size=3,stroke=1.2) +
  scale_color_manual(name=s1,values=colors1) +
  scale_x_continuous(breaks = seq(-74, -67, by = 2)) +
  #theme_void() + #theme(strip.text = element_text(size = 20, color="white"))
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
     axis.ticks.y = element_blank(), axis.text.y = element_blank(),
     strip.text.x = element_text(size = 14),
     legend.text = element_text(size=12)) +
  facet_wrap(~scenario,ncol=ns1)
ggsave(paste0("figs/map_huc01_",s1,"_valid.png"), gg1, width=12.5,height=5,units="in",dpi=300)
}