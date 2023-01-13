# plot the upstream catchments of calibration basins in the old and new hydrofabrics 

rm(list=ls())

library(sf)
library(data.table)

# HUC-01 catchments (old version)
ver_receiver <- "v0"
shps_rec <- st_read(paste0("../datasets/gpkg_",ver_receiver,"/catchment_data.geojson"))

# HUC-01 nexus (old version)
nex01 <- st_read(paste0("../datasets/gpkg_",ver_receiver,"/nexus_data.geojson"))

# shapefile of USGS gages in HUC-01
# usgs1 <- st_read("../shapefile/usgs_gages_huc01.shp")

# calibration basins
scenario <- "kge"
f1 <- paste0("data/calib_",scenario,"_dds/cfe_noah_",scenario,"_valid_stat.csv")
dtStat <- read.csv(f1,header=TRUE,colClasses=c("site_no"="character"))
gages0 <- unique(dtStat$site_no)

# usgs gage info
load("../datasets/gpkg_v0/obsStrMeta_gages_retro.Rdata")
usgs1 <- subset(obsStrMeta,site_no %in% gages0)
usgs1 <- usgs1[,c("site_no","dec_lat_va","dec_long_va")] 
names(usgs1) <- c("gage","lat","lon")
usgs1 <- st_as_sf(usgs1,coords=c("lon","lat"),crs=st_crs(shps_rec))

# identify the catchments where gages are located
sf::sf_use_s2(FALSE)
out <- st_intersection(usgs1,shps_rec)

# trace all upstream catchments and nexus for each gage
catchments <- nexus <- vector("list",nrow(usgs1))
names(catchments) <- names(nexus) <- usgs1$gage
for (i1 in 1:nrow(out)) {
  message(i1)
  g1 <- out$gage[i1]
  cat1 <- out$id[i1]
  cats <- nexs <- NULL
  while(1) {
    cats <- c(cats,cat1)
    cats <- unique(cats)
    nex1 <- nex01$id[unlist(sapply(cat1,function(x) which(nex01$toid==x)))]
    nexs <- c(nexs, nex1)
    if (length(nex1)==0) break
    cat1 <- shps_rec$id[unlist(sapply(nex1, function(x) which(shps_rec$toid==x)))]
    cat1 <- unique(cat1)
  }
  catchments[[g1]] <- cats
  nexus[[g1]] <- nexs
}

# number of cathcments for each usgs gage
nc1 <- sapply(catchments,function(x) length(x))

# summary
print(table(nc1))

save(catchments,file="data/calib_gage_catchments_huc01.Rdata")

# =============== below is extra testing code
ver_donor <- "v1.2"
shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))

# plot old and new hydrfabric for the gage
for (gage1 in gages0) {
  png(paste0("figs/calib_basin_v0_v1.2_crosswalk_",gage1,"_new.png"),width=5.5,height=5,units="in",res=300)
  plot(st_geometry(subset(shps_rec, id %in% catchments[[gage1]])),lwd=4,border="grey",main=gage1)
  plot(st_geometry(subset(shps_don,id %in% subset(cwt,gages==gage1)$id)),add=T,border="red")
  plot(st_geometry(subset(out,gage==gage1)),add=T,pch=2,color="green",cex=2)
  dev.off()
}


cwt1 <- as.data.table(read.csv("../datasets/gpkg_v0/nhd-crosswalk.csv"))
cwt1 <- subset(cwt1,gages!="")
gages <- cwt$gages
gages <- strsplit(cwt1$gages,split=",")
gages <- lapply(gages, function(x) unique(x))
cwt1 <- subset(cwt1,sapply(gages,function(x) length(x))==1)
cwt1 <- cwt1[,c("ID","gages")]
cwt1 <- as.data.table(cwt1[!duplicated(cwt1),])
cwt1 <- subset(cwt1,!(duplicated(gages)))
cwt1 <- cwt1[,.(gage=unique(strsplit(gages,split=","))[[1]]),by=.(ID)]
cwt1 <- cwt1[!duplicated(cwt1),]


# single-cathcment basins
print(data.table::data.table(catchment=names(catchments[nc1==1]),gage=as.vector(catchments[nc1==1])))


# plot the catchments,nexus and gage for a given basin (using plot, no labels)
i0 <- 300
i1 <- which(huc01$id %in% catchments[[out$gage[i0]]])
i2 <- which(usgs1$gage==out$gage[i0])
i3 <- which(nex01$id %in% nexus[[out$gage[i0]]])
str1 <- paste0(usgs1$gage[i2],"  ", usgs1$name[i2])
plot(st_geometry(huc01[i1,]), col = "grey", border = 'black', main=str1,axes = TRUE)
plot(st_geometry(usgs1[i2,]), pch = 17, cex=1.5, col = 'red', add = TRUE)
plot(st_geometry(nex01[i3,]), pch = 16, col = 'blue', add = TRUE)


# using ggplot2
library(ggplot2)
ggplot(huc01[i1,]) +
  geom_sf() +
  geom_sf(data=usgs1[i2,],color="red",shape=17,size=3) +
  geom_sf(data=nex01[i3,],color="blue",shape=16,size=3) +
  geom_sf_label(aes(label = id)) +
  geom_sf_label(data=usgs1[i2,],aes(label=gage),color="red",nudge_x = 0.02) +
  geom_sf_label(data=nex01[i3,],aes(label=id),color="blue",nudge_x = 0.02) +
  ggtitle(str1) + xlab("") +ylab("") +
  theme(plot.title=element_text(size=11))

  
