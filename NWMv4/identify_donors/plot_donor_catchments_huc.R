# plot all donor catchments for a given HUC and hydrofabric version

rm(list=ls())

library(sf)

# hydrofabric version & huc region
ver1 <- "v1.2"; h1 <- "17"
ver1 <- "v2.0pre"; h1 <- "12" 

# v3 calib gage meta data
meta <- data.table::as.data.table(read.csv("data/Domain_Meta_NWM_v3.0.csv",stringsAsFactors=FALSE))
meta <- meta[, c("gage_id", "lat", "lon","site_name"), with = FALSE]
meta <- meta[!duplicated(meta$gage_id)]
meta <- na.omit(meta)

# create sf points from gage lat/lon
sf_gages <- st_as_sf(meta, coords = c("lon","lat"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# v3 calibration basins & classification (for CONUS)
dt0 <- get(load("data/all_nwmv30_gages_classified.Rdata"))
all_gages <- as.character(dt0$gage)
donor_gages <- as.character(subset(dt0, Final_Classification=="donor")$gage)
write.table(donor_gages, file="data/donor_gages_nwmv30.csv",quote=FALSE, row.names=FALSE,col.names=FALSE)

# HUC catchments
gfile <- paste0('../../datasets/gpkg_',ver1,'/nextgen_huc',h1,'.gpkg')
huc <- read_sf(gfile, "divides")
id_str <- "id"; if (ver1=="v2.0pre") id_str <- "divide_id"

# transform sf_gages to be same crs as huc
sf_gages <- st_transform(sf_gages,st_crs(huc))

# get potential donor catchments in HUC
cwt <- read.csv(paste0("data/crosswalk_gage_cat_huc",h1,"_",ver1,".csv"), colClasses=c(rep("character",3),"numeric","character"))
gages0 <- unique(cwt$gages)
gages1 <- gages0[gages0 %in% donor_gages]
cwt <- subset(cwt, gages %in% gages1)

# plot huc catchments, donor catchments and gages
png(paste0("figs/map_donors_huc",h1,"_",ver1,".png"),width=8, height=7,units="in", res=300)
plot(st_geometry(huc),border="grey",lwd=0.5,main=paste0("HUC",h1," donor catchments"))
plot(st_geometry(subset(huc, get(id_str) %in% cwt$id)), border="blue", lwd=0.5, add=T)
plot(st_geometry(subset(sf_gages, gage_id %in% gages1)),pch=24,col="red",bg="yellow",cex=0.8,add=T)
dev.off()