rm(list=ls())

library(sf)

ver1 <- "1.2"; h1 <- "17" # hydrofabric version & huc region
ver1 <- "2.0pre"; h1 <- "12" # hydrofabric version & huc region

# v3 calib gages
meta <- as.data.table(read.csv("data/Domain_Meta_NWM_v3.0.csv",stringsAsFactors=FALSE))
meta <- meta[, c("gage_id", "lat", "lon","site_name"), with = FALSE]
meta <- meta[!duplicated(meta$gage_id)]
meta <- na.omit(meta)

# create sf points from gage lat/lon
sf_gages <- st_as_sf(meta, coords = c("lon","lat"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# v3 calibration basins & classification (for CONUS)
dt0 <- get(load("data/all_nwmv30_gages_classified.Rdata"))
all_gages <- as.character(dt0$gage)
donor_gages <- as.character(subset(dt0, Final_Classification=="donor")$gage)

# HUC catchments
gfile <- paste0('../../datasets/gpkg_v',ver1,'/nextgen_',h1,'.gpkg')
huc <- read_sf(gfile, "divides")

# transform sf_gages to be same crs as huc
sf_gages <- st_transform(sf_gages,st_crs(huc))

# get potential donor catchments in HUC
if (h1=="17") {
    cwt <- get(load(paste0("output/crosswalk_gage_cat_huc",h1,".Rdata")))
    gages0 <- unique(cwt$gages)
    gages1 <- gages0[gages0 %in% donor_gages]
    cwt1 <- subset(cwt, gages %in% gages1)
} else if (h1=="12") {
    gages0 <- sf_gages[lengths(st_intersects(sf_gages, huc))>0,]$gage_id
    gages1 <- gages0[gages0 %in% donor_gages]
    cwt1 <- data.table()
    for (g1 in gages1) {
        gfile1 <- paste0("../../datasets/gpkg_v",ver1,"/gages/huc",h1,"/",g1,".gpkg")
        if (file.exists(gfile1)) {
            cats1 <- read_sf(gfile1,"divides")
            cwt1 <- rbind(cwt1,data.table(id=cats1$id, gages=g1))
        }
    }
}

# plot huc catchments, donor catchments and gages
png(paste0("figs/map_donors_huc",h1,".png"),width=8, height=7,units="in", res=300)
plot(st_geometry(huc),border="grey",lwd=0.5,main=paste0("HUC",h1," donor catchments"))
plot(st_geometry(subset(huc, id %in% cwt1$id)), border="blue", lwd=0.5, add=T)
plot(st_geometry(subset(sf_gages, gage_id %in% gages1)),pch=24,col="red",bg="yellow",cex=0.8,add=T)
dev.off()