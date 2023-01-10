rm(list=ls())
library(sf)
library(data.table)
sf::sf_use_s2(FALSE)

gage_calib <- get(load("output/v1.2/calib_gages_screened_kge_dds.Rdata"))

# calibration catchments
ver_donor <- "v1.2"
shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))
sf1 <- st_union(shps_don)

# donor catchments
ver_receiver <- "v0"
shps_rec <- st_read(paste0("../datasets/gpkg_",ver_receiver,"/catchment_data.geojson"))

# gage meta
gage_meta <- get(load("../datasets/obsStrMeta_gages_retro.Rdata"))


for (gage1 in gages_calib[["cfe"]]) {

png(paste0("figs/calib_basin_v0_v1.2_crosswalk_",gage1,".png"),width=5.5,height=5,units="in",res=300)
cats1 <- subset(cwt, gages==gage1)$id
shps1 <- subset(shps_don, id %in% cats1)
shps2 <- st_crop(shps_rec,st_bbox(shps1))
ixs <- st_intersects(shps1,shps2,sparse = FALSE)

stop()
plot(st_geometry(shps1),border="grey",lwd=3,main=gage1)
plot(st_geometry(st_intersection(shps1,shps_rec)), border="red",add=T)
#plot(st_geometry(st_crop(shps_rec,st_bbox(shps1))),border="red",add=T)
plot(st_geometry(st_as_sf(subset(gage_meta,site_no==gage1),
    coords=c("dec_long_va","dec_lat_va"),crs=st_crs(shps_don))),
    add=T,pch=2,cex=2,col="darkgreen",lwd=2)
dev.off()
}
