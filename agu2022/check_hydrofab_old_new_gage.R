rm(list=ls())

library(sf)

ver_donor <- "v1.2"
shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
shps_rec <- st_read("../datasets/gpkg_v0/catchment_data.geojson")

# receivers
plot(st_geometry(subset(shps_rec,id %in% c("cat-34950","cat-34952"))))
plot(st_geometry(subset(shps_rec,id %in% c("cat-34949"))),add=T,border="red")

# donors
dtDonorAll <- get(load("output/v0/donor_hlr_kge_dds_CFE.Rdata"))
subset(dtDonorAll,id %in% c("cat-34950","cat-34952","cat-34949"))
plot(st_geometry(subset(shps_don,id %in% c("cat-16581","cat-16793"))),add=T,border="blue")
plot(st_geometry(subset(shps_don,id %in% c("cat-16881"))),add=T,border="brown")


cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))
subset(cwt,id=="cat-16881")

gages_calib <- get(load(paste0("output/",ver_donor,"/calib_gages_screened_kge-dds.Rdata")))
gages_calib <- get(load("output/v1.2/calib_gages_screened_kge_dds.Rdata"))
tmp <- subset(cwt,id=="cat-16881")$gages
tmp[tmp %in% gages_calib[["cfe"]]]
load("../datasets/obsStrMeta_gages_retro.Rdata")
c1 <- subset(obsStrMeta,site_no==gage1)
plot(st_as_sf(c1,coords=c("dec_long_va","dec_lat_va"),crs=st_crs(shps_don)),add=T,pch=1,cex=3,col="darkgreen")
