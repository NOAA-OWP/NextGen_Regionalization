
library(sf)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson") 
#st_write(huc01,"../shapefile/huc01_catchments.shp")

# read in headwater catchments
cats <- read.table("../data/headwater_catchments_huc01.txt",header=TRUE,colClasses = "character")
huc <- subset(huc01,id %in% cats$catchment)
huc$gage <- cats$gages[match(huc$id,cats$catchment)]
st_write(huc,"../shapefile/huc01_headwater.shp",delete_layer = TRUE)

# create usgs gages shapefile
f1 <- "../data/usgs_stations_all_2021-08-23.txt"
gage_meta <- read.csv(f1,skip=32,header=TRUE,sep="\t",stringsAsFactors=FALSE,colClasses=c(rep("character",3),rep("numeric",2),rep("character",2),"numeric",rep("character",3),"numeric","numeric"))
gages <- gage_meta[,c("site_no","dec_lat_va","dec_long_va")]
names(gages) <- c("gage","lat","lon")
gages <- subset(gages, !is.na(lat) & !is.na(lon))
usgs <- st_as_sf(gages,coords=c("lon","lat"),crs=st_crs(huc))
usgs$huc8 <- gage_meta$huc_cd[match(gages$gage,gage_meta$site_no)]
usgs$name <- gage_meta$station_nm[match(gages$gage,gage_meta$site_no)]
st_write(usgs,"../shapefile/usgs_gages_conus.shp")

usgs1 <- subset(usgs,substr(huc8,1,2)=="01")
st_write(usgs1,"../shapefile/usgs_gages_huc01.shp")


# select a couple multi-catchment basins for illustration
basin1 <- subset(huc01,id %in% c("cat-33311","cat-33314","cat-33315","cat-33158"))
st_write(basin1,"../shapefile/basin1.shp")

cat1 <- subset(huc01,id %in% c("cat-33339","cat-33345"))
st_write(cat1,"../shapefile/cat1.shp")

cat2 <- subset(huc01,id %in% c("cat-33190"))
st_write(cat2,"../shapefile/cat2.shp")

cat3 <- subset(huc01,id %in% c("cat-33311","cat-33314","cat-33315"))
st_write(cat3,"../shapefile/cat3.shp")

cat4 <- subset(huc01,id %in% c("cat-33158"))
st_write(cat4,"../shapefile/cat4.shp")

hw1 <- subset(huc01,id %in% c("cat-33375"))
st_write(hw1,"../shapefile/hw1.shp")
hw2 <- subset(huc01,id %in% c("cat-33305"))
st_write(hw2,"../shapefile/hw2.shp")
               

# camels
camels <- st_read("../shapefile/camels_basins.shp")
camels <- subset(camels, ID %in% gages$gage)
camels$huc8 <- usgs$huc8[match(camels$ID, usgs$gage)]
camels1 <- subset(camels,substr(huc8,1,2)=="01")
st_write(camels1,"../shapefile/camels_huc01.shp")

# nwm v2.1 calibration basins
calib <- st_read("../shapefile/Final_Calib_V2.1_2018_9_30.shp")
calib$huc8 <- usgs$huc8[match(calib$ID, usgs$gage)]
calib1 <- subset(calib,substr(huc8,1,2)=="01")
st_write(calib1,"../shapefile/calib_nwm_v21_huc01.shp")

nrow(usgs1)
nrow(camels1)
nrow(calib1)


