# derive landcover attributes from NLCD data (with gaps filled by NMW 1km data)

rm(list=ls())

library(zonal)
library(sf)
library(terra)
library(data.table)
library(ncdf4)
library(raster)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson")
huc01$area_sqkm <- huc01$toid <- NULL

# ncld land cover class
nlcd <- rast("../datasets/nlcd/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")

# percentage of each land cover class based on NLCD
# no need to reproject (execute_zonal likely handles that automatically?)
system.time({
  lc_prc <- execute_zonal(nlcd,geom=huc01,ID="id",FUN="freq",join=TRUE)
})
lc_prc$geometry <- NULL
lc_prc <- as.data.table(lc_prc)
save(lc_prc, file="../datasets/nlcd/nlcd_lc_perc_huc01.Rdata")

# fraction of various landcover classes from NLCD
strs <- c("urban", "forest","shrub","grassland","cropland","wetland")
ids <- list(21:24,41:43,52,71,81:82,c(90,95))
names(ids) <- strs
for (s1 in strs) {
  tmp <- as.data.table(subset(lc_prc,value %in% ids[[s1]]))
  tmp1 <- tmp[,.(prc=round(sum(percentage),2)),by=.(id)]
  names(tmp1) <- c("id",paste0(s1,"_nlcd"))
  huc01 <- merge(huc01, tmp1,by="id",all.x=TRUE)
  huc01[[paste0(s1,"_nlcd")]][is.na(huc01[[paste0(s1,"_nlcd")]])] <- 0
}

# value==0 means missing data in NLCD
l1 <- subset(lc_prc,value!=0)
nlcd_cov <- l1[,.(nlcd_coverage=sum(percentage)),by=.(id)]
huc01 <- merge(huc01,nlcd_cov,by="id",all.x=TRUE)

# save the computed attributes 
attrs <- as.data.table(huc01)
attrs$geometry <- NULL
save(attrs,file="../output/landcover_attr_huc01.Rdata")

#USGS 24 class land cover (1km) used by the NWM;   
f1 <- "../datasets/nwm_lc_1km/geo_em_conus_elev_landcover.nc"
nc <- nc_open(f1)

# NWM 1-km raster mask (to get raster crs, extent, resolution)
m0 <- raster("../data/geogrid_1km_blank.tif")

# read in NWM landuse fraction data
v1 <- ncvar_get(nc,"LANDUSEF")
s1 <- do.call(stack,lapply(1:dim(v1)[3], function(x) raster(t(v1[,,x])[nrow(t(v1[,,x])):1,])))
crs(s1) <- crs(m0); extent(s1) <- extent(m0); res(s1) <- res(m0)

# fraction of various landcover types from the 1-km NWM dataset
strs <- c("urban", "forest","shrub","grassland","cropland","wetland")
ids <- list(1,11:15,8:9,7,2:6,c(10,17:18))
for (str1 in strs) {
  r2 <- calc(s1[[ids[[match(str1,strs)]]]],sum)
  huc01 <- execute_zonal(rast(r2),geom=huc01,ID="id",FUN="mean",join=TRUE)
  huc01$V1 <- round(huc01$V1, 2)
  names(huc01)[names(huc01)=="V1"] <- paste0(ids[[str1]],"_nwm")
}

message("compute gvf parameters")
v1 <- ncvar_get(nc,"GREENFRAC")
s1 <- do.call(stack,lapply(1:dim(v1)[3], function(x) raster(t(v1[,,x])[nrow(t(v1[,,x])):1,])))
crs(s1) <- crs(m0); extent(s1) <- extent(m0); res(s1) <- res(m0)
r2 <- calc(s1,max)
r3 <- calc(s1,min)
r4 <- r2 - r3
huc01 <- execute_zonal(rast(r2),geom=huc01,ID="id",FUN="mean",join=TRUE)
huc01$V1 <- round(huc01$V1, 2)
names(huc01)[names(huc01)=="V1"] <- "gvf_max"
huc01 <- execute_zonal(rast(r4),geom=huc01,ID="id",FUN="mean",join=TRUE)
huc01$V1 <- round(huc01$V1, 2)
names(huc01)[names(huc01)=="V1"] <- "gvf_diff"

message("compute lai parameters")
v1 <- ncvar_get(nc,"LAI12M")
s1 <- do.call(stack,lapply(1:dim(v1)[3], function(x) raster(t(v1[,,x])[nrow(t(v1[,,x])):1,])))
crs(s1) <- crs(m0); extent(s1) <- extent(m0); res(s1) <- res(m0)
r2 <- calc(s1,max)
r3 <- calc(s1,min)
r4 <- r2 - r3
huc01 <- execute_zonal(rast(r2),geom=huc01,ID="id",FUN="mean",join=TRUE)
huc01$V1 <- round(huc01$V1, 2)
names(huc01)[names(huc01)=="V1"] <- "lai_max"
huc01 <- execute_zonal(rast(r4),geom=huc01,ID="id",FUN="mean",join=TRUE)
huc01$V1 <- round(huc01$V1, 2)
names(huc01)[names(huc01)=="V1"] <- "lai_diff"  

nc_close(nc)

# save the computed attributes 
attrs <- as.data.table(huc01)
attrs$geometry <- NULL
save(attrs,file="../output/landcover_attr_huc01.Rdata")

# check attribtue summary
pars <- names(huc01)
pars <- pars[!pars %in% c("id","geometry")]
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}

# compare land cover fractions between NWM and NLCD
for (str1 in strs) {
  message(str1)
  png(filename = paste0("../figs/attr_",str1,"_huc01.png"),width = 12,height=5,units="in",res=300)
  huc01[[paste0(str1,"_diff")]] <- abs(huc01[[paste0(str1,"_nlcd")]] - huc01[[paste0(str1,"_nwm")]])
  print(plot(huc01[c("nlcd_coverage",paste0(str1,c("_nlcd","_nwm","_diff")))], border=NA, key.pos=1))
  dev.off() 
}

# plot the gvf/lai attributes separately
pars <- c("gvf_max", "gvf_diff","lai_max","lai_diff")
for (c1 in pars) {
  message(c1)
  png(filename = paste0("../figs/attr_",c1,"_huc01.png"),width = 5,height=5,units="in",res=300)
  print(plot(huc01[c1], border=NA, key.pos=1))
  dev.off()
}


### NLCD land cover classes
#2019 Land Cover Class for the conterminous United States	Percentage
#11. Water	5.26
#12. Perennial Ice Snow	0.01
#21. Developed, Open Space	2.68
#22. Developed, Low Intensity	1.71
#23. Developed, Medium Intensity	1.03
#24. Developed High Intensity	0.37
#31. Bare Rock/Sand/Clay	0.97
#41. Deciduous Forest	9.29
#42. Evergreen Forest	11.51
#43. Mixed Forest	3.40
#52. Shrub/Scrub	21.85
#71. Grasslands/Herbaceous	13.34
#81. Pasture/Hay	6.24
#82. Cultivated Crops	16.31
#90. Woody Wetlands	4.50
#95. Emergent Herbaceous Wetlands	1.53
#Total	100.00%


######################################
#USGS 24-category Land Use Categories
######################################
#
#Land_Use_Category Land_Use_Description
#1 Urban and Built-up Land
#2 Dryland Cropland and Pasture
#3 Irrigated Cropland and Pasture
#4 Mixed Dryland/Irrigated Cropland and Pasture
#5 Cropland/Grassland Mosaic
#6 Cropland/Woodland Mosaic
#7 Grassland
#8 Shrubland
#9 Mixed Shrubland/Grassland
#10 Savanna
#11 Deciduous Broadleaf Forest
#12 Deciduous Needleleaf Forest
#13 Evergreen Broadleaf
#14 Evergreen Needleleaf
#15 Mixed Forest
#16 Water Bodies
#17 Herbaceous Wetland
#18 Wooden Wetland
#19 Barren or Sparsely Vegetated
#20 Herbaceous Tundra
#21 Wooded Tundra
#22 Mixed Tundra
#23 Bare Ground Tundra
#24 Snow or Ice
