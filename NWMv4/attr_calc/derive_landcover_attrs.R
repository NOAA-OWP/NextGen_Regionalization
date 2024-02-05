# derive landcover attributes from NLCD data (with gaps filled by NMW 1km data)

rm(list=ls())

library(zonal)
library(sf)
library(terra)
library(data.table)
library(ncdf4)
library(raster)
source("correct_geojson.R")

vers <- "2.0"
hucs <- c("01")
for (ver1 in vers)  {
for (h1 in hucs) {

message(paste0("read geojson v", ver1," huc",h1))

# version 1.2
#huc <- st_read(paste0("../../datasets/gpkg_v",ver1,"/huc",h1,"/catchment_data.geojson"))
#huc <- correct_geojson(h1, huc)

# v2.0 pre-release
huc <- read_sf(paste0("../../datasets/gpkg_v",ver1,"/nextgen_",h1,".gpkg"),"divides")
huc$id <- NULL
names(huc)[names(huc)=="divide_id"] <- "id"

# ncld land cover class
nlcd <- rast("../../datasets/nlcd/nlcd_2019_land_cover_l48_20210604.img")

message("calculating percentage of each land cover class based on NLCD ...")
# use exact_extract since zonal does not work for fraction calculation for some reason
# reproject first since exactextractr::exact_extract does not automatically take care of that (even though zonal does)
huc <- st_transform(huc,crs(nlcd))
system.time({
  #lc_prc <- execute_zonal(nlcd,geom=huc1,ID="id",FUN="freq",join=FALSE)
  lc_prc <- exactextractr::exact_extract(nlcd,huc,fun="frac",append_cols = 'id', progress = TRUE,max_cells_in_memory=3e+07)
  lc_prc <- as.data.table(lc_prc)
})
save(lc_prc, file=paste0("../output_attr/nlcd_lc_perc_huc",h1,"_v",ver1,".Rdata"))

# initialize the attribute data table
attrs <- data.table(id=huc$id)

# fraction of various landcover classes from NLCD
strs <- c("urban", "forest","shrub","grassland","cropland","wetland","water")
ids <- list(21:24,41:43,52,71,81:82,c(90,95),11)
names(ids) <- strs
lc_prc <- melt(lc_prc,id.vars="id")
for (s1 in strs) {
  tmp <- subset(lc_prc,variable %in% paste0("frac_",ids[[s1]]))
  tmp1 <- tmp[,.(prc=round(sum(value),2)),by=.(id)]
  names(tmp1) <- c("id",paste0(s1,"_nlcd"))
  attrs <- merge(attrs, tmp1,by="id",all=TRUE)
  attrs[[paste0(s1,"_nlcd")]][is.na(attrs[[paste0(s1,"_nlcd")]])] <- 0
}

# frac_0 means missing data in NLCD
l1 <- subset(lc_prc,variable!="frac_0")
nlcd_cov <- l1[,.(nlcd_coverage=sum(value)),by=.(id)]
attrs <- merge(attrs,nlcd_cov,by="id",all=TRUE)

# save the computed attributes
outfile <- paste0("../output_attr/landcover_attr_huc",h1,"_v",ver1,".Rdata")
save(attrs,file=outfile)

#USGS 24 class land cover (1km) used by the NWM;   
f1 <- "../../datasets/nwm_lc_1km/geo_em_conus_elev_landcover.nc"
nc <- nc_open(f1)

# NWM 1-km raster mask (to get raster crs, extent, resolution)
m0 <- raster("../../datasets/nwm_geogrid_1km_blank.tif")

# read in NWM landuse fraction data
v1 <- ncvar_get(nc,"LANDUSEF")
s1 <- do.call(stack,lapply(1:dim(v1)[3], function(x) raster(t(v1[,,x])[nrow(t(v1[,,x])):1,])))
crs(s1) <- crs(m0); extent(s1) <- extent(m0); res(s1) <- res(m0)

# fraction of various landcover types from the 1-km NWM dataset
strs <- c("urban", "forest","shrub","grassland","cropland","wetland","water")
ids <- list(1,11:15,8:9,7,2:6,c(10,17:18),16)
for (str1 in strs) {
  message(paste0("NWM ", str1))
  r2 <- calc(s1[[ids[[match(str1,strs)]]]],sum)
  dt1 <- execute_zonal(rast(r2),geom=huc,ID="id",join=FALSE)
  dt1[[2]]<- round(dt1[[2]], 2)
  names(dt1)<- c("id",paste0(str1,"_nwm"))
  attrs <- merge(attrs, dt1, by="id", all=TRUE)
}
save(attrs,file=outfile)

message("compute gvf parameters")
v1 <- ncvar_get(nc,"GREENFRAC")
s1 <- do.call(stack,lapply(1:dim(v1)[3], function(x) raster(t(v1[,,x])[nrow(t(v1[,,x])):1,])))
crs(s1) <- crs(m0); extent(s1) <- extent(m0); res(s1) <- res(m0)
r2 <- calc(s1,max)
r3 <- calc(s1,min)
r4 <- r2 - r3
dt1 <- execute_zonal(rast(r2),geom=huc,ID="id",join=FALSE)
dt1[[2]] <- round(dt1[[2]], 2)
names(dt1) <- c("id","gvf_max")
attrs <- merge(attrs, dt1, by="id", all=TRUE)

dt1 <- execute_zonal(rast(r4),geom=huc,ID="id",join=FALSE)
dt1[[2]] <- round(dt1[[2]], 2)
names(dt1) <- c("id","gvf_diff")
attrs <- merge(attrs, dt1, by="id", all=TRUE)

message("compute lai parameters")
v1 <- ncvar_get(nc,"LAI12M")
s1 <- do.call(stack,lapply(1:dim(v1)[3], function(x) raster(t(v1[,,x])[nrow(t(v1[,,x])):1,])))
crs(s1) <- crs(m0); extent(s1) <- extent(m0); res(s1) <- res(m0)
r2 <- calc(s1,max)
r3 <- calc(s1,min)
r4 <- r2 - r3
dt1 <- execute_zonal(rast(r2),geom=huc,ID="id",join=FALSE)
dt1[[2]] <- round(dt1[[2]], 2)
names(dt1) <- c("id","lai_max")
attrs <- merge(attrs, dt1, by="id", all=TRUE)

dt1 <- execute_zonal(rast(r4),geom=huc,ID="id",join=FALSE)
dt1[[2]] <- round(dt1[[2]], 2)
names(dt1) <- c("id","lai_diff")
attrs <- merge(attrs, dt1, by="id", all=TRUE)
save(attrs,file=outfile)

nc_close(nc)

# check attribtue summary
pars <- names(attrs)
pars <- pars[pars !="id"]
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}

# compare land cover fractions between NWM and NLCD
huc <- merge(huc, attrs, by="id", all=TRUE)
huc <- merge(huc, attrs, by="id", all=TRUE)
for (str1 in strs) {
  message(str1)
  f1 <- paste0("../figs/attrs/huc",h1,"_v",ver1,"/attr_",str1,"_huc",h1,"_v",ver1,".png")
  if (!dir.exists(dirname(f1))) dir.create(dirname(f1))
  png(filename = f1,width = 12,height=5,units="in",res=300)
  huc[[paste0(str1,"_diff")]] <- abs(huc[[paste0(str1,"_nlcd")]] - huc[[paste0(str1,"_nwm")]])
  print(plot(huc[c("nlcd_coverage",paste0(str1,c("_nlcd","_nwm","_diff")))], border=NA, key.pos=1))
  dev.off() 
}

# plot the gvf/lai attributes separately
pars <- c("gvf_max", "gvf_diff","lai_max","lai_diff")
for (c1 in pars) {
  message(c1)
  f1 <- paste0("../figs/attrs/huc",h1,"_v",ver1,"/attr_",c1,"_huc",h1,"_v",ver1,".png")
  if (!dir.exists(dirname(f1))) dir.create(dirname(f1))
  png(filename = f1,width = 5,height=5,units="in",res=300)
  print(plot(huc[c1], border=NA, key.pos=1))
  dev.off()
}
} # loop huc
} # loop hydrogabric version

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