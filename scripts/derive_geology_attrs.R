# derive geologic attributes from GLHYMPS data

rm(list=ls())

library(zonal)
library(data.table)
library(sf)
library(raster)
library(terra)
library(stars)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson")
huc01$toid <- huc01$area_sqkm <- NULL

# load huc01 catchment raster (30m)
r1 <- rast("../datasets/elev/huc01_catchments_30m.tif")

# GLHYMPS data (note permeability is in log10 scale)
s1 <- st_read("../shapefile/GLHYMPS.gdb")
s1 <- st_transform(s1,crs(r1))

message("derive geology parameter rasters and save to disk")
grd <- st_as_stars(st_bbox(r1),nx=ncol(r1),ny=nrow(r1),values = NA)
pars <- c("geo_porosity","geo_permeability")
names(s1)[names(s1)=="Porosity"] <- pars[1]
names(s1)[names(s1)=="Permeability_permafrost"] <- pars[2]
for (p1 in pars) {
  message(p1)
  r2 <- st_rasterize(s1[,p1],grd)
  outfile <- paste0("../datasets/GLHYMPS_huc01/huc01_",p1,"_30m.tif")
  if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile),recursive=TRUE)
  write_stars(r2,outfile)
}

message("compute areal mean for huc01 catchments")
for (p1 in pars) {
  message(p1)
  outfile <- paste0("../datasets/GLHYMPS_huc01/huc01_",p1,"_30m.tif")
  r2 <- rast(outfile)
  huc01 <- execute_zonal(r2,geom=huc01,ID="id",FUN="mean",join=TRUE)
  names(huc01)[names(huc01)=="V1"] <- p1
  rm(r2); gc()
}

attrs <- as.data.table(huc01)
attrs$geometry <- NULL
save(attrs,file="../output/geo_attr_huc01_GLHYMPS.Rdata")

message("compute data coverage for each parameter (if there are data gaps)")
for (p1 in pars) {
  message(p1)
  outfile <- paste0("../datasets/GLHYMPS_huc01/huc01_",p1,"_30m.tif")
  r1 <- raster(outfile)
  r2 <- r1
  r2[] <- 1
  r2[is.na(r1)] <- 0
  r2 <- ratify(r2)
  rat <- levels(r2)[[1]]
  rat$cat <- c("invalid","valid")
  levels(r2) <- rat
  rcl <- dplyr::select(rat,from=ID,to=cat)
  prc0 <- execute_zonal(r2,geom=huc01,ID="id",FUN="freq",rcl=rcl, join=FALSE)
  prc1 <- subset(prc0,value=="valid")
  prc1$value <- NULL
  names(prc1) <- c("id",paste0(p1,"_coverage"))
  huc01 <- merge(huc01,prc1,by="id")
  
  rm(r1,r2); gc()
}

attrs <- as.data.table(huc01)
attrs$geometry <- NULL
save(attrs,file="../output/geo_attr_huc01_GLHYMPS.Rdata")

# check attribtue summary
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}

# plot the attributes (multiple panels, no legend)
plot(huc01[pars],border=NA)

# plot the attributes separately with legend
for (c1 in pars) {
  message(c1)
  png(filename = paste0("../figs/attr_",c1,"_huc01.png"),width = 5,height=5,units="in",res=300)
  print(plot(huc01[c1], border=NA, key.pos=1))
  dev.off()
}