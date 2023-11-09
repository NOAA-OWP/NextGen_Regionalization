# derive geologic attributes from GLHYMPS data

rm(list=ls())

library(zonal)
library(data.table)
library(sf)
library(raster)
library(terra)
library(stars)
source("correct_geojson.R")

geol_raster <- FALSE # if geologic property raters already exist
pars <- c("geo_porosity","geo_permeability")

vers <- "2.0pre"
hucs <- c("12")
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

attrs <- data.table(id=huc$id)
fout <- paste0("../output_attr/geo_attr_huc",h1,"_v",ver1,".Rdata")
if (file.exists(fout)) attrs <- get(load(fout))

# if the rasters do not exist yet, rasterize the shapefiles
if (!geol_raster) {
  
message("load DEM (30m) for raster template")
r1 <- rast(paste0("../../datasets/elev/DEM/ned",h1,".tif"))

# GLHYMPS data (note permeability is in log10 scale)
s1 <- st_read("../../datasets/shapefile/GLHYMPS.gdb")
s1 <- st_transform(s1,crs(r1))

message("derive geology parameter rasters and save to disk")
names(s1)[names(s1)=="Porosity"] <- pars[1]
names(s1)[names(s1)=="Permeability_permafrost"] <- pars[2]
for (p1 in pars) {
  message(p1)
  r2 <- st_rasterize(s1[,p1],st_as_stars(st_bbox(r1),nx=ncol(r1),ny=nrow(r1),values = NA))
  outfile <- paste0("../../datasets/GLHYMPS_huc",h1,"_v",ver1,"/huc",h1,"_",p1,"_30m.tif")
  if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile),recursive=TRUE)
  write_stars(r2,outfile)
}
}

message("compute areal mean all catchments")
for (p1 in pars) {
  message(p1)
  outfile <- paste0("../../datasets/GLHYMPS_huc",h1,"_v",ver1,"/huc",h1,"_",p1,"_30m.tif")
  r2 <- rast(outfile)
  dt1 <- execute_zonal(r2,geom=huc,ID="id",join=FALSE) #if join=TRUE, some NA entries would be removed
  names(dt1) <- c("id", p1)
  attrs <- merge(attrs, dt1,by="id",all=TRUE)  
}
save(attrs,file=fout)

message("compute data coverage for each parameter (if there are data gaps)")
for (p1 in pars) {
  message(p1)
  outfile <- paste0("../../datasets/GLHYMPS_huc",h1,"_v",ver1,"/huc",h1,"_",p1,"_30m.tif")
  r1 <- rast(outfile)
  r1[!is.na(r1)] <- 1
  r1[is.na(r1)] <- 0
  # use exact_extract since zonal does not work for fraction calculation for some reason
  huc <- st_transform(huc,crs(r1))
  prc1 <- exactextractr::exact_extract(r1,huc,fun="frac",append_cols = 'id', progress = TRUE,max_cells_in_memory=3e+07)
  prc1$frac_0 <- NULL
  names(prc1) <- c("id",paste0(p1,"_coverage"))
  attrs <- merge(attrs,prc1,by="id",all=TRUE)  
  rm(r1); gc()
}
save(attrs,file=fout)

# check attribtue summary
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}
huc <- merge(huc,attrs,by="id",all=TRUE)

# plot the attributes (multiple panels, no legend)
#plot(huc01[pars],border=NA)

# plot the attributes separately with legend
for (c1 in pars) {
  message(c1)
  png(filename = paste0("figs/attr_",c1,"_huc",h1,"_v",ver1,".png"),width = 5,height=5,units="in",res=300)
  print(plot(huc[c1], border=NA, key.pos=1))
  dev.off()
}
}
}