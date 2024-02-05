# Derive topographic attributes from 30m SRTM elevation data

rm(list=ls())

library(data.table)
library(zonal)
library(sf)
library(terra)
library(raster)
library(units)
library(stars)

source("correct_geojson.R")

vers <- "2.0"
hucs <- c("01")
for (ver1 in vers)  {
for (h1 in hucs) {

message(paste0("read geojson v", ver1," huc",h1))

# version 1.2
#huc <- st_read(paste0("../../datasets/gpkg_v",ver1,"/huc",h1,"/catchment_data.geojson"))
#huc <- correct_geojson(h1, huc)

# v2.0 and v2.0 pre-release
huc <- read_sf(paste0("../../datasets/gpkg_v",ver1,"/nextgen_",h1,".gpkg"),"divides")
huc$id <- NULL
names(huc)[names(huc)=="divide_id"] <- "id"

# initialize attribute data table
fout <- paste0("../output_attr/topo_attr_huc",h1,"_v",ver1,".Rdata")
attrs <- data.table(id = huc$id)
if (file.exists(fout)) load(fout)

str1 <- "areasqkm"
if (!str1 %in% names(huc)) str1 <- "area_sqkm"
attrs$areasqkm <- huc[[str1]]

message("circularity index")
attrs$cidx <- attrs$areasqkm/(drop_units(st_length(st_cast(huc,"MULTILINESTRING")))/1000)^2*4*pi

file1 <- paste0("../../datasets/elev/DEM/ned",h1,".tif")
message(paste0("read elevation data into SpaRaster: ", file1))
if (!file.exists(file1)) stop(paste0("File does not exist: ", file1))
elev0 <- rast(file1)/100 # the original elevation data is in cm; convert to meters

message("compute slope")
slope <- terra::terrain(elev0,'slope',unit='radians')
dt1 <- execute_zonal(slope,geom=huc,ID="id",join=FALSE, progress=TRUE)
attrs <- merge(attrs, dt1, by="id", all.x=TRUE)
save(attrs, file=fout)

message("compute mean elevation")
dt1 <- execute_zonal(elev0,geom=huc,ID="id",join=FALSE, progress=TRUE)
names(dt1)[2] <- "elev_mean"
attrs <- merge(attrs, dt1, by="id",all.x=TRUE)
save(attrs, file=fout)

# relief
message("compute max elevation")
dt1 <- execute_zonal(elev0,geom=huc,ID="id",fun="max",join=FALSE, progress=TRUE)
names(dt1) <- c("id", "maxelev")

message("compute min elevation")
tmp <- execute_zonal(elev0,geom=huc,ID="id",fun="min",join=FALSE, progress=TRUE)
names(tmp) <- c("id", "minelev")
dt1 <- merge(dt1,tmp,by="id",all=TRUE)

message("compute relief")
dt1[,relief:=maxelev-minelev]

save(dt1, file="dt1.Rdata")

message("calculate the middle elevation raster")
dt1[,midelev:=(minelev+maxelev)/2]
huc <- merge(huc,dt1[,c("id","midelev"),with=F],by="id",all.x=TRUE)
huc1 <- subset(huc[,'midelev'],!is.na(midelev))
huc1 <- st_transform(huc1,crs(elev0))
r_midelev <- st_rasterize(huc1,st_as_stars(st_bbox(elev0), nx = ncol(elev0), ny = nrow(elev0), crs=crs(elev0),values = NA_real_))
r_midelev <- rast(r_midelev)

message("categorize upland/lowland and flatland")
r1 <- slope
r1[] <- NA
r1[slope<=0.01 & elev0>r_midelev] <- 1     #flat upland
r1[slope<=0.01 & elev0<=r_midelev] <- 2    #flat lowland
r1[slope>0.01 & elev0>r_midelev] <- 3      #nonflat upland
r1[slope>0.01 & elev0<=r_midelev] <- 4     #nonflat lowland

cls <- data.frame(id=1:4, cat=c("flat_upland","flat_lowland","nonflat_upland","nonflat_lowland"))
levels(r1) <- cls

message("compute precentage of flatland in upland and lowland areas")
#prc1 <- execute_zonal(r1,geom=huc,ID="id",FUN="frac",rcl=cls, join=FALSE)
#prc1 <- execute_zonal(r1,geom=huc,ID="id", FUN="frac", join=FALSE)
# use exact_extract since zonal does not work for fraction calculation for some reason
huc <- st_transform(huc,crs(r1))
prc1 <- exactextractr::exact_extract(r1,huc,fun="frac",append_cols = 'id', progress = TRUE,max_cells_in_memory=3e+07)
#save(prc1, file="prc1.Rdata")
prc2 <- data.table(na.omit(prc1))
for (i1 in 1:4) names(prc2)[names(prc2)==paste0("frac_",i1)] <- subset(cls,id==i1)$cat
prc2[,prcFlatTotal:=flat_lowland+flat_upland]
prc2[,prcFlatUpland:=flat_upland/(flat_upland + nonflat_upland)]
prc2[,prcFlatLowland:=flat_lowland/(flat_lowland + nonflat_lowland)]
cols <- c("prcFlatTotal","prcFlatUpland","prcFlatLowland")
for (c1 in cols) prc2[[c1]][is.na(prc2[[c1]])] <- 0

dt1 <- merge(dt1,prc2[,c("id",cols),with=FALSE], all=TRUE)
attrs <- merge(attrs,dt1[,c("id","prcFlatTotal","prcFlatLowland","prcFlatUpland","relief"),with=F],by="id",all.x=TRUE)
save(attrs, file=fout)

message("check attribtue summary")
cols <- names(attrs)
cols <- cols[cols != "id"]
for (c1 in cols) {
  message(c1)
  print(summary(attrs[[c1]]))
}

huc <- merge(huc, attrs[,c("id",cols[!cols %in% names(huc)]),with=FALSE], by="id", all=TRUE)

# plot the attributes (multiple panels, no legend)
# plot(huc[cols],border=NA)

message("plot the attributes separately with legend")
for (c1 in cols) {
  message(c1)
  f1 <- paste0("../figs/attrs/huc",h1,"_v",ver1,"/attr_",c1,"_huc",h1,"_v",ver1,".png")
  if (!dir.exists(dirname(f1))) dir.create(dirname(f1))
  png(filename = f1,width = 5,height=5,units="in",res=300)
  print(plot(huc[c1], border=NA, key.pos=1))
  dev.off()
}
} # huc
} # version