# Derive topographic attributes from 30m SRTM elevation data

rm(list=ls())

library(data.table)
library(zonal)
library(sf)
library(terra)
library(raster)
library(units)
library(stars)

ver1 <- "v0"
for (ver1 in c("v0","v1.2"))  {

message("read HUC-01 geojson")
huc01 <- st_read(paste0("../datasets/gpkg_",ver1,"/catchment_data.geojson"))

# initialize attribute data table
attrs <- data.table(id = huc01$id)

str1 <- "areasqkm"
if (!str1 %in% names(huc01)) str1 <- "area_sqkm"
attrs$areasqkm <- huc01[[str1]]

message("circularity index")
attrs$cidx <- attrs$areasqkm/(drop_units(st_length(st_cast(huc01,"MULTILINESTRING")))/1000)^2*4*pi

message("read elevation data into raster")
elev0 <- raster(paste0("../datasets/elev_",ver1,"/DEM_huc01_30m.tif"))

message("compute slope")
slope <- terrain(elev0,opt='slope',unit='tangent')
dt1 <- execute_zonal(rast(slope),geom=huc01,ID="id",join=FALSE)
names(dt1)[names(dt1)=="V1"] <- "slope"
attrs <- merge(attrs, dt1, by="id", all=TRUE)

message("compute mean elevation")
dt1 <- execute_zonal(rast(elev0),geom=huc01,ID="id",join=FALSE)
names(dt1)[names(dt1)=="V1"] <- "elev_mean"
attrs <- merge(attrs, dt1, by="id", all=TRUE)

# relief
message("compute max elevation")
dt1 <- execute_zonal(elev0,geom=huc01,ID="id",FUN="max",join=FALSE)
names(dt1) <- c("id", "maxelev")

message("compute min elevation")
tmp <- execute_zonal(elev0,geom=huc01,ID="id",FUN="min",join=FALSE)
names(tmp) <- c("id", "minelev")
dt1 <- merge(dt1,tmp,by="id")

message("compute relief")
dt1[,relief:=maxelev-minelev]

message("calculate the middle elevation raster")
dt1[,midelev:=(minelev+maxelev)/2]
huc01 <- merge(huc01,dt1[,c("id","midelev"),with=F],by="id",all=TRUE)
r_midelev <- st_rasterize(huc01[,"midelev"],
                          st_as_stars(st_bbox(elev0), nx = ncol(elev0), ny = nrow(elev0), values = 0))
r_midelev <- raster(rast(r_midelev))

message("categorize upland/lowland and flatland")
r1 <- slope
r1[] <- NA
r1[slope<=0.01 & elev0>r_midelev] <- 1     #flat upland
r1[slope<=0.01 & elev0<=r_midelev] <- 2    #flat lowland
r1[slope>0.01 & elev0>r_midelev] <- 3      #nonflat upland
r1[slope>0.01 & elev0<=r_midelev] <- 4     #nonflat lowland

f1 <- ratify(r1)
rat <- levels(f1)[[1]]
rat$cat <- c("flat_upland","flat_lowland","nonflat_upland","nonflat_lowland")
levels(f1) <- rat

message("compute precentage of flatland in upland and lowland areas")
rcl <- dplyr::select(rat,from=ID,to=cat)
prc1 <- execute_zonal(f1,geom=huc01,ID="id",FUN="freq",rcl=rcl, join=FALSE)
prc1 <- subset(prc1, !is.na(value))
prc2 <- dcast(prc1,id ~ value, value.var="percentage")
cols <- names(prc2)
cols <- cols[cols!="id"]
for (c1 in cols) prc2[[c1]][is.na(prc2[[c1]])] <- 0 

prc2[,prcFlatTotal:=flat_lowland+flat_upland]
prc2[,prcFlatUpland:=flat_upland/(flat_upland + nonflat_upland)]
prc2[,prcFlatLowland:=flat_lowland/(flat_lowland + nonflat_lowland)]
cols <- c("prcFlatTotal","prcFlatUpland","prcFlatLowland")
for (c1 in cols) prc2[[c1]][is.na(prc2[[c1]])] <- 0

dt1 <- merge(dt1,prc2[,c("id",cols),with=FALSE], all=TRUE)

attrs <- merge(attrs,dt1[,c("id","prcFlatTotal","prcFlatLowland","prcFlatUpland","relief"),with=F],by="id", all=TRUE)

message("save the attributes")
save(attrs, file=paste0("output/",ver1,"/topo_attr_huc01.Rdata"))

message("check attribtue summary")
cols <- names(attrs)
cols <- cols[cols != "id"]
for (c1 in cols) {
  message(c1)
  print(summary(huc01[[c1]]))
}

huc01 <- merge(huc01, attrs, by="id", all=TRUE)

# plot the attributes (multiple panels, no legend)
# plot(huc01[cols],border=NA)

message("plot the attributes separately with legend")
for (c1 in cols) {
  message(c1)
  png(filename = paste0("figs/",ver1,"/attr_",c1,"_huc01.png"),width = 5,height=5,units="in",res=300)
  print(plot(huc01[c1], border=NA, key.pos=1))
  dev.off()
}
}
