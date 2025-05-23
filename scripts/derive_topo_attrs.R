# Derive topographic attributes from 30m SRTM elevation data

rm(list=ls())

library(data.table)
library(zonal)
library(sf)
library(terra)
library(raster)
library(units)
library(stars)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson")
huc01$toid <- NULL

# circularity index
huc01$cidx <- huc01$area_sqkm/(drop_units(st_length(st_cast(huc01,"MULTILINESTRING")))/1000)^2*4*pi

# read elevation data into raster
elev0 <- raster("../datasets/elev/DEM_huc01_30m.tif")

# compute slope
slope <- terrain(elev0,opt='slope',unit='tangent')
huc01 <- execute_zonal(rast(slope),geom=huc01,ID="id",join=TRUE)
names(huc01)[names(huc01)=="V1"] <- "slope"

# mean elevation
huc01 <- execute_zonal(rast(elev0),geom=huc01,ID="id",join=TRUE)
names(huc01)[names(huc01)=="V1"] <- "elev_mean"

# relief
dt1 <- execute_zonal(rast(elev0),geom=huc01,ID="id",FUN="max",join=FALSE)
names(dt1) <- c("id", "maxelev")

tmp <- execute_zonal(rast(elev0),geom=huc01,ID="id",FUN="min",join=FALSE)
names(tmp) <- c("id", "minelev")
dt1 <- merge(dt1,tmp,by="id")

dt1[,relief:=maxelev-minelev]

# create huc01 catchment raster (30m)
huc01$idx <- 1:nrow(huc01)
grd = st_as_stars(st_bbox(elev0), nx = ncol(elev0), ny = nrow(elev0), values = 0)
r1 <- st_rasterize(huc01[,"idx"],grd)
write_stars(r1,"../datasets/elev/huc01_catchments_30m.tif")

# calculate the middle elevation mask
dt1[,midelev:=(minelev+maxelev)/2]
dt1$idx <- 1:nrow(dt1)
r1 <- raster("../datasets/elev/huc01_catchments_30m.tif")
r_midelev <- reclassify(r1,as.matrix(dt1[,c("idx","midelev"),with=F])) #note: this line takes > 1hr
writeRaster(r_midelev,"../datasets/elev/midelev_huc01_30m.tif")

# categorize upland/lowland and flatland
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

# compute precentage of flatland in upland and lowland areas
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

dt1 <- merge(dt1,prc2[,c("id",cols),with=FALSE])

huc01 <- merge(huc01,dt1[,c("id","prcFlatTotal","prcFlatLowland","prcFlatUpland","relief"),with=F],by="id")

# check attribtue summary
cols <- names(huc01)
cols <- cols[!cols %in% c("id","geometry")]
for (c1 in cols) {
  message(c1)
  print(summary(topoAll[[c1]]))
}

# plot the attributes (multiple panels, no legend)
plot(huc01[cols],border=NA)

# plot the attributes separately with legend
for (c1 in cols) {
  message(c1)
  png(filename = paste0("../figs/attr_",c1,"_huc01.png"),width = 5,height=5,units="in",res=300)
  print(plot(huc01[c1], border=NA, key.pos=1))
  dev.off()
}

# save the attributes
huc01$geometry <- NULL
attrs <- as.data.table(huc01)
save(attrs, file="../output/topo_attr_huc01.Rdata")