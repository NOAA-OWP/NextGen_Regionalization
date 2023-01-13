# plot illustrations of catchments before and after regionalization

rm(list=ls())

library(data.table)
library(sf)

g1 <- "01116905"

gpkg1 <- paste0("../datasets/gpkg_v1.2/gauge_",g1,".gpkg")

shp1 <- st_read(gpkg1,"divides")
flow1 <- st_read(gpkg1,"flowpaths")
nex1 <- st_read(gpkg1,"nexus")

# before regionalization
plot(st_geometry(shp1),col="grey90",lwd=2,main=g1)
plot(st_geometry(subset(shp1,id=="cat-16452")),col="mistyrose",add=T)
plot(st_geometry(subset(shp1,id=="cat-16451")),col="lightblue",add=T)
plot(st_geometry(flow1),col="blue",lwd=3,add=T)
plot(st_geometry(subset(nex1,id %in% c("nex-16444","nex-16449"))),pch=24,col="red",bg="yellow",cex=3,lwd=2,add=T)
plot(st_geometry(subset(nex1,id == "nex-16445")),pch=21,col="black",bg="red",lwd=2,cex=3,add=T)

# after regionalization
plot(st_geometry(shp1),col="grey90",lwd=2,main=g1)
plot(st_geometry(subset(shp1,id=="cat-16452")),col="mistyrose",lwd=2,add=T)
plot(st_geometry(subset(shp1,id=="cat-16451")),col="lightblue",lwd=2,add=T)
plot(st_geometry(subset(shp1,id=="cat-16449")),col="lightblue",lwd=2,add=T)
plot(st_geometry(subset(shp1,id %in% c("cat-16444","cat-16450"))),col="mistyrose",lwd=2,add=T)
plot(st_geometry(flow1),col="blue",lwd=3,add=T)
plot(st_geometry(subset(nex1,id %in% c("nex-16444","nex-16449"))),pch=24,col="red",bg="yellow",cex=3,lwd=2,add=T)
plot(st_geometry(subset(nex1,id == "nex-16445")),pch=21,col="black",bg="red",lwd=2,cex=3,add=T)