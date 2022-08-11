setwd("C:/Users/***REMOVED***/Desktop/Ngen/regionalization/scripts")

rm(list=ls())

library(sf)

# shapefile of HUC-01 catchments
huc01 <- st_read("../shapefile/catchment_data.geojson") 

# shapefile of HUC-01 nexus
nex01 <- st_read("../shapefile/nexus_data.geojson") 

# shapefile of USGS gages in HUC-01
usgs1 <- st_read("../shapefile/usgs_gages_huc01.shp")

# shapefile of headwater basins in HUC-01
huc01_hw <- st_read("../shapefile/huc01_headwater.shp")

# identify the catchments where gages are located
sf::sf_use_s2(FALSE)
out <- st_intersection(usgs1,huc01)

# trace all upstream catchments and nexus for each gage
catchments <- nexus <- vector("list",nrow(usgs1))
names(catchments) <- names(nexus) <- usgs1$gage
for (i1 in 1:nrow(out)) {
  message(i1)
  g1 <- out$gage[i1]
  cat1 <- out$id[i1]
  cats <- nexs <- NULL
  while(1) {
    cats <- c(cats,cat1)
    cats <- unique(cats)
    nex1 <- nex01$id[unlist(sapply(cat1,function(x) which(nex01$toid==x)))]
    nexs <- c(nexs, nex1)
    if (length(nex1)==0) break
    cat1 <- huc01$id[unlist(sapply(nex1, function(x) which(huc01$toid==x)))]
    cat1 <- unique(cat1)
  }
  catchments[[g1]] <- cats
  nexus[[g1]] <- nexs
}

save(catchments,file="../data/usgs_gage_catchments_huc01.Rdata")

# number of cathcments for each usgs gage
nc1 <- sapply(catchments,function(x) length(x))

# summary
print(table(nc1))

# single-cathcment basins
print(data.table::data.table(catchment=names(catchments[nc1==1]),gage=as.vector(catchments[nc1==1])))


# plot the catchments,nexus and gage for a given basin (using plot, no labels)
i0 <- 300
i1 <- which(huc01$id %in% catchments[[out$gage[i0]]])
i2 <- which(usgs1$gage==out$gage[i0])
i3 <- which(nex01$id %in% nexus[[out$gage[i0]]])
str1 <- paste0(usgs1$gage[i2],"  ", usgs1$name[i2])
plot(st_geometry(huc01[i1,]), col = "grey", border = 'black', main=str1,axes = TRUE)
plot(st_geometry(usgs1[i2,]), pch = 17, cex=1.5, col = 'red', add = TRUE)
plot(st_geometry(nex01[i3,]), pch = 16, col = 'blue', add = TRUE)


# using ggplot2
library(ggplot2)
ggplot(huc01[i1,]) +
  geom_sf() +
  geom_sf(data=usgs1[i2,],color="red",shape=17,size=3) +
  geom_sf(data=nex01[i3,],color="blue",shape=16,size=3) +
  geom_sf_label(aes(label = id)) +
  geom_sf_label(data=usgs1[i2,],aes(label=gage),color="red",nudge_x = 0.02) +
  geom_sf_label(data=nex01[i3,],aes(label=id),color="blue",nudge_x = 0.02) +
  ggtitle(str1) + xlab("") +ylab("") +
  theme(plot.title=element_text(size=11))

  
