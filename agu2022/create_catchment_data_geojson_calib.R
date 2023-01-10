rm(list=ls())

library(geojsonsf)
library(sf)
library(jsonlite)
library(geojsonio)

catchments <- get(load("data/calib_gage_catchments_huc01.Rdata"))
gages0 <- names(catchments)

# catchment data
file1 <- "../datasets/gpkg_v0/catchment_data.geojson"
lines1 <- readLines(file1)

# nesus data
file2 <- "../datasets/gpkg_v0/nexus_data.geojson"
lines2 <- readLines(file2)

# nexus ids
s1 <- st_read(file1)

#header lines
ixs0 <- 1:5  

for (gage1 in gages0) {

# first create catchment data geojson
cats <- catchments[[gage1]]
ixs <- ixs0
for (c1 in cats) ixs <- c(ixs,grep(c1,lines1)) # header lines + catchment lines
ixs <- c(ixs, length(lines1)-1,length(lines1)) # last two lines
lines <- lines1[ixs]
n1 <- length(lines)
n2 <- nchar(lines[n1-2])
if (substr(lines[n1-2],n2,n2)==",") lines[n1-2] <- substr(lines[n1-2],1,n2-1) # remove the comma from the last catchment line
writeLines(lines,paste0("../datasets/gpkg_v0/hf_calib_basins/catchment_data_",gage1,".geojson"))

# then create nexus data geojson
toids <- unique(subset(s1, id %in% cats)$toid)
ixs <- ixs0
for (c1 in toids) ixs <- c(ixs0,grep(c1,lines2))
ixs <- c(ixs, length(lines2)-1,length(lines2))
lines <- lines2[ixs]
n1 <- length(lines)
n2 <- nchar(lines[n1-2])
if (substr(lines[n1-2],n2,n2)==",") lines[n1-2] <- substr(lines[n1-2],1,n2-1)
writeLines(lines,paste0("../datasets/gpkg_v0/hf_calib_basins/nexus_data_",gage1,".geojson"))
}

#cwt <- get(load("output/v1.2/crosswalk_gage_cat_huc01.Rdata"))
# cwt <- read.csv("output/v0/nhd-crosswalk.csv")
# cwt <- subset(cwt,gages!="")
# gages <- cwt$gages
# gages <- strsplit(cwt$gages,split=",")
# gages <- lapply(gages, function(x) unique(x))
# cwt1 <- subset(cwt,sapply(gages,function(x) length(x))==1)
# cwt1 <- cwt1[,c("ID","gages")]
# cwt1 <- as.data.table(cwt1[!duplicated(cwt1),])
# cwt1 <- subset(cwt1,!(duplicated(gages)))
# cwt1 <- cwt1[,.(gage=unique(strsplit(gages,split=","))[[1]]),by=.(ID)]
# cwt1 <- cwt1[!duplicated(cwt1),]
#cats <- subset(cwt, gages==gage1)$id
# s2 <- subset(s1, id %in% cats)
# g1 <- sf_geojson(s2,atomise=TRUE)
# write(toJSON(g1,digits=7,pretty=TRUE,auto_unbox=TRUE),"../datasets/gpkg_v1.2/catchment_data_01115630.geojson")

# g2 <- geojson_json(s2)
# geojson_write(g2,precision=10,file="../datasets/gpkg_v0/catchment_data_01115630.geojson")
