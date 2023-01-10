rm(list=ls())

library(geojsonsf)
library(sf)
library(jsonlite)
library(geojsonio)

gage1 <- "01115630"
cats <- c("cat-34950","cat-34952","cat-34949")
file1 <- "../datasets/gpkg_v0/catchment_data.geojson"
ixs <- 1:5
lines <- readLines(file1)
for (c1 in cats) ixs <- c(ixs,grep(c1,lines))
ixs <- c(ixs, length(lines)-1,length(lines))
lines1 <- lines[ixs]
n1 <- length(lines1)
lines1[n1-2] <- substr(lines1[n1-2],1,nchar(lines1[n1-2])-1)
writeLines(lines1,paste0("../datasets/gpkg_v0/catchment_data_",gage1,".geojson"))

s1 <- st_read(file1)
toids <- unique(subset(s1, id %in% cats)$toid)

file1 <- "../datasets/gpkg_v0/nexus_data.geojson"
ixs <- 1:5
lines <- readLines(file1)
for (c1 in toids) ixs <- c(ixs,grep(c1,lines))
ixs <- c(ixs, length(lines)-1,length(lines))
lines1 <- lines[ixs]
n1 <- length(lines1)
lines1[n1-2] <- substr(lines1[n1-2],1,nchar(lines1[n1-2])-1)
writeLines(lines1,paste0("../datasets/gpkg_v0/nexus_data_",gage1,".geojson"))

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
