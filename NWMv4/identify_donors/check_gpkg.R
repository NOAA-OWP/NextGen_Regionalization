# sanity check to make sure all catchments in the gage gpkg file created by hfsubset can be found in the HUC gpkg 

rm(list=ls())

library(sf)
library(data.table)

ver1 <- "v2.0pre"
hucs <- c("12", "17")
dt_hucs <- data.table()
files <- list.files(paste0("../../datasets/gpkg_",ver1,"/gages"),pattern=".gpkg",full.names=TRUE)

for (h1 in hucs) {

# read HUC hydrofabric
if (ver1=="v2.0pre") {
    huc <- read_sf(paste0("../../datasets/gpkg_",ver1,"/nextgen_",h1,".gpkg"),"divides")
} else {
    huc <- st_read(paste0("../../datasets/gpkg_",ver1,"/huc",h1,"/catchment_data.geojson"))
}

for (f1 in files) {
    gage1 <- gsub(".gpkg","",basename(f1))
    message(gage1)
    sf1 <- read_sf(f1, 'divides')
    cats <- sf1$divide_id
    cats1 <- cats[!cats %in% huc$divide_id]
    if (length(cats1)==0) dt_hucs <- rbind(dt_hucs,data.table(gage=gage1,huc=h1))
    if (length(cats1)>0) message(paste0(length(cats1)," out of ", length(cats)," catchments not found"))
}}

meta <- as.data.table(read.csv("../data/Domain_Meta_NWM_v3.0.csv",stringsAsFactors=FALSE))
meta <- meta[, c("gage_id", "lat", "lon"), with = FALSE]
meta <- meta[!duplicated(meta$gage_id)]
meta <- na.omit(meta)
dt_hucs <- merge(dt_hucs, meta, by.x="gage",by.y="gage_id",all.x=TRUE)

save(dt_hucs, file="../data/list_gage_huc.Rdata")