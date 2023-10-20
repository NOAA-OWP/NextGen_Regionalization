rm(list=ls())

ver1 <- "v2.0pre"; ver2 <- "pre-release"; h1 <- "12"
ver1 <- "v2.0pre"; ver2 <- "pre-release"; h1 <- "17"
#ver1 <- "v1.2"; ver2 <- ver1; h1 <- "17"

library(data.table)
library(sf)
source("../attr_calc/correct_geojson.R")
sf::sf_use_s2(FALSE)

# v3 calibration basins
gages <- as.character(get(load("data/all_nwmv30_gages_classified.Rdata"))$gage)

# domain meta for v3 calibration
meta <- as.data.table(read.csv("data/Domain_Meta_NWM_v3.0.csv",stringsAsFactors=FALSE))
meta <- meta[, c("gage_id", "lat", "lon"), with = FALSE]
meta <- meta[!duplicated(meta$gage_id)]
meta <- meta[gage_id %in% gages,]
meta <- na.omit(meta)

# create sf points from gage lat/lon
sf_gages <- st_as_sf(meta, coords = c("lon","lat"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# read HUC hydrofabric
if (ver1=="v2.0pre") {
    huc <- read_sf(paste0("../../datasets/gpkg_",ver1,"/nextgen_",h1,".gpkg"),"divides")
} else {
    huc <- st_read(paste0("../../datasets/gpkg_",ver1,"/huc",h1,"/catchment_data.geojson"))
    huc <- correct_geojson(h1, huc)
}

# transform crs for gage points
sf_gages <- st_transform(sf_gages,st_crs(huc))

# identify gages within the given HUC
gages1 <- sf_gages[lengths(st_intersects(sf_gages, huc))>0,]$gage_id

# loop through gages1 to derive gpkg 
for (g1 in gages1) {
    outfile <- paste0('../../datasets/gpkg_', ver1,'/',g1,'.gpkg')
    if (file.exists(outfile)) next

    message(paste0(g1," ",match(g1, gages1)))
    cmd1 <- paste0('./hfsubset -l core -o ',outfile,' -r "', ver2,'" -t hl "Gages-',g1,'"')
    system(cmd1)
}