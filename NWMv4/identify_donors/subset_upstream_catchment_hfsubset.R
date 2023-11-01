# create gpkg file for gages using the tool hfsubset and creates the gage-catchment crosswalk table
# note: this tool does not work for hydrofabric versions before v2.0 pre-release

rm(list=ls())

ver1 <- "v2.0pre"; ver2 <- "pre-release"; h1 <- "12"
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
huc <- read_sf(paste0("../../datasets/gpkg_",ver1,"/nextgen_huc",h1,".gpkg"),"divides")
huc <- correct_geojson(h1, huc)

# transform crs for gage points
sf_gages <- st_transform(sf_gages,st_crs(huc))

# identify gages within the given HUC
gages1 <- sf_gages[lengths(st_intersects(sf_gages, huc))>0,]$gage_id

# loop through gages1 to derive gpkg (and creates the crosswalk table)
cwt <- data.table()
for (g1 in gages1) {
    outfile <- paste0('../../datasets/gpkg_', ver1,'/gages/huc',h1,"/",g1,'.gpkg')
    #if (file.exists(outfile)) next

    message(paste0(g1," ",match(g1, gages1)))
    cmd1 <- paste0('./hfsubset -l core -o ',outfile,' -r "', ver2,'" -t hl "Gages-',g1,'"')
    #system(cmd1)

    cats1 <- read_sf(outfile,"divides")
    cwt <- rbind(cwt,data.table(id=cats1$divide_id, toid=cats1$toid, type=cats1$type, areasqkm=cats1$areasqkm,gages=g1))
}
write.csv(cwt,file=paste0("data/crosswalk_gage_cat_huc",h1,"_", ver1,".csv"),quote=FALSE,row.names=FALSE)
