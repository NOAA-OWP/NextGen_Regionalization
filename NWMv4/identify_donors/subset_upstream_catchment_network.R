# derive the upstream catchments for gages via network subsetting (for hydrofabric versions before v2.0 pre-release)
# this script also creates the catchment-gage crosswalk table 

library(nhdplusTools)
library(sf)
library(dplyr)
library(data.table)
source("network_subsetting.R")

ver1 <- "1.2"; h1 <- "17" # hydrofabric version & huc region

# read crosswalk from hydrofabric
gfile <- paste0('../../datasets/gpkg_v1.2/nextgen_huc',h1,'.gpkg')
cw1 <- read_sf(gfile, "crosswalk")
subcw <- subset(cw1, POI_TYPE=="Gages", select=c("id","toid","POI_ID","POI_VALUE"))

# v3 calibration basins
gages <- as.character(get(load("../data/all_nwmv30_gages_classified.Rdata"))$gage)

# trace the network from the ngen ID associated with the gage
df1 <- tibble()
for (i in 1:nrow(subcw)) {
  cw <- subcw[i, ]
  if (!cw$POI_VALUE %in% gages) next

  print(i)

  trace = subset_network(gpkg         = gfile,
                     origin           = cw$toid,
                     attribute_layers = c("flowpath_attributes",
                                          "cfe_noahowp_attributes",
                                          "forcing_metadata"))
  # extract upstream catchment  
  catchments <- trace$divides
  catchments['gages'] <- cw$POI_VALUE
  df1 <- rbind(df1, catchments)                                        
}

# save 
filename <- paste0('../../datasets/gpkg_v1.2/upstream_catchment_huc',h1,'.gpkg')
st_write(df1, filename, "catchments")

# save the crosswalk table
cwt <- as.data.table(df1)
cwt$geom <- NULL
write.csv(cwt,file=paste0("../data/crosswalk_gage_cat_huc",h1,"_", ver1,".csv"),quote=FALSE,row.names=FALSE)