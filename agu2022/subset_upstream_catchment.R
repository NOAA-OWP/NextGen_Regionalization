# derive the upstream catchments for gages within HUC01

library(hydrofabric)
library(sf)
library(dplyr)
library(data.table)
source("network_subsetting.R")

gfile <- '../datasest/gpkg_v1.2/nextgen_01.gpkg'
cw1 <- read_sf(gfile, 'lookup_table')
subcw <- subset(cw1, POI_TYPE=="Gages", select=c("id","toid","POI_ID","POI_VALUE"))

# trace the network from the ngen ID associated with the gage
df1 <- tibble()
for (i in 1:nrow(subcw)) {
  print(i)
  cw <- subcw[i, ]
  trace = subset_network(gpkg         = gfile,
                     origin           = cw$toid,
                     attribute_layers = c("flowpath_attributes",
                                          "lake_attributes",
                                          "cfe_noahowp_attributes",
                                          "forcing_attributes"))
  # extract upstream catchment  
  catchments <- trace$divides
  catchments['gages'] <- cw$POI_VALUE
  df1 <- rbind(df1, catchments)                                        
}

# save 
filename <- '../datasets/gpkg_v1.2/upstream_catchment_huc01.gpkg'
st_write(df1, filename, "catchments")

# save the table
dt1 <- as.data.table(df1)
dt1$geom <- NULL
save(dt1,file="output/crosswalk_gage_cat_huc01.Rdata")