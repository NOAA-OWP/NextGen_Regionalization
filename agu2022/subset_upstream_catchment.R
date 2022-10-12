library(hydrofabric)
library(dplyr)
library(sf)
library(data.table)

# check crosswalk for gage ID/ngen ID
gfile <- "shapefile/nextgen_01.gpkg"
cw1 <- read_sf(gfile, 'crosswalk')
subcw <- subset(cw1, POI_TYPE=="Gages", select=c("id","toid","POI_ID","POI_VALUE"))

# trace the network from the ngen ID associated with the gage
df1 <- tibble()
for (i in 1:nrow(subcw)) {
  print(i)
  cw <- subcw[i, ]
  trace <- subset_network(gfile, origin=cw$id)

  # extract upstream catchment  
  catchments <- trace$divides
  catchments['gages'] <- cw$POI_VALUE
  df1 <- rbind(df1, catchments)
}

# save 
filename <- 'shapefile/upstream_catchment_huc01.gpkg'
st_write(df1, filename, "catchments")

# save the table
dt1 <- as.data.table(df1)
dt1$geom <- NULL
save(dt1,file="output/crosswalk_gage_cat_huc01.Rdata")