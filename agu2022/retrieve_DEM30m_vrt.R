rm(list=ls())

library(zonal)
library(sf)
library(terra)

ver1 <- "v1.2"

# read HUC-01 geojson into sf
huc01 <- st_read("../datasets/gpkg_v1.2/catchment_data.geojson") 

# retrieve the 30-m DEM raster from VRT for HUC-01
system.time({
  r1 <- crop(
    #rast("/vsicurl/http://mikejohnson51.github.io/opendap.catalog/ned_1_tester.vrt"),
    rast("../datasets/elev_v1.2/ned_USGS_1.vrt"),vect(huc01)
  )
})


# write the HUC-01 elevation raster to disk
dir1 <- paste0("../datasets/elev_",ver1)
writeRaster(r1, filename=file.path(dir1, "DEM_huc01_30m.tif"), overwrite=TRUE)

# View
plot(rast(file.path(dir1, "DEM_huc01_30m.tif")))

# compute mean elevation using zonal
#system.time({
#  elev_mean <- execute_zonal("../datasets/elev/DEM_huc01_30m.tif",geom=huc01,ID="id",join=FALSE)
#})
