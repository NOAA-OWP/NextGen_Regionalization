setwd("C:/Users/yuqiong.liu/Desktop/Ngen/regionalization/scripts")

library(zonal)
library(sf)
library(terra)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson") 

# retrieve the 30-m DEM raster from VRT for HUC-01
system.time({
  r1 <- crop(
    rast("/vsicurl/http://mikejohnson51.github.io/opendap.catalog/ned_1_tester.vrt"),
    vect(huc01)
  )
})


# write the HUC-01 elevation raster to disk
writeRaster(r1, filename=file.path("../datasets", "DEM_huc01_30m.tif"), overwrite=TRUE)

# View
plot(rast(file.path("../datasets/elev", "DEM_huc01_30m.tif")))

# compute mean elevation using zonal
system.time({
  elev_mean <- execute_zonal("../datasets/elev/DEM_huc01_30m.tif",geom=huc01,ID="id",join=FALSE)
})
