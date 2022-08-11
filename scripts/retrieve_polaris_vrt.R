setwd("C:/Users/***REMOVED***/Desktop/Ngen/regionalization/scripts")

library(zonal)
library(sf)
library(terra)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson")

# retrieve Polaris parameters from VRT for HUC-01
for (par in c("silt","sand","clay","bd","theta_s","theta_r","ksat","ph","om","lambda","hb","n","alpha")) {
  for (layer in c("0_5","5_15","15_30","30_60","60_100","100_200")) {
    
    outfile <- file.path("../datasets/Polaris_huc01/", paste0(par,"_mean_",layer,".tif"))
    if (file.exists(outfile)) next
    
    message(paste0("retrieving ", par," for layer ", layer))
    
    system.time({
      r1 <- crop(
        rast(paste0("/vsicurl/http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/",par,"_mean_",layer,".vrt")),
        vect(huc01)
      )
    })
    
    
    # write the HUC-01 elevation raster to disk
    writeRaster(r1, filename=outfile, overwrite=TRUE)
}}

#http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/alpha_mean_0_5.vrt