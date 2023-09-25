library(easypackages)
libraries("rgdal", "gdalUtils", "raster")

h1 <- "17"

files <- list.files("../../datasets/elev/DEM",full.names=TRUE)
files <- files[grepl(paste0("ned",h1),files)]

dems <- vector("list",length(files))
for (i1 in 1:length(files)) dems[[i1]] <- raster(files[i1])

dem1 <- do.call(merge, dems)
writeRaster(dem1, file=paste0("../../datasets/elev/DEM/ned",huc,".tif"),format="GTiff")






