setwd("C:/Users/***REMOVED***/Desktop/Ngen/regionalization/scripts")

library(zonal)
library(sf)
library(terra)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson") 

# ncld land cover class
nlcd <- rast("../datasets/nlcd/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")

huc01 <- st_transform(huc01,crs(nlcd))

# dominant land cover using zonal
system.time({
  lc_class <- execute_zonal(nlcd,geom=huc01,ID="id",FUN="mode",join=FALSE)
})

# percentage of each land cover class
system.time({
  lc_prc <- execute_zonal(nlcd,geom=huc01,ID="id",FUN="percent",join=FALSE)
})


###
#2019 Land Cover Class for the conterminous United States	Percentage
#11. Water	5.26
#12. Perennial Ice Snow	0.01
#21. Developed, Open Space	2.68
#22. Developed, Low Intensity	1.71
#23. Developed, Medium Intensity	1.03
#24. Developed High Intensity	0.37
#31. Bare Rock/Sand/Clay	0.97
#41. Deciduous Forest	9.29
#42. Evergreen Forest	11.51
#43. Mixed Forest	3.40
#52. Shrub/Scrub	21.85
#71. Grasslands/Herbaceous	13.34
#81. Pasture/Hay	6.24
#82. Cultivated Crops	16.31
#90. Woody Wetlands	4.50
#95. Emergent Herbaceous Wetlands	1.53
#Total	100.00%