# compute soil attributes from STATSGO data (PennState dataset)

rm(list=ls())

library(data.table)
library(zonal)
library(sf)
library(terra)
library(stars)
library(raster)
source("correct_geojson.R")

soil_raster <- TRUE #if soil property rasters already exist
# attributes to compute
pars <- c("soil_depth","soil_porosity","soil_conductivity","sand_frac","silt_frac","clay_frac","organic_frac","water_frac","other_frac")

vers <- "1.2"
hucs <- c("17")
for (ver1 in vers)  {
for (h1 in hucs) {

message(paste0("read geojson v", ver1," huc",h1))
huc <- st_read(paste0("../../datasets/gpkg_v",ver1,"/huc",h1,"/catchment_data.geojson"))
huc <- correct_geojson(h1, huc)

attrs <- data.table(id=huc$id)
fout <- paste0("output/soil_attr_huc",h1,"_v",ver1,"_statsgo.Rdata")
if (file.exists(fout)) attrs <- get(load(fout))

# if soil property rasters do not already exist, rasterize the shapefile first
# tried computing zonal statistics directly from vector layer (i.e., without coverting to raster first) 
# using intersection() but it was too slow
if ((!soil_raster)) {
  
message("load the original pennstate data")
s1 <- st_read("../../datasets/shapefile/statsgo_v5_WGS84.shp")

message("load DEM (30m) for raster template")
r1 <- rast(paste0("../../datasets/elev/DEM/ned",h1,".tif"))

# Compute soil properties following Addor et al. 2017 (Table 5) and Cosby et al. 1984 (Table 4)
# Addor N.,A. J. Newman, N. Mizukami, and M. P. Clark, 2017: The CAMELS data set: catchment attributes and meteorology for large-sample studies. Hydrol. Earth Syst. Sci., 21, 5293–5313, https://doi.org/10.5194/hess-21-5293-2017
# Cosby, B. J., Hornberger, G. M., Clapp, R. B., and Ginn, T. R., 1984: A Statistical Exploration of the Relationships of Soil Moisture Characteristics to the Physical Properties of Soils, Water Resour.  Res., 20, 682–690, https://doi.org/10.1029/WR020i006p00682.

d1 <- c(5,5,10,10,10,20,20,20,50,50,50)*0.01
cols <- paste0("L",1:11,"_TCODE") #soil class columns
cols1 <- paste0("SAND_L",1:9) #sand fraction columns
cols2 <- paste0("CLAY_L",1:9) #clay fraction columns

for (p1 in pars) {

  tot1 <- dep1 <- 0
  for (i1 in 1:9) {
    depths <- rep(d1[i1],nrow(s1))
    if (p1=="soil_depth") {
      idx1 <- s1[[cols[i1]]] %in% 14:15
    } else if (p1 == "sand_frac") {
      tmp <- s1[[paste0("SAND_L",i1)]]*d1[i1]
      idx1 <- s1[[cols[i1]]] %in% 13:16
    } else if (p1 == "silt_frac") {
      tmp <- s1[[paste0("SILT_L",i1)]]*d1[i1]
      idx1 <- s1[[cols[i1]]] %in% 13:16
    } else if (p1 == "clay_frac") {
      tmp <- s1[[paste0("CLAY_L",i1)]]*d1[i1]
      idx1 <- s1[[cols[i1]]] %in% 13:16
    } else if (p1 == "soil_porosity") {
      tmp <- (50.5-0.142*s1[[cols1[i1]]]-0.037*s1[[cols2[i1]]])/100*d1[i1]
      tmp[s1[[cols[i1]]]==13] <- 0.9*d1[i1] # origanic material layer
      idx1 <- s1[[cols[i1]]] %in% 14:16
    } else if (p1 == "soil_conductivity") {
      tmp <- -0.6+0.0126*s1[[cols1[i1]]]-0.0064*s1[[cols2[i1]]]
      tmp <- (10^tmp)*2.54 #(convert from log in/h to cm/h)
      tmp <- log(tmp)*d1[i1] # use weighted geometric mean rather than arithmetic mean
      tmp[s1[[cols[i1]]]==13] <- log(36)*d1[i1]
      idx1 <- s1[[cols[i1]]] %in% 14:16
    } else if (p1 == "water_frac") {
      idx1 <- s1[[cols[i1]]] != 14
    } else if (p1 == "organic_frac") {
      idx1 <- s1[[cols[i1]]] != 13
    } else if (p1 == "other_frac") {
      idx1 <- s1[[cols[i1]]] != 16
    }

    if (!p1 %in% c("soil_depth","water_frac","water_frac")) {
      tmp[idx1] <- 0; tot1 <- tot1 + tmp
    }
    depths[idx1] <- 0; dep1 <- dep1 + depths
  }
  
  if (p1 == "soil_depth") {
    s1[[p1]] <- dep1
  } else if (p1 == "water_frac") {
    s1[[p1]] <- dep1/1.5
  } else if (p1 %in% c("organic_frac","other_frac")) {
    s1[[p1]] <- dep1/s1[["soil_depth"]]
  } else if (p1 == "soil_conductivity") {
    s1[[p1]] <- exp(tot1/dep1)
  } else {
    s1[[p1]] <- tot1/dep1
  }
}
    
message("derive soil parameter rasters and save to disk")
s1 <- st_transform(s1,crs(r1))
for (p1 in pars) {
  message(p1)
  r2 <- st_rasterize(s1[,p1],st_as_stars(st_bbox(r1),nx=ncol(r1),ny=nrow(r1), values = NA))
  outfile <- paste0("../../datasets/Statsgo_huc",h1,"_v",ver1,"/huc",h1,"_",p1,"_30m.tif")
  if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile),recursive=TRUE)
  write_stars(r2,outfile)
}
}

message(paste0("compute areal mean for huc",h1," catchments"))
for (p1 in pars) {
  if (p1 %in% names(attrs)) next
  message(p1)
  outfile <- paste0("../../datasets/Statsgo_huc",h1,"_v",ver1,"/huc",h1,"_",p1,"_30m.tif")
  r2 <- rast(outfile)
  if (p1 == "soil_conductivity") {
    r2[is.na(r2)] <- -999 #gm_mean function requires source data with no NA values
    dt1 <- execute_zonal(r2,geom=huc,ID="id",fun=geometric_mean,join=FALSE,summarize_df=TRUE,progress = TRUE)
  } else {
    dt1 <- execute_zonal(r2,geom=huc,ID="id",join=FALSE,progress = TRUE)
  }
  names(dt1)[2] <- p1
  attrs <- merge(attrs, dt1, by="id", all=TRUE)
  save(attrs,file=fout)
  rm(r2); gc()
}

message("compute data coverage for each parameter (if there are data gaps)")
for (p1 in pars) {
  if (paste0(p1,"_coverage") %in% names(attrs)) next
  message(p1)
  outfile <- paste0("../../datasets/Statsgo_huc",h1,"_v",ver1,"/huc",h1,"_",p1,"_30m.tif")
  r1 <- rast(outfile)
  r1[!is.na(r1)] <- 1
  r1[is.na(r1)] <- 0
  # use exact_extract since zonal does not work for fraction calculation for some reason
  huc <- st_transform(huc,crs(r1))
  prc1 <- exactextractr::exact_extract(r1,huc,fun="frac",append_cols = 'id', progress = TRUE,max_cells_in_memory=3e+07)
  prc1$frac_0 <- NULL
  names(prc1) <- c("id",paste0(p1,"_coverage"))
  attrs <- merge(attrs,prc1,by="id",all=TRUE)
  save(attrs,file=fout)
  rm(r1); gc()
}

# check attribtue summary
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}

# plot the attributes (multiple panels, no legend)
huc <- merge(huc,attrs, by="id",all=TRUE)
plot(huc[pars],border=NA)

# plot the attributes separately with legend
for (c1 in pars) {
  message(c1)
  png(filename = paste0("figs/attr_",c1,"_huc",h1,"_v",ver1,".png"),width = 5,height=5,units="in",res=300)
  print(plot(huc[c1], border=NA, key.pos=1))
  dev.off()
}

}}

