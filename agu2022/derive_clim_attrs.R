# Derive climate attributes (P, PET, aridity etc) from AORC data

rm(list=ls())

library(data.table)
library(zonal)
library(sf)
library(raster)

# load regridded 1km climate data (from NWM v3)
period1 <- "2008-2021"
apcp0 <- get(load(paste0("../datasets/AORC/mean_annual_APCP_",period1,"_AORC.Rdata")))
pet0 <- get(load(paste0("../datasets/AORC/mean_annual_PET_",period1,"_AORC.Rdata")))
p1 <- raster(t(apcp0)[nrow(t(apcp0)):1,])
pe1 <- raster(t(pet0)[nrow(t(pet0)):1,])
sf <- get(load(paste0("../datasets/AORC/mean_annual_snow_frac_",period1,".Rdata")))
sf <- raster(t(sf)[nrow(t(sf)):1,])

# set the raster CRS, extent, resolution
r1 <- raster("../datasets/hucs.tif") #fron NWM, which provides the crs, extent, and res of NWM 1km grids
crs(p1) <- crs(pe1) <- crs(sf) <- crs(r1)
extent(p1) <- extent(pe1) <- extent(sf) <- extent(r1)
res(p1) <- res(pe1) <- res(sf) <- res(r1)

# read HUC-01 geojson into sf
huc01 <- st_read("shapefile/catchment_data_drano.geojson")

# initialize attributes table
attrs <- data.table(id=huc01$id)

# compute areal mean annual pcp
dt1 <- execute_zonal(p1,geom=huc01,ID="id",fun="mean",join=FALSE)
names(dt1)<- c("id","p_mean")
attrs <- merge(attrs, dt1, by="id", all=TRUE)

# compute areal mean annual PET
dt1 <- execute_zonal(pe1,geom=huc01,ID="id",fun="mean",join=FALSE)
names(dt1)<- c("id","pet_mean")
attrs <- merge(attrs, dt1, by="id", all=TRUE)

# compute aridity
attrs[,aridity:=pet_mean/p_mean]

# compute Feddema moisture index
attrs[,FMI:=ifelse(p_mean>=pet_mean, 1-pet_mean/p_mean, p_mean/pet_mean-1)]

# snow fraction
dt1 <- execute_zonal(sf,geom=huc01,ID="id",fun="mean",join=FALSE)
names(dt1)<- c("id","snow_frac")
attrs <- merge(attrs, dt1, by="id", all=TRUE)

# high and low precip frequency and duration
dtPcpFreq <- get(load(paste0("data/aorc_pcp_freq_duration_huc01_",period1,".Rdata")))
names(dtPcpFreq) <- c("id","high_prec_freq","low_prec_freq","high_prec_dur","low_prec_dur")
attrs <- merge(attrs,dtPcpFreq,by="id", all=TRUE)

# deal with unrealic values from zero pcp
ix1 <- which(attrs$p_mean == 0)
attrs$high_prec_freq[ix1] <- NA
attrs$high_prec_dur[ix1] <- NA
attrs$low_prec_freq[ix1] <- NA
attrs$low_prec_dur[ix1] <- NA

save(attrs,file="output/clim_attr_huc01.Rdata")

# check attribtue summary
pars <- names(attrs)
pars <- pars[!pars %in% c("id","geometry")]
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}

# plot the attributes (multiple panels, no legend)
plot(huc01[pars],border=NA)

# plot the attributes separately with legend
huc01 <- merge(huc01, attrs, all.x=TRUE, by="id")
for (c1 in pars) {
  message(c1)
  png(filename = paste0("figs/attr_",c1,"_huc01.png"),width = 5,height=5,units="in",res=300)
  print(plot(huc01[c1], border=NA, key.pos=1))
  dev.off()
}


