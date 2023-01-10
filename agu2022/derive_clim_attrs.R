# Derive climate attributes (P, PET, aridity etc) from AORC data

rm(list=ls())


library(data.table)
library(zonal)
library(sf)
library(raster)

for (ver1 in c("v1.2","v0")) {

if (ver1 == "v1.2") {
  date1 <- "20121001"; date2 <- "20210831"; years <- 2013:2020
} else if (ver1 == "v0") {
  date1 <- "20070101"; date2 <- "20191231"; years <- 2008:2019
}

hours0 <- seq(as.POSIXct(date1,format="%Y%m%d",tz="UTC"),
              as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H",tz="UTC"),by="hour")
hours1 <- seq(as.POSIXct(paste0(years[1]-1,"1001"),format="%Y%m%d",tz="UTC"),
              as.POSIXct(paste0(years[length(years)],"093023"),format="%Y%m%d%H",tz="UTC"),by="hour")
ix1 <- match(hours1,hours0)
if (sum(is.na(ix1))>0) stop("ERROR: dataset is not complete!")
nyear <- length(years)


# read HUC-01 geojson into sf
huc01 <- st_read(paste0("../datasets/gpkg_",ver1,"/catchment_data.geojson"))

# initialize attributes table
attrs <- data.table(id=huc01$id)

# compute mean annual pcp
pcp <- get(load(paste0("../datasets/AORC_hydrofabric_",ver1,"/aorc_RAINRATE_huc01_",date1,"-",date2,"_hourly.Rdata")))
pmean <- sapply(pcp, function(x) sum(x[ix1])/nyear)
attrs <- merge(attrs, data.table(id=names(pmean),p_mean=pmean),by="id",all=TRUE)

# mean annual PET
pet <- get(load(paste0("output/",ver1,"/aorc_pet_huc01.Rdata")))
names(pet) <- c("id","pet_mean")
pet$pet_mean <- pet$pet_mean * 0.408  #covert from MJ/m2 to mm
attrs <- merge(attrs, pet, by="id",all=TRUE)

# compute aridity
attrs[,aridity:=pet_mean/p_mean]

# compute Feddema moisture index
attrs[,FMI:=ifelse(p_mean>=pet_mean, 1-pet_mean/p_mean, p_mean/pet_mean-1)]

# compute snow fraction
ta <- get(load(paste0("../datasets/AORC_hydrofabric_",ver1,"/aorc_T2D_huc01_",date1,"-",date2,"_hourly.Rdata")))
ta1 <- lapply(ta,function(x) colMeans(matrix(x[ix1],nrow=24)))
pcp1 <- lapply(pcp,function(x) colSums(matrix(x[ix1],nrow=24)))
sf1 <- sapply(names(ta1), function(x) round(sum(pcp1[[x]][ta1[[x]]<273.15])/sum(pcp1[[x]]),2))
attrs <- merge(attrs, data.table(id=names(sf1),snow_frac=sf1),by="id",all=TRUE)

# high and low precip frequency and duration
dtPcpFreq <- get(load(paste0("output/",ver1,"/aorc_pcp_freq_duration_huc01_",years[1],"-",years[length(years)],".Rdata")))
names(dtPcpFreq) <- c("id","high_prec_freq","low_prec_freq","high_prec_dur","low_prec_dur")
attrs <- merge(attrs,dtPcpFreq,by="id", all=TRUE)

# deal with unrealic values from zero pcp
ix1 <- which(attrs$p_mean == 0)
attrs$high_prec_freq[ix1] <- NA
attrs$high_prec_dur[ix1] <- NA
attrs$low_prec_freq[ix1] <- NA
attrs$low_prec_dur[ix1] <- NA

save(attrs,file=paste0("output/",ver1,"/clim_attr_huc01.Rdata"))

# check attribtue summary
pars <- names(attrs)
pars <- pars[pars != "id"]
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}

huc01 <- merge(huc01, attrs, by="id", all=TRUE)

# plot the attributes (multiple panels, no legend)
#plot(huc01[pars],border=NA)

# plot the attributes separately with legend
for (c1 in pars) {
  message(c1)
  png(filename = paste0("figs/",ver1,"/attr_",c1,"_huc01.png"),width = 5,height=5,units="in",res=300)
  print(plot(huc01[c1], border=NA, key.pos=1))
  dev.off()
}

} #ver1

