# Derive soil attributes from Polaris data

rm(list=ls())

library(data.table)
library(zonal)
library(sf)
library(terra)
library(raster)
library(units)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson")
huc01$toid <- huc01$area_sqkm <- NULL

# load the attributes already calcualted to start from where last run ends (if applicable)
outfile <- "../output/soil_attr_huc01_polaris.Rdata"
attrs <- data.table(id=huc01$id)
if (file.exists(outfile)) attrs <- get(load(outfile))

# loop through parameters and layers to compute catchment mean
pars <- c("silt","sand","clay","bd","theta_s","theta_r","ksat","ph","om","lambda","hb","n","alpha")
layers <- c("0_5","5_15","15_30","30_60","60_100","100_200")
for (p1 in pars) {
  for (l1 in layers) {
    str1 <- paste0(p1,"_",l1)
    if (str1 %in% names(attrs)) next
    
    message(paste0(p1,"  ", l1))
    huc01 <- execute_zonal(paste0("../datasets/Polaris_huc01/",p1,"_mean_",l1,".tif"), geom=huc01,
                           ID="id",join=TRUE)
    names(huc01)[names(huc01)=="V1"] <- str1
    
    # save the attributes
    tmp <- as.data.table(huc01)
    tmp$geometry <- NULL
    cols <- names(tmp)
    cols <- cols[! cols %in% names(attrs)]
    attrs <- merge(attrs, tmp[,c("id",cols),with=F],by="id")
    save(attrs, file=outfile)
  }
}

# compute multi-layer averages
for (p1 in pars) {
  tmp <- attrs[,names(attrs)[grepl(paste0(p1,"_"),names(attrs),fixed=T)],with=F]
  attrs[[paste0(p1,"_ave")]] <- rowMeans(tmp)
}
save(attrs, file=outfile)

# compute data coverage
r1 <- raster(paste0("../datasets/Polaris_huc01/",p1,"_mean_",l1,".tif"))
r2 <- r1
r2[] <- 1
r2[is.na(r1)] <- 0
r2 <- ratify(r2)
rat <- levels(r2)[[1]]
rat$cat <- c("invalid","valid")
levels(r2) <- rat
rcl <- dplyr::select(rat,from=ID,to=cat)
prc0 <- execute_zonal(r2,geom=huc01,ID="id",FUN="freq",rcl=rcl, join=FALSE)
prc1 <- subset(prc0,value=="valid")
prc1$value <- NULL
names(prc1) <- c("id","coverage")
attrs <- merge(attrs, prc1,by="id",all.x=TRUE)
save(attrs, file=outfile)

# merge with geom
cols <- names(attrs)
cols <- cols[!cols %in% names(huc01)]
huc01 <- merge(huc01,attrs[,c("id",cols),with=F],by="id")

# for plotting, set the attribute vlaue to NA if data coverage less than minimum coverage
for (c1 in names(huc01)) {
  if (c1 %in% c("id","coverage","geometry")) next
  huc01[[c1]] <- ifelse(huc01$coverage>=minCoverage,huc01[[c1]],NA)
}

# plot by parameter
library(tidyr)
library(dplyr)
library(ggplot2)
for (p1 in pars) {
  message(p1)
  dt1 <- as.data.table(huc01[,c("id","donor",p1)])
  dt1$geometry <- NULL
  names(dt1)[names(dt1)==p1] <- "value"
  dt1[,dist_prc:=abs(dt1$value[match(dt1$donor,dt1$id)]-dt1$value)/diff(range(dt1$value,na.rm=TRUE))]
  
  sf1 <- huc01 %>% select(names(huc01)[grepl(paste0(p1,"_"),names(huc01))]) %>% gather(VAR, value, -geometry)
  sf1$VAR <- factor(sf1$VAR, levels=paste0(p1,"_",c(layers,"ave")))
  gg1 <- ggplot() + 
    geom_sf(data = sf1, mapping=aes(fill = value),color=NA) + 
    facet_wrap(~VAR, ncol = 4)
  ggsave(paste0("../figs/attr_",p1,"_polaris_huc01.png"),gg1,width=12,height=8,units="in",dpi=300)
}


