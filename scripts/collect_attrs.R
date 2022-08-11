# collect all the catchment attributes that have been computed
# attributes for catchments with data coverage below the user defined threshold is set to missing

rm(list=ls())

library(data.table)
library(sf)

# data coverage threshold
coverageTh <- 0.5

# shapefile of HUC-01 catchments
huc01 <- st_read("../shapefile/catchment_data.geojson") 
dtAttrAll <- data.table(id=huc01$id)

# climate attributes
attrs <- get(load("../output/clim_attr_huc01.Rdata"))
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# topo attributes
attrs <- get(load("../output/topo_attr_huc01.Rdata"))
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# soil attributes (STATSGO)
attrs <- get(load("../output/soil_attr_huc01_statsgo.Rdata"))
pars <- names(attrs)
pars <- pars[!grepl("_coverage",pars)]
pars <- pars[pars!="id"]
for (p1 in pars) attrs[,eval(p1):=ifelse(get(paste0(p1,"_coverage"))>=coverageTh,get(p1),NA)]
attrs <- attrs[,c("id",pars),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# hydrologic soil group
attrs <- get(load("../output/hsg_attr_huc01.Rdata"))
attrs <- attrs[,c("id","hsg"),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# geology attributes
attrs <- get(load("../output/geo_attr_huc01_GLHYMPS.Rdata"))
pars <- names(attrs)
pars <- pars[!grepl("_coverage",pars)]
pars <- pars[pars!="id"]
for (p1 in pars) attrs[,eval(p1):=ifelse(get(paste0(p1,"_coverage"))>=coverageTh,get(p1),NA)]
attrs <- attrs[,c("id",pars),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# landcover attributes (NLCD + NWM 1km)
attrs <- get(load("../output/landcover_attr_huc01.Rdata"))
pars <- names(attrs)
pars <- pars[grepl("_nlcd",pars)]
pars <- gsub("_nlcd","",pars)
for (p1 in pars) 
  attrs[,eval(paste0(p1,"_frac")):=ifelse(is.na(nlcd_coverage),get(paste0(p1,"_nwm")),ifelse(nlcd_coverage>=coverageTh,get(paste0(p1,"_nlcd")),get(paste0(p1,"_nwm"))))]
attrs <- attrs[,c("id",paste0(pars,"_frac"),"gvf_max","gvf_diff","lai_max","lai_diff"),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

save(dtAttrAll,file="../output/all_attrs.Rdata")
