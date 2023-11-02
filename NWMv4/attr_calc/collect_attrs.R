# collect all the catchment attributes that have been computed
# attributes for catchments with data coverage below the user defined threshold is set to missing

rm(list=ls())

library(data.table)
library(sf)

# data coverage threshold
coverageTh <- 0.5

vers <- c("2.0pre","1.2")
hucs <- c("12","17")
for (ver1 in vers)  {

# shapefile of catchments in HUC
h1 <- hucs[match(ver1, vers)]
huc <- read_sf(paste0("../../datasets/gpkg_v",ver1,"/nextgen_",h1,".gpkg"),"divides")
if ("divide_id" %in% names(huc)) {
    huc$id <- NULL
    names(huc)[names(huc)=="divide_id"] <- "id"
}
dtAttrAll <- data.table(id=huc$id)

# climate attributes
attrs <- get(load(paste0("output/clim_attr_huc",h1,"_v",ver1,".Rdata")))
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# topo attributes
attrs <- get(load(paste0("output/topo_attr_huc",h1,"_v",ver1,".Rdata")))
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# soil attributes (STATSGO)
attrs <- get(load(paste0("output/soil_attr_huc",h1,"_v",ver1,"_statsgo.Rdata")))
pars <- names(attrs)
pars <- pars[!grepl("_coverage",pars)]
pars <- pars[pars!="id"]
for (p1 in pars) attrs[,eval(p1):=ifelse(get(paste0(p1,"_coverage"))>=coverageTh,get(p1),NA)]
attrs <- attrs[,c("id",pars),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)
names(dtAttrAll)[names(dtAttrAll)=="water_frac"] <- "water_soil" # to avoid using the same name from land cover

# hydrologic soil group
attrs <- get(load(paste0("output/hsg_attr_huc",h1,"_v",ver1,".Rdata")))
attrs <- attrs[,c("id","hsg"),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# geology attributes
attrs <- get(load(paste0("output/geo_attr_huc",h1,"_v",ver1,".Rdata")))
pars <- names(attrs)
pars <- pars[!grepl("_coverage",pars)]
pars <- pars[pars!="id"]
for (p1 in pars) attrs[,eval(p1):=ifelse(get(paste0(p1,"_coverage"))>=coverageTh,get(p1),NA)]
attrs <- attrs[,c("id",pars),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

# landcover attributes (NLCD + NWM 1km)
attrs <- get(load(paste0("output/landcover_attr_huc",h1,"_v",ver1,".Rdata")))
pars <- names(attrs)
pars <- pars[grepl("_nlcd",pars)]
pars <- gsub("_nlcd","",pars)
for (p1 in pars) 
  attrs[,eval(paste0(p1,"_frac")):=ifelse(is.na(nlcd_coverage),get(paste0(p1,"_nwm")),ifelse(nlcd_coverage>=coverageTh,get(paste0(p1,"_nlcd")),get(paste0(p1,"_nwm"))))]
attrs <- attrs[,c("id",paste0(pars,"_frac"),"gvf_max","gvf_diff","lai_max","lai_diff"),with=FALSE]
dtAttrAll <- merge(dtAttrAll,attrs,by="id",all.x=TRUE)

message("\nNumber of NA values for each attribute for huc", h1," v", ver1)
for(c1 in names(dtAttrAll)) message(paste0(c1," ",sum(is.na(dtAttrAll[[c1]]))))

write.csv(dtAttrAll,file=paste0("output/all_attrs_huc",h1,"_v",ver1,".csv"),quote=FALSE,row.names=FALSE)
}
