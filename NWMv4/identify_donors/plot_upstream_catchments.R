# plot upstream catchments given a gage

library(sf)

# hydrofabric version & huc region
ver1 <- "1.2"; h1 <- "17" 
ver1 <- "2.0pre"; h1 <- "12" 

# v3 calib gages
meta <- as.data.table(read.csv("../data/Domain_Meta_NWM_v3.0.csv",stringsAsFactors=FALSE))
meta <- meta[, c("gage_id", "lat", "lon","site_name"), with = FALSE]
meta <- meta[!duplicated(meta$gage_id)]
meta <- na.omit(meta)

# create sf points from gage lat/lon
sf_gages <- st_as_sf(meta, coords = c("lon","lat"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# plot upstream catchments
i1 <- 200
if (ver1 == "1.2") {
    
    gfile <- paste0('../../datasets/gpkg_v',ver1,'/upstream_catchment_huc',h1,'.gpkg')
    cats_all <- read_sf(gfile, "catchments")
    cats_all <- st_transform(cats_all, st_crs(sf_gages))
    gages1 <- unique(cats_all$gages)
    name1 <- subset(sf_gages, gage_id==gages1[i1])$site_name

    png(paste0("figs/upstream_catchments_",gages1[i1],".png"))
    plot(st_geometry(subset(cats_all,gages==gages1[i1])),col=NA,border="blue",lwd=0.5, main=paste0("USGS ",gages1[i1]," \n",name1))
    plot(st_geometry(subset(sf_gages, gage_id==gages1[i1])),pch=24,col="red",bg="yellow",cex=2,add=T)
} else if (ver1 == "2.0pre") {
    gfile <- list.files(paste0('../../datasets/gpkg_v',ver1,'/gages'),full.names=TRUE)[i1]
    cats <- read_sf(gfile, "divides")
    cats <- st_transform(cats, st_crs(sf_gages))
    flowpath <- read_sf(gfile,"flowpaths")
    flowpath <- st_transform(flowpath,st_crs(sf_gages))
    gage1 <- gsub(".gpkg","",basename(gfile))
    name1 <- subset(sf_gages, gage_id==gage1)$site_name
    png(paste0("figs/upstream_catchments_",gage1,".png"))
    plot(st_geometry(cats),col=NA,border="blue",lwd=0.5, main=paste0("USGS ",gage1," \n",name1))
    plot(st_geometry(flowpath),col="darkblue",lwd=1.5,add=T)
    plot(st_geometry(subset(sf_gages, gage_id==gage1)),pch=24,col="red",bg="yellow",cex=2,add=T)   
}

dev.off()
