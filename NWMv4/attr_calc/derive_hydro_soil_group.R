# derive the hydrologic soil group (HSG) for catchments based on the original 250m data

rm(list=ls())

library(zonal)
library(sf)
library(data.table)

vers <- c("2.0pre","1.2")
hucs <- c("12","17")
for (ver1 in vers)  {

# read in catchments in huc
h1 <- hucs[match(ver1, vers)]
huc <- read_sf(paste0("../../datasets/gpkg_v",ver1,"/nextgen_",h1,".gpkg"),"divides")
if ("divide_id" %in% names(huc)) {
    huc$id <- NULL
    names(huc)[names(huc)=="divide_id"] <- "id"
}

# compute dominant HSG class
attrs <- data.table(execute_zonal("../../datasets/HYSOGs250m/HYSOGs250m.tif",geom=huc,ID="id",fun="mode",join=FALSE,progress=TRUE))
names(attrs)[2] <- "hsg"

# map to letters (A,B,C,D)
attrs$hsg <- factor(attrs$hsg, levels=c(1:4,11:14), labels=c("A","B","C",rep("D",5)))

# save data
save(attrs,file=paste0("../output_attr/hsg_attr_huc",h1,"_v",ver1,".Rdata"))

}