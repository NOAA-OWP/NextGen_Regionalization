# read HDF5 output from t-route into Rdata format for all gages in the model domain

rm(list=ls())

if(!require(rhdf5)) {
    install.packages("BiocManager")
    BiocManager::install("rhdf5")
}

library(rhdf5)
library(sf)
library(data.table)
library(magrittr)

date1 <- "20081001"
date2 <- "20140930"
regs <- "hlr" # regionalization scenarios
runs <- c("CFE","TOPMODEL","mosaic") # model scenarios
runs <- c("CFE")
calib_scenario <- "kge-dds"

# get crosswalk table
cwt <- read.csv("../data/nhd-crosswalk.csv")
cwt <- subset(cwt,gages!="")
gages <- cwt$gages
gages <- strsplit(cwt$gages,split=",")
gages <- lapply(gages, function(x) unique(x))
cwt1 <- subset(cwt,sapply(gages,function(x) length(x))==1)
cwt1 <- cwt1[,c("ID","gages")]
cwt1 <- as.data.table(cwt1[!duplicated(cwt1),])
cwt1 <- subset(cwt1,!(duplicated(gages)))
cwt1 <- cwt1[,.(gage=unique(strsplit(gages,split=","))[[1]]),by=.(ID)]
cwt1 <- cwt1[!duplicated(cwt1),]

# loop through regionalization and model scenarios
for (reg1 in regs) {
for (run1 in runs) {

    #scenario <- paste0("noah_owp_",run1,".",reg1)
    scenario <- paste0(run1,"_",reg1,"_",calib_scenario)
    message(scenario)
    
    # read in output in hdf5 format
    file1 <- paste0("../routing_outputs/flowveldepth_",scenario,".h5")
    h5f <- H5Fopen(file1)
    flow <- h5f&"/qvd/block0_values"
    ids <- h5f$"/qvd/axis1"

    # Get indices for retrieving hourly streamflow for all gages in the crosswalk table
    # Note the hdf5 file stores three variables (flow, velocity & depth) every 5 minutes
    dims <- as.numeric(strsplit(subset(h5ls(file1),group=="/qvd" & name=="block0_values")$dim,split=" x ")[[1]])   
    #idx1 <- seq(1,dims[1],by=36) # time dimension
    idx1 <- seq(1,dims[1],by=3) # time dimension
    idx2 <- match(cwt1$ID,ids) # catchment dimension
    flows <- flow[idx1,idx2]
    flows <- as.data.table(flows)
    names(flows) <- cwt1$gage

    # create flow data tabe with valid time and gage names
    dt1 <- data.table(validTime=seq(as.POSIXct(paste0(date1,"00"),format="%Y%m%d%H",tz="UTC"),
                                as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H",tz="UTC"),by="hour"))

    dtFlow <- cbind(dt1,flows)
    save(dtFlow,file=paste0("../output/flow_",date1,"_",date2,"_",scenario,".Rdata"))
    h5closeAll()
}}