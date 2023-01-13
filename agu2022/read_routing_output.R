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

date1 <- "20121001"
date2 <- "20160930"

scenarios <- C("CFE_hlr_kge", "TOPMODEL_hlr_kge","TOPMODEL_camels_kge","CFE_camels_kge", 
               "CFE_hlr_nse","TOPMODEL_hlr_nse","CFE+TOPMODEL_camels_kge","CFE+TOPMODEL_hlr_kge",
               "CFE_camels_nse","TOPMODEL_camels_nse","CFE+TOPMODEL_hlr_nse","CFE+TOPMODEL_camels_nse")

# get crosswalk table
cwt <- read.csv("data/nhd-crosswalk.csv")
cwt <- as.data.table(subset(cwt,gages!=""))
gages1 <- strsplit(cwt$gages,split=",")
gages1 <- lapply(gages1, function(x) unique(x))
cwt1 <- subset(cwt,sapply(gages1,function(x) length(x))==1)
cwt1 <- cwt1[,c("ID","gages"),with=F]
cwt1 <- cwt1[!duplicated(cwt1),]
gages2 <- cwt1$gages[duplicated(cwt1$gages)] # gages corresponding to more than one catchments
cwt1 <- subset(cwt1,!gages %in% gages2)
cwt1 <- cwt1[,.(gage=unique(strsplit(gages,split=","))[[1]]),by=.(ID)]
cwt1 <- cwt1[!duplicated(cwt1),]

# loop through regionalization and model scenarios
for (scenario in scenarios) {

    message(scenario)
    
    # read in output in hdf5 format
    file1 <- paste0("routing_outputs/flowveldepth_",scenario,".h5")
    #h5f <- H5Fopen(file1,flags="H5F_ACC_RDONLY")
    h5f <- H5Fopen(file1)
    flow <- h5f&"/qvd/block0_values"
    ids <- h5f$"/qvd/axis1"

    # Get indices for retrieving hourly streamflow for all gages in the crosswalk table
    # Note the hdf5 file stores three variables (flow, velocity & depth) every 5 minutes
    dims <- as.numeric(strsplit(subset(h5ls(file1),group=="/qvd" & name=="block0_values")$dim,split=" x ")[[1]])   
    idx1 <- seq(1,dims[1],by=36) # time dimension
    idx2 <- match(cwt1$ID,ids) # catchment dimension
    flows <- flow[idx1,idx2]
    flows <- as.data.table(flows)
    names(flows) <- cwt1$gage

    # create flow data tabe with valid time and gage names
    dt1 <- data.table(validTime=seq(as.POSIXct(paste0(date1,"00"),format="%Y%m%d%H",tz="UTC"),
                                as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H",tz="UTC"),by="hour"))

    dtFlow <- cbind(dt1,flows)
    h5closeAll()
    save(dtFlow,file=paste0("flows/flow_",date1,"_",date2,"_",scenario,".Rdata"))
    
}
