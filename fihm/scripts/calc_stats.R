# compute statistics for simulations from different runs, each time skipping runs that have already been computed

rm(list=ls())

library(dplyr)
library(data.table)
source("calc_gof.R")

if (!require(hydroGOF)) install.packages("hydroGOF",lib="/home/***REMOVED***/R/x86_64-redhat-linux-gnu-library")

##################

date1 <- "20081001"; date2 <- "20140930"  #model run time period
regs <- c("default","camels","hlr")  # regionalization scenarios
runs <- c("CFE","TOPMODEL","CFE+TOPMODEL") # fomulation scenarios
forcs <- c("csv","netcdf") # forcing scenarios

# evaluation period
startDate <- as.POSIXct("2009100100","%Y%m%d%H",tz="UTC")
endDate <- as.POSIXct("2014093023","%Y%m%d%H",tz="UTC")
times <- seq(startDate, endDate, by="hour")

# load obs for all gages
obsDt <- get(load(paste0("../data/usgs_hourly_flow_",date1,"_",date2,"_huc01.Rdata")))

# get donor basins
df1 <- read.table("../data/headwater_catchments_huc01.txt",header=TRUE,colClasses = "character")
cats1 <- c("cat-15200", "cat-18746", "cat-33305") # not used as donors due to poor performance
calib_basins <- subset(df1, !catchment %in% cats1)$gages

# Stats file to write
statsOutFile <- paste0("../stat/stat_retro_",format(startDate,"%Y%m%d"),"_",format(endDate, "%Y%m%d"),".Rdata")
if (file.exists(statsOutFile)) {
    load(statsOutFile)
} else {
    stats_str <- data.table()
}
scenarios0 <- unique(stats_str$scenario)

# loop through all runs
startDateMin <- startDate
endDateMax <- endDate
for (f1 in forcs) {
for (reg1 in regs) {
for (run1 in runs) {

    scenario <- paste0("noah_owp_",run1,".",reg1)

    # check if model simulation file exists
    outfile <- paste0("../output/flow_",date1,"_",date2,"_",scenario,".Rdata")
    if (f1=="netcdf") {
        outfile <- paste0("../output/flow_",date1,"_",date2,"_",scenario,"_netcdf.Rdata")
        scenario <- paste0("noah_owp_",run1,".",reg1,"_netcdf")
    }
    if (scenario %in% scenarios0) next
    if (!file.exists(outfile)) next

    message(scenario)

    #load model simulation
    dtFlow <- get(load(outfile))
    dtFlow <- subset(dtFlow, validTime>=startDate & validTime<=endDate)
    modDt <- melt(dtFlow,id.vars="validTime",variable.name="site_no",value.name="q_cms")

    #Join mod and obs
    pairedData <- dplyr::inner_join(modDt, obsDt, by=c('site_no','validTime'),suffix=c('_mod','_obs'))
  
    #Calc stats
    statsTmp <- calc_gof(pairedData,
                               obsColName='q_cms_obs',
                               modColName='q_cms_mod',
                               groupVars=c('site_no')
    )

    # Reshape
    #statsTmp <- reshape2::dcast(statsTmp, site_no ~ statistic)
    statsTmp <- dcast(as.data.table(statsTmp), site_no ~ statistic)
  
    # Record count
    DT <- data.table(pairedData)
    countPairs <- DT[, .N, by = c("site_no")]
    names(countPairs)[2] <- "t_n"
    statsTmp <- plyr::join(statsTmp, countPairs, by="site_no")
    statsTmp$scenario <- scenario
  
    names(statsTmp)[names(statsTmp)=="ME"] <- "MSD"
    names(statsTmp)[names(statsTmp)=="r"] <- "CORR"

    if ("lat" %in% names(stats_str)) {
        statsTmp$seas <- "Full"
        statsTmp$lat <- stats_str$lat[match(statsTmp$site_no, stats_str$site_no)]
        statsTmp$lon <- stats_str$lon[match(statsTmp$site_no, stats_str$site_no)]
    }

    # Merge
    stats_str <- rbind(stats_str, statsTmp)
  
    # Track dates for plotting
    startDateMin <- min(startDateMin, min(pairedData$validTime))
    endDateMax <- min(endDateMax, max(pairedData$validTime))

    #Drop dataframe
    rm(pairedData, statsTmp)
    gc()
}}}

# Add plotting stuff
if (!"seas" %in% names(stats_str)) {
stats_str$seas <- "Full"
stats_qmean <- obsDt[, list(qmean=mean(q_cms, na.rm=TRUE)), by=list(site_no)]
stats_qmean$typ <- "Obs"
stats_qmean$seas <- "Full"
stats_dates <- list(Full=list(start=startDateMin, end=endDateMax))

# Join gage coords
load("../data/obsStrMeta_gages_retro.Rdata")
stnIDs <- unique(obsStrMeta[c("site_no", "dec_lat_va", "dec_long_va")])
stats_str <- plyr::join(stats_str, stnIDs, by="site_no")

# Change names
names(stats_str)[names(stats_str)=="ME"] <- "MSD"
names(stats_str)[names(stats_str)=="r"] <- "CORR"
names(stats_str)[names(stats_str)=="dec_lat_va"] <- "lat"
names(stats_str)[names(stats_str)=="dec_long_va"] <- "lon"

# Join gage coords
stats_str[stats_str=="Inf"]<-NA 
stats_str[stats_str=="-Inf"]<-NA
}

# Save
save(stats_str, stats_qmean, stats_dates, file=statsOutFile, compress=FALSE)

