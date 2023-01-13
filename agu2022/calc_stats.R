# compute statistics for simulations from different runs, each time skipping runs 
# that have already been computed

rm(list=ls())

.libPaths("R_lib")
library(dplyr)
library(data.table)
source("calc_gof.R")

if (!require(hydroGOF)) install.packages("hydroGOF",lib="R_lib")
library(hydroGOF)

##################
date1 <- "20121001"; date2 <- "20160930"  #model run time period
scenarios <- c("TOPMODEL_hlr_kge", "CFE_hlr_kge", "TOPMODEL_camels_kge", "CFE_camels_kge", 
               "CFE+TOPMODEL_hlr_kge", "CFE+TOPMODEL_camels_kge",
               "TOPMODEL_hlr_nse", "CFE_hlr_nse", "TOPMODEL_camels_nse", "CFE_camels_nse",
               "CFE+TOPMODEL_hlr_nse", "CFE+TOPMODEL_camels_nse")

# evaluation period
startDate <- as.POSIXct("2013100100","%Y%m%d%H",tz="UTC")
endDate <- as.POSIXct("2016093023","%Y%m%d%H",tz="UTC")
times <- seq(startDate, endDate, by="hour")

# load obs for all gages
obsDt <- get(load(paste0("../datasets/usgs_hourly_flow_",date1,"_",date2,"_huc01.Rdata")))

# Stats file to write
statsOutFile <- paste0("stat/stat_retro_",format(startDate,"%Y%m%d"),"_",format(endDate, "%Y%m%d"),".Rdata")
if (file.exists(statsOutFile)) {
    load(statsOutFile)
} else {
    stats_str <- data.table()
}
scenarios0 <- unique(stats_str$scenario)

# loop through all runs
startDateMin <- startDate
endDateMax <- endDate
for (scenario in scenarios) {

    # check if model simulation file exists
    outfile <- paste0("flows/flow_",date1,"_",date2,"_",scenario,".Rdata")
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
}

# Add plotting stuff
if (!"seas" %in% names(stats_str)) {
stats_str$seas <- "Full"
stats_qmean <- obsDt[, list(qmean=mean(q_cms, na.rm=TRUE)), by=list(site_no)]
stats_qmean$typ <- "Obs"
stats_qmean$seas <- "Full"
stats_dates <- list(Full=list(start=startDateMin, end=endDateMax))

# Join gage coords
load("../datasets/obsStrMeta_gages_retro.Rdata")
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

