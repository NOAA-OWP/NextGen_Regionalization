rm(list=ls())

library(dplyr)
library(data.table)
source("calc_gof.R")

if (!require(hydroGOF)) install.packages("hydroGOF")

##################

date1 <- "20081001"; date2 <- "20140930"  #model run time period
regs <- "camels"
runs <- c("CFE_default","TOPMODEL_default","CFE_donated","TOPMODEL_donated","mosaic_donated")

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

# loop through all runs
stats_str <- data.table()
startDateMin <- startDate
endDateMax <- endDate
for (reg1 in regs) {
for (run1 in runs) {

    scenario <- paste0("noah_owp_",run1)
    message(scenario)

    #load model simulation
    outfile <- paste0("../output/flow_",date1,"_",date2,"_",reg1,"_",scenario,".Rdata")
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
  
    # Merge
    stats_str <- rbind(stats_str, statsTmp)
  
    # Track dates for plotting
    startDateMin <- min(startDateMin, min(pairedData$validTime))
    endDateMax <- min(endDateMax, max(pairedData$validTime))

    #Drop dataframe
    rm(pairedData, statsTmp)
    gc()
}}

# Add plotting stuff
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
#names(stats_str)[names(stats_str)=="ME"] <- "t_msd"
names(stats_str)[names(stats_str)=="ME"] <- "MSD"
#names(stats_str)[names(stats_str)=="RMSE"] <- "t_rmse"
#names(stats_str)[names(stats_str)=="PBIAS"] <- "t_bias"
#names(stats_str)[names(stats_str)=="NSE"] <- "t_nse"
#names(stats_str)[names(stats_str)=="r"] <- "t_cor"
names(stats_str)[names(stats_str)=="r"] <- "CORR"
#names(stats_str)[names(stats_str)=="logNSE"] <- "t_lognse"
#names(stats_str)[names(stats_str)=="wtNSE"] <- "t_wtnse"
names(stats_str)[names(stats_str)=="dec_lat_va"] <- "lat"
names(stats_str)[names(stats_str)=="dec_long_va"] <- "lon"

# Join gage coords
stats_str[stats_str=="Inf"]<-NA 
stats_str[stats_str=="-Inf"]<-NA

# Save
save(stats_str, stats_qmean, stats_dates, file=statsOutFile, compress=FALSE)

