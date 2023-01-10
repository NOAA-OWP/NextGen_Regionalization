# compute statistics for simulations from different runs, each time skipping runs that have already been computed

rm(list=ls())

.libPaths("R_lib")
library(dplyr)
library(data.table)
source("calc_gof.R")

if (!require(hydroGOF)) install.packages("hydroGOF",lib="R_lib")
library(hydroGOF)

##################
date1 <- "20121001"; date2 <- "20160930"  #model run time period
#scenarios <- c("CFE_nse_dds_no_screening", "TOPMODEL_nse_dds_no_screening")
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
#statsOutFile <- paste0("stat/stat_retro_",format(startDate,"%Y%m%d"),"_",format(endDate, "%Y%m%d"),"_no_screening.Rdata")
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

# compare with FIHM runs
# library(ggplot2)
# library(magrittr)
# load("stat/stat_retro_20091001_20140930.Rdata")
# #s1 <- subset(stats_str,scenario=="CFE.hlr")
# s1 <- subset(stats_str,scenario=="TOPMODEL_hlr_kge-dds")
# load("../fihm/stat/stat_retro_20091001_20140930.Rdata")
# #s0 <- subset(stats_str, scenario=="noah_owp_CFE.hlr")
# s0 <- subset(stats_str, scenario=="noah_owp_TOPMODEL.hlr")

# metrics <- c("RMSE","PBIAS","NSE","CORR","KGE")
# dt1 <- merge(s0[,c("site_no",metrics),with=FALSE],s1[,c("site_no",metrics),with=FALSE],by="site_no",suffix=c(".old",".new"))

# dt1[["NSE.new"]][dt1[["NSE.new"]] < -1] <- -1
# dt1[["NSE.old"]][dt1[["NSE.old"]] < -1] <- -1
# dt1[["KGE.new"]][dt1[["KGE.new"]] < -1] <- -1
# dt1[["KGE.old"]][dt1[["KGE.old"]] < -1] <- -1
# dt1[["PBIAS.old"]] <- abs(dt1[["PBIAS.old"]])
# dt1[["PBIAS.new"]] <- abs(dt1[["PBIAS.new"]])
# dt1[["PBIAS.new"]][dt1[["PBIAS.new"]]>200] <- 200
# dt1[["PBIAS.old"]][dt1[["PBIAS.old"]]>200] <- 200

# dt2 <- melt(dt1,id.vars="site_no")
# dt2[,run:=ifelse(grepl("new",variable),"new","old")]
# dt2[,stat:=gsub(".old","",variable)]
# dt2[,stat:=gsub(".new","",stat)]
# dt3 <- dcast(dt2,site_no + stat ~ run, measure.vars=value)

# gages_calib <- get(load("output/v1.2/calib_gages_screened_kge_dds.Rdata"))
# #dt4 <- subset(dt3,site_no %in% gages_calib[["cfe"]])
# dt4 <- subset(dt3,site_no %in% gages_calib[["topmodel"]])

# gg1 <- ggplot() +
#        geom_point(data=dt3,aes(x=old, y=new),color="blue",shape=2) + geom_abline(slope=1) +
#        geom_point(data=dt4,aes(x=old, y=new),color="red",shape=3,size=3) +
#        facet_wrap(~stat, ncol=3, scales="free") 
       
# #ggsave("figs/compare_stat_CFE_hlr.png",gg1,width=9,height=7,units="in",dpi=300)
# ggsave("figs/compare_stat_TOPMODEL_hlr.png",gg1,width=9,height=7,units="in",dpi=300)