# plot/compare (violins of) summary stats of different regionalization runs 

rm(list=ls())
.libPaths("R_lib")
library(data.table)
library(magrittr)
if (!require(ztable)) install.packages("ztable",lib="R_lib")
library(ztable)

# calibration gages
gages_calib <- get(load("output/v0/calib_gages_screened_kge_nse.Rdata"))

# stats from new runs
load("stat/stat_retro_20131001_20160930.Rdata")
stats1 <- copy(stats_str)
stats1 <- subset(stats1, !site_no %in% gages_calib)

# stats from using all calib gages with no screening
load("stat/stat_retro_20131001_20160930_no_screening.Rdata")
stats0 <- copy(stats_str)
stats0 <- subset(stats0, site_no %in% gages_calib)
stats0$scenario <- gsub("_dds_no_screening","",stats0$scenario)
stats0$scenario <- gsub("_nse","_calib_nse",stats0$scenario)
stats0$scenario <- gsub("_kge","_calib_kge",stats0$scenario)

# combine stats
stats1 <- rbind(stats0,stats1)

# QC stats
stats <- subset(stats1, QMEAN>0 & t_n>1000)
stats[,nNSE:=1/(2-NSE)]

# compute median stats
stats2 <- subset(stats,!site_no %in% gages_calib) 
stats2 <- stats2[,c("site_no","scenario","KGE","NSE","nNSE","CORR","logNSE","PBIAS"),with=FALSE]
stats2$PBIAS <- abs(stats2$PBIAS)
#names(stats2) <- c("site_no","scenario","KGE","NSE","normalized NSE","Correlation","logrithm NSE","absolute % bias")
stats2 <- stats2 %>% melt(id.vars=c("site_no","scenario"))
# exclude gages with NA in KGE and CORR (due to constant simulated flow; 2 such gages: "01104430" "01115170")
gages1 <- unique(subset(stats2,is.na(value))$site_no)
stats2 <- subset(stats2,!site_no %in% gages1)
stats2 <- stats2[,.(value=quantile(value,0.5)),by=.(scenario,variable)]
stats2$value <- round(stats2$value,2)
stats2 <- dcast(stats2, scenario ~ variable, measure.vars="value")  
stats3 <- copy(stats2)
stats3$scenario <- NULL
stats3 <- as.matrix(stats3)
row.names(stats3) <- stats2$scenario

pal1 <- "YlGnBu"
#if (s1 %in% c("PBIAS"))  for (i1 in 2:7) dt3[,i1] <- abs(dt3[,i1])
z <- ztable(stats3)
z <- makeHeatmap(z,palette=pal1,margin=2)
print(z)
