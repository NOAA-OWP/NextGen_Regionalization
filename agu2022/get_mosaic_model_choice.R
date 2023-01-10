rm(list=ls())

library(data.table)

ver_donor <- "v1.2"
ver_receiver <- "v0"
screening <- "kge_nse"

# calibration gages screened for performance
gages_calib <- get(load(paste0("output/",ver_receiver,"/calib_gages_screened_",screening,".Rdata")))

# stats simulated with old BMI and hydrofabric
load("stat/stat_retro_20131001_20160930_no_screening.Rdata")
stats0 <- subset(stats_str, site_no %in% gages_calib)
stats0$model <- sapply(stats0$scenario,function(x) strsplit(x,split="_")[[1]][1])
stats0$obj <- sapply(stats0$scenario,function(x) strsplit(x,split="_")[[1]][2])

dt1 <- data.table()
for (s1 in c("kge","nse")) {
  stats1 <- stats0[,c("site_no",toupper(s1),"model","obj"),with=FALSE]
  stats1 <- subset(stats1,obj==s1)
  stats1$obj <- NULL
  stats1 <- dcast(stats1,site_no ~ model, value.var=toupper(s1))
  stats1[,model:=ifelse(CFE>TOPMODEL,"CFE","TOPMODEL")]
  stats1$obj <- s1
  dt1 <- rbind(dt1,stats1)
}

save(dt1,file="output/v0/calib_gage_model_choice_mosaic.Rdata")