rm(list=ls())

library(data.table)
library(sf)
library(magrittr)
library(ggplot2)
library(dplyr)

scenario <- "kge_dds"
period1 <- "valid"
run1 <- "valid_best"
model1 <- "topmodel"

gage_calib <- get(load("output/v1.2/calib_gages_screened_kge_dds.Rdata"))

# calibration results (50 gages)
f1 <- paste0("data/calib_",scenario,"/",model1,"_noah_kge_valid_stat.csv")
dtStats <- read.csv(f1,header=TRUE,colClasses=c("site_no"="character"))
dtStats1 <- subset(dtStats, run==run1 & period==period1)
dtStats1 <- dtStats1 %>% select(c(site_no,PBIAS,KGE,NSE)) %>% as.data.table()
dtStats1[["NSE"]][dtStats1[["NSE"]] < -1] <- -1
dtStats1[["KGE"]][dtStats1[["KGE"]] < -1] <- -1
dtStats1[["PBIAS"]] <- abs(dtStats1[["PBIAS"]])
dtStats1[["PBIAS"]][dtStats1[["PBIAS"]] > 100] <- 100
dtStats1 <- melt(dtStats1,id.vars="site_no")

# regionalizaion
load("stat/stat_retro_20131001_20160930.Rdata")
dtStats2 <- subset(stats_str,site_no %in% dtStats1$site_no)
dtStats2 <- subset(dtStats2,scenario==paste0(toupper(model1),"_hlr_kge_dds"))
dtStats2 <- dtStats2 %>% select(c(site_no,PBIAS,KGE,NSE))
dtStats2[["NSE"]][dtStats2[["NSE"]] < -1] <- -1
dtStats2[["KGE"]][dtStats2[["KGE"]] < -1] <- -1
dtStats2[["PBIAS"]] <- abs(dtStats2[["PBIAS"]])
dtStats2[["PBIAS"]][dtStats2[["PBIAS"]] > 100] <- 100
dtStats2 <- melt(dtStats2,id.vars="site_no")

dtAll <- merge(dtStats1,dtStats2,by=c("site_no","variable"),suffix=c(".calib",".reg"))
dtAll1 <- subset(dtAll,site_no %in% gages_calib[[model1]])

my_breaks <- function(x){seq(min(x), max(x), length.out = 5)}
my_labels <- function(x){round(x, digits = 2)}

gg1 <- ggplot(dtAll,aes(x=value.calib,y=value.reg)) +
  geom_point(color="blue",shape=2) + geom_abline(slope=1) +
  geom_point(data=dtAll1,aes(x=value.calib,y=value.reg),color="red",shape=2) +
  ggtitle(paste0(toupper(model1),": calib vs regionalization")) +
  xlab("Calibration results") + ylab("Regionalization results") +
  #scale_x_continuous(breaks = my_breaks, labels = my_labels) +
  #scale_y_continuous(breaks = my_breaks, labels = my_labels) +
  facet_wrap(~variable, ncol=3, scales="free") 

ggsave(paste0("figs/compare_stat_calib_reg_",model1,".png"),gg1,width=10,height=4,units="in",dpi=300)
