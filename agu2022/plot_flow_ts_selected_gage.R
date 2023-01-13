# plot streamflow time series for selected gages and scenarios

rm(list=ls())

library(data.table)
library(ggplot2)
library(magrittr)
library(dplyr)
library(gridExtra)

val_date1 <- "20131001"
val_date2 <- "20160930"
sim_date1 <- "20121001"
sim_date2 <- "20160930"
times <- seq(as.POSIXct(val_date1,"%Y%m%d",tz="UTC"), 
             as.POSIXct(paste0(val_date2,"23"),"%Y%m%d%H",tz="UTC"),by="hour")

# observation
obsDt <- get(load(paste0("../datasets/usgs_hourly_flow_",sim_date1,"_",sim_date2,"_huc01.Rdata")))
obsDt <- subset(obsDt, validTime %in% times)

# calibration gages
gages_calib <- get(load("output/v0/calib_gages_screened_kge_nse.Rdata"))

# stats from different runs
load(paste0("stat/stat_retro_",val_date1,"_",val_date2,".Rdata"))
stats1 <- subset(stats_str, !site_no %in% gages_calib)
stats1$scenario <-gsub("CFE[+]TOPMODEL","mixed", stats1$scenario)

# get the gage which has the largest KGE difference among the different scenarios
gage1 <- stats1 %>% select(site_no,scenario,KGE) %>% 
  .[,.(min1=min(KGE),max1=max(KGE)),by=.(site_no)] %>% 
  .[,range1:=max1-min1] %>% 
  filter(max1>0.6) %>%
  slice_max(range1) %>%
  pull(site_no) 

scenarios <- c("CFE_camels_kge","CFE_camels_nse","mixed_camels_nse")
stats2 <- subset(stats1,site_no==gage1 & scenario %in% scenarios) %>% 
  select(scenario,KGE,NSE,CORR,PBIAS)
stats2[["KGE"]] <- round(stats2[["KGE"]],2)
stats2[["NSE"]] <- round(stats2[["NSE"]],2)
stats2[["CORR"]] <- round(stats2[["CORR"]],2)
stats2[["model"]] <- sapply(strsplit(stats2$scenario,split="_"),function(x) x[1])
stats2[["reg_scenario"]]=sapply(strsplit(stats2$scenario,split="_"),function(x) x[2])
stats2[["calib_obj_func"]]=sapply(strsplit(stats2$scenario,split="_"),function(x) x[3])
stats2 <- stats2 %>% select(scenario,model,calib_obj_func,reg_scenario,KGE,NSE,CORR,PBIAS)
row.names(stats2) <- NULL

# model simulations
dtMod <- data.table()
for (s1 in scenarios) {
s2 <- gsub("mixed","CFE+TOPMODEL",s1)
dtFlow <- get(load(paste0("flows/flow_",sim_date1,"_",sim_date2,"_",s2,".Rdata")))
tmp <- dtFlow %>% select(validTime,gage1) %>% 
  filter(validTime %in% times) %>% 
  setnames(c("validTime","mod"))
tmp$scenario <- s1
dtMod <- rbind(dtMod, tmp)
}

# merge obs and mod
dtMod <- dcast(dtMod,validTime ~ scenario, value.var = "mod")
obsDt1 <- obsDt %>% filter(site_no==gage1) %>% 
  select("validTime","q_cms") %>% 
  setnames(c("validTime","USGS"))
dtAll <- merge(dtMod, obsDt1, by="validTime", all.x=TRUE)
dtAll <- melt(dtAll,id.vars = "validTime")
dtAll$variable <- factor(dtAll$variable,levels=c("USGS",scenarios))
levels(dtAll$variable) <- c("USGS","CFE_kge_camels","CFE_nse_camels","mixed_nse_camels")

# plot obs vs. model simulation time series
mycolors <- c("black","red","gold4","blue")
gg1 <- ggplot(dtAll,aes(x=validTime,y=value)) +
  geom_line(aes(group=variable,color=variable,linetype=variable)) +
  scale_color_manual(name="",values=mycolors) +
  scale_linetype_manual(name="",values=c("solid","dashed","solid","solid")) +
  labs(x="Time",y="Streamflow (cms)", title="Flow Observation vs. Simulations") +
  theme(text=element_text(size=14), plot.title=element_text(size=20,hjust=0.5),
        legend.position="right",legend.text = element_text(size=14))

stats2$PBIAS <- abs(stats2$PBIAS)
stats2$scenario[stats2$scenario=="CFE_camels_kge"] <- "CFE_kge_camels"
stats2$scenario[stats2$scenario=="CFE_camels_nse"] <- "CFE_nse_camels"
stats2$scenario[stats2$scenario=="mixed_camels_nse"] <- "mixed_nse_camels"
gg2 <- ggplot()+ theme_minimal() +
  annotation_custom(tableGrob(stats2))

ggsave(file=paste0("figs/flow_ts_",gage1,".png"),plot=gg1,width=12.5,height=5,units="in",dpi=300)
ggsave(file=paste0("figs/stats_table_",gage1,".png"),plot=gg2,width=8,height=3,units="in",dpi=300)




