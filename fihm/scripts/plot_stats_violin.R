# plot/compare (violins of) summary stats of different regionalization runs 

rm(list=ls())
library(ggplot2)
library(viridis)

# load stats 
load("../stat/stat_retro_20091001_20140930.Rdata")

# QC stats
stats <- subset(stats_str, QMEAN>0 & t_n>1000)
stats[,nNSE:=1/(2-NSE)]
stat_names <- c("KGE","NSE","nNSE","CORR","RMSE", "PBIAS")
stat_names <- c("nNSE","KGE")
dt1 <- stats[, c("site_no","scenario",stat_names), with=FALSE]

# remove the incorrect netcdf run
dt1 <- subset(dt1,!grepl("netcdf",scenario))

# rescale
dt2 <- copy(dt1)
dt2$KGE[dt2$KGE < -1] <- -1 
#dt2$NSE[dt2$NSE < -1] <- -1 
#dt2$RMSE[dt2$RMSE > 100] <- 100
#dt2$PBIAS[dt1$PBIAS > 100] <- 100
#names(dt2)[names(dt2)=="KGE"] <- "KGE (lower bound rescaled to -1)"
#names(dt2)[names(dt2)=="NSE"] <- "NSE (lower bound rescaled to -1 )"
#names(dt2)[names(dt2)=="RMSE"] <- "RMSE (upper bound rescaled to 100)"
#names(dt2)[names(dt2)=="PBIAS"] <- "PBIAS (upper bound rescaled to 100)"

# reshape stats
dt2 <- melt(dt2, id.vars=c("site_no","scenario"),variable.name="metric")

#runs <- c("CFE.default","CFE.camels","CFE.hlr","TOPMODEL.default","TOPMODEL.camels","CFE+TOPMODEL.camels")
#dt2$scenario <- factor(dt2$scenario)
#levels(dt2$scenario) <- gsub("noah_owp_","",levels(dt2$scenario))
#dt2$scenario <- factor(dt2$scenario,levels=runs)

# remove outlier sites (poor performance)
#tmp <- subset(dt1, scenario=="CFE_default")
#basins1 <- subset(tmp, metric=="KGE" & value < -1)$site_no
#dt2 <- subset(dt1, ! site_no %in% basins1)
mycolors <- c("blue","pink","purple","green","yellow","cyan","orange","grey","red")
n1 <- length(unique(dt2$site_no))
gg1 <- ggplot(data=dt2,aes(fill=scenario, y=value, x=scenario)) + 
    geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
    #scale_fill_viridis(discrete=T, name="") +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="", y="", title=paste0("Streamflow results in ", n1," uncalibrated basins in HUC-01")) +
    guides(fill=guide_legend(ncol=2)) +
    theme(axis.text.x=element_blank(),text=element_text(size=14),
        plot.title = element_text(hjust = 0.5), legend.position="top",
        strip.text.x = element_text(size = 14,face="bold"), axis.text.y=element_text(size=12),
        legend.text = element_text(size=12)) +
    facet_wrap(~metric, ncol=1, scales="free")
#ggsave("../figs/stats_retro_violin_3.png",plot=gg1,width=12,height=7,units="in",dpi=300)

#gg2 <- gg1 + geom_jitter(shape=1, color="blue", size=0.5, position=position_jitter(0.2)) 
#ggsave("../figs/stats_retro_violin_jitter_3.png",plot=gg2,width=12,height=7,units="in",dpi=300)

gg2 <- gg1 + geom_boxplot(width=0.1, color="black", alpha=0.2)
ggsave("../figs/stats_retro_violin_boxplot_3.png",plot=gg2,width=7,height=8,units="in",dpi=300)

