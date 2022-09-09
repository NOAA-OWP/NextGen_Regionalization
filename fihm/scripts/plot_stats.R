rm(list=ls())
library(ggplot2)
library(viridis)

# load stats 
load("../stat/stat_retro_20091001_20140930.Rdata")

# QC stats
stats <- subset(stats_str, QMEAN>0 & t_n>1000)
stats[,nNSE:=1/(2-NSE)]

# reshape stats
stat_names <- c("KGE","NSE","nNSE","CORR","RMSE", "PBIAS")
dt1 <- stats[, c("site_no","scenario",stat_names), with=FALSE]
dt1 <- melt(dt1, id.vars=c("site_no","scenario"),variable.name="metric")

# remove outlier sites (poor performance)
tmp <- subset(dt1, scenario=="noah_owp_TOPMODEL")
basins1 <- subset(tmp, metric=="KGE" & value < -0.5)$site_no
dt2 <- subset(dt1, ! site_no %in% basins1)

n1 <- length(unique(dt2$site_no))
gg1 <- ggplot(data=dt2,aes(fill=scenario, y=value, x=scenario)) + 
    geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
    scale_fill_viridis(discrete=T, name="") +
    labs(x="", y="", title=paste0("Streamflow stats distribution over HUC01 (",n1," gages)")) +
    theme(axis.text.x=element_blank(),text=element_text(size=14),
        plot.title = element_text(hjust = 0.5), legend.position="top") +
    facet_wrap(~metric, ncol=3, scales="free")
ggsave("../figs/stats_retro_violin.png",plot=gg1,width=10,height=7,units="in",dpi=300)

gg2 <- gg1 + geom_jitter(shape=1, color="blue", size=0.5, position=position_jitter(0.2)) 
ggsave("../figs/stats_retro_violin_jitter.png",plot=gg2,width=10,height=7,units="in",dpi=300)

gg2 <- gg1 + geom_boxplot(width=0.1, color="blue", alpha=0.2)
ggsave("../figs/stats_retro_violin_boxplot.png",plot=gg2,width=10,height=7,units="in",dpi=300)

