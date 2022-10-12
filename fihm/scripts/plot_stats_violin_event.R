# plot/compare (violins of) summary stats of different regionalization runs 

rm(list=ls())
library(ggplot2)
library(viridis)
library(data.table)
library(dplyr)

# load stats 
load("../stat/stat_retro_20091001_20140930_events.Rdata")

# take absolute values of peak/volume biases
cols <- c("peak_bias","volume_bias")
dtEventStats[ , (cols) := lapply(.SD, "abs"), .SDcols = cols]

# compute median peak/volume biases
dt1 <- dtEventStats %>% group_by(gage, scenario) %>% 
   summarise_at(vars("peak_bias","volume_bias"), quantile, prob=0.5, na.rm=T)

# reshape stats
dt2 <- melt(as.data.table(dt1), id.vars=c("gage","scenario"),variable.name="metric")

# rescale
dt2$value[dt2$value>100] <- 100

mycolors <- c("blue","pink","purple","green","yellow","cyan","orange","grey","red")
n1 <- length(unique(dt2$gage))
labels <- as_labeller(c(`peak_bias`="Median peak bias (event_threshold=90%)", `volume_bias`="Median volume bias (event_threshold=90%)"))
gg1 <- ggplot(data=dt2,aes(fill=scenario, y=value, x=scenario)) + 
    geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
    #scale_fill_viridis(discrete=T, name="") +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="", y="", title=paste0("Streamflow peak/volume biases in ", n1," uncalibrated HUC-01 basins")) +
    guides(fill=guide_legend(ncol=2)) +
    theme(axis.text.x=element_blank(),text=element_text(size=14),
        plot.title = element_text(hjust = 0.5), legend.position="top",
        strip.text.x = element_text(size = 14,face="bold"), axis.text.y=element_text(size=12),
        legend.text = element_text(size=12)) +
    facet_wrap(~metric, ncol=1, scales="free",labeller=labels)

gg2 <- gg1 + geom_boxplot(width=0.1, color="black", alpha=0.2)
ggsave("../figs/stats_retro_violin_boxplot_event.png",plot=gg2,width=8.5,height=8,units="in",dpi=300)

