# plot/compare (violins of) summary stats of different regionalization runs 

rm(list=ls())
library(ggplot2)
library(viridis)
library(data.table)
library(magrittr)

# stats from FIHM runs
# load("../fihm/stat/stat_retro_20091001_20140930.Rdata")
# stats0 <- copy(stats_str)
# stats0 <- subset(stats0,scenario=="noah_owp_TOPMODEL.default")
# stats0$scenario <- "TOPMODEL_def"

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
stat_names <- c("KGE","NSE","nNSE","CORR","RMSE", "PBIAS")
stat_names <- c("NSE","KGE","PBIAS","CORR")
dt1 <- stats[, c("site_no","scenario",stat_names), with=FALSE]

# rescale
dt2 <- copy(dt1)
dt2$KGE[dt2$KGE < -0.5] <- -0.5
dt2$NSE[dt2$NSE < -1] <- -1
#dt2$RMSE[dt2$RMSE > 50] <- 50
dt2$PBIAS <- abs(dt2$PBIAS)
dt2$PBIAS[dt1$PBIAS > 100] <- 100
dt2[,scenario:=gsub("CFE[+]TOPMODEL","mixed",scenario)]
#dt2[,scenario:=gsub("TOPMODEL","TOP",scenario)]
dt2[,scenario:=gsub("kge","KGE",scenario)]
dt2[,scenario:=gsub("nse","NSE",scenario)]

# reshape stats
dt2 <- melt(dt2, id.vars=c("site_no","scenario"),variable.name="metric")
dt2$model <- sapply(dt2$scenario,function(x) strsplit(x,split="_")[[1]][1])
dt2$obj <- sapply(dt2$scenario,function(x) strsplit(x,split="_")[[1]][3])
dt2$reg <- sapply(dt2$scenario,function(x) strsplit(x,split="_")[[1]][2])
dt2[,mod_reg:=paste0(model,"_",reg)]
dt2[,obj_reg:=paste0(obj,"_",reg)]
dt2[,mod_obj:=paste0(model,"_",obj)]
#dt2[,metric:=paste0(metric," (validation)")]
dt2[,metric:=gsub("PBIAS","Absolute percent bias",metric)]
dt2[,metric:=gsub("CORR","Correlation",metric)]
dt2$metric <- factor(dt2$metric,levels=c("KGE","NSE","Correlation","Absolute percent bias"))
dt2$model <- factor(dt2$model,levels=c("CFE","TOPMODEL","mixed"))
dt2$mod_obj <- factor(dt2$mod_obj,levels=c("CFE_KGE","TOPMODEL_KGE","mixed_KGE","CFE_NSE","TOPMODEL_NSE","mixed_NSE"))
dt2$mod_reg <- factor(dt2$mod_reg,levels=c("CFE_hlr","TOPMODEL_hlr","mixed_hlr","CFE_camels","TOPMODEL_camels","mixed_camels"))

# exclude gages with NA in KGE and CORR (due to constant simulated flow; 2 such gages: "01104430" "01115170")
gages1 <- unique(subset(dt2,is.na(value))$site_no)
dt2 <- subset(dt2,!site_no %in% gages1)

# theme for calibration plot
mytheme0 <- theme(text=element_text(size=14), axis.text.x=element_blank(),
        plot.title = element_text(size=16,face="bold",hjust = 0.5), legend.position="top",
        strip.text = element_text(size = 14,face="bold"), axis.text.y=element_text(size=14), 
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size=14))

# theme for validation plots
mytheme <- theme(text=element_text(size=14),
        plot.title = element_text(size=16,face="bold",hjust = 0.5), legend.position="top",
        strip.text = element_text(size = 14,face="bold"), axis.text.y=element_text(size=14),
        axis.text.x = element_text(angle = 20, vjust = 1, hjust=1,size=14), 
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size=14))

# plot calibration by groups of objective function
data1 <- subset(dt2,site_no %in% gages_calib)
data1$scenario <- gsub("_calib","",data1$scenario)
data2 <- data1[,.(value=max(value)),by=.(site_no,metric,obj)]
data2[,scenario:=paste0("mixed_",obj)]
data1 <- rbind(data1[,names(data2),with=F],data2)
data1$scenario <- factor(data1$scenario,levels=c("CFE_KGE","TOPMODEL_KGE","mixed_KGE","CFE_NSE","TOPMODEL_NSE","mixed_NSE"))
ns1 <- length(unique(data1[["scenario"]]))
mycolors <- c("blue","pink","purple","yellow","cyan","orange","grey")
mycolors <- rep(mycolors,3)[1:ns1]
n1 <- length(unique(dt2$site_no))
gg1 <- ggplot(data=data1,aes(y=value, x=scenario,fill=scenario)) + 
    #geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
    geom_boxplot(width=0.4, color="black", alpha=0.3) +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="Model and calibration objective function",y="", title=paste0("Streamflow results at calibration basins")) +
    #guides(fill=guide_legend(nrow=2,byrow=TRUE)) + 
    mytheme + theme(legend.position = "none") +
    facet_wrap(~metric, ncol=2, scales="free_y")
gg1 <- gg1 + geom_jitter(shape=1, color="blue", size=2, position=position_jitter(0.2))
ggsave(paste0("figs/stats_calib_boxplot.png"),plot=gg1,width=10,height=7,units="in",dpi=300)

# plot validation by groups of objective function
group1 <- "obj"
group_text <- "calibration objective function"
ns1 <- length(unique(dt2[[group1]]))
mycolors <- c("blue","pink","purple","yellow","cyan","orange","grey")
mycolors <- rep(mycolors,3)[1:ns1]
n1 <- length(unique(dt2$site_no))
gg1 <- ggplot(data=subset(dt2,!site_no %in% gages_calib),aes(y=value, x=mod_reg,fill=get(group1))) + 
    geom_boxplot(width=0.4, color="black", alpha=0.5) +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="Model and regionalization scenarios",y="", title=paste0("Streamflow results at validation basins in HUC-01 by ",group_text)) +
    guides(fill=guide_legend(ncol=ns1)) + mytheme +
    facet_wrap(~metric, ncol=2, scales="free")
ggsave(paste0("figs/stats_valid_boxplot_group_",group1,".png"),plot=gg1,width=12.5,height=7,units="in",dpi=300)

# plot validation by groups of model choice
group1 <- "model"
group_text <- "model choice"
ns1 <- length(unique(dt2[[group1]]))
mycolors <- c("blue","pink","purple","yellow","cyan","orange","grey")
mycolors <- rep(mycolors,3)[1:ns1]
n1 <- length(unique(dt2$site_no))
gg1 <- ggplot(data=subset(dt2,!site_no %in% gages_calib),aes(y=value, x=obj_reg,fill=get(group1))) + 
    geom_boxplot(width=0.4, color="black", alpha=0.5) +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="Calibration objective function and regionalization scenarios",y="", title=paste0("Streamflow results at validation basins in HUC-01 by ",group_text)) +
    guides(fill=guide_legend(ncol=ns1)) + mytheme +
    facet_wrap(~metric, ncol=2, scales="free")
ggsave(paste0("figs/stats_valid_boxplot_group_",group1,".png"),plot=gg1,width=12.5,height=7,units="in",dpi=300)

# plot validation by groups of regionalization approach
group1 <- "reg"
group_text <- "regionalization approach"
ns1 <- length(unique(dt2[[group1]]))
mycolors <- c("blue","pink","purple","yellow","cyan","orange","grey")
mycolors <- rep(mycolors,3)[1:ns1]
n1 <- length(unique(dt2$site_no))
gg1 <- ggplot(data=subset(dt2,!site_no %in% gages_calib),aes(y=value, x=mod_obj,fill=get(group1))) + 
    geom_boxplot(width=0.4, color="black", alpha=0.5) +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="Model and calibration objective function",y="", title=paste0("Streamflow results at validation basins in HUC-01 by ",group_text)) +
    guides(fill=guide_legend(ncol=ns1)) + mytheme +
    facet_wrap(~metric, ncol=2, scales="free_y")
ggsave(paste0("figs/stats_valid_boxplot_group_",group1,".png"),plot=gg1,width=12.5,height=7,units="in",dpi=300)

# scatter plot grouped by obj
dt3 <- subset(dt2,!site_no %in% gages_calib)
dt4 <- dcast(dt3,site_no + mod_reg + metric ~ obj, value.var="value")
dt4 <- subset(dt4,metric %in% c("KGE","NSE"))
ns1 <- length(unique(dt4$mod_reg))
mycolors <- c("blue","red","bisque4","orange","cyan","darkgreen")
gg1 <- ggplot(data=dt4,aes(x=KGE,y=NSE,color=mod_reg)) +
    geom_point(shape=1,size=2,stroke=0.8) +
    geom_abline(slope=1,intercept = 0,linetype="dashed") +
    geom_vline(xintercept = 0,linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
    scale_color_manual(name="model & regionalization scenario",values=mycolors) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    labs(x="Calibration objective function = KGE", y="Calib. objective func = NSE",
         title="Streamflow validation statistics") +
    mytheme +
    facet_wrap(~metric, scales="free",ncol=2)
ggsave("figs/stats_valid_scatter_group_obj.png",plot=gg1,width=10,height=5,units="in",dpi=300)

# scatter plot grouped by regionalization
dt3 <- subset(dt2,!site_no %in% gages_calib)
dt4 <- dcast(dt3,site_no + mod_obj + metric ~ reg, value.var="value")
dt4 <- subset(dt4,metric %in% c("KGE","NSE"))
ns1 <- length(unique(dt4$mod_obj))
mycolors <- c("blue","red","bisque4","orange","cyan","darkgreen")
gg1 <- ggplot(data=dt4,aes(x=hlr,y=camels,color=mod_obj)) +
  geom_point(shape=1,size=2,stroke=0.8) +
  geom_abline(slope=1,intercept = 0,linetype="dashed") +
  geom_vline(xintercept = 0,linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  scale_color_manual(name="model & obj_func scenario",values=mycolors) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  labs(x="Regionalization (HLR)", y="Regionalization (CAMELS)",
       title="Streamflow validation statistics") +
  mytheme +
  facet_wrap(~metric, scales="free",ncol=2)
ggsave("figs/stats_valid_scatter_group_reg.png",plot=gg1,width=10,height=5,units="in",dpi=300)

# scatter plot grouped by model selection
dt3 <- subset(dt2,!site_no %in% gages_calib)
dt4 <- dcast(dt3,site_no + obj_reg + metric ~ model, value.var="value")
dt4 <- subset(dt4,metric %in% c("KGE","NSE"))
dt4 <- melt(dt4,id.vars=c("site_no","obj_reg","metric","mixed"),measure.vars = c("CFE","TOPMODEL"))
dt4$variable <- factor(dt4$variable)
dt4$metric <- factor(dt4$metric)
ns1 <- length(unique(dt4$obj_reg))
mycolors <- c("blue","red","bisque4","darkgreen")
gg1 <- ggplot(data=dt4,aes(x=mixed,y=value,color=obj_reg)) +
  geom_point(shape=1, size=2,stroke=0.8) +
  geom_abline(slope=1,intercept = 0,linetype="dashed") +
  geom_vline(xintercept = 0,linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  scale_color_manual(name="obj_func & regionalization scenario",values=mycolors) +
  guides(fill=guide_legend(nrow=1)) +
  labs(x="Mixed model (heterogeneous)", y="",
       title="Streamflow validation statistics") +
  mytheme +
  facet_grid(variable ~ metric, space = "free", switch = "y")
ggsave("figs/stats_valid_scatter_group_model.png",plot=gg1,width=10,height=7.5,units="in",dpi=300)
    