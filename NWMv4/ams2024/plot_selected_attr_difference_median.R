# plot the median difference in attributes between receivers and their donors for selected attributes

rm(list=ls())

library(data.table)
library(sf)
library(ggplot2)
suppressMessages(suppressWarnings(library(yaml)))

# define attr units
attr_units = c("slope"="[radian]", "elev_mean"="[m]", "p_mean"="[mm/yr]", "pet_mean"="[mm/yr]", "aridity"="[-]", 
        "snow_frac"="[%]", "high_prec_freq"="[days/yr]","low_prec_freq"="[days/yr]", "high_prec_dur"="[days/yr]", "low_prec_dur"="[days/yr]", "forest_frac"="[%]", "gvf_max"="[-]", "gvf_diff"="[-]","lai_max"="[-]", "lai_diff"="[-]", 
        "geo_porosity"="[-]", "geo_permeability"="[log(m2)]", "sand_frac"="[%]", "clay_frac"="[%]",
        "soil_porosity"="[-]", "soil_conductivity"="[cm/h]", "soil_depth"="[m]")

# attributes to plot
attrs1 <- c("sand_frac","clay_frac","forest_frac","soil_conductivity","geo_porosity","aridity","snow_frac","elev_mean","slope")

# order of algorithms (in barchart) from high to low performance
algorithms <- c("urf","gower","kmeans","kmedoids","birch","hdbscan","proximity","random")

# scenario to plot
scenario1 <- "camels"

dtAll <- data.table()
hucs <- c("01","12","17")
vers <- c("2.0","2.0pre","1.2")
for (huc in hucs) {

message("huc",huc)
ver <- vers[match(huc,hucs)]

# Read the configuration file
config <- read_yaml(paste0("data/config_huc",huc,".yaml"),readLines.warn=FALSE)
for (s1 in names(config$inputs)) {
    config$inputs[[s1]] <- gsub("\\{huc\\}",huc,config$inputs[[s1]])
    config$inputs[[s1]] <- gsub("\\{ver\\}",ver,config$inputs[[s1]])
}

# Read attribute table for all donors and receivers
dtAttrAll <- as.data.table(read.csv(config$inputs$file_attrs_data))

# filter to those to be used in barchart
dtAttrAll <- dtAttrAll[,c("id", attrs1), with=FALSE]

# Read donor-receiver pairs
scenarios <- names(config$scenarios)[unlist(config$scenarios)]
funcs = c('gower','urf','kmeans','kmedoids','hdbscan','birch')

dtDonorsAll <- data.table()
for (s1 in scenarios) {
    for (f1 in funcs) {
        file1 <- paste0("../output_donor/donor_", s1,"_",f1,"_huc",huc,"_v",ver,".csv")
        dtDonors <- as.data.table(read.csv(file1))
        dtDonors <- dtDonors[,c("id","donor","distSpatial"), with=FALSE]
        dtDonors$scenario <- s1
        dtDonors$algorithm <- f1 
        dtDonors$huc <- huc       
        dtDonorsAll <- rbind(dtDonorsAll, dtDonors)
    }
}

# Add the spatial proximity scenario
dtSpatialDist <- as.data.table(read.csv(config$inputs$file_distance))
names(dtSpatialDist)[1] <- "receiver"
names(dtSpatialDist) <- gsub("[.]","-",names(dtSpatialDist))
dtSpatialDist <- subset(dtSpatialDist, !receiver %in% names(dtSpatialDist))
dt1 <- data.table()
dt1$id <- dtSpatialDist$receiver
dtSpatialDist$receiver <- NULL
dt1$donor <- names(dtSpatialDist)[(apply(dtSpatialDist,1,which.min))]
dt1$distSpatial <- apply(dtSpatialDist,1,min)
dt1$scenario <- "proximity"
dt1$algorithm <- "proximity"
dt1$huc <- huc
dtDonorsAll <- rbind(dtDonorsAll, dt1)

# Add the random donor scenario
dt1$donor <- sample(names(dtSpatialDist),nrow(dt1),replace=TRUE)
col.sel <- match(dt1$donor,names(dtSpatialDist))
dt1$distSpatial <- as.data.frame(dtSpatialDist)[cbind(seq_along(col.sel),col.sel)]
dt1$scenario <- "random"
dt1$algorithm <- "random"
dt1$huc <- huc
dtDonorsAll <- rbind(dtDonorsAll, dt1)

# Merge with attributes table for receivers
dtDonorsAll <- merge(dtDonorsAll, dtAttrAll, by="id",all.x=TRUE)

# Add donor attributes
dtDonorsAll <- merge(dtDonorsAll, dtAttrAll, by.x="donor",by.y='id',all.x=TRUE,suffixes=c(".r",".d"))

# compute donor-receiver attribute differences
for (attr1 in attrs1) {
    if (sum(grepl(attr1,names(dtDonorsAll)))<2) {
        message("Warning: ", attr1, " is not found in the attribute table for both donors and receivers")
    } else {
        dtDonorsAll[,eval(paste0(attr1,".dif")):=abs(get(paste0(attr1,".r")) - get(paste0(attr1,".d")))]
        dtDonorsAll[,eval(paste0(attr1,".prc.dif")):=round(abs(get(paste0(attr1,".r")) - get(paste0(attr1,".d")))/abs(get(paste0(attr1,".r")))*100)]
    }
}

# compute median percent attribute difference
dtData <- dtDonorsAll[,c("scenario","algorithm","huc",names(dtDonorsAll)[grepl("prc.dif",names(dtDonorsAll))]),with=FALSE]
dtData <- subset(dtData, scenario %in% c(scenario1,"proximity","random"))
dtData$scenario <- NULL
dtData <- melt(dtData,id.vars=c("algorithm","huc"))
dtData1 <- dtData[,.(value=quantile(value,probs=0.5,na.rm=T)),by=.(algorithm,huc,variable)]
dtData1$variable <- gsub("[.]prc[.]dif","",dtData1$variable)
dtData1$algorithm <- factor(dtData1$algorithm,levels=algorithms)

# scale the data
dtData1$value[dtData1$value>55] <- 55

# combine data for different huc regions
dtAll <- rbind(dtAll, dtData1)
}

# plot median percent attribute difference
mytheme <- theme(text=element_text(size=14),
        plot.title = element_text(size=16,face="bold",hjust = 0.5), legend.position="top",
        strip.text = element_text(size = 14,face="bold"), axis.text.y=element_text(size=14),
        axis.text.x = element_text(angle = 20, vjust = 1, hjust=1,size=14),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size=14))
mycolors <- c("blue","pink","purple","yellow","cyan","orange","tomato","grey")

dtAll[,huc:=ifelse(huc=="01","New England",ifelse(huc=="12","Texas-Gulf","Pacific Northwest"))]

gg1 <- ggplot(dtAll,aes(y=value, x=factor(huc),fill=algorithm)) +
    geom_bar(position = position_dodge(width = 0.8),stat = 'identity',color="black",alpha=0.7) +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="",y="Median absolute percent difference in attributes", title="") +
    guides(fill=guide_legend(ncol=8)) + mytheme +
    facet_wrap(~variable, ncol=3, scales="fixed")
ggsave(paste0("figs/attr_diff/median_prc_diff_",scenario1,"_selected.png"),plot=gg1,width=12.5,height=8,units="in",dpi=300)

# plot median percent attribute difference without the random scenario
dtAll1 <- subset(dtAll, algorithm != "random")
dtAll1$value[dtAll1$value > 40] <- 40
gg1 <- ggplot(dtAll1,aes(y=value, x=factor(huc),fill=algorithm)) +
    geom_bar(position = position_dodge(width = 0.8),stat = 'identity',color="black",alpha=0.7) +
    scale_fill_manual(name="",values=mycolors) +
    labs(x="",y="Median absolute percent difference in attributes", title="") +
    guides(fill=guide_legend(ncol=8)) + mytheme +
    facet_wrap(~variable, ncol=3, scales="fixed")
ggsave(paste0("figs/attr_diff/median_prc_diff_",scenario1,"_selected_no_random.png"),plot=gg1,width=12.5,height=8,units="in",dpi=300)