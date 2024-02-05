# plot spatial maps of regionalized parameters

rm(list=ls())

library(sf)
library(data.table)
library(viridis)
library(ggplot2)

# hydrofabric version & huc region
ver <- "2.0"; h1 <- "01"
#ver <- "1.2"; h1 <- "17"
#ver <- "2.0pre"; h1 <- "12" 

# read in donor gages
file1 <- "../data/donor_gages_nwmv30.csv"
if (h1=="01") file1 <- "../data/donor_gages_huc01_screened.csv"
donor_gages <- read.csv(file1,header=FALSE,colClasses="character")$V1

# all catchments in the HUC region
gfile <- paste0('../../datasets/gpkg_v',ver,'/nextgen_huc',h1,'.gpkg')
huc <- read_sf(gfile, "divides")
huc1 <- huc %>% st_union() %>% sfheaders::sf_remove_holes()
id_str <- "id"; if (ver %in% c("2.0pre","2.0")) id_str <- "divide_id"

# get potential donor catchments in HUC
col_classes <- c(rep("character",3),"numeric","character")
if (h1=="17") col_classes <- c("character","numeric",rep("character",3))
cwt <- read.csv(paste0("../data/crosswalk_gage_cat_huc",h1,"_v",ver,".csv"), colClasses=col_classes)
gages0 <- unique(cwt$gages)
donor_gages1 <- gages0[gages0 %in% donor_gages]
cwt <- subset(cwt, gages %in% donor_gages1)

# calibrated parameters
dtPars0 <- as.data.table(read.csv(paste0("../data/calibration_results/huc01_v1.2/cfe_noah_parameters.csv"),colClasses=c("character",rep("numeric",11))))
pars <- names(dtPars0)[names(dtPars0)!="site_no"]

# define parameter units
par_unit <- c("b"="[-]","satdk"="[m s-1]","satpsi"="[m]","slope"="[m/m]","maxsmc"="[m/m]","refkdt"="[-]","Klf"="[-]",
    "Kn"="[-]","max_gw_storage"="[m]","expon"="[-]","Cgw"="[m h-1]")

# Read the configuration file
suppressMessages(suppressWarnings(library(yaml)))
config <- read_yaml(paste0("data/config_huc",h1,".yaml"),readLines.warn=FALSE)
for (s1 in names(config$inputs)) {
    config$inputs[[s1]] <- gsub("\\{huc\\}",h1,config$inputs[[s1]])
    config$inputs[[s1]] <- gsub("\\{ver\\}",ver,config$inputs[[s1]])
}

# Read donor-receiver pairs and plot the pairing
scenarios <- names(config$scenarios)[unlist(config$scenarios)]
funcs = c('gower','urf','kmeans','kmedoids','hdbscan','birch')
for (s1 in scenarios) {
    for (f1 in funcs) {
        file1 <- paste0("../output_donor/donor_", s1,"_",f1,"_huc",h1,"_v",ver,".csv")
        dtDonors <- as.data.table(read.csv(file1))
        dtDonors <- dtDonors[,c("id","donor"), with=FALSE]

        # add donor catchments to the donor table
        donor_cats <- huc[[id_str]][!huc[[id_str]] %in% dtDonors$id]
        dtDonors <- rbind(dtDonors,data.table(id=donor_cats, donor=donor_cats))

        # merge huc with donor table, crosswalk, and calibrated parameters
        shps <- merge(huc, dtDonors, by.x=id_str, by.y="id", all.x=TRUE)
        shps <- merge(shps, cwt, by.x="donor",by.y="id",all.x=TRUE)
        shps <- merge(shps, dtPars0, by.x="gages", by.y="site_no",all.x=TRUE)

        # loop through each parameter and plot the map
        for (p1 in "b") {
            
            # parameter map before regionalization
            gg1 <- ggplot() + geom_sf(data = subset(shps,divide_id %in% donor_cats), aes(fill = get(p1),color=get(p1))) + 
                    geom_sf(data = huc1, color="black",fill=NA) +
                    scale_y_continuous(breaks = 34:36) +
                    scale_x_continuous(breaks = 34:36) +
                    labs(color=paste0(p1," ",par_unit[p1]), fill=paste0(p1," ",par_unit[p1])) +
                    scale_fill_viridis(direction = -1) + 
                    scale_color_viridis(direction = -1) + 
                    theme_classic() + 
                    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                        legend.text = element_text(size=24,face="bold"),legend.title = element_text(size=24,face="bold"))
            ggsave(paste0("figs/map_pars/map_par_huc",h1,"_v",ver,"_",p1,"_before_regionalization.png"),gg1,width=7.5,height=8,units="in",dpi=300)            

            # parameter map after regionalization
            gg1 <- ggplot() + geom_sf(data = shps, aes(fill = get(p1),color=get(p1))) + 
                    geom_sf(data = huc1, color="grey",fill=NA) +
                    scale_y_continuous(breaks = 34:36) +
                    scale_x_continuous(breaks = 34:36) +
                    labs(color=paste0(p1," ",par_unit[p1]), fill=paste0(p1," ",par_unit[p1])) +
                    scale_fill_viridis(direction = -1) + 
                    scale_color_viridis(direction = -1) + 
                    theme_classic() + 
                    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                        legend.text = element_text(size=24,face="bold"),legend.title = element_text(size=24,face="bold"))
            ggsave(paste0("figs/map_pars/map_par_huc",h1,"_v",ver,"_",s1,"_",f1,"_",p1,".png"),gg1,width=7.5,height=8,units="in",dpi=300)
        }        
}}


# ================ donors based on spatial proximity ====================
dtSpatialDist <- as.data.table(read.csv(config$inputs$file_distance))
names(dtSpatialDist)[1] <- "receiver"
names(dtSpatialDist) <- gsub("[.]","-",names(dtSpatialDist))
dtSpatialDist <- subset(dtSpatialDist, !receiver %in% names(dtSpatialDist))
dt1 <- data.table()
dt1$id <- dtSpatialDist$receiver
dtSpatialDist$receiver <- NULL
dt1$donor <- names(dtSpatialDist)[(apply(dtSpatialDist,1,which.min))]

# add donor catchments to the donor table
donor_cats <- huc[[id_str]][!huc[[id_str]] %in% dt1$id]
dt1 <- rbind(dt1,data.table(id=donor_cats, donor=donor_cats))

# merge huc with donor table, crosswalk, and calibrated parameters
shps <- merge(huc, dt1, by.x=id_str, by.y="id", all.x=TRUE)
shps <- merge(shps, cwt, by.x="donor",by.y="id",all.x=TRUE)
shps <- merge(shps, dtPars0, by.x="gages", by.y="site_no",all.x=TRUE)

# loop through each parameter and plot the map
for (p1 in pars) {
    gg1 <- ggplot() + geom_sf(data = shps, aes(fill = get(p1),color=get(p1))) + 
            geom_sf(data = huc1, color="grey",fill=NA) +
            scale_y_continuous(breaks = 34:36) +
            scale_x_continuous(breaks = 34:36) +
            labs(color=paste0(p1," ",par_unit[p1]), fill=paste0(p1," ",par_unit[p1])) +
            scale_fill_viridis(direction = -1) + 
            scale_color_viridis(direction = -1) + 
            theme_classic() + 
            theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                legend.text = element_text(size=24,face="bold"),legend.title = element_text(size=24,face="bold"))
    ggsave(paste0("figs/map_pars/map_par_huc",h1,"_v",ver,"_proximity_",p1,".png"),gg1,width=7.5,height=8,units="in",dpi=300)
}        