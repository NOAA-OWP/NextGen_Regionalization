# plot all donor catchments for a given HUC and hydrofabric version

rm(list=ls())

library(sf)
library(data.table)
library(dplyr)

# hydrofabric version & huc region
ver <- "2.0"; h1 <- "01"
ver <- "1.2"; h1 <- "17"
ver <- "2.0pre"; h1 <- "12" 

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

# retrieve donor catchments & merge with cwt
shps_don <- huc %>% filter(get(id_str) %in% cwt$id) %>% merge(cwt,by.x=id_str, by.y="id",all.x=TRUE)

# combine donor catchments by gage and compute centroid of donor gages 
shps_don <- shps_don %>% group_by(gages) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
cent_don <- st_centroid(shps_don)

# define colors and symbols to be used for each donor gage
colors <- c("gold","darkgreen","cadetblue","bisque4","aquamarine4","brown1","blueviolet","blue2",
            "coral4","cornflowerblue","darkcyan","pink","darkgoldenrod4","darkmagenta","darkorange2","darkolivegreen3")
colors <- rep(colors,ceiling(length(donor_gages)/length(colors)))
symbols <- c(21,22,25)
symbols <- rep(symbols,ceiling(length(donor_gages)/length(symbols)))

# plot receiver catchments, donor catchments and gages
png(paste0("figs/map_pairing/map_donors_huc",h1,"_v",ver,".png"),width=8, height=7,units="in", res=300)
plot(st_geometry(huc),border="grey",lwd=0.5,main=paste0("HUC",h1," donor catchments"))
plot(st_geometry(huc1),border="black",lwd=1,add=T)
for (g1 in donor_gages1) {
    ix1 <- match(g1,donor_gages1)
    plot(st_geometry(subset(shps_don, gages == g1)), border=NA,col=colors[ix1], lwd=0.5, add=T)
    plot(st_geometry(subset(cent_don, gages == g1)),pch=symbols[ix1],col="black",bg=colors[ix1],cex=1.5,add=T)
}
dev.off()

# Read the configuration file
suppressMessages(suppressWarnings(library(yaml)))
config <- read_yaml(paste0("data/config_huc",h1,".yaml"),readLines.warn=FALSE)
for (s1 in names(config$inputs)) {
    config$inputs[[s1]] <- gsub("\\{huc\\}",h1,config$inputs[[s1]])
    config$inputs[[s1]] <- gsub("\\{ver\\}",ver,config$inputs[[s1]])
}

# Read donor-receiver pairs and plot the pairing
# scenarios <- names(config$scenarios)[unlist(config$scenarios)]
# funcs = c('gower','urf','kmeans','kmedoids','hdbscan','birch')
# for (s1 in scenarios) {
#     for (f1 in funcs) {
#         file1 <- paste0("../output_donor/donor_", s1,"_",f1,"_huc",h1,"_v",ver,".csv")
#         dtDonors <- as.data.table(read.csv(file1))
#         dtDonors <- dtDonors[,c("id","donor","distSpatial"), with=FALSE]

#         # merge huc with donor table and crosswalk
#         shps_rec <- merge(huc, dtDonors, by.x=id_str, by.y="id", all.x=TRUE)
#         shps_rec <- merge(shps_rec, cwt, by.x="donor",by.y="id",all.x=TRUE)

#         # plot receiver & donor catchments
#         png(paste0("figs/map_pairing/map_donor_pairing_huc",h1,"_v",ver,"_",s1,"_",f1,".png"),width=8, height=7,units="in", res=300)
#         plot(st_geometry(huc),border="grey",lwd=0.5,main=paste0("HUC",h1," donor catchments"))
#         plot(st_geometry(huc1),border="black",lwd=1,add=T)
#         for (g1 in donor_gages1) {
#             ix1 <- match(g1,donor_gages1)
#             plot(st_geometry(subset(shps_don, gages == g1)), border=NA,col=colors[ix1], lwd=0.5, add=T)
#             plot(st_geometry(subset(shps_rec, gages == g1)), border=NA,col=colors[ix1], lwd=0.5, add=T)
#             plot(st_geometry(subset(cent_don, gages == g1)),pch=symbols[ix1],col="black",bg=colors[ix1],cex=1.5,add=T)
#         }
#         # plot donor gages
#         for (g1 in donor_gages1) {
#             ix1 <- match(g1,donor_gages1)
#             plot(st_geometry(subset(cent_don, gages == g1)),pch=symbols[ix1],col="black",bg=colors[ix1],cex=1.5,add=T)
#         }        
#         dev.off()
# }}

# ================ donors based on spatial proximity ====================
dtSpatialDist <- as.data.table(read.csv(config$inputs$file_distance))
names(dtSpatialDist)[1] <- "receiver"
names(dtSpatialDist) <- gsub("[.]","-",names(dtSpatialDist))
dtSpatialDist <- subset(dtSpatialDist, !receiver %in% names(dtSpatialDist))
dt1 <- data.table()
dt1$id <- dtSpatialDist$receiver
dtSpatialDist$receiver <- NULL
dt1$donor <- names(dtSpatialDist)[(apply(dtSpatialDist,1,which.min))]
dt1$distSpatial <- apply(dtSpatialDist,1,min)

# merge huc with donor table and crosswalk
shps_rec <- merge(huc, dt1, by.x=id_str, by.y="id", all.x=TRUE)
shps_rec <- merge(shps_rec, cwt, by.x="donor",by.y="id",all.x=TRUE)

# plot receiver & donor catchments
png(paste0("figs/map_pairing/map_donor_pairing_huc",h1,"_v",ver,"_proximity.png"),width=8, height=7,units="in", res=300)
plot(st_geometry(huc),border="grey",lwd=0.5,main=paste0("HUC",h1," donor catchments"))
plot(st_geometry(huc1),border="black",lwd=1,add=T)
for (g1 in donor_gages1) {
    ix1 <- match(g1,donor_gages1)
    plot(st_geometry(subset(shps_don, gages == g1)), border=NA,col=colors[ix1], lwd=0.5, add=T)
    plot(st_geometry(subset(shps_rec, gages == g1)), border=NA,col=colors[ix1], lwd=0.5, add=T)
    plot(st_geometry(subset(cent_don, gages == g1)),pch=symbols[ix1],col="black",bg=colors[ix1],cex=1.5,add=T)
}
# plot donor gages
for (g1 in donor_gages1) {
    ix1 <- match(g1,donor_gages1)
    plot(st_geometry(subset(cent_don, gages == g1)),pch=symbols[ix1],col="black",bg=colors[ix1],cex=1.5,add=T)
}        
dev.off()

#============== random donor scenario ====================
dt1$donor <- sample(names(dtSpatialDist),nrow(dt1),replace=TRUE)
col.sel <- match(dt1$donor,names(dtSpatialDist))
dt1$distSpatial <- as.data.frame(dtSpatialDist)[cbind(seq_along(col.sel),col.sel)]

# merge huc with donor table and crosswalk
shps_rec <- merge(huc, dt1, by.x=id_str, by.y="id", all.x=TRUE)
shps_rec <- merge(shps_rec, cwt, by.x="donor",by.y="id",all.x=TRUE)

# plot receiver & donor catchments
png(paste0("figs/map_pairing/map_donor_pairing_huc",h1,"_v",ver,"_random.png"),width=8, height=7,units="in", res=300)
plot(st_geometry(huc),border="grey",lwd=0.5,main=paste0("HUC",h1," donor catchments"))
plot(st_geometry(huc1),border="black",lwd=1,add=T)
for (g1 in donor_gages1) {
    ix1 <- match(g1,donor_gages1)
    plot(st_geometry(subset(shps_don, gages == g1)), border=NA,col=colors[ix1], lwd=0.5, add=T)
    plot(st_geometry(subset(shps_rec, gages == g1)), border=NA,col=colors[ix1], lwd=0.5, add=T)
    plot(st_geometry(subset(cent_don, gages == g1)),pch=symbols[ix1],col="black",bg=colors[ix1],cex=1.5,add=T)
}
# plot donor gages
for (g1 in donor_gages1) {
    ix1 <- match(g1,donor_gages1)
    plot(st_geometry(subset(cent_don, gages == g1)),pch=symbols[ix1],col="black",bg=colors[ix1],cex=1.5,add=T)
}        
dev.off()