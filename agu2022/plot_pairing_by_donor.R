
# plot each donor and all of its receiver catchments using the same color on a spatial map

rm(list=ls())

library(sf)
library(tidyr)
library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)
sf::sf_use_s2(FALSE)

screening <- "kge_nse"
models <- c("uniform","mosaic")

colors <- c("gold","darkgreen","cadetblue","bisque4","aquamarine4","brown1","blueviolet","blue2",
            "coral4","cornflowerblue","darkcyan","pink","darkgoldenrod4","darkmagenta","darkorange2","darkolivegreen3")
symbols <- rep(c(21,22,25),10)

# hydrofabric versions of receivers and donors
ver_receiver <- "v0"
ver_donor <- "v1.2"

shps_rec <- st_read(paste0("../datasets/gpkg_",ver_receiver,"/catchment_data.geojson"))
shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
cent_rec <- st_centroid(shps_rec)
cent_don <- st_centroid(shps_don)

# boundary of huc01
sf1 <- st_union(shps_rec)

# calibration gages screened for performance
gages_calib <- get(load(paste0("output/",ver_receiver,"/calib_gages_screened_",screening,".Rdata")))

# upstream catchments of gages
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))
names(cwt)[names(cwt)=="id"] <- "donor"
cwt1 <- subset(cwt, gages %in% gages_calib)

#====  handle nested gages =====
# catchments in nested gages
cats0 <- cwt1 %>% select("donor") %>% duplicated() %>% filter(cwt1,.) %>% select("donor")
cats0 <- cats0$donor
if (length(cats0)>0) {
# nested gages
tmp <- cwt1 %>% filter(donor %in% cats0) %>% select("gages")
gages1 <- unique(tmp$gages)
# choose inner gage
tmp <- cwt1 %>% filter(gages %in% gages1) %>% count(gages) %>% slice_min(n)
gage1 <- tmp$gages
# remove lines that are not inner gages
ix1 <- which((cwt1$donor %in% cats0) & (cwt1$gages!=gage1))
cwt1 <- cwt1[-ix1,]
}

# merge donor catchments with cwt
shps_don1 <- shps_don %>% 
  filter(id %in% cwt1$donor) %>% 
  select(c("id","geometry")) %>% 
  setnames(c("donor","geometry")) %>% 
  merge(cwt1,by="donor",all.x=TRUE)

# merge donor catchments by gage
shps_don1 <- shps_don1 %>% group_by(gages) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()

# compute centroid of donor gages
cent_don <- st_centroid(shps_don1)

# plot calibration gages and uncalibrated catchments
gg1 <- ggplot() +
  geom_sf(data = shps_rec,color="grey",fill="white") +
  geom_sf(data = cent_don,aes(fill=gages,shape=gages),color="black",size=5,stroke=1.5) +
  geom_sf(data = sf1,color="grey80",fill=NA,stroke=1.5) +
  scale_fill_manual(name="Calibration Basins",values=colors) +
  scale_shape_manual(name="Calibration Basins",values=symbols) +
  ggtitle(paste0("Donors and uncalibrated catchments")) +
  theme_void() +
  theme(text=element_text(size=14), 
        plot.title = element_text(size=16,face="bold",hjust = 0.5),
        legend.text = element_text(size=14))

ggsave("figs/donors_uncalibrated_catchments_huc01.png",gg1,width=7.5,height=8,units="in",dpi=300)


# plot donor pairing
for (scenario in c("hlr","camels")) {
  
dtDonorAll <- get(load(paste0("output/",ver_receiver,"/donor_",scenario,"_",screening,".Rdata")))
dtDonorAll <- dtDonorAll %>% select(c("id","donor")) %>% merge(cwt1,by="donor",all.x=TRUE)

# add donor pairing to receiver sf
shp_rec1 <- merge(shps_rec,dtDonorAll, by="id")
cent_don1 <- merge(cent_don, dtDonorAll,by="gages")

symbols1 <- rep(symbols,100)[1:length(gages_calib)]
colors1 <- rep(colors,10)[1:length(gages_calib)]

gg1 <- ggplot() +
  geom_sf(data = sf1,color="black",fill=NA) +
  geom_sf(data = shp_rec1,aes(fill=gages),color=NA) +
  geom_sf(data = cent_don1,aes(fill=gages,shape=gages),color="black",size=4,stroke=1.5) +
  scale_fill_manual(name="Calibration Basin",values=colors1) +
  scale_shape_manual(name="Calibration Basin",values=symbols1) +
  ggtitle(paste0("Donor selection from ",toupper(scenario),"-based regionalization")) +
  theme_void() +
  theme(text=element_text(size=14), 
        plot.title = element_text(size=16,face="bold",hjust = 0.5),
        legend.text = element_text(size=14))
  
ggsave(paste0("figs/pairing_huc01_",scenario,"_",screening,".png"),gg1,width=7.5,height=8,units="in",dpi=300)

}

# donor difference between HLR and CAMELS scenarios
dt1 <- data.table(id=shps_rec$id)
for (scenario in c("hlr","camels")) {
  dtDonorAll <- get(load(paste0("output/",ver_receiver,"/donor_",scenario,"_",screening,".Rdata")))
  dt1 <- merge(dt1,dtDonorAll,by="id",all.x=TRUE)
}
dt1[,gage.x:=cwt1$gages[match(donor.x,cwt1$donor)]]
dt1[,gage.y:=cwt1$gages[match(donor.y,cwt1$donor)]]
dt1[,gage.diff:=ifelse(gage.x!=gage.y,"yes","no")]

shp_rec1 <- merge(shps_rec,dt1[,c("id","gage.diff"),with=F])
names(shp_rec1)[names(shp_rec1)=="gage.diff"] <- "donor different?"

gg1 <- ggplot() +
  geom_sf(data = shp_rec1,aes(fill="donor different?"),color=NA) +
  geom_sf(data = sf1,color="black",fill=NA) +
  ggtitle(paste0("Donor choice difference between HLR-based and CAMELS-based regionalization")) +
  theme_void()
ggsave(paste0("figs/different_donors_hlr_camels_",screening,".png"),gg1,width =7.5,height=8,units="in",dpi=300)

