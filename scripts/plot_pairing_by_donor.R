# plot door-receiver pairing on multi-panel spatial maps

rm(list=ls())

library(sf)
library(tidyr)
library(ggplot2)
library(data.table)

# read HUC-01 geojson into sf
huc01 <- st_read("../shapefile/catchment_data.geojson")

# compute centroid of HUC-01 catchments
sf::sf_use_s2(FALSE)
cent_huc01 <- st_centroid(huc01)

# donor pairing
for (scenario in c("hlr","camels")) {
dtDonorAll <- get(load(paste0("../output/donor_",scenario,".Rdata")))
donors <- unique(dtDonorAll$donor)

# plot the receivers for each donor
cent1 <- subset(cent_huc01,id %in% donors)
cent1$donor <- cent1$id
huc01 <- merge(huc01,dtDonorAll, by="id")
sf1 <- st_union(huc01)
gg1 <- ggplot() +
  #geom_sf(data = huc01,mapping = aes(fill=distAttr),color="grey") +
  geom_sf(data = huc01,color="orange") +
  geom_sf(data = cent1, color="blue", shape=17, size=2) +
  geom_sf(data = sf1,color="black",fill=NA) +
  scale_fill_viridis_c(option = "E") +
  scale_x_continuous(breaks = seq(-74, -67, by = 2)) +
  facet_wrap(~donor,ncol=6)
ggsave(paste0("../figs/pairing_huc01_",scenario,".png"),gg1,width=12,height=8,units="in",dpi=300)
}

# donor difference between HLR and CAMELS scenarios
dt1 <- data.table(id=huc01$id)
for (scenario in c("hlr","camels")) {
  dtDonorAll <- get(load(paste0("../output/donor_",scenario,".Rdata")))
  dt1 <- merge(dt1,dtDonorAll)
}
dt1[,donor.same:=ifelse(donor.x==donor.y,"yes","no")]
dt1 <- subset(dt1,donor.same=="no")

gg1 <- ggplot() +
  geom_sf(data=subset(huc01,id %in% dt1$id),color="blue") +
  geom_sf(data = sf1,color="black",fill=NA) +
  labs(title=paste0("Catchments (",nrow(dt1),")with different donors \nbetween HLR & CAMELS scenarios"))
ggsave("../figs/different_donors_hlr_camels.png",gg1,width = 5,height=5,units="in",dpi=300)
  
# plot model distribution
# read all optimal paramete sets
pars0 <- as.data.table(read.csv("../data/headwater_cat_optimal_params.csv",header = TRUE))
pars <- subset(pars0, period=="valid" & model_type=="CFE+TOPMODEL")
for (scenario in c("hlr","camels")) {
  dtDonorAll <- get(load(paste0("../output/donor_",scenario,".Rdata")))
  donors <- sort(unique(dtDonorAll$donor))
  dtDonorAll <- rbind(dtDonorAll,data.table(id=donors,donor=donors,distAttr=0,distSpatial=0,tag="donor"))
  dtDonorAll[,model:=ifelse(is.na(pars$b[match(donor,pars$catid)]),"Topmodel","CFE")]
  sf0 <- merge(huc01,dtDonorAll,by="id")
  
  gg1 <- ggplot() +
    geom_sf(data=sf0,mapping = aes(fill=model),color=NA) +
    geom_sf(data=sf1,color="black",fill=NA) +
    labs(title=paste0("Model mosaic with ",toupper(scenario)," scenario"))
  ggsave(paste0("../figs/model_distribution_",scenario,".png"),gg1,width = 5,height=5,units="in",dpi=300)  
}
