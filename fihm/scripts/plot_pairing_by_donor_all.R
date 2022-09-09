# For each donor, plot its receivers on multi-panel spatial maps

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

# huc-01 boundary
sf1 <- st_union(huc01)

# colors
colors1 <- c("grey","deeppink","blue","red","green","orange","pink","lightblue","brown",
   "purple","cyan","darkblue","darkred","aquamarine4","darkgoldenrod1","darkolivegreen")

# donor pairing
for (scenario in c("camels")) {
dtDonorAll <- get(load(paste0("../data/donor_",scenario,".Rdata")))
donors <- as.data.table(table(dtDonorAll$donor))
names(donors) <- c("donor","nrec")
donors <- setorder(donors,-nrec)$donor

# plot receivers (colored by donor)
cent_huc01 <- merge(cent_huc01, dtDonorAll,by="id")
cent_huc01$donor <- factor(cent_huc01$donor, levels=donors)
gg1 <- ggplot() + 
       geom_sf(data = sf1,color="black",fill=NA) +
       geom_sf(data = cent_huc01, aes(color=donor), shape=1, size=2) +
       scale_color_manual(name="Donors (16)",values=colors1) +
       scale_x_continuous(breaks = seq(-74, -67, by = 2)) + theme_classic() +
       theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          strip.text.x = element_text(size = 14),
          #legend.key = element_rect(fill = "lightgrey"),
          legend.text = element_text(size=12))

ggsave(paste0("../figs/pairing_huc01_",scenario,"_all.png"),gg1,width=7,height=7,units="in",dpi=300)
}

