# check and plot the spatial and attribute distances between donors and receivers
rm(list=ls())

library(data.table)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)

# shapefile of HUC-01 catchments
huc01 <- st_read("../shapefile/catchment_data.geojson")

# collection of all attributes (for donors and receivers)
dtAttrAll <- get(load("../output/all_attrs.Rdata"))

# list of attributes used for different scenarios 
attrs <- vector("list")
attrs[["base"]] <- c("elev_mean","aridity","forest_frac","sand_frac","geo_porosity")
attrs[["hlr"]] <- c("prcFlatLowland","prcFlatUpland","prcFlatTotal","relief","aridity","sand_frac","clay_frac",
                    "soil_depth","forest_frac","cropland_frac","urban_frac")
attrs[["camels"]] <- c("slope","elev_mean","p_mean","pet_mean","aridity","snow_frac","high_prec_freq",
                       "low_prec_freq","high_prec_dur","low_prec_dur","forest_frac","gvf_max","gvf_diff",
                       "lai_max","lai_diff","geo_porosity","geo_permeability","sand_frac","clay_frac",
                       "soil_porosity","soil_conductivity","soil_depth")

for (scenario in c("hlr","camels")) {
  
  # donor pairing
  dtDonorAll <- get(load(paste0("../output/donor_",scenario,".Rdata")))
  
  # merge attributes and donors
  sf0 <- merge(huc01,dtAttrAll,by="id",all.x=T)
  sf0 <- merge(sf0,dtDonorAll,by="id",all.x=T)
  
  # plot attribute and spatial distances
  for (c1 in c("tag","distAttr","distSpatial")) {
    png(filename = paste0("../figs/",c1,"_",scenario,".png"),width = 5,height=5,units="in",res=300)
    print(plot(sf0[c1], border=NA, key.pos=1))
    dev.off()
  }  
  
  # plot the attribute distance (as percentage of range)
  sf1 <- sf0
  for (p1 in unique(c(attrs[["base"]],attrs[[scenario]]))) 
    sf1[[p1]] <- round(abs(sf1[[p1]][match(sf1$donor,sf1$id)]-sf1[[p1]])/diff(range(sf1[[p1]],na.rm=TRUE))*100)
  
  sf2 <- sf1 %>% select(all_of(attrs[[scenario]])) %>% gather(VAR, value, -geometry)
  sf2$VAR <- factor(sf2$VAR,levels <- attrs[[scenario]])
  gg1 <- ggplot() + 
    geom_sf(data = sf2, mapping=aes(fill = value),color=NA) + 
    theme_void() +
    scale_fill_viridis_c(option = "D") +
    facet_wrap(~VAR, ncol = 6)
  ggsave(paste0("../figs/reg_diff_attr_prc_",scenario,".png"),gg1,width=12,height=8,units="in",dpi=300)  
  
  # plot attribute difference individually
  for (c1 in unique(c(attrs[["base"]],attrs[[scenario]]))) {
    message(c1)
    sf1 <- sf0 %>% select(c("id","donor",all_of(c1)))
    sf1[[c1]] <- abs(sf1[[c1]]-sf1[[c1]][match(sf1[["donor"]],sf1[["id"]])])
    png(filename = paste0("../figs/reg_diff_attr_",c1,"_",scenario,".png"),width = 5,height=5,units="in",res=300)
    print(plot(sf1[c1], border=NA, key.pos=1))
    dev.off()
  }
  
  # density plots
  sf1 <- sf0
  for (c1 in unique(c(attrs[["base"]],attrs[[scenario]]))) {
    sf1[[c1]] <- abs(sf1[[c1]]-sf1[[c1]][match(sf1$donor, sf1$id)])
    dens <- vector("list")
    dens[["all"]] <- density(sf1[[c1]],na.rm=TRUE)
    dens[[scenario]] <- density(subset(sf1,tag==scenario)[[c1]],na.rm=TRUE)
    dens[["base"]] <- density(subset(sf1,tag=="base")[[c1]],na.rm=TRUE)
    dens[["proximity"]] <- density(subset(sf1,tag=="proximity")[[c1]],na.rm=TRUE)
    
    png(filename=paste0("../figs/density_diff_attr_",c1,"_",scenario,".png"),width=7.5,height=5,units="in",res=300)
    plot(NA, xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")),
         lwd=2.0, xlab=paste0("Difference in ",c1),ylab="Density",
         main=paste0("Distribution of donor-receiver difference in ", c1))
    mapply(lines, dens, col=1:length(dens))
    legend("topright", legend=names(dens), fill=1:length(dens))
    dev.off()
  }
}