rm(list=ls())

library(data.table)
library(sf)

# CFE calibration results (50 gages)
scenario <- "kge"
f1 <- paste0("data/calib_",scenario,"_dds/cfe_noah_",scenario,"_valid_stat.csv")
gages0 <- unique(read.csv(f1,header=TRUE,colClasses=c("site_no"="character"))$site_no)

# stats simulated with old BMI and hydrofabric
load("stat/stat_retro_20131001_20160930_no_screening.Rdata")

# subset stats to calibartion gages
stats0 <- subset(stats_str, site_no %in% gages0)

min_metric <- 0.3
gages_calib <- gages0
for (scenario in c("kge","nse")) {
  for (model in c("CFE","TOPMODEL")) {
    c1 <- paste0(model,"_",scenario,"_dds_no_screening")
    gages1 <- stats0 %>% 
              filter(scenario==c1) %>% 
              filter(!!as.symbol(toupper(scenario))>=min_metric) %>% 
              pull(site_no) %>% unique(.)
    gages_calib <- intersect(gages_calib,gages1)
  }
}

save(gages_calib, file="output/v0/calib_gages_screened_kge_nse.Rdata")

# plot calibration catchments
ver_donor <- "v1.2"
shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))
sf::sf_use_s2(FALSE)
sf1 <- st_union(shps_don)

# all HUC-01 cathcments
plot(st_geometry(sf1),border="black",main="Calibration catchments")

# all calibration catchments
donors <- unique(subset(cwt, gages %in% gages0)$id)
plot(st_geometry(subset(shps_don,id %in% donors)),border="darkgrey",add=T)

# selected catchments with good performance in all scenarios
donors <- unique(subset(cwt, gages %in% gages_calib)$id)
plot(st_geometry(subset(shps_don,id %in% donors)),border="blue",add=T)

legend("topright",legend=c("All","selected"),text.col=c("darkgrey","blue"),border=FALSE)