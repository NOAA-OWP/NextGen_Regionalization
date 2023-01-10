
rm(list=ls())

library(data.table)
library(sf)
library(magrittr)
library(ggplot2)
library(dplyr)

sf::sf_use_s2(FALSE)

scenario <- "kge"
period1 <- "valid"
run1 <- "valid_best"
min_kge <- 0.3
min_nse <- 0.1
min_corr <- 0.1
max_pbias <- 60

scenario <- "nse"
min_kge <- 0.1
min_nse <- 0.3

# CFE results (50 gages)
f1 <- paste0("data/calib_",scenario,"_dds/cfe_noah_",scenario,"_valid_stat.csv")
dtGages1 <- read.csv(f1,header=TRUE,colClasses=c("site_no"="character"))

# Topmodel results (50 gages)
f2 <- paste0("data/calib_",scenario,"_dds/topmodel_noah_",scenario,"_valid_stat.csv")
dtGages2 <- read.csv(f2,header=TRUE,colClasses=c("site_no"="character"))

gages_cfe <- unique(subset(dtGages1,period==period1 & run==run1 & KGE>=min_kge & 
          NSE>=min_nse & Corr>=min_corr & abs(PBIAS)<=max_pbias)$site_no)
gages_topmodel <- unique(subset(dtGages2,period==period1 & run==run1 & KGE>=min_kge & 
          NSE>=min_nse & Corr>=min_corr & abs(PBIAS)<=max_pbias)$site_no)

gages_calib <- list(cfe=gages_cfe, topmodel=gages_topmodel)

save(gages_calib, file=paste0("output/v1.2/calib_gages_screened_",scenario,"_dds.Rdata"))

# plot calibration catchments
ver_donor <- "v1.2"
shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))
sf1 <- st_union(shps_don)

# all HUC-01 cathcments
plot(st_geometry(sf1),border="grey",main="Calibration catchments")

# all topmodel calibration catchments
donors <- unique(subset(cwt, gages %in% unique(dtGages2$site_no))$id)
plot(st_geometry(subset(shps_don,id %in% donors)),border="lightblue",add=T)

# Topmodel calibration catchments with good performance
donors <- unique(subset(cwt, gages %in% gages_topmodel)$id)
plot(st_geometry(subset(shps_don,id %in% donors)),border="blue",add=T)

# CFE calibration catchments with good performance
donors <- unique(subset(cwt, gages %in% gages_cfe)$id)
plot(st_geometry(subset(shps_don,id %in% donors)),border="red",add=T)

# calibration catchments with good performance for both Topmodel and CFE
donors <- unique(subset(cwt, gages %in% intersect(gages_cfe,gages_topmodel))$id)
plot(st_geometry(subset(shps_don,id %in% donors)),border="purple",add=T)

legend("topright",legend=c("All","Topmodel_good","CFE_good","final"),
       text.col=c("lightblue","blue","red","purple"),border=FALSE)


# subset cwt for calibration basins
cwt <- subset(cwt, gages %in% unique(dtGages2$site_no))
names(cwt)[names(cwt)=="id"] <- "donor"

# merge donor catchments with cwt
shps_don1 <- shps_don %>%
  filter(id %in% cwt$donor) %>% 
  select(c("id","geometry")) %>% 
  setnames(c("donor","geometry")) %>% 
  merge(cwt,by="donor",all.x=TRUE)

# handle netsted donor gages
cats0 <- shps_don1 %>% select("donor") %>% duplicated() %>% 
  filter(shps_don1,.) %>% select("donor")
tmp0 <- shps_don1 %>% filter(donor %in% cats0$donor)
nests <- cwt %>% filter(gages %in% unique(tmp0$gages)) %>% 
  group_by(gages) %>% count() %>% arrange(desc(n))
gage1 <- nests$gages[nrow(nests)]
tmp0 <- subset(tmp0,gages==gage1)
shps_don1 <- rbind(tmp0,subset(shps_don1,!donor %in% cats0$donor)) %>%
  group_by(gages) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()

# compute centroid of donor gages
cent_don <- st_centroid(shps_don1)

cent1 <- cbind(cent_don$gages,st_coordinates(cent_don)) %>% 
  as.data.table() %>% 
  setnames(c("gage","lon","lat")) %>%
  setorder(cols="lat") 

ggplot() + geom_sf(data = sf1,color="black",fill=NA) +
  geom_sf(data=cent_don, color="red", shape=3) + 
  geom_sf_label(data=cent_don,aes(label=gages))