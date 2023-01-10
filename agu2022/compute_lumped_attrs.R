rm(list=ls())

library(data.table)
library(sf)

# huc-01
#huc01 <- st_read("shapefile/catchment_data.geojson")

# upstream catchments of gages
dtUpstream <- get(load("output/crosswalk_gage_cat_huc01.Rdata"))
names(dtUpstream)[names(dtUpstream)=="gages"] <- "gage"

# attributes of individual catchments
dtAttrAll <- get(load("output/all_attrs.Rdata"))

# merge with upstream catchments
dtAll <- merge(dtAttrAll,dtUpstream[,c("id","areasqkm","gage"),with=F],by="id")
dtAll <- subset(dtAll, !is.na(gage))

# for each gage, compute area-weighted attributes based on upstream catchments
attrs <- names(dtAttrAll)
attrs <- attrs[attrs != "id"]
dtAttrLump <- data.table(gage=unique(dtAll$gage))
dtArea <- dtAll[,.(total_area=sum(areasqkm)),by=.(gage)]
for (a1 in attrs) {
  if (a1 != "hsg") {
    tmp <- dtAll[,.(sum1=sum(get(a1)*areasqkm,na.rm=TRUE)),by=.(gage)]
    tmp <- merge(tmp,dtArea,by="gage",all=TRUE)
    tmp[,eval(a1):=sum1/total_area]
  } else {
    tmp <- dtAll[,.(sum1=sum(areasqkm)),by=.(gage,hsg)]
    tmp <- tmp[,.(hsg=hsg[which.max(sum1)]),by=.(gage)]
  }
  dtAttrLump <- merge(dtAttrLump,tmp[,c("gage",a1),with=F], by="gage")
}

save(dtAttrLump,file="output/all_attrs_lumped_gage.Rdata")
