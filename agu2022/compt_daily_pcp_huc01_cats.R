rm(list=ls())

library(data.table)
library(zonal)
library(sf)

dir1 <- "/glade/scratch/ariz01/aorc/"
date1 <- "20071001"
date2 <- "20210930"
dates <- format(seq(as.POSIXct(date1,format="%Y%m%d",tx="UTC"), as.POSIXct(date2,format="%Y%m%d",tx="UTC"),by="day"), "%Y%m%d", tz="UTC")

# read HUC-01 geojson into sf
huc01 <- st_read("../data/catchment_data_drano.geojson")

# loop through dates to compute mean areal apcp for HUC-01 catchments
for (d1 in dates) {
    outfile <- paste0(dir1,"APCP_daily_huc01/",d1,".Rdata")
    if (file.exists(outfile)) next
    message(d1)

    f1 <- paste0(dir1,"APCP_daily_netcdf/",d1,".nc")
    if (!file.exists(f1)) stop(paste0("ERROR: file does not exist - ", f1))
    dt_apcp <- as.data.table(execute_zonal(f1,geom=huc01,ID="id",join=FALSE))
    save(dt_apcp, file=outfile)
}
