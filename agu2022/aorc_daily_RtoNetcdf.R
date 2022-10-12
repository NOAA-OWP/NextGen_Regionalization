# read previously compute daily AORC apcp in Rdata format and rewrite to netcdf format for execute_zonal

rm(list=ls())

library(ncdf4)

dir1 <- "/glade/scratch/ariz01/aorc/"

date1 <- "20071001"
date2 <- "20210930"
dates <- format(seq(as.POSIXct(date1,format="%Y%m%d",tx="UTC"), as.POSIXct(date2,format="%Y%m%d",tx="UTC"),by="hour"), "%Y%m%d", tz="UTC")

for (d1 in dates) {

    # check if file already exists
    f1 <- paste0(dir1,"APCP_daily_netcdf/",d1,".nc")
    if (file.exists(f1)) next

    message(d1)

    # read daily APCP in R format (previously computed)
    apcp0 <- get(load(paste0(dir1,"APCP_daily/",d1,".Rdata")))

    # first copy a template netcdf file
    file.copy("../data/2007100100.nc",f1,overwrite=TRUE)

    # then replace RAINRATE, which is actually daily apcp in mm
    nc1 <- nc_open(f1,write=TRUE)
    ncvar_put(nc1, "RAINRATE", apcp0)
    nc_close(nc1)
}
