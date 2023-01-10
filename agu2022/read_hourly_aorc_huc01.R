# read the hourly AORC data for each catchment from csv files into Rdata format

rm(list=ls())

# data period
#date1 <- "20121001"; date2 <- "20210831"
date1 <- "20070101"; date2 <- "20191231"
h1 <- as.POSIXct(date1,format="%Y%m%d")
h2 <- as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H")
hours <- data.frame(Time=seq(h1,h2,by="hour"))

# hourly data for all forcing variables in individual csv files
#dir1 <- "../huc01/huc01_v1.2_2012_2021_AORC_forcings/csv_files"
dir1 <- "../huc01/huc_01/forcing/csv"
files <- list.files(dir1,full.names=FALSE)
cats <- gsub(".csv","",files)

#for (var1 in c("RAINRATE","LWDOWN","SWDOWN","T2D")) {
for (var1 in c("LWDOWN","SWDOWN","T2D")) {

# output file: hourly pcp for all catchments in a single Rdata file
#outfile <- paste0("AORC_hydrofabric_v1.2/aorc_",var1,"_huc01_",date1,"-",date2,"_hourly.Rdata")
outfile <- paste0("AORC_hydrofabric_v0/aorc_",var1,"_huc01_",date1,"-",date2,"_hourly.Rdata")
if (file.exists(outfile)) {
  load(outfile)
} else {
  aorc <- vector("list",length(cats))
  names(aorc) <- cats
}

# loop through all catchments to read the csv files
for (c1 in cats) {

  # check if there are basins with no data (read from previous run)
  # check if there is NA values (earlier version of csv files has data gaps for some catchments)
  if (!is.null(aorc[[c1]])) { 
     if (length(aorc[[c1]])==nrow(hours) & sum(is.na(aorc[[c1]]))==0) next
  }

  i1 <- match(c1,cats)
  message(paste0(var1,"   ",i1,"   ",c1))
  f1 <- paste0(dir1,"/",c1,".csv")
  df1 <- read.csv(f1,header=TRUE)
  df1$Time <- as.POSIXct(df1$Time, format="%Y-%m-%d %H:00:00",tz="UTC") 
  df1 <- merge(hours,df1,by="Time",all.x=TRUE)
  pcp <- df1[[var1]] 
  if (var1=="RAINRATE") pcp <- round(pcp * 3600,3) # convert pcp from mm/s to mm
  if (var1 %in% c("LWDOWN","SWDOWN","T2D")) pcp <- round(pcp)
  aorc[[c1]] <- pcp

  # save data to output file every 2000 catchments
  if ((i1 %% 2000)==0) save(aorc,file=outfile)
}

save(aorc,file=outfile)
}
