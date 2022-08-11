# read the hourly AORC data for each catchment from csv files into Rdata format

rm(list=ls())

# data period
date1 <- "20070101"
date2 <- "20191231"
h1 <- as.POSIXct(date1,format="%Y%m%d")
h2 <- as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H")
hours <- data.frame(Time=seq(h1,h2,by="hour"))

# hourly data for all forcing variables in individual csv files
dir1 <- "/local/apd_common/agu_2021/forcing/huc_01/csv"
files <- list.files(dir1,full.names=FALSE)
cats <- gsub(".csv","",files)

# output file: hourly pcp for all catchments in a single Rdata file
outfile <- paste0("../datasets/AORC/aorc_huc01_",date1,"-",date2,"_hourly.Rdata")
if (file.exists(outfile)) {
  load(outfile)
} else {
  aorc <- vector("list",length(cats))
  names(aorc) <- cats
}

# loop through all catchments to read the csv files
for (c1 in cats) {
  if (!is.null(aorc[[c1]])) next
  i1 <- match(c1,cats)
  message(paste0(i1,"   ",c1))
  f1 <- paste0(dir1,"/",c1,".csv")
  df1 <- read.csv(f1,header=TRUE)
  df1$Time <- as.POSIXct(df1$Time, format="%Y-%m-%d %H:00:00",tz="UTC") 
  df1 <- merge(hours,df1,by="Time",all.x=TRUE)
  pcp <- df1$RAINRATE # mm/hr
  pcp <- round(pcp * 3600,3) # convert to pcp amount in mm
  aorc[[c1]] <- pcp

  # save data to output file every 200 catchments
  if ((i1 %% 200)==0 | i1==length(cats)) save(aorc,file=outfile)
}

