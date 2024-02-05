# Download daily streamflow data from usgs and save in pixml format
# (for donors and streamflow signature basins)

#rm(list=ls())
date1 <- "2007-10-01"
date2 <- "2021-09-30"
nyear_min <- 3

library(dataRetrieval)
library(data.table)
library(doParallel)
library(foreach)

getDailyFlowUSGS <- function(site,startDate,endDate) {
   url <- constructNWISURL(site,'00060',startDate,endDate,'dv')
   #data <- importWaterML1(gsub('http','https',url), asDateTime=TRUE)
   data <- importWaterML1(url, asDateTime=TRUE)
   dates <- seq(as.Date(startDate),as.Date(endDate),by='day')
   rawData <- data.frame(date=dates, flow=-999.0)
   if (nrow(data)>0) {
     dates1 <- format(data$dateTime,'%Y-%m-%d',tz='UTC')
     idx <- match(dates1,as.character(rawData$date))
     rawData$flow[idx] <- data$X_00060_00003
     #icol <- which(colnames(data)=="X_00060_00003")
     #if (length(icol)==0)
     #   icol <- grepl('^X_',colnames(data)) & grepl('00060_00003$',colnames(data))
     #if (length(icol)>0) {
     #   dates1 <- format(data$dateTime,'%Y-%m-%d',tz='UTC')
     #   idx <- match(dates1,as.character(rawData$date))
     #   rawData$flow[idx] <- data[,icol]
     #}
   }
   return(rawData)
}

# ss basins
basins <- c(get(load("../../data/meta_ss_basins.Rdata"))$ID, get(load("../../data/meta_donors_v3.Rdata"))$ID)

# loop through basins 
basins1 <- ""
for (i in 1:length(basins)) {

if (nchar(basins[i])<8) next

# check if data has slready been downloaded, if yes, skip to the next location
outFile <- paste0("/glade/work/ariz01/data/usgs/daily_streamflow/",basins[i],'_daily_streamflow.Rdata')
if (file.exists(outFile)) next
if (!dir.exists(dirname(outFile))) dir.create(dirname(outFile),recursive=TRUE)

# retrieve usgs data
print(paste0("downloading data for basin ",i," ", basins[i]))
startDate <- as.character(as.Date(date1))
endDate <- as.character(as.Date(date2))
dtFlow <- getDailyFlowUSGS(basins[i],startDate,endDate)
dtFlow <- as.data.table(dtFlow)
n1 <- sum(dtFlow$flow>=0,na.rm=T)

if (n1 >= 365*nyear_min) {
dtFlow$flow <- dtFlow$flow*(0.3048^3) #convert cfs to cms
dtFlow$flow[dtFlow$flow < 0] <- -999.0
names(dtFlow) <- c("date","flow_cms")
save(dtFlow,file=outFile)
} else {
basins1 <- c(basins1,basins[i])
}}

basins1 <- basins1[basins1!=""]
save(basins1,file="../../data/basins_missing_usgs_data.Rdata")
