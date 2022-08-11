# download Polairs data for a given lat/lon box

rm(list=ls())

lat1 <- 23; lat2 <- 50
lon1 <- -124; lon2 <- -80
layers <- c("0_5","5_15","15_30","30_60","60_100","100_200")

urlFileExist <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status == HTTP_STATUS_OK, status = status)
}

for (layer in layers) {
for (lat in lat1:(lat2-1)) {
  for (lon in lon1:(lon2-1)) {
    file1 <- paste0("http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/alpha/mean/",layer,"/lat",lat,lat+1,"_lon",lon,lon+1,".tif")
    dest1 <- paste0("../datasets/Polaris/",layer,"/",basename(file1))
    if (!file.exists(dest1)) {
      if (urlFileExist(file1)$exists) {
         message(paste0(layer," ", lat," ",lon))
         if (!dir.exists(dirname(dest1))) dir.create(dirname(dest1),recursive=TRUE) 
            download.file(file1, dest1,method="curl")
    }}
}}}
