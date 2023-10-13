# Derive climate attributes (P, PET, aridity etc) from catchment csv data (based on AORC)

rm(list=ls())

library(data.table)

rm(list=ls())

ver1 <- "2.0pre" #hydrofabric version
huc1 <- "12"

# hourly data for all forcing variables in individual csv files
files <- list.files(paste0("../../datasets/AORC_hydrofabric_v",ver1,"/huc",huc1),full.names=TRUE)

# hours expected
date1 <- "20121001"; date2 <- "20210831"
h1 <- as.POSIXct(date1,format="%Y%m%d")
h2 <- as.POSIXct(paste0(date2,"23"),format="%Y%m%d%H")
hours <- data.table(Time=seq(h1,h2,by="hour"))

attrs <- data.table()
outfile <- paste0("output/clim_attr_huc",huc1,"_v",ver1,".Rdata")
if (file.exists(outfile))   load(outfile)

for (f1 in files) {
  
  cat1 <- gsub(".Rdata","",strsplit(basename(f1),split="_")[[1]][5])
  if (nrow(attrs)>0) if (cat1 %in% attrs[['id']]) next

  message(match(f1,files))

  df1 <- get(load(f1))
  df1$Time <- hours$Time

  # check missing data
  for (c1 in names(df1)) {
    if (c1 == "Time") next
    n1 <- sum(is.na(df1[[c1]]))
    if (n1>0) message(paste0("WARNING: ", n1, "(",round(n1/nrow(df1)*100)," %) missing data points found for ", f1))
  }
  
  # mean annual pcp
  nyear <- nrow(df1)/365/24
  p_mean <- sum(df1[['APCP']])/nyear

  df1[,yr:=format(Time,"%Y")]
  df1[,mn:=format(Time,"%m")]
  df1[,yr:=ifelse(mn>=10,as.numeric(yr)+1,yr)]
  df1[,day:=format(Time,"%d")]

  # daily pcp, daily mean/min/max temperature 
  df_daily <- df1[,.(pcp=sum(APCP),meant=mean(T2D),mint=min(T2D),maxt=max(T2D)),by=.(yr,mn,day)]

  # monthly mean/min/max temperature
  ta <- df_daily[,.(meant=mean(meant)-273.15,mint=mean(mint)-273.15,maxt=mean(maxt)-273.15),by=.(yr,mn)]
  
  # monthly short- & longwave
  ra <- df1[,.(sw=sum(SWDOWN)*0.0036,lw=sum(LWDOWN)*0.0036),by=.(yr,mn)]

  # Hargreaves method for PET
  dt1 <- merge(ta,ra,by=c("yr","mn"),all=TRUE)
  dt1[,pet:=0.0023*(sw+lw)*(meant+17.8)*(maxt-mint)^0.5] #monthly PET
  dt1 <- dt1[,.(pet=sum(pet)),by=.(yr)] #annual PET
  pet_mean <- sum(dt1$pet)/nyear * 0.408  #mean annual PET, covert from MJ/m2 to mm

  #aridity
  aridity <- pet_mean/p_mean

  # compute Feddema moisture index
  FMI <- ifelse(p_mean>=pet_mean, 1-pet_mean/p_mean, p_mean/pet_mean-1)

  # compute snow fraction
  sf1 <- round(sum(df_daily$pcp[df_daily$meant<273.15],na.rm=T)/sum(df_daily$pcp,na.rm=T),2)

  # high and low precip frequency and duration 
  hpf1 <- lpf1 <- hpd1 <- lpd1 <- NA
  p0 <- mean(df_daily$pcp,na.rm=TRUE)
  if (p0 > 0.00001) {
    df_daily[,high:=ifelse(pcp>=5*p0,1,0)]
    df_daily[,low:=ifelse(pcp<1,1,0)]
    hpf <- df_daily[,.(hpf=sum(high)),by=.(yr)]
    hpf1 <- mean(hpf$hpf)
    lpf <- df_daily[,.(lpf=sum(low)),by=.(yr)]
    lpf1 <- mean(lpf$lpf)

    # function for computing high/low precip duration
    myfunc <- function(x) {
      y <- nchar(strsplit(paste(x,collapse=""),split="0")[[1]])
      y <- mean(y[y>0])
    }  
    hpd1 <- myfunc(df_daily$high)
    lpd1 <- myfunc(df_daily$low)
  }

  attrs <- rbind(attrs, data.table(id=cat1, p_mean=p_mean, pet_mean=pet_mean, FMI=FMI, aridity=aridity,snow_frac=sf1,
                  high_prec_freq=hpf1,low_prec_freq=lpf1,high_prec_dur=hpd1,low_prec_dur=lpd1))

  save(attrs,file=outfile)
}

# check attribtue summary
library(sf)
pars <- names(attrs)
pars <- pars[pars != "id"]
for (c1 in pars) {
  message(c1)
  print(summary(attrs[[c1]]))
}
#huc <- st_read(paste0("../../datasets/gpkg_v",ver1,"/huc",huc1,"/catchment_data.geojson"))
huc <- read_sf(paste0("../../datasets/gpkg_v",ver1,"/nextgen_",huc1,".gpkg"),"divides")
huc$id <- NULL
names(huc)[names(huc)=="divide_id"] <- "id"
huc <- merge(huc, attrs, by="id", all=TRUE)

# plot the attributes (multiple panels, no legend)
#plot(huc01[pars],border=NA)

# plot the attributes separately with legend
for (c1 in pars) {
  message(c1)
  png(filename = paste0("figs/attr_",c1,"_huc",huc1,"_",ver1,".png"),width = 5,height=5,units="in",res=300)
  print(plot(huc[c1], border=NA, key.pos=1))
  dev.off()
}


