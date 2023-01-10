# compute mean annual P, PET, and snow fraction (mm) in 1km grid
# leveraging the annual P/PET/SF 1-km grid previously calcualted for NWM v3 (on Cheyenne)

years <- 2013:2021
dir1 <- "../datasets/AORC_hydrofabric_v0/yearly/" 
dir2 <- "../datasets/AORC_hydrofabric_v1.2/"

message("first compute mean annual PCP ...")
for (yr in years) {
  load(paste0(dir1,"apcp_aorc_",yr,".Rdata"))
  if (yr==years[1]) {
    apcp0 <- apcp
  } else {
    apcp0 <- apcp0 + apcp
  }
}
apcp0 <- apcp0/length(years)
save(apcp0,file=paste0(dir2,"mean_annual_APCP_",years[1],"-",years[length(years)],"_AORC.Rdata"))

message("then compute mean annual PET ...")
for (yr in years) {

  load(paste0(dir1,"PET_aorc_",yr,".Rdata"))
  if (yr==years[1]) {
    pet0 <- pet
  } else {
    pet0 <- pet0 + pet
  }

}
pet0 <- pet0/length(years)*0.408 #convert from  MJ/m2 to mm
save(pet0,file=paste0(dir2,"mean_annual_PET_",years[1],"-",years[length(years)],"_AORC.Rdata"))

message("then compute mean annual snow fraction ...")
for (y1 in years) {
load(paste0(dir1,"aorc_apcp_snow_frac_",y1,".Rdata"))
if (y1==years[1]) {
  snow_frac <- sf
} else {
  snow_frac <- snow_frac + sf
}}
snow_frac <- snow_frac/length(years)

save(snow_frac, file=paste0(dir2,"mean_annual_snow_frac_",years[1],"-",years[length(years)],".Rdata"))
