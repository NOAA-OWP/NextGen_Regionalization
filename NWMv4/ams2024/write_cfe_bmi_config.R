# This script replaces the parameters values the default CFE BMI config files with calibrated/regionalized parameters

rm(list=ls())

library(data.table)
library(sf)

# regionalization run and region
run <- "camels_urf"  # a run is defined by the attribute scenario (e.g., camels) and the algorithm (e.g., urf)
h1 <- "01"; ver <- "2.0"
region <- paste0("huc",h1,"_v",ver)

# all catchments in the HUC region
gfile <- paste0('../../datasets/gpkg_v',ver,'/nextgen_huc',h1,'.gpkg')
huc <- read_sf(gfile, "divides")

# read donor-receiver pairs
dtDonors <- as.data.table(read.csv(paste0("../output_donor/donor_",run,"_",region,".csv")))
dtDonors <- dtDonors[,c("id","donor"), with=FALSE]

# add donor catchments to the donor table
id_str <- "id"; if (ver %in% c("2.0pre","2.0")) id_str <- "divide_id"
donor_cats <- huc[[id_str]][!huc[[id_str]] %in% dtDonors$id]
dtDonors <- rbind(dtDonors,data.table(id=donor_cats, donor=donor_cats))

# crosswalk table
cwt <- as.data.table(read.csv(paste0("../data/crosswalk_gage_cat_", region,".csv"),colClasses="character"))

# calibrated parameters
dtPars0 <- as.data.table(read.csv(paste0("../data/calibration_results/huc01_v1.2/cfe_noah_parameters.csv"),colClasses=c("character",rep("numeric",11))))
names(dtPars0)[names(dtPars0)=="Kn"] <- "K_nash"
names(dtPars0)[names(dtPars0)=="Klf"] <- "K_lf"
names(dtPars0)[names(dtPars0)=="b"] <- "soil_params.b"
names(dtPars0)[names(dtPars0)=="satdk"] <- "soil_params.satdk"
names(dtPars0)[names(dtPars0)=="satpsi"] <- "soil_params.satpsi"
names(dtPars0)[names(dtPars0)=="maxsmc"] <- "soil_params.smcmax"
names(dtPars0)[names(dtPars0)=="slope"] <- "soil_params.slop"

# calibration basins used as donors
calib_gages <- read.csv("../data/donor_gages_huc01_screened.csv", colClasses="character", header=FALSE)$V1

# loop through bmi files to change the parameter values based on donor-receiver pairs
dir1 <- paste0("../data/bmi_config/",region,"/",run)
if(!dir.exists(dir1)) dir.create(dir1,recursive=TRUE)
files <- list.files(paste0("../data/bmi_config/",region,"/default"),pattern="CFE",full.names=TRUE)

for (f1 in files) {

    # read lines in the BMI file
    message(match(f1,files))
    lines0 <- readLines(f1)

    # determine the catchment ID
    cat1 <- basename(f1) %>% gsub("CFE_","",.) %>% gsub(".ini","",.)

    # find the donor gage ID given the catchment ID based on the crosswalk
    gage1 <- subset(cwt, id==subset(dtDonors,id==cat1)$donor)$gages
    gage1 <- gage1[gage1 %in% calib_gages]

    # choose inner gage if nested
    if (length(gage1)>1) gage1 <- cwt %>% filter(gages %in% gage1) %>% pull(gages) %>% table(.) %>% which.min(.) %>% names()

    if (length(gage1)!=1) stop(message("Error for ",f1,", number of donor gage should be 1, not ", length(gage1)))
    
    # get the calibrated parameters given the gage ID
    pars1 <- subset(dtPars0,site_no==gage1)
    if (nrow(pars1)==0) stop(message("Calibrated parameters not found for gage ", gage1," for file=", f1))
    for (p1 in names(pars1)) {
        if (p1=="site_no") next
        idx <- which(startsWith(lines0,paste0(p1,"=")))
        strs1 <- strsplit(lines0[idx],split="=")[[1]]
        strs2 <- strsplit(strs1[2],split="\\[")[[1]]
        line1 <- paste0(p1,"=",format(pars1[[p1]],scientific=FALSE))
        if (length(strs2)>1) line1 <- paste0(line1,"[",strs2[2])
        lines0[idx] <- line1
    }

    # write the new BMI file
    writeLines(lines0,gsub("default",run,f1))
}

# check the new BMI files to make sure all parameter fields are properly filled
for (f1 in files) {
    message(match(f1,files))
    lines1 <- readLines(gsub("default",run,f1))
    for (l1 in lines1) {
        par1 <- l1 %>% strsplit(.,"[[]") %>% { .[[1]][1] } %>% strsplit(.,"=") %>% { .[[1]][2] }
        if (is.na(par1) | (par1=="nan")) stop(message("Error in parameter assignment: ", f1))
    }
}