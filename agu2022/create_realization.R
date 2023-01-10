# create realization files from a template containing all models (currently CFE and TOPMODEL), 
# using donor pairing and calibrated parameters for donor catchments

rm(list=ls())

library(data.table)
library(ggplot2)
library(plyr)
library(jsonlite)
library(magrittr)
library(dplyr)

ver_donor <- "v1.2"
ver_receiver <- "v0"
screening <- "kge_nse"

# calibration gages screened for performance
gages_calib <- get(load(paste0("output/",ver_receiver,"/calib_gages_screened_",screening,".Rdata")))

# stats simulated with old BMI and hydrofabric
load("stat/stat_retro_20131001_20160930_no_screening.Rdata")
stats0 <- subset(stats_str, site_no %in% gages_calib)

# upstream cathcments of gages
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))

# model/formulation scenarios
models <- c("CFE","TOPMODEL","CFE+TOPMODEL") 

# regionalization scenarios
runs <- c("hlr","camels") 

# calib scenarios
calib_scenarios <- c("kge","nse")

for (calib1 in calib_scenarios) {

# loop through model scenarios
for (model1 in models) {

if (model1 != "CFE+TOPMODEL") {

  # optimal parameters
  f1 <- paste0("data/calib_",calib1,"_dds/",tolower(model1),"_noah_",calib1,"_calib_best_params_2016_2021.csv")
  dtPars <- as.data.table(read.csv(f1,header=TRUE,colClasses=c("site_no"="character")))
  dtPars<- setNames(split(dtPars, seq(nrow(dtPars))), dtPars$site_no) #convert to list
  dtPars <- lapply(dtPars, function(x) {x$model <- model1;x})

} else {

  mods <- strsplit(model1,split="[+]")[[1]]
  dtPars0 <- vector("list", length(mods))
  names(dtPars0) <- mods
  
  for (m1 in mods) {

    # optimal parameters
    f1 <- paste0("data/calib_",calib1,"_dds/",tolower(m1),"_noah_",calib1,"_calib_best_params_2016_2021.csv")
    dtPars0[[m1]] <- as.data.table(read.csv(f1,header=TRUE,colClasses=c("site_no"="character")))
  }
  
  # get the optimal model and its parameters for each donor gage
  dtPars <- vector("list",length(gages_calib))
  names(dtPars) <- gages_calib
  for (g1 in gages_calib) {
    scenarios <- paste0(mods,"_",calib1,"_dds_no_screening")
    stats1 <- subset(stats0,scenario %in% scenarios & site_no==g1)
    mod1 <- strsplit(slice_max(stats1,get(toupper(calib1)))$scenario,split="_")[[1]][1]
    dtPars[[g1]] <- subset(dtPars0[[mod1]], site_no==g1)
    dtPars[[g1]]$model <- mod1
  }
}

# loop through regionalization scenarios
for (run1 in runs) {

# donor pairing
dtDonorAll <- get(load(paste0("output/",ver_receiver,"/donor_",run1,"_",screening,".Rdata")))
dtDonorAll <- dtDonorAll[,c("id","donor"), with=FALSE]

# realization template file
# set simplifyVector=FALSE to avoid reading into data.frame that would create issue for toJSON
l1 <- fromJSON("realization/realization_template.json", simplifyVector=FALSE) 

# loop through all receiver catchments
for (id1 in dtDonorAll$id) {
    
    message(paste0(model1," ",run1," ",match(id1,dtDonorAll$id)))
    
    # get donor gage for the current catchment
    donor1 <- dtDonorAll %>% filter(id==id1) %>% select(donor)
    suppressMessages(
    gage1 <- cwt %>% filter(id==donor1$donor) %>% select(gages)
    )
    gage1 <- gage1$gages
    gage1 <- gage1[gage1 %in% gages_calib]
    
    if (length(gage1)==0)  stop("ERROR: no donor gage found!")
    
    # if nested, assign to the inner gage
    if (length(gage1)>1) {
      gage1 <- cwt %>% subset(gages %in% gage1) %>% count("gages") %>% slice_min(freq)
      gage1 <- gage1$gages
    }

    # get donor parameters
    pars1 <- dtPars[[gage1]]
    
    # parameter names
    par_names <- names(pars1)[!is.na(pars1)]
    par_names <- par_names[!par_names %in% c("site_no","model")]

    # get the model template
    l2 <- l1$catchments[[paste0("cat-temp-",pars1$model)]]
    
    # assign donor parameters and add to realization
    n1 <- length(l2$formulations[[1]]$params$modules)
    for (p1 in par_names) 
        l2$formulations[[1]]$params$modules[[n1]]$params$model_params[[p1]] <- pars1[[p1]]
        #l2$formulations[[1]]$params$modules[[2]]$params$model_params[[p1]] <- as.character(sprintf("%0.7f",pars1[[p1]]))

    # forcing
    l2$forcing$path <- gsub(paste0("cat-temp-",pars1$model),id1,l2$forcing$path)
    l1$catchments[[id1]] <- l2
}

# remove templates
cat_all <- names(l1$catchments)
cats_temp <- cat_all[substr(cat_all,1,9)=="cat-temp-"]
for (c1 in cats_temp) l1$catchments[[c1]] <- NULL

# write realization to file
write(toJSON(l1,digits=7,pretty=TRUE,auto_unbox=TRUE),
      paste0("realization/",model1,"_",run1,"_",calib1,".json"))
}}}


