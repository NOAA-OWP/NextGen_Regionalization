# create realization files from a template containing all models (currently CFE and TOPMODEL), 
# using donor pairing and calibrated parameters for donor catchments
#
# Here the realization file for each individual calibration basin was created, but
# eventually not used for AGU 2022

rm(list=ls())

library(data.table)
library(ggplot2)
library(plyr)
library(jsonlite)
library(magrittr)
library(dplyr)

calib_scenario <- "kge"
ver_donor <- "v1.2"
ver_receiver <- "v0"
metric0 <- "KGE"
period0 <- "valid"
run0 <- "valid_best"

# upstream cathcments of gages
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))

# model/formulation scenarios
models <- c("CFE","TOPMODEL")

# upstream catchments of calibration gages
catchments <- get(load("data/calib_gage_catchments_huc01.Rdata"))
gagesAll <- names(catchments)

# loop through model scenarios
for (model1 in models) {

# optimal parameters
f1 <- paste0("data/calib_",calib_scenario,"_dds/",tolower(model1),"_noah_",calib_scenario,"_calib_best_params_2016_2021.csv")
dtPars <- as.data.table(read.csv(f1,header=TRUE,colClasses=c("site_no"="character")))
dtPars<- setNames(split(dtPars, seq(nrow(dtPars))), dtPars$site_no) #convert to list
dtPars <- lapply(dtPars, function(x) {x$model <- model1;x})
  
# loop through calibration gages
for (gage0 in gagesAll) {
  
  message(paste0(model1," ",gage0))
  
  # get donor parameters
  pars1 <- dtPars[[gage0]]
  
  # parameter names
  par_names <- names(pars1)[!is.na(pars1)]
  par_names <- par_names[!par_names %in% c("site_no","model")]
  
  # realization template file
  # set simplifyVector=FALSE to avoid reading into data.frame that would create issue for toJSON
  l1 <- fromJSON("realization/realization_template.json", simplifyVector=FALSE) 
  
  # loop through catchments
  for (id1 in catchments[[gage0]]) {
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
      paste0("realization/calib_basins/",model1,"_",ver_receiver,"_",calib_scenario,"_",gage0,".json"))
}}


