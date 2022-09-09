# create realization files from a template containing all models (currently CFE and TOPMODEL), 
# using donor pairing and calibrated parameters for donor catchments

rm(list=ls())

library(data.table)
library(ggplot2)
library(plyr)
library(jsonlite)
library(magrittr)

# define calibrated parameters for each model
opt_pars <- list(CFE=c("b", "satdk", "maxsmc", "slope", "max_gw_storage", "expon", "Cgw", "Klf", "Kn"),
                 TOPMODEL=c("sr0", "srmax", "szm", "t0", "td"))

# model/formulation scenarios
models <- c("CFE","TOPMODEL","CFE+TOPMODEL") 

# regionalization scenarios
runs <- c("hlr","camels") 

# optimal parameter sets from validation period for donors
pars0 <- as.data.table(read.csv("../data/headwater_cat_optimal_params.csv",header = TRUE))
pars0 <- subset(pars0, period=="valid")

# loop through model scenarios
for (model1 in models[1]) {
pars_calib <- subset(pars0,model_type==model1)
pars_calib <- pars_calib[, c("catid", unlist(opt_pars)), with=F]

# loop through regionalization scenarios
for (run1 in runs[1]) {

# donor pairing
dtDonorAll <- get(load(paste0("../data/donor_",run1,".Rdata")))
dtDonorAll <- dtDonorAll[,c("id","donor"), with=FALSE]

# add donor catchments to the donor table (receiver==donor for donor catchments)
donors <- unique(pars_calib$catid)
dtDonorAll <- subset(dtDonorAll, ! id %in% donors)
dtDonorAll <- rbind(dtDonorAll, data.table(id=donors,donor=donors)) 

# realization template file
# set simplifyVector=FALSE to avoid reading into data.frame that would create issue for toJSON
l1 <- fromJSON("../realization/realization_template.json", simplifyVector=FALSE) 

# loop through all catchments
for (id1 in dtDonorAll$id) {
    
    message(paste0(model1," ",run1," ",match(id1,dtDonorAll$id)))
    
    # get donor parameters for the current catchment
    donor1 <- dtDonorAll$donor[match(id1, dtDonorAll$id)]
    pars1 <- subset(pars_calib, catid==donor1)
    
    #determine which model template to use (CFE or TOPMODEL) for the current catchment
    model2 <- ""
    par_names <- names(pars1)[!is.na(pars1)]
    par_names <- par_names[par_names!="catid"]
    for (m1 in names(opt_pars)) {
      if (sum(par_names %in% opt_pars[[m1]])==length(opt_pars[[m1]])) {
        model2 <- m1; break
    }}
    if (model2=="") stop(paste0("ERROR: optimized parameters do not match for ", id1))
    
    # get the model template
    l2 <- l1$catchments[[paste0("cat-temp-",model2)]]
    
    # assign donor parameters and add to realization
    for (p1 in par_names) 
        l2$formulations[[1]]$params$modules[[2]]$params$model_params[[p1]] <- pars1[[p1]]
        #l2$formulations[[1]]$params$modules[[2]]$params$model_params[[p1]] <- as.character(sprintf("%0.7f",pars1[[p1]]))

    # forcing
    l2$forcing$path <- gsub(paste0("cat-temp-",model2),id1,l2$forcing$path)
    l1$catchments[[id1]] <- l2
}

# remove templates
cat_all <- names(l1$catchments)
cats_temp <- cat_all[substr(cat_all,1,9)=="cat-temp-"]
for (c1 in cats_temp) l1$catchments[[c1]] <- NULL

# write realization to file
write(toJSON(l1,digits=7,pretty=TRUE,auto_unbox=TRUE),paste0("../realization/noah_owp_",model1,"_",run1,".json"))
}}


