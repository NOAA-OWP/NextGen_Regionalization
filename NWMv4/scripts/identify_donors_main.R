# Create donor-receiver pairing using Principal Component Analysis (PCA) and 
#    Gower's distance, using different sets of attributes (HLR or CAMELS) 
# Multiple rounds are conducted to handle spatial gaps (NA values) in some attributes 
#
# This version allows donors and receivers to have different hydrofabric files

rm(list=ls())

library(data.table)
library(yaml)

source("apply_pca.R")
source("apply_donor_constraints.R")
source("gower_dist.R")
source("kmeans_clust.R")
source("compute_dist_spatial.R")

# Read the configuration file and assign parameters
config <- read_yaml("../data/config.yaml",readLines.warn=FALSE)

# donor attributes
dtAttrDonor <- get(load("../data/all_attrs_donors.Rdata"))
dtAttrDonor$tag <- "donor"

# receiver attributes
dtAttrReceiver <- get(load("../data/all_attrs_receivers.Rdata"))
dtAttrReceiver$tag <- "receiver"

# attributes for both donors and receivers
dtAttrAll <- rbind(dtAttrDonor, dtAttrReceiver)
dtAttrAll[,snowy:=ifelse(snow_frac>=config$pars$general$minSnowFrac,TRUE,FALSE)] #snowiness
rm(dtAttrDonor, dtAttrReceiver)

# columns in the attrs table that are not actual attributes
config$non_attr_cols <- c("id","tag","snowy","hsg")

# compute spatial distance between all receivers and donors if not already exists
f1 <- paste0("../data/dist_spatial_donor_receiver.Rdata")
if (file.exists(f1)) {
  distSpatial0 <- get(load(f1))
} else {
  distSpatial0 <- compute_dist_spatial(f1, subset(dtAttrAll,tag=="donor")$id, subset(dtAttrAll,tag=="receiver")$id)
}

# generate the donor-receiver pairing for the defined attr scenarios using different algorithms (functions)
scenarios <- names(config$attrs)
scenarios <- scenarios[scenarios!="base"]
funcs <- c("gower_dist","kmeans_clust","pamk_clust")
for (func1 in funcs) {
  for (scenario in scenarios) {
    
    outfile <- paste0("../output/donor_",scenario,"_",func1,".Rdata")
    if (file.exists(outfile)) next
    
    stop()
    message(paste0("\n======== processing donors: ", func1, "  ", scenario," =============\n"))
    dtDonorAll <- do.call(func1,list(config, dtAttrAll,scenario, distSpatial0))
    
    # if receivers and donors share the same hydrofabric, assign donors directly
    if (ver_donor == ver_receiver) {
      dtDonorAll <- subset(dtDonorAll,! id %in% donorsAll)
      dt_donor <- data.table(id=donorsAll, donor=donorsAll, distAttr=0, distSpatial=0, tag="donor")
      dtDonorAll <- rbind(dt_donor,dtDonorAll)
    }
    
    save(dtDonorAll,file=outfile)
    
  } #loop scenario
} # loop func

#dt1 <- copy(dtDonorAll)
#load("output/v0/donor_camels_kge_nse.Rdata")
#dt2 <- merge(dt1,dtDonorAll,by="id")
#dt2[,same:=ifelse(donor.x==donor.y, TRUE,FALSE)]
#subset(dt2,!same)
