# Create donor-receiver pairing based on physical similarity, supplemented with spatial proximity. 
# Currently implemented pairing methods include:
#   1) Similarity metrics:
#        1.1 Gower’s distance
#        1.2 Unsupervised Random Forest (URF)
#   2) Clustering
#        2.1 K-means clustering
#        2.2 K-medoids clustering
#        2.3 Hierarchical Density-Based Spatial Clustering of Applications with noise (HDBSCAN)
#        2.4 Balanced Iterative Reducing & Clustering using Hierarchy (BIRCH). 
#
# Physical similarity is computed from a set of hydroclimatic and physiographic characteristics 
# defined by a conceptual framework. Currently implemented conceptual frameworks include:
#   1) the Hydrologic Landscape Region (HLR)
#   2) the Catchment Attributes and Meteorology for Large-Sample studies (CAMELS)
# 
# Notes:
#   1) All methods except for URF employ Principal Component Analysis (PCA) to 
#       reduce the dimension of the physical similarity problem  
#   2) This version allows donors and receivers to have different hydrofabric files

rm(list=ls())

library(data.table)
library(yaml)

source("apply_pca.R")
source("apply_donor_constraints.R")
source("gower_dist.R")
source("kmeans_clust.R")
source("pamk_clust.R")
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
#funcs <- "pamk_clust"
for (func1 in funcs) {
  for (scenario in scenarios) {
    
    outfile <- paste0("../output/donor_",scenario,"_",func1,".Rdata")
    if (file.exists(outfile)) next
    
    message(paste0("\n======== processing donors: ", func1, "  ", scenario," =============\n"))
    dtDonorAll <- do.call(func1,list(config, dtAttrAll,scenario, distSpatial0))
    
    save(dtDonorAll,file=outfile)
    
  } #loop scenario
} # loop func

#dt1 <- copy(dtDonorAll)
#load("output/v0/donor_camels_kge_nse.Rdata")
#dt2 <- merge(dt1,dtDonorAll,by="id")
#dt2[,same:=ifelse(donor.x==donor.y, TRUE,FALSE)]
#subset(dt2,!same)
