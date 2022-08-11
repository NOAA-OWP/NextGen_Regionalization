# Create donor-receiver pairing using Principal Component Analysis (PCA) and 
#    Gower's distance, using both v21 (HLR) and v3 (CAMELS) regionalization 
#    algorithms
# Multiple rounds are conducted to handle spatial gaps in some attributes, 
#    which have resulted in NA vlaues for some receivers and donors 

rm(list=ls())

library(data.table)
library(rgeos)
library(rgdal)
library(raster)
library(psych)
library(cluster)
library(doParallel)
library(foreach)

# algorithm parameters shared by HLR & CMALES based algorithms
maxBuffer <- 15 # max. buffer (in degrees) around each validation basin to search for potential donors 
buffer1 <- 2 # starting buffer
minAttrDist <- 0.05 #minimum attr distance (if one or more donors with distance smaller than this threshold, stop searching)
maxAttrDist <- 0.1 #maximum attr distance (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
maxSpaDist <- 2500 # maximum spatial distance (km) (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
maxNDonor<- 5  # v3 only: maximum number of donors to retain (to count for donor uncertainty); for v2.1, only a single donor is retained

# parameters for v3 algorithm only
minKGE <- 0.5  # minimum KGE (from cal+val period) for screening out poorly performing donor basins
maxAreaDif <- 100 # maximum acceptable %difference in basin area between that from USGS and area computed from shapefile
minDataYears <- 3 #minimum number of data years required for valid streamflow signatures
ssOnly <- TRUE #for ss basins, whether to use streamflow signatures only in regionalization or use both CAMELS attributes and streamflow signatures

# list of attributes to be used for HLR-based algorithm (ignore prcFlatTotal to avoid singular correction matrix since prcFlatTotal=prcFlatUpland+prcFlatLowland)
attrs_all_v21 <- list(HLR=c("prcFlatLowland","prcFlatUpland","relief","cidx","FMI","sand_frac","clay_frac","soil_depth_pelletier","forest_frac","cropland_frac","urban_frac"))

# list of attributes to be used for CAMELS-based algorithm (note root_depth_99,root_depth_50,stream_elas are not included due to data uncertainty)
if (!ssOnly) {
attrs_all_v3 <- list(CAMELS_SS=c("slope_mean","elev_mean","p_mean","pet_mean","aridity","snow_frac","high_prec_freq","low_prec_freq","high_prec_dur","low_prec_dur","forest_frac","gvf_max","gvf_diff","lai_max","lai_diff","geol_porosity","geol_permeability","sand_frac","clay_frac","soil_porosity","soil_conductivity","soil_depth_statsgo","soil_depth_pelletier","q_mean","runoff_ratio","slope_fdc","baseflow_index","hfd_mean","Q5","Q95","high_q_freq","high_q_dur","low_q_freq","low_q_dur","zero_q_freq"))
} else {
attrs_all_v3 <- list(
SS=c("q_mean","runoff_ratio","slope_fdc","baseflow_index","hfd_mean","Q5","Q95","high_q_freq","high_q_dur","low_q_freq","low_q_dur","zero_q_freq"),
CAMELS=c("slope_mean","elev_mean","p_mean","pet_mean","aridity","snow_frac","high_prec_freq","low_prec_freq","high_prec_dur","low_prec_dur","forest_frac","gvf_max","gvf_diff","lai_max","lai_diff","geol_porosity","geol_permeability","sand_frac","clay_frac","soil_porosity","soil_conductivity","soil_depth_statsgo","soil_depth_pelletier"))
}

attrs_all <- list(HLR=attrs_all_v21, CAMELS=attrs_all_v3)

# collection of all attributes (for donors and receivers)
dtAttrAll <- get(load("../../output/all_attrs.Rdata"))

# columns of the attribute table that are not attributes but needed 
nonAttrCols <- c("ID","domainID","tag","tag1","lat_cent","lon_cent","snowy")

# generate the donor-receiver pairing for both HLR and CAMELS (separately)
#for (run1 in c("HLR","CAMELS")) {
for (run1 in c("CAMELS")) {

# make a copy of the original attribute dataset
dtAttr0 <- copy(dtAttrAll) 

# if HLR, exclude the streamflow signature basins from the receivers
if (run1 == "HLR") dtAttr0 <- subset(dtAttr0, tag!="ss_basins")

# further donor screening for v3
if (run1=="CAMELS") {

# remove basins with inconsistent usgs/shapefile area
dtAttr0 <- subset(dtAttr0, is.na(area_dif_prc) | (area_dif_prc <= maxAreaDif))

# remove donors with kge < minKGE
load("../../data/allStats_FullRouting.Rdata")
df1 <- subset(allStats_FullRouting,evalPeriod=="full")
gages0 <- subset(df1,kge_calib<minKGE)$gage_id
dtAttr0 <- subset(dtAttr0,tag != "donor" | !ID %in% gages0)

# remove basins (donors and ss_basins) with data record too short for valid streamflow signatures
dtAttr0 <- subset(dtAttr0, is.na(data_years) | (!is.na(data_years) & data_years >= minDataYears))
}

# start multiple rounds of processing
dtDonorAll <- data.table()
for (attr1 in names(attrs_all[[run1]])) {

kround <- 1
varsExclude <- NULL 
while (1) {

# attributes to start with 
vars <- attrs_all[[run1]][[attr1]]
vars <- vars[! vars %in% varsExclude]

# determine which attributes to exclude for the current round (by identifying those with the largest number of receivers with NA value)
if (kround==1) {
  vars0 <- NULL
} else {
  nmiss <- NULL 
  for (v1 in vars) nmiss <- c(nmiss,sum(is.na(dtAttr0[[v1]])))
  vars0 <- vars[nmiss==max(nmiss)]
}

vars <- vars[! vars %in% vars0]
if (length(vars)==0) break
varsExclude <- c(varsExclude, vars0)
dtAttr <- dtAttr0[,c(nonAttrCols,vars),with=F]

# ignore donors & receivers with NA attribute values (those will be handled in following rounds with less attributes)
dtAttr <- na.omit(dtAttr)

# donor and receiver IDs
nDonor <- sum(dtAttr$tag1 == "donor")
nReceiver <- sum(dtAttr$tag1 == "hucs")
donorIDs <- subset(dtAttr,tag1 == "donor")$ID
receiverIDs <- subset(dtAttr,tag1 == "hucs")$ID
donorDomainIDs <- subset(dtAttr,tag1 == "donor")$domainID
receiverDomainIDs <- subset(dtAttr,tag1 == "hucs")$domainID

message(paste0("\n------------------------",run1, "  ",attr1,"  Round ",kround,"--------------------"))
if (is.null(vars0)) {
message(paste0("Including all ", attr1," attributes"))
} else {
message(paste0("Excluding ", length(vars0)," ", attr1," attributes: ", paste(vars0,collapse=",")))
}
message(paste0(nReceiver," receivers, ",nDonor," donors, ",length(vars)," attributes"))

# lat/lon of donors
i1 <- match(donorIDs, dtAttr$ID)
lon1 <- dtAttr$lon_cent[i1]
lat1 <- dtAttr$lat_cent[i1]

# reduce table to only attributes that are to be used
dtAttr1 <- dtAttr[,vars, with=FALSE]

# standardize the data 
dtAttr2 <- scale(dtAttr1)

# Conduct PCA to remove correlation between attributes
# first conduct a preliminary PCA with princomp to help decide
# how many principle components to use
pca <- princomp(dtAttr2,cor=TRUE)
pvar <- (pca$sdev)^2/sum((pca$sdev)^2)
pvar <- rbind(pvar,cumsum(pvar))
rownames(pvar) <- c('var','cum_var')
colnames(pvar) <- 1:ncol(dtAttr1)

# decide which PCs to use base on variance explained for each individual PC
# keep all PCs that explain >10% variance; total variance explained should be larger than 80%
nc1 <- max(which(pvar[1,]>=0.1)) #number of PCs to use
if (cumsum(pvar[2,nc1]<0.8)) nc1 <- min(which(pvar[2,]>=0.8))
message(paste0("Number of PCs selected: ", nc1))
message(paste0("PCA total portion of variance explained ... ", cumsum(pvar[2,nc1])))

# conduct refined PCA with "principal" (psych package) given the chosen number of PCs
pca1 <- principal(dtAttr2,nfactors=nc1,rotate="varimax")

# Get the PCA scores for calculating Gower's distance
mydata <- pca1$scores

# weights for chosen PCs proporitonal to the variances they explained
#w1 <- pvar[1,1:nc1]/pvar[2,nc1]
w1 <- pca1$Vaccounted["Proportion Explained",]

# check the loadings corresponding to each PC (if needed)
if (FALSE) {
source("../func/func_plot_loadings.R")
plot_loadings(pca1)
}

# determine which receivers to be processed for the current round
# process only those not-yet processed receivers
if (nrow(dtDonorAll)==0) {
ihucs <- 1:nReceiver
} else {
ihucs <- which(! receiverDomainIDs %in% dtDonorAll$domainID)
}

message(paste0(length(ihucs)," receivers to be processed this round"))
t1 <- table(subset(dtAttr, tag1=="hucs" & domainID %in% receiverDomainIDs[ihucs])$tag)
str1 <- NULL
for (i1 in 1:length(t1)) str1 <- paste0(str1,"  ",names(t1)[i1],"(",t1[[i1]],")")
message(str1)

# prarallel processing all the receivers for the current round
ncores <- 32 # set ncores to 1 to forgo parallelization
cl <- makeForkCluster(ncores)
registerDoParallel(cl)
dtDonor <- foreach (ihuc=ihucs,.combine=rbind) %dopar% {

ID1 <- receiverIDs[ihuc]
domainID1 <- receiverDomainIDs[ihuc]
ix1 <- match(domainID1,dtAttr$domainID)
lat0 <- dtAttr$lat_cent[ix1]
lon0 <- dtAttr$lon_cent[ix1]

# find gages within the defined buffer iteratively so that the attr distance
# is below the predefined value; consider only donors in the same snowy category
buffer2 <- buffer1 - 0.5
while(buffer2 <= maxBuffer) {

buffer2 <- buffer2 + 0.5

ix0 <- which(lon1>=(lon0-buffer2) & lon1<=(lon0+buffer2) & 
             lat1>=(lat0-buffer2) & lat1<=(lat0+buffer2) & 
             dtAttr$snowy[1:nDonor]==dtAttr$snowy[nDonor+ihuc])
gages1 <- donorIDs[ix0]

if (length(gages1) < 1) next

# assemble data (first row is receiver basin, followed by available donor basins)
mydata1 <- mydata[c(nDonor+ihuc,ix0),]

# compute weighted Gower distance between the current receiver and potential donors 
# the daisy function automatically does standardization before calculating distance
dist1 <- as.matrix(daisy(mydata1,metric="gower",weights=w1))

# potential donors with dist <= maxAttrDist
dist2 <- dist1[1,2:nrow(dist1)]
ix1 <- which(dist2 <= maxAttrDist)
if (length(ix1) < 1) next

dist2 <- dist2[ix1]
gages1 <- gages1[ix1]

# if a suitable donor is found, break the loop
if (min(dist2) <= minAttrDist) break
}

# choose the closest donors
ix2 <- order(dist2)[1:min(length(dist2),maxNDonor)]
donor1 <- gages1[ix2]
distAttr1 <- dist2[ix2]

# restrict attr distance to maxAttrDist
ix3 <- which(distAttr1 <= maxAttrDist)
if (length(ix3)==0) ix3 <- which.min(distAttr1)
donor1 <- donor1[ix3]
distAttr1 <- distAttr1[ix3]

# calcualte donor-receiver spatial distance
ix <- match(donor1,dtAttrAll$ID)
distSpatial1 <- round(pointDistance(cbind(dtAttrAll$lon_cent[ix],dtAttrAll$lat_cent[ix]),c(lon0,lat0),lonlat=TRUE)/1000)

# restrict by spatial distance
ix4 <- which(distAttr1 <= maxSpaDist)
if (length(ix4)==0) ix4 <- which.min(distAttr1)
distSpatial1 <- distSpatial1[ix4]
distAttr1 <- distAttr1[ix4]
donor1 <- donor1[ix4]

data.frame(ID=ID1,domainID=domainID1, donor=donor1, distAttr=round(distAttr1,3),
  distSpatial=distSpatial1)

} #loop ihuc 
stopCluster(cl)
dtDonor <- as.data.table(dtDonor)
dtDonorAll <- rbind(dtDonorAll, dtDonor)

kround <- kround + 1
if (nReceiver==sum(dtAttr0$tag1=="hucs")) break

} #loop rounds
} #loop attr category

# for HLR-based algorithm, only keep the spatially closest (instead of most similar) donor
if (run1 == "HLR") {
dtDonor <- dtDonorAll[,.(ID=ID[which.min(distSpatial)],
                       donor=donor[which.min(distSpatial)],
                       distAttr=distAttr[which.min(distSpatial)],
                       distSpatial=min(distSpatial)),by=.(domainID)]
} else {
dtDonor <- copy(dtDonorAll)
}

# add a tag (whether the reciver is huc10, columbia, great lakes, or ss basin)
# note: ID is not unqiue (columbia and greate lakes share the same ID naming convention)
tmp <- subset(dtAttr0, tag!="donor")
dtDonor$tag <- tmp$tag[match(dtDonor$domainID,tmp$domainID)]

# sanity check number of receivers with donors
message("\nTotal number of receivers with donors identified:")
for (s1 in unique(dtDonor$tag)) message(paste0(s1," ", length(unique(subset(dtDonor,tag==s1)$domainID))))

save(dtDonor,file=paste0("../../output/donors_final_",run1,".Rdata"))
}

