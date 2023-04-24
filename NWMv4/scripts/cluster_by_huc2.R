# This script collects all the required HLR parameters (that are already derived
# for all HUC10 basins and calibration basins) into a big HLR data table, analyze 
# the correlation among the parameters, conduct principal component analysis, and 
# finally conduct clustering analysis. Here the clustering analysis was conducted
# separately for each HUC2 region. The purpose of clustering is to identify one or
# more donors from the pool of calibration basins for each HUC10 basin (receiver)
# to be used during parameter regionalization, based on similar hydrologic 
# characteristics (HLR parameters). 
#
# This script produces the clusters for each HUC10/calibration basin for
# the full-routing, CONUS configuration
#
# Comments: ***REMOVED***@noaa.gov
#

clusterByHUC2 <- function(hlrParsAll, vars, fileTag) {

nDonorMax <- 5 #number of donor basins in a cluter when no further clustering is needed
buffer1 <- 2 #the buffer zone (in degrees) around each HUC2 region to include potential donors that are outside of the HUC2 region

outfile <- paste0("output/hlr_cluster_",fileTag,".Rdata")
if (file.exists(outfile)) {
  print(paste0("**** File exists: ", outfile,". Exit ..."))
  return(outfile)
}

# load calibration basin metadata (lat/lon, gage ID, domainID etc)
# domainID is an integer used to identify the basin during calibration
Domain_Meta <- get(load('data/Domain_Meta.Rdata')) 

# load list of snowy HUC10s
snowyHucs <- as.character(get(load("data/snownyHUCs.Rdata")))

# get IDs and tags
receiverIDs <- subset(hlrParsAll,region!="donor")$shapeID
donorIDs <- subset(hlrParsAll,region=="donor")$shapeID
nDonor <- length(donorIDs)
nReceiver <- length(receiverIDs)

# lat/lon of gages
i1 <- match(donorIDs, Domain_Meta$ID)
lon1 <- Domain_Meta$longitude[i1]
lat1 <- Domain_Meta$latitude[i1]

# read HUC2 shape files
poly1 <- readOGR("/glade/work/ariz01/data/gis/WBD/WBD.gdb","WBDHU2")

# huc2 region of the calibration basins
donorHUC2 <- get(load("data/huc2IDs_calib_basins_all.Rdata")) 
gage_huc2s <- donorHUC2$huc2[match(donorIDs,donorHUC2$gage)]

# loop through HUC2 regions
# Note clustering analysis is done for each HUC2 region separately here
huc2s <- sprintf("%02d",1:18)
hlrClustersByHUC2 <- data.table()
for (huc2 in huc2s) {
#for (huc2 in "12") {

print(paste0("------------------ HUC2 ", huc2, " ---------------------------------"))

# Identify all the HUC10 basins within the current HUC2 
# these HUC10 basins are the parameter receiver basins
dtHUC10_new <- subset(hlrParsAll, region=="HUC10" & substr(shapeID,1,2) == huc2)

# Identify calibration basins (donors) to be used
#if (huc2 %in% c("15","16","17","18")) {
# Some Western HUC2s require special treatments to have enough calibration basins 
# For Lower Colorado (15), Great Basins (16), Pacific Northwest (17) and 
# California (18), use all calibrations in these HUC2 regions plus those in Missouri (10)
#i2 <- gage_huc2s %in% c("15","16","17","18","10")

#} else {
# For other HUC2 regions, use all calibration basins within the current HUC2 with a 5-degree buffer
ext1 <- extent(poly1[which(poly1$HUC2==huc2),])
ext1[1]<-ext1[1]-buffer1; ext1[2]<-ext1[2]+buffer1
ext1[3]<-ext1[3]-buffer1; ext1[4]<-ext1[4]+buffer1
i2 <- lon1>=ext1[1] & lon1<=ext1[2] & lat1>=ext1[3] & lat1<=ext1[4]
#}
dtGage_new <- subset(hlrParsAll,region=="donor")[i2,]

# break the HUC10s and gages into two groups: snowy and non-snowy
dtHUC10_new1 <- subset(dtHUC10_new, shapeID %in% as.character(snowyHucs))
dtHUC10_new2 <- subset(dtHUC10_new, ! shapeID %in% as.character(snowyHucs))
dtGage_new1 <- subset(dtGage_new, Domain_Meta$snowy[match(shapeID, Domain_Meta$ID)]==1)
dtGage_new2 <- subset(dtGage_new, Domain_Meta$snowy[match(shapeID, Domain_Meta$ID)]==0)

if (huc2=="04") dtHUC10_new1 <- rbind(dtHUC10_new1, subset(hlrParsAll, region=="GreatLakes"))
if (huc2=="17") dtHUC10_new1 <- rbind(dtHUC10_new1, subset(hlrParsAll, region=="Columbia"))

listHUC10 <- list(dtHUC10_new1, dtHUC10_new2)
listGage <- list(dtGage_new1, dtGage_new2)

# run PCA and clustering for snowy and non-snowy basins separately
strs <- c("snowy","non-snowy")
for (ii in 1:2) {

print(strs[ii])

# combine huc10 and gages into one data table for clustering 
hlrPars = rbind(listHUC10[[ii]], listGage[[ii]])

# remove rows containg NA
hlrPars <- na.omit(hlrPars)

n1 <- sum(hlrPars$region %in% c("HUC10","Columniba","GreatLakes"))
if (n1==0) next
if (n1>0 & sum(hlrPars$region=="donor")==0) {
  print(paste0("WARNINF: no gages available to serve as donors for ", n1," basins"))
  next
} 

if (nrow(hlrPars) <= length(vars)) {
  print("WARNIGN: not enough units")
  next
}

# total number of huc10 and gages
nhuc0 <- sum(hlrPars$region %in% c("HUC10","Columbia","GreatLakes"))
print(paste0(nhuc0," HUC10 polygons"))
ngage0 <- sum(hlrPars$region=="donor")
print(paste0(ngage0," calibrated gages"))

hlrPars1 <- hlrPars[,vars,with=F]

# standardize the parameters (mean=0, variance=1) so that the Principal Component
# Analysis (PCA) can be applied properly 
hlrPars2 <- scale(hlrPars1)

# Conduct PCA to remove correlation between the HLR parameters
# first conduct a preliminary PCA with princomp to help decide
# how many principle components to use 
pca <- princomp(hlrPars2,cor=TRUE)
pvar <- (pca$sdev)^2/sum((pca$sdev)^2)
pvar <- rbind(pvar,cumsum(pvar))
rownames(pvar) <- c('var','cum_var')
colnames(pvar) <- 1:ncol(hlrPars1)

# decide which PCs to use; variance explained for each individual PC 
nc1 <- max(which(pvar[1,]>=0.1)) #number of PCs to use
if (cumsum(pvar[2,nc1]<0.8)) nc1 <- min(which(pvar[2,]>=0.8))
print(paste0("Number of PCs selected: ", nc1))
print(paste0("PCA total portion of variance explained ... ", cumsum(pvar[2,nc1])))

# conduct refined PCA with "principal" (psych package) given the chosen
# number of PCs
pca1 <- principal(hlrPars2,nfactors=nc1,rotate="varimax")

# Get the PCA scores for clustering 
mydata <- pca1$scores

# cluster number of each polygon (huc10 or calibration basin)
# initlize all of them with 1 (i.e., start with one big group)
myCluster <- rep(1,nhuc0+ngage0)

# table to count the number of gages in each cluster
dtCluster <- as.data.table(table(myCluster[(nhuc0+1):(nhuc0+ngage0)])) 

# flag to indicate whether clustering is completely done for a group (i.e., no further grouping is possible)
# initialize to 0
myFlag <- rep(0,nhuc0+ngage0) 

# while there exists huc10 basins that require further clustering
# proceed with clustering, iteratively
while(sum(myFlag[1:nhuc0]==0)>=1) {

# for each cluster that has more than the preset max. number of gages
for (i1 in which(dtCluster$N > nDonorMax)) {

# retrieve all the huc10 basins and gages within the cluster
idx1 <- which(myCluster == as.integer(dtCluster$V1[i1]))

# if myFlag is 1 for all basins in the cluster, proceed to the next cluster
if (sum(myFlag[idx1]==0)==0) next

# otherwise retrieve the HLR PCA scores for basins in this cluster
mydata1 <- mydata[idx1,]
nhuc1 <- sum(idx1<=nhuc0)
ngage1 <- sum(idx1>nhuc0)

# determine number of clusters for K-Means Clustering; choose the maximum number
# of clusters that can ensure each cluster contains at least one calibration basin 
# and that no group is left with only calibration basins (i.e., no potential receiver
# is included in the group)
for (nc in 2:(nrow(mydata1)-1)){
set.seed(42)
fit <- kmeans(mydata1, nc) # chosen cluster solution
c0 <- fit$cluster[1:nhuc1]
c1 <- fit$cluster[(nhuc1+1):(nhuc1+ngage1)]
nhuc_nogage <- sum(!(c0 %in% c1))
ngage_nohuc <- sum(!(c1 %in% c0))
if (nhuc_nogage >=1 | ngage_nohuc>=1) break
}
nc2 <- nc-1 
#nc2 is the nubmer of sub-clusters we would want to break the current cluster into

#if the cluster cannot be subset further, change myFlag to 1
if (nc2==1) {
  myFlag[idx1] <- 1 
  next 
}

# Given the chosen number of clusters, perform a final round of K-mean clusterng analysis
set.seed(42)
fit <- kmeans(mydata1, nc2)
tmp1 <- fit$cluster

# assign cluster number to the subclusters
# first subcluster 1 get the parent cluster number
tmp1[fit$cluster==1] <- as.integer(dtCluster$V1[i1])
# the remaining subclusters become new members of the parent group
tmp1[fit$cluster>1] <- tmp1[fit$cluster>1]+max(myCluster) - 1
myCluster[idx1] <- tmp1
}

# determine which clusters do not need further clustering
# and change the flag to 1; if the number of potentail donors in a cluster is 
# smaller than the defined max number, then no further clustering is needed
dtCluster <- as.data.table(table(myCluster[(nhuc0+1):(nhuc0+ngage0)])) 
i2 <- which(dtCluster$N <= nDonorMax)
idx0 <- myCluster %in% as.integer(dtCluster$V1[i2])
myFlag[idx0] <- 1

} #while loop

# combine the results from different HUC2 groups
runStr <- "snowy"
if (ii==2) runStr <- "non-snowy"
hlrClustersByHUC2 <- rbind(hlrClustersByHUC2, cbind(hlrPars,data.table(cluster=myCluster,run=runStr,HUC2=huc2)))
} #snowy loop
} #huc2 loop

# save the results
save(hlrClustersByHUC2, file=outfile)

return(outfile)
}
