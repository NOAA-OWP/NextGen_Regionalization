# Create donor-receiver pairing using Principal Component Analysis (PCA) and 
#    Gower's distance, using different sets of attributes (HLR or CAMELS) 
# Multiple rounds are conducted to handle spatial gaps (NA values) in some attributes 

rm(list=ls())

library(data.table)
library(sf)
#library(rgeos)
#library(rgdal)
#library(raster)
library(psych)
library(cluster)
#library(doParallel)
#library(foreach)

# algorithm parameters
minAttrDist <- 0.05 #minimum attr distance (if one or more donors with distance smaller than this threshold, stop searching)
maxAttrDist <- 0.2 #maximum attr distance (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
minSpaDist <- 500 #starting distance for searching donors
maxSpaDist <- 1500 # maximum spatial distance (km) (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
maxNDonor<- 5 # maximum number of donors to keep that satisfy the attribute distance requirement  
minKGE <- 0.5  # minimum KGE (from cal+val period) for screening out poorly performing donor basins
maxElevDiff <- 1200 # maximum elevation difference between donors and receivers
maxSnowFracDiff <- 0.3 # maximum snow fraction difference between donors and receivers
maxForestFracDiff <- 0.3 # maximum forest fraction difference between donors and receivers
maxUrbanFracDiff <- 0.3 # maximum urban fraction difference between donors and receivers
maxCroplandFracDiff <- 0.3 # maximum crop fraction difference between donors and receivers

# list of attributes to be used for different scenarios 
attrs <- vector("list")
attrs[["base"]] <- c("elev_mean","aridity","forest_frac","sand_frac","geo_porosity")
attrs[["hlr"]] <- c("prcFlatLowland","prcFlatUpland","prcFlatTotal","relief","cidx","aridity","sand_frac","clay_frac",
                    "soil_depth","forest_frac","cropland_frac","urban_frac")
attrs[["camels"]] <- c("slope","elev_mean","p_mean","pet_mean","aridity","snow_frac","high_prec_freq",
                       "low_prec_freq","high_prec_dur","low_prec_dur","forest_frac","gvf_max","gvf_diff",
                       "lai_max","lai_diff","geo_porosity","geo_permeability","sand_frac","clay_frac",
                       "soil_porosity","soil_conductivity","soil_depth")

# primary attributes to use for first-order screening, in order of high to low importance
attrs0 <- c("elev_mean","snow_frac","forest_frac","cropland_frac","urban_frac")
threshs0 <- c(maxElevDiff,maxSnowFracDiff,maxForestFracDiff,maxCroplandFracDiff,maxUrbanFracDiff)

# shapefile of HUC-01 catchments
huc01 <- st_read("../shapefile/catchment_data.geojson") 

# donors: single catchment donors within HUC-01
donors <- st_read("../shapefile/huc01_headwater.shp")

# remove donors with poor calibration/validation performance
donors <- subset(donors,! id %in% c("cat-15200", "cat-18746","cat-33305"))

# compute centroid of HUC-01 catchments
sf::sf_use_s2(FALSE)
cent_huc01 <- st_centroid(huc01)

# compute distance between donors and all receivers (km)
dist_huc01 <- st_distance(cent_huc01[match(donors$id,cent_huc01$id),], cent_huc01)
dist_huc01 <- matrix(round(as.numeric(dist_huc01)/1000),ncol=nrow(donors),byrow = TRUE)

# collection of all attributes (for donors and receivers)
dtAttrAll <- get(load("../output/all_attrs.Rdata"))

# generate the donor-receiver pairing for the defined scenarios
scenarios <- names(attrs)
scenarios <- scenarios[scenarios != "base"]
for (scenario in scenarios) {
  dtDonorAll <- data.table()
  # "base" is used for catchments where no donor is found for the given scenario
  for (run1 in c(scenario,"base")) { 
    
    # check if donors are identified for all receivers
    recs0 <- huc01$id
    recs0 <- recs0[!recs0 %in% c(donors$id, dtDonorAll$id)]
    if (length(recs0)==0) next
    
    # get the attributes to use
    dtAttr0 <- copy(dtAttrAll) 
    dtAttr0 <- dtAttr0[,c("id",attrs[[run1]]),with=F]
    
    # iteratively process all the receivers to handle data gaps
    recs1 <- NULL #record recerivers that have already been processed
    kround <- 0
    while(sum(recs0 %in% recs1)!=length(recs0)) {
      
      kround <- kround + 1
      
      # get the valid attributes for the first receiver to be processed this round
      dt1 <- subset(dtAttr0, id %in% recs0 & (!id %in% recs1))[1,]
      vars <- names(dt1)[!is.na(dt1)]
      dtAttr <- dtAttr0[,vars,with=F]
      vars <- vars[vars != "id"]
      vars0 <- attrs[[run1]][!attrs[[run1]] %in% vars]
      
      if (length(vars)==0) break
      
      # ignore donors & receivers with NA attribute values
      dtAttr <- na.omit(dtAttr)
      
      # determine which receivers to be processed for the current round
      # process only those not-yet processed receivers
      recs <- recs0[(recs0 %in% dtAttr$id) & (!recs0 %in% recs1)]
      recs1 <- c(recs1, recs)
      
      message(paste0("\n------------------------",run1, "  Round ",kround,"--------------------"))
      if (length(vars0)>0) {
        message(paste0("Excluding ", length(vars0)," attributes: ", paste(vars0,collapse=",")))
      } else {
        message("Using all attributes")
      }
      message(paste0(length(recs)," receivers to be processed this round"))
      
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
      
      # decide which PCs to use base on variance explained by each individual PC
      # keep all PCs that explain >10% variance; total variance explained should be larger than 80%
      nc1 <- max(which(pvar[1,]>=0.1)) #number of PCs to use
      if (cumsum(pvar[2,nc1]<0.8)) nc1 <- min(which(pvar[2,]>=0.8))
      message(paste0("Number of PCs selected: ", nc1))
      message(paste0("PCA total portion of variance explained ... ", cumsum(pvar[2,nc1])))
      
      # conduct refined PCA with "principal" (psych package) given the chosen number of PCs
      pca1 <- principal(dtAttr2,nfactors=nc1,rotate="varimax")
      
      # Get the PCA scores for calculating Gower's distance
      mydata <- pca1$scores
      
      # weights for chosen PCs are proporitonal to the variances they explained
      w1 <- pca1$Vaccounted["Proportion Explained",]
      
      # compute the dissimilarity (attribute distance) between all receivers and donors
      dists0 <- as.matrix(daisy(mydata,metric="gower",weights=w1))
      
      # check the loadings corresponding to each PC (if needed)
      if (FALSE) {
        source("func_plot_loadings.R")
        plot_loadings(pca1)
      }
      
      # loop through all the receivers to be processed
      for (rec1 in recs) {
        
        # find donors within the defined buffer iteratively so that the attr distance
        # is below the predefined value
        buffer <- minSpaDist - 100
        while(buffer < (maxSpaDist -100)) {
          
          buffer <- buffer + 100
          
          # narrow down to donors within the buffer
          donors1 <- donors[which(dist_huc01[match(rec1,huc01$id),]<= buffer),]
 
          # further narrow down based on first-order screening with primary attributes
          for (att1 in attrs0) 
            donors1 <- donors1[abs(dtAttrAll[[att1]][match(rec1,dtAttrAll$id)] - dtAttrAll[[att1]][match(donors1$id,dtAttrAll$id)]) < threshs0[match(att1,attrs0)],]
          if (nrow(donors1) < 1) next
                    
          # potential donors with dist <= maxAttrDist
          dist1 <- dists0[match(rec1,dtAttr$id),]
          dist2 <- dist1[match(donors1$id,dtAttr$id)]
          ix1 <- which(dist2 <= maxAttrDist)
          if (length(ix1) < 1) next
          
          # if a suitable donor is found, break the loop and stop searching
          if (min(dist2[ix1]) <= minAttrDist | nrow(donors1)==nrow(donors)) break
        }
        
        # donors that satisfy the maxAttrDist threshold
        dist2 <- dist2[ix1]
        donor1 <- donors1$id[ix1]
        
        # choose donors in the same HSG (hydrologic soil group)
        hsg <- dtAttrAll$hsg[match(rec1,dtAttrAll$id)]
        hsg1 <- dtAttrAll$hsg[match(donor1,dtAttrAll$id)]
        ix1 <- which(hsg1==hsg)
        if (length(ix1)>0) {dist2 <- dist2[ix1]; donor1 <- donor1[ix1]}
        
        # choose the donors with the smallest attribute distance
        ix2 <- order(dist2)[1:min(length(dist2),maxNDonor)]
        distAttr1 <- dist2[ix2]
        donor1 <- donor1[ix2]
        donor1 <- donor1[!is.na(donor1)]
        
        # donor-receiver spatial distance
        distSpatial1 <- dist_huc01[match(rec1,huc01$id),match(donor1,donors$id)]
        
        # choose the donor with the smallest spatial distance
        if (length(donor1)>1) {
          ix3 <- which.min(distSpatial1)
          donor1 <- donor1[ix3]
          distAttr1 <- distAttr1[ix3]
          distSpatial1 <- distSpatial1[ix3]
        }
        
        # if no donors found, get the spatially closest donor in the same HSG
        tag1 <- run1
        if (length(donor1)==0 & run1=="base") {
          
          # get all donors and their spatial distance to the receiver
          donor0 <- donors$id
          dist0 <- dist_huc01[match(rec1,huc01$id),]
          
          # narrow down to those donors within maxSpaDist
          ix1 <- which(dist0 <= maxSpaDist)
          if (length(ix1)>1) {donor0 <- donor0[ix1]; dist0 <- dist0[ix1]}
          
          # further narrow down based on first-order attributes
          for (att1 in attrs0) {
            ix1 <- which(abs(dtAttrAll[[att1]][match(rec1,dtAttrAll$id)] - dtAttrAll[[att1]][match(donors1$id,dtAttrAll$id)]) <= threshs0[match(att1,attrs0)])
            if (length(ix1)>0) {dist0 <- dist0[ix1]; donor0 <- donor0[ix1]}
          }
          
          # further narrow down to donors in the same HSG
          hsg1 <- dtAttrAll$hsg[match(donor0,dtAttrAll$id)]
          ix1 <- which(hsg1==hsg)
          if (length(ix1)>0) {dist0 <- dist0[ix1]; donor0 <- donor0[ix1]}
          
          # choose the donor that is spatially closest
          donor1 <- donor0[which.min(dist0)]
          distSpatial1 <- min(dist0)
          distAttr1 <- dists0[match(rec1,dtAttr$id),match(donor1,dtAttr$id)]
          tag1 <- "proximity"
        }
        
        if (length(donor1)>0) {
          df1 <- data.frame(id=rec1,donor=donor1, distAttr=round(distAttr1,3),distSpatial=distSpatial1,tag=tag1)
          dtDonorAll <- rbind(dtDonorAll, as.data.table(df1))
        } 
      } # loop receiver
    } # loop round
    
    kround <- kround + 1
    
  } #loop scenario
  
  save(dtDonorAll,file=paste0("../output/donor_",scenario,".Rdata"))
  
  # save to csv for porpulating the realization file
  # first add the donor catchments
  dtDonorAll <- rbind(dtDonorAll,data.table(id=donors$id,donor=donors$id,distAttr=0,distSpatial=0,tag="donor"))
  write.csv(dtDonorAll,file=paste0("../output/donor_",scenario,".csv"),quote=FALSE)
}
