# Create donor-receiver pairing using Principal Component Analysis (PCA) and 
#    Gower's distance, using different sets of attributes (HLR or CAMELS) 
# Multiple rounds are conducted to handle spatial gaps (NA values) in some attributes 
#
# This version allows donors and receivers to have different hydrofabric files

rm(list=ls())

library(data.table)
library(sf)
library(psych)
library(cluster)
library(dplyr)


# algorithm parameters
minAttrDist <- 0.1 #minimum attr distance (if one or more donors with distance smaller than this threshold, stop searching)
maxAttrDist <- 0.3 #maximum attr distance (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
minSpaDist <- 200 #starting distance for searching donors (km)
maxSpaDist <- 1500 # maximum spatial distance (km) (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
maxNDonor<- 10 # maximum number of donors to keep that satisfy the attribute distance requirement 

maxElevDiff <- 1200 # maximum elevation difference between donors and receivers
maxSnowFracDiff <- 0.3 # maximum snow fraction difference between donors and receivers
maxForestFracDiff <- 0.3 # maximum forest fraction difference between donors and receivers
maxUrbanFracDiff <- 0.3 # maximum urban fraction difference between donors and receivers
maxCroplandFracDiff <- 0.3 # maximum crop fraction difference between donors and receivers

# primary attributes to use for initial screening, in order of high to low importance
attrs0 <- c("elev_mean","snow_frac","forest_frac","cropland_frac","urban_frac")
threshs0 <- c(maxElevDiff,maxSnowFracDiff,maxForestFracDiff,maxCroplandFracDiff,maxUrbanFracDiff)

# hydrofabric versions of receivers and donors
ver_receiver <- "v1.2"
ver_donor <- "v1.2"

# calibration gages
gages_calib <- c('01059000','01102345','01041000','01185500','01169000','01053500','01077400','01049550','01082000','01105876','01030500','01186000','01118360','01126500','01208990','01068910','01115500','01173500','01162500','01043500')

# lumped attrs over drainage of gages
dtAttrLump <- get(load("output/v1.2/all_attrs_lumped_gage.Rdata"))
names(dtAttrLump)[names(dtAttrLump)=="gage"] <- "id"

# streamflow signature
dtAttrSS <- get(load("output/v1.2/ss_attr_huc01.Rdata"))
names(dtAttrSS)[names(dtAttrSS)=="ID"] <- "id"

# donor gages and receiver gages
donorsAll <- gages_calib[gages_calib %in% dtAttrSS$id]
receiversAll <- dtAttrSS$id[!dtAttrSS$id %in% donorsAll]

# donor & receiver catchments
cwt_donor <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))
donor_cats0 <- unique(subset(cwt_donor, gages %in% donorsAll)$id)
cwt_receiver <- get(load(paste0("output/",ver_receiver,"/crosswalk_gage_cat_huc01.Rdata")))
receiver_cats0 <- unique(subset(cwt_receiver, gages %in% receiversAll)$id)

# donor attributes
dtAttrDonor <- subset(dtAttrSS, id %in% donorsAll)
dtAttrDonor$tag <- "donor"

# receiver attributes
dtAttrReceiver <- subset(dtAttrSS, id %in% receiversAll)
dtAttrReceiver$tag <- "receiver"

# attributes for both donors and receivers
dtAttrAll <- rbind(dtAttrDonor, dtAttrReceiver)

# compute spatial distance between all receivers and donors if not already exists
f1 <- paste0("output/",ver_receiver,"/dist_spatial_all_ss.Rdata")
if (file.exists(f1)) {
  distSpatial0 <- get(load(f1))
} else {
  
  sf_use_s2(FALSE)
  
  # receiver shapefile
  shps_rec <- st_read(paste0("../datasets/gpkg_",ver_receiver,"/catchment_data.geojson"))
  shps_rec <- merge(shps_rec, cwt_receiver[,c("id","gages"),with=F])
  shps_rec <- subset(shps_rec, gages %in% receiversAll)
  cent_receivers <- shps_rec %>% group_by(gages) %>%
    summarise(geometry = sf::st_union(geometry)) %>% st_centroid()
  cent_receivers[match(receiversAll,cent_receivers$gages),]
  
  # donor shapefile
  shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
  shps_don <- merge(shps_don, cwt_donor[,c("id","gages"),with=F])
  shps_don <- subset(shps_don, gages %in% donorsAll)
  cent_donors <- shps_don %>% group_by(gages) %>%
    summarise(geometry = sf::st_union(geometry)) %>% st_centroid()
  cent_donors[match(donorsAll,cent_donors$gages),]
  
  # compute distance between donors and all receivers (km)
  # row: receivers; column: donors
  distSpatial0 <- st_distance(cent_donors, cent_receivers)
  distSpatial0 <- matrix(round(as.numeric(distSpatial0)/1000),ncol=length(donorsAll),byrow = TRUE)
  row.names(distSpatial0) <- receiversAll
  colnames(distSpatial0) <- donorsAll
  save(distSpatial0, file=f1)
}

# list of attributes to be used for different scenarios 
attrs <- vector("list")
attrs[["ss"]] <- c('q_mean','runoff_ratio','stream_elas','slope_fdc','baseflow_index','hfd_mean','Q5','Q95','high_q_freq','high_q_dur','low_q_freq','low_q_dur','zero_q_freq')

# generate the donor-receiver pairing for the defined scenarios
scenarios <- "ss"
for (scenario in scenarios) {
  
  dtDonorAll <- data.table()

  for (run1 in c(scenario)) { 
    
    # check if donors are identified for all receivers
    recs0 <- receiversAll
    recs0 <- recs0[!recs0 %in% dtDonorAll$id]
    if (length(recs0)==0) next
    
    # get the attributes to use
    dtAttr0 <- copy(dtAttrAll) 
    dtAttr0 <- dtAttr0[,c("id","tag",attrs[[run1]]),with=F]
    
    # iteratively process all the receivers to handle data gaps
    recs1 <- NULL #recerivers that have already been processed in previous rounds
    kround <- 0
    while(sum(recs0 %in% recs1)!=length(recs0)) {
      
      kround <- kround + 1
      
      # get the valid attributes for the first receiver to be processed this round
      dt1 <- subset(dtAttr0, (id %in% recs0) & (!id %in% recs1) & (tag=="receiver"))[1,]
      vars <- names(dt1)[!is.na(dt1)]
      dtAttr <- dtAttr0[,vars,with=F]
      vars <- vars[!vars %in% c("id","tag")]
      vars0 <- attrs[[run1]][!attrs[[run1]] %in% vars]
      
      if (length(vars)==0) break
      
      # ignore donors & receivers with NA attribute values
      dtAttr <- na.omit(dtAttr)
      
      # receivers and donors in current round
      donorsAll1 <- subset(dtAttr,tag=="donor")$id
      receiversAll1 <- subset(dtAttr, tag=="receiver")$id
      
      # determine which receivers to be processed for the current round
      # process only those not-yet processed receivers
      recs <- recs0[(recs0 %in% receiversAll1) & (!recs0 %in% recs1)]
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
      #system.time({
      #distAttr0 <- as.matrix(daisy(mydata,metric="gower",weights=w1))
      #nd1 <- length(donorsAll1)
      #distAttr0 <- distAttr0[(nd1+1):nrow(distAttr0),1:nd1]
      #})

      # compute Gower's distance between donors and receivers only, i.e., avoid calculating distance
      # between donors and donors, receivers and receivers (faster)
      system.time({      
      nd1 <- length(donorsAll1)
      nr1 <- length(receiversAll1)
      rng1 <- sapply(colnames(mydata), function(x) diff(range(mydata[,x])))
      rng2 <- matrix(rep(rng1,nr1), nrow=nr1, byrow=TRUE)
      wgt2 <- matrix(rep(w1,nr1),nrow=nr1, byrow=TRUE)
      distAttr0 <- sapply(1:nd1, function(x) rowSums(abs(mydata[(nd1+1):nrow(mydata),]-
            matrix(rep(mydata[x,],nr1),nrow=nr1,byrow=TRUE))/rng2*wgt2))
      })     
      
      # check the loadings corresponding to each PC (if needed)
      if (FALSE) {
        source("func_plot_loadings.R")
        plot_loadings(pca1)
      }
      
      # loop through all the receivers to be processed
      for (rec1 in recs) {
        
        message(paste0("Round ", kround,": ", rec1," ", match(rec1, recs)))
        
        # find donors within the defined buffer iteratively so that donors with attr distance
        # below the predefined value can be found
        buffer <- minSpaDist - 100
        while(buffer < (maxSpaDist -100)) {
          
          buffer <- buffer + 100
          
          # narrow down to donors within the buffer
          donors1 <- names(which(distSpatial0[rec1,donorsAll1]<= buffer))
 
          # potential donors with dist <= maxAttrDist
          dist1 <- distAttr0[match(rec1,receiversAll1),]
          dist1 <- dist1[match(donors1,donorsAll1)]
          ix1 <- which(dist1 <= maxAttrDist)
          if (length(ix1) < 1) next
          
          # if a suitable donor is found, break the loop and stop searching
          if (min(dist1[ix1]) <= minAttrDist | length(donors1)==length(donorsAll1)) break
        }
        
        donor1 <- NULL
        if (length(donors1)>1) {
          # donors that satisfy the maxAttrDist threshold
          dist1 <- dist1[ix1]
          donor1 <- donors1[ix1]

          # further narrow down based on first-order attributes
          for (att1 in attrs0) {
            ix1 <- which(abs(dtAttrLump[[att1]][match(rec1,dtAttrLump$id)] - 
                               dtAttrLump[[att1]][match(donor1,dtAttrLump$id)]) <= threshs0[match(att1,attrs0)])
            if (length(ix1)>0) {dist1 <- dist1[ix1]; donor1 <- donor1[ix1]}
          }
          
          # choose donors in the same HSG (hydrologic soil group)
          hsg <- dtAttrLump$hsg[match(rec1,dtAttrLump$id)]
          hsg1 <- dtAttrLump$hsg[match(donor1,dtAttrLump$id)]
          ix1 <- which(hsg1==hsg)
          if (length(ix1)>0) {dist1 <- dist1[ix1]; donor1 <- donor1[ix1]}
          
          # choose the donors with the smallest attribute distance
          ix2 <- order(dist1)[1:min(length(dist1),maxNDonor)]
          distAttr1 <- dist1[ix2]
          donor1 <- donor1[ix2]
          donor1 <- donor1[!is.na(donor1)]
          
          # donor-receiver spatial distance
          distSpatial1 <- distSpatial0[rec1,donor1]
          
          # choose the donor with the smallest spatial distance
          if (length(donor1)>1) {
            ix3 <- which.min(distSpatial1)
            donor1 <- donor1[ix3]
            distAttr1 <- distAttr1[ix3]
            distSpatial1 <- distSpatial1[ix3]
          }
        }
        
        # if no donors found, get the spatially closest donor in the same HSG
        tag1 <- run1
        if (length(donor1)==0) {
          
          # get all donors and their spatial distance to the receiver
          donor0 <- donorsAll
          dist0 <- distSpatial0[rec1,]
          
          # narrow down to those donors within maxSpaDist
          ix1 <- which(dist0 <= maxSpaDist)
          if (length(ix1)>1) {donor0 <- donor0[ix1]; dist0 <- dist0[ix1]}
          
          # further narrow down based on first-order attributes
          for (att1 in attrs0) {
            ix1 <- which(abs(dtAttrLump[[att1]][match(rec1,dtAttrLump$id)] - 
                dtAttrLump[[att1]][match(donor0,dtAttrLump$id)]) <= threshs0[match(att1,attrs0)])
            if (length(ix1)>0) {dist0 <- dist0[ix1]; donor0 <- donor0[ix1]}
          }
          
          # further narrow down to donors in the same HSG
          hsg <- dtAttrLump$hsg[match(rec1,dtAttrLump$id)]
          hsg1 <- dtAttrLump$hsg[match(donor0,dtAttrLump$id)]
          ix1 <- which(hsg1==hsg)
          if (length(ix1)>0) {dist0 <- dist0[ix1]; donor0 <- donor0[ix1]}
          
          # choose the donor that is spatially closest
          donor1 <- donor0[which.min(dist0)]
          distSpatial1 <- min(dist0)
          distAttr1 <- distAttr0[match(rec1,receiversAll1),match(donor1,donorsAll1)]
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
  
  save(dtDonorAll,file=paste0("output/",ver_receiver,"/donor_",scenario,".Rdata"))
  
  # save to csv for porpulating the realization file
  #write.csv(dtDonorAll,file=paste0("output/",ver_receiver,"/donor_",scenario,".csv"),quote=FALSE)
}
