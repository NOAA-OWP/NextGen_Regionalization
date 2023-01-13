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

calib_scenario <- "kge_nse"

# algorithm parameters
minAttrDist <- 0.1 #minimum attr distance (if one or more donors with distance smaller than this threshold, stop searching)
maxAttrDist <- 0.25 #maximum attr distance (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
minSpaDist <- 200 #starting distance for searching donors (km)
maxSpaDist <- 1500 # maximum spatial distance (km) (donors with distance larger than this value are discarded, unless no donor smaller than this threshold is available)
maxNDonor<- 10 # maximum number of donors to keep that satisfy the attribute distance requirement 

maxElevDiff <- 1200 # maximum elevation difference between donors and receivers
maxSnowFracDiff <- 0.3 # maximum snow fraction difference between donors and receivers
maxForestFracDiff <- 0.5 # maximum forest fraction difference between donors and receivers
maxUrbanFracDiff <- 0.5 # maximum urban fraction difference between donors and receivers
maxCroplandFracDiff <- 0.5 # maximum crop fraction difference between donors and receivers

# list of attributes to be used for different scenarios 
attrs <- vector("list")
attrs[["base"]] <- c("elev_mean","aridity","forest_frac","sand_frac","geo_porosity")
attrs[["hlr"]] <- c("prcFlatLowland","prcFlatUpland","prcFlatTotal","relief","cidx","aridity","sand_frac",
                    "clay_frac","soil_depth","forest_frac","cropland_frac","urban_frac")
attrs[["camels"]] <- c("slope","elev_mean","p_mean","pet_mean","aridity","snow_frac","high_prec_freq",
                       "low_prec_freq","high_prec_dur","low_prec_dur","forest_frac","gvf_max","gvf_diff",
                       "lai_max","lai_diff","geo_porosity","geo_permeability","sand_frac","clay_frac",
                       "soil_porosity","soil_conductivity","soil_depth")

# primary attributes to use for initial screening, in order of high to low importance
attrs0 <- c("elev_mean","snow_frac","cropland_frac","urban_frac","forest_frac")
threshs0 <- c(maxElevDiff,maxSnowFracDiff,maxCroplandFracDiff,maxUrbanFracDiff,maxForestFracDiff)

# hydrofabric versions of receivers and donors
ver_receiver <- "v0"
ver_donor <- "v1.2"

# donors
gages_calib <- get(load(paste0("output/",ver_receiver,"/calib_gages_screened_",calib_scenario,".Rdata")))
cwt <- get(load(paste0("output/",ver_donor,"/crosswalk_gage_cat_huc01.Rdata")))
donorsAll <- unique(subset(cwt, gages %in% gages_calib)$id)

# donor attributes
dtAttrDonor <- get(load(paste0("output/",ver_donor,"/all_attrs.Rdata")))
dtAttrDonor <- subset(dtAttrDonor, id %in% donorsAll)
dtAttrDonor$tag <- "donor"

# receiver attributes
dtAttrReceiver <- get(load(paste0("output/",ver_receiver,"/all_attrs.Rdata")))
dtAttrReceiver$tag <- "receiver"

# receivers
receiversAll <- dtAttrReceiver$id
if (ver_donor == ver_receiver) receiversAll <- receiversAll[!receiversAll %in% donorsAll]
dtAttrReceiver <- subset(dtAttrReceiver, id %in% receiversAll)
receiversAll <- dtAttrReceiver$id

# attributes for both donors and receivers
dtAttrAll <- rbind(dtAttrDonor, dtAttrReceiver)

# compute spatial distance between all receivers and donors if not already exists
f1 <- paste0("output/",ver_receiver,"/dist_spatial_all_",calib_scenario,".Rdata")
if (file.exists(f1)) {
  distSpatial0 <- get(load(f1))
} else {
  
  # receiver shapefile
  shps_rec <- st_read(paste0("../datasets/gpkg_",ver_receiver,"/catchment_data.geojson"))
  ix1 <- match(receiversAll, shps_rec$id)
  shps_rec <- shps_rec[ix1,]
  
  # donor shapefile
  shps_don <- st_read(paste0("../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
  ix1 <- match(donorsAll, shps_don$id)
  shps_don <- shps_don[ix1,]  

  # compute centroids
  sf_use_s2(FALSE)
  cent_receivers <- st_centroid(shps_rec)
  cent_donors <- st_centroid(shps_don)

  # compute distance between donors and all receivers (km)
  # row: receivers; column: donors
  distSpatial0 <- st_distance(cent_donors, cent_receivers)
  distSpatial0 <- matrix(round(as.numeric(distSpatial0)/1000),ncol=length(donorsAll),byrow = TRUE)
  row.names(distSpatial0) <- receiversAll
  colnames(distSpatial0) <- donorsAll
  save(distSpatial0, file=f1)
}

# generate the donor-receiver pairing for the defined scenarios
scenarios <- names(attrs)
scenarios <- scenarios[scenarios != "base"]
for (scenario in scenarios) {
 
  dtDonorAll <- data.table()

  # if receivers and donors share the same hydrofabric, assign donors directly
  if (ver_donor == ver_receiver) 
    dtDonorAll <- data.table(id=donorsAll, donor=donorsAll, distAttr=0, distSpatial=0, tag="donor")
  
  outfile <- paste0("output/",ver_receiver,"/donor_",scenario,"_",calib_scenario,".Rdata")
  if (file.exists(outfile)) dtDonorAll <- get(load(outfile))

  # "base" is used for catchments where no donor is found for the given scenario
  for (run1 in c(scenario,"base")) { 
    
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
        
        #if (rec1=="cat-26665") stop()

        # find donors within the defined buffer iteratively so that donors with attr distance
        # below the predefined value can be found
        buffer <- minSpaDist - 100
        while(buffer < (maxSpaDist -100)) {
          
          buffer <- buffer + 100
          
          # if there exists donor catchment within 1km, select that catchment as donor
          donors1 <- names(which(distSpatial0[rec1,donorsAll1]<=1))
          if (length(donors1)>0) {
            donors1 <- donors1[which.min(distSpatial0[rec1,donors1])]
          } else {
            # narrow down to donors within the buffer
            donors1 <- names(which(distSpatial0[rec1,donorsAll1]<= buffer))
          }
          

          # potential donors with dist <= maxAttrDist
          dist1 <- distAttr0[match(rec1,receiversAll1),]
          dist1 <- dist1[match(donors1,donorsAll1)]
          ix1 <- which(dist1 <= maxAttrDist)
          if (length(ix1)==0) next
          
          # if a suitable donor is found, break the loop and stop searching
          if (min(dist1[ix1]) <= minAttrDist | length(donors1)==length(donorsAll1)) break
        }
        
        donor1 <- NULL
        if (length(donors1)>0) {
          
          # donors that satisfy the maxAttrDist threshold
          dist1 <- dist1[ix1]
          donor1 <- donors1[ix1]
          
          # narrow down based on first-order screening with primary attributes
          for (att1 in attrs0) {
            ix1 <- which(abs(dtAttrReceiver[[att1]][match(rec1,dtAttrReceiver$id)] - 
                 dtAttrDonor[[att1]][match(donor1,dtAttrDonor$id)]) <= threshs0[match(att1,attrs0)])
            donor1 <- donor1[ix1]; dist1 <- dist1[ix1]
          }
          
          if (length(donor1)>0) {
          # choose donors in the same HSG (hydrologic soil group)
          hsg <- dtAttrReceiver$hsg[match(rec1,dtAttrReceiver$id)]
          hsg1 <- dtAttrDonor$hsg[match(donor1,dtAttrDonor$id)]
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
          if (length(donor1)>0) {
            ix3 <- which.min(distSpatial1)
            donor1 <- donor1[ix3]
            distAttr1 <- distAttr1[ix3]
            distSpatial1 <- distSpatial1[ix3]
          }}
        }
        
        # if no donors found, get the spatially closest donor in the same HSG
        tag1 <- run1
        if (length(donor1)==0 & run1=="base") {
          
          # get all donors and their spatial distance to the receiver
          donor0 <- donorsAll
          dist0 <- distSpatial0[rec1,]
          
          # narrow down to those donors within maxSpaDist
          ix1 <- which(dist0 <= maxSpaDist)
          if (length(ix1)>0) {donor0 <- donor0[ix1]; dist0 <- dist0[ix1]}
          
          # further narrow down based on first-order attributes
          for (att1 in attrs0) {
            ix1 <- which(abs(dtAttrReceiver[[att1]][match(rec1,dtAttrReceiver$id)] - 
                dtAttrDonor[[att1]][match(donor0,dtAttrDonor$id)]) <= threshs0[match(att1,attrs0)])
            if (length(ix1)>0) {dist0 <- dist0[ix1]; donor0 <- donor0[ix1]}
          }
          
          # further narrow down to donors in the same HSG
          hsg <- dtAttrReceiver$hsg[match(rec1,dtAttrReceiver$id)]
          hsg1 <- dtAttrDonor$hsg[match(donor0,dtAttrDonor$id)]
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
    
  } #loop scenario + base
  
  save(dtDonorAll,file=outfile)
  
  # print summaries
  print(table(dtDonorAll$tag))
  print(summary(subset(dtDonorAll,tag==scenario)$distAttr))
  print(summary(subset(dtDonorAll,tag=="base")$distAttr))
  print(summary(subset(dtDonorAll,tag=="proximity")$distAttr))

  # save to csv for porpulating the realization file
  #write.csv(dtDonorAll,file=paste0("output/",ver_receiver,"/donor_",scenario,".csv"),quote=FALSE)
} #loop scenario
