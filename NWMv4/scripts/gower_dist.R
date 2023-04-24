# apply Gower distance measure to identify physically similar donors
gower_dist <- function(config, dtAttrAll, attr_scenario, dist_spatial) {
  
  #receiversAll <- dtAttrReceiver$id
  dtDonorAll <- data.table()
  
  # two rounds processing, first with attrs$main, and then with attrs/$base for catchments with no donors found in 1st round
  attrs1 <- list(main=config$attrs[[attr_scenario]], base=config$attrs$base)
  for (run1 in names(attrs1)) { 
    
    # check if donors are identified for all receivers
    recs0 <- subset(dtAttrAll, tag=="receiver")$id
    recs0 <- recs0[!recs0 %in% dtDonorAll$id]
    if (length(recs0)==0) next
    
    # get the attributes to use
    dtAttr0 <- dtAttrAll[,c(config$non_attr_cols,attrs1[[run1]]),with=F]
    
    # iteratively process all the receivers to handle data gaps
    recs1 <- NULL #recerivers that have already been processed in previous rounds
    kround <- 0
    while(sum(recs0 %in% recs1)!=length(recs0)) {
      
      kround <- kround + 1
      message(paste0("\n------------------------",run1, " attributes,  Round ",kround,"--------------------"))
      
      # perform pca to obtain the scores and weights (as well as the attribute table used for PCA)
      pca1 <- apply_pca(recs0, recs1,dtAttr0, attrs1[[run1]])
      
      # donors and receivers for this round
      donorsAll1 <- subset(pca1$dtAttr,tag=="donor")$id
      receiversAll1 <- subset(pca1$dtAttr,tag=="receiver")$id
      
      # compute Gower's distance between donors and receivers only, i.e., avoid calculating distance
      # between donors and donors, receivers and receivers (faster)
      system.time({      
        nd1 <- length(donorsAll1)
        nr1 <- length(receiversAll1)
        rng1 <- sapply(colnames(pca1$scores), function(x) diff(range(pca1$scores[,x])))
        rng2 <- matrix(rep(rng1,nr1), nrow=nr1, byrow=TRUE)
        wgt2 <- matrix(rep(pca1$weights,nr1),nrow=nr1, byrow=TRUE)
        distAttr0 <- sapply(1:nd1, function(x) rowSums(abs(pca1$scores[(nd1+1):nrow(pca1$scores),]-
                                                             matrix(rep(pca1$scores[x,],nr1),nrow=nr1,byrow=TRUE))/rng2*wgt2))
      })     
      
      # determine which receivers to be processed for the current round
      # process only those not-yet processed receivers
      recs <- recs0[(recs0 %in% receiversAll1) & (!recs0 %in% recs1)]
      recs1 <- c(recs1, recs)
      message(paste0(length(recs)," receivers to be processed this round"))      
      
      # loop through all the receivers to be processed
      for (rec1 in recs) {
        
        # find donors within the defined buffer iteratively so that donors with attr distance
        # below the predefined value can be found
        buffer <- config$pars$gower$minSpaDist - 100
        while(buffer < (config$pars$general$maxSpaDist -100)) {
          
          buffer <- buffer + 100
          
          # if there exists donor catchment within 1km, select that catchment as donor
          donors1 <- names(which(dist_spatial[rec1,donorsAll1]<=1))
          if (length(donors1)>0) {
            donors1 <- donors1[which.min(dist_spatial[rec1,donors1])]
          } else {
            # narrow down to donors within the buffer
            donors1 <- names(which(dist_spatial[rec1,donorsAll1]<= buffer))
          }
          
          # potential donors with dist <= config$pars$gower$maxAttrDist
          dist1 <- distAttr0[match(rec1,receiversAll1),]
          dist1 <- dist1[match(donors1,donorsAll1)]
          ix1 <- which(dist1 <= config$pars$gower$maxAttrDist)
          if (length(ix1)==0) next
          
          # if a suitable donor is found, break the loop and stop searching
          if (min(dist1[ix1]) <= config$pars$gower$minAttrDist | length(donors1)==length(donorsAll1)) break
        }
        
        donor1 <- NULL
        if (length(donors1)>0) {
          
          # donors that satisfy the config$pars$gower$maxAttrDist threshold
          dist1 <- dist1[ix1]
          donor1 <- donors1[ix1]
          
          # apply additional donor constraints
          list1 <- apply_donor_constraints(rec1, donor1, dist1, config$pars, dtAttrAll)
          dist1 <- list1$dist; donor1 <- list1$donor
          
          if (length(donor1)>0) {
            
            # choose the donors with the smallest attribute distance
            ix2 <- order(dist1)[1:min(length(dist1),config$pars$gower$nDonorMax)]
            distAttr1 <- dist1[ix2]
            donor1 <- donor1[ix2]
            donor1 <- donor1[!is.na(donor1)]
            
            # donor-receiver spatial distance
            distSpatial1 <- dist_spatial[rec1,donor1]
            
            # choose the donor with the smallest spatial distance (best donor)
            if (length(donor1)>0) {
              ix3 <- which.min(distSpatial1)
              donor_best1 <- donor1[ix3]
              distAttr_best1 <- distAttr1[ix3]
              distSpatial_best1 <- distSpatial1[ix3]
            }}
        }
        
        # if no donors found, get the spatially closest donor in the same HSG
        tag1 <- run1
        if (length(donor1)==0 & run1=="base") {
          
          # get all donors and their spatial distance to the receiver
          donor0 <- subset(dtAttrAll, tag=="donor")$id
          dist0 <- dist_spatial[rec1,]
          
          # apply additional donor constraints
          list1 <- apply_donor_constraints(rec1, donor0, dist0, config$pars, dtAttrAll)
          dist0 <- list1$dist; donor0 <- list1$donor
          
          # choose the donor that is spatially closest
          donor1 <- donor0[which.min(dist0)]
          distSpatial1 <- min(dist0)
          distAttr1 <- distAttr0[match(rec1,receiversAll1),match(donor1,donorsAll1)]
          tag1 <- "proximity"
          
          # best donor (same as donor1)
          donor_best1 <- donor_best1
          distSpatial_best1 <- distSpatial1
          distAttr_best1 <- distAttr1
        }
        
        if (length(donor1)>0) {
          df1 <- data.frame(id=rec1, tag=tag1,
                            donor=donor_best1,distAttr=round(distAttr_best1,3),distSpatial=distSpatial_best1,
                            donors=paste(donor1,collapse=","), 
                            distAttrs=paste(round(distAttr1,3),collapse=","),
                            distSpatials=paste(distSpatial1,collapse=","))
          dtDonorAll <- rbind(dtDonorAll, as.data.table(df1))
        } 
      } # loop receiver
    } # loop round
    
    #kround <- kround + 1
    
  } #loop attrs main + base
  
  # print summaries
  message("\nNumber of receivers in each category:")
  print(table(dtDonorAll$tag))
  message("\nSummary of Gower's distance:")
  message("==== main ====")
  print(summary(subset(dtDonorAll,tag=="main")$distAttr))
  message("==== base ====")
  print(summary(subset(dtDonorAll,tag=="base")$distAttr))
  message("==== proximity ====")
  print(summary(subset(dtDonorAll,tag=="proximity")$distAttr))
  
  return(dtDonorAll)
  
} # function gower