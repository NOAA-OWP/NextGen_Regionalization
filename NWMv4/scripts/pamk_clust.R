# apply pamk-based clustering approach to identify donors
pamk_clust <- function(config, dtAttrAll, attr_scenario, dist_spatial) {
  
  #config: algorithm parameters
  #dtAttrAll: attributes table of all donors and receivers
  #attr_scenario: scenario for attributes (e.g., HLR or CAMELS)
  #dist_spatial: spatial distance (1km) between all donors and receivers
  
  require(fpc)
  
  dtDonorAll <- data.table()

  # two rounds processing, first with attrs$main, and then with attrs$base for catchments with no donors found in 1st round
  attrs1 <- list(main=config$attrs[[attr_scenario]], base=config$attrs$base)
  for (run1 in names(attrs1)) { 
  
  # check if donors are identified for all receivers
  recs0 <- subset(dtAttrAll, tag=="receiver")$id
  recs0 <- recs0[!recs0 %in% dtDonorAll$id]
  if (length(recs0)==0) next
  
  # get the attributes to use
  dtAttr0 <- copy(dtAttrAll)
  dtAttr0 <- dtAttr0[,c("id","tag","snowy",attrs1[[run1]]),with=F]
  
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
    
    # determine which receivers to be processed for the current round
    # process only those not-yet processed receivers
    recs <- recs0[(recs0 %in% receiversAll1) & (!recs0 %in% recs1)]
    recs1 <- c(recs1, recs)
    message(paste0(length(recs)," receivers to be processed this round"))   
    
    # process snowy and non-snowy catchments sparately
    for (snow1 in unique(pca1$dtAttr$snowy)) {
      
      message(paste0("======= ",ifelse(snow1,"snowy","non-snowy")," basins ========"))
      
      # the current snowy group
      dtAttr1 <- subset(pca1$dtAttr, snowy==snow1)
      donors <- subset(dtAttr1,tag=="donor")$id
      receivers <- subset(dtAttr1,tag=="receiver")$id
      receivers <- receivers[receivers %in% recs]
      ndonor <- length(donors)
      nreceiver <- length(receivers)
      if (nreceiver==0) next
      
      # keep only donors and and receiver for this round
      idx0 <- c(1:ndonor,ndonor + which(subset(dtAttr1,tag=="receiver")$id %in% receivers))
      dtAttr1 <- dtAttr1[idx0,]
      scores1 <- pca1$scores[pca1$dtAttr$snowy==snow1,]
      scores1 <- scores1[idx0,]
      
      # initialize cluster number of all catchments to 1 (i.e., start with one big group)
      myCluster <- rep(1,nrow(dtAttr1))
      
      # table to count the number of donors in each cluster
      dtCluster <- as.data.table(table(myCluster[1:ndonor])) 
      
      # flag to indicate whether clustering is completely done for a group (i.e., no further grouping is possible)
      # initialize to 0
      myFlag <- rep(0,ndonor+nreceiver) 
      
      # while there exist receiver catchments that require further clustering
      # proceed with clustering, iteratively
      #kk <- 0
      while(sum(myFlag[(ndonor+1):(ndonor+nreceiver)]==0)>=1) {
        #kk <- kk + 1
        # for each cluster that has more than the preset max. number of donors, break it into smaller clusters
        clusts <- which(dtCluster$N > config$pars$pamk$nDonorMax)
        if (length(clusts)==0) break
        for (i1 in clusts) {
          
          # retrieve all the receivers and donors within the cluster
          idx1 <- which(myCluster == as.integer(dtCluster$V1[i1]))
          
          # if myFlag is 1 for all basins in the cluster, proceed to the next cluster
          if (sum(myFlag[idx1]==0)==0) next
          
          # otherwise retrieve the PCA scores for basins in this cluster
          mydata1 <- scores1[idx1,]
          ndonor1 <- sum(idx1<=ndonor)
          nreceiver1 <- sum(idx1>ndonor)
          
          # perform pamk clustering (use "multisaw" for large datasets and "asw" for small datasets)
          if (nrow(mydata1)>config$pars$pamk$nClusterMax * config$pars$pamk$nSubset) {
            fit <- pamk(mydata1,krange = 2:config$pars$pamk$nClusterMax,
                    criterion = "multiasw",usepam = FALSE,
                    ns=config$pars$pamk$nSubset,critout = FALSE,seed=7777)
          } else {
            ncmax <- min(nrow(mydata1)-1,config$pars$pamk$nClusterMax)
            fit <- pamk(mydata1,krange = 2:ncmax,criterion="asw",usepam=TRUE,
                    ns=config$pars$pamk$nSubset,critout = FALSE,seed=7777)
          }
          clusters0 <- fit$pamobject$clustering
          clusters <- clusters0

          # if any resulting cluster has no donors, abort the clustering
          # clusts1 <- as.numeric(names(table(clusters[which(idx1<ndonor)]))) #cluster numbers of donors
          # if (sum(! (unique(clusters) %in% clusts1)) >= 1) next

          # assign cluster number to the subclusters;first subcluster gets the parent cluster number
          clusters[clusters0==1] <- as.integer(dtCluster$V1[i1])
          # the remaining subclusters become new members of the parent group
          clusters[clusters0>1] <- clusters[clusters0>1]+max(myCluster) - 1
          myCluster[idx1] <- clusters
          #print(paste0("kk=",kk,"   i1=",i1))
          #print(table(myCluster))
          dtCluster <- as.data.table(table(myCluster[1:ndonor])) 
        }
        
        # determine which clusters do not need further clustering
        # and change the flag to 1; if the number of potentail donors in a cluster is 
        # smaller than the defined max number, then no further clustering is needed
        dtCluster <- as.data.table(table(myCluster[1:ndonor])) 
        i2 <- which(dtCluster$N <= config$pars$pamk$nDonorMax)
        idx0 <- myCluster %in% as.integer(dtCluster$V1[i2])
        myFlag[idx0] <- 1
        
      } #while loop    
      
      ###### assign donors based on clusters and spatial distance and apply additional constrains
      for (rec1 in recs) {
        i1 <- match(rec1,subset(dtAttr1,tag=="receiver")$id)
        if (is.na(i1)) next
        c1 <- myCluster[i1+ndonor]
        donors1 <- donors[myCluster[1:ndonor]==c1]
        dists1 <- dist_spatial[rec1,donors1]
        if (length(donors1)==0) next

        # apply additional donor constraints
        list1 <- apply_donor_constraints(rec1, donors1, dists1, config$pars, dtAttrAll)
        dists1 <- list1$dist; donors1 <- list1$donor
        if (length(donors1)==0) next

        # add donor/receiver pair to donor table
        dtDonorAll <- rbind(dtDonorAll, data.table(id=rec1, tag=run1,
                                                   donor=donors1[which.min(dists1)],distSpatial=min(dists1),
                                                   donors=paste(donors1,collapse=","), 
                                                   distSpatials=paste(dists1,collapse=",")))
      }
    }# snowy or non-snowy
  } # loop round to deal with missing attributes for some catchments
  } # main or base attrs
  
  # if no donors found (after main & base attrs rounds), get the spatially closest donor with constraints applied
  recs0 <- subset(dtAttrAll, tag=="receiver")$id
  recs0 <- recs0[!recs0 %in% dtDonorAll$id]
  if (length(recs0)>0) {
    for (rec1 in recs0) {
      # get all donors and their spatial distance to the receiver
      donor0 <- subset(dtAttrAll, tag=="donor")$id
      dist0 <- dist_spatial[rec1,]
      
      # apply additional donor constraints
      list1 <- apply_donor_constraints(rec1, donor0, dist0, config$pars, dtAttrAll)
      dist0 <- list1$dist; donor0 <- list1$donor
      
      # choose the donor that is spatially closest
      donor1 <- donor0[which.min(dist0)]
      distSpatial1 <- min(dist0)
      dtDonorAll <- rbind(dtDonorAll, data.table(id=rec1, tag="proximity",
                                                donor=donor1,distSpatial=distSpatial1,
                                                donors=donor1,distSpatials=distSpatial1))      

    }
  }
  return(dtDonorAll)
  
} # function pamk_clust