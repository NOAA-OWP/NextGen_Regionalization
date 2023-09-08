# apply kmeans clustering approach to identify donors

kmeans_clust <- function(config, dtAttrAll, attr_scenario, dist_spatial) {
  
  #config: algorithm parameters
  #dtAttrAll: attributes table of all donors and receivers
  #attr_scenario: scenario for attributes (e.g., HLR or CAMELS)
  #dist_spatial: spatial distance (1km) between all donors and receivers
  
  require(cluster)
  
  dtDonorAll <- data.table()
  
  # attributes to be used
  attrs1 <- config$attrs[[attr_scenario]]
  
  # check if donors are identified for all receivers
  recs0 <- subset(dtAttrAll, tag=="receiver")$id
  recs0 <- recs0[!recs0 %in% dtDonorAll$id]
  if (length(recs0)==0) next
  
  # get the attributes to use
  dtAttr0 <- copy(dtAttrAll)
  dtAttr0 <- dtAttr0[,c("id","tag","snowy",attrs1),with=F]
  
  # iteratively process all the receivers to handle data gaps
  recs1 <- NULL #recerivers that have already been processed in previous rounds
  kround <- 0
  while(sum(recs0 %in% recs1)!=length(recs0)) {
    
    kround <- kround + 1
    
    message(paste0("\n------------------------ Round ",kround,"--------------------"))
    
    # perform pca to obtain the scores and weights (as well as the attribute table used for PCA)
    pca1 <- apply_pca(recs0, recs1,dtAttr0, attrs1)
    
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
      while(sum(myFlag[(ndonor+1):(ndonor+nreceiver)]==0)>=1) {
        
        # for each cluster that has more than the preset max. number of donors, break it into smaller clusters
        for (i1 in which(dtCluster$N > config$pars$kmeans$nDonorMax)) {
          
          # retrieve all the receivers and donors within the cluster
          idx1 <- which(myCluster == as.integer(dtCluster$V1[i1]))
          
          # if myFlag is 1 for all basins in the cluster, proceed to the next cluster
          if (sum(myFlag[idx1]==0)==0) next
          
          # otherwise retrieve the PCA scores for basins in this cluster
          mydata1 <- scores1[idx1,]
          ndonor1 <- sum(idx1<=ndonor)
          nreceiver1 <- sum(idx1>ndonor)
          
          # determine number of clusters for K-Means Clustering; choose the maximum number
          # of clusters that can ensure 1) each cluster contains at least one donor 
          # and that 2) no group is left with only donors (i.e., no potential receiver
          # is included in the group) 
          # but note the 2nd condition is commented out below
          for (nc in 2:(nrow(mydata1)-1)) {
          #ss1 <- NULL
          #for (nc in 1:15) {
            set.seed(7777)
            fit <- kmeans(mydata1, nc, iter.max=config$pars$kmeans$nIterMax,nstart=config$pars$kmeans$nSubset) # chosen cluster solution
            #ss1 <- c(ss1,fit$tot.withinss)
            #print(paste0("nc=",nc,", withinss=",fit$tot.withinss))
            c0 <- fit$cluster[1:ndonor1]
            c1 <- fit$cluster[(ndonor1+1):(ndonor1+nreceiver1)]
            nrec_nodonor <- sum(!(c1 %in% c0))
            ndon_norec <- sum(!(c0 %in% c1))
            #if (nrec_nodonor >=1 | ndon_norec>=1) break
            if (nrec_nodonor >=1) break
          }
          nc2 <- nc-1 
          #nc2 is the nubmer of sub-clusters we would want to break the current cluster into
          
          #if the cluster cannot be subset further, change myFlag to 1
          if (nc2==1) {
            myFlag[idx1] <- 1 
            next 
          }
          
          # Given the chosen number of clusters, perform a final round of K-mean clustering analysis
          set.seed(7777)
          fit <- kmeans(mydata1, nc2, iter.max=config$pars$kmeans$nIterMax,nstart=config$pars$kmeans$nSubset)
          tmp1 <- fit$cluster
          
          # assign cluster number to the subclusters; first subcluster gets the parent cluster number
          tmp1[fit$cluster==1] <- as.integer(dtCluster$V1[i1])
          # the remaining subclusters become new members of the parent group
          tmp1[fit$cluster>1] <- tmp1[fit$cluster>1]+max(myCluster) - 1
          myCluster[idx1] <- tmp1
          dtCluster <- as.data.table(table(myCluster[1:ndonor])) 
        }
        
        # determine which clusters do not need further clustering 
        # and change the flag to 1; if the number of potentail donors in a cluster is 
        # smaller than the defined max number, then no further clustering is needed
        dtCluster <- as.data.table(table(myCluster[1:ndonor])) 
        i2 <- which(dtCluster$N <= config$pars$kmeans$nDonorMax)
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
        
        # apply additional donor constraints
        list1 <- apply_donor_constraints(rec1, donors1, dists1, config$pars, dtAttrAll)
        dists1 <- list1$dist; donors1 <- list1$donor

        # if the number of donors for a given receiver is greater than nDonorMax, ignore the additional donors
        ix1 <- order(dists1)
        nd_max <- min(length(dists1),config$pars$kmeans$nDonorMax)
        dists1 <- dists1[ix1[1:nd_max]]
        donors1 <- donors1[ix1[1:nd_max]]

        dtDonorAll <- rbind(dtDonorAll, data.table(id=rec1, tag="main",
                                                   donor=donors1[1],distSpatial=dists1[1],
                                                   donors=paste(donors1,collapse=","), 
                                                   distSpatials=paste(dists1,collapse=",")))
      }
    }# snowy or non-snowy
  } # loop round
  
  return(dtDonorAll)
  
} # function kmeans_clust