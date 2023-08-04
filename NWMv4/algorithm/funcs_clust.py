# This function performs donor-receiver pairing based on clustering using   
#   k-means clustering (method = "kmeans") 
#   k-medoids clustering (method = "kmedoids")
#   DBSCAN (method = "dbscan") - Density-Based Spatial Clustering of Applications with Noise. 
#      Finds core samples of high density and expands clusters from them. Good for data which contains clusters of similar density.
def func(config, dtAttrAll,scenario, dist_spatial, method):
    
    print("perform clustering using " + str(method) + " approach ...")

    import pandas as pd
    import numpy as np
    #import apply_donor_constraints
    from sklearn.cluster import KMeans, DBSCAN, Birch
    from sklearn_extra.cluster import KMedoids
    import math
    import hdbscan

    import my_utils

    dtDonorAll = pd.DataFrame()    

    # attributes to be used
    attrs1 = config['attrs'][scenario]

    # all receivers to find donors for
    recs0 = dtAttrAll.query("tag=='receiver'")['id'].values
    #if dtDonorAll.shape[1] > 0:
    #    recs0 = [value for value in recs0 if value not in dtDonorAll["id"].values]

    # reduce the attribute table to attributes
    dtAttr0 = dtAttrAll[config['non_attr_cols']+attrs1]

    # iteratively process all the receivers to handle data gaps
    recs1 = list() #recerivers that have already been processed in previous rounds
    kround = 0
    while len([x for x in recs0 if x in recs1]) != len(recs0):
    
        kround = kround + 1   
        print("\n------------------------" + scenario + " attributes,  Round " + str(kround) + "--------------------")

        # figure out valid attributes to use this round
        dtAttr = my_utils.get_valid_attrs(recs0, recs1, dtAttr0, attrs1, config)

        # apply principal component analysis
        myscores, weights = my_utils.apply_pca(dtAttr.drop(config['non_attr_cols'], axis=1))    
                
        # donors and receivers for this round
        #donorsAll1 = dtAttr.query("tag=='donor'")['id'].tolist()
        receiversAll1 = dtAttr.query("tag=='receiver'")['id'].tolist()

        # determine which receivers to be processed for the current round
        # process only those not-yet processed receivers
        recs = [r1 for r1 in recs0 if r1 in receiversAll1 and r1 not in recs1]
        recs1 = recs1 + recs
        print(str(len(recs)) + " receivers to be processed this round")  

        # process snowy and non-snowy catchments sparately
        for snow1 in np.unique(dtAttr['snowy']):
      
            str1 = "non-snowy"
            if snow1:
                str1 = "snowy"
            print("======= ",str1," basins ========")

            # the current snowy group
            dtAttr1 = dtAttr.query("snowy==@snow1")
            donors = dtAttr1.query("tag=='donor'")['id'].tolist()
            receivers = dtAttr1.query("tag=='receiver'")['id'].tolist()
            receivers = [x for x in receivers if x in recs]
            ndonor = len(donors)
            nreceiver = len(receivers)  

            # keep only donors and and receivers for this round
            keep1 = (dtAttr1.tag=="donor") | ((dtAttr1.tag=="receiver") & (dtAttr1.id.isin(receivers)))    
            dtAttr1 = dtAttr1[keep1.values]     
            scores1 = myscores[(dtAttr['snowy']==snow1).values][keep1.values]
                    
            # initialize cluster number of all catchments to 1 (i.e., start with one big group)
            myCluster = np.ones(dtAttr1.shape[0])

            # dictionary to count the number of donors in each cluster
            unique, counts = np.unique(myCluster[:ndonor], return_counts=True)
            dtCluster = dict(zip(unique, counts)) 

            # flag to indicate whether clustering is completely done for a group (i.e., no further grouping is possible)
            # initialize to 0
            myFlag = np.zeros(ndonor+nreceiver) 

            # while there exist receiver catchments that require further clustering
            # proceed with clustering, iteratively
            #while sum(myFlag[(ndonor):(ndonor+nreceiver-1)]==0) >= 1:
            while sum(myFlag[-nreceiver:]==0) >= 1:
                # for each cluster that has more than the preset max. number of donors, break it into smaller clusters
                clusts = [k for k, v in dtCluster.items() if v > config['pars'][method]['nDonorMax']]
                for i1 in clusts:
                    #print("i1=" + str(i1))
                    # retrieve all the receivers and donors within the cluster
                    idx1 = np.where(myCluster == i1)

                    # if myFlag is 1 for all basins in the cluster, proceed to the next cluster
                    if sum(myFlag[idx1]==0)==0:
                        continue

                    # otherwise retrieve the PCA scores for basins in this cluster
                    mydata1 = scores1.iloc[idx1]
                    ndonor1 = (np.array(idx1)<=(ndonor-1)).sum()
                    nreceiver1 = (np.array(idx1)>=ndonor).sum()
                    
                    # determine number of clusters for K-Means/K-medoids Clustering; choose the maximum number
                    # of clusters that can ensure 1) each cluster contains at least one donor 
                    # and that 2) no group is left with only donors (i.e., no potential receiver
                    # is included in the group) 
                    # but note the 2nd condition is commented out below
                    if (method in ['kmeans','kemdoids']):
                        for nc in range(2,(mydata1.shape[0])):
                            if method == 'kmeans':
                                fit1 = KMeans(init=config['pars'][method]['init'], n_clusters=nc, n_init=config['pars'][method]['nInit'], max_iter=config['pars'][method]['nIterMax'], random_state=42).fit(mydata1)
                            elif method == 'kmedoids':
                                fit1 = KMedoids(init=config['pars'][method]['init'], n_clusters=nc, max_iter=config['pars'][method]['nIterMax'], random_state=42).fit(mydata1)    
                                                    
                            c0 = fit1.labels_[:ndonor1]
                            c1 = fit1.labels_[ndonor1:(ndonor1+nreceiver1)] 
                            nrec_nodonor = len(c1) - np.isin(c1,c0).sum()    
                            #ndon_norec = len(c0) - np.isin(c0,c1).sum()                     

                            if nrec_nodonor >= 1:
                                break
                        nc2 = nc - 1 #nc2 is the nubmer of sub-clusters we would want to break the current cluster into
                        
                        #if the cluster cannot be subset further, change myFlag to 1
                        if nc2==1:
                            myFlag[idx1] = 1
                            continue
                        
                        # Given the chosen number of clusters, perform a final round of K-means clustering analysis
                        if method == 'kmeans':
                            fit1 = KMeans(init=config['pars'][method]['init'], n_clusters=nc2, n_init=config['pars'][method]['nInit'], max_iter=config['pars'][method]['nIterMax'], random_state=42).fit(mydata1)
                        elif method == 'kmedoids':
                            fit1 = KMedoids(init=config['pars'][method]['init'], n_clusters=nc2, max_iter=config['pars'][method]['nIterMax'], random_state=42).fit(mydata1)
                            
                    elif method == 'dbscan':
                        fit1 = DBSCAN(eps=config['pars'][method]['esp'],min_samples=config['pars'][method]['minSamples']).fit(mydata1)
                    
                    elif method == 'hdbscan':
                        fit1 = hdbscan.HDBSCAN(min_cluster_size=config['pars'][method]['minClusterSize']).fit(mydata1)
                        c1 = np.unique(fit1.labels_[:ndonor1], return_counts=False); c1 = np.sort(c1[c1!=-1])
                        labels_ = np.ones(len(fit1.labels_)) * (-1)
                        for ii,vv in enumerate(c1):
                            labels_[np.where(fit1.labels_==vv)] = ii
                        fit1.labels_ = labels_
                        c0 = np.unique(labels_, return_counts=False); c0 = np.sort(c0[c0!=-1])
                        nrec_donor = sum(labels_[ndonor1:] >= 0)
                        nrec_no_donor = sum(labels_[ndonor1:] == -1)
                        print('i1='+ str(i1) + ', n_clust=' + str(len(c0)) + ', nrec_donor=' + str(nrec_donor) + ', nrec_no_donor=',str(nrec_no_donor))
                        #import sys; sys.exit()
                    elif method == 'birch':
                        
                        for thresh in np.arange(config['pars'][method]['minThresh'],config['pars'][method]['maxThresh']+0.1,0.1):
                            for kk in range(config['pars'][method]['maxResample']):
                                mydata2 = mydata1.sample(frac=1)
                                fit1 = Birch(branching_factor=config['pars'][method]['branching_factor'],n_clusters=None,
                                         threshold=thresh).fit(mydata2)
                                fit1.labels_ = fit1.labels_[[mydata2.index.get_loc(x) for x in mydata1.index]]
                                u1 = np.unique(fit1.labels_[:ndonor1], return_counts=False)
                                n_clust = len(np.unique(fit1.labels_, return_counts=False))
                                nrec_donor = sum(np.in1d(fit1.labels_[ndonor1:], u1))
                                #print("thresh=" + str(round(thresh,1)) + ', resample='+str(kk) + ', n_clust=' + str(n_clust) + ', nrec_donor=' + str(nrec_donor))
                                if ((n_clust>1) and (nrec_donor==nreceiver1)):
                                    break
                            if ((n_clust>1) and (nrec_donor==nreceiver1)):
                                break                                
                        
                        while n_clust<=1:
                            mydata2 = mydata1.sample(frac=1)
                            fit1 = Birch(branching_factor=config['pars'][method]['branching_factor'],n_clusters=None,
                                         threshold=thresh).fit(mydata2) 
                            fit1.labels_ = fit1.labels_[[mydata2.index.get_loc(x) for x in mydata1.index]]
                            u1 = np.unique(fit1.labels_[:ndonor1], return_counts=False)
                            n_clust = len(np.unique(fit1.labels_, return_counts=False))
                            nrec_donor = sum(np.in1d(fit1.labels_[ndonor1:], u1))
                                                    
                    # assign cluster number to the subclusters; first subcluster gets the parent cluster number
                    tmp1 = fit1.labels_ + 1 
                    tmp1[fit1.labels_==0] = i1
                    # the remaining subclusters become new members of the parent group
                    tmp1[fit1.labels_>0] = tmp1[fit1.labels_>0]+myCluster.max() - 1
                    myCluster[idx1] = tmp1
                    values, counts = np.unique(myCluster[:ndonor], return_counts=True)
                    dtCluster = dict(zip(values, counts))
                                                                                  
                    
                # determine which clusters do not need further clustering 
                # and change the flag to 1; if the number of potentail donors in a cluster is 
                # smaller than the defined max number, then no further clustering is needed
                values, counts = np.unique(myCluster[:ndonor], return_counts=True)
                dtCluster = dict(zip(values, counts))
                clusts = [k for k, v in dtCluster.items() if v <= config['pars'][method]['nDonorMax']]
                idx0 = np.where(np.isin(myCluster, clusts))
                myFlag[idx0] = 1    
                myFlag[myCluster==0] = -1    
                    
            ###### assign donors based on clusters and spatial distance and apply additional constrains
            dtDonorAll = pd.concat((dtDonorAll, my_utils.assign_donors(scenario, donors, receivers, myCluster, config['pars']['general'], dist_spatial, dtAttrAll)),axis=0)
        # end of loop snow1
    # end of loop kround

    return dtDonorAll