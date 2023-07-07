def func(config, dtAttrAll,scenario, dist_spatial, method="kmeans"):
    
    print("perform clustering using " + str(method) + " approach ...")

    import pandas as pd
    import numpy as np
    import apply_pca
    import apply_donor_constraints
    from sklearn.cluster import KMeans
    from sklearn_extra.cluster import KMedoids
    import math

    dtDonorAll = pd.DataFrame()    
    
    # algorithm parmeters
    init = config['pars'][method]['init']
    max_iter = config['pars'][method]['nIterMax']
    max_donor = config['pars'][method]['nDonorMax']
    if method == 'kmeans':
        n_init = config['pars'][method]['nInit']
    
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

        # apply principal component analysis
        myscores, weights, dtAttr = apply_pca.func(recs0, recs1, dtAttr0, attrs1, config)    
                
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
            while sum(myFlag[(ndonor):(ndonor+nreceiver-1)]==0) >= 1:

                # for each cluster that has more than the preset max. number of donors, break it into smaller clusters
                clusts = [k for k, v in dtCluster.items() if v > max_donor]
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
                    
                    # determine number of clusters for K-Means Clustering; choose the maximum number
                    # of clusters that can ensure 1) each cluster contains at least one donor 
                    # and that 2) no group is left with only donors (i.e., no potential receiver
                    # is included in the group) 
                    # but note the 2nd condition is commented out below
                    for nc in range(2,(mydata1.shape[0])):
                        if method == 'kmeans':
                            fit1 = KMeans(init=init, n_clusters=nc, n_init=n_init, max_iter=max_iter, random_state=42).fit(mydata1)
                        elif method == 'kmedoids':
                            fit1 = KMedoids(init=init, n_clusters=nc, max_iter=max_iter, random_state=42).fit(mydata1)                        

                        c0 = fit1.labels_[:ndonor1]
                        c1 = fit1.labels_[ndonor1:(ndonor1+nreceiver1)]
                        nrec_nodonor = len(np.where(np.isin(c1,c0)==False)[0])
                        #ndon_norec = len(np.where(np.isin(c0,c1)==False)[0])

                        if nrec_nodonor >= 1:
                            break
                    nc2 = nc - 1 #nc2 is the nubmer of sub-clusters we would want to break the current cluster into
                    
                    #if the cluster cannot be subset further, change myFlag to 1
                    if nc2==1:
                        myFlag[idx1] = 1
                        continue
                    
                    # Given the chosen number of clusters, perform a final round of K-mean clustering analysis
                    if method == 'kmeans':
                        fit1 = KMeans(init=init, n_clusters=nc2, n_init=n_init, max_iter=max_iter, random_state=42).fit(mydata1)
                    elif method == 'kmedoids':
                        fit1 = KMedoids(init=init, n_clusters=nc2, max_iter=max_iter, random_state=42).fit(mydata1)
                    tmp1 = fit1.labels_ + 1   
                                    
                    # assign cluster number to the subclusters; first subcluster gets the parent cluster number
                    tmp1[fit1.labels_==0] = i1
                    # the remaining subclusters become new members of the parent group
                    tmp1[fit1.labels_>0] = tmp1[fit1.labels_>0]+myCluster.max() - 1
                    myCluster[idx1] = tmp1
                    unique, counts = np.unique(myCluster[:ndonor], return_counts=True)
                    dtCluster = dict(zip(unique, counts))              
                    
                # determine which clusters do not need further clustering 
                # and change the flag to 1; if the number of potentail donors in a cluster is 
                # smaller than the defined max number, then no further clustering is needed
                unique, counts = np.unique(myCluster[:ndonor], return_counts=True)
                dtCluster = dict(zip(unique, counts))
                #print(dtCluster)
                #import sys
                #sys.exit()
                #print(yaml.dump(dtCluster, default_flow_style=False))
                clusts = [k for k, v in dtCluster.items() if v <= max_donor]
                idx0 = np.where(np.isin(myCluster, clusts))
                myFlag[idx0] = 1
                    #print("myflag==1: " + str(sum(myFlag)))
                    
            ###### assign donors based on clusters and spatial distance and apply additional constrains
            #print(myCluster)
            for rec1 in receivers:
                #print("rec1=",rec1)
                #print(len(receivers))
                i1 = [ receivers.index(x)+1 for x in receivers if x == rec1 ][0]
                
                if math.isnan(i1):
                    continue
                c1 = myCluster[i1+ndonor-1]
                donors1 = [donors[:ndonor][i] for i in np.where(myCluster[:ndonor]==c1)[0]]       
                dists1 = dist_spatial.loc[rec1,donors1]
        
                # apply additional donor constraints
                donors1, dists1 = apply_donor_constraints.func(rec1, donors1, dists1, config['pars'], dtAttrAll)
                
                # order donors by spatial distance
                ix1 = np.argsort(dists1)
                dists1 = dists1[ix1]
                donors1 = donors1[ix1]
                
                # if the number of donors for a given receiver is greater than nDonorMax, ignore the additional donors
                nd_max = min(len(dists1),max_donor)
                dists1 = dists1[range(nd_max)]
                donors1 = donors1[range(nd_max)]

                # add the donor/receiver pair to the pairing table    
                if len(donors1)>0:
                    pair1 = {'id': rec1, 'tag': scenario, 'donor': donors1[0],
                        'distSpatial': dists1[0],
                        'donors': ','.join(donors1), 
                        'distSpatials': ','.join(map(str,pd.Series(dists1)))}
                    dtDonorAll = pd.concat((dtDonorAll, pd.DataFrame(pair1,index=[0])),axis=0)
            # end of loop rec1
        # end of loop snowy
    # end of loop kround

    return dtDonorAll