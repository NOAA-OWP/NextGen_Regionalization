# This function performs donor-receiver pairing based on clustering using   
#   k-means clustering (method = "kmeans") 
#   k-medoids clustering (method = "kmedoids")
#   HDBSCAN (method = "hdbscan") - Hierarchical Density-Based Spatial Clustering of Applications with Noise. 
#      Finds core samples of high density and expands clusters from them. 
#   BIRCH (method = "birch") - Balanced Iterative Reducing & Clustering with Hierarchy. Scalable for large datasets. 
#      Order of points in the dataset influences the outcome. Hence interative resampling is implemented here.

def func(config, dtAttrAll,scenario, dist_spatial, method):
    
    print("perform clustering using " + str(method) + " approach ...")

    import pandas as pd
    import numpy as np
    from sklearn.cluster import KMeans, Birch
    from sklearn_extra.cluster import KMedoids
    import hdbscan
    import my_utils

    dfDonorAll= pd.DataFrame()    

    # attributes to be used
    attrs1 = config['attrs'][scenario]

    # all receivers to find donors for
    recs0 = dtAttrAll.query("tag=='receiver'")['id'].values
    print('\n Total number of receivers to be paired with donors: ' + str(len(recs0)))

    # reduce the attribute table to attributes
    dtAttr0 = dtAttrAll[config['non_attr_cols']+attrs1]

    # iteratively process all the receivers to handle data gaps (becasuse some attributes may be missing for some catchments)
    kround = 0
    while True:
    
        recs = list()
        if dfDonorAll.shape[0]>0:
            recs = np.unique(dfDonorAll['id'])
        if len([x for x in recs0 if x in recs]) == len(recs0):
            break
        
        kround = kround + 1   
        print("\n------------------------" + scenario + " attributes,  Round " + str(kround) + "--------------------")

        # figure out valid attributes to use this round
        dtAttr = my_utils.get_valid_attrs(recs0, recs, dtAttr0, attrs1, config)

        # apply principal component analysis
        myscores, weights = my_utils.apply_pca(dtAttr.drop(config['non_attr_cols'], axis=1))    

        # process snowy and non-snowy catchments sparately
        for snow1 in np.unique(dtAttr['snowy']):
      
            str1 = "non-snowy"
            if snow1:
                str1 = "snowy"

            # the current snowy group
            dtAttr1 = dtAttr.query("snowy==@snow1")
            scores1 = myscores[(dtAttr['snowy']==snow1).values]
            donors = dtAttr1.query("tag=='donor'")['id'].tolist()
            receivers = dtAttr1.query("tag=='receiver'")['id'].tolist()

            # receivers to be processed in this round
            recs1 = receivers.copy()
            if dfDonorAll.shape[0]>0:
                recs1 = [x for x in recs1 if x not in dfDonorAll['id'].tolist()]
            print("\n======= " + str(len(recs1)) + ' ' + str1 + " basins ========")           
            if len(recs1)==0:
                continue

            # define starting labels
            labels = np.zeros(scores1.shape[0]) # start with a single cluser (label = 0)
            label_done = -99 # label = -99 indicates donor identified
            
            # for those already processed, assign "label_done"
            if dfDonorAll.shape[0]>0:
                labels[[len(donors)+receivers.index(x) for x in receivers if x in dfDonorAll['id'].tolist()]] = label_done
            
            # identify donors iteratively
            dfDonorSnow = pd.DataFrame() # data frame to hold donor table for the current snowy group
            iter1 = 0
            nrec_with_donor0 = dfDonorSnow.shape[0]
            while dfDonorSnow.shape[0] < len(recs1):
                
                iter1 = iter1 + 1
                
                label_rec, count_rec = np.unique(labels[len(donors):], return_counts=True) # receiver clusters
                count_rec = count_rec[label_rec != label_done]; label_rec = label_rec[label_rec != label_done]                           
                
                # make a copy of current labels (which will change during the iteration of clusters)
                labels1 = labels.copy()
                
                # iterate through all clusters
                for ll in label_rec: 
                    
                    # donors and receivers in the current cluster
                    donors1 = [x for jj,x in enumerate(donors) if labels1[jj]==ll]
                    receivers1 =[ x for jj,x in enumerate(receivers) if labels1[jj+len(donors)]==ll]
                    
                    # receivers in the current cluster that still need to be processed
                    recs2 = receivers1.copy()
                    if dfDonorSnow.shape[0] > 0:
                        recs2 = [x for x in recs2 if x not in dfDonorSnow['id'].tolist()]
                    if dfDonorAll.shape[0] > 0:
                        recs2 = [x for x in recs2 if x not in dfDonorAll['id'].tolist()]
                    
                    # if there exist donors in the cluster                                            
                    if len(donors1)>0:
                        # if number of donors in the cluster is larger than the defined 'nDonorMax', proceed to break the cluster further down
                        if len(donors1) > config['pars'][method]['nDonorMax']:
                            idx1 = [donors.index(x) for x in donors1]
                            idx2 = [receivers.index(x) for x in receivers1]
                            idx0 = idx1 + [x+len(donors) for x in idx2]
                            mydata1 = scores1.iloc[idx0]

                            if (method in ['kmeans','kmedoids']):
                                # break the dataset into two clusters each time
                                if method == 'kmeans':
                                    fit1 = KMeans(init=config['pars'][method]['init'], n_clusters=2, n_init=config['pars'][method]['nInit'], max_iter=config['pars'][method]['nIterMax'], random_state=42).fit(mydata1)
                                elif method == 'kmedoids':
                                    fit1 = KMedoids(init=config['pars'][method]['init'], n_clusters=2, max_iter=config['pars'][method]['nIterMax'], random_state=42).fit(mydata1)    

                            elif method == 'hdbscan':
                                fit1 = hdbscan.HDBSCAN(min_samples=11,min_cluster_size=config['pars'][method]['minClusterSize'],allow_single_cluster=False).fit(mydata1)

                            elif method == 'birch':                    
                                for thresh in np.arange(config['pars'][method]['minThresh'],config['pars'][method]['maxThresh']+0.1,0.1):
                                    for kk in range(config['pars'][method]['maxResample']):
                                        mydata2 = mydata1.sample(frac=1)
                                        fit1 = Birch(branching_factor=config['pars'][method]['branching_factor'],n_clusters=None,
                                                    threshold=thresh).fit(mydata2)
                                        fit1.labels_ = fit1.labels_[[mydata2.index.get_loc(x) for x in mydata1.index]]
                                        u1 = np.unique(fit1.labels_[:len(donors1)], return_counts=False)
                                        n_clust = len(np.unique(fit1.labels_, return_counts=False))
                                        nrec_donor = sum(np.in1d(fit1.labels_[len(donors1):], u1))
                                        if ((n_clust>1) and (nrec_donor==len(receivers1))):
                                            break
                                    if ((n_clust>1) and (nrec_donor==len(receivers1))):
                                        break   
                                # if n_clust==1:
                                #     print("*** Warning: only one cluster formed - cannot subset further - assign donors now ***")
                            
                            fit1.labels_ = fit1.labels_ + 2 # add 2 to the cluster labels because hdbscan cluster starts with -1
                            labels[idx0] = fit1.labels_ + labels.max()
                            label_rec1, count_rec1 = np.unique(fit1.labels_[len(donors1):], return_counts=True) # receiver clusters
                            label_don1, count_don1 = np.unique(fit1.labels_[:len(donors1)], return_counts=True) # donor clusters
                            
                            # print("donor clusters: " + str(label_don1))
                            # print("donor cluster counts: " + str(count_don1))
                            # print("receiver clusters: " + str(label_rec1))
                            # print("receiver cluster counts: " + str(count_rec1))
                                                                                  
                            # for receivers in clusters without donors, or clusters that cannot be subset further,
                            # choose donors from those in the parent cluster (donors1)
                            l1 = [x for x in label_rec1 if x not in label_don1]
                            # n_clust = len(np.unique(fit1.labels_, return_counts=False))
                            # if n_clust==1:
                            #     l1 = label_rec1
                            if len(l1) > 0:                                
                                recs3 = [x for ii,x in enumerate(receivers1) if fit1.labels_[len(donors1):][ii] in l1]
                                dfDonorSnow = pd.concat((dfDonorSnow, my_utils.assign_donors(scenario, donors1, recs3, config['pars']['general'], dist_spatial, dtAttrAll)),axis=0)
                                labels[[receivers.index(x)+len(donors) for x in recs3]] = label_done
                                                    
                        else:
                            # for receivers in clusters with number of donors smaller than 'nDonorMax', no further clustering is needed
                            # identify donors from the current cluster
                            dfDonorSnow = pd.concat((dfDonorSnow, my_utils.assign_donors(scenario, donors1, recs2, config['pars']['general'], dist_spatial, dtAttrAll)),axis=0)
                            labels[labels == ll] = label_done  
                    
                    # for receivers in clusters without donors, chooses from all donors based on spatial proximity        
                    else: 
                        dfDonorSnow = pd.concat((dfDonorSnow, my_utils.assign_donors('proximity', donors, recs2, config['pars']['general'], dist_spatial, dtAttrAll)),axis=0)            
                        labels[labels == ll] = label_done           
                # end cluster loop
                
                # Algorithm converged
                if (nrec_with_donor0 == dfDonorSnow.shape[0]) & (iter1 > 30): 
                    if dfDonorSnow.shape[0] == 0:
                        recs2 = recs1.copy()
                    else:
                        recs2 = [x for x in recs1 if x not in dfDonorSnow['id'].tolist()]
                    if len(recs2) > 0:
                        print("\nAlgorithm converged without donors identified for " + str(len(recs2)) + " receivers ... use proximity for these receivers")
                        dfDonorSnow = pd.concat((dfDonorSnow, my_utils.assign_donors('proximity', donors, recs2, config['pars']['general'], dist_spatial, dtAttrAll)),axis=0) 
                    break   
                        
                # update progress on donor-receiver paring
                if iter1 > 1: 
                    print('\n---------------- iteration = ' + str(iter1-1) + ' -------------')
                    print("Number of receivers with donors identified: " + str(dfDonorSnow.shape[0]))
                    if dfDonorSnow.shape[0] > 0:
                        uniq, freq = np.unique(dfDonorSnow['tag'],return_counts=True)
                        print(dict(zip(uniq, freq)))

                # update number of receivers with donors identified
                nrec_with_donor0 = dfDonorSnow.shape[0]
                
            # end while (iteration) loop
            # add to the final donor table
            dfDonorAll = pd.concat((dfDonorAll, dfDonorSnow),axis=0)
            
        # end of loop snow1 (to separate processing for snow and non-snow dominated receivers)
    # end of loop kround (to use valid attributes)

    return dfDonorAll