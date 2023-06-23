def func(config, dtAttrAll,scenario, dist_spatial):
    print("calling function gower_dist ...")

    import pandas as pd
    import numpy as np
    import apply_pca
    import apply_donor_constraints

    dtDonorAll = pd.DataFrame()

    # two rounds of processing, first with attrs defined by the selected scenario (i.e., 'main'), 
    # and then with 'base' attrs for catchments with no donors found in 1st round
    attrs1 = {'main': config['attrs'][scenario],
            'base': config['attrs']['base']}
    for run1 in attrs1: # attr round
        # check if donors are identified for all receivers
        recs0 = dtAttrAll.query("tag=='receiver'")['id'].values
        if dtDonorAll.shape[1] > 0:
            recs0 = [value for value in recs0 if value not in dtDonorAll["id"].values]
        if len(recs0)==0:
            continue
    
        # reduce the attribute table to attributes for the current round
        dtAttr0 = dtAttrAll[config['non_attr_cols']+attrs1[run1]]

        # iteratively process all the receivers to handle data gaps so that 
        # receivers with the same missing attributes are processed in the same round
        recs1 = list() #receivers that have already been processed in previous krounds
        kround = 0
        while len([x for x in recs0 if x in recs1]) != len(recs0):
            kround = kround + 1
            print("\n------------------------" + run1 + " attributes,  Round " + str(kround) + "--------------------")   
            #pdb.set_trace()
            
            # apply principal component analysis
            myscores, weights, dtAttr = apply_pca.func(recs0, recs1, dtAttr0, attrs1[run1])
            
            # donors and receivers for this round
            donorsAll1 = dtAttr.query("tag=='donor'")['id'].tolist()
            receiversAll1 = dtAttr.query("tag=='receiver'")['id'].tolist()

            # compute Gower's distance between donors and receivers only, i.e., avoid calculating distance
            # between donors and donors, receivers and receivers (faster)     
            nd1 = len(donorsAll1)
            nr1 = len(receiversAll1)
            rng1 = myscores.max() - myscores.min()
            rng2 = np.repeat(np.matrix(rng1),nr1,axis=0)
            wgt2 = np.repeat(np.matrix(weights),nr1,axis=0)
            distAttr0 = pd.DataFrame()
            scores_receiver = myscores.iloc[nd1:]
            for r1 in range(nd1):
                scores_donor = np.repeat(np.matrix(myscores.iloc[r1]),nr1,axis=0)
                df1 = ((scores_donor - scores_receiver).abs()/rng2*wgt2).sum(axis=1)
                distAttr0 = pd.concat((distAttr0,df1),axis=1)
            distAttr0.columns = donorsAll1
            distAttr0.index = receiversAll1

            # determine which receivers to be processed for the current round
            # process only those not-yet processed receivers
            recs = [r1 for r1 in recs0 if r1 in receiversAll1 and r1 not in recs1]
            recs1 = recs1 + recs
            print(str(len(recs)) + " receivers to be processed this round")     

            # loop through all the receivers to be processed
            for rec1 in recs:

                # find donors within the defined buffer iteratively so that closest donors 
                # with attr distance below the predefined value can be found
                buffer = config['pars']['gower']['minSpaDist'] - 100
                while buffer < config['pars']['general']['maxSpaDist'] -100 :

                    buffer = buffer + 100
                    # print("buffer = " + str(buffer))
                    
                    #if there exists donor catchment within a short distance,               
                    s1 = dist_spatial.loc[rec1]
                    donors1 = s1.loc[s1<=config['pars']['gower']['zeroSpaDist']].index.tolist()                    
                    if len(donors1)>0 :
                        # select that catchment as donor;
                        donors1 = [s1[donors1].idxmin()]
                    else:
                        # otherwise, narrow down to donors within the buffer
                        donors1 = s1.loc[s1<=buffer].index.tolist()
                                
                    # potential donors with dist <= maxAttrDist
                    dist1 = distAttr0.loc[rec1, donors1]
                    ix1 = [x for x in dist1.index if dist1[x] <= config['pars']['gower']['maxAttrDist']]
                    if (len(ix1)==0): # if not, continue to the next round with a larger neighbourhood
                        continue

                    # if a suitable donor is found (or all donors have been assessed), break the loop and stop searching
                    if dist1[ix1].min() <= config['pars']['gower']['minAttrDist'] or len(donors1)==len(donorsAll1): 
                        break

                # if donors are identified
                if len(donors1)>0:   
                                        
                    # narrow down to those that satisfy the maxAttrDist threshold
                    dist1 = dist1.loc[ix1]
                    donor1 = dist1.index.tolist()
                
                    # apply additional donor constraints
                    donor1, dist1 = apply_donor_constraints.func(rec1, donor1, dist1, config['pars'], dtAttrAll)
                
                    # if donors remain
                    if len(donor1)>0:          
                        # choose those with the smallest attribute distance
                        ix2 = np.argsort(dist1)[range(min(len(dist1),config['pars']['gower']['nDonorMax']))] 
                        distAttr1 = dist1[ix2]
                        donor1 = donor1[ix2]            
                
                        # get donor-receiver spatial distance
                        distSpatial1 = np.array(dist_spatial.loc[rec1,donor1])
                
                        # choose the donor with the smallest spatial distance (best donor)
                        if len(donor1)>0:
                            ix3 = np.where(distSpatial1 == distSpatial1.min())[0][0] #"[0][0]" gets index of 1st minimum if multiple minima exist                                               
                            donor_best1 = donor1[ix3].item()
                            distAttr_best1 = distAttr1[ix3].item()
                            distSpatial_best1 = distSpatial1[ix3].item()
                
                # if no donors found (after both 'main' and 'base' attribute rounds), 
                # get the spatially closest donor with some constraints
                tag1 = run1
                if len(donor1)==0 and run1=="base":                       
                    # get all donors and their spatial distance to the receiver
                    donor0 = dtAttrAll.query("tag=='donor'")['id']
                    dist0 = dist_spatial.loc[rec1,]
                
                    # apply additional donor constraints
                    donor0, dist0 = apply_donor_constraints.func(rec1, donor0, dist0, config['pars'], dtAttrAll)
                
                    # choose the donor that is spatially closest
                    donor_best1 = donor1 = donor0[np.where(dist0==min(dist0))]
                    distSpatial_best1 = distAttr1 = min(dist0)
                    distAttr_best1 = distSpatial1 = distAttr0.loc[rec1,donor1]
                    tag1 <- "proximity"
                
                # add the donor/receiver pair to the pairing table    
                if len(donor1)>0:
                    pair1 = {'id': rec1, 'tag': tag1, 'donor': donor_best1,
                        'distAttr': np.round(distAttr_best1,3),
                        'distSpatial': distSpatial_best1,
                        'donors': ','.join(donor1), 
                        'distAttrs': ','.join(map(str,pd.Series(np.round(distAttr1,3)))),
                        'distSpatials': ','.join(map(str,pd.Series(distSpatial1)))}
                    dtDonorAll = pd.concat((dtDonorAll, pd.DataFrame(pair1,index=[0])),axis=0)
    
    return dtDonorAll
