def func(recs0, recs1, dtAttr0, attrs, config):

    from sklearn.preprocessing import StandardScaler
    from sklearn.decomposition import PCA    
    import pandas as pd
    
    # get the valid attributes for the first receiver to be processed this round
    dt1 = dtAttr0.query("tag == 'receiver'")
    dt1 = dt1[dt1.id.isin(recs0) & ~dt1.id.isin(recs1)].iloc[0]
    dt1 = dt1[~dt1.index.isin(config['non_attr_cols'])]
    vars = config['non_attr_cols'] + dt1.index[~dt1.isna()].tolist()
    dtAttr = dtAttr0[vars]        
    vars = [value for value in vars if value in attrs]    # attrs included for current round
    vars0 = [value for value in attrs if value not in vars] # attrs excluded for current round
    
    if len(vars) > 0:
        if len(vars0) >0:
            print("Excluding " + str(len(vars0)) + " attributes: "+ ','.join(vars0))
        else:
            print("Using all attributes")
        
        # ignore donors & receivers with NA attribute values
        dtAttr.dropna(subset=[x for x in attrs if x not in vars0], inplace=True)

        # reduce table to only attributes that are to be used
        dtAttr1 = dtAttr[vars]
    
        # standardize the data 
        scalar = StandardScaler()
        scalar.fit(dtAttr1)
        scaled_data = scalar.transform(dtAttr1)

        # iteratively increase no of principle componenets to make sure 
        # the total explained variance is greater than 80%
        for n1 in range(1,len(dtAttr1)):
            pca = PCA(n_components = n1)
            pca.fit(scaled_data)
            if sum(pca.explained_variance_ratio_)>=0.8:
              break
        
        print("Number of PCs selected: " + str(n1))
        print("PCA total portion of variance explained ... " + str(sum(pca.explained_variance_ratio_)))
        x_pca = pca.transform(scaled_data)
        
        # convert to dataframe
        strs1 = ['pc'] * n1
        strs2 = list(map(str,list(range(1,n1+1))))
        cols = [i + j for i, j in zip(strs1, strs2)]
        x_pca = pd.DataFrame(x_pca, columns = cols)
    
        # weights for chosen PCs are proporitonal to the variances they explained
        w1 = pca.explained_variance_ratio_ / sum(pca.explained_variance_ratio_)
    
        # return scores and weights 
        return x_pca, w1, dtAttr
