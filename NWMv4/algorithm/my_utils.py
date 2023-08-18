from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA    
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
    
def get_valid_attrs(recs0, recs1, dtAttr0, attrs, config):
   
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
    dtAttr = dtAttr.dropna(subset=[x for x in attrs if x not in vars0], inplace=False)
    
    if dtAttr.shape[0]==0:
        print("WARNING: no valid attributes found for the following receivers: ")
        print(dtAttr['id'].tolist())
       
    return dtAttr

def apply_pca(data0, min_var=0.8):
    
    # standardize the data        
    scaled_data = StandardScaler().fit_transform(data0)

    # perform PCA given the required minimum total explained variance
    pca = PCA(min_var)
    pca.fit(scaled_data)
    n1 = pca.n_components_
        
    print("Number of PCs selected: " + str(n1))
    print("PCA total portion of variance explained ... " + str(sum(pca.explained_variance_ratio_)))
    x_pca = pca.transform(scaled_data)
        
    # standardize the reduced data (comment out because it is not necessary)
    # x_pca = StandardScaler().fit_transform(x_pca)
        
    # convert to dataframe
    strs1 = ['pc'] * n1
    strs2 = list(map(str,list(range(1,n1+1))))
    cols = [i + j for i, j in zip(strs1, strs2)]
    x_pca = pd.DataFrame(x_pca, columns = cols)
    
    # weights for chosen PCs are proporitonal to the variances they explained
    w1 = pca.explained_variance_ratio_ / sum(pca.explained_variance_ratio_)
    
    # return scores and weights 
    return x_pca, w1

# apply a few constraints to donors identified (e.g., via Gower's distance or another technique)
def apply_donor_constraints(rec, donors, dists, pars, dtAttr):
  
    # 1. narrow down to donors with the same snowiness category
    snowy = dtAttr.query("id == @rec & tag=='receiver'")['snowy']
    snowy1 = dtAttr.query("id in @donors & tag=='donor'")['snowy']
    ix1 = snowy1.isin(snowy)
    if sum(ix1) > 0:
        dists = np.array(dists)[ix1]
        donors = np.array(donors)[ix1]
  
    # 2. further narrow down to those donors within maximum spatial distance defined
    ix1 = dists <= pars['maxSpaDist']
    if sum(ix1) > 0:
        dists = np.array(dists)[ix1]
        donors = np.array(donors)[ix1]  
  
    # 3. further narrow down based on screening attributes
    for att1 in pars['maxAttrDiff'].keys():
        ix1 = abs(np.array(dtAttr.query("id==@rec & tag=='receiver'")[att1]) - \
            np.array(dtAttr.query("id in @donors & tag=='donor'")[att1])) \
                <= pars['maxAttrDiff'][att1]
        if sum(ix1) > 0:
            dists = np.array(dists)[ix1]
            donors = np.array(donors)[ix1]  
        
    # 4. further narrow down to donors in the same HSG
    hsg = dtAttr.query("id==@rec & tag=='receiver'")['hsg']
    hsg1 = dtAttr.query("id in @donors & tag=='donor'")['hsg']
    ix1 = hsg1.isin(hsg)
    if sum(ix1) > 0:
        dists = np.array(dists)[ix1]
        donors = np.array(donors)[ix1]
  
    return donors, dists

###### assign donors based on clusters and spatial distance and apply additional constrains
def assign_donors(scenario, donors, receivers,pars, dist_spatial, dtAttr):
    df_donor = pd.DataFrame()
    for rec1 in receivers:

        # get spatial distances
        dists1 = dist_spatial.loc[rec1,donors]

        # apply additional donor constraints
        donors1, dists1 = apply_donor_constraints(rec1, donors, dists1, pars, dtAttr)
        
        # order donors by spatial distance
        ix1 = np.argsort(dists1)
        dists1 = dists1[ix1]
        donors1 = np.array(donors1)[ix1]
        
        # if the number of donors for a given receiver is greater than nDonorMax, ignore the additional donors
        nd_max = min(len(dists1),pars['nDonorMax'])
        dists1 = dists1[range(nd_max)]
        donors1 = donors1[range(nd_max)]

        # add the donor/receiver pair to the pairing table    
        if len(donors1)>0:
            pair1 = {'id': rec1, 'tag': scenario, 'donor': donors1[0],
                'distSpatial': dists1[0],
                'donors': ','.join(donors1), 
                'distSpatials': ','.join(map(str,pd.Series(dists1)))}
            df_donor = pd.concat((df_donor, pd.DataFrame(pair1,index=[0])),axis=0)
 
    return df_donor

def plot_clusters(data1,labels,ndonor):
    fig = plt.figure(figsize=(20, 14))
    cols_all = [[0,1],[0,2],[3,4],[3,5]]
    for i1, cols in enumerate(cols_all):
        
        if max(cols) > data1.shape[1]:
            break
        ax = fig.add_subplot(2,2,i1+1)
        
        # receiver - noise
        d1 = labels[ndonor:] == -1
        plt.scatter(data1.iloc[:,cols[0]][ndonor:][d1],data1.iloc[:,cols[1]][ndonor:][d1],c='grey',marker='.')

        # receiver non-noise
        d1 = labels[ndonor:] != -1
        plt.scatter(data1.iloc[:,cols[0]][ndonor:][d1],data1.iloc[:,cols[1]][ndonor:][d1],c=labels[ndonor:][d1],cmap='rainbow',marker='.')
        plt.colorbar()

        # donor - noise
        d1 = labels[:ndonor] == -1
        plt.scatter(data1.iloc[:,cols[0]][:ndonor][d1],data1.iloc[:,cols[1]][:ndonor][d1],c='black',marker='x',s=100)

        # donor non-noise
        d1 = labels[:ndonor] != -1
        plt.scatter(data1.iloc[:,cols[0]][:ndonor][d1],data1.iloc[:,cols[1]][:ndonor][d1],c='green',marker='x',s=100)
        
        plt.title(data1.columns[cols[1]] + ' vs ' + data1.columns[cols[0]], fontsize=20, fontweight='bold')

    plt.subplots_adjust(left=0.07, bottom=0.07, right=0.95, top=0.93, hspace=0.15,wspace=0.03)
    plt.show()