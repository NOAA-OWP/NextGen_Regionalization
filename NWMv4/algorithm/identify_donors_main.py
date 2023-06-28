import yaml
import pandas as pd
import os.path
import time

import gower_dist
import kmeans_clust
import unsupervised_random_forest

# read configuration (algorithm parameters etc) into dictionary
with open('../data/config.yaml', 'r') as stream:
    try:
        config = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

# read donor attributes
dtAttrDonor = pd.read_csv('../data/all_attrs_donors.csv')
dtAttrDonor['tag'] = 'donor'

# read receiver attributes
dtAttrReceiver = pd.read_csv('../data/all_attrs_receivers.csv')
dtAttrReceiver['tag'] = 'receiver'

# combine donor & receiver attributes
dtAttrAll = pd.concat([dtAttrDonor, dtAttrReceiver])

# detemine whether the catchment is snowy (as snowy and non-snowy catchments are processed separately)
dtAttrAll['snowy'] = dtAttrAll['snow_frac'].apply(lambda x: True if x >= config['pars']['general']['minSnowFrac'] else False)
del dtAttrDonor, dtAttrReceiver

# columns in the attrs table that are not actual attributes
config['non_attr_cols'] = ["id","tag","snowy","hsg"]

# pretty print nested dictionaries
# print(yaml.dump(config, default_flow_style=False)) 

# compute spatial distance between all receivers and donors if not already exists
f1 = '../data/dist_spatial_donor_receiver.csv'
if os.path.isfile(f1):
    distSpatial0 = pd.read_csv(f1,index_col=0)
else:
    pass
    #distSpatial0 = compute_dist_spatial(f1, subset(dtAttrAll,tag=="donor")$id, subset(dtAttrAll,tag=="receiver")$id)

# generate the donor-receiver pairing for the defined attr scenarios using different algorithms (functions)
# which are assembled in a dictionary 
scenarios = list(config['attrs'].keys())
scenarios.remove('base') # the base scenario is only used together with CAMELS or HLR
funcs = ['gower_dist','kmeans_clust','kmedoids_clust']
scenarios = ['hlr', 'camels']
functions = {'gower_dist': gower_dist,
             'kmeans_clust': kmeans_clust,
             'kmedoids_clust': kmeans_clust,
             }
for func1 in funcs:
    for scenario in scenarios:
        outfile = '../output/donor_' + scenario + '_' + func1 + '.csv'
        if os.path.isfile(outfile):
            continue
        
        print("\n======== processing donors: " + func1 + "  " + scenario +" =============\n")
        
        start_time = time.time()
        if func1 == "kmeans_clust":
            dtDonorAll = functions[func1].func(config, dtAttrAll,scenario, distSpatial0, "kmeans")
        elif func1 == "kmedoids_clust":
            dtDonorAll = functions[func1].func(config, dtAttrAll,scenario, distSpatial0, "kmedoids")
        else:
            dtDonorAll = functions[func1].func(config, dtAttrAll,scenario, distSpatial0)  
        print("\nTotal processing time: --- %s seconds ---" % (time.time() - start_time))  
            
        dtDonorAll.to_csv(outfile, index=False)

