import yaml
import pandas as pd
import os.path
import time

import funcs_clust
import funcs_dist

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

# if donors overlap with receivers (i.e., they share the same hydrofabric files), exclude them from the receivers
if config['hydrofabric']['shared']:
    dtAttrReceiver = dtAttrReceiver.loc[~dtAttrReceiver['id'].isin(dtAttrDonor['id'])]

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


# different attribute scenarios
scenarios = list(config['attrs'].keys())
scenarios.remove('base') # the base scenario is only used together with CAMELS or HLR
#scenarios = ['camels']
#scenarios = ['hlr']

# different regionalization algorithms
functions = {'urf': funcs_dist,
             'gower': funcs_dist,
             'kmeans': funcs_clust,
             'kmedoids': funcs_clust,
             'dbscan': funcs_clust,
             'hdbscan': funcs_clust,
             'birch': funcs_clust,
             }
funcs = functions.keys()
funcs = ['hdbscan']
funcs = ['kmedoids']
funcs = ['birch']

# loop through regionalization algorithms and scenarios to generate donor-receiver pairings
for func1 in funcs:
    for scenario in scenarios:
        outfile = '../output/donor_' + scenario + '_' + func1 + '.csv'

        print("\n======== processing donors: " + func1 + "  " + scenario +" =============\n")       
        start_time = time.time()
        dtDonorAll = functions[func1].func(config, dtAttrAll,scenario, distSpatial0, func1)           
        print("\nTotal processing time: --- %s seconds ---" % (time.time() - start_time))  
        
        # save donor receiver pairing to csv file    
        dtDonorAll.to_csv(outfile, index=False)

