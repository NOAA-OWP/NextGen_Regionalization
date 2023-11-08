# Main program: create donor-receiver pairs based on physical similarity, in conjuction with spatial proximity
# 
# Physical similarity is computed from a set of hydroclimatic and physiographic characteristics,
#   defined by a conceptual framework. Currently implemented conceptual frameworks include:
#       1) the Hydrologic Landscape Region (HLR)
#       2) the Catchment Attributes and Meteorology for Large-Sample studies (CAMELS)
# 
# Donor-receiver pairing is accomplished with two types of methods:
#       1) Similarity matrix: Gower’s distance, unsupervised Random Forest (URF)
#       2) Clustering: K-means clustering, K-medoids clustering, 
#                   Hierarchical Density-Based Spatial Clustering of Applications with noise (HDBSCAN)
#                   Balanced Iterative Reducing & Clustering using Hierarchy (BIRCH)
#
# Notes:
# 1) all pairing methods (except for URF) employ a Principal Component Analysis (PCA) to remove correlation
#   among the basin attributes and reduce the dimentionality of the similarity proplem
# 2) donor and receivers are allowed to have different hydrofabric data

# external packages
import yaml
import pandas as pd
import os.path
import time

# local functions developed for NextGen regionalization
import sys
sys.path.append('../algorithm/')
#sys.path.insert(1, '/home/***REMOVED***/NextGen_Regionalization/NWMv4/algorithm')
import funcs_clust
import funcs_dist_new
import my_utils

# read configuration (algorithm parameters etc)
with open('data/config.yaml', 'r') as stream:
    try:
        config = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

# realize file names based on huc number and hydrofabric version defined
config_str = yaml.dump(config)
config_str = config_str.replace('{huc}', config['huc'])
config_str = config_str.replace('{ver}', config['ver'])
config = yaml.safe_load(config_str)
               
# pretty print config info (nested dictionaries)
# print(yaml.dump(config, default_flow_style=False)) 

print('\n------------------------------')
print('processing ' + config['huc'] + ', hydrofab version ' + config['ver'])

# check if donors and receivers share the same hydrofabric
# if yes, use the defined receiver hydrofabric and attribute files for both receivers and donors
# in other words, donor files defined in the config file are ignored
if config['inputs']['hydrofab_shared']:
    config['inputs']['file_attrs_data']['donor'] = config['inputs']['file_attrs_data']['receiver']
    config['inputs']['file_hydrofab']['donor'] = config['inputs']['file_hydrofab']['receiver']
    
# read receiver attributes
dfAttrReceiver = pd.read_csv(config['inputs']['file_attrs_data']['receiver'])
dfAttrReceiver['tag'] = 'receiver'

# read donor attributes
dfAttrDonor = pd.read_csv(config['inputs']['file_attrs_data']['donor'])
dfAttrDonor['tag'] = 'donor'

# narrow down to donor catchments in the donor list and crowsswalk table
gages = pd.read_csv(config['inputs']['file_donor_list'], header=None,dtype='str').iloc[:,0].tolist()
cwt = pd.read_csv(config['inputs']['file_crosswalk'],dtype={'gages':'str'})
donorsAll = cwt.loc[cwt['gages'].isin(gages)]['id'].tolist()
dfAttrDonor = dfAttrDonor.loc[dfAttrDonor['id'].isin(donorsAll)]
donorsAll = set(dfAttrDonor['id'].tolist())

# if donors overlap with receivers (i.e., they share the same hydrofabric files), exclude them from the receivers
if config['inputs']['hydrofab_shared']:
    dfAttrReceiver = dfAttrReceiver.loc[~dfAttrReceiver['id'].isin(dfAttrDonor['id'])]
receiversAll = dfAttrReceiver['id'].tolist()

# combine donor & receiver attributes
dfAttrAll = pd.concat([dfAttrDonor, dfAttrReceiver])
del dfAttrDonor, dfAttrReceiver

# detemine whether the catchment is snowy (as snowy and non-snowy catchments are processed separately)
dfAttrAll['snowy'] = dfAttrAll['snow_frac'].apply(lambda x: True if x >= config['pars']['general']['minSnowFrac'] else False)

# columns in the attrs table that are not actual attributes
config['non_attr_cols'] = ["id","tag","snowy","hsg"]

# add scenario for using all attributes
config['attrs']['all'] = [x for x in dfAttrAll.columns.tolist() if x not in config['non_attr_cols']]

# compute spatial distance between all receivers and donors if not already computed
f1 = config['inputs']['file_distance']
if os.path.isfile(f1):
    distSpatial0 = pd.read_csv(f1,index_col=0)
else:
    distSpatial0 = my_utils.calculate_spatial_distance(shp_file_rec=config['inputs']['file_hydrofab']['receiver'], 
                                                       shp_file_don=config['inputs']['file_hydrofab']['donor'], 
                                                       donors=donorsAll, receivers=receiversAll)
    distSpatial0.to_csv(f1, index=True, header=True)

# attribute scenarios
scenarios = list(config['scenarios'].keys())

# run only those scenarios specified to run in config file
scenarios =[x for x in scenarios if config['scenarios'][x]]

# pairing/regionalization algorithms
functions = {
             'gower': funcs_dist_new,
             'urf': funcs_dist_new,
             'kmeans': funcs_clust,
             'kmedoids': funcs_clust,
             'hdbscan': funcs_clust,
             'birch': funcs_clust,
             }
funcs = functions.keys()

# run only those methods specified to run in config file
funcs = [x for x in funcs if config['algorithms'][x]]

# loop through regionalization algorithms and scenarios to generate donor-receiver pairings
for func1 in funcs:
    for scenario in scenarios:
        outfile = 'output/donor_' + scenario + '_' + func1 + '_' + config['huc']+'_'+config['ver']+'.csv'
        if os.path.exists(outfile):
            print('Pair file already exist: ' + str(outfile))
            #value = input("Rerun? Enter Y/y to rerun or any other key to skip: ")
            #if ((value == '') or (value.upper()[0] != "Y")):
            print('Skip the current run: ' + func1 + ' ' + scenario)
            continue

        print("\n======== Identify donors for: " + func1 + "  " + scenario +" =============\n")       
        start_time = time.time()
        dfDonorAll = functions[func1].func(config, dfAttrAll,scenario, distSpatial0, func1)           
        print("\nTotal processing time for (" + func1 + ", " + scenario +"): --- %s seconds ---" % (time.time() - start_time))  
        
        # save donor receiver pairing to csv file    
        dfDonorAll.to_csv(outfile, index=False, float_format='%.3f')

