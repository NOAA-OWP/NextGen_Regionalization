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
# 2) donor and receivers are allowed to have different hydrofabric data (but not recommended)

# external packages
import yaml
import pandas as pd
import os.path
import time

# local functions for the algorithms 
import sys
sys.path.append('../algorithm/')
import funcs_clust
import funcs_dist
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
print('processing huc' + config['huc'] + ', hydrofab version ' + config['ver'])

# read attributes (donors & receivers)
dfAttrAll = pd.read_csv(config['inputs']['file_attrs_data'])
dfAttrAll['tag'] = 'receiver'

# determine which catchments are donors (based on the donor gage list and crowsswalk table)
gages = pd.read_csv(config['inputs']['file_donor_list'], header=None,dtype='str').iloc[:,0].tolist()
cwt = pd.read_csv(config['inputs']['file_crosswalk'],dtype={'gages':'str'})
donorsAll = cwt.loc[cwt['gages'].isin(gages)]['id'].tolist()

# the remaining catchments are receivers
dfAttrAll.loc[dfAttrAll['id'].isin(donorsAll),'tag'] = 'donor'
receiversAll = dfAttrAll.loc[dfAttrAll['tag']=="receiver"]['id'].tolist()

print('Total number of catchments: ' + str(dfAttrAll.shape[0]))
print('Total number of donor catchments: ' + str(len(donorsAll)))
print('Total number of receiver catchments: ' + str(len(receiversAll)))
print('Total number of calibration basins: ' + str(len(set(cwt.loc[cwt['gages'].isin(gages)]['gages'].tolist()))))

# sort the rows so that donors are at the top
dfAttrAll = dfAttrAll.sort_values(by="tag")

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
    distSpatial0 = my_utils.calculate_spatial_distance(shp_file=config['inputs']['file_hydrofab'], 
                                                       donors=donorsAll, receivers=receiversAll)
    distSpatial0.to_csv(f1, index=True, header=True)

# attribute scenarios
scenarios = list(config['scenarios'].keys())

# run only those scenarios specified to run in config file
scenarios =[x for x in scenarios if config['scenarios'][x]]

# pairing/regionalization algorithms
functions = {
             'proximity': funcs_dist,
             'gower': funcs_dist,
             'urf': funcs_dist,
             'kmeans': funcs_clust,
             'kmedoids': funcs_clust,
             'hdbscan': funcs_clust,
             'birch': funcs_clust,
             }
funcs = functions.keys()

# run only those methods specified to run in the config file
funcs = [x for x in funcs if config['algorithms'][x]]

# loop through regionalization algorithms and scenarios to generate donor-receiver pairings for each algorithm/scenario combination
for func1 in funcs:
    for scenario in scenarios:
        if func1 == 'proximity':
            scenario = 'spatial'
        outfile = config['inputs']['outdir'] + '/donor_' + scenario + '_' + func1 + '_huc' + config['huc']+'_v'+config['ver']+'.csv'
        if os.path.exists(outfile):
            print('Pair file already exist: ' + str(outfile))
            print('Skip the current run: ' + func1 + ' ' + scenario)
            continue

        print("\n======== Identify donors for: " + scenario + " " + func1 +" =============\n")       
        start_time = time.time()
        dfDonorAll = functions[func1].func(config, dfAttrAll,scenario, distSpatial0, func1)           
        print("\nTotal processing time for (" + scenario + " " + func1 + "): --- %s seconds ---\n" % (time.time() - start_time))  
        
        # save donor receiver pairing to csv file    
        if not os.path.exists(os.path.dirname(outfile)):
            os.makedirs(os.path.dirname(outfile))
        dfDonorAll.to_csv(outfile, index=False, float_format='%.3f')

