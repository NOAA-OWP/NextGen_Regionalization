import pandas as pd
import os
from datetime import datetime
from hydrotools.events.event_detection import decomposition as ev

# Use pandas to resample the data
from pandas import Grouper


# Use a helper function to detect events at multiple sites
def list_events_helper(mi_series, level, halflife, window):
    """Reduce multi-index series before applying event detection."""
    return ev.list_events(mi_series.droplevel(level), halflife, window)

runs = ["camels","hlr"]
models = ["CFE","TOPMODEL","CFE+TOPMODEL"]

runs = ["default"]
models = ["CFE","TOPMODEL"]

runs = ["obs"]
models = [""]

for r1 in runs:
    for m1 in models:
        
        # scenario
        scenario = m1 + "." + r1
        print(scenario)

        # read modeled flow
        f1 = '/home/***REMOVED***/reg_ngen/fihm/output/flow_20081001_20140930_noah_owp_' + scenario + '.csv'
        if scenario=="obs.":
           f1 = '/home/***REMOVED***/reg_ngen/fihm/output/usgs_hourly_flow_20081001_20140930_huc01.csv'

        if not os.path.exists(f1):
            break     
    
        df1 = pd.read_csv(f1)

        # melt the data frame
        df2 = pd.melt(df1, id_vars="validTime")

        # change columns names and types for hydrotools.events
        df2 = df2.rename(columns={"validTime": "value_time", "variable": "usgs_site_code"})
        df2['value_time'] = pd.to_datetime(df2['value_time'])
        df2['usgs_site_code'] = df2['usgs_site_code'].astype('category')

        # Resample to hourly, keep first measurement in each 1-hour bin
        df2 = df2.groupby(['usgs_site_code',Grouper(key='value_time', freq='H')]).first().ffill()

        # Detect events
        events = df2['value'].groupby(level='usgs_site_code').apply(list_events_helper,
            level='usgs_site_code', halflife='6H', window='7D')

        # write the events to CSV
        events.to_csv(f1.replace(".csv","_events.csv"))