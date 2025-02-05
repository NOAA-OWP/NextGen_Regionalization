# Parameter/Fomulation Regionalization for NextGen

**Description**: NextGen regionalization is adapted from the NWMv3.0 regionalization framework, which uses a physical similarity-based approach to transfer model formulations and parameters from calibrated basins (donors) to uncalibrated areas (receivers). Currently, the framework considers two different sets of basin attributes to represent physical similarity: Hydrologic Landscape Regions (HLR, Winter 2001 and Wolock et al. 2004 ) and Catchment Attributes and Meteorology for Large-sample Studies (CAMELS, Addor et al. 2017). 

**List of documents & presentations**
- [Documentation for NextGen Regionalization](/doc/Summary_NextGen_Regionalization_Feb2024.pdf)
- [AMS 2024 presentation slides](https://github.com/NOAA-OWP/OWP-Presentations/blob/33b411be176994a9687ffa02e255d55dbcd8786e/AMS/AMS%202024/Oral%20Presentations/Liu_AMS%202024.pdf)
- [AGU 2022 presentation slides](https://github.com/NOAA-OWP/OWP-Presentations/blob/33b411be176994a9687ffa02e255d55dbcd8786e/AGU/AGU%202022/Oral%20Presentations/Liu_AGU_2022.pdf)

    
**Notes about the scripts in subfolders**

  - **./NWMv4**: latest developments related to transforming NextGen regionalization into a new framework towards the operational NWMv4, with the regionalization algorithms coded in Python and the calculation of catchment attributes still coded in R. 
  - **./scripts**: earlier scripts related to computing catchment attributes and generating donor-receiver pairings 
  - **./fihm/scripts**: earlier scripts related to generating ngen realization files and processing/analyzing the outputs for the FIHM MVP
  - **./agu2022**: earlier scripts related to deriving the attributes, conducting donor-receier pairing in HUC-01, generating realization files for various scenarios, and evaluating/visualizing the results for the AGU 2022 MVP

![workflow](https://github.com/NOAA-OWP/NextGen_Regionalization/blob/master/doc/NextGen_regionalization_workflow.png)

![algorithms](https://github.com/NOAA-OWP/NextGen_Regionalization/blob/master/doc/NextGen_regionalization_algorithms.png)

## Dependencies

The scripts use the python libary [scikit-learn](https://scikit-learn.org/stable/modules/clustering.html) as well as R libraries including the zonal package in the [hydrofabric](https://github.com/NOAA-OWP/hydrofabric) tools 

## Credits and references

**Acknowlegement**: The NextGen regionalization framework was adapted from the NWM v3 regionalization work and has benefited from discussions and help from the NWM calibration team, as well as the valuable feedback from the RFCs.

**References**:

Addor, N., A.J. Newman, N. Mizukami, and M.P. Clark, 2017. The CAMELS data set: catchment attributes and meteorology for large-sample studies. Hydrol. and Earth Syst. Sci., 21, 5293-5313, doi:10.5194/hess-21-5293-2017

Winter, T. C. (2001) The concept of hydrologic landscapes. J. Am. Water Resour. Assoc. 37, 335–349.

Wolock, D. M., Winter, T. C. & McMahon, M. (2004) Delineation and evaluation of hydrologic-landscape regions in the United States using geographic information system tools and multivariate statistical analyses. Environ. Manage. 34, S71–S88.
