# Installation instructions

Detailed instructions on how to clone and run NextGen_Regionalization are provided below. The workflow requires the following Python libraries:
  * `yaml` (`pip install pyyaml`)
  * `scikit-learn` 
  * `scikit-learn-extra`
  * `matplotlib`
  * `geopandas` 
  * `hdbscan`
  * `numba` 

## Clone the repository
```
git clone https://github.com/NOAA-OWP/NextGen_Regionalization
cd NextGen_Regionalization
```

## Run the Example Regionalization Workflow ([presented at AMS 2024](https://docs.google.com/presentation/d/1xkYs-Hs3_cmIheLZ1Di7Dy3vWaiL3jmx/edit?usp=sharing&ouid=117267696082803250432&rtpof=true&sd=true))
0. Activate the virtual python environment with all dependencies installed

1. Go to the example directory
```
cd NWMv4/ams2024 
```

2. Download the [hydrofabric data](https://www.lynker-spatial.com/#hydrofabric/v20.1/gpkg/) for HUC 12 (Texas-Gulf Region) to `NextGen_Regionalization/NWMv4/datasets/nextgen_12.gpkg`

3. `python main_identify_donors.py`

4. Explore the output 
