#!/bin/bash

# submit HUC-01 simulations with regionalized parameters

dir1=/home/***REMOVED***/NextGen_Regionalization/agu2022
dir2=/local/ngen/data/fihm

# runs to be submitted (dummy is for moving the output of the last run)
#runs=("CFE+TOPMODEL_camels_kge" "CFE_hlr_nse" "TOPMODEL_hlr_nse" "TOPMODEL_camels_nse" "CFE_camels_nse" "CFE+TOPMODEL_hlr_nse" "CFE+TOPMODEL_camels_nse" "dummy")
runs=("CFE+TOPMODEL_camels_nse" "dummy")

# run currently going on ("none" if no run is currently running)
run0="none"

# loop through runs to be submitted
for run1 in ${runs[@]}; do

   if [ "${run0}" != "none" ]; then
      echo "Wait for current ngen run ${run0} to finish ... "
      while [ "$(ps -u ***REMOVED*** | grep ngen | wc -l)" != 0 ]; do
          sleep 60
      done

      echo "Current run ${run0} finished; move output files to designated output directory"
      #mkdir -p ${dir1}/ngen_outputs/${run0}
      #mv ${dir2}/cat-*.csv ${dir1}/ngen_outputs/${run0}/.
      #mv ${dir2}/nex-*.csv ${dir1}/ngen_outputs/${run0}/.
      #mv ${dir2}/tnx-*.csv ${dir1}/ngen_outputs/${run0}/.

      mkdir -p ${dir2}/output/${run0}
      mv ${dir2}/cat-*.csv ${dir2}/output/${run0}/.
      mv ${dir2}/nex-*.csv ${dir2}/output/${run0}/.
      mv ${dir2}/tnx-*.csv ${dir2}/output/${run0}/.      
   fi

   if [ "${run1}" != "dummy" ]; then
      run0=${run1}
      cd ${dir2}
      echo "Submit new run ${run1}"
      nohup time mpirun -np 64 /local/ngen/bin/ngen data/hydrofabric/catchment_data.geojson all data/hydrofabric/nexus_data.geojson all realization/${run1}.json data/huc01_partitions_64.json > ${run1}.out 2>&1&
      echo "============================================="
      echo
   else
      echo "All runs completed!"
   fi
done