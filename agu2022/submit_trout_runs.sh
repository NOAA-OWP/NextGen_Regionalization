#!/bin/bash

# submit trout runs for different scenarios

dir1=/home/***REMOVED***/NextGen_Regionalization/agu2022
dir2=/local/ngen/data/fihm

#echo "Wait for ngen to finish ..."
#while [ "$(ps -u ***REMOVED*** | grep ngen | wc -l)" != 0 ]; do
#   sleep 20m
#done

# runs to be submitted (dummy is for moving the output of the last run)
runs=("TOPMODEL_camels_nse" "CFE_camels_nse" "CFE+TOPMODEL_hlr_nse" "CFE+TOPMODEL_camels_nse" "dummy")

# run currently going on ("none" if no run is currently running)
run0="none"

# loop through runs to be submitted
for run1 in ${runs[@]}; do

   if [ "${run0}" != "none" ]; then
      echo "Wait for current trout run ${run0} to finish ... "
      while [ "$(ps -u ***REMOVED*** | grep python | wc -l)" != 0 ]; do
          sleep 60
      done

      echo "Current run ${run0} finished" 
   fi

   if [ "${run1}" != "dummy" ]; then
      run0=${run1}
      cd ${dir2}
      # check if ngen output has already been produced for the current run
      FILE=/etc/resolv.conf
      echo "Wait for current ngen run ${run1} to finish ..."
      while [ ! -d "/local/ngen/data/fihm/output/${run1}" ]; do
         sleep 100
      done
      echo "ngen run ${run} finished"
      echo "Submit new run ${run1}"
      nohup python -m ngen_routing.ngen_main -f data/routing/${run1}.yaml > ${run1}.out 2>&1&
      echo "============================================="
      echo
   else
      echo "All runs completed!"
   fi
done