#!/bin/bash
RUNTIME_PATH=$1
WORKER_NAME=$2
WORKER_PATH=${RUNTIME_PATH}/${WORKER_NAME}

## Remove footprints from previous run
rm ${WORKER_PATH}/activity_schedule*

cd ${WORKER_PATH}

COUNT_ARCHIVES=$(ls -1 ./ | grep '^behavior_vc_archive*' | wc -l)
COUNT_ARCHIVES=`expr ${COUNT_ARCHIVES} + 1`

## Zip current behaviour_vc
cp -r "behavior_vc" "behavior_vc_archive_${COUNT_ARCHIVES}"
zip -r "behavior_vc_archive_${COUNT_ARCHIVES}.zip" "behavior_vc_archive_${COUNT_ARCHIVES}"
rm -r "behavior_vc_archive_${COUNT_ARCHIVES}"

## Execute
singularity exec -B /scratch:/scratch simmobility.sif /simmobility-prod/dev/Basic/Debug/SimMobility_Medium simulation.xml simrun_MidTerm.xml
