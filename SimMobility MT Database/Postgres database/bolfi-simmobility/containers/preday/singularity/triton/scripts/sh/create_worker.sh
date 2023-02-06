#!/bin/bash
CONFIG_PATH=$1
RUNTIME_PATH=$2
WORKER_NAME=$3
WORKER_PATH=${RUNTIME_PATH}/${WORKER_NAME}
IMAGE_NAME=$4

## Please note that the singularity images are run without database in the context of Triton. The sif need to be replaced in order to consider complete setup with psql database

MIDTERM_XML=simrun_MidTerm.xml
SIMULATION_XML=simulation.xml

# Delete if exists and (re-)create the worker's path // This is not valid for singularity, as the creation is handled in slrm (triton) script
#if [ -d "$WORKER_PATH" ]; then rm -Rf $WORKER_PATH; fi
#mkdir -p $WORKER_PATH

# Copy necessary files/scripts
cp -r ${CONFIG_PATH}/image/simmobility/* ${WORKER_PATH}/
cp ${CONFIG_PATH}/scripts/sh/docker_exec_simulation.sh ${WORKER_PATH}/
cp ${CONFIG_PATH}/scripts/sh/local_update_param.sh ${WORKER_PATH}/

# Update the configuration to consider lua files in the worker's path
FIND_TERM='<model_scripts path='
REPLACE_TERM='<model_scripts path="'${WORKER_PATH}'/behavior_vc" format="lua">'
sed -i'.bck' -e "s/.*${FIND_TERM}.*/${REPLACE_TERM//\//\\/}/" "${WORKER_PATH}/${MIDTERM_XML}"

# Update database port simulation.xml
## NOTE: please make sure POSTGRES_PORT is exported global variable
FIND_TERM='<port value='
REPLACE_TERM='<port value="'${POSTGRES_PORT}'"/>'
sed -i'.bck' -e "s/.*${FIND_TERM}.*/${REPLACE_TERM//\//\\/}/" "${WORKER_PATH}/${SIMULATION_XML}"
