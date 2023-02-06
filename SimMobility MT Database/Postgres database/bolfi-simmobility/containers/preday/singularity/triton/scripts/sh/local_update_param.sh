#!/bin/bash

MODEL_FILE=$1
PARAM=$2
PARAM_VALUE=$3
MODEL_NAME=$4
RUNTIME_PATH=$5
WORKER_NAME=$6

if [ -z "${WORKER_NAME}" ] ; then
  #grep -qF -- "$PARAM" "$MODEL_FILE" || echo "$PARAM" >> "$MODEL_FILE"
  sed -i'.bck' -e "s/.*${PARAM}.*/${PARAM_VALUE}/" "${MODEL_FILE}"
else
  #grep -qF -- "$PARAM" "$MODEL_FILE" || echo "$PARAM" >> "${RUNTIME_PATH}/${WORKER_NAME}/${MODEL_NAME}"
  sed -i'.bck' -e "s/.*${PARAM}.*/${PARAM_VALUE}/" "${RUNTIME_PATH}/${WORKER_NAME}/behavior_vc/${MODEL_NAME}"
fi
