#!/bin/bash

set -e

## ------------------------
RUN_FINGERPRINT="test_run_1"
REPLICATION=1
CONFIG_FILEPATH="config/preday/config_server_tallinn_tripsmode_qn.yml"
OUTPUT_PATH="output/preday/city_tallinn_test_run"
SRC_FILE="app_preday_calibration.R"
ENTRYPOINT="containers/preday/singularity/triton/db/entrypoint"
IMAGE_FILE="containers/preday/singularity/triton/image/postgres/postgres_latest.sif"
# DB_FILE="containers/preday/singularity/triton/db/tallinn_03052022.dump"
S_MODEL="rf"
A_FUNC="ei"
## ------------------------

export BOLFI_HOME='./'
export PREDAY_RUNTIME="./containers/preday/singularity/triton/runtime/${RUN_FINGERPRINT}"
export POSTGRES_PORT=5432

## Create postgres folders
mkdir -p ${PREDAY_RUNTIME}/var/{lib,run}

# Run postgres in a singularity image, forward output to files, catch PID for process
singularity run --env POSTGRES_PASSWORD=mysecretpassword --env LC_ALL=C --env PGPORT=${POSTGRES_PORT} -B /scratch:/scratch -B ${PREDAY_RUNTIME}/var/lib:/var/lib/postgresql -B ${PREDAY_RUNTIME}/var/run:/var/run -B ${ENTRYPOINT}:/docker-entrypoint-initdb.d ${IMAGE_FILE} 2> ${PREDAY_RUNTIME}/postgres.err 1> ${PREDAY_RUNTIME}/postgres.out &
POSTGRES_PID=$!

# Give postgres few seconds to initialize
sleep 10

# Set up a trap so that postgres will be killed when job finishes
trap "kill $POSTGRES_PID ; exit" TERM EXIT

# module load r/3.6.3-python3

Rscript --vanilla "$SRC_FILE" -f "${RUN_FINGERPRINT}" -m "${S_MODEL}" -u "${A_FUNC}" -C "$CONFIG_FILEPATH" -O "$OUTPUT_PATH/$REPLICATION"
