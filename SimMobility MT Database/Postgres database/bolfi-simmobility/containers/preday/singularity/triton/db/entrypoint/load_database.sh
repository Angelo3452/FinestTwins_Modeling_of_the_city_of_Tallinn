#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 --username "postgres" --dbname "postgres" <<-EOSQL
	CREATE USER symmobility WITH PASSWORD 'symmobility1#2';
	GRANT postgres TO symmobility;
	CREATE DATABASE simmobtallinn OWNER symmobility;
	GRANT ALL PRIVILEGES ON DATABASE simmobtallinn TO symmobility;
EOSQL

TALLINN_DUMP_FILE='/scratch/work/kuzmanv1/R/bolfi_framework/containers/preday/singularity/triton/db/tallinn_03052022.dump'

pg_restore -U symmobility -d simmobtallinn -Fc -j 4 --no-owner ${TALLINN_DUMP_FILE}
echo "The database has been loaded."


