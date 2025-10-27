from datetime import datetime
import subprocess
import time
import shutil
import rpy2.robjects as robjects
import pandas as pd
import os

import numpy as np
import csv
import psycopg2
qt = "q595"
patham = r'/home/angelo/PycharmProjects/AV/Data/AM/quantileCalculation/pub_cost/'+qt+'/'
pathpm = r'/home/angelo/PycharmProjects/AV/Data/PM/quantileCalculation/pub_cost/'+qt+'/'
pathop = r'/home/angelo/PycharmProjects/AV/Data/Starting tables/'
con = psycopg2.connect(database = "simmobcity - all licenses", user = "postgres", password = "Beltorak", host = "localhost", port = '5432') #connect to the database
cursor_obj = con.cursor() # needed to feed postgres commands - https://www.commandprompt.com/education/how-to-connect-to-postgresql-database-server-using-python/
cursor_obj.execute("TRUNCATE TABLE demand.amcosts")
cursor_obj.execute("TRUNCATE TABLE demand.learned_amcosts")
cursor_obj.execute("TRUNCATE TABLE demand.learned_amcosts_calib")
cursor_obj.execute("TRUNCATE TABLE demand.pmcosts")
cursor_obj.execute("TRUNCATE TABLE demand.learned_pmcosts")
cursor_obj.execute("TRUNCATE TABLE demand.learned_pmcosts_calib")
cursor_obj.execute("TRUNCATE TABLE demand.opcosts")
cursor_obj.execute("TRUNCATE TABLE demand.learned_opcosts")
cursor_obj.execute("TRUNCATE TABLE demand.learned_opcosts_calib")
cursor_obj.execute("COPY demand.amcosts FROM '" + patham +"amcost_final.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.learned_amcosts FROM '" + patham +"amcost_final.csv' DELIMITER ','") #tested on the query tool, it works
cursor_obj.execute("COPY demand.learned_amcosts_calib FROM '" + patham +"amcost_final.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.pmcosts FROM '" + pathpm +"pmcost_final.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.learned_pmcosts FROM '" + pathpm +"pmcost_final.csv' DELIMITER ','")  # tested on the query tool, it works
cursor_obj.execute("COPY demand.learned_pmcosts_calib FROM '" + pathpm +"pmcost_final.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.opcosts FROM '" + pathop +"opcost_av.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.learned_opcosts FROM '" + pathop +"opcost_av.csv' DELIMITER ','")  # tested on the query tool, it works
cursor_obj.execute("COPY demand.learned_opcosts_calib FROM '" + pathop +"opcost_av.csv' DELIMITER ','")
con.commit()
con.close()
# export database
subprocess.call(['/usr/bin/pg_dump', '--file', "/home/angelo/PycharmProjects/AV/Data/quantile"+qt+"_2", '--host', "localhost", '--port', "5432", '--username', "postgres", '--no-password', '--verbose', '--format=c', '--blobs', "simmobcity - all licenses"])



