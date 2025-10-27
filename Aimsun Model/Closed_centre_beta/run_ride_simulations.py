#!python
from datetime import datetime
import subprocess
import time
import shutil
import rpy2.robjects as robjects
import pandas as pd
import os

import numpy as np
import csv
print("start time: "+str(datetime.now()))
start = datetime.now()
num = 2
skip1 = True
skip2 = True
skip3 = True
iteration = 11

aimsuntosimmob = True
if num > 0 and skip1 == False:
    print('start Ride replication 1')
    p = subprocess.Popen('python3 /home/angelo/Desktop/"Aimsun Ride"/samples/Operators/DRT-Taxi/operator.py --operator /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/operator_definition.json', shell=True)
    subprocess.call('/home/angelo/Aimsun_Next_22/aconsole -log --verbose --project /home/angelo/Desktop/Tallinn/Model/av.ang --command execute --target 2412421 --mod-scenario /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/scenario.json --mod-offset-time -60', shell=True)# | tee home\angelo\Desktop\Tallinn\Model\OUTPUT1.txt', shell=True)  #| tee home\angelo\Desktop\Tallinn\Model\OUTPUT1.txt', shell=True)
    p.terminate()
    print('end subprocess')
    print(datetime.now())
    print(datetime.now()-start)
    time.sleep(60) # pauses for 1 min
    shutil.copy("/home/angelo/PycharmProjects/AV/final_data.csv", "/home/angelo/PycharmProjects/AV/archive/analytics/"+str(iteration)+"/statistics_rep1.csv")
    shutil.copy("/home/angelo/Desktop/Aimsun Ride/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/request.json", "/home/angelo/PycharmProjects/AV/archive/analytics/" + str(iteration) + "/request.json")
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/_Skim_Car_2412421.csv /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/_Skim_Car_2412421.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder iteration
    shutil.copy("/home/angelo/PycharmProjects/AV/archive/analytics/"+str(iteration)+"/statistics_rep1.csv",
                "/home/angelo/PycharmProjects/AV/statistics_rep.csv")
    robjects.r.source("/home/angelo/PycharmProjects/AV/Scripts/statistics_ride.Rmd", encoding="utf-8") ## uses general file, no indexes to be corrected
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/am_wt.csv /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/av_wt_1.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/am_ivt.csv /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/av_ivt_1.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/avg_am_wt.txt /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/txt/avg_am_wt.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/avg_am_ivt.txt /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/txt/avg_am_ivt.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/ivtcountam_av.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/ivtcountam.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/wt_am_map.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/wt_am_map.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/wtcountam.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/wtcountam.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/Gwtcountam.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/Gwtcountam.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/am_wtgarages.txt /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/txt/am_wtgarages.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/wt_scatter_am.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/wt_scatter_am.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/ivt_scatter_am.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/ivt_scatter_am.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1


if num > 3:
    print('start Ride replication 4')
    p = subprocess.Popen('python3 /home/angelo/Desktop/"Aimsun Ride"/samples/Operators/DRT-Taxi/operator.py --operator /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/operator_definition.json', shell=True)
    subprocess.call('/home/angelo/Aimsun_Next_22/aconsole -log --verbose --project /home/angelo/Desktop/Tallinn/Model/av.ang --command execute --target 2411283 --mod-scenario /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/scenario_pm.json --mod-offset-time -60 | tee home\angelo\Desktop\Tallinn\Model\OUTPUT4.txt', shell=True)  #| tee home\angelo\Desktop\Tallinn\Model\OUTPUT.txt', shell=True)
    p.terminate()
    print('end subprocess')
    print(datetime.now())
    print(datetime.now()-start)
    time.sleep(60) # pauses for 1 min
    shutil.copy("/home/angelo/PycharmProjects/AV/final_data.csv", "/home/angelo/PycharmProjects/AV/archive/analytics/"+str(iteration)+"/statistics_rep4.csv")
    shutil.copy("/home/angelo/Desktop/Aimsun Ride/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/request_pm.json", "/home/angelo/PycharmProjects/AV/archive/analytics/" + str(iteration) + "/request_pm.json")
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/_Skim_Car_2411283.csv /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/_Skim_Car_2411283.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    ## now run R code to generate statistics from the final_data.csv
    shutil.copy("/home/angelo/PycharmProjects/AV/archive/analytics/"+str(iteration)+"/statistics_rep4.csv",
                "/home/angelo/PycharmProjects/AV/statistics_rep.csv")
    robjects.r.source("/home/angelo/PycharmProjects/AV/Scripts/statistics_ride_pm.Rmd", encoding="utf-8")  ##  ## uses general file, no indexes to be corrected
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pm_wt.csv /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/av_wt_1.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pm_ivt.csv /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/av_ivt_1.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/avg_pm_wt.txt /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/txt/avg_pm_wt.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/avg_pm_ivt.txt /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/txt/avg_pm_ivt.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/ivtcountpm_av.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/ivtcountpm.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/wt_pm_map.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/wt_pm_map.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/wtcountpm.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/wtcountpm.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/Gwtcountpm.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/Gwtcountpm.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pm_wtgarages.txt /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/txt/pm_wtgarages.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/wt_scatter_pm.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/wt_scatter_pm.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/ivt_scatter_pm.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/ivt_scatter_pm.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder 1

if aimsuntosimmob == True:
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/AM/"+str(iteration)+"/_Skim_Car_2412421.csv",
                "/home/angelo/PycharmProjects/AV/Data/_Skim_Car_2412421.csv")

    shutil.copy("/home/angelo/PycharmProjects/AV/Data/PM/"+str(iteration)+"/_Skim_Car_2411283.csv",
                "/home/angelo/PycharmProjects/AV/Data/_Skim_Car_2411283.csv")

    robjects.r.source("/home/angelo/PycharmProjects/AV/Scripts/avg car skim.Rmd", encoding="utf-8")  ## averages car skims from the iteration folder (e.g. it.1, it.2)

    os.remove('/home/angelo/PycharmProjects/AV/Data/_Skim_Car_2412421.csv')
    os.remove('/home/angelo/PycharmProjects/AV/Data/_Skim_Car_2411283.csv')
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/cartt_am.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/cartt_am.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/cartt_pm.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/cartt_pm.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/cartt_am.txt /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/txt/cartt_am.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/cartt_pm.txt /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/txt/cartt_pm.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/ivtcountpm_car.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/ivtcountpm_car.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/ivtcountam_car.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/ivtcountam_car.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/cartt_am_subset.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/cartt_am_subset.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/cartt_pm_subset.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/cartt_pm_subset.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/AVGam.csv",
                "/home/angelo/PycharmProjects/AV/Data/AM/"+str(iteration)+"/AVG.csv")
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/AVGpm.csv",
                "/home/angelo/PycharmProjects/AV/Data/PM/"+str(iteration)+"/AVG.csv")
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/AM/"+str(iteration)+"/static.csv",
                "/home/angelo/PycharmProjects/AV/staticam.csv")
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/PM/"+str(iteration)+"/static.csv",
                "/home/angelo/PycharmProjects/AV/staticpm.csv")

    subprocess.call('python3 /home/angelo/PycharmProjects/AV/Scripts/aimsun_to_simmobility_dynamic.py', shell=True) # (uses files in general folders, no need to update indexes) write aimsun skims in simmob table while overwriting missing tt with static ones
    # then run r script to calculate and write the av averages in amcost and pmcost
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/AM/"+str(iteration)+"/av_wt_1.csv",
                "/home/angelo/PycharmProjects/AV/Data/av_wt_am.csv")
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/PM/"+str(iteration)+"/av_wt_1.csv",
                "/home/angelo/PycharmProjects/AV/Data/av_wt_pm.csv")
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/AM/"+str(iteration)+"/av_ivt_1.csv",
                "/home/angelo/PycharmProjects/AV/Data/av_ivt_am.csv")
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/PM/"+str(iteration)+"/av_ivt_1.csv",
                "/home/angelo/PycharmProjects/AV/Data/av_ivt_pm.csv")

    robjects.r.source("/home/angelo/PycharmProjects/AV/Scripts/coordinates to garages.Rmd", encoding="utf-8") # verified, seems to be working as intended (no need to update indexes)

    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/am_wtavg.csv /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/am_wtavg.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pm_wtavg.csv /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/pm_wtavg.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/am_ivtavg.csv /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/am_ivtavg.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pm_ivtavg.csv /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/pm_ivtavg.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/amcost_final.csv /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/amcost_final.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pmcost_final.csv /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/pmcost_final.csv' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pmpubcost.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/pmpubcost.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/ampubcost.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/ampubcost.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/am_pubcost_avg.txt /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/txt/am_pubcost_avg.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pm_pubcost_avg.txt /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/txt/pm_pubcost_avg.txt' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/tt_comparison_pm.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/tt_comparison_pm.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/tt_comparison_am.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/tt_comparison_am.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/amcost_avivt.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/amcost_avivt.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pmcost_avivt.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/pmcost_avivt.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/amcost_carivt.png /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/png/amcost_carivt.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pmcost_carivt.png /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/png/pmcost_carivt.png' ### archiving
    subprocess.call(command, shell = True) # move skim to the replication folder


    os.remove('/home/angelo/PycharmProjects/AV/Data/AVGam.csv')
    os.remove('/home/angelo/PycharmProjects/AV/Data/AVGpm.csv')
    os.remove('/home/angelo/PycharmProjects/AV/Data/av_ivt_am.csv')
    os.remove('/home/angelo/PycharmProjects/AV/Data/av_ivt_pm.csv')
    os.remove('/home/angelo/PycharmProjects/AV/Data/av_wt_am.csv')
    os.remove('/home/angelo/PycharmProjects/AV/Data/av_wt_pm.csv')
    os.remove('/home/angelo/PycharmProjects/AV/staticam.csv')
    os.remove('/home/angelo/PycharmProjects/AV/staticpm.csv')
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/amcost_output.csv /home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/amcost_output.csv' ### archiving
    subprocess.call(command, shell = True)
    command = 'mv -f /home/angelo/PycharmProjects/AV/Data/pmcost_output.csv /home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/pmcost_output.csv' ### archiving
    subprocess.call(command, shell = True)
    import psycopg2
    patham = r'/home/angelo/PycharmProjects/AV/Data/AM/'+str(iteration)+'/'
    pathpm = r'/home/angelo/PycharmProjects/AV/Data/PM/'+str(iteration)+'/'
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
    cursor_obj.execute("COPY demand.amcosts FROM '" + patham +"/amcost_final.csv' DELIMITER ','")
    cursor_obj.execute("COPY demand.learned_amcosts FROM '" + patham +"/amcost_final.csv' DELIMITER ','") #tested on the query tool, it works
    cursor_obj.execute("COPY demand.learned_amcosts_calib FROM '" + patham +"/amcost_final.csv' DELIMITER ','")
    cursor_obj.execute("COPY demand.pmcosts FROM '" + pathpm +"/pmcost_final.csv' DELIMITER ','")
    cursor_obj.execute("COPY demand.learned_pmcosts FROM '" + pathpm +"/pmcost_final.csv' DELIMITER ','")  # tested on the query tool, it works
    cursor_obj.execute("COPY demand.learned_pmcosts_calib FROM '" + pathpm +"/pmcost_final.csv' DELIMITER ','")
    cursor_obj.execute("COPY demand.opcosts FROM '" + pathop +"/opcost_av.csv' DELIMITER ','")
    cursor_obj.execute("COPY demand.learned_opcosts FROM '" + pathop +"/opcost_av.csv' DELIMITER ','")  # tested on the query tool, it works
    cursor_obj.execute("COPY demand.learned_opcosts_calib FROM '" + pathop +"/opcost_av.csv' DELIMITER ','")
    con.commit()
    con.close()
    # export database
    subprocess.call(['/usr/bin/pg_dump', '--file', "/home/angelo/PycharmProjects/AV/Data/das"+str(iteration+2), '--host', "localhost", '--port', "5432", '--username', "postgres", '--no-password', '--verbose', '--format=c', '--blobs', "simmobcity - all licenses"])






