#!python
from datetime import datetime
import subprocess
import time
import shutil
import rpy2.robjects as robjects
import pandas as pd
import os
iteration = 11

print('start Ride replication 1')
p = subprocess.Popen('python3 /home/angelo/Desktop/"Aimsun Ride"/samples/Operators/DRT-Taxi/operator.py --operator /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/operator_definition.json', shell=True)
subprocess.call('/home/angelo/Aimsun_Next_22/aconsole -log --verbose --project /home/angelo/Desktop/Tallinn/Model/av.ang --command execute --target 2412421 --mod-scenario /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/scenario.json --mod-offset-time -60 | tee home\angelo\Desktop\Tallinn\Model\OUTPUT1.txt', shell=True)  #| tee home\angelo\Desktop\Tallinn\Model\OUTPUT.txt', shell=True)
p.terminate()
shutil.copy("/home/angelo/PycharmProjects/AV/Scripts/final_data.csv", "/home/angelo/PycharmProjects/AV/archive/analytics/"+str(iteration)+"/statistics_rep1.csv")

print('start Ride replication 4')
p = subprocess.Popen('python3 /home/angelo/Desktop/"Aimsun Ride"/samples/Operators/DRT-Taxi/operator.py --operator /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/operator_definition.json', shell=True)
subprocess.call('/home/angelo/Aimsun_Next_22/aconsole -log --verbose --project /home/angelo/Desktop/Tallinn/Model/av.ang --command execute --target 2411283 --mod-scenario /home/angelo/Desktop/"Aimsun Ride"/samples/Samples/CampNou/Scenarios/ScenarioDRT-Taxi/scenario_pm.json --mod-offset-time -60 | tee home\angelo\Desktop\Tallinn\Model\OUTPUT4.txt', shell=True)  #| tee home\angelo\Desktop\Tallinn\Model\OUTPUT.txt', shell=True)
p.terminate()
shutil.copy("/home/angelo/PycharmProjects/AV/Scripts/final_data.csv", "/home/angelo/PycharmProjects/AV/archive/analytics/"+str(iteration)+"/statistics_rep4.csv")

