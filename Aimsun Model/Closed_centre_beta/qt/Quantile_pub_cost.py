import numpy as np
import pandas as pd
import os
import warnings
import sys
warnings.filterwarnings("ignore")
import subprocess
import shutil
po = 1
for i in range(5,9):
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/AM/"+str(i)+"/amcost_final.csv","/home/angelo/PycharmProjects/AV/Data/AM/quantileCalculation/pub_cost/costs/am_cost_"+str(i)+".csv")
    shutil.copy("/home/angelo/PycharmProjects/AV/Data/PM/" + str(i) + "/pmcost_final.csv","/home/angelo/PycharmProjects/AV/Data/PM/quantileCalculation/pub_cost/costs/pm_cost_" + str(i) + ".csv")


# In[5]:



pub_cost_am = pd.DataFrame(columns=["origin_zone","destination_zone"])
pub_cost_pm = pd.DataFrame(columns=["origin_zone","destination_zone"])
# In[7]:

qt = 0.6475
qt_pm = 0.6170
print(qt)
print(qt_pm)
print("process started")
for cost_am_path, cost_pm_path, in zip(["/home/angelo/PycharmProjects/AV/Data/AM/quantileCalculation/pub_cost/costs/" + file for file in sorted(os.listdir("/home/angelo/PycharmProjects/AV/Data/AM/quantileCalculation/pub_cost/costs/"))],
                                                              ["/home/angelo/PycharmProjects/AV/Data/PM/quantileCalculation/pub_cost/costs/" + file for file in sorted(os.listdir("/home/angelo/PycharmProjects/AV/Data/PM/quantileCalculation/pub_cost/costs"))]):
    #read files
    cost_am=pd.read_csv(cost_am_path,  names=["origin_zone",	"destination_zone",	"distance",	"car_cost_erp",	"car_ivt","pub_ivt","pub_walkt","pub_wtt","pub_cost","avg_transfer","pub_out"])
    cost_pm=pd.read_csv(cost_pm_path,  names=["origin_zone",	"destination_zone",	"distance",	"car_cost_erp",	"car_ivt","pub_ivt","pub_walkt","pub_wtt","pub_cost","avg_transfer","pub_out"])
    #print(cost_am_path)
    cost_am_table = cost_am
    cost_pm_table = cost_pm
    cost_am_table = cost_am_table[["origin_zone", "destination_zone", "pub_cost"]]
    cost_pm_table = cost_pm_table[["origin_zone", "destination_zone", "pub_cost"]]
    if po == 1:
        pub_cost_am = pd.concat([pub_cost_am.set_index(["origin_zone","destination_zone"]), cost_am_table.set_index(["origin_zone","destination_zone"])], axis=1)
        pub_cost_pm = pd.concat([pub_cost_pm.set_index(["origin_zone", "destination_zone"]),
                                 cost_pm_table.set_index(["origin_zone", "destination_zone"])], axis=1)
        po = po+1
    else:
        pub_cost_am = pd.concat([pub_cost_am,cost_am_table.set_index(["origin_zone", "destination_zone"])], axis=1)
        pub_cost_pm = pd.concat([pub_cost_pm, cost_pm_table.set_index(["origin_zone", "destination_zone"])], axis=1)


quantile = qt
vector_am_quantile=pub_cost_am.quantile(q=quantile,axis=1)#.unstack()
vector_pm_quantile=pub_cost_pm.quantile(q=qt_pm,axis=1)#.unstack()
cost_am_quantile = pd.concat([cost_am.set_index(["origin_zone","destination_zone"]), vector_am_quantile], axis=1).reset_index()
ind = round(qt, 5)
indpm = round(qt_pm, 5)


cost_am_quantile["pub_cost"] = cost_am_quantile[ind]
cost_am_quantile = cost_am_quantile.drop(columns=[ind])
cost_pm_quantile = pd.concat([cost_pm.set_index(["origin_zone","destination_zone"]), vector_pm_quantile], axis=1).reset_index()
cost_pm_quantile["pub_cost"] = cost_pm_quantile[indpm]
cost_pm_quantile = cost_pm_quantile.drop(columns=[indpm])

cost_am_quantile.to_csv("/home/angelo/PycharmProjects/AV/Data/AM/quantileCalculation/pub_cost/amcost_output_quantile.csv", index = False, header = False)
cost_pm_quantile.to_csv("/home/angelo/PycharmProjects/AV/Data/PM/quantileCalculation/pub_cost/pmcost_output_quantile.csv", index = False, header = False)
print(np.average(cost_am_quantile["pub_cost"]))
print(np.average(cost_pm_quantile["pub_cost"]))

#print((np.average(pub_cost_am.iloc[:,0])+np.average(pub_cost_am.iloc[:,1])+np.average(pub_cost_am.iloc[:,2])+np.average(pub_cost_am.iloc[:,3])+np.average(pub_cost_am.iloc[:,4])+np.average(pub_cost_am.iloc[:,5])+np.average(pub_cost_am.iloc[:,6])+np.average(pub_cost_am.iloc[:,7]))/8)
#print((np.average(pub_cost_pm.iloc[:, 0]) + np.average(pub_cost_pm.iloc[:, 1]) + np.average(
#    pub_cost_pm.iloc[:, 2]) + np.average(pub_cost_pm.iloc[:, 3]) + np.average(pub_cost_pm.iloc[:, 4]) + np.average(
#    pub_cost_pm.iloc[:, 5]) + np.average(pub_cost_pm.iloc[:, 6]) + np.average(pub_cost_pm.iloc[:, 7])) / 8)

import psycopg2
patham = r'/home/angelo/PycharmProjects/AV/Data/AM/quantileCalculation/pub_cost'
pathpm = r'/home/angelo/PycharmProjects/AV/Data/PM/quantileCalculation/pub_cost'
pathop = r'/home/angelo/PycharmProjects/AV/Data/Starting tables'
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
cursor_obj.execute("COPY demand.amcosts FROM '" + patham +"/amcost_output_quantile.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.learned_amcosts FROM '" + patham +"/amcost_output_quantile.csv' DELIMITER ','") #tested on the query tool, it works
cursor_obj.execute("COPY demand.learned_amcosts_calib FROM '" + patham +"/amcost_output_quantile.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.pmcosts FROM '" + pathpm +"/pmcost_output_quantile.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.learned_pmcosts FROM '" + pathpm +"/pmcost_output_quantile.csv' DELIMITER ','")  # tested on the query tool, it works
cursor_obj.execute("COPY demand.learned_pmcosts_calib FROM '" + pathpm +"/pmcost_output_quantile.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.opcosts FROM '" + pathop +"/opcost_av.csv' DELIMITER ','")
cursor_obj.execute("COPY demand.learned_opcosts FROM '" + pathop +"/opcost_av.csv' DELIMITER ','")  # tested on the query tool, it works
cursor_obj.execute("COPY demand.learned_opcosts_calib FROM '" + pathop +"/opcost_av.csv' DELIMITER ','")
con.commit()
con.close()
# export database
subprocess.call(['/usr/bin/pg_dump', '--file', "/home/angelo/PycharmProjects/AV/Data/quantile"+str(qt), '--host', "localhost", '--port', "5432", '--username', "postgres", '--no-password', '--verbose', '--format=c', '--blobs', "simmobcity - all licenses"])


