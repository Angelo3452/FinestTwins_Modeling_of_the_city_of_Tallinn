#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import os
import warnings
import shutil
import similaritymeasures
import scipy.interpolate as interp
warnings.filterwarnings("ignore")

#### figure out how to import counter from other file, you knew this
# In[2]:
counter = 6
numfiles = counter+1
numminone = numfiles-1
numplusone = numfiles + 1
bw_adjust_morning=0.7
bw_adjust_afternoon=0.7
path_results = '/home/angelo/PycharmProjects/AV/Results'
path_input = '/home/angelo/PycharmProjects/AV/Data'

#drop Total column and row in skim matrix
def drop_total(skim_matrix):
    skim_matrix=skim_matrix.drop(columns=["Total"])
    skim_matrix=skim_matrix.drop(index=["Total"])
    return skim_matrix

def drop_unnamed(activity_schedule):
    activity_schedule=activity_schedule.drop(columns=["Unnamed: 14"])
    activity_schedule=activity_schedule.drop(columns=["Unnamed: 15"])
    activity_schedule=activity_schedule.drop(columns=["Unnamed: 16"])
    return activity_schedule


# In[3]:


#convert AimSun indexes and column names to numerical format
def skim_matrix_col_index_fix(skim_matrix):
    skim_matrix.columns=list(map(lambda x: int(x), skim_matrix.columns))
    skim_matrix.index=list(map(lambda x: int(x), skim_matrix.index))
    return skim_matrix


# In[4]:


#rename some columns in AimSun tables
def skim_matrix_renaming(skim_matrix):
    skim_matrix.columns=["origin_aimsun","destination_aimsun","travel_time_aimsun"]
    return skim_matrix


# In[5]:


#dataframes to store number of trips and travel times for each iteration
travel_times_am=pd.DataFrame()
trips_number_am=pd.DataFrame()
travel_times_pm=pd.DataFrame()
trips_number_pm=pd.DataFrame()


# In[6]:


#count number of unique O-D couples for number of trips
len_trips_number_am=[]
len_trips_number_pm=[]
len_tt_couples_am=[]


# In[7]:

i = 1

for skim_am_path, skim_pm_path, activity_schedule_path in zip([path_input+'/AM/moeInputs/' + file for file in sorted(os.listdir(path_input+"/AM/moeInputs"))],
                                                              [path_input+'/PM/moeInputs/' + file for file in sorted(os.listdir(path_input+"/PM/moeInputs"))],
                                                              [path_input+'/DAS/moeInputs/' + file for file in sorted(os.listdir(path_input+"/DAS/moeInputs"))]):

    skim_am=pd.read_csv(skim_am_path,index_col=0)
    skim_pm=pd.read_csv(skim_pm_path,index_col=0)
    activity_schedule=pd.read_csv(activity_schedule_path, header=0, index_col=False)#, names=["person_id","tour_no","tour_type","stop_no","stop_type","stop_location",
                                                                   #"stop_zone","stop_mode","primary_stop","arrival_time","departure_time",
                                                                   #"prev_stop_location","prev_stop_zone","prev_stop_departure_time","pid"])
    print(skim_am_path)
    print(skim_pm_path)
    print(activity_schedule_path)
    i = i+1
    #remove total columns and rows
    #skim_am=drop_total(skim_am)
    #skim_pm=drop_total(skim_pm)
    activity_schedule=drop_unnamed(activity_schedule)
    #print(sorted(os.listdir("activity_schedule")))
    #fix indexes (some of them are in string format)
    skim_am=skim_matrix_col_index_fix(skim_am)
    skim_pm=skim_matrix_col_index_fix(skim_pm)

    #str values to float
    skim_am=skim_am.applymap(lambda x: str(x))
    skim_pm=skim_pm.applymap(lambda x: str(x))
    skim_am=skim_am.applymap(lambda x: x.replace(",","."))
    skim_pm=skim_pm.applymap(lambda x: x.replace(",","."))
    skim_am=skim_am.applymap(lambda x: float(x)/60)
    skim_pm=skim_pm.applymap(lambda x: float(x)/60)

    #skim matrixes to tables - 2 columns - it works as intended
    skim_am_table=skim_am.stack().reset_index()
    skim_pm_table=skim_pm.stack().reset_index()

    #rename skim matrix column names
    skim_am_table=skim_matrix_renaming(skim_am_table)
    skim_pm_table=skim_matrix_renaming(skim_pm_table)

    #delete O-D couples where origin and destination zones are the same
    skim_am_table=skim_am_table[skim_am_table["origin_aimsun"]!=skim_am_table["destination_aimsun"]]
    skim_pm_table=skim_pm_table[skim_pm_table["origin_aimsun"]!=skim_pm_table["destination_aimsun"]]
    ### addition by serio
    # ### fix an handful of zones that are not in Aimsun, no idea why I decided to not put them, possibly they were in the middle of a park or similar
    activity_schedule.loc[activity_schedule["prev_stopZone"]==132, "prev_stopZone"] = 133
    activity_schedule.loc[activity_schedule["prev_stopZone"]==227, "prev_stopZone"] = 228
    activity_schedule.loc[activity_schedule["prev_stopZone"]==248, "prev_stopZone"] = 247
    activity_schedule.loc[activity_schedule["prev_stopZone"]==249, "prev_stopZone"] = 250
    activity_schedule.loc[activity_schedule["prev_stopZone"]==371, "prev_stopZone"] = 370
    activity_schedule.loc[activity_schedule["prev_stopZone"]==452, "prev_stopZone"] = 451
    activity_schedule.loc[activity_schedule["stopZone"]==132, "stopZone"] = 133
    activity_schedule.loc[activity_schedule["stopZone"]==227, "stopZone"] = 228
    activity_schedule.loc[activity_schedule["stopZone"]==248, "stopZone"] = 247
    activity_schedule.loc[activity_schedule["stopZone"]==249, "stopZone"] = 250
    activity_schedule.loc[activity_schedule["stopZone"]==371, "stopZone"] = 370
    activity_schedule.loc[activity_schedule["stopZone"]==452, "stopZone"] = 451
#   take away O-D couples along the diagonal also from activity_schedule
    activity_schedule = activity_schedule[activity_schedule["prev_stopZone"]!=activity_schedule["stopZone"]]


    #selection of am and pm time range and car transport mode
    activity_schedule_am=activity_schedule[(activity_schedule["arrival_time"]>=7.25) & (activity_schedule["arrival_time"]<=9.75)]
    activity_schedule_am=activity_schedule_am[(activity_schedule_am["stop_mode"]=="Taxi")]
    activity_schedule_pm=activity_schedule[(activity_schedule["arrival_time"]>=16.25) & (activity_schedule["arrival_time"]<=18.75)]
    activity_schedule_pm=activity_schedule_pm[(activity_schedule_pm["stop_mode"]=="Taxi")]

    #count number of trips for each zone couple -> the activity schedules are re-ordered to have prev_stop_zone clustered
    #-> it does sum everything (etc. arrival time), so anything other than prev_stop_zone, stop_zone and count is useless
    activity_schedule_am["for_count"]=1
    activity_schedule_pm["for_count"]=1
    activity_schedule_am["for_count"]=1
    activity_schedule_am.loc[activity_schedule_am["prev_stopZone"]==98, "prev_stopZone"] = 97
    activity_schedule_am.loc[activity_schedule_am["stopZone"]==98, "stopZone"] = 97
    activity_schedule_pm.loc[activity_schedule_pm["prev_stopZone"]==98, "prev_stopZone"] = 97
    activity_schedule_pm.loc[activity_schedule_pm["stopZone"]==98, "stopZone"] = 97
#    activity_schedule_am.to_csv(r"activity_sch_before count.csv")
    activity_schedule_am_count=activity_schedule_am.groupby(["prev_stopZone","stopZone"]).sum().reset_index()
    activity_schedule_pm_count=activity_schedule_pm.groupby(["prev_stopZone","stopZone"]).sum().reset_index()
    #activity_schedule_am_count.to_csv(r"activity_sch_after count.csv")
    skim_am_table_count=skim_am_table.groupby(["origin_aimsun","destination_aimsun"]).sum().reset_index()


    #count number of unique O-D couples for number of trips
    len_trips_number_am.append(len(activity_schedule_am_count))
    len_trips_number_pm.append(len(activity_schedule_pm_count))
    len_tt_couples_am.append(len(skim_am_table_count))

    skimindex = skim_am_table.set_index(["origin_aimsun","destination_aimsun"])
    #skimindex.to_csv(r"skimindex.csv")
    #join skim matrix and table with number of trips received from activity schedule using O-D couples
    am_concated=pd.concat([skim_am_table.set_index(["origin_aimsun","destination_aimsun"]),activity_schedule_am_count.set_index(["prev_stopZone","stopZone"])],axis=1)
    pm_concated=pd.concat([skim_pm_table.set_index(["origin_aimsun","destination_aimsun"]),activity_schedule_pm_count.set_index(["prev_stopZone","stopZone"])],axis=1)
    #am_concated.to_csv(r"concated_start"+str(i)+".csv")

    #leave only columns with number of trips and travel time
    am_concated=am_concated[["travel_time_aimsun","for_count"]]
    pm_concated=pm_concated[["travel_time_aimsun","for_count"]]

    #rename "for_count" column
    am_concated=am_concated.rename(columns={"for_count":"trips_number"})
    pm_concated=pm_concated.rename(columns={"for_count":"trips_number"})
    am_concated.to_csv(r"concated_start_filtered.csv")
    #add trips number and travel time columns to correspondent dataframes
    travel_times_am=pd.concat([travel_times_am,am_concated["travel_time_aimsun"]],axis=1)
    trips_number_am=pd.concat([trips_number_am,am_concated["trips_number"]],axis=1)

#    travel_times_am["pasted"]=travel_times_am.index#.iloc[:,0].astype(str)+"-"+travel_times_am.iloc[:,1].astype(str)
#    trips_number_am["pasted"] = trips_number_am.index#iloc[:, 0].astype(str) +"-"+ trips_number_am.iloc[:, 1].astype(str)
#    travel_times_am["pasted"].isin(trips_number_am["pasted"]).value_counts()
#    travel_times_am = travel_times_am.loc[travel_times_am['pasted'].isin(travel_times_am['pasted'])]
#    trips_number_am = trips_number_am.loc[trips_number_am['pasted'].isin(trips_number_am['pasted'])]
#    travel_times_am.drop(columns=["pasted"])
#    trips_number_am.drop(columns=["pasted"])
#    trips_number_am.drop(columns=["pasted"])

    travel_times_am.to_csv(r"ttam.csv")
    trips_number_am.to_csv(r"tpam.csv")
    travel_times_pm=pd.concat([travel_times_pm,pm_concated["travel_time_aimsun"]],axis=1)
    trips_number_pm=pd.concat([trips_number_pm,pm_concated["trips_number"]],axis=1)


# In[8]:


#renaming columns, column name = iteration number
def column_rename(df):
    df.columns=list(range(1,df.shape[1]+1))
    return df

travel_times_am=column_rename(travel_times_am)
trips_number_am=column_rename(trips_number_am)
travel_times_pm=column_rename(travel_times_pm)
trips_number_pm=column_rename(trips_number_pm)


# In[9]:


#fill empty trips number by 0
trips_number_am=trips_number_am.fillna(0)
trips_number_pm=trips_number_pm.fillna(0)


# In[10]:


#multiplication of travel time columns representing each iteration by trips number for each iteration
def travel_times_trips_number_multipl(trips_number,travel_times):
    trips_number.columns=list(range(1,trips_number.shape[1]+1))
    travel_times.columns=list(range(1,travel_times.shape[1]+1))
    return trips_number*travel_times


# In[11]:


#multiplication values for am and pm data
am_multipl=travel_times_trips_number_multipl(trips_number_am,travel_times_am)
#am_multipl.to_csv(r"am_multipl.csv")
pm_multipl=travel_times_trips_number_multipl(trips_number_pm,travel_times_pm)

# In[18]:
new_df = travel_times_am[travel_times_am[2]==0]
#print("here")
#print(new_df)
#replace 0 values by 0.00001

#### take this away in the dynamic iterations (although it does not look like it is triggered)
#travel_times_am=travel_times_am.replace([0,0.12])#0.00001])
#travel_times_pm=travel_times_pm.replace([0,0.12])#0.00001])


os.listdir



# In[42]:

am_multipl_plot=am_multipl
pm_multipl_plot=pm_multipl



# In[43]:

############################################

import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.font_manager
plt.rcParams['font.family'] = "Times New Roman"
csfont = {'fontname':'Times New Roman'}
#plt.rcParams['font.serif'] = ['Times New Roman']
plt.legend(prop={'family': 'Times New Roman'})
import matplotlib.patches as mpatches
list_n = []
list_bins = []
list_intersection = []
list_areaintx = []
list_area = []
list_DA = []
kws = dict(histtype= "stepfilled")
colors = ["#8B8378","#104E8B","#8B2323","#FF3030","#FF8C00","#68228B","#DC143C","#00CED1","#EE1289","#FF1493","#00688B","#104E8B","#B22222","#CD2626","#666666","#0A0A0A"]#["green","blue","gray","red"]#
Lmaximum = []
nbins = 1000
list_bars = []

for i in range(1, numfiles):
    if i == 1:
        Lmaximum.append(max(am_multipl_plot[am_multipl_plot.iloc[:, i-1] != 0].iloc[:, i-1]))
    Lmaximum.append(max(am_multipl_plot[am_multipl_plot.iloc[:, i] != 0].iloc[:, i]))

for i in range(0,numfiles):
    maximum = max(Lmaximum)
    n, bins, _ = plt.hist(am_multipl_plot[am_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum))
    plt.clf()
    plt.hist(am_multipl_plot[am_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i))
    plt.legend(fontsize=34, prop={"size": 22, "family": "Times New Roman"})
    plt.xlim(0, 400)
    #plt.ylim(0,300)
    plt.ylabel('Observation in each bin')
    plt.xlabel('Value intervals')
    plt.title('A'+str(i)+'.png')
    plt.savefig(path_results+"/aTaxi_It "+str(i+1)+'.png')
    list_n.append(n)
    list_bins.append(bins)
    area = list_n[i].sum()
    list_area.append(area)
plt.clf()

def createList(r1, r2):
    return [item for item in range(r1, r2+1)]
r1, r2 = 0, nbins-1
keys = createList(r1, r2)
value = []
dict_v = {key: list(value) for key in keys}
list_v = []
for i in range(0,numfiles): ### deviation only for the first 4, as we are plotting them for now
    for c in range(0,nbins): ### one list of 4 for each bin
        dict_v[c].append(list_n[i][c])#(list_area[i][c])

dfdict_v = pd.DataFrame.from_dict(dict_v)
dfdict_v.insert(loc = 0, column = "Name", value=np.arange(len(dfdict_v)))

plt.figure(figsize=(20, 16))
for i in range(0,numfiles):
    maximum = max(Lmaximum)
    step = maximum/nbins
    n, bins, _ = plt.hist(am_multipl_plot[am_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum), fill = False, edgecolor="white")
    for c in range(0, 200):
        plt.boxplot(dict_v[c], positions=[c * step + step / 2], patch_artist=True,
                    boxprops=dict(facecolor="white", color="white", linewidth = 0.5), capprops=dict(color="#E9967A", linewidth = 0.5),
                    whiskerprops=dict(color="white", linewidth = 0.5))
        plt.xticks([])
        plt.legend([])
    if (i % 2) == 0: # if even
        plt.hist(am_multipl_plot[am_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i), edgecolor=colors[i], **kws, fill = False,linewidth=3,zorder=10, linestyle = 'dashed')
    if i % 2: # if odd
        plt.hist(am_multipl_plot[am_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i), edgecolor=colors[i], **kws, fill = False,linewidth=3,zorder=10, linestyle = 'dotted')
#plt.legend(["1","2","3","4","5"],fontsize=28, prop={"size": 22, "family": "Times New Roman"})
handle1=mpatches.Patch(color=colors[0], label="Iteration 1", alpha = 0.4)
handle2=mpatches.Patch(color=colors[1], label="Iteration 2", alpha = 0.4)
handle3=mpatches.Patch(color=colors[2], label="Iteration 3", alpha = 0.4)
handle4=mpatches.Patch(color=colors[3], label="Iteration 4", alpha = 0.4)
handle5=mpatches.Patch(color=colors[4], label="Iteration 5", alpha = 0.4)
handle6=mpatches.Patch(color=colors[5], label="Iteration 6", alpha = 0.4)
#handle6=mpatches.Patch(color=colors[5], label="6", alpha = 0.4)

plt.legend(handles=[handle1,handle2, handle3,handle4,handle5,handle6], fontsize = 50, prop={"size": 50, "family": "Times New Roman"})
plt.xlim(0, 400)
#plt.ylim(0, 500)
#plt.xticks(ticks = [10,50,100,150,200],labels = ["10","50","100","150","200"], fontsize=50)
plt.yticks(fontsize=50)
plt.ylabel('Observation in each bin',fontsize=50, fontname='Times New Roman')
plt.xlabel('Intervals',fontsize=50, fontname='Times New Roman')
plt.title('Iterations (am)',fontsize=60, fontname='Times New Roman')
plt.savefig(path_results+"/aTaxi_Iterations.png")#(r'/home/angelo/Scripting (Static)/New/Script-plots and MoE/histogram/am/Static Iterations.png')
plt.clf()



value = []
dict_v = {key: list(value) for key in keys}
list_v = []
for i in range(0,numfiles): ### deviation only for the first 4, as we are plotting them for now
    for c in range(0,nbins): ### one list of 4 for each bin
        dict_v[c].append(list_n[i][c])
plt.figure(figsize=(25, 20))
for i in range(1,len(list_area)):
    intersection = np.minimum(list_n[i-1], list_n[i])
    areaintx = intersection.sum()
    list_intersection.append(intersection)
    list_areaintx.append(areaintx)
    plt.clf()
    maximum = max(Lmaximum)
    #maximum = max(max(pm_multipl_plot[pm_multipl_plot.iloc[:, i-1] != 0].iloc[:, i-1]),max(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i]))
    plt.hist(am_multipl_plot[am_multipl_plot.iloc[:, i-1] != 0].iloc[:, i-1], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i-1))
    plt.hist(am_multipl_plot[am_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i))
    #plt.bar(list_bins[i - 1][:-1], list_intersection[i - 1], width=list_bins[i][i] - list_bins[i - 1][i-1],label=str(i-1), alpha = 0.5)
    #plt.bar(list_bins[i][:-1], list_intersection[i], width=list_bins[i][i] - list_bins[i - 1][i - 1], label=str(i), alpha = 0.5)
    plt.legend([str(i), str(i+1)],fontsize=60, prop={"size": 60, "family": "Times New Roman"})
    #plt.ylim(0,200)
    plt.xlim(0, 500)
    plt.ylabel('Observation in each bin',fontsize=60, fontname='Times New Roman')
    plt.xlabel('Intervals',fontsize=60, fontname='Times New Roman')
    plt.xticks(fontsize=60)
    plt.yticks(fontsize=60)
    plt.title('ΔA between iteration '+str(i)+' and '+str(i+1),fontsize=60, fontname='Times New Roman')
    #plt.savefig(path_results+"/It_"+str(i+1)+'.png')#(r'/home/angelo/Scripting (Static)/New/Script-plots and MoE/histogram/am/'+str(i)+'am.png')
    x = 2 * ((list_area[i-1] + list_area[i]) - list_areaintx[i-1] * 2) / (list_area[i-1] + list_area[i])
    print(i)
    print(x)
    list_DA.append(x)

list_DA_am = list_DA
###################################################################################
#PM
###################################################################################

list_n = []
list_bins = []
list_intersection = []
list_areaintx = []
list_area = []
list_DA = []
kws = dict(histtype= "stepfilled")
colors = ["#8B8378","#104E8B","#8B2323","#FF3030","#FF8C00","#68228B","#DC143C","#00CED1","#EE1289","#FF1493","#00688B","#104E8B","#B22222","#CD2626","#666666","#0A0A0A"]#["green","blue","gray","red"]#
LmaximumPM = []
nbins = 1000
list_bars = []
for i in range(1, numfiles):
    if i == 1:
        LmaximumPM.append(max(pm_multipl_plot[pm_multipl_plot.iloc[:, i-1] != 0].iloc[:, i-1]))
    LmaximumPM.append(max(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i]))
if max(LmaximumPM) > max(Lmaximum):
    print("PM has to be run first")
    Lmaximum = LmaximumPM

for i in range(0,numfiles):
    maximum = max(Lmaximum)
    n, bins, _ = plt.hist(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum))
    plt.clf()
    plt.hist(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i))
    plt.legend(fontsize=34, prop={"size": 33, "family": "Times New Roman"})
    plt.xlim(0, 400)
    plt.ylabel('Observation in each bin')
    plt.xlabel('Value intervals')
    plt.title('A'+str(i)+'.png')
    #plt.savefig(path_results+'pm/single '+str(i)+'.png')#(r'/home/angelo/Scripting (Static)/New/Script-plots and MoE/histogram/pm/single '+str(i)+'.png')
    list_n.append(n)
    list_bins.append(bins)
    area = list_n[i].sum()
    list_area.append(area)

def createList(r1, r2):
    return [item for item in range(r1, r2+1)]
r1, r2 = 0, nbins-1
keys = createList(r1, r2)
value = []
dict_v = {key: list(value) for key in keys}
list_v = []
for i in range(0,numfiles): ### deviation only for the first 4, as we are plotting them for now
    for c in range(0,nbins): ### one list of 4 for each bin
        dict_v[c].append(list_n[i][c])#(list_area[i][c])

dfdict_v = pd.DataFrame.from_dict(dict_v)
dfdict_v.insert(loc = 0, column = "Name", value=np.arange(len(dfdict_v)))

plt.figure(figsize=(20, 16))
for i in range(0,numfiles):
    maximum = max(Lmaximum)
    step = maximum/nbins
    n, bins, _ = plt.hist(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum), fill = False, edgecolor="white")
    for c in range(0, 200):
        plt.boxplot(dict_v[c], positions=[c * step + step / 2], patch_artist=True,
                    boxprops=dict(facecolor="white", color="white", linewidth = 0.5), capprops=dict(color="#E9967A", linewidth = 0.5),
                    whiskerprops=dict(color="white", linewidth = 0.5))
        plt.xticks([])
        plt.legend([])
    if (i % 2) == 0: # if even
        plt.hist(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i), edgecolor=colors[i], **kws, fill = False,linewidth=3,zorder=10, linestyle = 'dashed')
    if i % 2: # if odd
        plt.hist(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i), edgecolor=colors[i], **kws, fill = False,linewidth=3,zorder=10, linestyle = 'dotted')
#plt.legend(["1","2","3","4","5"],fontsize=28, prop={"size": 22, "family": "Times New Roman"})
handle1=mpatches.Patch(color=colors[0], label="Iteration 1", alpha = 0.4)
handle2=mpatches.Patch(color=colors[1], label="Iteration 2", alpha = 0.4)
handle3=mpatches.Patch(color=colors[2], label="Iteration 3", alpha = 0.4)
handle4=mpatches.Patch(color=colors[3], label="Iteration 4", alpha = 0.4)
handle5=mpatches.Patch(color=colors[4], label="Iteration 5", alpha = 0.4)
handle6=mpatches.Patch(color=colors[5], label="Iteration 6", alpha = 0.4)
#handle6=mpatches.Patch(color=colors[5], label="Iteration 6", alpha = 0.4)
plt.legend(handles=[handle1,handle2, handle3,handle4,handle5,handle6], fontsize = 50, prop={"size": 50, "family": "Times New Roman"})
plt.xlim(0, 500)
#plt.ylim(0, 600)
#plt.xticks(ticks = [10,50,100,150,200],labels = ["10","50","100","150","200"], fontsize=50)
plt.yticks(fontsize=50)
plt.ylabel('Observation in each bin',fontsize=50, fontname='Times New Roman')
plt.xlabel('Intervals',fontsize=50, fontname='Times New Roman')
plt.title('Static iterations (pm)',fontsize=60, fontname='Times New Roman')
plt.savefig(path_results+"/aTaxi_Iterations_pm.png")#(r'/home/angelo/Scripting (Static)/New/Script-plots and MoE/histogram/pm/Static Iterations.png')
plt.clf()

value = []
dict_v = {key: list(value) for key in keys}
list_v = []
for i in range(numfiles): ### deviation only for the first 4, as we are plotting them for now
    for c in range(0,nbins): ### one list of 4 for each bin
        dict_v[c].append(list_n[i][c])

for i in range(1, (len(list_area))):
    intersection = np.minimum(list_n[i-1], list_n[i])
    areaintx = intersection.sum()
    list_intersection.append(intersection)
    list_areaintx.append(areaintx)
    plt.clf()
    maximum = max(Lmaximum)
    #maximum = max(max(pm_multipl_plot[pm_multipl_plot.iloc[:, i-1] != 0].iloc[:, i-1]),max(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i]))
    plt.hist(pm_multipl_plot[pm_multipl_plot.iloc[:, i-1] != 0].iloc[:, i-1], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i-1), edgecolor="k", **kws, fill = False)#, color="white", edgecolor='green')
    plt.hist(pm_multipl_plot[pm_multipl_plot.iloc[:, i] != 0].iloc[:, i], alpha=0.4, bins=nbins, range=(0, maximum),label=str(i), edgecolor="red", **kws, fill = False)#, color="white", edgecolor='red')
    #plt.bar(list_bins[i - 1][:-1], list_intersection[i - 1], width=list_bins[i][i] - list_bins[i - 1][i-1],label=str(i-1), alpha = 0.5)
    #plt.bar(list_bins[i][:-1], list_intersection[i], width=list_bins[i][i] - list_bins[i - 1][i - 1], label=str(i), alpha = 0.5)
    plt.legend(fontsize=60, prop={"size": 60, "family": "Times New Roman"})
    #plt.ylim(0,200)
    plt.xlim(0, 500)
    plt.ylabel('Observation in each bin')
    plt.xlabel('Value intervals')
    plt.title('DA between iteration '+str(i)+' and '+str(i+1)+'.png')
    #plt.savefig(path_results+'pm/'+str(i)+'.png')#(r'/home/angelo/Scripting (Static)/New/Script-plots and MoE/histogram/pm/'+str(i)+'.png')
    x = 2 * ((list_area[i-1] + list_area[i]) - list_areaintx[i-1] * 2) / (list_area[i-1] + list_area[i])
    list_DA.append(x)

list_DA_pm = list_DA
with open(path_results+"/aTaxi_DA.txt","a") as file1:
    file1.write(str(list_DA_am)+' '+str(list_DA_pm))


print("histogram comparison")
print(list_DA_am)
print(list_DA_pm)

plt.clf()
plt.figure(figsize=(20, 16))
plt.bar(range(1,numfiles),list_DA_am)
plt.xticks(ticks = [1,2,3,4,5],labels = ["1","2","3","4","5"],fontsize=60)
plt.yticks(fontsize=60)
plt.ylim(0,2)
plt.ylabel('MoE',fontsize=60, fontname='Times New Roman')
plt.xlabel('ΔA',fontsize=60, fontname='Times New Roman')
plt.title('ΔA (am)',fontsize=60, fontname='Times New Roman')
plt.savefig(path_results+'/aTaxi_DA_am_bigger_font.png')#(r'/home/angelo/Scripting (Static)/New/Script-plots and MoE/histogram/DA_am.png')

plt.clf()
plt.figure(figsize=(20, 16))
plt.bar(range(1,numfiles),list_DA_pm)
plt.xticks(ticks = [1,2,3,4,5],labels = ["1","2","3","4","5"],fontsize=60)
plt.yticks(fontsize=60)
plt.ylim(0,2)
plt.ylabel('MoE',fontsize=60, fontname='Times New Roman')
plt.xlabel('ΔA',fontsize=60, fontname='Times New Roman')
plt.title('ΔA (pm)',fontsize=60, fontname='Times New Roman')
plt.savefig(path_results+'/aTaxi_DA_pm_bigger_font.png')#(r'/home/angelo/Scripting (Static)/New/Script-plots and MoE/histogram/DA_pm.png')

