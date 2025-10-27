#!/usr/bin/env python
# coding: utf-8

# In[49]:


import pandas as pd
import numpy as np
import csv

cost_table = pd.read_csv("/home/angelo/PycharmProjects/AV/Data/Starting tables/amcost_start") # From IT4 - most recent baseline iteration with improved behav parameters -> car ivt and av ivt/wt are updated so this file does not need change with iterations
aimsun_am=pd.read_csv("/home/angelo/PycharmProjects/AV/Data/AVGam.csv",index_col=0)
aimsun_pm=pd.read_csv("/home/angelo/PycharmProjects/AV/Data/AVGpm.csv",index_col=0)




#aimsun_am = aimsun_am.drop(aimsun_am.columns[[0]],axis = 1)
#aimsun_pm = aimsun_pm.drop(aimsun_pm.columns[[0]],axis = 1)


#replacing the values with no traffic (0) with travel times from the static assignment

df1 = pd.read_csv("/home/angelo/PycharmProjects/AV/staticam.csv", index_col = False) # static assignment of each iteration, filling missing od pairs
df1 = df1.drop(df1.columns[[0]],axis = 1)
df1.columns = df1.iloc[0]
df1=df1.drop(df1.index[[0]], axis = 0)
df1.index = df1['Name'] #you need to have missing 98 -> and indeed you have it so this is verified and working
df1=df1.drop(df1.columns[[0]], axis = 1)
aimsun_am = aimsun_am.where(aimsun_am != 0, df1.values*60)


#aimsun_am.mask[aimsun_am == 0].combine_first(df1)
#print(aimsun_am.head)
df1 = pd.read_csv("/home/angelo/PycharmProjects/AV/staticpm.csv", index_col = False)
df1 = df1.drop(df1.columns[[0]],axis = 1)
df1.columns = df1.iloc[0]
df1=df1.drop(df1.index[[0]], axis = 0)
df1.index = df1['Name'] #you need to have missing 98
df1=df1.drop(df1.columns[[0]], axis = 1)
aimsun_pm = aimsun_pm.where(aimsun_pm != 0, df1.values*60)
# In[52]:


#convert AimSun indexes and column names to numerical format
def aimsun_col_index_fix(aimsun):
    aimsun.columns=list(map(lambda x: int(x), aimsun.columns))
    aimsun.index=list(map(lambda x: int(x), aimsun.index))
    return aimsun

aimsun_am=aimsun_col_index_fix(aimsun_am)
aimsun_pm=aimsun_col_index_fix(aimsun_pm)


# In[54]:


#convert skim matrix to table
aimsun_am_table=aimsun_am.stack().reset_index()
aimsun_pm_table=aimsun_pm.stack().reset_index()


# In[55]:


#rename some columns in AimSun tables
def aimsun_columns_renaming(aimsun_table):
    aimsun_table.columns=["origin_aimsun","destination_aimsun","travel_time_aimsun"]
    return aimsun_table

aimsun_am_table=aimsun_columns_renaming(aimsun_am_table)
aimsun_pm_table=aimsun_columns_renaming(aimsun_pm_table)


# In[56]:


#merging SimMobility and AimSun files
def simmobility_aimsun_merging(cost_table,skim_matrix):
    return pd.merge(cost_table,skim_matrix,how="left",left_on=["origin_zone","destination_zone"],right_on=["origin_aimsun","destination_aimsun"])


merged_am=simmobility_aimsun_merging(cost_table,aimsun_am_table)


# In[57]:


#replace abscent values in merged table by 999 multiplied by 60 (so when we will convert to hours we will get 999)
#merged_am["travel_time_aimsun"]=merged_am["travel_time_aimsun"].fillna(str(999*60))
#rather than imposing 999 (which cause everyone to use pt or walk) select previous row tt, i.e. a nearby centroid
merged_am["travel_time_aimsun"]=merged_am["travel_time_aimsun"].ffill()

# In[58]:


#conversion of string values to float and reformatting to hours
merged_am["travel_time_aimsun"]=merged_am["travel_time_aimsun"].apply(lambda x: float(x)/3600)#.replace(",","."))/60)

# In[59]:


#replacing values in "car_ivt" column by values that we got from AimSun
merged_am["car_ivt"]=merged_am["travel_time_aimsun"]


# In[60]:


#removing columns that are not needed anymore
merged_am=merged_am.drop(columns=["origin_aimsun","destination_aimsun","travel_time_aimsun"])
#remove first row
#merged_am=merged_am.iloc[1: , :]
#print(merged_am.head)
# In[ ]:
merged_am.to_csv('/home/angelo/PycharmProjects/AV/Data/amcost_output.csv',index=False, header = None)


#cost_table=pd.read_csv("/home/angelo/Scripting/pmcost.csv")

cost_table=pd.read_csv("/home/angelo/PycharmProjects/AV/Data/Starting tables/pmcost_start")
merged_pm=simmobility_aimsun_merging(cost_table,aimsun_pm_table)


# In[57]:


#replace abscent values in merged table by 999 multiplied by 60 (so when we will convert to hours we will get 999)
#merged_pm["travel_time_aimsun"]=merged_pm["travel_time_aimsun"].fillna(str(999*60))
#rather than imposing 999 (which cause everyone to use pt or walk) select previous row tt, i.e. a nearby centroid
merged_pm["travel_time_aimsun"]=merged_pm["travel_time_aimsun"].ffill()

# In[58]:


#conversion of string values to float and reformatting to hours
merged_pm["travel_time_aimsun"]=merged_pm["travel_time_aimsun"].apply(lambda x: float(x)/3600)#float(x.replace(",","."))/60)


# In[59]:


#replacing values in "car_ivt" column by values that we got from AimSun
merged_pm["car_ivt"]=merged_pm["travel_time_aimsun"]


# In[60]:


#removing columns that are not needed anymore
merged_pm=merged_pm.drop(columns=["origin_aimsun","destination_aimsun","travel_time_aimsun"])


# In[ ]:
merged_pm.to_csv('/home/angelo/PycharmProjects/AV/Data/pmcost_output.csv',index=False, header = None)

