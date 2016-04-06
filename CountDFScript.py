
# coding: utf-8

# In[1]:

import fileinput
import re
import numpy as np
from collections import defaultdict
import pandas as pd
import csv
import math
from matplotlib import pyplot as plt
from collections import Counter
import matplotlib
import random


def binary_search(seq, t):
    min = 0
    max = len(seq) - 1
    while True:
        if max < min:
            return -1
        m = (min + max) // 2
        if seq[m] < t:
            min = m + 1
        elif seq[m] > t:
            max = m - 1
        else:
            return m



TrainFile = "../train.csv" #sys.argv[1]
TempTrainFile = "../TempTrain.csv" ## Created using 'head -nX train.csv' as a subset of the training data to test algos below
blank = ["", "-1"]


# In[118]:

N = 40428968
NumSamples = 0.05*N # The sample size over which to create the set of unique values
sampling = 'yes'
Sample = random.sample(range(1,N),int(NumSamples)) if sampling == 'yes' else range(1,N)
Sample.sort()
NumRecords = [] # This is a list of dictionaries to hold the frequency of terms for each column
NumClicks = [] # This is a list of dict to hold the number of clicks for each term in each column

f = open(TrainFile, 'r')
header = f.next().split(',')
# Initializing the 2 lists of dictionaries
for i in range(len(header)):
    NumRecords.append({})
    NumClicks.append({})

features = ['weekday', 'hour'] + header[3:] # Defining the feature vector
for x,row in enumerate(f):                  # Looking through the file
    if(x%25000==0):                         # Does nothing. Only to keep track
        print('%d of %d rows covered' %(x,N))
    if (binary_search(Sample,x) != -1 or sampling == 'no'):     #If the row number exists in the file
        keys = row.split(',')[1:]
        click = keys[0]
        # As seen above that 'weekday','hour' are the first 2 vectors and hence they are being defined here
        keys[0] = 'weekend' if (keys[1][4:6] in ['25','26']) else 'weekday' 
        keys[1] = int(keys[1][6:8])
        for i in range(len(features)):              # Looping through the features
            if(i != features.index("device_ip")):   # Not creating this summary view for device_ip
                if(not NumRecords[i]):              # If NumRecords for that feature is empty then initialize
                    NumRecords[i][keys[i]] = 0
                    NumClicks[i][keys[i]] = 0
                if(keys[i] not in NumRecords[i].keys()): #If the corresponding value has not been seen below then create that key
                    NumRecords[i][keys[i]] = 0
                    NumClicks[i][keys[i]] = 0
                NumRecords[i][keys[i]] += 1         # Increase the NumRecords
                NumClicks[i][keys[i]] += int(click) # Add 1 if a click was made in the row
f.close()


# In[153]:

# Print the unique number of values for each categorical variable as seen in train
print([('%s, %d'%(features[i],len(NumRecords[i]))) for i in range(len(features))])


# In[229]:

# Defines the data frame from the dict of the first feature. I.e. the variable values and their Freq come along a column
ItemCountDF = pd.DataFrame.from_dict(NumRecords[0], orient = 'index')
ItemCountDF['ItemValue'] = ItemCountDF.index
ItemCountDF.columns = ['Count','ItemValue']
ItemCountDF['Column'] = features[0]
# Create a DF where the first column is the #clicks for each variable value
ClickCountDF = pd.DataFrame.from_dict(NumClicks[0], orient = 'index')
ItemCountDF['CTR'] = ClickCountDF.ix[:,0]/ItemCountDF.Count*1.0
ItemCountDF = ItemCountDF.sort('Count', ascending=0)
# In[231]:

# Remove the rows that have a very small frequency in the training data.
# The cutoff below indicates to include all those values of the categorical variables that constitute 99% of the training data
CutOffLimit = N*.99 if sampling == 'no' else NumSamples*.99
for i in range(1,len(features)):
    if(i != features.index("device_ip")):
        #-- Create a DF of the terms in one column and num records and num clicks in the other
        TempDF = pd.DataFrame.from_dict(NumRecords[i], orient = 'index')
        TempDF['ItemValue'] = TempDF.index      # Contains the value of the variable
        TempDF.columns = ['Count','ItemValue']  
        TempDF['Column'] = features[i]          # Contains the variable name that is constant for all the values that belog to that categorical variable
        ClickCountDF = pd.DataFrame.from_dict(NumClicks[i], orient = 'index')
        TempDF['CTR'] = ClickCountDF.ix[:,0]/TempDF.Count*1.0
        TempDF = TempDF.sort('Count', ascending=0) if features[i] != 'hour' else TempDF # If we are summarizing the "hour" feature then we should not sort by Freq as hours have an order
        CutoffRow = np.where(np.cumsum(TempDF.Count)>CutOffLimit)   #Define the cutoff row
        # plot the CTR of each variable over all it's values to visually analyze the distribution
        plt.figure()
        plt.suptitle(features[i])
        plt.plot(TempDF.iloc[0:(CutoffRow[0][0]+1),3])
        
        ItemCountDF = ItemCountDF.append(TempDF, ignore_index=True)




# In[1]:
# Now lets try to look at how many categorical values will we encounter if we discard the less frequent ones

sum(ClickCountDF.ix[:,0])*1.0/sum(TempDF.Count) # This is the overall CTR
NumSigCategories = 0

for i in range(len(features)):
    if(i != features.index("device_ip")):
        TempDF = ItemCountDF.ix[ItemCountDF['Column']==features[i],:]
        CutoffRow = np.where(np.cumsum(TempDF.Count)>CutOffLimit)
        NumSigCategories += CutoffRow[0][0]+1

NumSigCategories


