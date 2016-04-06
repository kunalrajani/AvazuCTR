
# coding: utf-8

# In[35]:

import numpy as np
import csv
import random

# Function to search for an item 't' in a sorted sequence 'seq'
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


# File names
TrainFile = "train.csv" #sys.argv[1]
TrainSample = "TrainSample.csv"
OOSValSample = "OOSVal.csv"
OOTValSample = "OOTVal.csv"
TestSample = "TestSample.csv"

blank = ["", "-1"]


# In[21]:

# Now we need to obtain the beginning line numbers for each day
f = open(TrainFile, 'r')
header = f.next().split(',')        #Get the first header row
DayHour = f.next().split(',')[2]    # Get the DayHour values and then extract the Date
Day = DayHour[:6]
DayLineNos = [(Day,2)]
ctr = 1

# The following code loops through the complete TrainFile and finds the row number for each day
for i, row in enumerate(f):
    DayHour = row.split(',')[2]
    if(Day != DayHour[:6]):     #When the current line's Day value is not equal to the previous one. This means that a new day has started in the data
        DayLineNos.append((DayHour[:6],i))
        Day = DayHour[:6]
        ctr = ctr+1
        print('The day %s begins at %d row' %(Day, i))

f.close()


# In[32]:

N = 40428968    #Total number of rows in TrainFile

# The boundaries below have been obtained based on the output of the code above and contain the min and max row numbers that the sample will be chosen from
TestLines = (36210028,N)
OOTValLines = (32377420, 36210027)
TrainLines = (1,32377419)

TrainSamp = random.sample(range(TrainLines[0], TrainLines[1]), np.int((TrainLines[1]-TrainLines[0])*0.1))

# Defining all the row numbers for the samples below
OOSValSamp = TrainSamp[(np.int(len(TrainSamp)*.7)+1):]
TrainSamp = TrainSamp[1:np.int(len(TrainSamp)*.7)]
OOTValSamp = random.sample(range(OOTValLines[0],OOTValLines[1]),np.int((OOTValLines[1]-OOTValLines[0])*0.5))
TestSamp = random.sample(range(TestLines[0],TestLines[1]),np.int((TestLines[1]-TestLines[0])*0.5))


# In[47]:

# Simply sorting all the row numbers
TrainSamp.sort()
OOSValSamp.sort()
OOTValSamp.sort()
TestSamp.sort()


# In[56]:

f = open(TrainFile, 'r')
header = f.next()

# Open the file for write access
f1 = open(TrainSample, 'w')
f2 = open(OOSValSample, 'w')
f3 = open(OOTValSample, 'w')
f4 = open(TestSample, 'w')

f1.write(header)
f2.write(header)
f3.write(header)
f4.write(header)

ctr1 = ctr2 = ctr3 = ctr4 = 0

#Running through the file and outputing each row to the respective sample file
for i, row in enumerate(f):
    if (i%1000000 == 0):
        print('%d rows covered' %i)
    if(i == TrainSamp[ctr1]):
        f1.write(row)
        ctr1 = (ctr1 + 1) if ctr1<len(TrainSamp)-1 else ctr1
    else:
        if(i == OOSValSamp[ctr2]):
            f2.write(row)
            ctr2 = (ctr2 + 1) if ctr2<len(OOSValSamp)-1 else ctr2
        else:
            if(i == OOTValSamp[ctr3]):
                f3.write(row)
                ctr3 = (ctr3 + 1) if ctr3<len(OOTValSamp)-1 else ctr3
            else:
                if(i == TestSamp[ctr4]):
                    f4.write(row)
                    ctr4 = (ctr4 + 1) if ctr4<len(TestSamp)-1 else ctr4

f.close()
f1.close()
f2.close()
f3.close()
f4.close()

    


                
                
# In[54]:





