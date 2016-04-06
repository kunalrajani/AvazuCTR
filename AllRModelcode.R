
library("plyr")
library(randomForest)
library(ROCR)

TrainData = read.csv(file = "TrainSample.csv", header = T)
TestData = read.csv(file = "TestSample.csv", header = T)
OOSVal = read.csv(file = "OOSVal.csv", header = T)
OOTVal = read.csv(file = "OOTVal.csv", header = T)

#-----------------------------------------------------------------
# compute the Logloss function taking actual and predicted as vector inputs
LogLoss <- function(act, pred)
{
  eps = 1e-15
  pred[pred<eps] = eps
  pred[pred>(1-eps)] = (1-eps)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll*-1/(length(act))
  return(ll)
}

Concordance = function(actual,predicted) {
  outcome_and_fitted_col = cbind(actual,predicted)
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = 0
  disc = 0
  ties = 0
  for (i in 1:length(ones[,1]))
  {
    for(j in 1:length(zeros[,1]))
    {
      if (ones[i,2] > zeros [i,2])
        conc = conc + 1
      else if (ones[i,2] == zeros [i,2])
        ties = ties + 1
      else if (ones[i,2] < zeros [i,2])
        disc = disc + 1
    }
  }
  # Here we save the various rates
  conc_rate = conc/(dim(ones)[1]*dim(zeros)[1])
  disc_rate = disc/(dim(ones)[1]*dim(zeros)[1])
  tie_rate = ties/(dim(ones)[1]*dim(zeros)[1])
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
}

# ColSummary creates the summary table FreqDF that is then further used for analysis
ColSummary <- function(data)
{
  header = colnames(data)
  FreqDF = data.frame()    # Initialize the data frame
  for (i in 1:ncol(data))
  {
    # device_ip 
    if(!header[i] %in% c('id','hour','device_ip'))
    {
      CTR = aggregate(as.formula(paste('click~',header[i],sep="")), data, function(x) mean(x))
      FreqTable = data.frame(colname = header[i], table(data[,i]))
      FreqTable = merge(FreqTable, CTR, by.x='Var1', by.y=header[i])
      FreqDF = rbind(FreqDF, FreqTable)
    }
    if(header[i] == 'hour')
    {
      Hour = data.frame(hour=substr(as.character(data$hour),7,8))
      CTR = aggregate(data$click, by=list(hour=Hour$hour), function(x) mean(x))
      FreqTable = data.frame(colname = 'Hour', table(Hour$hour))
      FreqTable = merge(FreqTable, CTR, by.x='Var1', by.y="hour")
      colnames(FreqTable) = c('Var1','colname','Freq','click')
      FreqDF = rbind(FreqDF, FreqTable)
      Day = data.frame(day=substr(as.character(data$hour),5,6), stringsAsFactors = F)
      Weekday = Day
      Weekday$day[Day$day %in% c(21,28)] = 'Tue'
      Weekday$day[Day$day %in% c(22,29)] = 'Wed'
      Weekday$day[Day$day %in% c(23,30)] = 'Thu'
      Weekday$day[Day$day == 24] = 'Fri'
      Weekday$day[Day$day == 25] = 'Sat'
      Weekday$day[Day$day == 26] = 'Sun'
      Weekday$day[Day$day == 27] = 'Mon'
      CTR = aggregate(data$click, by=list(weekday=Weekday$day), function(x) mean(x))
      FreqTable = data.frame(colname = 'DayOfWeek', table(Weekday$day))
      FreqTable = merge(FreqTable, CTR, by.x='Var1', by.y="weekday")
      colnames(FreqTable) = c('Var1','colname','Freq','click')
      FreqDF = rbind(FreqDF, FreqTable)    
    }
  }
  data = data.frame(data, Hour = Hour$hour, DayOfWeek = Weekday$day)
  colnames(FreqDF) = c('VarValue','Colname','Freq','CTR')
  rm(Hour)
  rm(Weekday)
  rm(Day)
  
  FreqDF = ddply(FreqDF,.(Colname),function(x) {return(x[order(x$Freq, decreasing=T),])} )
  FreqDF[, sapply(FreqDF, is.factor)] = lapply(FreqDF[, sapply(FreqDF, is.factor)], as.character)

  return(list(FreqDF=FreqDF, data = data));
}

#-- Creating the "Hour and DayofWeek" variables
HourAndDay <- function(data)
{
  Hour = data.frame(hour=substr(as.character(data$hour),7,8))
  Day = data.frame(day=substr(as.character(data$hour),5,6), stringsAsFactors = F)
  Weekday = Day
  Weekday$day[Day$day %in% c(21,28)] = 'Tue'
  Weekday$day[Day$day %in% c(22,29)] = 'Wed'
  Weekday$day[Day$day %in% c(23,30)] = 'Thu'
  Weekday$day[Day$day == 24] = 'Fri'
  Weekday$day[Day$day == 25] = 'Sat'
  Weekday$day[Day$day == 26] = 'Sun'
  Weekday$day[Day$day == 27] = 'Mon'
  data = data.frame(data, Hour = Hour$hour, DayOfWeek = Weekday$day)
  return(data)
}

####################################### Main code begins here ####################################

##--- Create the Summary data set grouped by Variables and Categories.
temp = ColSummary(TrainData)
TrainData = temp$data
#FreqDFTrain = temp$FreqDF    # Commented since it has been run once and stored as seen in the next statement
#write.csv(FreqDFTrain, file="AllColumnFreqTrain.csv", row.names = FALSE)
FreqDFTrain = read.csv(file = "AllColumnFreqTrain.csv", header=T, stringsAsFactors = F)
FreqDFTrain$Clicks = FreqDFTrain$CTR*FreqDFTrain$Freq

temp = ColSummary(TestData)
TestData = temp$data
#FreqDFTest = temp$FreqDF   # Commented since it has been run once and stored as seen in the next statement
#rm(temp)
#write.csv(FreqDFTest, file="AllColumnFreqTest.csv", row.names = FALSE)
FreqDFTest = read.csv(file = "AllColumnFreqTest.csv", header=T, stringsAsFactors = F)

##-- Create the Hour and Day variables on a data. This would be used if the ColSummary() function has already
##-- been run on the data and we have the FreqDF since ColSummary() is the one that creates these Hour and DayofWeek variables
TrainData = HourAndDay(TrainData)
TestData = HourAndDay(TestData)
OOSVal = HourAndDay(OOSVal)
OOTVal = HourAndDay(OOTVal)


## Now I try to see how much is the overlap between the Train and Test
TrainTestOverlap = merge(FreqDFTrain, FreqDFTest, by = c('Colname','VarValue'), all=T, suffix = c('.Train','.Test'))

TrainTestCommon = merge(aggregate(Freq.Train~Colname, TrainTestOverlap, function(x) sum(!is.na(x))),
                        aggregate(Freq.Test~Colname, TrainTestOverlap, function(x) sum(!is.na(x))),
                        by = 'Colname')
TrainTestCommon = merge(TrainTestCommon,aggregate(Freq.Train~Colname, 
                  TrainTestOverlap[!is.na(TrainTestOverlap$Freq.Test),], function(x) sum(!is.na(x))),
                  by = 'Colname')
TrainTestCommon = merge(TrainTestCommon,aggregate(Freq.Train~Colname, 
                        TrainTestOverlap[!is.na(TrainTestOverlap$Freq.Test),], function(x) sum(x, na.rm=T)),
                        by = 'Colname')
colnames(TrainTestCommon) = c('Colname','NumTrainCat','NumTestCat','TestCatinTrain', 'TrainCommonCount')
TrainTestCommon$TestinTrain = TrainTestCommon$TestCatinTrain*1.0/TrainTestCommon$NumTestCat
TrainTestCommon$TrainCommonCount = TrainTestCommon$TrainCommonCount*1.0/2266417


## Looking at how predictive each person can be. Assuming device_id is unique to a person
FreqDFTrain$Clicks = FreqDFTrain$CTR*FreqDFTrain$Freq
CTRperDevice = ddply(FreqDFTrain[FreqDFTrain$Colname=='device_id',], .(VarValue), function(x) {c(sum(x$Clicks),sum(x$Freq))})
CTRperDevice$IDinTest = CTRperDevice$VarValue %in% FreqDFTest$VarValue[FreqDFTest$Colname == 'device_id']
colnames(CTRperDevice) = c('VarValue','Clicks','TotalRecords','IDinTest')


#################################################################################################
###################################### Naive Bayes Algorithm ####################################
#################################################################################################

## Now I will take a subset of the categorical values that are sparse in nature to avoid computing a probablity on them
RemoveSparse <- function(x) # This function removes the sparse categories by considering a cutoff based on the total sample size that have less than 100 samples
{
  x = x[order(x$Freq, decreasing=T),]
  x$VarValue[which(cumsum(x$Freq)>CutOff & x$Freq<100) ] = 'Other'
  return(x)
}
CutOff = ceiling(0.995*nrow(TrainData))
FreqDFTrain = ddply(FreqDFTrain, .(Colname), RemoveSparse)
FreqDFTrain[, sapply(FreqDFTrain, is.factor)] = lapply(FreqDFTrain[, sapply(FreqDFTrain, is.factor)], as.character)

FreqDFTrain = aggregate(cbind(Clicks,Freq)~Colname+VarValue, FreqDFTrain, sum)
FreqDFTrain = FreqDFTrain[FreqDFTrain$Colname != 'device_id',]
FreqDFTrain = FreqDFTrain[order(FreqDFTrain$Colname, FreqDFTrain$VarValue),]
TotalClicks = sum(TrainData$click)
TotalRows = nrow(TrainData)
#write.csv(FreqDFTrain, file = 'SummarybyCategory.csv', row.names = F)

## for the Baiyes algo to work it needs to search the P(x) for each category. So to make it faster we can create a 
## Dataframe with the first and last row numbers
CatRowNums = data.frame(Colname = unique(FreqDFTrain$Colname))
CatRowNums$RowStart = apply(CatRowNums, 1, function(x) min(which(FreqDFTrain$Colname==x['Colname'])))
CatRowNums$RowEnd = apply(CatRowNums, 1, function(x) max(which(FreqDFTrain$Colname==x['Colname'])))

BayesProbVec <- function(X)
{
  if( !all(unique(FreqDFTrain$Colname) %in% names(X)) )
    message("You do not have all the variables available in the data. The predictions may not be accurate. Use with caution.")
  # Initializing the probablity vectors for both responses. These will be updated for each record
  Prob0 = 0
  Prob1 = 0
  for(i in 1:nrow(X))
  {
    # Initializing the Probability for each record
    RecProb1 = 1
    RecProb0 = 1
    for(j in 1:ncol(X))
    {
      #Filtering for the columns on which the Naive bayes won't be applied
      if(!names(X)[j] %in% c('id','hour','click','device_ip','device_id','app_id','C14','C15','site_domain'))
      {
        VarValue = X[i,j]
        #Obtain the rows numbers for the corresponding variable in FreqDFTrain
        Rows = CatRowNums[CatRowNums$Colname == names(X)[j],]
        # Create the Freq table for the particular variable being considered
        ColCategories = FreqDFTrain[Rows$RowStart:Rows$RowEnd,]
        #checking to see if the category of the corresponding sample is found in Training. If not then replace it with "Other"
        if(!X[i,j] %in% ColCategories$VarValue )
          VarValue = 'Other'
        Sample = ColCategories[ColCategories$VarValue == VarValue,]
        # RecProb1 = Product( Prob(Category_i/Y=1))
        RecProb1 = RecProb1*Sample$Clicks*Sample$Freq*1.0/TotalClicks
        RecProb0 = RecProb0*(Sample$Freq - Sample$Clicks)*Sample$Freq*1.0/(TotalRows - TotalClicks)
#        cat(sprintf("Prob(X/Click) = %f",FreqDFTrain$Clicks[FreqDFTrain$Colname == names(X)[j] & FreqDFTrain$VarValue == VarValue]*1.0/TotalClicks))
      }
    }
    RecProb0 = RecProb0*(nrow(TrainData)-TotalClicks)/nrow(TrainData)
    RecProb1 = RecProb1*(TotalClicks)/nrow(TrainData)
    Prob0[i] = RecProb0/(RecProb0 + RecProb1)
    Prob1[i] = RecProb1/(RecProb0 + RecProb1)
    if(i %% 100==0)
      cat(sprintf("For the %d record the P(Click) is %f and P(No Click) is %f \n", i, Prob1[i], Prob0[i]))
  }
  return(data.frame(Prob0 = Prob0, Prob1 = Prob1))
}
OOSValSample = OOSVal[sample(1:nrow(OOSVal),size = 10000, replace = F),]
Prob = BayesProbVec(OOSValSample)
ProbModBayes = BayesProbVec(OOSValSample)

#### Evaluating Naive Bayes output
LogLoss(OOSValSample$click, Prob$Prob1)
LogLoss(OOSValSample$click, ProbModBayes$Prob1)
## Looking at the cross tab of the actual vs predicted
table(OOSValSample$click, Prob$Prob1>Prob0)
table(OOSValSample$click, Prob$Prob1>Prob0)/c(sum(OOSValSample$click==0),sum(OOSValSample$click))

######################################## End of Naive Baiyes #########################################
######################################################################################################


###################################################################################################
#################################### Random Forest #################################################
## Only extracting the variables that have less than 50 categories

temp = aggregate(Freq~Colname, FreqDFTrain, function(x) length(x))
LT20Catg = temp$Colname[temp$Freq<=20]
GT20Catg = temp$Colname[temp$Freq>20]

ModelDataV2 = TrainData[,names(TrainData) %in% c('click',LT20Catg)]


## Building a randomforest model
bestmtry <- tuneRF(ModelDataV2[,-1],ModelDataV2$click, ntreeTry=30, 
                   stepFactor=1.0,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

Click.RF = randomForest(ModelDataV2[,-1], factor(ModelDataV2$click), mtry=3, ntree=1000, nodesize = 1000, 
                        strata = factor(ModelDataV2$click), sampsize = c(300000,300000), importance=TRUE)

save(Click.RF, file = 'RF3mtry1000ntree300Ksamples')

pred = Click.RF$votes
pred = pred[,2]
eps = 1e-15
pred[pred<eps]=eps
pred[pred>(1-eps)] = 1-eps
sum(ModelDataV2$click*log(pred) + (1-ModelDataV2$click)*log(1-pred))*-1/length(pred)



#################################### End of Random Forest #########################################


###################################################################################################
#################################### Logistic Regression ##########################################


temp = aggregate(Freq~Colname, FreqDFTrain, function(x) length(x))
LT20Catg = temp$Colname[temp$Freq<=20]
GT20Catg = temp$Colname[temp$Freq>20]

ModelDataV2 = TrainData[,names(TrainData) %in% c('click',LT20Catg)]
ModelDataV2[,-1] = data.frame(lapply(ModelDataV2[,-1], as.factor))

fit = glm(data = ModelDataV2, formula = click~., family = binomial)
LogLoss(ModelDataV2$click, fit$fitted.values)

save(fit, file = 'LogisticLT20CatVars')

#### Looking for interaction terms using logistic regression
sampling = sample(1:dim(ModelDataV2)[1], 100000, replace = F)

fitIx = glm(data = TrainData[sampling,c('click','site_category','app_category')], formula = click~site_category:app_category, family = binomial)
LogLoss(TrainData$click[sampling], fitIx$fitted.values)

#################################### End of Logistic Regression ###################################


## Only extracting the variables that have less than 20 categories
temp = aggregate(Freq~Colname, FreqDFTrain, function(x) length(x))
LT20Catg = temp$Colname[temp$Freq<=20]
GT20Catg = temp$Colname[temp$Freq>20]

################## Looking at the histogram of CTR for variables with many categories #############
par(mfrow=c(2,2))
for (i in 1:length(GT20Catg))
{
  h = hist(FreqDFTrain$CTR[FreqDFTrain$Colname==GT20Catg[i]], xlab = 'CTR', main = GT20Catg[i], 
           breaks = seq(0,1,0.05), freq = FALSE)
  temp = FreqDFTrain[FreqDFTrain$Colname==GT20Catg[i],]
  temp = temp[order(temp$CTR),]
  # This is something that can be tricky in python
  temp$bin = as.numeric(cut(temp$CTR, breaks = seq(0,1,0.05), labels = seq(1,length(seq(0,1,0.05))-1,1)))
  temp = aggregate(Freq~bin, temp, sum)
  lines(temp$bin/20,temp$Freq*h$density[1]/max(temp$Freq))
}
par(mfrow=c(1,1))
## Finding the most number of records where CTR > X%
temp = FreqDFTrain[FreqDFTrain$CTR<=0.02,]
aggregate(Freq~Colname, temp, sum)


################ Finding the correlation (association) between variables ###################
CorMat = data.frame(Var1=as.character(), Var2=as.character(), Corr=as.numeric(), P.Val = as.numeric(), stringsAsFactors = F)
for (i in 1:(dim(TrainData)[2]-1))
{
  for (j in i:dim(TrainData)[2])
  {
    if(!any(names(TrainData)[c(i,j)] %in% c('id','hour','device_ip','device_id')))
    {
      print(paste("Columns being worked upon: ", i," ", j))
      chi2 = chisq.test(table(TrainData[,c(i,j)]), correct = F)   
      corr = sqrt(chi2$statistic/sum(table(TrainData[,c(i,j)])))
      p.val = chi2$p.value
      CorMat[nrow(CorMat)+1,] = c(names(TrainData)[c(i,j)],corr,p.val)
    }
  }
}

CorMat2 = matrix(0, nrow=21, ncol = 21)
ctr = 2
for (i in 1:21)
{
  for (j in i:21)
  {
    CorMat2[j,i] = CorMat$Corr[ctr]
    ctr = ctr+ 1
  }
}
dimnames(CorMat2) = list(unique(CorMat$Var2),unique(CorMat$Var2))
CorMat2 = cbind(CorMat2, CorMat$Corr[2:22])
write.csv(CorMat2, file = "TrainCorMatrix.csv", )


########## Looking at the CTR of the most populous 2 categories for each variable
FreqDFTrain = FreqDFTrain[order(FreqDFTrain$Colname, FreqDFTrain$Clicks, decreasing=T),]
FreqDFTrain$CTR = FreqDFTrain$Clicks/FreqDFTrain$Freq
CTRTop2 = ddply(FreqDFTrain, .(Colname), function(x) x[1:3,])


########################################## Looking at interactions #########################################
AppSummary = ddply(TrainData, .(app_category,app_domain,app_id), function(x) c(sum(x$click), length(x$click)))
colnames(AppSummary) = c('app_category','app_domain','app_id','Clicks','Freq')
AppSummary$CTR = AppSummary$Clicks/AppSummary$Freq
sum(AppSummary$Freq[AppSummary$Freq>10 & AppSummary$CTR<.03])



################################ Looking at Time Series of CTR broken by category ################################
AppSummary = ddply(TrainData, .(site_category, DayOfWeek, Hour), function(x) c(sum(x$click), length(x$click)))
colnames(AppSummary) = c('site_category','DayOfWeek','Hour','Clicks','Freq')
AppSummary$CTR = AppSummary$Clicks/AppSummary$Freq
TimeCorr = ddply(AppSummary, .(site_category), function(x) {c(length(x$CTR),cor(x$CTR, 1:length(x$CTR)))})
TimeCorr = TimeCorr[order(TimeCorr$V1, decreasing = T),]

AppSiteSummary = ddply(TrainData, .(site_category,site_category), function(x) c(sum(x$click), length(x$click)))


