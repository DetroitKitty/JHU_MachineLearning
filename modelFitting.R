library(plyr)
library(dplyr)
library(caret)
library(randomForest)
library(ISLR)

## Load training and testing data sets
pmlTrain <- read.csv("./pml-training.csv")
pmlTest <- read.csv("./pml-testing.csv")
vnames <- colnames(pmlTrain)

## Used naCnt to check missing values
naCnt = function(x) {
    sums = vector()
    for (j in 1:length(colnames(x)) ) 
   	sums[j] <- sum(is.na(x[,j]))
    sums
 }

## percentCorrect used in cross-validation
percentCorrect = function(predictions,values) {
	if (length(values) > 0)
		x <- sum(predictions==values)/length(values)
	else x<- 0
	x
}

pml_write_files = function(x) {
   n = length(x)
   for (i in 1:n){
	filename = paste0("problem_id_",i,".txt")
 	write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

## Examining this data informed me that there are 406 complete cases
## and 19,216 cases that are complete for 93 variables
## Keep variables w/o na
nacnt <- sum(pmlTrain)
naTF <- nacnt == 0
allComplete <- pmlTrain[,naTF]
cnames <- colnames(allComplete)

## Look at data using this (for some reason needed 93 or "y" (ie classe) never got a row/col
## Repeated until all variables plotted against classe
fP1 <- featurePlot(x=allComplete[,c(3,5,9,11,12,93)],y=training$classe,plot="pairs")
fP41t48 <- featurePlot(x=training[,c(41:48,93)],y=training$classe,plot="pairs")

##
## Ignore variables with skew, kurtosis, cvtd_timestamp
## new_window, max_, min_, amplitude_
## This leaves me with 56 variables
##
## Will start with complete cases, full variable set (-X)
## X seems to be row number after a sort by classe
##
## Repeated following for various variable names.
## Realize now it is more efficient to spell out variables in train rather than keep making dataframes
cn <- colSelect(2,9,vnames)
testTrain <- allComplete[,cn]
set.seed(8383)
tT1 <- sample(1:dim(testTrain)[1],size=dim(testTrain)[1]/2,replace=F)
tTrain1 <- testTrain[tT1,]
tTest1 <- testTrain[-tT1,]
mF1 <- train(classe ~ . , data = tTrain1, method="rf",prox=TRUE, importance=TRUE)
## CROSS-VALIDATION
## first try on the above got 89% correct on tTest1
predictions <- predict(mF1,tTest1)
values <- tTest1$classe
percentCorrect(predictions,values)

## That seemed too easy and variables with missing values include all averages, stddevs
## and variances
## So start looking at 92 variables w/o missing values (and -X)
varIdx <- colSelect(1,10,cnames)
testTrain <- allComplete[,varIdx]
set.seed(8387)
tT1 <- sample(1:dim(testTrain)[1],size=200,replace=F)
tTrain1 <- testTrain[tT1,]
tTest1 <- testTrain[-tT1,]
mF1 <- train(classe ~ . , data = tTrain1, method="rf",prox=TRUE, importance=TRUE)
predictions <- predict(mF1,tTest1)
values <- tTest1$classe
## CROSS VALIDATE
percentCorrect(predictions,values)
## Based on this keep roll_belt, pitch_belt, num_window and add others
## Be sure X has been removed from completeTrain
cnames <- colnames(completeTrain)
set.seed(1913)
ranVar <- sample(11:length(cnames),size=10,replace=F)
varIdx <- c(2,6,7,8,ranVar,length(cnames))
sort(varIdx)
testTrain <- completeTrain[,varIdx]
## Random selection picked many kurtosis and skewness variables which added nothing 
## remove this selection using 15 variables performed worse than one using 10
##
## used grp to remove "kurtosis", "skew" and create completeTrain2
## start over with seed(1913) using completeTrain2
## partitioned with 50% testing, 50% training
##
##
inTrain <- createDataPartition(y=allComplete$classe,p=0.5,list=FALSE)
training <- allComplete[inTrain,]
testing <- allComplete[-inTrain,]
## Quitting here!  91% on training, 89% on testing (50/50 breakdown on allComplete)
fit <- train(classe ~ yaw_belt + accel_belt_z + pitch_forearm + accel_dumbbell_x + magnet_belt_y + roll_belt + roll_forearm + gyros_arm_y,method = "gbm", data = training)
## training was 9812 by 56 as was testing
## Check on testing
predictions <- predict(fit,data=testing)
values <- testing$values
percentCorrect(predictions)
## 

