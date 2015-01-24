library(plyr)
library(dplyr)
library(caret)
library(randomForest)
library(ISLR)

## Load training and testing data sets
pmlTrain <- read.csv("./pml-training.csv")
pmlTest <- read.csv("./pml-testing.csv")
vnames <- colnames(pmlTrain)
allComplete <- complete.cases(pmlTrain)  ## Select cases with no missing values

## Used naCnt to check missing values
naCnt = function(x) {
    sums = vector()
    for (j in 1:length(colnames(x)) ) 
   	sums[j] <- sum(is.na(x[,j]))
    sums
 }

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

## used colSelect to help select subsets of variables to use in modeling
colSelect = function(n1,n2,cnames) {
    c(n1:n2,length(cnames))
}
## Look at data using this (for some reason needed 56 or "y" never got a row/col
fP1 <- featurePlot(x=training[,c(3,5,9,11,12,56)],y=training$classe,plot="pairs")
fP41t48 <- featurePlot(x=training[,c(41:48,56)],y=training$classe,plot="pairs")

nacnt <- naCnt(pmlTrain)
chkNA <- vector()
for (i in 1:length(nacnt)) chkNA[i] <- nacnt[i] > 0
## Get variable names for missing data
## Examining this data informed me that there are 406 complete cases
## and 19,216 cases that are complete for 93 variables
missIdx <- which(chkNA)
completeTrain <- pmlTrain[,-missIdx]
missingTrain <- pmlTrain[,missIdx]
cnames <- colnames(completeTrain)
##
## Ignore variables with skew, kurtosis, cvtd_timestamp
## new_window, max_, min_, amplitude_
## This leaves me with 56 variables
##
## Will start with complete cases, full variable set (-X)
## X seems to be row number after a sort by classe
##
cn <- colSelect(2,9,vnames)
testTrain <- allCompleteTrain[,cn]
set.seed(8383)
tT1 <- sample(1:dim(testTrain)[1],size=dim(testTrain)[1]/2,replace=F)
tTrain1 <- testTrain[tT1,]
tTest1 <- testTrain[-tT1,]
mF1 <- train(classe ~ . , data = tTrain1, method="rf",prox=TRUE, importance=TRUE)
## first try on the above got 89% correct on tTest1
predictions <- predict(mF1,tTest1)
values <- tTest1$classe

## That seemed too easy and variables with missing values include all averages, stddevs
## and variances
## So start looking at 92 variables w/o missing values (and -X)
varIdx <- colSelect(1,10,cnames)
testTrain <- completeTrain[,varIdx]
set.seed(8387)
tT1 <- sample(1:dim(testTrain)[1],size=200,replace=F)
tTrain1 <- testTrain[tT1,]
tTest1 <- testTrain[-tT1,]
mF1 <- train(classe ~ . , data = tTrain1, method="rf",prox=TRUE, importance=TRUE)
predictions <- predict(mF1,tTest1)
values <- tTest1$classe
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
## Want to check correlations as data prep
## Get errors because many variables are factors (not numeric)
## trouble using as.numeric in mutate or apply
##
## Used following to find factor variables
factIdx <- c()
for (i in 1:88) { if (!is.numeric(corTrain[1,i]) == TRUE) factIdx <- append(factIdx,i)}
##
## Quitting here!  91% on training, 89% on testing (50/50 breakdown)
fit <- train(classe ~ yaw_belt + accel_belt_z + pitch_forearm + accel_dumbbell_x + magnet_belt_y + roll_belt + roll_forearm + gyros_arm_y,method = "gbm", data = training)
## training was 9812 by 56 as was testing
## 

