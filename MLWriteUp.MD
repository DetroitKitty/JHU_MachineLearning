#  Practical Machine Learning Write-up
### Note that I have not provided a compiled HTML file as I do not know how to do this and don't have the time to figure it out right now.  I am providing this write-up and a .R file with steps used in analyzing the data and the final model.

## Goal: Predict manner exercise is performed based on data taken during exercise. The variable classe describes the exercise method. It is a factor variable with 5 levels denoted A:E.

## Summary of steps
1. Data cleaning:
   a. looked for and removed missing values 
2. Exporing data
   a. plotted data again classe to look for correlations, patterns
   b. ran correlation function
3. Looked for significant variables
   a. Selected subsets of 200 observations and 10 variables and fit using random forest.
   b. Performed cross-validation on all runs. 
   c.  Narrows variables down to 5 or 6 but realized this is misuse of random forests as it selects observations and variables
   d.  Repeated a and b using 50% of training data and random tree. (Would use more than 50% next time.)
   e. Cross validated and made variable selection.
4. As random tree results weren't great, used gbm with selected variables.  Cross-validated. Results reasonable. Stopped. 

## Data:
The Training data contains 19,622 observations of 160 variables.
### First pass at data - look for missing values.
*  I wrote a short function `naCnt()` to find variables with missing values.
*  This showed 67 variables were missing 19,216 observations. 
*  Setting a TRUE/FALSE vector based on na's > 0 I could see which variables.
	* All variables with missing values had std or mean in the name
The 67 variables with missing values were removed using `complete.cases()`

## Data Features:
### Visualization
I used featurePlot to examine the variables pair-wise

_fP1 <- featurePlot(x=training[,c(3,5,9,11,12,56)],y=training$classe,plot="pairs")_
 
* Many of the variables with x,y and z components looked as if the x, y and z were correlated.
* Some of the variables did not vary much in value.  With more time I would have scaled and checked. 
* The variable X was removed from consideration as it seemed to be row number and with data sorted by classe.

### Correlation
I used the correlation function to quantitatively explore correlation.  
* Many variables were correlated by more than 80%.

## Selecting significant variables
1. I selected a small subset (200) of training observations and ran Random Forest repeatedly using different variables, then cross-validated with the remainder of the training data.
* This process picked out yaw_belt, magnet_belt, roll_belt and roll_forearm as being significant
2. I read more on random forest and realized random forest performed the test I was running with a random tree model.  So I tried running on random forest on all remaining data and variables - it had not stopped after 18 hours so I killed it.
3. I selected random samples of 50% of the training data and 10 variables and ran random tree.
* I settled on yaw_belt + accel_belt_z + pitch_forearm + accel_dumbbell_x + magnet_belt_y + roll_belt + roll_forearm + gyros_arm_y as variables.
4. I wrote a function percentCorrect to quickly see the percent of predictions that equaled the classe value.

## Final model
The random tree was not producing great fits (in the 70s percentile.)  I did not see strong relationships in any of the data when I plotted the data.  I thought it was worthwhile to attempt boosting by using method gbm. This resulted in 91% success for training data (50% of original training data selected at random) and 89% success on cross-validation (remaining 50%).  I expected at least 80% success on the test data but I only achieved 14/20. 14/20 seems poor but the it is much better than random as there is 20% chance of correct classification. In restrospect I would use a higer percentage of the training data to fit the model and reduce the percentage for cross-validation. At the time I was concerned about speed but the number of variables seems to slow the process much more than the number of observations.

_fit <- `train(classe ~ yaw_belt + accel_belt_z + pitch_forearm + accel_dumbbell_x + magnet_belt_y + roll_belt + roll_forearm + gyros_arm_y,method = "gbm", data = training)`_

## Conclusion
This was a good learning experience if not the most satisfying result. But there is much to learn.



