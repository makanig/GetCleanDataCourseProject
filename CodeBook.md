---
title: "CodeBook.md - Description of the 5 Steps to derive the required tidy data set and the Data Dictionary"
output: html_document
---


### This is a summary of the steps - refer to the R code in run_analysis.R for full details

## Step 1 Combine the testing and training data

```
yTrainData <- read.table(yTrain,header=FALSE)
xTrainData <- read.table(xTrain,header=FALSE)
subTrainData <- read.table(subTrain,header=FALSE)

yTestData <- read.table(yTest,header=FALSE)
xTestData <- read.table(xTest,header=FALSE)
subTestData <- read.table(subTest,header=FALSE)

train <- cbind(subTrainData, yTrainData, xTrainData)
test <- cbind(subTestData, yTestData, xTestData)
combined <- rbind(train, test, make.row.names=FALSE)
```

## Step 2 Look for attribute names matching mean and std
```
meanCols <- sqldf("select * from featuresData where V2 LIKE '%mean%'")
stdCols <- sqldf("select * from featuresData where V2 LIKE '%std%'")
colSet <- rbind(meanCols, stdCols, make.row.names=FALSE)
colnames(colSet) <- c("AttrIndex", "AttrName")
```
## Step 3 Uses descriptive activity names to name the activities in the data set
## This is done by culling the attributes with the filtered list from above
```
neededColumns <- c(1:2, as.numeric(colSetOrdered$AttrIndex))
combined <- combined[,neededColumns]

colnames(combined) <- c("Subject", "Activity", colSetOrdered$AttrName)
activityLabelData <- read.table(activityLabels, stringsAsFactors = FALSE, col.names = c("Index", "Activity"))

combined$Activity <- activityLabelData$Activity[combined$Activity]

```

## Step 4 Appropriately labels the data set with descriptive variable names (happens after 5)
```
colSetOrdered <-  colSet[order(colSet$AttrIndex),] 
colSetOrdered$AttrIndex <- colSetOrdered$AttrIndex + 2

colSetOrdered[] <- lapply(colSetOrdered, as.character)
colnames(d) <- c("Subject", "Activity", colSetOrdered$AttrName)

```
# Step 5  Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

```
neededColumns <- c(1:2, as.numeric(colSetOrdered$AttrIndex))
combined <- combined[,neededColumns]

colnames(combined) <- c("Subject", "Activity", colSetOrdered$AttrName)

activityLabelData <- read.table(activityLabels, stringsAsFactors = FALSE, col.names = c("Index", "Activity"))
combined$Activity <- activityLabelData$Activity[combined$Activity]

library(plyr)
doMean <- function (x) { # Mean by activity and subject
  n <- ncol(x)
  l <- lapply(x[,3:n], mean)
  as.numeric(l)
}
d <- ddply(combined, .(Subject, Activity),  c("doMean"))
```
## The resultant data set is a tidy set because each variable measured is in one column and each different (mean) observation is in one row.

## The full list of attributes and data types is described below

* Subject           Integer  1-30
* Activity           Factor  WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
* tBodyAcc-mean()-X   Numeric  Average across measurements
* tBodyAcc-mean()-Y   Numeric  Average across measurements
* tBodyAcc-mean()-Z   Numeric  Average across measurements
* tBodyAcc-std()-X   Numeric  Average across measurements
* tBodyAcc-std()-Y   Numeric  Average across measurements
* tBodyAcc-std()-Z   Numeric  Average across measurements
* tGravityAcc-mean()-X   Numeric  Average across measurements
* tGravityAcc-mean()-Y   Numeric  Average across measurements
* tGravityAcc-mean()-Z   Numeric  Average across measurements
* tGravityAcc-std()-X   Numeric  Average across measurements
* tGravityAcc-std()-Y   Numeric  Average across measurements
* tGravityAcc-std()-Z   Numeric  Average across measurements
* tBodyAccJerk-mean()-X   Numeric  Average across measurements
* tBodyAccJerk-mean()-Y   Numeric  Average across measurements
* tBodyAccJerk-mean()-Z   Numeric  Average across measurements
* tBodyAccJerk-std()-X   Numeric  Average across measurements
* tBodyAccJerk-std()-Y   Numeric  Average across measurements
* tBodyAccJerk-std()-Z   Numeric  Average across measurements
* tBodyGyro-mean()-X   Numeric  Average across measurements
* tBodyGyro-mean()-Y   Numeric  Average across measurements
* tBodyGyro-mean()-Z   Numeric  Average across measurements
* tBodyGyro-std()-X   Numeric  Average across measurements
* tBodyGyro-std()-Y   Numeric  Average across measurements
* tBodyGyro-std()-Z   Numeric  Average across measurements
* tBodyGyroJerk-mean()-X   Numeric  Average across measurements
* tBodyGyroJerk-mean()-Y   Numeric  Average across measurements
* tBodyGyroJerk-mean()-Z   Numeric  Average across measurements
* tBodyGyroJerk-std()-X   Numeric  Average across measurements
* tBodyGyroJerk-std()-Y   Numeric  Average across measurements
* tBodyGyroJerk-std()-Z   Numeric  Average across measurements
* tBodyAccMag-mean()   Numeric  Average across measurements
* tBodyAccMag-std()   Numeric  Average across measurements
* tGravityAccMag-mean()   Numeric  Average across measurements
* tGravityAccMag-std()   Numeric  Average across measurements
* tBodyAccJerkMag-mean()   Numeric  Average across measurements
* tBodyAccJerkMag-std()   Numeric  Average across measurements
* tBodyGyroMag-mean()   Numeric  Average across measurements
* tBodyGyroMag-std()   Numeric  Average across measurements
* tBodyGyroJerkMag-mean()   Numeric  Average across measurements
* tBodyGyroJerkMag-std()   Numeric  Average across measurements
* fBodyAcc-mean()-X   Numeric  Average across measurements
* fBodyAcc-mean()-Y   Numeric  Average across measurements
* fBodyAcc-mean()-Z   Numeric  Average across measurements
* fBodyAcc-std()-X   Numeric  Average across measurements
* fBodyAcc-std()-Y   Numeric  Average across measurements
* fBodyAcc-std()-Z   Numeric  Average across measurements
* fBodyAcc-meanFreq()-X   Numeric  Average across measurements
* fBodyAcc-meanFreq()-Y   Numeric  Average across measurements
* fBodyAcc-meanFreq()-Z   Numeric  Average across measurements
* fBodyAccJerk-mean()-X   Numeric  Average across measurements
* fBodyAccJerk-mean()-Y   Numeric  Average across measurements
* fBodyAccJerk-mean()-Z   Numeric  Average across measurements
* fBodyAccJerk-std()-X   Numeric  Average across measurements
* fBodyAccJerk-std()-Y   Numeric  Average across measurements
* fBodyAccJerk-std()-Z   Numeric  Average across measurements
* fBodyAccJerk-meanFreq()-X   Numeric  Average across measurements
* fBodyAccJerk-meanFreq()-Y   Numeric  Average across measurements
* fBodyAccJerk-meanFreq()-Z   Numeric  Average across measurements
* fBodyGyro-mean()-X   Numeric  Average across measurements
* fBodyGyro-mean()-Y   Numeric  Average across measurements
* fBodyGyro-mean()-Z   Numeric  Average across measurements
* fBodyGyro-std()-X   Numeric  Average across measurements
* fBodyGyro-std()-Y   Numeric  Average across measurements
* fBodyGyro-std()-Z   Numeric  Average across measurements
* fBodyGyro-meanFreq()-X   Numeric  Average across measurements
* fBodyGyro-meanFreq()-Y   Numeric  Average across measurements
* fBodyGyro-meanFreq()-Z   Numeric  Average across measurements
* fBodyAccMag-mean()   Numeric  Average across measurements
* fBodyAccMag-std()   Numeric  Average across measurements
* fBodyAccMag-meanFreq()   Numeric  Average across measurements
* fBodyBodyAccJerkMag-mean()   Numeric  Average across measurements
* fBodyBodyAccJerkMag-std()   Numeric  Average across measurements
* fBodyBodyAccJerkMag-meanFreq()   Numeric  Average across measurements
* fBodyBodyGyroMag-mean()   Numeric  Average across measurements
* fBodyBodyGyroMag-std()   Numeric  Average across measurements
* fBodyBodyGyroMag-meanFreq()   Numeric  Average across measurements
* fBodyBodyGyroJerkMag-mean()   Numeric  Average across measurements
* fBodyBodyGyroJerkMag-std()   Numeric  Average across measurements
* fBodyBodyGyroJerkMag-meanFreq()   Numeric  Average across measurements
* angle(tBodyAccMean,gravity)   Numeric  Average across measurements
* angle(tBodyAccJerkMean),gravityMean)   Numeric  Average across measurements
* angle(tBodyGyroMean,gravityMean)   Numeric  Average across measurements
* angle(tBodyGyroJerkMean,gravityMean)   Numeric  Average across measurements
* angle(X,gravityMean)   Numeric  Average across measurements
* angle(Y,gravityMean)   Numeric  Average across measurements
* angle(Z,gravityMean)   Numeric  Average across measurements
