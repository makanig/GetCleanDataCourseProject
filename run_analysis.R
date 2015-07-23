# Getting & Cleaning Data Project
# You should create one R script called run_analysis.R that does the following. 
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


setdir <- function() {
  setwd("C:/Users/gautam.Mom81/Documents/courseraDataScienceSpecialization/gettingCleaningData/project")
}

subTest <- "data/test/subject_test.txt"
xTest <- "data/test/X_test.txt"
yTest <- "data/test/y_test.txt"

subTrain <- "data/train/subject_train.txt"
xTrain <- "data/train/X_train.txt"
yTrain <- "data/train/y_train.txt"

features <- "data/features.txt"
activityLabels <- "data/activity_labels.txt"

#if they used some other way of saving the file than a default write.table, this step will be different

yTrainData <- read.table(yTrain,header=FALSE)
xTrainData <- read.table(xTrain,header=FALSE)
subTrainData <- read.table(subTrain,header=FALSE)

yTestData <- read.table(yTest,header=FALSE)
xTestData <- read.table(xTest,header=FALSE)
subTestData <- read.table(subTest,header=FALSE)

train <- cbind(subTrainData, yTrainData, xTrainData)
test <- cbind(subTestData, yTestData, xTestData)
combined <- rbind(train, test, make.row.names=FALSE)

featuresData <- read.table(features, header=FALSE)

library(sqldf)

meanCols <- sqldf("select * from featuresData where V2 LIKE '%mean%'")
stdCols <- sqldf("select * from featuresData where V2 LIKE '%std%'")
colSet <- rbind(meanCols, stdCols, make.row.names=FALSE)
colnames(colSet) <- c("AttrIndex", "AttrName")

# sort by column and add 2 to each column number to account for the first 2 columns
colSetOrdered <-  colSet[order(colSet$AttrIndex),] 
colSetOrdered$AttrIndex <- colSetOrdered$AttrIndex + 2

colSetOrdered[] <- lapply(colSetOrdered, as.character)

#reduce combined to the first 2 columns and the ones in AttrIndex 
neededColumns <- c(1:2, as.numeric(colSetOrdered$AttrIndex))
combined <- combined[,neededColumns]

colnames(combined) <- c("Subject", "Activity", colSetOrdered$AttrName)

activityLabelData <- read.table(activityLabels, stringsAsFactors = FALSE, col.names = c("Index", "Activity"))

combined$Activity <- activityLabelData$Activity[combined$Activity]

library(plyr)

# Mean by activity and subject

doMean <- function (x) {
  n <- ncol(x)
  l <- lapply(x[,3:n], mean)
  as.numeric(l)
}

d <- ddply(combined, .(Subject, Activity),  c("doMean"))

#set colnames
colnames(d) <- c("Subject", "Activity", colSetOrdered$AttrName)

write.table(d, file="projectOutput.txt", row.names=FALSE)

## Write out CodeBook.md
sink("CodeBook.md")
fmt <- "* %-15s  %8s  %s\n"

cat(sprintf(fmt, "Subject", "Integer", "1-30"))
cat(sprintf(fmt, "Activity", "Factor", toString(activityLabelData$Activity) ))
ncolD <- ncol(d)
attrNames <- colnames(d[,3:ncolD])
for (a in attrNames) {
  cat(sprintf(fmt, a, "Numeric", "Average across measurements"))
}
sink()

