# GettingAndCleaningDataAssignment
R Programming - Getting And Cleaning Data Week 4 Assignment 

# Library 

library(data.table)
library(dplyr)

# Reading supporting metadata 

metafeature <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE)

# Reading training data 

subjectTrain <- read.table("subject_train.txt", header = FALSE)
activityTrain <- read.table("y_train.txt", header = FALSE)
featuresTrain <- read.table("X_train.txt", header = FALSE)

# Reading test data 

subjectTest <- read.table("subject_test.txt", header = FALSE)
activityTest <- read.table("y_test.txt", header = FALSE)
featuresTest <- read.table("X_test.txt", header = FALSE)

# Merging training and test data 

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Naming columns and merging

colnames(features) <- t(metafeature[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


# Extracting columns with means and std. 

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)

extractedData <- completeData[,requiredColumns]

# Use descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}


# factor the activity variable 

extractedData$Activity <- as.factor(extractedData$Activity)

# appropriately labels the data set with descriptive variable names

  #names
  
names(extractedData)

# changing names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

# check new names 

names(extractedData)

# creates a second, independent tidy data set with the average of each variable for each activity and each subject

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# create tidy data

TidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
TidyData <- TidyData[order(TidyData$Subject,TidyData$Activity),]
write.table(TidyData, file = "TidyData.txt", row.names = FALSE)
