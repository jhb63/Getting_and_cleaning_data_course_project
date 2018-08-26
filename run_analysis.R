#packages

library(dplyr)

# download zip file 
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file 
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

#Read Data sets

# read training 
trainsub <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainvalue <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainactivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test 
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)


# read activity 
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")



#TASK 1: Merge the training and the test sets to create one data set


# merge both data tables + Rename
ActivityData <- rbind(
  cbind(trainsub, trainvalue, trainactivity),
  cbind(testSubjects, testValues, testActivity)
)
colnames(ActivityData) <- c("subject", features[, 2], "activity")



#TASK2: - Extract only the measurements on the mean and standard deviationfor each measurement


# select columns
selectcolums <- grepl("subject|activity|mean|std", colnames(ActivityData))
ActivityData <- ActivityData[, selectcolums]


#TASK3: Use descriptive activity names to name the activities in the data set

ActivityData$activity <- factor(ActivityData$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# Step 4 - Appropriately label the data set with descriptive variable names
ActivityDataCols <- colnames(ActivityData)

ActivityDataCols <- gsub("^f", "frequencyDomain", ActivityDataCols)
ActivityDataCols <- gsub("^t", "timeDomain", ActivityDataCols)
ActivityDataCols <- gsub("Acc", "Accelerometer", ActivityDataCols)
ActivityDataCols <- gsub("Gyro", "Gyroscope", ActivityDataCols)
ActivityDataCols <- gsub("Mag", "Magnitude", ActivityDataCols)
ActivityDataCols <- gsub("Freq", "Frequency", ActivityDataCols)
ActivityDataCols <- gsub("mean", "Mean", ActivityDataCols)
ActivityDataCols <- gsub("std", "StandardDeviation", ActivityDataCols)
ActivityDataCols <- gsub("BodyBody", "Body", names(Data))


#New labels 
colnames(ActivityData) <- ActivityDataCols


# Step 5 - Create a second, independent tidy set with the average of each variable for each activity and each subject

# Dplyr-piping/group by
ActivityDataMeans <- ActivityData %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(ActivityDataMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

getwd

