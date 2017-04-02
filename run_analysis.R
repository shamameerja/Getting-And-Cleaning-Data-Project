#### downloading data using R code and extracting zip file

if(!file.exists("./samsungdata")){dir.create("./samsungdata")}

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="./samsungdata/Dataset.zip",mode = "wb")

unzip(zipfile="./samsungdata/Dataset.zip",exdir="./samsungdata")

#### to use read.table function loading required package
library(dplyr)

#### creating all data tables to see look and feel of the data

features <- read.table("./samsungdata/UCI HAR Dataset/features.txt" , as.is = TRUE)

activities <- read.table("./samsungdata/UCI HAR Dataset/activity_labels.txt")

trainingSubjects <- read.table("./samsungdata/UCI HAR Dataset/train/subject_train.txt")
trainingValues <- read.table("./samsungdata/UCI HAR Dataset/train/X_train.txt")
trainingActivity <- read.table("./samsungdata/UCI HAR Dataset/train/y_train.txt")

testSubjects <- read.table("./samsungdata/UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("./samsungdata/UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("./samsungdata/UCI HAR Dataset/test/y_test.txt")

####changing activities labels
colnames(activities) <- c("activityId", "activityLabel")

#### used summary,head to check the data and understand the structure.
#### Since 561 features values are in X_test,X_train files ,we need combine both and assign column names from features.txt file.
#### then only search for mean ,sd using grep ,i would like merge with activities table to get labels.

# concatenate individual data tables to make single data table
ActivityData <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(ActivityData) <- c("subject", features[, 2], "activityId")

####step1 result is achieved !!!

#### Fetching only mean ,sd data out of 561 features list.
columnsToRetain <- grep("subject|activityId|mean|std" , colnames(ActivityData))
ActivityData <- ActivityData[,columnsToRetain]

####Uses descriptive activity names to name the activities in the data set

ActivityData <- merge(ActivityData,activities,by = "activityId")

####Appropriately labels the data set with descriptive variable names.
#### changing junk symbols
ActivityDataCols <- colnames(ActivityData)
ActivityDataCols  <- gsub("[\\(\\)-]", "",ActivityDataCols)

ActivityDataCols  <- gsub("BodyBody", "Body",ActivityDataCols)

ActivityDataCols <- gsub("^f", "frequencyDomain", ActivityDataCols)
ActivityDataCols <- gsub("^t", "timeDomain", ActivityDataCols)
ActivityDataCols <- gsub("Acc", "Accelerometer", ActivityDataCols)
ActivityDataCols <- gsub("Gyro", "Gyroscope", ActivityDataCols)
ActivityDataCols <- gsub("Mag", "Magnitude", ActivityDataCols)
ActivityDataCols <- gsub("Freq", "Frequency", ActivityDataCols)
ActivityDataCols <- gsub("mean", "Mean", ActivityDataCols)
ActivityDataCols <- gsub("std", "StandardDeviation", ActivityDataCols)

colnames(ActivityData) <- ActivityDataCols

#### removing activityId

ActivityData <- ActivityData[,-(1)]

####From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

ActivityDataMeans <- ActivityData %>% group_by(subject, activityLabel) %>% summarise_each(funs(mean))

#### code to write tidy data to file
write.table(ActivityDataMeans, "tidy_data.txt", row.names = FALSE,quote = FALSE)

