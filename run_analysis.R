##PROBLEM
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names.
#5.From the data set in step 4, creates a second, independent tidy data set with the average
#  of each variable for each activity and each subject.


##SOLUTION
#download the data file and load the data into R
temp <- tempfile()
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, temp, mode="wb") #save the zipfile into the temp
raw <- unzip(temp)
unlink(temp) #remove temp file
subject_test <- read.table(raw[14])
X_test <- read.table(raw[15])
y_test <- read.table(raw[16])
subject_train <- read.table(raw[26])
X_train <- read.table(raw[27])
y_train <- read.table(raw[28])

#merge the training and testing data sets
subject <- rbind(subject_train, subject_test)
X <- rbind(X_train, X_test)
y <- rbind(y_train,y_test)

#extract the measurements on the mean and standard deviation for each measurement
features <- read.table(raw[2],stringsAsFactors=FALSE)
cols <- grep("(mean|std)", features[,2])

#.Uses descriptive activity names to name the activities in the data set
activities <- read.table(raw[1],stringsAsFactors=FALSE)
activity <- merge(y,activities,by="V1",sort=FALSE)[,2]
bodyMoving <- cbind(subject, X[,cols], activity)

#name the variables
names(bodyMoving) <- c("subject", features[cols,2], "activity")

#5.creates a second, independent tidy data set with the average of each variable for each
#  activity and each subject.
library(dplyr)
library(tidyr)
bodyMoving_avg <- bodyMoving %>%
                  gather(vars, values, -c(subject,activity)) %>% 
                  group_by(subject, activity, vars) %>%
                  summarize(AVG = mean(values)) %>%
                  spread(vars, AVG)


