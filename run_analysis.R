#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis, and 
# 3) a code book that describes the variables, the data, and any transformations or work that you performed to 
# clean up the data called CodeBook.md. 
# You should also include a README.md in the repo with your scripts. 
# This repo explains how all of the scripts work and how they are connected.

#One of the most exciting areas in all of data science right now is wearable computing - 
#  see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
#'The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy 
#S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 


#You should create one R script called run_analysis.R that does the following. 


########################################################################################################################

library(dplyr)
library(data.table)
getwd()
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  

#Extract the dataset with the name Week4_project
# first check if the file exists
filename <- "week4_project"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  
# check if the folder with the name exists before unzipping UCI HAR Dataset 
if (!file.exists("UCI HAR Dataset")){
  unzip(filename)
}

#Merges the training and the test sets to create one data set.
## features and activity
features<-read.table("UCI HAR Dataset/features.txt",col.names = c("Srno.","Assessments"))
activity<-read.table("UCI HAR Dataset/activity_labels.txt",col.names = c("Code","Activities"))

## test data:
XTest<- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$Assessments)
YTest<- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "Code")
SubjectTest <-read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "Subject no.")

## train data:
XTrain<- read.table("UCI HAR Dataset/train/X_train.txt" , col.names = features$Assessments)
YTrain<- read.table("UCI HAR Dataset/train/Y_train.txt" , col.names = "Code")
SubjectTrain <-read.table("UCI HAR Dataset/train/subject_train.txt" , col.names = "Subject no.")


# Merging the train and test datasets
XAll <- rbind(XTrain, XTest)
YAll <- rbind(YTrain, YTest)
Subject <- rbind(SubjectTrain, SubjectTest)
Merged_Data <- cbind(Subject, YAll, XAll)


# Extracts only the measurements on the mean and standard deviation for each measurement. 
TidyData <- Merged_Data %>% select(Subject.no., Code, contains("mean"), contains("std"))

# Uses descriptive activity names to name the activities in the data set

TidyData$Code <- activity[TidyData$Code, 2]

# Appropriately labels the data set with descriptive variable names. 
names(TidyData)[2] <-"Activity"

names(TidyData) <- gsub("ACC","_Accelerometer",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub("Gyro","_Gyroscope",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub("BodyBody","Body",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub("Mag","_Magnitude",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub(".Mean.","_Mean",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub("gravit","_Gravity",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub("_gravityy","_Gravity",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub(".std","_STD",names(TidyData),ignore.case = TRUE)
names(TidyData) <- gsub("^t","Time", names(TidyData))
names(TidyData) <- gsub("^f","Frequency", names(TidyData))
names(TidyData) <- gsub("angle.", "Angle", names(TidyData))
names(TidyData) <- gsub("freq","_Frequency",names(TidyData))
names(TidyData) <- gsub("Meanreq","Mean_Frequency",names(TidyData),ignore.case=TRUE, fixed = TRUE)
names(TidyData) <- gsub("TimeBody", "Time_Body", names(TidyData))
names(TidyData) <- gsub("FrequencyBody", "Frequency_Body", names(TidyData))
names(TidyData) <- gsub("AngletBody", "Angle_Body", names(TidyData))

# From the data set in step 4, 
# creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Average_Data <- TidyData %>% 
  group_by(Subject.no., Activity) %>%
  summarise_all(funs(mean))