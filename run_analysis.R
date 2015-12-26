
## Set Working directory and download files

setwd("C:/Users/my lapy/Desktop/Data Science/coursera data scientist specilization/3----getting and cleaning data")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","./rproj.zip")
unzip("./UCI HAR Dataset.zip")
setwd("./UCI HAR Dataset")

## Load necessary libraries

library(XLConnect)
library(dplyr)
library(plyr)
setwd("./UCI HAR Dataset")
list.files()

## Read features

features <- read.table("features.txt" )
head(features)
class(features)
dim(features)
features[,2]=as.character(features[,2])

## Read activity labels

activity_labels<- read.table("./activity_labels.txt")
head(activity_labels)
class(activity_labels[,2])
activity_labels[,2] = as.character(activity_labels[,2])

## Load train data set

X_train <- read.table("./train/X_train.txt")
View(X_train)
dim(X_train)
colnames(X_train) <- as.character(features[,2])

## Extract features wanted (containing mean and std)

featuresWanted <- grep(".*mean.*|.*std.*", features[,2])

## Filter the required training set

X_train_needed <- X_train[featuresWanted]
View(X_train_needed)
dim(X_train_needed)
Y_train <- read.table("./train/Y_train.txt")
View(Y_train)
dim(Y_train)
subject_train <- read.table("./train/subject_train.txt")
colnames(subject_train) = "subject"

## Make a function which will replace the activity labels with descriptive label names

create_activity <- function(activity_labels,Y_train)
{
  activity <- c()
  for(i in 1:length(Y_train[,1]))
  {
    k = Y_train[,1][i]
    name = activity_labels[,2][k]
    activity = c(activity,name)
  }
  activity
}
activity <- create_activity(activity_labels,Y_train)

## Form the required training set

train <- cbind(X_train_needed,activity,subject = subject_train)




## Load the required test set 

X_test <- read.table("./test/X_test.txt")
View(X_test)
dim(X_test)
colnames(X_test) <- as.character(features[,2])

## Filter the required test set

featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
X_test_needed <- X_test[featuresWanted]
View(X_test_needed)
dim(X_test_needed)
Y_test <- read.table("./test/Y_test.txt")
View(Y_test)
dim(Y_test)
subject_test <- read.table("./test/subject_test.txt")

## Make descriptive names for subject and activity label

colnames(subject_test) = "subject"
activity_test<- create_activity(activity_labels,Y_test)

## Form required test set

test <- cbind(X_test_needed,activity = activity_test,subject = subject_test)
dim(test)

## Make complete data

complete_data <- rbind(train,test)

## Melt and dcast data to a tidy data required for submission

melted=melt(complete_data,id.vars = c("subject","activity"))
casted=dcast(melted,subject+activity~variable,mean)
View(casted)
write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

