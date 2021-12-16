# Data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# install.packages("dplyr")
library(dplyr)

# Downloading the data and preparing the data
currdir <- "./data"
if(!dir.exists("./data")) dir.create("./data")
setwd(currdir)

dburl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip <- "UCI HAR Dataset.zip"
download.file(dburl, zip)

if(file.exists(zip)) unzip(zip)

# Download files
bdir <- "UCI HAR Dataset"
featuresfile <- paste(bdir, "features.txt", sep="/")
activitylabelsfile <- paste(bdir, "activity_labels.txt", sep="/")
testvariablesfile <- paste(bdir, "test/X_test.txt", sep="/")
testactivityfile <- paste(bdir, "test/y_test.txt", sep="/")
testsubjectfile <- paste(bdir, "test/subject_test.txt", sep="/")
trainvariablesfile <- paste(bdir, "train/X_train.txt", sep="/")
trainactivityfile <- paste(bdir, "train/y_train.txt", sep="/")
trainsubjectfile <- paste(bdir, "train/subject_train.txt", sep="/")

files <- c(featuresfile, activitylabelsfile, testvariablesfile, 
           testactivityfile, testsubjectfile, trainvariablesfile, 
           trainactivityfile, trainsubjectfile)

# Read featuresfile
features <- read.table(featuresfile, col.names=c("rownumber","variablename"))

# Fix the issue with duplicate names (e.g.) 516. fBodyBodyAccJerkMag-mean()
allvar <- 
  mutate(features, variablename = gsub("BodyBody", "Body", variablename))

# Filter the 66 variables - mean() and std()
needvar <- filter(allvar, grepl("mean\\(\\)|std\\(\\)", variablename))

# Transform allvar, Remove special characters, Convert to lower case
allvar <- mutate(allvar, variablename = gsub("-", "", variablename),
                       variablename = gsub("\\(", "", variablename),
                       variablename = gsub("\\)", "", variablename),
                       variablename = tolower(variablename))

# Transform needvar, Remove special characters, Convert to lower case
needvar <- mutate(needvar, variablename = gsub("-", "", variablename),
                          variablename = gsub("\\(", "", variablename),
                          variablename = gsub("\\)", "", variablename),
                          variablename = tolower(variablename))

# Read activitylabelsfile
activitylabels <- read.table(activitylabelsfile, 
                             col.names=c("activity", "activitydescription"))

# Read testvariablesfile
testvalues <- read.table(testvariablesfile, col.names = allvar$variablename)
testneedval <- testvalues[ , needvar$variablename]

# Read testactivityfile
testactivities <- read.table(testactivityfile, col.names=c("activity"))

# Read testsubjectfile
testsubjects <- read.table(testsubjectfile, col.names=c("subject"))

# Activity description
testactivitieswithdescr <- inner_join(testactivities, activitylabels)

# Put the test data together, Combining values, activities, subjects
testdata <- cbind(testactivitieswithdescr, testsubjects, testneedval)

# Read trainvariablesfile
trainvalues <- read.table(trainvariablesfile, col.names = allvar$variablename)
trainneededvalues <- trainvalues[ , needvar$variablename]

# Read trainactivityfile
trainactivities <- read.table(trainactivityfile, col.names=c("activity"))

# Read trainsubjectfile
trainsubjects <- read.table(trainsubjectfile, col.names=c("subject"))

# Activity description
trainactivitieswithdescr <- inner_join(trainactivities, activitylabels)

# Put the train data together
traindata <- cbind(trainactivitieswithdescr, trainsubjects, trainneededvalues)

# Combine the testdata and traindata, Make subject a factor
finaldata <- rbind(testdata, traindata) %>% select( -activity )
finaldata <- mutate(finaldata, subject = as.factor(finaldata$subject))

# Write the data
write.table(finaldata, "Mean and StdDev for Activity Subject.txt")

# Create a second, independent tidy data set with the average of each 
# variable for each activity and each subject.
allgroupeddata <- group_by(finaldata,activitydescription,subject)
summariseddata <- summarise_each(allgroupeddata, funs(mean))
write.table(summariseddata, "Average variable by Activity Subject.txt", 
            row.names = FALSE)
