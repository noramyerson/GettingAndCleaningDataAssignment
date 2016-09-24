
## Running this script will perform the following to the UCI HAR data set available from the URL below:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## 1. Merge the training and test data sets
## 2. Extract only the measurements of mean and standard deviation for each measurement in the set
## 3. Use descriptive activity names to identify values
## 4. Label the data set with those names
## 5. Create a second, seperate, tidy data set containing the average value for each variable per activity in each set

## Successful use of this script creates two data sets - 
## masterData containing just the measurements of the mean and std dev for each measure and tidyMasterData containing the average of each variable for each activity and each subject

##Start  We will start by reading in the data we need from the test and training folders and creating two "master" sets 
##to do our final analysis

## First Set the WD to the UCI HAR DAta Set - Assumes WD  in "documents" (using windows OS) and file has been unzipped

setwd("./UCI HAR Dataset")

## Read in the data for the master test data set we will use header = FALSE to make adding descriptive names easier

featureData <- read.table("./features.txt", header = FALSE) #this will be used for both the train and test data sets
subjectDataTest <- read.table("./test/subject_test.txt", header = FALSE)
testX <- read.table("./test/x_test.txt", header = FALSE)
testY <- read.table("./test/y_test.txt", header = FALSE)

##Label test data with descriptive names

colnames(subjectDataTest) = "subjectID"
colnames(testX) = featureData[,2]
colnames(testY) = "activityID"

## Merge together testY, testX and subjectDataTest data to get the main training set we will manipulate. WE use cbind because we 
## have the same number of observations across sets

masterTest <- cbind(testY,subjectDataTest,testX)


## Read in Necessary Documents for the master training data set. Use 'header = FALSE' to make adding descriptive names easier

subjectDataTrain <- read.table("./train/subject_train.txt", header = FALSE)
trainX  <- read.table("./train/x_train.txt", header = FALSE)
trainY <-read.table("./train/y_train.txt", header = FALSE)

##Label train data with descriptive names

colnames(subjectDataTrain) = "subjectID"
colnames(trainX) = featureData[,2]
colnames(trainY) = "activityID"

## Merge together trainY, trainX and subjectData to get the main training set we will manipulate. We use cbind since we have the same
## number of observations across each set

masterTrain <- cbind(trainY,subjectDataTrain, trainX)

## Create our final master set of data by merging our masterTrain and masterTest sets together we will use rbind this time
## to match our rows correctly and because our sets have different numbers of observations, with training data having more
## observations than our testing data. 

masterData <- rbind(masterTrain, masterTest)


##2. Extract our measurements of standard deviation and mean for each measure in the masterData set

## we will ectract the column names of our final set into a vector - this will make it easier to manipulate our column names 

columnNames <- colnames(masterData)

## Next, we will create a logical vector to narrow down our selection to the columns we want. Column names containign IDs, means,
## and std deviations will show true all other columns will be false and we can use this vector to filter our master data set


measuresWanted <- (grepl("activity..",columnNames) | grepl("subject..",columnNames) | grepl("-mean..",columnNames) & !grepl("-meanFreq..",columnNames) & !grepl("mean..-",columnNames) | grepl("-std..",columnNames) & !grepl("-std()..-",columnNames))

## Now we can use our logical of measuresWanted to subset our masterData set

masterData = masterData[measuresWanted == TRUE]

##3. Now we will deal with using our descriptive names we will read in the activity data, and merge it with our masterData so we can assign our descriptive names to our activities and measures

## read in the activity data to create descriptive names 

activityData <- read.table("./activity_labels.txt" , header = FALSE) 

## name the columns of our acitivy data 

colnames(activityData) = c("activityID", "activityTypes")

##Merge masterData and activityData to add our descriptive activity names

masterData = merge(masterData, activityData, by = "activityID", all.x = TRUE)

##4. Label the data set with the names added in part 3

## we will need our columnNames vector again, so we will need to reassign the vector to our newly subsetted masterData which also includes our activity types

columnNames <- colnames(masterData)

##clean up those dirty dirty column names ;) We will use a for loop to make our lives easier -naming conventions were determined using the original data's features_info file

for (i in 1:length(columnNames)) {
  columnNames[i] = gsub("-mean", "Mean", columnNames[i])                    # containing -mean as Mean
  columnNames[i] = gsub("^(t)", "time", columnNames[i])                     #starting with 't' as time
  columnNames[i] = gsub("Acc", "Acceleration", columnNames[i])              #containing Acc as Acceleration
  columnNames[i] = gsub("-std", "StdDev", columnNames[i])                   #ending with std as StdDev 
  columnNames[i] = gsub("^(f)", "frequency", columnNames[i])                #starting with 'f' as frequncy 
  columnNames[i] = gsub("BodyBody", "Body", columnNames[i])                 #Replaces dublicate Body with singular
  columnNames[i] = gsub("Mag", "Magnitude", columnNames[i])                 #Replaces Mag with Magnitude
  columnNames[i] = gsub("\\()","", columnNames[i])                          #Removes '()' from end of names
}

##Now that our names are SUPER descriptive, we can use our character vector to rename our masterData set

colnames(masterData) = columnNames

##5. Create a second, seperate, tidy data set containing the average value for each variable per activity in each set

##aggregate masterData table into a tidyMasterData set that will include the mean of each variable for each activity & subject - we remove activityID/Types and subjectID from our aggregation of means

tidyMasterData = aggregate(masterData[,names(masterData) != c("activityID","subjectID","activityTypes")], by = list(activityID = masterData$activityID, subjectID = masterData$subjectID, activityTypes = masterData$activityTypes),mean)


