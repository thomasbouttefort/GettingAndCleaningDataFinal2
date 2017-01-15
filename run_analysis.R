## Part I - Setting wd & getting the data 

##Loading the two R packages I'll need to perform the task
packages = c("data.table", "reshape2")
sapply(packages, library, character.only = TRUE, quietly = TRUE)

## Making sure I'm in the correct working directory and creating 
## the specific one I'll need 
workingdir = "/Users/thomasbouttefort/Desktop/Coursera R/R/GettingAndCleaningDataFinal2"
if (!file.exists(workingdir)) {
  dir.create(workingdir)
}
setwd("/Users/thomasbouttefort/Desktop/Coursera R/R/GettingAndCleaningDataFinal2")

## Downloading the files and unziping them. 
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file = "Dataset.zip"

download.file(url, destfile = file)
unzip(file)

## Setting the wd to the file that we just unzipped 
setwd("UCI HAR Dataset")

## Reading the subject files
dtSubjectTrain = fread("train/subject_train.txt")
dtSubjectTest = fread("test/subject_test.txt")

## Reading the activity files
dtActivityTrain = fread("train/Y_train.txt")
dtActivityTest = fread("test/Y_test.txt")

## Reading the data files by creating a small function that combines read.table 
## & data.table. Fread can have errors here. 
ReadDataFile = function(f) {
  df = read.table(f)
  dt = data.table(df)
}
dtTrain = ReadDataFile("train/X_train.txt")
dtTest = ReadDataFile("test/X_test.txt")

## Part II - Merging the data

## Concatenate
dtSubject = rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity = rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt = rbind(dtTrain, dtTest)

## Merge  colmuns
dtSubject = cbind(dtSubject, dtActivity)
dt = cbind(dtSubject, dt)

## Set key
setkey(dt, subject, activityNum)

## Part III  - Extracting the mean and the standard deviation

## Reading the features.txt file to see which variable are the mean & the sd. 
dtFeatures = fread("features.txt")
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

## Subseting them and converting the column numbers to a vector of variable 
## names matching columns in dt
dtFeatures = dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
dtFeatures$featureCode = dtFeatures[, paste0("V", featureNum)]
select = c(key(dt), dtFeatures$featureCode)
dt = dt[, select, with = FALSE]

## Part III  - Using descriptive activity names

dtActivityNames = fread("activity_labels.txt")
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

## Part IV  - Labeling with descriptive activity names

## Merging activity labels.
dt = merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)

## Adding  activityName as a key.
setkey(dt, subject, activityNum, activityName)

## Melting the data table to reshape it 
dt = data.table(melt(dt, key(dt), variable.name = "featureCode"))

## Merging activity name.
dt = merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
            all.x = TRUE)

## Creating a new variable, activity that is equivalent to activityName 
## as a factor class. Create a new variable, feature that is equivalent to 
## featureName as a factor class.
dt$activity = factor(dt$activityName)
dt$feature = factor(dt$featureName)

## Creating a helper function greplthose that will 
## seperate features from featureName 

greplthose = function(regex) {
  grepl(regex, dt$feature)
}

## Features with 1 category
dt$featJerk = factor(greplthose("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude = factor(greplthose("Mag"), labels = c(NA, "Magnitude"))

## Features with 2 categories
n = 2
y = matrix(seq(1, n), nrow = n)
x = matrix(c(greplthose("^t"), greplthose("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x = matrix(c(greplthose("Acc"), greplthose("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x = matrix(c(greplthose("BodyAcc"), greplthose("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x = matrix(c(greplthose("mean()"), greplthose("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))

## Features with 3 categories
n = 3
y = matrix(seq(1, n), nrow = n)
x = matrix(c(greplthose("-X"), greplthose("-Y"), greplthose("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

## Part V  - Creating a tidy data set and extracting it

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
dtTidy = dt[, list(count = .N, average = mean(value)), by = key(dt)]
write.table(dtTidy, "dtTidy.txt", sep="\t")