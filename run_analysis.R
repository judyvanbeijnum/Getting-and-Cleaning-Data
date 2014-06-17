setwd("~/DATA/Courses/Coursera/Getting and cleaning data/Assignment/UCI HAR Dataset")
#################
###Step 1 Merges the training and the test sets to create one data set.

## Read in data of test set
SubTest <- read.table("test/subject_test.txt")  # Subject identifyers
XTest <- read.table("test/X_test.txt")  # Observations of the smartphones
YTest <- read.table("test/y_test.txt")  # Activity labels (1-6)
Feat <- read.table("features.txt")  # Measured features -> column variables
Act <- read.table("activity_labels.txt") # Activity labels explained
IDTest <- as.data.frame(c(1:2947))  # Create identifyer for each measurement for easy binding


## Assemble test set 
# Provide all separate tables with column names
colnames(XTest) <- Feat[,2]  
colnames(SubTest) <- "Subject"
colnames(YTest) <- "Activity label"
colnames(Act) <- c("Activity label", "Activity type")
colnames(IDTest) <- "ID"

TestA <- cbind(IDTest, YTest)  # Give activity measurements an ID
TestAct <- merge(TestA, Act, by="Activity label")  # Merge measurements with activity
TestAct <-TestAct[order(TestAct$ID),]   # Restore order after merging
TestB <- cbind(IDTest, SubTest)  # Give subject measurements an ID
TestC <-cbind(IDTest, XTest) # Give measurements an ID
TestAll <-merge(TestB, TestAct, by="ID")  # Merge sets on ID
TestAll3 <- merge(TestAll, TestC, by= "ID")  # Merge sets on ID

TestSet <- TestAll3  ## All test data, preceded by colums reflecting measurement ID, subject ID and activity


## Read in data of training set
SubTrain <- read.table("train/subject_train.txt")  # Subject identifyers
XTrain <- read.table("train/X_train.txt")  # Observations of the smartphones
YTrain <- read.table("train/y_train.txt")  # Activity labels (1-6)
IDTrain <- as.data.frame(c(1:7352))  # Create identifyer for each measurement for easy binding


## Assemble training set 
# Provide all separate tables with column names
colnames(XTrain) <- Feat[,2]  
colnames(SubTrain) <- "Subject"
colnames(YTrain) <- "Activity label"
colnames(IDTrain) <- "ID"

TrainA <- cbind(IDTrain, YTrain)
TrainAct <- merge(TrainA, Act, by="Activity label")
TrainAct <-TrainAct[order(TrainAct$ID),]
TrainB <- cbind(IDTrain, SubTrain)
TrainC <-cbind(IDTrain, XTrain)
TrainAll <-merge(TrainB, TrainAct, by="ID")
TrainAll3 <- merge(TrainAll, TrainC, by= "ID")

TrainSet <- TrainAll3  ## All train data, preceded by colums reflecting measurement ID, subject ID and activity


## Merge Training and Test set
intersect(colnames(TestSet),colnames(TrainSet)) # Verify presence of variables
Data <- rbind(TrainSet, TestSet) 
View(Data)



###################
### Step2  Extracts only the measurements on the mean and standard deviation for each measurement. 
## Reduce the number of variables/ measurements to the ones that make most sense (Mean & Stdev)

Varnames1 <- grep("mean()", names(Data))  # Select columns with means
Varnames2<- grep("std()", names(Data))  # Select columns with stdevs
Varnames<- c(Varnames1, Varnames2)  # Combine
Varnames3 <- grep("meanFreq", names(Data))  # Identify 'meanFreq'
Varnames <- Varnames[! Varnames %in% Varnames3]  # Remove ´meanFreq´ columns
VarnamesAll <-sort(c(1:4, Varnames))  # Add subject/activity columns
VarnamesAll 

DataSelect <- Data[, VarnamesAll]
View(DataSelect)
colnames(DataSelect)

##############
### Step 3 Uses descriptive activity names to name the activities in the data set
levels(DataSelect[,4]) # Names were already included in merging of datasets

##############
### Step 4 Appropriately labels the data set with descriptive variable names
## Create gsub function for multiple replacement

gsub2 <- function(pattern, replacement, x, ...) {
      for(i in 1:length(pattern))
            x <- gsub(pattern[i], replacement[i], x, ...)
      x
} 

## Define descriptions for the different parts of the variable names
from = c("tBodyAcc-", "fBodyAcc-", "tGravityAcc-", 'tBodyAccJerk-', 
          "tBodyGyro-", "tBodyGyroJerk-", "tBodyAccMag-",
          "tGravityAccMag-", "tBodyAccJerkMag-", "tBodyGyroMag-",
          "tBodyGyroJerkMag-", "fBodyAccJerk-", "fBodyGyro-",
          "fBodyAccMag-", "fBodyBodyAccJerkMag-", "fBodyBodyGyroMag-",
          "fBodyBodyGyroJerkMag-", "mean()", "std()",  "-X", "-Y", "-Z")
to=c ("Body acceleration in time", "Body acceleration frequency", "Gravitational acceleration in time", "Body acceleration jerk in time",
       "Body gyroscope signal in time", "Body gyroscope jerk signal in time", "Body acceleration magnitude in time",
       "Gravitational acceleration magnitude in time", "Body acceleration jerk magnitude in time","Body gyroscope signal magnitude in time", 
       "Body gyroscope jerk signal magnitude in time", "Body acceleration jerk frequency","Body gyroscope signal frequency",
       "Body acceleration frequency magnitude","Body acceleration jerk frequency magnitude", "Body gyroscope signal frequency magnitude",
       "Body gyroscope jerk signal frequency magnitude", " (Mean)", " (Std)", " on X-axis", " on Y-axis", " on Z-axis")

## Rename variables in combined dataset
colnames(DataSelect) <- gsub2(from, to, colnames(DataSelect), fixed=T)
View(DataSelect)


###################
### Step 5 Creates a second, independent tidy data set  with the average of each variable for each activity and each subject. 

## Split data based on actvity and create subdatasets
DataSplit <- split(DataSelect, DataSelect[,4]) 

Laying <- DataSplit[[1]]
Laying <- Laying[order(Laying$Subject),] # Reorder on subject number

Sitting <- DataSplit[[2]]
Sitting <- Sitting[order(Sitting$Subject),]

Standing <-DataSplit[[3]]
Standing <- Standing[order(Standing$Subject),]

Walking <- DataSplit[[4]]      
Walking <- Walking[order(Walking$Subject),]

WalkingDown <- DataSplit[[5]] 
WalkingDown <- WalkingDown[order( WalkingDown$Subject),]

WalkingUp <- DataSplit[[6]]
WalkingUp <- WalkingUp[order(WalkingUp$Subject),]


## Extract means of measurements for all subject, per activity
meansLaying <- cbind(unique(Laying$Subject), rep("Laying", 30))  # Laying
for (i in 5:70) { 
      SubLaying <- tapply(Laying[,i], Laying$Subject, mean ) 
            meansLaying <- cbind(meansLaying, SubLaying)
}
colnames(meansLaying) <- c("Subject", "Activity", colnames(Laying[,5:70]))


meansSitting <- cbind(unique(Sitting$Subject), rep("Sitting", 30))  # Sitting
for (i in 5:70) { 
      SubSitting <- tapply(Sitting[,i], Sitting$Subject, mean )
            meansSitting <- cbind(meansSitting, SubSitting)
}
colnames(meansSitting) <- c("Subject", "Activity", colnames(Sitting[,5:70]))


meansStanding <- cbind(unique(Standing$Subject), rep("Standing", 30))  # Standing
for (i in 5:70) { 
      SubStanding <- tapply(Standing[,i], Standing$Subject, mean ) 
      meansStanding <- cbind(meansStanding, SubStanding)
}
colnames(meansStanding) <- c("Subject", "Activity", colnames(Standing[,5:70]))


meansWalking <- cbind(unique(Walking$Subject), rep("Walking", 30))  # Walking
for (i in 5:70) { 
      SubWalking <- tapply(Walking[,i], Walking$Subject, mean ) 
      meansWalking <- cbind(meansWalking, SubWalking)
}
colnames(meansWalking) <- c("Subject", "Activity", colnames(Walking[,5:70]))


meansWalkingDown <- cbind(unique(WalkingDown$Subject), rep("Walking Down", 30))  # Walking down
for (i in 5:70) { 
      SubWalkingDown <- tapply(WalkingDown[,i], WalkingDown$Subject, mean ) 
      meansWalkingDown <- cbind(meansWalkingDown, SubWalkingDown)
}
colnames(meansWalkingDown) <- c("Subject", "Activity", colnames(WalkingDown[,5:70]))


meansWalkingUp <- cbind(unique(WalkingUp$Subject), rep("Walking Up", 30))   # Walking up
for (i in 5:70) { 
      SubWalkingUp <- tapply(WalkingUp[,i], WalkingUp$Subject, mean )
      meansWalkingUp <- cbind(meansWalkingUp, SubWalkingUp)
}
colnames(meansWalkingUp) <- c("Subject", "Activity", colnames(WalkingUp[,5:70]))


# Create tidy data set
NewData <- rbind(meansLaying, meansSitting, meansStanding, meansWalking, meansWalkingDown, meansWalkingUp)

write.table(NewData, "TidyDataSet.txt", row.names=F)
# read.table("TidyDataSet.txt", header=T, check.names=F)  # Read in the file properly
