
# 1. Merges the training and the test sets to create one data set
	setwd("C:/Users/abelardo.arango/Desktop/R/Clean_Fitness_Data/UCI HAR Dataset")
## Read in data files
	testSubf <- read.table("./test/subject_test.txt", header=FALSE)
	trainSubf <- read.table("./train/subject_train.txt", header=FALSE)
	testXf <- read.table("./test/X_test.txt", header=FALSE)
	testYf <- read.table("./test/y_test.txt", header=FALSE) 
	activityType <- read.table("./activity_labels.txt", header=FALSE)
	Featf <- read.table("./features.txt", header=FALSE)
	trainSubf <- read.table("./train/subject_train.txt", header=FALSE)
	trainXf <- read.table("./train/X_train.txt", header=FALSE)
	trainYf <- read.table("./train/y_train.txt", header=FALSE)

## Column names provided to 'train' set
	colnames(trainSubf) = "subjectId";
	colnames(activityType) = c("activityId", "activityType");
	colnames(trainXf) = Featf[,2];
	colnames(trainYf) = "activityId";

## Merge Train data set
	trainData <- cbind(trainYf,trainSubf,trainXf);

## Column names provided to 'test' set
	colnames(testSubf) = "subjectId";
	colnames(testXf) = Featf[,2];
	colnames(testYf) = "activityId";

## Merge test data set
	testData <- cbind(testYf,testSubf,testXf);

## Bind all data
	endData <- rbind(trainData, testData);

## Vector for the column names form endData
	ColNames = colnames(endData);

# 2 Extracts only the measurements on the mean and standard deviation for each measurement
	logicalVector = (grepl("activity..",colNames) |
		grepl("subject..",colNames) | grepl("-mean..",colNames) &
		!grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) |
		grepl("-std..",colNames) & !grepl("-std()..-",colNames));
	
## Subset endData from logical vectors	
	endData = endData[logicalVector==TRUE];	
##3 Use desc. activity names	to name the activities in the data set

## Merge the endData with the activityType table to include names
endData = merge(endData,activityType,by='activityId', all.x=TRUE);
## Use desc. activity names	to name the activities in the data set
# Update colNames
colNames = colnames(endData);

# 4. Label the data set with the activity names 

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
  };
  
  ## Reassigning the new descriptive column names to the endDataData set
colnames(endData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Build new table endDataNoType without the activityType column
endDataNoType  = endData[,names(endData) != 'activityType'];

# #Summarizing the endDataNoType table to include just the mean 
tidyData    = aggregate(endDataNoType[,names(endDataNoType) != c('activityType','subjectId')],by=list(activityId=endDataNoType$activityId,subjectId = endDataNoType$subjectId),mean);

# Merging tidyData with activityType to include descriptive names
tidyData    = merge(tidyData,activityType,by='activityType',all.x=TRUE);

# Write table for submission of Course 'TidyData.txt' report
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');