####====================================================================================
## Coursera Getting and Cleaning Data - Course Project
## 25 Oct 2014

# run_Analysis.R File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

###====================================================================================

# 1. Merge the training and the test sets to create one data set.
# Read in the data from files
features     = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/features.txt',header=FALSE); #imports features.txt
activityType = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/activity_labels.txt',header=FALSE); #imports activity_labels.txt

# Read in the train data
subjectTrain = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/y_train.txt',header=FALSE); #imports y_train.txt

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Combine the yTrain, subjectTrain and xTrain into the train dataset
trainData = cbind(yTrain,subjectTrain,xTrain);

# Read in the test data
subjectTest = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# Combine the xTest, yTest and subjectTest data into the test dataset
testData = cbind(yTest,subjectTest,xTest);

# Combine train and test data to create a final data set
allData = rbind(trainData,testData);


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# Get the column names from the allData
colNames  = colnames(allData);
# Create a vector containing TRUE values for the ID, mean() & stddev() columns and FALSE for others
vec = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset allData based on the vector to keep only required columns
allData = allData[vec==TRUE];

# 3. Use descriptive activity names to name the activities in the data set
# Merge the allData set with the acitivityType to include descriptive activity names
allData = merge(allData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(allData); 

# 4. Appropriately label the data set with descriptive activity names. 
for (i in 1:length(colNames)) 
{
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the allData set
colnames(allData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Create a new table, allData2 without the activityType column
allData2  = allData[,names(allData) != 'activityType'];

# Summarizing the allData2 table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(allData2[,names(allData2) != c('activityId','subjectId')],by=list(activityId=allData2$activityId,subjectId = allData2$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './Coursera/Module3/Course Project/UCI HAR Dataset/tidyData.txt',row.names=FALSE,sep='\t');
