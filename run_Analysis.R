####====================================================================================
## Coursera Getting and Cleaning Data - Course Project
## 25 Oct 2014
# run_Analysis.R File
###====================================================================================

# step 1. Merge the training and the test sets to create one data set.
# Read in the data from files
features     = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/features.txt',header=FALSE); 
activityType = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/activity_labels.txt',header=FALSE); 

# Read in the train data
subjectTrain = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/subject_train.txt',header=FALSE); 
xTrain       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/x_train.txt',header=FALSE); 
yTrain       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/y_train.txt',header=FALSE); 

# Assigin column names to the data
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Combine the yTrain, subjectTrain and xTrain into the train dataset
trainData = cbind(yTrain,subjectTrain,xTrain);

# Read in the test data
subjectTest = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/subject_test.txt',header=FALSE); 
xTest       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/x_test.txt',header=FALSE); 
yTest       = read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/y_test.txt',header=FALSE); 

# Assign column names to the test dataset
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# Combine the xTest, yTest and subjectTest data into the test dataset
testData = cbind(yTest,subjectTest,xTest);

# Combine train and test data to a new data set
allData = rbind(trainData,testData);


# Step 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# Get the column names from the allData
headers  = colnames(allData);
# Create a vector containing TRUE values for the ID, mean() & stddev() columns and FALSE for others
vec = ((grepl("activity..",headers) | grepl("subject..",headers) | grepl("-mean..",headers) | grepl("-std..",headers)) & !grepl("-meanFreq..",headers) & !grepl("mean..-",headers) & !grepl("-std()..-",headers));

# Subset allData based on the vector to keep only required columnsheaders
allData = allData[vec==TRUE];

# Step 3. Use descriptive activity names to name the activities in the data set
# Merge the allData set with the acitivityType to include descriptive activity names
allData = merge(allData,activityType,by='activityId',all.x=TRUE);

# Updating the headers to include the new column names after merge
headers  = colnames(allData); 

# Step 4. Appropriately label the data set with descriptive activity names. 
for (i in 1:length(headers)) 
{
    headers[i] = gsub("-std$","StdDev",headers[i])
    headers[i] = gsub("-mean","Mean",headers[i])
    headers[i] = gsub("\\()","",headers[i])
    headers[i] = gsub("AccMag","AccMagnitude",headers[i])
    headers[i] = gsub("GyroMag","GyroMagnitude",headers[i])
    headers[i] = gsub("JerkMag","JerkMagnitude",headers[i])

};

# Reassigning the new descriptive column names to the allData set
colnames(allData) = headers;

# Step 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Create a new table allData2 without the activityType column
allData2  = allData[,names(allData) != 'activityType'];

# Calculate the mean of each variable for each activity and each subject
tidyData = aggregate(allData2[,names(allData2) != c('activityId','subjectId')],by=list(activityId=allData2$activityId,subjectId = allData2$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './Coursera/Module3/Course Project/UCI HAR Dataset/tidyData.txt',row.names=FALSE,sep='\t');
