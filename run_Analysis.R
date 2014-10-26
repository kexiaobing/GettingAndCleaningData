####====================================================================================
## 25 Oct 2014
## run_Analysis.R File
###====================================================================================

### Step 1: Merge the training and the test sets to create one data set.
# Read in the train data
subject_train <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/subject_train.txt',header=FALSE); 
x_train <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/x_train.txt',header=FALSE); 
y_train <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/train/y_train.txt',header=FALSE); 

# Read in the feature and activity data
features <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/features.txt',header=FALSE); 
activity_label <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/activity_labels.txt',header=FALSE); 

# Assign column names to the dataset
colnames(subject_train) <- "subjectId";
colnames(x_train) <- features[,2]; 
colnames(y_train) <- "activityId";
colnames(activity_label) <- c('activityId','activity_label');

# Combine the y_train, subject_train and x_train into the train dataset
trainData <- cbind(y_train,subject_train,x_train);

# Read in the test data
subject_test <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/subject_test.txt',header=FALSE); 
x_test <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/x_test.txt',header=FALSE); 
y_test <- read.table('./Coursera/Module3/Course Project/UCI HAR Dataset/test/y_test.txt',header=FALSE); 

# Assign column names to the test dataset
colnames(subject_test) <- "subjectId";
colnames(x_test) <- features[,2]; 
colnames(y_test) <- "activityId";

# Combine the x_test, y_test and subject_test data into the test dataset
testData <- cbind(y_test,subject_test,x_test);

# Combine train and test data to create a new data set
allData <- rbind(trainData,testData);



### Step 2: Extract only the measurements on the mean and standard deviation for each measurement. 
# Get the column names from the allData
headers <- colnames(allData);
# Generate a vector containing TRUE values for the activityID, subjectID, mean & std columns
vec <- ( (grepl("-mean..",headers) | grepl("-std..",headers) | grepl("activity..",headers) | grepl("subject..",headers))  & !grepl("-meanFreq..",headers) & !grepl("mean..-",headers)  & !grepl("-std()..-",headers));

# Subset allData based on the vector to keep only required columns
allData <- allData[vec==TRUE];



### Step 3: Use descriptive activity names to name the activities in the data set
# Merge the allData with descriptive activity names in the acitivity labels
allData <- merge(allData,activity_label,by='activityId',all.x=TRUE);

# Updating the headers vector with the new column names
headers <- colnames(allData); 



### Step 4: Appropriately label the data set with descriptive activity names. 
for (i in 1:length(headers)) 
{
    headers[i] <- gsub("-std$","StdDev",headers[i])
    headers[i] <- gsub("-mean","Mean",headers[i])
    headers[i] <- gsub("AccMag","AccMagnitude",headers[i])
    headers[i] <- gsub("JerkMag","JerkMagnitude",headers[i])
    headers[i] <- gsub("GyroMag","GyroMagnitude",headers[i])
    headers[i] <- gsub("\\()","",headers[i])
};

# Update the allData with the new descriptive column names 
colnames(allData) <- headers;



### Step 5: Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Remove the activity_label column from the dataset
allData2 <- allData[,names(allData) != 'activity_label'];

# Calculate the mean of each variable for each activity and each subject
tidyData <- aggregate(allData2[,names(allData2) != c('activityId','subjectId')],by=list(activityId=allData2$activityId,subjectId = allData2$subjectId),mean);

# Make descriptive acitvity names
tidyData <- merge(tidyData,activity_label,by='activityId',all.x=TRUE);

# Generate the result dataset tidyData 
write.table(tidyData, './Coursera/Module3/Course Project/UCI HAR Dataset/tidyData.txt',row.names=FALSE,sep='\t');
