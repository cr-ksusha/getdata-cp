# this function downloads archive and extract the file with power consumption data
setwd('/Users/almaleksia/Documents/coursera/getdata/cp');
loadAndUnpackData <- function(archiveUrl) { 
  temp <- tempfile();
  download.file(archiveUrl, temp, method = "curl");
  unzip(temp);
  unlink(temp)
}

# this function constructs dataset from test or train data (depends on the argument)
makeDataSet <- function(type) { 
  # features file
  data <- read.table(
    paste(path,'/', type, '/X_', type, '.txt', sep ='') );
  # activities file
  activities <- read.table(
    paste(path,'/', type, '/y_', type ,'.txt', sep ='') );
  # subjects file
  subjects <- read.table(
    paste(path,'/', type , '/subject_', type ,'.txt', sep ='') );
  
  # append subjects and activities vectors to the end of the data frame
  data = cbind(data, subjects);
  data = cbind(data, activities);

  return(data)
}


path = './UCI HAR Dataset'

if (!file.exists(path)) {
  loadAndUnpackData("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip");
}


featureData <- read.table(
  paste(path,'/features.txt', sep ='') )

# make train and test data set using makeDataSet function defined above
trainData <- makeDataSet('train')
testData <- makeDataSet('test')

# merge train and test datasets together
mergedData <- rbind(trainData, testData);

# determine numbers of columns containing activity and subject ids
nc <- ncol(mergedData)
lastCols <-c(nc-1, nc);

# filter features by their names (we only need features which names contain -std() or -mean())
selectedFeatures <- grep('-(mean|std)\\(', features[,2]);

# make a vector of needed column numbers
selectedCols <- append(selectedFeatures, lastCols);

# subset data frame by needed columns
mergedData <- mergedData[, selectedCols]

# make a vector of needed feature names
selectedFeatureNames <- features[selectedFeatures, 2]

# make a vector of column names for data frame
resultNames <- union(selectedFeatureNames, c('subject', 'activityId'))
colnames(mergedData) <- resultNames

activityLabels <- read.table(
  paste(path,'/activity_labels.txt', sep ='') )

colnames(activityLabels) <- c("id", "activityLabel");


library(plyr)
# split dataframe into list of dataframes by subject and activityId columns
# and calculate the average value of each column in these dataframes
tidyData <- dlply(mergedData, .(subject, activityId), colMeans);

# turn the list to a dataframe
tidyData <- ldply(tidyData)

# add to data frame a column with human readable activity labels
tidyData <- merge(tidyData, activityLabels, by.x = "activityId", by.y = "id")

nc <- ncol(tidyData)

# rearrange columns in dataframe: 
# remove activityId column and move subject and activityLabel columns to the beginning
tidyData <- tidyData[c(nc-1, nc, 4:nc-2)]

# sort data by subject
attach(tidyData)
tidyData <- tidyData[order(subject),];

write.table(tidyData, 'tidy_data.txt', row.names = FALSE)
detach(tidyData)


