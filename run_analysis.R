getwd()
#Install necessarry packages
install.packages("data.table")
install.packages("reshape2")
library(data.table)
library(reshape2)
require("data.table")
require("reshape2")
#Unzip the file and create a working directory 
unzip("UCI HAR Dataset.zip", exdir="specdata")
#Load: activity labels 
#Use read.table to load the file
act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
#Load the data column names
features <- read.table("./UCI HAR Dataset/features.txt")[,2]
#Check the length of the data
length(act_labels)
length(features)
#Extract only the measurements on the mean and standard deviation for each measuremnet.
extracted_from_features<- grep("mean|sd", features)
#Load and process X_test and Y_test and store inside two different variables
X_test<- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<- read.table("./UCI HAR Dataset/test/subject_test.txt")
#Create a name for one variable
names(X_test) = features
#Extract only the measurements on the mean and standard deviation for each measurement.
X_test = X_test[,extracted_from_features]
#Load activty labels
y_test[,2] = act_labels[y_test[,1]]
names(y_test) = c("Act_ID", "Act_Label")
names(subject_test)="Subject"
#Bind the data
test_data <- cbind(as.data.table(subject_test), y_test, X_test)
# Load and process X_training and y_training data.
X_training <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_training <- read.table("./UCI HAR Dataset/train/y_train.txt")

subject_training <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(X_training) = features
#Extract only the measurements on the mean and standard deviation for each measurement.
X_training = X_training[,extracted_from_features]

#Load the activity data
y_training[,2] = act_labels[y_training[,1]]
names(y_training) = c("Act_ID", "Act_Label")
names(subject_training) = "Subject"

#Bind the data
training_data <- cbind(as.data.table(subject_training), y_training, X_training)

#Merge test and train data
dat= rbind(test_data, training_data, fill=TRUE)
#Specify the data and label
id_labels = c("Subject", "Act_ID", "Act_Label")
data_labels = setdiff(colnames(dat), id_labels)
melt_data = melt(dat, id = id_labels, measure.vars = data_labels,variable.name = "variable", na.rm = FALSE)

#Apply mean function to dataset using dcast function
tidy_data = dcast(melt_data, Subject + Act_Label ~ variable, mean, drop = FALSE)
write.table(tidy_data, file= "./tiy_data.txt")
