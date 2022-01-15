### Number 1
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


### Number 2
# Extract the file and create a working directory
AB <- unzip("household_power_consumption.txt", exdir="specdata")
#Set your working directory
setwd("~/Final proj/specdata")
#Read the the file and create a table with teh following column names
b <- read.table(text = grep("^[1,2]/2/2007", readLines(AB), value = TRUE), col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), sep = ";", header = TRUE)
#Creating Plot 1
hist(b$Global_active_power, col = "red", main = paste("Global Active Power"), xlab = "Global Active Power (kilowatts)")
#Creating a png for plot 1
dev.copy(png,"plot1.png")
dev.off()
#Creating plot 2
#Read the file as csv
a <- read.csv("household_power_consumption.txt", header = T, sep = ';', 
     na.strings = "?", nrows = 2075259, check.names = F, 
     stringsAsFactors = F, comment.char = "", quote = '\"')
#Locate the date
a$Date <- as.Date(a$Date, format = "%d/%m/%Y")
## Subsetting the data
data <- subset(a, subset = (Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(a)

## Converting dates and time
date_time <- paste(as.Date(data$Date), data$Time)
data$Datetime <- as.POSIXct(date_time)

## Generating Plot 2
plot(data$Global_active_power ~ data$Datetime, type = "l",
     ylab = "Global Active Power (kilowatts)", xlab = "")
#creating a png for plot 2
dev.copy(png,"plot2.png")

#Creaing plot 3
#Use "with" to plot data, including the lines and legends
with(data, {
  plot(Sub_metering_1 ~ Datetime, type = "l", 
       ylab = "Global Active Power (kilowatts)", xlab = "")
  lines(Sub_metering_2 ~ Datetime, col = 'Red')
  lines(Sub_metering_3 ~ Datetime, col = 'Blue')
})
legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#Creating a png for plot 3
dev.copy(png,"plot3.png")
dev.off()

#Creating plot 4
#Create four graphs within a graph by specifying their dimensions
par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
#Like in plot 3, use "with" to access data in all the parameters passed
with(data, {
  plot(Global_active_power ~ Datetime, type = "l",#Plot for Global power like in plot 2 
       ylab = "Global Active Power", xlab = "")
  plot(Voltage ~ Datetime, type = "l", ylab = "Voltage", xlab = "datetime")#Plot for voltage
  plot(Sub_metering_1 ~ Datetime, type = "l", ylab = "Energy sub metering",#plot for Energey sub metering like in plot 3
       xlab = "")
  lines(Sub_metering_2 ~ Datetime, col = 'Red')
  lines(Sub_metering_3 ~ Datetime, col = 'Blue')
  legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, 
         bty = "n",
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power ~ Datetime, type = "l", #Plot for Global reactive power
       ylab = "Global_rective_power", xlab = "datetime")
})
#Creating a png for plot 4
dev.copy(png,"plot4.png")
dev.off()

