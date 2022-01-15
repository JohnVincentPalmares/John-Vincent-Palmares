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