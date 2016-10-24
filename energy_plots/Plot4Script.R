# IMPORTANT
# execute this script in the directory containing household_power_consumption.txt
#----------------------------------------------------------------------------------#

#Step 0: load libraries, download and extract zip file
library(data.table)
library(plyr)
library(dplyr)

download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = "hpu.zip")
unzip(zipfile = "hpu.zip")

#Step 1: read in data, put it in a data.table object
power_data <- read.table(file = "household_power_consumption.txt",sep=";",header=TRUE)
power_data <- as.data.table(power_data)

#Step 2: filter out data for the dates we don't want
new_power_data <- filter(power_data, Date == "1/2/2007" | Date == "2/2/2007")

#Step 3: Convert each column to the type we want
numeric_variables <- sapply(new_power_data[,3:9,with=FALSE],function(a_factor) as.numeric(a_factor))
dates <- as.Date(new_power_data[["Date"]])
times <- strptime(new_power_data$Time, format = "%H:%M:%S")

#Step 4: put the new data table together
newer_power_data <- data.table(dates, times, numeric_variables)

#Step 5: reset par
par(mfcol=c(2,2))

#Step 6: create global active power time-series
time_series <- ts(newer_power_data$Global_active_power)

#Step 7: draw the first plot: global active power over time
plot(time_series, axes = FALSE, ylab = "Global Active Power")
axis(side = 2, at = c(0,1000,2000,3000), labels = c(0,1,2,3))
axis(side = 1, at = c(0,1440,2880), labels = c("Thursday","Friday","Saturday"))

#Step 8: Create sub-metering time-series
sub_1 <- ts(newer_power_data$Sub_metering_1)
sub_2 <- ts(newer_power_data$Sub_metering_2)
sub_3 <- ts(newer_power_data$Sub_metering_3)

#Step 9: draw the second plot: sub-metering power over time
plot(x= newer_power_data$Sub_metering_1, axes=FALSE, type="n", xlab = "Time", ylab="Energy Sub-Metering")
lines(x = 1:2880, y= sub_1, col = "black")
lines(x = 1:2880, y= sub_2, col = "red")
lines(x = 1:2880, y= sub_3, col = "blue")
axis(side=1, at = c(0,1440,2880), labels = c("Thursday", "Friday", "Saturday"))
axis(side=2, at=seq(0,35,by=5),labels =seq(0,35,by=5) )
legend("topright", legend = c("meter 1", "meter 2", "meter 3"), lty = 1, col = c("black","red","blue"))

#Step 10: create a voltage time-series
volt_time <- ts(newer_power_data$Voltage)

#Step 11: draw the third plot: voltage over time
plot(volt_time, axes=FALSE, ylab="Voltage", xlab="Time")
axis(side=1,at=c(0,1440,2880),labels=c("Thursday","Friday","Saturday"))
axis(side=2,at=c(800,1200,1600,2000),labels=c(800,1200,1600,2000))

#Step 12: create a global-reactive power time series
react_time <- ts(newer_power_data$Global_reactive_power)

#Step 13: draw the fourth plot: global reactive power over time
plot(react_time,axes=FALSE,ylab="Global Reactive Power", xlab = "Time")
axis(side=1,at=c(0,1440,2880),labels=c("Thursday","Friday","Saturday"))
axis(side=2, at = c(50,100,150,200,250), labels = c(50,100,150,200,250))

#Step 14: save as png
dev.copy(png, "plot4.png")
dev.off()
