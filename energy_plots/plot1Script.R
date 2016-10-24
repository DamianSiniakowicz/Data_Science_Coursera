
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

#Step 5: set canvas dimensions
par(mfrow=c(1,1),mar=c(4,4,2,1))

#Step 6: plot the histogram
hist(newer_power_data$Global_active_power, col = "red", main = "Global Active Power", axes = FALSE, xlab = "Global Active Power (megawatts)")
axis(side = 1, at = c(0,1000,2000,3000,4000), labels=c(0,1,2,3,4))
axis(side = 2, at = c(0,200,400,600,800,1000,1200), labels = c(0,200,400,600,800,1000,1200))

#Step 7: save as png
dev.copy(png, 'plot1.png', width = 480, height = 480)
dev.off()


