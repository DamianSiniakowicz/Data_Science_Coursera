# assignment 2 data exploration

# load useful packages
library(plyr)
library(dplyr)
library(ggplot2)

# make a folder to store your work, download the zip, extract the zip
dir.create("pollution_folder")
setwd("pollution_folder")
download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",destfile="./pollution.zip")
unzip("pollution.zip")

# read files into a tables
NEI <- readRDS("summarySCC_PM25.rds")
Source_Code <- readRDS("Source_Classification_Code.rds")

# Plot total emissions for 1999 - 2008
NEI_by_year <- group_by(NEI, year)
total_by_year <- summarize(NEI_by_year, total_emissions = sum(Emissions))
png("Pollution_Plot_1.png", height = 480, width = 480, units = "px")
with(total_by_year, plot(total_emissions~year, type="l"))
dev.off()

# Plot total emissions in Baltimore for 1999-2008
baltimore_by_year <- filter(NEI_by_year,fips=="24510")
baltimore_total <- summarize(baltimore_by_year, total_emissions = sum(Emissions))
png("Pollution_Plot_2.png", height = 480, width = 480, units = "px")
with(baltimore_total, plot(total_emissions~year, type="l"))
dev.off()

# plot pollution by source for baltimore from 1999 to 2008
baltimore <- filter(NEI, fips=="24510")
year_source_baltimore <- group_by(baltimore,year,type)
summary_y_s_b <- summarize(year_source_baltimore, total_emissions = sum(Emissions))
png("Pollution_Plot_3.png", height = 480, width = 480, units = "px")
ggplot(summary_y_s_b,aes(x=year,y=total_emissions)) + facet_wrap(~type) + geom_line()
dev.off()

# plot coal combustion related pollution from 1999 to 2008
coal <- filter(Source_Code, grepl("Coal",EI.Sector))
coal_scc <- coal$SCC
coal_NEI <- filter(NEI, SCC %in% coal_scc)
coal_year_NEI <- group_by(coal_NEI, year)
summary_c_y_NEI <- summarize(coal_year_NEI, total_coal_emissions = sum(Emissions))
png("Pollution_Plot_4.png", height = 480, width = 480, units = "px")
with(summary_c_y_NEI, plot(total_coal_emissions~year, type="l"))
dev.off()

# plot motor vehicle emissions in Baltimore from 1999 to 2008
motor_vehicle_scc <- filter(Source_Code, grepl("On-Road", EI.Sector))
MV_scc <- motor_vehicle_scc$SCC
MV_NEI <- filter(baltimore, SCC %in% MV_scc)
MV_year_NEI <- group_by(MV_NEI,year)
total_MV_year <- summarize(MV_year_NEI, total_MV_emissions = sum(Emissions))
png("Pollution_Plot_5.png", height = 480, width = 480, units = "px")
with(total_MV_year, plot(total_MV_emissions~year, type="l"))
dev.off()

# plot motor vehicle emissions of LA vs. Baltimore from 1999 to 2008

# get LA data and Baltimore data in separate frames
Los_Angeles <- filter(NEI, fips == "06037")
Baltimore <- filter(NEI, fips == "24510")

# reduce LA and Baltimore data frames to motor vehicle only data frames
motor_vehicle_scc <- filter(Source_Code, grepl("On-Road", EI.Sector))
MV_scc <- motor_vehicle_scc$SCC
LA_MV <- filter(Los_Angeles, SCC %in% MV_scc)
Balt_MV <- filter(Baltimore, SCC %in% MV_scc)

# group both cities motor vehicle data by year, and summarize pollution by year
Balt_year_MV <- group_by(Balt_MV,year)
LA_year_MV <- group_by(LA_MV, year)
Balt_total <- summarize(Balt_year_MV, total_pollution = sum(Emissions))
LA_total <- summarize(LA_year_MV, total_pollution = sum(Emissions))

# normalize the data by initial pollution levels
init_LA <- LA_total[[1,2]]
init_Balt <- Balt_total[[1,2]]
LA_total <- mutate(LA_total, normalized = total_pollution/init_LA)
Balt_total <- mutate(Balt_total, normalized = total_pollution/init_Balt)

# just plot, it'll be ok
png("Pollution_Plot_6.png", height = 480, width = 480, units = "px")
par(mar=c(4,4,3,1))
plot(x=c(1999,2002,2005,2008),y=seq(0,1.25,len=4),type="n",ylab="normalized pollution",xlab="year")
legend("center",legend=c("Los Angeles","Baltimore"),col=c("blue","red"),lty=1)
lines(x=Balt_total$year,y=Balt_total$normalized,col="red")
lines(x=LA_total$year,y=LA_total$normalized,col="blue")
dev.off()





