# other solution 1

library(ggplot2)
library(plyr)

##Reference on how to download the data source files
##download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",destfile = "hw4o4.zip",method="curl")
##zipfile4o4="hw4o4.zip"
##unzip(zipfile4o4,exdir=getwd())

## Read the Data Source files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Subset the data from the year of 1999 / 2002 / 2005 / 2008 
subsetdata1<-subset(NEI,NEI$year==1999 | NEI$year==2002 | NEI$year==2005 | NEI$year==2008)

##Sum up all the PM25 emission for those years
totEmission<-tapply(subsetdata1$Emissions,subsetdata1$year,sum)

#Create the png file
png(filename = "plot1.png",width = 480,height=480)

#Create the plot on the png file
plot(totEmission,type="l",col="red",xlab = "Year",ylab="Emissions",main="Total Annual Emissions")

#Shut down the png device
dev.off()

############

## Subset the data for fips == "24510"
subset24510<-subset(NEI,NEI$fips=="24510")

##Sum up all the PM25 emission for those years
totEmission24510<-tapply(subset24510$Emissions,subset24510$year,sum)

#Create the png file
png(filename = "plot2.png",width = 480,height=480)

#Create the plot on the png file
plot(totEmission24510,type="l",col="blue",xlab = "Year",ylab="Emissions",main="Annually Emissions in Baltimore City")

#Shut down the png device
dev.off()

###################

## Subset the data for fips == "24510"
subset24510<-subset(NEI,fips=="24510")

##Sum up all the emission by type for those years
totEmissionbyType2<-ddply(subset24510, c("year","type"), function(data) sum(data$Emissions))

##Name the third column (Sum) which is the sum of Emission
colnames(totEmissionbyType2)[3]<-"Emissions"

##Create the png file
png(filename = "plot3.png",width = 480,height=480)

#Create the plot on the png file
qplot(year,Emissions,data=totEmissionbyType2,geom = c("point","line"),color=type,xlab="Year",ylab = "Emissions",main = "Annual Emissions by Type and Year")

#Shut down the png device
dev.off()

#####################

subsetCoal<-SCC[grepl("Fuel.+Coal",SCC$EI.Sector),]

## Align the SCC Coal Sector with NEI data
subsetNEIforCoal <- NEI[(NEI$SCC %in% subsetCoal$SCC),]

##Sum up all the emission by type for those years
NEIforCoalEmissions<-ddply(subsetNEIforCoal, c("year"), function(data) sum(data$Emissions))

##Name the third column (Sum) which is the sum of Emission
colnames(NEIforCoalEmissions)[2]<-"Emissions"

##Create the png file
png(filename = "plot4.png",width = 480,height=480)

##Create the plot on the png file
barplot(NEIforCoalEmissions$Emissions,
        xlab="Year",
        ylab = "Emissions",
        main = expression(Total~PM[2.5]~From~Coal~Emissions~Combustion~Sources))
axis(1, at=c(1:4),labels=c("1999", "2002", "2005", "2008"))

##Shut down the png device
dev.off()

###################

## Subset the data for fips == "24510" and type == "ON-ROAD"
subsetOnRoad<-NEI[(NEI$type=="ON-ROAD" & NEI$fips=="24510"),]

##Sum up all the emission by type for those years
NEIforMotorEmissions<-ddply(subsetOnRoad, c("year"), function(data) sum(data$Emissions))

##Name the third column (Sum) which is the sum of Emission
colnames(NEIforMotorEmissions)[2]<-"Emissions"

##Create the png file
png(filename = "plot5.png",width = 480,height=480)

##Create the ggplot on the png file
ggplot(NEIforMotorEmissions, aes(x=factor(year), y=Emissions)) +
    geom_bar(stat="identity") +
    xlab("year") +
    ylab(expression("Emissions")) +
    ggtitle("Motor Vehicle Emissions in Baltimore City")

##Shut down the device
dev.off()

##########################

## Subset the data Motor source of Baltimore City for fips == "24510" and type == "ON-ROAD"
Bmore_subsetOnRoad<-NEI[(NEI$type=="ON-ROAD" & NEI$fips=="24510"),]

## Subset the data Motor source of Los Angeles County for fips == "06037" and type == "ON-ROAD"
LA_subsetOnRoad<-NEI[(NEI$type=="ON-ROAD" & NEI$fips=="06037"),]


##Sum up all the emission by type for those years
Bmore_MotorEmissions<-ddply(Bmore_subsetOnRoad, c("year"), function(data) sum(data$Emissions))
LA_MotorEmissions<-ddply(LA_subsetOnRoad, c("year"), function(data) sum(data$Emissions))


##Name the third column (Sum) which is the sum of Emission
colnames(Bmore_MotorEmissions)[2]<-"Emissions"
colnames(LA_MotorEmissions)[2]<-"Emissions"

##Assign one column to label the City names
Bmore_MotorEmissions$city<-"Baltimore"
LA_MotorEmissions$city<-"Los Angeles"


##Combine the Motor source data into one data frame
Combine_MotorEmissions<-rbind(Bmore_MotorEmissions,LA_MotorEmissions)


##Create the png file
png(filename = "plot6.png",width = 480,height=480)


##Create the ggplot on the png file
ggplot(Combine_MotorEmissions, aes(fill=city,x=factor(year), y=Emissions)) +
    geom_bar(stat="identity") +
    facet_grid(city~.,scales = "free")+  ##Split the plot in terms of City with free of limit grids
    xlab("year") +
    ylab(expression("Emissions")) +
    ggtitle("Motor Vehicle Emissions in Baltimore & Los Angeles")

##Shut down the device
dev.off()