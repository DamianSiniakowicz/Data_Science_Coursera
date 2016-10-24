# script


### step 0 : get the raw data into a data frame and load packages

library(ggplot2)
library(plyr)
library(dplyr)

download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "raw_steps_data.zip")

unzip("raw_steps_data.zip",exdir = "steps_assignment")

raw_steps_data <- read.csv("steps_assignment/activity.csv")

### step 1 : steps per day

# group by date and sum over steps

steps_per_day <- group_by(raw_steps_data,date) %>% summarise(Steps = sum(steps,na.rm=TRUE))

# plot a histogram
ggplot(data = steps_per_day, aes(x=Steps)) + 
    scale_y_continuous(name="Count",breaks=seq(0,15,by=1),labels=seq(0,15,by=1)) + 
    geom_histogram(binwidth = 2000) + 
    scale_x_continuous(breaks=seq(0,20000,by=5000)) + 
    ggtitle("frequency of step count")

# get the mean and median of the number of steps per day

SpD_median <- median(steps_per_day$Steps,na.rm = TRUE)
SpD_mean <- mean(steps_per_day$Steps,na.rm=TRUE)

### step 2 : plot steps against intervals

# group by intervals and take the mean steps
interval_steps <- group_by(raw_steps_data,interval) %>% 
    summarize(mean_steps = mean(steps,na.rm =TRUE))

#plot
qplot(data = interval_steps, x= interval, y = mean_steps, geom="line",main="activity over a day")

# find most active interval
most_active_steps <- max(interval_steps$mean_steps)
most_active_interval <- interval_steps[[match(most_active_steps,interval_steps$mean_steps),1]]

### step 3 : redo stuff with imputed values

# calculate the number of missing values
missing_values <- sum(is.na(raw_steps_data$steps))

# replace each missing value with the average for its interval
x = 0
imputed_steps_data <- raw_steps_data
for(entry in 1:nrow(imputed_steps_data)){
    
    if(is.na(imputed_steps_data[["steps"]][entry])){
            x <- x + 1
            the_interval <- imputed_steps_data[["interval"]][entry]
            sub_value <- interval_steps[interval_steps$interval==the_interval,][["mean_steps"]]
            imputed_steps_data[["steps"]][entry] <- sub_value            
            
    }
}
# recalculate steps per day with imputed values
new_steps_per_day <- group_by(imputed_steps_data,date) %>% summarize(daily_steps = sum(steps))

#replot the histogram
qplot(x=new_steps_per_day$daily_steps,binwidth=2000,ylab="frequency", xlab = "steps per day",main="absolute frequency of steps per day, bin size = 2000")

#recalculate mean and median
new_median <- median(new_steps_per_day$daily_steps)
new_mean <- mean(new_steps_per_day$daily_steps)

### step 4 : compare weekday and weekend activity

# convert date variable to class Date
imputed_steps_data <- mutate(imputed_steps_data, DOW = weekdays(as.Date(date)))
imputed_steps_data <- mutate(imputed_steps_data, week_end_day = ifelse(DOW=="Saturday" | DOW == "Sunday", "Weekend","Weekday"))
imputed_steps_data <- mutate(imputed_steps_data, week_end_day = factor(week_end_day))

# group and plot
group_day_int <- group_by(imputed_steps_data,week_end_day,interval) %>%
                                        summarize(the_mean=mean(steps))

qplot(x=interval,y=the_mean,facets = .~week_end_day,geom="line",data=group_day_int)