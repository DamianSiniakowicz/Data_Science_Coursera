# IMPORTANT
# the UCI HAR dataset must be in your working directory

# load required packages
library(data.table)
library(plyr)
library(dplyr)
library(xlsx)

# read in the data
test_data <- read.table("UCI HAR Dataset/test/X_test.txt")
train_data <- read.table("UCI HAR Dataset/train/X_train.txt")

# put it in a table
test_data <- data.table(test_data)
train_data <- data.table(train_data)

# merge the two tables
huh <- rbind(test_data,train_data)

# give all the columns names
features <- read.table(file = "UCI HAR Dataset/features.txt",header=FALSE, sep = " ")
metric_names <- features[,2]
metric_names <- as.character(metric_names)
names(huh) <- metric_names

# keep only columns for mean and standard deviation
mean_std_indices <- grep("mean|std", names(huh))
mean_std_cols <- huh[,mean_std_indices,with=FALSE]
freq <- grep("Freq",names(mean_std_cols))
mean_std_without_freq <- mean_std_cols[,-freq,with=FALSE]
new_table <- mean_std_without_freq

# get the activity numbers into a table
train_activity <- read.table(file="UCI HAR Dataset/train/y_train.txt")
test_activity <- read.table(file="UCI HAR Dataset/test/y_test.txt")
all_activity <- rbind(train_activity,test_activity)
names(all_activity) <- "activity"

# read in the activity number to activity name table
activity_labels <- read.table(file="UCI HAR Dataset/activity_labels.txt", col.names = c("activity_number", "activity_name"), stringsAsFactors = FALSE)

# turn activity into a factor with a number level for each activity and string labels
all_activity <- factor(x = all_activity$activity, levels = activity_labels$activity_number, labels = activity_labels$activity_name)

# create a table with metrics and a column indicating what activity was done 
new_table <- cbind(new_table,all_activity)

# create a factor for subject identification number
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
subjects <- rbind(train_subject,test_subject)
names(subjects) <- "subject_number"
subjects$subject_number <- factor(subjects$subject_number,levels=1:30)

# make a table with measurments, activity, and subject info
new_table <- cbind(new_table,subjects)

# we cross activity and subject to make a new factor: act-sub, and add it to our table
activity_subject_pair <- interaction(new_table$all_activity,new_table$subject_number,drop = TRUE)
new_table <- mutate(new_table, Activity_Subject_Pair = activity_subject_pair)

# we split the table by the act-sub factor
split_table <- split(new_table[,1:66,with=FALSE],new_table$Activity_Subject_Pair)

# we take the mean of each measurement for each act-sub pair 
averages_final <- sapply(split_table, function(tab) sapply(tab,mean))

# transpose the df. tables are built columnwise, so the previous line flipped your rows and cols
averages_final <- t(averages_final)

#write it to a file
write.table(x = averages_final,file = "samsung_data.txt", row.names=TRUE)

#write.xlsx(averages_final,file="averages.xlsx")
