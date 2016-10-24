# Read Me !!!

## high level overview of script

1. merged the test and train data sets into a data frame containing all measurements
2. named the columns
3. removed all columns except for those containing mean and standard deviation data
4. created factors for activity names and subject numbers
5. crossed the two factors
6. added the new factor as a column in my measurement data frame
7. split the measurement data frame along the factor
8. took the mean of each measurement for each activity-subject factor level
9. returned the means in a matrix

see the script for details