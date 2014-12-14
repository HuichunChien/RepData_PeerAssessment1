# Codes for project assignment on reproducible research on Coursera

# Loading and preprocessing the data
# Show any code that is needed to
# 1. Load the data (i.e. read.csv())
# 2. Process/transform the data (if necessary) into a format #suitable for your analysis

data <- read.csv("activity.csv")
head(data)
# use "complete.cases" function to remove NAs from dataset
data <- data[complete.cases(data), ]

# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 1. Make a histogram of the total number of steps taken each day
# 2. Calculate and report the mean and median total number of steps taken per day

library(plyr)
# sum over steps on individual day
# If I use "tapply" function to calculate sum, i.e. stepsperday0, then the function would return "NA" for those days which have no step, the total number of rows on dates is 61. 
# To avoid issue of "NA", I use ddply function, i.e. stepsperday, the total number of non-zero rows is 53.
stepsperday0 <- tapply(data$steps, data$date, sum) 
stepsperday <- ddply(data, c("date"), summarize, dailysteps = sum(steps)) 

hist(stepsperday$dailysteps)


# calcuate mean and median of total number of steps taken per day
# found ddply function can't calculate mean function, use tapply instead.

# Attention: the total number of classes on "date" is 61 instead of 53.
# Since I consider NA as 0, the total number of items is 61 instead of 53
meanstepsperday <- sum(stepsperday$dailysteps)/nrow(stepsperday0) 
print(paste("the mean is ", medainstepsperday))

# The index is retrieved from stepsperday0 instead of stepsperday1.
# After I sorted stepsperday0, the NA items are missed. Thus median index needs to be revised.
index <-round(nrow(stepsperday0)/2)+1-(nrow(stepsperday0)-nrow(stepsperday)) 
medainstepsperday <- sort(stepsperday0)[index]
print(medainstepsperday)


# What is the average daily activity pattern
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

stepsperinterval <- data.frame(cbind(unique(data$interval), tapply(data$steps, data$interval, mean, na.rm = TRUE)))
names(stepsperinterval) <- c("interval","stepsperinterval")

plot(stepsperinterval$interval, stepsperinterval$stepsperinterval , type = "l", xlab="Interval", ylab="Averaged Steps") 

# max of number of steps
max(stepsperinterval$stepsperinterval)
# the interval with the max value of steps
stepsperinterval$interval[stepsperinterval$stepsperinterval==max(stepsperinterval$stepsperinterval)]


# Imputing missing values
# 1. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# 2. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# 3. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 4. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# 5. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

rawdata <- read.csv("activity.csv")
sum(is.na(rawdata$steps))

NAsIndex=is.na(rawdata$steps)
plot(NAsIndex, type="l") # plot NAs, and found out NAs seem happen sequentially.

# replace missing step values by averaged steps retrieved from those on daily activity pattern
NAsinterval<-rawdata$interval[NAsIndex] # total number of 2304
uniqueNAsinterval <- unique(NAsinterval) # total number of 288

# the designated replacement on missing step values; total number of 288
replacementofMissingNAs <- stepsperinterval$stepsperinterval[unique(data$interval)==uniqueNAsinterval]


# ##Are there differences in activity patterns between weekdays and weekends?
# 1. For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 2. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# 3. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
