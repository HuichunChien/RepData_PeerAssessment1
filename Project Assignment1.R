# Codes for project assignment on reproducible research on Coursera

# Loading and preprocessing the data
# Show any code that is needed to
# 1. Load the data (i.e. read.csv())
# 2. Process/transform the data (if necessary) into a format #suitable for your analysis

path <- getwd()
data <- read.csv("./activity.csv")
dataclean <- data[-which(is.na(data)),]

# # What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 1. Make a histogram of the total number of steps taken each day
# 2. Calculate and report the mean and median total number of steps taken per day

totalsteps <- aggregate(steps~date, data=dataclean, sum)
hist(totalsteps$steps)

mean(totalsteps$steps)
# [1] 10766.19
median(totalsteps$steps)
# [1] 10765

# # What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

averagestepsperday <- aggregate(steps~interval, data=dataclean, mean)
plot(averagestepsperday$interval, averagestepsperday$steps, type="l")

averagestepsperday$interval[which(averagestepsperday$steps==max(averagestepsperday$steps))]
# [1] 835

# # Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

sum(is.na(data[,1]))
# [1] 2304

plot(which(is.na(data)))  
# na series are continuous


## fill in the missing values with the median value of the total steps / (number of intervals per day)
datanew <- data
datanew$steps[which(is.na(data))] <- median(totalsteps$steps)/(max(data$interval)/5)
totalstepsnew <- aggregate(steps~date, data=datanew, sum)

hist(totalstepsnew$steps)

> median(totalsteps$steps)*5
[1] 53825

mean(totalstepsnew$steps)
# [1] 10217.5
median(totalstepsnew$steps)
# [1] 10395


# # Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

data$day<-as.factor(ifelse(weekdays(as.Date(data$date))==c("Saturday","Sunday"), "weekend", "weekday"))

averagestepsweekdaysubset <- aggregate(steps~interval+day, data=data, mean)

install.packages("lattice")
library(lattice)
xyplot(steps~interval|day, data=averagestepsweekdaysubset, type="l", layout = c(1,2),  xlab="Interval", ylab="Averaged numer of steps")

