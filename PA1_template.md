# PA_Assignment1
Huichun Chien  
January 16, 2015  

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. The data for this assignment can be downloaded from the course web site.

The analysis steps and answers of individual questions in this assignment are mentioned separately.


### Procedure 1: Loading and preprocessing the data
* Step 1: Load the data (i.e. read.csv())
* Step 2: Process/transform the data (if necessary) into a format suitable for your analysis: take away "NAs" in the data


```r
data <- read.csv("./activity.csv")
dataclean <- data[-which(is.na(data)),]
```


### Question 1: What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
* Step 1: Make a histogram of the total number of steps taken each day

```r
totalsteps <- aggregate(steps~date, data=dataclean, sum)
hist(totalsteps$steps, xlab="Total Steps", main="Total Steps taken per day")
```

![](./Peer_Assignment1_files/figure-html/unnamed-chunk-2-1.png) 

* Step 2: Calculate and report the mean and median total number of steps taken per day

```r
mean(totalsteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalsteps$steps)
```

```
## [1] 10765
```


### Question 2: What is the average daily activity pattern?
* Step 1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averagestepsperday <- aggregate(steps~interval, data=dataclean, mean)
plot(averagestepsperday$interval, averagestepsperday$steps, type="l")
```

![](./Peer_Assignment1_files/figure-html/unnamed-chunk-4-1.png) 

* Step 2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averagestepsperday$interval[which(averagestepsperday$steps==max(averagestepsperday$steps))]
```

```
## [1] 835
```


### Procedure 2: Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
* Step 1: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data[,1]))
```

```
## [1] 2304
```

* Step 2: Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I plot missing data in order to understand how missing data distribute. I found missing data occur in consequent intervals.

```r
plot(which(is.na(data)), xlab="Interval per day", ylab="Observation Index")  
```

![](./Peer_Assignment1_files/figure-html/unnamed-chunk-7-1.png) 

Thus, the strategy I use is following: filling in the missing values with the median value of the total steps / (number of intervals per day) in individual 5-minute interval.

* Step 3: Create a new dataset that is equal to the original dataset but with the missing data filled in: te new dataset created is called datanew.

```r
datanew <- data
datanew$steps[which(is.na(data))] <- median(totalsteps$steps)/(max(data$interval)/5)
totalstepsnew <- aggregate(steps~date, data=datanew, sum)
```

* Step 4: Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(totalstepsnew$steps)
```

![](./Peer_Assignment1_files/figure-html/unnamed-chunk-9-1.png) 

The main difference between the first histgram and the second histgram is the increased frequecny between 5000 and 10000 of total steps. The reason for this difference is that I impute the missing value with the averaged median steps per day; the total steps per day impued equals to median(totalsteps$steps)*5, which equals to 53825.

```r
median(totalsteps$steps)*5
```

```
## [1] 53825
```

Also, the mean and median total number of steps taken per day become values following.

```r
mean(totalstepsnew$steps)
```

```
## [1] 10217.5
```

```r
median(totalstepsnew$steps)
```

```
## [1] 10395
```


### Question 3: Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
* Step 1: Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.: the variable is called averagestepsweekdaysubset.

```r
data$day<-as.factor(ifelse(weekdays(as.Date(data$date))==c("Saturday","Sunday"), "weekend", "weekday"))
averagestepsweekdaysubset <- aggregate(steps~interval+day, data=data, mean)
```


* Step 2: Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(lattice)
xyplot(steps~interval|day, data=averagestepsweekdaysubset, type="l", layout = c(1,2),  xlab="Interval", ylab="Averaged numer of steps")
```

![](./Peer_Assignment1_files/figure-html/unnamed-chunk-13-1.png) 



