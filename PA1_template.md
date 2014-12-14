---
title: "Assignment 1"
author: "Huichun Chien"
date: "December 14, 2014"
output: html_document
---

### Loading and preprocessing the data:
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

* load .csv form of data, and check data structure.


```r
data <- read.csv("activity.csv")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

* use "complete.cases" function to remove NAs from dataset

```r
data <- data[complete.cases(data), ]
```


### What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day

* process data for histgram plot

```r
# sum over steps on individual day
stepsperday0 <- tapply(data$steps, data$date, sum) 
str(stepsperday0)
```

```
##  int [1:61(1d)] NA 126 11352 12116 13294 15420 11015 NA 12811 9900 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
```

```r
# If I use "tapply" function to calculate sum, i.e. stepsperday0, then the function would return "NA" for those days which have no step, the total number of  rows on dates is 61. 
# To avoid issue of "NA", I use ddply function, i.e. stepsperday, the total number of non-zero rows is 53.
library(plyr)
stepsperday <- ddply(data, c("date"), summarize, dailysteps = sum(steps)) 
str(stepsperday)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date      : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ dailysteps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

* Plot histgram

```r
hist(stepsperday$dailysteps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

* Calculate the mean of total number of steps taken per day

```r
# Attention: the total number of classes on "date" is 61 instead of 53.
# Since I consider NA as 0, the total number of items is 61 instead of 53
# The mean is calculated following
meanstepsperday <- sum(stepsperday$dailysteps)/nrow(stepsperday0)
meanstepsperday
```

```
## [1] 9354.23
```

* Calculate the median of total number of steps taken per day

```r
# The index is retrieved from stepsperday0 instead of stepsperday1.
# After I sorted stepsperday0, the NA items are missed. Thus median index needs to be revised.
index <-round(nrow(stepsperday0)/2)+1-(nrow(stepsperday0)-nrow(stepsperday)) 
medainstepsperday <- sort(stepsperday0)[index]
medainstepsperday
```

```
## 2012-10-20 
##      10395
```


### What is the average daily activity pattern
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

* average steps over all days, and use unique functon to get number of intervals match to that of averaged steps

```r
stepsperinterval <- data.frame(cbind(unique(data$interval), tapply(data$steps, data$interval, mean, na.rm = TRUE)))
names(stepsperinterval) <- c("interval","stepsperinterval")
plot(stepsperinterval$interval, stepsperinterval$stepsperinterval , type = "l", xlab="Interval", ylab="Averaged Steps") 
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

* The max value of averaged steps 

```r
# max of number of steps
max(stepsperinterval$stepsperinterval)
```

```
## [1] 206.1698
```

* The interval with the max averaged steps value

```r
# the interval with the max value of steps
stepsperinterval$interval[stepsperinterval$stepsperinterval==max(stepsperinterval$stepsperinterval)]
```

```
## [1] 835
```


### Imputing missing values
1. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
2. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
3. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
4. Create a new dataset that is equal to the original dataset but with the missing data filled in.
5. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

* The total number of missing value is following.

```r
rawdata <- read.csv("activity.csv")
sum(is.na(rawdata$steps))
```

```
## [1] 2304
```

* Create a new dataset by filling the missing data with averaged value

```r
data.filled <- rawdata
for (i in seq(nrow(rawdata))) {
    if (is.na(rawdata$steps[i])) {
        data.filled$steps[i] <- mean(rawdata$steps[rawdata$interval == rawdata$interval[i]], na.rm = TRUE)
    }
}
```

* Histogram of the total number of steps taken each day on new dataset, data.filled

```r
library(ggplot2)
q <- qplot(date, weight = steps, data = data.filled, geom = "histogram", 
           main = "Histogram of The Total Number of Steps Taken Each Day",
           ylab = "Total Number of Steps", xlab = "Date") + 
    geom_histogram(aes(fill = ..count..))

print(q)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 



```r
mean(tapply(data.filled$steps, data.filled$date, sum, na.rm = TRUE))
```

```
## [1] 10766.19
```


```r
median(tapply(data.filled$steps, data.filled$date, sum, na.rm = TRUE))
```

```
## [1] 10766.19
```

Conclusion : The dataset with filled in value on missing values actually increase both the mean and median valuse retrieved in the first part of assignment.

