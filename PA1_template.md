# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
activity <- read.csv(file = "activity.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
aggdata <- aggregate(x = activity$steps, by = list(date = activity$date), FUN = "sum")
ggplot(data = aggdata, aes(x = aggdata$x)) + geom_histogram()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

Mean: 10766.19, Median: 10765

```r
mean(aggdata$x, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(aggdata$x, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
complete <- activity[complete.cases(activity$steps), ]
aggdata2 <- aggregate(x = complete$steps, by = list(interval = complete$interval), FUN = "mean")
ggplot(data = aggdata2, aes(interval, x)) + geom_line() + xlab("5-minute interval") + ylab("Average steps across all days") + scale_x_continuous(breaks = round(seq(min(aggdata2$interval), max(aggdata2$interval), by = 250), 1))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Interval '835' has the maximum number of steps:


```r
aggdata2[which.max(aggdata2$x), ]
```

```
##     interval        x
## 104      835 206.1698
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2304 rows with NAs


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy to use: Set NAs values with the mean of the 5-minute interval.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity2 <- activity

complete <- activity[complete.cases(activity$steps), ]
# Calculate the mean for each interval across all days
aggdata2 <- aggregate(x = complete$steps, by = list(interval = complete$interval), FUN = "mean")

# replace NA values
for(i in 1:nrow(activity2)){
  if(is.na(activity2[i, "steps"])) {
    activity2[i, "steps"] <- aggdata2[which(aggdata2$interval == activity2[i, "interval"]), "x"]
  }
}

head(activity2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Histogram of steps taken each day

```r
aggdata3 <- aggregate(x = activity2$steps, by = list(date = activity2$date), FUN = "sum")
ggplot(data = aggdata3, aes(x = aggdata3$x)) + geom_histogram()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Mean: 10766.19, Median: 10766.19

```r
mean(aggdata3$x)
```

```
## [1] 10766.19
```

```r
median(aggdata3$x)
```

```
## [1] 10766.19
```

As we can see, the Median value increases when imputing NA values, while the Mean value stays the same. 


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity2$date <- as.Date(activity2$date)
#create a vector of weekdays
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#adding a column for indicating if the date is a weekday or weekend
activity2$dayOfWeek <- factor((weekdays(activity2$date) %in% weekdays), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

head(activity2)
```

```
##       steps       date interval dayOfWeek
## 1 1.7169811 2012-10-01        0   weekday
## 2 0.3396226 2012-10-01        5   weekday
## 3 0.1320755 2012-10-01       10   weekday
## 4 0.1509434 2012-10-01       15   weekday
## 5 0.0754717 2012-10-01       20   weekday
## 6 2.0943396 2012-10-01       25   weekday
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:




```r
dataWeekdays <- activity2[which(activity2$dayOfWeek == "weekday"), ]
dataWeekends <- activity2[which(activity2$dayOfWeek == "weekend"), ]
aggdataWeekdays <- aggregate(x = dataWeekdays$steps, by = list(interval = dataWeekdays$interval), FUN = "mean")
aggdataWeekends <- aggregate(x = dataWeekends$steps, by = list(interval = dataWeekends$interval), FUN = "mean")

g1 <- ggplot(data = aggdataWeekends, aes(interval, x)) + geom_line() + xlab("Interval") + ylab("Number of steps") + scale_x_continuous(breaks = round(seq(min(aggdataWeekends$interval), max(aggdataWeekends$interval), by = 250), 1)) + ggtitle("Weekend")

g2 <- ggplot(data = aggdataWeekdays, aes(interval, x)) + geom_line() + xlab("Interval") + ylab("Number of steps") + scale_x_continuous(breaks = round(seq(min(aggdataWeekdays$interval), max(aggdataWeekdays$interval), by = 250), 1)) + ggtitle("Weekday")

multiplot(g1, g2, cols=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
