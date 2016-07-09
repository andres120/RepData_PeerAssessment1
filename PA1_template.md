# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
activity <- read.csv(unz("activity.zip", "activity.csv"))
activity <- cbind(activity, weekdays(as.Date(activity$date)))
names(activity)[4] <- "day"
```

## What is mean total number of steps taken per day?


1. Make a histogram of the total number of steps taken each day



```r
summ <- group_by(activity, day) %>% 
  summarise(Mean = mean(steps, na.rm=T), 
            Median = median(steps, na.rm=T), 
            Sum = sum(steps, na.rm=T))
ggplot(summ, aes(x=day, y=Sum)) + geom_bar(stat="identity") +
  ggtitle("Total Steps with NAs") +
  xlab("Day of the week") +
  ylab("Step count")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
summ[,1:3]
```

```
## Source: local data frame [7 x 3]
## 
##         day     Mean Median
##      <fctr>    <dbl>  <dbl>
## 1    Friday 42.91567      0
## 2    Monday 34.63492      0
## 3  Saturday 43.52579      0
## 4    Sunday 42.63095      0
## 5  Thursday 28.51649      0
## 6   Tuesday 31.07485      0
## 7 Wednesday 40.94010      0
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
means <- group_by(activity, interval) %>% 
  summarise(Mean = mean(steps, na.rm=T))
ggplot(means, aes(x=interval, y=Mean)) + geom_line() +
  ggtitle("Average of steps during the day") +
  xlab("Interval") +
  ylab("Steps average")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
means[which.max(means$Mean),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval     Mean
##      <int>    <dbl>
## 1      835 206.1698
```
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
dim(activity[!complete.cases(activity),])[1]
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Change the column name to avoid conflicts:
names(means)[1] <- "Interval"

# This functions returns the mean from an interval
int2mean <- function(interval) {
  return(means[means$Interval == interval,]$Mean)
}

# Ended up using a for loop as mutate did not seem to working well.
complete <- activity
for(x in c(1:17568)) {
  st <- activity[x,]$steps
  it <- activity[x,]$interval
  complete[x,]$steps <- ifelse(is.na(st), int2mean(it), st)
}
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
n <- group_by(complete, day) %>% summarise(sum = sum(steps))
ggplot(n, aes(day, sum))+
  geom_bar(stat="identity")+
  ggtitle("Total Steps without NAs")+
  xlab("Day of the week")+
  ylab("Step count")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
group_by(complete, day) %>% summarise(Mean = mean(steps), 
                                      Median = median(steps))
```

```
## Source: local data frame [7 x 3]
## 
##         day     Mean Median
##      <fctr>    <dbl>  <dbl>
## 1    Friday 41.68610      0
## 2    Monday 35.24552      0
## 3  Saturday 42.75789      0
## 4    Sunday 41.97491      0
## 5  Thursday 29.50162      0
## 6   Tuesday 31.07485      0
## 7 Wednesday 40.54483      0
```

Filling the missing data slightly affected the means, did not change the medians, and as expected it noticeable increased the total of steps.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
complete <- mutate(complete, 
                   weekday = ifelse(complete$day == "Saturday" | 
                                      complete$day == "Sunday", 
                                    "weekend", 
                                    "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
weekends <- group_by(complete[complete$weekday == "weekend",], 
                     interval) %>% 
  summarise(Mean = mean(steps, na.rm=T))
weekdays <- group_by(complete[complete$weekday == "weekday",], 
                     interval) %>% 
  summarise(Mean = mean(steps, na.rm=T))
par(mfrow=c(2,1))
plot(weekdays$Mean~weekdays$interval, type="l", 
     main="Weekdays", 
     ylab="Steps Avg.", 
     xlab="Interval")
plot(weekends$Mean~weekends$interval, type="l", 
     main="Weekends", 
     ylab="Steps Avg.", 
     xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->