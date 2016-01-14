# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv("D:\\Study\\DataScience\\Coursera_Assignments\\Course5\\Week1_Assignment\\RepData_PeerAssessment1\\activity.csv")
```


## What is mean total number of steps taken per day?

```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

total_step <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
par(mfrow = c(1, 1))

hist(total_step$steps, breaks = 20, 
     main = "Total Number of Steps Taken Each Day",
     col = "grey", border = "white", xlab = "Step", axes = FALSE)
axis(1)
axis(2, las = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```r
mean(total_step$steps)
```

```
## [1] 10766.19
```

```r
median(total_step$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
avg_step <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(avg_step$interval, avg_step$steps, type = "l", lwd = 2, col = "navy",
     main = "Time Series: Average Number of Steps Taken", axes = FALSE,
     xlab = "5-minute interval", ylab = "Average number of steps")
axis(1)
axis(2, las = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?

```r
avg_step$interval[which.max(avg_step$steps)]
```

```
## [1] 835
```


## Imputing missing values
There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
sum(is.na(activity))
```

```
## [1] 2304
```

All of the missing values are filled in with mean value for that 5-minute
interval.


```r
# Replace each missing value with the mean value of its 5-minute interval
imp <- activity # new dataset called imp
for (i in avg_step$interval) {
  imp[imp$interval == i & is.na(imp$steps), ]$steps <- 
    avg_step$steps[avg_step$interval == i]
}
```
Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.


```r
total_step_imp <- aggregate(steps ~ date, data = imp, sum, na.rm = TRUE)
hist(total_step_imp$steps, breaks = 20, 
     main = "Total Number of Steps Taken Each Day (Imputed)",
     col = "grey", border = "white", xlab = "Step", axes = FALSE)
axis(1)
axis(2, las = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```r
imp$day <- weekdays(imp$date)
imp$week <- ""
imp[imp$day == "Saturday" | imp$day == "Sunday", ]$week <- "weekend"
imp[!(imp$day == "Saturday" | imp$day == "Sunday"), ]$week <- "weekday"
imp$week <- factor(imp$week)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.

```r
avg_step_imp <- aggregate(steps ~ interval + week, data = imp, mean)
library(lattice)
xyplot(steps ~ interval | week, data = avg_step_imp, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekday days or weekend days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
