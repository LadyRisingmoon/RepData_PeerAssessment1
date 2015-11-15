---
title: "Reproducible Research Peer Assessment 1"
---

Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.


```r
# Research Log for activity pattern analysis of Activity Monitoring Data
setwd("C:/Users/Michelle/dataScience/ReproducibleResearch")
```

Loading and preprocessing the data

Show any code that is needed to

1) Load the data (i.e. read.csv())


```r
# Read the text file and handle missing dataset values which are coded as "?" 
mydata <- read.csv("activity.csv", header=TRUE, na.strings="NA")
```

2) Process/transform the data (if necessary) into a format suitable for your analysis


```r
# Convert between character representations and objects of class "Date" representing calendar dates
mydata$date <- as.Date(mydata$date)
```

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1) Calculate the total number of steps taken per day

2) If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
## Analysis: What is mean total number of steps taken per day?
# Histogram of the mean total number of steps taken each day
mydata$dateTime <- as.POSIXct(with(mydata, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
format="%Y-%m-%d %H:%M",tz="")
stepsPerDay <- setNames(aggregate(steps~as.Date(date), mydata, na.rm = TRUE, sum), c("date","steps"))
hist(stepsPerDay$steps, breaks = 28, main = "Mean Total Steps Taken Per Day", xlab = "Average Steps Per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

3) Calculate and report the mean and median of the total number of steps taken per day


```r
# Calculate and report the mean and median total number of steps taken per day
cat("Mean steps per day:", mean(stepsPerDay$steps))
```

```
## Mean steps per day: 10766.19
```

```r
cat("Median steps per day:", median(stepsPerDay$steps))
```

```
## Median steps per day: 10765
```

What is the average daily activity pattern?

1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
## Analysis: What is the average daily activity pattern?
# Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
stepsPerInterval <- tapply(mydata$steps, mydata$interval, mean, na.rm = TRUE)
plot(stepsPerInterval, type = "l", main="Average Steps Taken in 5-minute Intervals", xlab = "5-minute Intervals", ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
MaxFiveMinuteInterval <- which.max(stepsPerInterval)
cat("The max five minute value and interval number are:")
```

```
## The max five minute value and interval number are:
```

```r
print(MaxFiveMinuteInterval)
```

```
## 835 
## 104
```

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
## Inputing Missing Values
# Calculate and report the total number of missing values
mydataNACount <- sum(is.na(mydata$steps))
cat("Total NA values:", (mydataNACount))
```

```
## Total NA values: 2304
```

2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Devise a strategy for filling in all of the missing values in the dataset
# Duplicates the file to preserve original dataset
mydataNew <- mydata
```

3) Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Create a new dataset that is equal to the original dataset but with the missing data filled in
# Replaces NA values with the calculated mean of steps
mydataNew[is.na(mydataNew)] <- mean(mydataNew$steps, na.rm = TRUE)
```

4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# New dataset: Histogram of the total number of steps taken each day
stepsPerDayNew <- setNames(aggregate(steps~as.Date(date), mydataNew, sum), c("date","steps"))
hist(stepsPerDayNew$steps, breaks = 28, main = "New Dataset: Mean Total Steps Taken Per Day", xlab = "Average Steps Per Day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


```r
# New dataset: Calculate and report the mean and median total number of steps taken per day
cat("Filled dataset: Mean steps per day:", mean(stepsPerDayNew$steps))
```

```
## Filled dataset: Mean steps per day: 10766.19
```

```r
cat("Filled dataset: Median steps per day:", median(stepsPerDayNew$steps))
```

```
## Filled dataset: Median steps per day: 10766.19
```

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
mydataNew$date <- as.Date(mydataNew$date, "%Y-%m-%d")
mydataNew$day <- weekdays(mydataNew$date)
mydataNew$dayType <- c("Weekday")
for (i in 1:nrow(mydataNew)){
  if (mydataNew$day[i] == "Saturday" || mydataNew$day[i] == "Sunday"){
    mydataNew$dayType[i] <- "Weekend"
  }
}
```


```r
# Panel plot of the total number of steps taken for Weekends versus Weekdays
mydataNew$dayType <- as.factor(mydataNew$dayType)
dayClass <- aggregate(steps ~ interval+dayType, mydataNew, mean)
weekendClass <- (subset(dayClass, dayType == 'Weekend'))
weekdayClass <- (subset(dayClass, dayType == 'Weekday'))

par(mfrow = c(2,1))
plot(weekendClass$steps, weekendClass$intervals, type = "l", xlab="5-minute intervals", ylab= "Average Steps Per Day", main = "Weekends")
plot(weekdayClass$steps, weekdayClass$intervals, type = "l", xlab="5-minute intervals", ylab= "Average Steps Per Day", main = "Weekdays")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

