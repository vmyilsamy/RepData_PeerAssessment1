# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activityData <- read.csv("./Activity/activity.csv")
```

## What is mean total number of steps taken per day?

```r
totalSteps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
mean(totalSteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

## What is the average daily activity pattern?

```r
averageDailyActivity <- aggregate(steps ~interval, activityData, FUN=mean, na.rm=TRUE)
```

## Imputing missing values

```r
missingSteps <- is.na(activityData$steps)
table(missingSteps)
```

```
## missingSteps
## FALSE  TRUE 
## 15264  2304
```

## Are there differences in activity patterns between weekdays and weekends?

```r
missingSteps <- is.na(activityData$steps)
table(missingSteps)
```

```
## missingSteps
## FALSE  TRUE 
## 15264  2304
```

###Fill missing value with the mean value of its 5-minute interval

```r
FillMissingValue <- function(steps, interval) {
      filledWithMean <- NA
      if (!is.na(steps))
            filledWithMean <- c(steps)
      else
            filledWithMean <- (averageDailyActivity[averageDailyActivity$interval==interval, "steps"])
      return(filledWithMean)
}
filledWithMean.Data <- activityData
filledWithMean.Data$steps <- 
      mapply(FillMissingValue, filledWithMean.Data$steps, filledWithMean.Data$interval)
```

###Calculate and report the mean and median total number of steps taken per day

```r
totalSteps <- tapply(filledWithMean.Data$steps, filledWithMean.Data$date, FUN=sum)

mean(totalSteps)
```

```
## [1] 10766.19
```

```r
median(totalSteps)
```

```
## [1] 10766.19
```

###Differences in activity patterns between weekdays and weekends

```r
weekdayOrWeekend <- function(date) {
      day <- weekdays(date)
      if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
            return("weekday")
      else if (day %in% c("Saturday", "Sunday"))
            return("weekend")
      else
            stop("invalid date")
}
filledWithMean.Data$date <- as.Date(filledWithMean.Data$date)
filledWithMean.Data$day <- sapply(filledWithMean.Data$date, FUN=weekdayOrWeekend)
```
