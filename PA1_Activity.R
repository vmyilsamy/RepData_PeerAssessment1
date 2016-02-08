# Load data
activityData <- read.csv("./Activity/activity.csv")


#average total number of steps taken per day
library(ggplot2)
totalSteps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
png(file = "figure1.png", width = 480, height = 480, units = "px")
qplot(totalSteps, binwidth=1000, xlab="Total no. of steps taken per day")
dev.off()
mean(totalSteps, na.rm=TRUE)
median(totalSteps, na.rm=TRUE)


#average daily activity pattern
library(ggplot2)
averageDailyActivity <- aggregate(steps ~interval, activityData, FUN=mean, na.rm=TRUE)
png(file = "figure2.png", width = 480, height = 480, units = "px")
ggplot(data=averageDailyActivity, aes(x=interval, y=steps)) +
      geom_line() +
      xlab("5-minute interval") +
      ylab("Avg. no. of steps taken")
dev.off()
#maximum number of steps
averageDailyActivity[which.max(averageDailyActivity$steps),]


#Missing steps data
missingSteps <- is.na(activityData$steps)
table(missingSteps)


#Fill missing value with the mean value of its 5-minute interval
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

#Calculate and report the mean and median total number of steps taken per day
totalSteps <- tapply(filledWithMean.Data$steps, filledWithMean.Data$date, FUN=sum)
png(file = "figure3.png", width = 480, height = 480, units = "px")
qplot(totalSteps, binwidth=1000, xlab="Total no. of steps taken per day")
dev.off()

mean(totalSteps)

median(totalSteps)


#Differences in activity patterns between weekdays and weekends
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


#Time series plot of the 5-minute interval (x-axis) and the average number of steps taken
averageDaily <- aggregate(steps ~ interval + day, data=filledWithMean.Data, mean)
png(file = "figure4.png", width = 480, height = 480, units = "px")
ggplot(averageDaily, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
      xlab("5-minute interval") + ylab("Number of steps")
dev.off()
