library(lubridate)
library(dplyr)
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

# the data is a string, parsing it to a date
activity$date <- parse_date_time(activity$date, "Ymd")

#for some tasks the NA can be ignored, doing this here
activiy_withoutNA <- activity[complete.cases(activity),]

#assignment 1
stepsPerDay <- group_by(activiy_withoutNA, date)
stepsPerDay <- summarise(stepsPerDay, totalSteps = sum(steps))
hist(stepsPerDay$totalSteps, xlab="Total number of steps per day", main="Total number of steps per day", col="orangered1")

## assignment 2

stepsPerInterval <- group_by(activiy_withoutNA, interval)
stepsPerInterval <- summarise(stepsPerInterval, Steps = mean(steps))

plot(stepsPerInterval$interval, stepsPerInterval$Steps, type="l", xlab="Interval", ylab="Average number of steps per interval" , main="Average daily activity pattern", col="orangered1", fill="orangered1")

stepsPerInterval <- arrange(stepsPerInterval, desc(Steps))
test <- stepsPerInterval[[1, c("interval")]]

## imputing

library(Hmisc)
activityImputed<-activity
activityImputed$steps <- as.numeric(impute(activity$steps, mean))

stepsPerDayImputed <- group_by(activityImputed, date)
stepsPerDayImputed <- summarise(stepsPerDayImputed, totalSteps = sum(steps))


##weekday
activityImputed$Weekday <- wday(activityImputed$date, label=TRUE)

isWeekDay <- function(x)
{
  x <- as.character(x)
  if (x=="Sun" || x=="Sat")
  {
    return ("Weekend")
  }
  else
  {
    return ("Weekday")
  }
}

activityImputed$Weekend <- sapply(activityImputed$Weekday, isWeekDay)

activityImputed[, 'Weekend'] <- as.factor(activityImputed[, 'Weekend'])

stepsPerIntervalWeekday <- group_by(activityImputed[activityImputed$Weekend=="Weekday",], interval)
stepsPerIntervalWeekday <- summarise(stepsPerIntervalWeekday, Steps = mean(steps))

stepsPerIntervalWeekend <- group_by(activityImputed[activityImputed$Weekend=="Weekend",], interval)
stepsPerIntervalWeekend <- summarise(stepsPerIntervalWeekend, Steps = mean(steps))

par(mfrow=c(2,1), mar=c(2,5,4,3))
plot(stepsPerIntervalWeekend$interval, stepsPerIntervalWeekend$Steps, type="l", xlab="", ylab="", col="deepskyblue", main="Weekend" ,col.main="orange")

plot(stepsPerIntervalWeekday$interval, stepsPerIntervalWeekday$Steps,col="deepskyblue", type="l", xlab="Interval", ylab="",yaxt='n', main="Weekday" ,col.main="orange")
axis(side = 4)
mtext("Number of steps", side = 2, adj=3.5, padj=-4)


xyplot(Steps ~ interval, data=stepsPerIntervalWeekend, type='l')
