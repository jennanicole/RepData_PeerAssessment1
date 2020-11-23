# Loading and Processing the Data

library(dplyr)
library(tidyr)
library(lattice)
library(readr)
library(RColorBrewer)
setwd("~/Coursera/Reproducible Research/Week 2/Project 1")
d <- read.csv("activity.csv")

d$date <- as.Date(d$date)
d_m <- subset(d, !is.na(d$steps))

## What is the mean total number of steps taken per day?

sum_steps <- tapply(d_m$steps, d_m$date, sum, na.rm=TRUE, simplify=T)
sum_steps <- sum_steps[!is.na(sum_steps)]

hist(x=sum_steps,col = "steelblue1", breaks = 10, main = "Total Number of Steps per Day", xlab = "Total # of Steps", ylab = "Frequency")

mean_steps <- mean(sum_steps, digits = 0)
mean_steps

med_steps <- median(sum_steps)
med_steps

## What is the average daily activity pattern?

avg_int <- tapply(d_m$steps, d_m$interval, mean, na.rm=T, simplify=T)
d_avg <- data.frame(interval=as.integer(names(avg_int)), avg=avg_int)

with(d_avg,
     plot(interval, avg, type="l", xlab = "5-minute Interval", ylab = "Average Steps Taken Across all Days"))

max_steps <- max(d_avg$avg)
d_avg[d_avg$avg == max_steps, ]


sum(is.na(d$steps))

d2 <- is.na(d$steps)
d$steps[d2] <- avg_int[as.character(d$interval[d2])]

sum_steps_nm <- tapply(d$steps, d$date, sum, na.rm=T, simplify=T)

hist(x=sum_steps_nm, col = "palegreen3", breaks = 10, main = "Total Number of Steps per Day (missing data added)", xlab = "Total # of Daily Steps", ylab = "Frequency")

mean_steps_nm <- mean(sum_steps_nm)
mean_steps_nm

med_steps_nm <- median(sum_steps_nm)
med_steps_nm

## Are there any differences in activity patterns between weekdays and weekends?

is_weekday <- function(x) {
  wd <- weekdays(x)
  ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(d$date, is_weekday)
d$wk <- as.factor(wx)
head(d)

d_wk <- aggregate(steps ~wk+interval, data=d, FUN=mean)
xyplot(data=d_wk, steps ~ interval | factor(wk), layout = c(1, 2), type="l", lty=1, main = "Time Series Plot of 5-Min Interval and Steps Taken", xlab = "5-Min Interval", ylab = "# of Steps")


