---
title: "Reproducible Research Course Project 1"
author: "Mika Dumalaog"
date: "May 4, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load packages
```{r, echo = TRUE}
# Set Up Environment
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)
```

# Loading and preprocessing the data
```{r, echo = TRUE}
setwd("~/ReproResCP")
activity <- read.csv("activity.csv")
head(activity)
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)
is.regular(activity$date)
unique(activity$date)
summary(activity)
```

# What is mean total number of steps taken per day?
```{r, echo = TRUE}
png(file="plot1.png")
totalSBD <- aggregate(steps~date, activity, sum)
totalSBD
#Histogram of Total Number of Steps taken each day 
hist(totalSBD$steps, xlab="Class - Total Number of Steps per Day", ylab = "Number of Days", main = "Total Number of Steps Taken Each Day")
dev.off()
# Calculate and report the mean and median of the total number of steps taken per day
ave_steps <- mean(totalSBD$steps)
ave_steps
med_steps <- median(totalSBD$steps)
med_steps
```

# What is the average daily activity pattern?
```{r, echo = TRUE}
png(file="plot2.png")
averageSbyInt <- aggregate(steps~interval, activity, mean)
averageSbyInt
# Time series plot of the average number of steps
with(averageSbyInt, plot(interval, steps, type = "l", main = "Average steps per time interval"))
dev.off()
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max(activity$steps, na.rm = TRUE)
```

# Imputing missing values
```{r, echo = TRUE}
# Calculate and report the total number of missing values in the dataset
sum(is.na(activity$steps))

# Device the strategy for filling in all of the missing values in the dataset
# Create a new dataset that is equal to the original dataset but with the missing data filled in
png(file="plot3.png")
act2 <- activity
sapply(act2, class)
act2$steps[is.na(act2$steps)] <- mean(na.omit(activity$steps))
act2$date <- as.Date(act2$date, format = "%Y-%m-%d")

# Make a histogram of the total number of steps taken each day
day2steps <- aggregate(steps~date, rm.na = TRUE, data = act2, FUN = sum)
par(mfrow = c(1,2))
plot(day2steps, type = "h", lwd = 5, main = "With NAs")
plot(day2steps, type = "h", lwd = 5, main = "NAs filled")
dev.off()

# Compare mean and median to total steps taken per day

mean(day2steps$steps)
median(day2steps$steps)

aggregate(steps~date, data = activity, FUN = mean)
aggregate(steps~date, data = activity, FUN = median)
aggregate(steps~date, data = act2, FUN = mean)
aggregate(steps~date, data = act2, FUN = median)
```

# Are there differences in activity patterns between weekdays and weekends?
```{r, echo = TRUE}
# Create a new factor variable in the dataset with two levels indicating where it is a weekday or weekend
levels <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
levels
activity$date <- as.Date(strptime(activity$date, format = "%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"} else
  {y <- "Weekday"}
})

# Panel Plot 
png(file="plot4.png")
actdate <- aggregate(steps~interval +datetype, activity, mean, na.rm = TRUE)
plot <- ggplot(actdate, aes(x = interval, y = steps, color = datetype)) + 
  geom_line() + 
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") + 
  facet_wrap(~datetype, ncol = 1, nrow = 2)
print(plot)
dev.off()
```
