---
title: "JonPerezCP1"
output: html_document
---

```{r}
library(knitr)
library(lattice)
data=read.csv("~/Documents/Coursera/Reproducible Research/Course Project 1/activity.csv")
```
# What is mean total number of steps taken per day?

## 1.Calculate the total number of steps taken per day
```{r}
steps_day<-sum(data$steps,na.rm=TRUE)
steps_day
```

## 2.Make a histogram of the total number of steps taken each day

```{r}
steps_each_day<-aggregate(steps~date, data=data, FUN=sum, na.rm=TRUE)
hist(steps_each_day$steps)
```

## 3.Calculate and report the mean and median of the total number of steps taken per day

```{r}
steps_each_day_mean <- mean(steps_each_day$steps)
steps_each_day_median <- median(steps_each_day$steps)
steps_each_day_mean
steps_each_day_median
```

# What is the average daily activity pattern?

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
## average number of steps taken, averaged across all days (y-axis)

```{r}
five_minutes_average <- aggregate(steps~interval, data=data, FUN=mean, na.rm=TRUE)
plot(x = five_minutes_average$interval, y = five_minutes_average$steps, type = "l")
```

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the 
## maximum number of steps?

```{r}
max_steps <- max(five_minutes_average$steps)
  for (i in 1:288) 
  {
    if (five_minutes_average$steps[i] == max_steps)
      five_minute_interval_at_max_steps <- five_minutes_average$interval[i]
  }
five_minute_interval_at_max_steps 
```

# Imputing missing values

## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r, echo=TRUE}
total_na <- 0
for (i in 1:17568)
  {
   if(is.na(data$steps[i])) 
      total_na <- total_na+1 
  }
total_na
```

## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r,echo=TRUE}
activity_filled_in <- data
for (i in 1:17568) # loop to find the na
{
  if(is.na(activity_filled_in$steps[i])) # if steps is na store the pointer 
  { 
    five_minute_pointer <- activity_filled_in$interval[i] #store the value of pointer to find the mean on five minute interval
    for (j in 1:288)  # loop to find the value of pointer on the data frame of five minute interval
    {
      if (five_minutes_average$interval[j] == five_minute_pointer) # finding the value of mean of five minute interval data frame
        activity_filled_in$steps[i] <- five_minutes_average$steps[j] # replacing the na by the mean in that fime minute interval 
      
    }
  }
}
```

## 4.  Make a histogram of the total number of steps taken each day

```{r,echo=TRUE}
steps_each_day_filled_in <- aggregate(steps~date, data=activity_filled_in, FUN=sum, na.rm=TRUE)
hist(steps_each_day_filled_in$steps)
```

## 4. Calculate the mean and median and explain the imoact of imputing missing data on the estimates 
## of the total daily number of steps
```{r}
steps_each_day_mean_filled_in <- mean(steps_each_day_filled_in$steps)
steps_each_day_median_filled_in <- median(steps_each_day_filled_in$steps)
```

# Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels – “weekday” and 
## “weekend” indicating whether a given date is a weekday or weekend day.

```{r,echo=TRUE}

library(lubridate)

week <- wday(activity_filled_in$date)
week_day <- week
for (i in 1:17568) # loop to find the na
{
  if(week[i] == 1)
    week_day[i] <- 'weekend'
  if(week[i] == 2)
    week_day[i] <- 'weekday'
  if(week[i] == 3)
    week_day[i] <- 'weekday'
  if(week[i] == 4)
    week_day[i] <- 'weekday'
  if(week[i] == 5)
    week_day[i] <- 'weekday'
  if(week[i] == 6)
    week_day[i] <- 'weekday'
  if(week[i] == 7)
    week_day[i] <- 'weekend'
}
```

### Creating a new factor variable in the dataset "activity_filled_in" 
```{r,echo=TRUE}
activity_filled_in$weekday <-week_day
```
## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r,echo=TRUE}
weekday <- grep("weekday",activity_filled_in$weekday)
weekday_frame <- activity_filled_in[weekday,]
weekend_frame <- activity_filled_in[-weekday,]

five_minutes_average_weekday <- aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
five_minutes_average_weekend <- aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)

plot(x = five_minutes_average_weekday$interval, y = five_minutes_average_weekday$steps, type = "l") 
plot(x = five_minutes_average_weekend$interval, y = five_minutes_average_weekend$steps, type = "l") 
```

