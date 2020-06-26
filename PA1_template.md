---
title: 'Reproducible Research: Peer Assessment 1'
output: md_document
---


## Loading and preprocessing the data

```{r loaddata}
library(dplyr)

setwd("E:/r coding files/Reproducible research")
file <- "repdata_data_activity.zip"

if(!file.exists(file))
{
        file <-download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")
}

if(!file.exists("./data"))
{
        unzip(zipfile = file, exdir = "./data")
}

setwd("E:/r coding files/Reproducible research/data")

##read the data

read_file <- read.csv("activity.csv")
head(read_file)
```

## What is mean total number of steps taken per day?

```{r plots and mean}
##convert into tibble

read_file <- tbl_df(read_file)

## total steps each day
total_steps_by_date <-read_file %>%
        group_by(date) %>%
        summarise(sum=sum(steps,na.rm=T))

##plotting hist
hist(x=total_steps_by_date$sum,xlab="total number of steps taken per day",
     ylab="count of steps",main="steps count by date",col="red",
     breaks = length(unique(read_file$date)))

## mean of number of steps per day
 mean_total_steps_per_day <- summarise(total_steps_by_date,mean=mean(total_steps_by_date$sum,na.rm=T))
 mean_total_steps_per_day
 
## median of number of steps per day
 
 median_total_steps_per_day <- summarise(total_steps_by_date,median=median(total_steps_by_date$sum,na.rm=T))
 median_total_steps_per_day
 
```


## What is the average daily activity pattern?

```{r average activity pattern}

##time series plot

 average_total_steps_5min_interval<- read_file %>%
         group_by(interval) %>%
         summarise(average=mean(steps,na.rm = T))
 
plot(y=average_total_steps_5min_interval$average,x=average_total_steps_5min_interval$interval,xlab="5 minute interval",
      ylab="average number of steps",main="average number of steps by 5 minute interval",col="red",type="l")

##interval with maximum number of steps

average_total_steps_5min_interval[which.max(average_total_steps_5min_interval$average),1]

```

## Imputing missing values

```{r missing values}
##total number of missing value in the data set
nrow((read_file[read_file$steps=='NA',1]))

final_filled<-data.frame()

final_filled<- read_file[!is.na(read_file$steps),]


read_NA<-read_file[is.na(read_file$steps),]


x<- left_join(read_NA,average_total_steps_5min_interval)

##some operations for replacing with mean

x<- x[,c(2,3,4)]
x[,c(1,2,3)]<-x[,c(3,1,2)]
names(x)<-c("steps","date","interval")

final_filled<-rbind(final_filled,x)
final_filled<-arrange(final_filled,date,interval)

##check dimensions for clarity
dim(final_filled)
dim(read_file)

## plotting the histogram for updated file without NA

total_steps_by_date_new <-final_filled %>%
        group_by(date) %>%
        summarise(sum=sum(steps,na.rm=T))

hist(x=total_steps_by_date_new$sum,xlab="total number of steps taken per day",
     ylab="count of steps",main="steps count by date",col="red",
     breaks = length(unique(final_filled$date)))


## mean of number of steps per day
mean_total_steps_per_day_new <- summarise(total_steps_by_date_new,mean=mean(total_steps_by_date_new$sum,na.rm=T))
mean_total_steps_per_day_new

## median of number of steps per day

median_total_steps_per_day_new <- summarise(total_steps_by_date_new,median=median(total_steps_by_date_new$sum,na.rm=T))
median_total_steps_per_day_new
```


## Are there differences in activity patterns between weekdays and weekends?
```{r week based}
final_filled<- mutate(final_filled,date=as.Date(date))
final_filled<-mutate(final_filled,days=weekdays(date))
final_filled<-mutate(final_filled,week=gsub("Monday|Tuesday|Wednesday|Thursday|Friday","weekday",final_filled$days))
final_filled<-mutate(final_filled,week=gsub("Saturday|Sunday","weekend",final_filled$week))
final_filled<-mutate(final_filled,week=as.factor(final_filled$week))


##panel plots


average_total_steps_5min_interval_new<- final_filled %>%
        group_by(interval,week) %>%
        summarise(average=mean(steps,na.rm = T))

library(ggplot2)

ggplot(average_total_steps_5min_interval_new,aes(x=interval,y=average))+
        geom_line()+facet_grid(week~.)+labs(title = "Avg. Daily Steps by Weektype",
                                            x = "Interval", y = "No. of Steps")

```
