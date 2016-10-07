

library(dplyr)

##set file been used##
setwd("D:/Users/TM35460/Desktop/repdata-data-activity")  
activitydata<- read.csv("D:/Users/TM35460/Desktop/repdata-data-activity/activity.csv") 

###1.Code for reading in the dataset and/or processing the data####
head(activitydata) 
dim(activitydata) 
glimpse(activitydata)
summary(activitydata)  
activitydata$date<- as.Date(activitydata$date)


###2.Histogram of the total number of steps taken each day###
##total of steps
Total_Steps<- activitydata%>%
group_by(date)%>%
filter(!is.na(steps))%>%
summarise(total_steps = sum(steps, na.rm=TRUE))
Total_Steps
##Plot using ggplot##
library(ggplot2)
ggplot(Total_Steps, aes(x = total_steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Total Steps", y = "Frequency")


###3.Mean and median number of steps taken each day#####
##calculate mean###
Mean_Steps<- mean(Total_Steps$total_steps, na.rm=TRUE)
Mean_Steps
##calculate median##
Median_Steps<- median(Total_Steps$total_steps, na.rm=TRUE)
Median_Steps

##4.Time series plot of the average number of steps taken##
#####Calculating Avg. Steps####
Interval<- activitydata%>%
group_by(interval)%>%
filter(!is.na(steps))%>%
summarise(avg_steps = mean(steps, na.rm=TRUE))
Interval

###5. 5-minute interval that, on average, contains the maximum number of steps###
#####Plotting Avg. Steps:#####
ggplot(Interval, aes(x =interval , y=avg_steps)) +
  geom_line(color="blue", size=1) +
  labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
#####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps###
Interval[which.max(Interval$avg_steps),]

###6.Code to describe and show a strategy for imputing missing data##
##### 1. Calculate total number of missing values in the dataset:
sum(is.na(activitydata$steps))
##### 2. Imputing missing values using mean for each day and 3. Create a new dataset that is equal to the original dataset but with the missing data filled in:####
activitydata2<- activitydata
nas<- is.na(activitydata2$steps)
avg_interval<- tapply(activitydata2$steps, activitydata2$interval, mean, na.rm=TRUE, simplify = TRUE)
activitydata2$steps[nas] <- avg_interval[as.character(activitydata2$interval[nas])]
names(activitydata2)
#### 4. Check if no missing value is appearing:
sum(is.na(activitydata2))
##### 5. Reorder columns (for better understanding of the data):
activitydata2<- activitydata2[, c("date", "interval", "steps")]
head(activitydata2)

Total_Steps2<- activitydata2%>%
  group_by(date)%>%
  summarise(total_steps = sum(steps, na.rm=TRUE))
Total_Steps2
ggplot(Total_Steps2, aes(x = total_steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps including Missing values", x = "Interval", y = "No. of Steps")

Mean_Steps2<- mean(Total_Steps2$total_steps, na.rm=TRUE)
Mean_Steps2

Median_Steps2<- median(Total_Steps2$total_steps, na.rm=TRUE)
Median_Steps2
head(activitydata2)




##### 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activitydata2<- activitydata2%>%
  mutate(weektype= ifelse(weekdays(activitydata2$date)=="Saturday" | weekdays(activitydata2$date)=="Sunday", "Weekend", "Weekday"))

head(activitydata2)
###### Plotting:
Interval2<- activitydata2%>%
  group_by(interval, weektype)%>%
  summarise(avg_steps2 = mean(steps, na.rm=TRUE))
head(Interval2)


plot<- ggplot(Interval2, aes(x =interval , y=avg_steps2, color=weektype)) +
  geom_line() +
  labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(plot)