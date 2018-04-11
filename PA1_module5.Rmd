
library(dplyr)
library(ggplot2)


setwd("C:/Users/usahoo/Desktop/module_5")

#1.Code for reading in the dataset and/or processing the data

activity_data<-read.csv(file='activity.csv',head=TRUE)

data<-activity_data[with(activity_data,{!(is.na(steps))}),]

#2.Histogram of the total number of steps taken each day

steps_by_day<-data %>% group_by(date) %>% summarise(total_steps=sum(steps))
hist(steps_by_day$total_steps,main = 'Histogram of number of steps per Day',
                   xlab = 'Total number of steps per Day')

#3.Mean and median number of steps taken each day 

summary(steps_by_day)

#4.Time series plot of the average number of steps taken

steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps,type = 'l',
    main='Average number of steps over all days',xlab='Interval',ylab = 'steps')

#5.The 5-minute interval that, on average, contains the maximum number of steps
steps_by_interval[ which.max(steps_by_interval$steps),]

#6.Code to describe and show a strategy for imputing missing data

sum(is.na(activity_data$steps))

data_imputed <- activity_data
 for (i in 1:nrow(data_imputed)) {
           if (is.na(data_imputed$steps[i])) {
                      interval_value <- data_imputed$interval[i]
                     steps_value <- steps_by_interval[
                              steps_by_interval$interval == interval_value,]
                    data_imputed$steps[i] <- steps_value$steps
                 }
       }

#7.Histogram of the total number of steps taken each day after missing values are imputed

data_imputed_steps<-aggregate(steps~date,data_imputed,sum)
hist(data_imputed_steps$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")

#8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

data_imputed$date=as.Date(data_imputed$date,format="%d/%m/%Y")

data_imputed['type_day']<-weekdays((data_imputed$date))

data_imputed$type_day[data_imputed$type_day %in% c("Saturday","Sunday")]<-"Weekend" 

data_imputed$type_day[data_imputed$type_day!="Weekend"]<-"Weekday" 


data_imputed$type_day <- as.factor(data_imputed$type_day)
data_imputed_steps_typeofday<-aggregate(steps ~ interval + type_day, data_imputed, mean)
qplot(interval, 
      steps, 
      data = data_imputed_steps_typeofday, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_day, ncol = 1)
