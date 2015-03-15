library(dplyr)
library(ggplot2)
#library(plyr)
#read data
data <- read.csv("activity.csv", head = T, stringsAsFactor = F)

#part 1: calculate total steps taken per day
total.steps <- data %>%
        group_by(date) %>%
        summarize(total.steps = sum(steps, na.rm = T) )
names(total.steps)[2] <- "total"
total <- total.steps$total
total.steps
#make a histogram
ggplot(total.steps, aes(x = date, y = total)) + geom_histogram(stat = "identity", binwidth = 70, color = "white") +
  theme(axis.text.x = element_text(angle = 70)) + xlab("Date") + ylab("Total steps per day") + 
  ggtitle("The total number of steps taken each day")

#calculate mean and median
data <- group_by(data, date)
mean.steps <- summarize(data, mean(steps, na.rm = T))
median.steps <- summarize(data, median(steps, na.rm = T))
total.daily <- summarize(data, sum(steps, na.rm = T))
names(total.daily)[2] <- "total"
names(mean.steps)[2] <- "mean"
names(median.steps)[2] <- "median"
mean.steps
median.steps

#part 2:calculate average steps taken by 5 min interval
interval <- group_by(data, interval)
interval.steps <- summarize(interval, mean(steps, na.rm = T))
names(interval.steps)[2] <- "interval.total"
Average_steps <- interval.steps$interval.total
interval.steps

#make interval.steps as time series
#Q:interval.ts <- ts(interval.steps, frequency = 1, start = 1, end = 288)
ggplot(interval.steps, aes(interval, Average_steps), breaks = 10) + 
  geom_line(aes(colour = Average_steps)) + scale_colour_gradient(low = "red") + 
  xlab("5 minute interval time") + ylab("Average steps accross all day") + 
  ggtitle("Average number steps")

#the max number
max <- max(Total_steps)
interval <- interval.steps[Total_steps == max, ]

#part 3: sum total NA number
na.total <- sum(is.na(data$steps))
na.total
#merge data with interval steps
na.day <- merge(data, interval.steps)
#replace NA with average number by 5 mins interval
na.day$steps = ifelse(is.na(na.day$steps), na.day$interval.total, na.day$steps)

#creat a new data with NA filled with average steps taken in every 5 minutes interval.
data2 <- select(na.day, steps, date, interval)
data2 <- arrange(data2, interval)
data2 <- arrange(data2, date)
head(data)
head(data2)

#total number taken each day in new data
total.day <- group_by(data2, date)
total2<- summarize(total.day, sum(steps))
names(total2)[2] <- "total2"
#creat histogram of the total number of steps taken each day 
ggplot(total2, aes(x = date, y = total2)) + 
  geom_histogram(stat = "identity", binwidth = 70, color = "white") + 
  theme(axis.text.x = element_text(angle = 70)) + xlab("Date") + ylab("Total steps per day") + 
  ggtitle("The total number of steps taken each day with NA filling in")

#calculate mean and median value in data2
data2 <- group_by(data2, date)
mean.steps2 <- summarize(data2, mean(steps))
median.steps2 <- summarize(data2, median(steps))
names(mean.steps2)[2] <- "mean"
names(median.steps2)[2] <- "median"
head(mean.steps2)
head(median.steps2)
#the mean and median in two data are exactly the same except NA 
total.daily2 <- summarize(data2, sum(steps))
names(total.daily2)[2] <- "total"
total.daily
total.daily2
#total daily number depends on the step numbers taken every day

#part 4: creat weekday and weekend
data2$day <- weekdays(as.Date(data2$date))
week.day <- unique(day)[1:5]
data2$day <- ifelse( data2$day %in% week.day, "Weekday", "Weedend")
data3 <- data2 %>%
    group_by(interval, day) %>%
    summarize( avg.steps = mean(steps) )
head(data2)

#creat a panel plot 
ggplot(data3,  aes(interval, avg.steps )) +
  geom_line() + facet_grid( day ~ . ) + xlab("5 minutes interval") + ylab("Average numbers of steps") +
  ggtitle("Average numbers steps taken across all weekdays and weekend")     



