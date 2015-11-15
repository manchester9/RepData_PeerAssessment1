# Assignment_1

library(knitr)
library(ggplot2)
library(scales)
library(Hmisc)
library(dplyr)
library(lubridate)
library(lattice)

# *******************
if (!file.exists("data")) {dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "./data/activity.zip"

download.file(fileUrl, destfile = destfile)
dateDownloaded <- date()

dataset <- read.csv(unz("./data/activity.zip", "activity.csv"))

dim(dataset) # 17,568 * 3
colnames(dataset) #steps date interval
str(datasets) # int fact int

# *******************
# Stage 1
steps_by_day <- tapply(dataset$steps, dataset$date, sum, na.rm=TRUE) 

# Histogram
qplot(steps_by_day, xlab='Total steps per day', ylab='Frequency per day', binwidth=700)

# Mean and median steps by day
steps_by_day_mean <- mean(steps_by_day) # 9,354.23
steps_by_day_median <- median(steps_by_day) # 10,395

# *******************
# Stage 2

avg_steps_per_block <- aggregate(x=list(mean_steps=dataset$steps), by=list(interval=dataset$interval), FUN=mean, na.rm=TRUE)
dim(avg_steps_per_block) # 288 * 2
# interval (int)
# meanSteps (num)

ggplot(avg_steps_per_block, aes(mean_steps, interval)) + 
	   geom_line(color = "steelblue", size = 0.8) + 
	   labs(title = "Time Series of 5-minute interval", x = "5-minute intervals", y = "Average number of steps taken")

# *******************
# Stage 3

#Number of missing values
number_of_missing_values <- length(which(is.na(dataset$steps))) # 2,304

# Using the mean to impute
new_dataset <- dataset
new_dataset$steps <- impute(dataset$steps, fun=mean)

steps_by_day_new_dataset <- tapply(new_dataset$steps, new_dataset$date, sum)
qplot(steps_by_day_new_dataset, 
	  xlab='Total steps per day (Imputed)', 
	  ylab='Frequency per day', 
	  binwidth = 700)

steps_by_day_new_dataset_mean <- mean(steps_by_day_new_dataset)
steps_by_day_new_dataset_median <- median(steps_by_day_new_dataset)

# *******************
# Stage 4

# Are there differences in activity patterns between weekdays and weekends?

new_dataset$Weekday<-wday(new_dataset$date, label = TRUE, abbr = FALSE)
new_dataset <- new_dataset %>%
mutate(day_of_week = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

table(new_dataset$Weekend, new_dataset$Weekday)

avg_new_dataset <- aggregate(steps ~ interval + 
							 day_of_week, 
							 data=new_dataset, FUN = "mean")

# Creating a panel plot with the lattice library
xyplot(avg_new_dataset$steps ~ avg_new_dataset$interval | avg_new_dataset$day_of_week, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")



