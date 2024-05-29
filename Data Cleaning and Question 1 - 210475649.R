library("readr")
## Load the dataset

dataset_2006 <- read.csv("C:/Users/94772/OneDrive/Documents/Programming for Data Science coursework/datasets/2006.csv.bz2")
dataset_2007 <- read.csv("C:/Users/94772/OneDrive/Documents/Programming for Data Science coursework/datasets/2007.csv.bz2")

## Merge the datasets 

dataset <- rbind(dataset_2006, dataset_2007)


# Clean data
## Check for any missing values 

sum(is.na(dataset))

## Removing Duplicate Rows

dataset <- na.omit(dataset)

## Dropping the Cancellation Code column as majority of values are null

dataset <- dataset[ , ! names(dataset) %in% c("CancellationCode")]

## Correcting the Departure and Arrival times appropriately

dataset <- subset(dataset, ArrTime < 2400)
dataset <- subset(dataset, DepTime < 2400)

## Create a new column Total_Delay by adding ArrDelay and DepDelay

dataset$Total_Delay <- dataset$ArrDelay + dataset$DepDelay

## Saving cleaned data set to be used for other tasks

write.csv(dataset, file = "cleaned_dataset.csv", row.names = FALSE)

cleaned_dataset <- read.csv("C:/Users/94772/OneDrive/Documents/PROG CW - final/cleaned_dataset.csv")

library(data.table)
library(dplyr)
library(ggplot2)

# QUESTION 1

# When is the best time of day to fly 

library(tidyr)

delays <- cleaned_dataset[,c("Year","Month","DayOfWeek","DepTime","ArrTime","ArrDelay","DepDelay", "Total_Delay")]

## Drop rows with missing values

delays <- delays[complete.cases(delays$ArrDelay, delays$DepDelay),]

## Add a new column to the dataframe that categorizes the departure time into time slots

delays$TimeOfDay <- cut(delays$DepTime, breaks = c(0, 600, 1200, 1800, 2400),
                        labels = c("Night", "Morning", "Afternoon", "Evening"), include.lowest = TRUE)

## Group the data by time slot and find the average of ArrDelay, DepDelay, and Total_Delay

time_of_day <- labels
avg_arr_delay <- c()
avg_dep_delay <- c()
avg_total_delay <- c()

for (time in 1:length(time_of_day)) {
  avg_arr_delay <- c(avg_arr_delay, mean(delays[delays$TimeOfDay == time, "ArrDelay"]))
  avg_dep_delay <- c(avg_dep_delay, mean(delays[delays$TimeOfDay == time, "DepDelay"]))
  avg_total_delay <- c(avg_total_delay, mean(delays[delays$TimeOfDay == time, "Total_Delay"]))
}

## Create a table with time slot, average arrival delay, average departure delay, and average delay (from both columns)

delay_dataset <- delays %>% 
  group_by(TimeOfDay) %>%
  summarize(AvgArrDelay = mean(ArrDelay), 
            AvgDepDelay = mean(DepDelay),
            AvgTotalDelay = mean(Total_Delay))

## Plot a bar chart to visualize the average delay by time of day

ggplot(delay_dataset, aes(x = TimeOfDay, y = AvgTotalDelay)) + 
  geom_bar(stat = "identity", fill = "#00bfa5") + 
  xlab("Time of Day") + 
  ylab("Average Delay (min)") + 
  ggtitle("Average Delay by Time of Day")

# When is the best time of the week to fly

## Convert the DayOfWeek column to a string for easier readability

delays$DayOfWeek <- factor(delays$DayOfWeek, levels = c(1:7), labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
delays$DepDelay[is.na(delays$DepDelay)] <- 0
delays$ArrDelay[is.na(delays$ArrDelay)] <- 0
delays$AvgDepDelay <- ave(delays$DepDelay, delays$DayOfWeek, FUN = mean)
delays$AvgArrDelay <- ave(delays$ArrDelay, delays$DayOfWeek, FUN = mean)
delays$AvgTotalDelay <- ave((delays$Total_Delay), delays$DayOfWeek, FUN = mean)

## Create a table with the day of the week, average arrival delay, average departure delay, and average delay

delay_table <- aggregate(cbind(AvgArrDelay, AvgDepDelay, AvgTotalDelay) ~ DayOfWeek, data = delays, FUN = mean)

## Create a DataFrame with all days of the week

days_of_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
all_days <- data.frame(DayOfWeek = days_of_week)

## Merge the all_days DataFrame with the Delay_Table

delay_table <- merge(all_days, delay_table, by = 'DayOfWeek', all.x = TRUE)

## Create a line plot of the average delay for each day of the week

ggplot(delay_table, aes(x = DayOfWeek, y = AvgTotalDelay, group = 1)) +
  geom_line(color = '#00bfa5') +
  labs(x = 'Day of the Week', y = 'Average Delay (mins)', title = 'Average Flight Delays by Day of the Week') +
  scale_x_discrete(limits = days_of_week)


# When is the best time of the year to fly

## Add a new column to the dataframe that combines the year and month columns

delays$YearMonth <- as.Date(paste(delays$Year, delays$Month, sep=""), format="%Y%m")

## Convert the Month column to a string for easier readability

delays$Month <- replace(delays$Month, c(1:12), c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

## Group the data by month and find the average of Total_Delay

month_of_year <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
avg_total_delay <- c()

for (month in month_of_year) {
  avg_total_delay <- c(avg_total_delay, mean(delays[delays$Month == month,]$Total_Delay))
}

## Create a table with month AND average delay

delay_df <- data.frame(Month = month_of_year, AvgTotalDelay = avg_total_delay)
print(delay_df)

## Plot a line chart to visualize the average delay by month

ggplot(delay_df, aes(x=Month, y=AvgTotalDelay, group= 1)) +
  geom_line(color = '#00bfa5') +
  geom_point(color = '#00bfa5') +
  scale_x_discrete(limits=month_of_year) +
  labs(x='Month of Year', y='Average Delay (min)', title='Average Delay by Month of Year') 