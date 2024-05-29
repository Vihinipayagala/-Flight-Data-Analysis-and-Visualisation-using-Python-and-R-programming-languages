library(tidyverse)
library(lubridate)

## Importing the datasets required for Q3

cleaned_dataset <- read.csv("C:/Users/94772/OneDrive/Documents/PROG CW - final/cleaned_dataset.csv")
airport <- read.csv("C:/Users/94772/OneDrive/Documents/Programming for Data Science coursework/datasets/airports.csv")

## Removing Duplicate Rows from airport data

airport <- airport %>% distinct()

# Total Number of Flights Over Time

## Aggregate the number of flights by origin, destination, year, and month

df <- cleaned_dataset %>% 
  group_by(Origin, Dest, Year, Month) %>% 
  summarise(FlightNum = n()) %>% 
  ungroup()

## Convert the year and month columns to datetime format

df$Date <- ymd(paste(df$Year, df$Month, "01", sep = "-"))

## Aggregate the number of flights by year and month

flightsbymonth <- df %>% 
  group_by(Year, Month) %>% 
  summarise(FlightNum = sum(FlightNum)) %>% 
  ungroup()

## Create a line plot of the total number of flights over time

ggplot(flightsbymonth, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = FlightNum)) +
  geom_line(color = "#8c564b") +
  xlab("Month") +
  ylab("Number of Flights") +
  ggtitle("Total Number of Flights Over Time") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of Flights by Top 5 Airports 

## Merge airport data to get latitute and longitude for each airport

cleaned_dataset <- merge(cleaned_dataset, airport[, c("iata", "lat", "long")], by.x = "Origin", by.y = "iata", all.x = TRUE)

## Group flights by airport and month

flights <- cleaned_dataset %>% group_by(Month, Origin) %>% summarise(NumFlights = n())

## Get top 5 airports by number of flights

top_5_airports <- flights %>% group_by(Origin) %>% summarise(total_flights = sum(NumFlights)) %>% arrange(desc(total_flights)) %>% head(5) %>% pull(Origin)

## Replace numeric month values with month abbreviations for readability

flights$Month <- factor(flights$Month, levels = 1:12, labels = month.abb)

## Plot seasonal trends for each airport

ggplot(data = filter(flights, Origin %in% top_5_airports), aes(x = Month, y = NumFlights, color = Origin, group = Origin)) +
  geom_line(size=1.0) +
  scale_color_manual(values = c("#56B4E9", "#800080", "#009E73", "#FF0000", "#E69F00")) +
  labs(x = "Month", y = "Number of Flights", title = "Seasonal Trends for Top 5 Airports") 