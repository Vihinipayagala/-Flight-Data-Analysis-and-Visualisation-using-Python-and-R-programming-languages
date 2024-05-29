library(tidyverse)

## Importing the datasets required for Q2

planes <- read_csv("C:/Users/94772/OneDrive/Documents/Programming for Data Science coursework/datasets/plane-data.csv") 
cleaned_dataset <- read_csv("C:/Users/94772/OneDrive/Documents/PROG CW - final/cleaned_dataset.csv")

## select 'tailnum' and 'year' columns from the planes dataframe

planes <- planes[, c('tailnum', 'year')]
planes$year <- as.numeric(planes$year)

## Removing rows with NA values

planes <- na.omit(planes)

## Changing 'tailnum' to 'TailNum'

colnames(planes)[1] <- "TailNum"

## Merge the plane data with the cleaned dataset

merged_df <- merge(cleaned_dataset, planes, by = "TailNum", all = FALSE)

## Changing 'year' to 'YearOfManufacture'

colnames(merged_df)[which(names(merged_df) == "year")] <- "YearOfManufacture"
merged_df$YearOfManufacture <- as.integer(merged_df$YearOfManufacture)

## Calculate the age of each plane and add a new column to the dataframe

merged_df$plane_age <- merged_df$Year - merged_df$YearOfManufacture

## creating a new dataframe

planedata <- data.frame(Year = merged_df$Year, 
                        YearOfManufacture = merged_df$YearOfManufacture, 
                        plane_age = merged_df$plane_age, 
                        Total_Delay = merged_df$Total_Delay)

## Group the data by plane age and find the average of Total_Delay

bins <- c(0, 10, 20, 30, 40, 50, 60)
labels <- c('0-10', '10-20', '20-30', '30-40', '40-50', '50-60')
planedata$age_range <- cut(planedata$plane_age, breaks = bins, labels = labels, include.lowest = TRUE)

## Create a table with plane age and average delay

delay_df <- planedata %>%
  group_by(age_range) %>%
  summarize(Avgtotal_delay = mean(Total_Delay, na.rm = TRUE)) %>%
  na.omit() %>% # remove any rows with missing values
  ungroup()

library(dplyr)
library(ggplot2)

## Plot a bar chart to visualize the average delay by plane age

ggplot(delay_df, aes(x = age_range, y = Avgtotal_delay)) +
  geom_bar(stat = "identity", fill = "#87ceeb") +
  xlab("Age of Plane (years)") +
  ylab("Average Delay (minutes)") +
  ggtitle("Average Delay by Age of Plane")

## Regplot of Average Delay by Plane Age

agegrouped <- planedata %>%
  group_by(plane_age) %>%
  summarise(Avgtotal_delay = mean(Total_Delay)) %>%
  
## removing values of plane_age such as -1,2006,2007
  
  filter(!plane_age %in% c(-1, 2006, 2007))

## Regplot of Average Delay by Plane Age

ggplot(agegrouped, aes(x = plane_age, y = Avgtotal_delay)) +
  geom_point(stat = "identity", 
             fill = "#87ceeb") +
  geom_smooth(method = "lm", 
              se = TRUE)  +
  xlab("Age of Plane (years)") +
  ylab("Average Delay (minutes)") +
  ggtitle("Plane Age vs Average Delay")

## Check for correlation between plane age and Average total delay

cor(agegrouped$plane_age, agegrouped$Avgtotal_delay)