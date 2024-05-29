library(ggplot2)
library(dplyr)

## Importing the datasets required for Q4

cleaned_dataset <- read.csv("C:/Users/94772/OneDrive/Documents/PROG CW - final/cleaned_dataset.csv")

## Filter out cancelled flights

q4 <- cleaned_dataset[cleaned_dataset$Cancelled == 0,]

q4$Total_Delay <- ifelse(is.na(q4$Total_Delay), 0, q4$Total_Delay)

q4 <- q4[complete.cases(q4$TailNum, q4$CRSDepTime),]

## Filter out flights with missing scheduled departure times and tail number

q4 <- q4[order(q4$TailNum, q4$CRSDepTime),]

## Find the previous airport delay for each plane

q4 <- q4 %>% 
  group_by(TailNum) %>% 
  mutate(PrevAirportDelay = lag(Total_Delay, default = 0)) %>% 
  ungroup() %>% 
  na.omit()

## Create a scatter plot

ggplot(q4, aes(x = Total_Delay, y = PrevAirportDelay)) + 
  geom_point(color ="#9467bd") + 
  xlab('Total Delay in Current Airport (minutes)') + 
  ylab('Delay in Previous Airport (minutes)') + 
  ggtitle('Delays in Current and Previous Airports')


library(stats)

## Calculate the correlation coefficient and p-value

cor_test <- cor.test(q4$Total_Delay, q4$PrevAirportDelay)

corr <- cor_test$estimate
p_val <- cor_test$p.value

print(paste0('Correlation Coefficient: ', corr))
print(paste0('P-value: ', p_val))

if (p_val < 0.05) {
  print('The correlation coefficient is statistically significant.')
} else {
  print('The correlation coefficient is not statistically significant.')
}

print("Defining the hypothesis")
print("H0: There is no correlation between the delays in the current and previous airports") 
print("H1: There is a correlation between the delays in the current and previous airports.")

## Set the significance level
alpha <- 0.05

## Calculate the degrees of freedom
n <- nrow(q4)
df <- n - 2

## Calculate the t-statistic
t <- corr * sqrt(df) / sqrt(1 - corr^2)

## Calculate the critical value
critical_value <- qt(alpha/2, df)

## Check if the t-statistic is greater than the critical value
if (abs(t) > critical_value) {
  print("Reject the null hypothesis - There is a significant correlation between the delays in the current and previous airports.")
} else {
  print("Fail to reject the null hypothesis - There is no significant correlation between the delays in the current and previous airports.")
}
