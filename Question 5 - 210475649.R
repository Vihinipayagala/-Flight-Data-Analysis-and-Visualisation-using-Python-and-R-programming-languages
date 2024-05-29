# read csv file

cleaned_dataset <- read.csv("C:/Users/94772/OneDrive/Documents/PROG CW - final/cleaned_dataset.csv")

str(cleaned_dataset)
str(cleaned_dataset)

sapply(cleaned_dataset, function(x) length(unique(x)))


## check null values

colSums(is.na(cleaned_dataset))

dim(cleaned_dataset)

library(dplyr)
library(ggcorrplot)
library(RColorBrewer)
library(caret)
library(pheatmap)     

## Drop unnecessary columns

q5 <- select(cleaned_dataset,-Cancelled,-Diverted,-TailNum,-Origin,-Dest,-UniqueCarrier,-Total_Delay,-Year,-DayofMonth,-Distance,-FlightNum)

str(q5)
dim(q5)

## calculate the correlation matrix of the dataframe

cor_matrix <- cor(q5)

## find the highly correlated columns using the findCorrelation() function

highly_correlated_cols <- findCorrelation(cor_matrix, cutoff = 0.8)

## get the names of the highly correlated columns

highly_correlated_colnames <- names(q5[, highly_correlated_cols])


## print the highly correlated column names

highly_correlated_colnames

## remove columns

q5 <- select(q5,-ActualElapsedTime,-CRSElapsedTime,-AirTime,-DepTime)

str(q5)

dim(q5)



## calculate the correlation matrix of the dataframe
cor_matrix2 <- cor(q5)

## create a heatmap of the correlation matrix using the heatmap() function


ggcorrplot(corr = cor_matrix2, colors = (brewer.pal(3, "GnBu")), lab = TRUE, lab_size = 2)


## Install magrittr package 

install.packages("magrittr")

## Load magrittr package

library(magrittr) 

q5$ArrDelay <- ifelse(q5$ArrDelay < 0, 0, 1)

q5$ArrDelay

str(q5)

## remove columns

q5 <- select(q5,-ArrTime,-TaxiIn,-SecurityDelay)

## Split the data into training and testing sets

set.seed(123)
trainIndex <- createDataPartition(q5$ArrDelay, p = 0.8, list = FALSE)
X_train <- q5[trainIndex,]

## Remove target columns

X_train <- select(X_train, -ArrDelay)

X_test <- q5[-trainIndex,]
X_test <- select(X_test, -ArrDelay)

y_train <- q5[trainIndex, "ArrDelay"]
y_test <- q5[-trainIndex, "ArrDelay"]

y_train
y_test




## standerlize the numeric columns

X_train <- X_train %>%
  mutate_if(is.numeric, scale)

X_test <- X_test %>%
  mutate_if(is.numeric, scale)


## check null values

colSums(is.na(X_train))

## Train the model

log_reg_model <- glm(y_train ~ ., data = X_train, family = binomial())

## Print summary of the model

summary(log_reg_model)

## Make predictions on the test set

predictions <- predict(log_reg_model, newdata = X_test,type="response")

predictions

labels <- ifelse(predictions > 0.5, 1, 0)

## evaluate the model performance

accuracy <- mean(labels == y_test)
accuracy

confusion_matrix <- table(Actual = y_test, Predicted = labels)
confusion_matrix

classification_report <- caret::confusionMatrix(confusion_matrix)
classification_report


## Plot confusion matrix

pheatmap(confusion_matrix, display_numbers = T)

## load necessary packages

library(pROC)

## define object to plot and calculate AUC

rocobj <- roc(y_test, predictions)
auc <- round(auc(y_test, predictions),4)

## create ROC plot

ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))