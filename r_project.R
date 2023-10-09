library(tidyr)
library(dplyr)
install.packages("caret")
library(caret)

library(rpart)
install.packages("randomForest")
library(randomForest)
#library(Boruta)
install.packages("ROCR")
library(ROCR)
install.packages("gbm")
library(gbm)
install.packages("corrplot")
library(corrplot)
library(stringr)



# Read raw data
raw_data <- read.csv("C:/Users/Akshra_/Desktop/specialisation/heart_failure_clinical_records_dataset.csv")
#having missing data
is.na(raw_data)
# Removing missing values
clean_data <- na.omit(raw_data)
#check missing value
is.na(clean_data)
#check for duplicated data
sum(duplicated(clean_data))
#convert target variable to factor,to enable categorical variable analyis ,assign a numericalvalue o each categorical dats
clean_data$DEATH_EVENT <- factor(clean_data$DEATH_EVENT)
#to see summary of data frame
glimpse(clean_data)
#to get statistica summary- mean ,median ,quartile
summary(clean_data)
#to have correlation analysis
# Select only numeric variables for correlation
numeric_data <- clean_data[, sapply(clean_data, is.numeric)]

# Correlation matrix
if (ncol(numeric_data) > 1) { # check if we have at least two numeric variables
  cor_matrix <- cor(numeric_data, method = "pearson")
  print("Correlation Matrix:")
  print(cor_matrix)
  # Visualize correlation matrix
  library(corrplot)
  corrplot(cor_matrix, method = "circle")
} else {
  print("Not enough numeric variables for a correlation matrix.")
}





# Scatterplot Matrix using ggplot2
install.packages("GGally")
library(GGally)
clean_data %>%
  ggpairs(columns = c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_creatinine", "serum_sodium"),
          mapping = ggplot2::aes(color = DEATH_EVENT)) +
  ggplot2::theme_light()

