library(tidyr)
library(dplyr)
#tidyr and dplyr for data manipulation 
install.packages("caret")
#caret for ml 
library(caret)

library(rpart)
install.packages("randomForest")
library(randomForest)
#library(Boruta)
install.packages("ROCR")
library(ROCR)
install.packages("gbm")
library(gbm)
#corrplot for corelation matrix 
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
if (ncol(numeric_data) > 1) { # Check if we have at least two numeric variables
  cor_matrix <- cor(numeric_data, method = "pearson") # Calculate the Pearson correlation matrix for numeric variables
  print("Correlation Matrix:") # Print a message indicating that the following output is the correlation matrix
  print(cor_matrix) # Print the calculated correlation matrix
  
  # Visualize correlation matrix
  library(corrplot) # Load the corrplot package for visualization
  corrplot(cor_matrix, method = "circle") # Visualize the correlation matrix using circular plot method
} else {
  print("Not enough numeric variables for a correlation matrix.") # Print a message if there are not enough numeric variables
}






# Scatterplot Matrix using ggplot2
install.packages("GGally")
library(GGally)
clean_data %>%
  ggpairs(columns = c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_creatinine", "serum_sodium"),
          mapping = ggplot2::aes(color = DEATH_EVENT)) +
  ggplot2::theme_light()


# Age distribution Plots

# Simple Age distribution
clean_data %>%
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 5, 
                 color = "grey", 
                 alpha = 0.5) +
  labs(title = "Age Distribution") +
  scale_x_continuous(breaks = seq(40,100,10))

# Age distribution against Death Event
clean_data %>% 
  ggplot(aes(x = age, fill = DEATH_EVENT)) +
  geom_histogram(binwidth = 5, 
                 position = "identity",
                 alpha = 0.5,color = "white") +
  scale_fill_manual(values = c("#999999", "#1F77B4")) +
  labs(title = "Age Distribution with Death Event")+
  scale_x_continuous(breaks = seq(40,100,10))


# Boxplot of Age by Death Event
ggplot(clean_data, aes(x = DEATH_EVENT, y = age, fill = DEATH_EVENT)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Death Event") +
  scale_fill_brewer(palette = "Set1")


# Boxplot of Age by Sex
clean_data %>%
  ggplot(aes(x = as.factor(sex), y = age)) + geom_boxplot(aes(fill = as.factor(sex))) +
  labs(title = "Boxplot of Age by Sex") + xlab("Sex") + ylab("Age")

