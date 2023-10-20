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
# ****************************data preproccessing*********************

# Read raw data
raw_data <- read.csv("C:/Users/Akshra_/Desktop/specialisation/heart_failure_clinical_records_dataset.csv")
print(raw_data)
#having missing data

is.na(raw_data)
# Removing missing values
clean_data <- na.omit(raw_data)
#check missing value
is.na(clean_data)
#check for duplicated data
sum(duplicated(clean_data))
#convert target variable to factor,to enable categorical variable analyis ,assign a numericalvalue to each categorical dats
clean_data$DEATH_EVENT <- factor(clean_data$DEATH_EVENT)
#to see summary of data frame,rows ,colums,data types
glimpse(clean_data)
#to get statistical summary- mean ,median ,quartile
summary(clean_data)
#to check outliers
# Creating a box plot for the 'age' variable
boxplot(clean_data$age, main = "Age Box Plot", ylab = "Age", col="pink")

#to have correlation analysis
# Select only numeric variables for correlation
numeric_data <- clean_data[, sapply(clean_data, is.numeric)]
#sapply(clean_data, is.numeric) applies the is.numeric function to each column of clean_data
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
#how to read scatter plot - find out



#  each pair of variables in the dataset is represented by a scatter plot, allowing you to visually explore the relationships and correlations between multiple variables simultaneously.
# Age distribution Plots

# Simple Age distribution
# Create a histogram for the 'age' variable
hist(clean_data$age, 
     breaks = seq(40, 100, 5),  # Define the breaks for the bins
     col = "lightblue",              # Set color of bars to grey
     border = "black",           # Set color of bar borders to black
     main = "Age Distribution",  # Set the main title of the plot
     xlab = "Age in years",               # Set the x-axis label
     ylab = "Frequency",         # Set the y-axis label
     xlim = c(40, 100),          # Set the x-axis limits
     ylim = c(0, 50),            # Set the y-axis limits
     freq = TRUE                 # Plot frequencies on the y-axis
)


# Age distribution against Death Event
# Define breaks and colors
breaks <- seq(40, 100, 5)
colors <- c("pink", "lightblue")

# Create a histogram for the 'age' variable based on DEATH_EVENT
hist(clean_data$age[clean_data$DEATH_EVENT == 0],  # Subset for DEATH_EVENT = 0
     breaks = breaks,
     col = colors[1],  # Color for DEATH_EVENT = 0
     border = "white",
     main = "Age Distribution with Death Event", 
     xlab = "Age in years ", 
     ylab = "Frequency", 
     xlim = c(40, 100), 
     ylim = c(0, 50),
     freq = TRUE)

# Add histogram for DEATH_EVENT = 1
hist(clean_data$age[clean_data$DEATH_EVENT == 1],  # Subset for DEATH_EVENT = 1
     breaks = breaks,
     col = colors[2],  # Color for DEATH_EVENT = 1
     add = TRUE,  # Add to the existing plot
     freq = TRUE)

# Add legend
legend("topright", legend = c("No Death Event", "Death Event"), fill = colors, title = "DEATH_EVENT")


# Boxplot of Age by Death Event
# Create a box plot for 'age' grouped by 'DEATH_EVENT'
boxplot(age ~ DEATH_EVENT, 
        data = clean_data, 
        col = c("pink", "#377EB8"),  # Colors for the box plots
        main = "Box Plot of Age by Death Event",
        xlab = "DEATH_EVENT",
        ylab = "Age in years")

# Adding legend
legend("topright", 
       legend = c("No Death Event", "Death Event"), 
       fill = c("pink", "#377EB8"))

# Boxplot of Age by Sex
clean_data %>%
  ggplot(aes(x = as.factor(sex), y = age)) +
  geom_boxplot(aes(fill = as.factor(sex)), color = "black", width = 0.7) +  # Set color to black and adjust width if needed
  scale_fill_manual(values = c("lightblue", "pink")) +  # Set fill colors
  labs(title = "Boxplot of Age by Sex") +
  xlab("Sex") +
  ylab("Age in years")

#ml part 
set.seed(123)
#to generate the same random number in every trial
train_index <- createDataPartition(clean_data$DEATH_EVENT, p = 0.8, list = FALSE)
#80% for training 20 for testing
train_data <- clean_data[train_index, ]
#using caret
test_data <- clean_data[-train_index, ]
#-train_index gives testing data
head(train_data)


#rpart for decision tree
install.packages("rpart.plot")

library(rpart.plot)
# Build a decision tree using the training data
tree_model <- rpart(DEATH_EVENT ~ ., data = train_data, method = "class")
# Visualize the decision tree
rpart.plot(tree_model, main = "Decision Tree for Heart Failure", box.palette = "RdBu")

# Make predictions on the testing set
pred <- predict(tree_model, newdata = test_data, type = "class")
print(pred)
# Evaluate the performance of the model
confusionMatrix(pred, test_data$DEATH_EVENT)
#to create one ffor specific variables only 

tree_feat <- rpart(DEATH_EVENT ~ age +
                     ejection_fraction +
                     serum_creatinine +
                     time, 
                   data = train_data, 
                   method = "class")

# Prune the decision tree to reduce overfitting
model.pruned <- prune(tree_feat, cp = 0.01)
# Plot the pruned decision tree
prp(model.pruned, extra = 101, box.col = "pink", branch.lty = 3)
# Make predictions on the testing set
predictions <- predict(model.pruned, newdata = test_data, type = "class")

confusionMatrix(predictions, test_data$DEATH_EVENT)


acc_dt <- confusionMatrix(pred, as.factor(test_data$DEATH_EVENT))$overall["Accuracy"]
tpr_dt <- confusionMatrix(pred, as.factor(test_data$DEATH_EVENT))$byClass["Specificity"]
#random forest- 

# Train the random forest
rf_model <- randomForest(DEATH_EVENT ~ ., data = train_data, ntree = 500, mtry = 3)


# Make predictions on the testing set
pred <- predict(rf_model, newdata = test_data)

# Evaluate the performance of the model
confusionMatrix(pred, test_data$DEATH_EVENT)

acc_rf <- confusionMatrix(pred, as.factor(test_data$DEATH_EVENT))$overall["Accuracy"]
tpr_rf <- confusionMatrix(pred, as.factor(test_data$DEATH_EVENT))$byClass["Specificity"]

# Create variable importance plot
varImpPlot(rf_model, type = 2, main = "Variable Importance Plot for Random Forest")

plot(rf_model)

# Train the random forest
rf_model <- randomForest(DEATH_EVENT ~ time + 
                            
                           serum_creatinine + 
                            +
                           creatinine_phosphokinase, 
                         data = train_data, 
                         ntree = 250, 
                         mtry = 2)


# Make predictions on the testing set
pred <- predict(rf_model, newdata = test_data)

# Evaluate the performance of the model
confusionMatrix(pred, test_data$DEATH_EVENT)

#logistic regression- 

# Fit the logistic regression model
glm_model <- caret::train(DEATH_EVENT ~ ., 
                          data = train_data, 
                          method = "glm", 
                          trControl = trainControl(method = "cv", number = 10))

pred <- predict(glm_model, newdata = test_data)
confusionMatrix(pred, test_data$DEATH_EVENT)
acc_glm <- confusionMatrix(pred, as.factor(test_data$DEATH_EVENT))$overall["Accuracy"]
tpr_glm <- confusionMatrix(pred, as.factor(test_data$DEATH_EVENT))$byClass["Specificity"]
# Predict the outcome probabilities for the test set
pred <- predict(glm_model, newdata = test_data, type = "prob")

# Create the prediction object for performance analysis
pred_obj <- prediction(pred[, 2], test_data$DEATH_EVENT)

# Calculate TPR and FPR values at various thresholds
perf_obj <- ROCR::performance(pred_obj, measure = "tpr", x.measure = "fpr")

# Plot the ROC curve
plot(perf_obj, main = "ROC Curve for Logistic Regression Model", 
     xlab = "False Positive Rate", ylab = "True Positive Rate", colorize=T, lwd = 2)

# Add a diagonal reference line
abline(a = 0, b = 1, lwd = 1.5, col = "gray")

# Calculate the area under the ROC curve
auc <- ROCR::performance(pred_obj, "auc")
auc <- auc@y.values[[1]]

# Add the AUC value to the plot
legend("bottomright", paste("AUC = ", round(auc, 3)), bty = "n", cex = 0.8)






data.frame(algorithm = c("logistic\nregression", "decision\ntree", "random\nforest"),
           accuracy = c(acc_glm, acc_dt, acc_rf)*100,
           recall = c(tpr_glm, tpr_dt, tpr_rf)*100) %>%
  pivot_longer(col = -algorithm, names_to = "metrics", values_to = "percent") %>%
  ggplot(aes(x = reorder(algorithm, X = percent),
             y = percent,
             fill = metrics)) +
  geom_bar(stat = "identity",
           position = "dodge",
           alpha=0.9) +
  geom_text(aes(group = metrics, label = str_c(sprintf("%2.1f", percent), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.2) +
  scale_fill_manual(values = c("#1F77B4", "pink")) +
  labs(x = "algorithm", title = "Metrics of different classifier models") +
  theme_minimal(base_size = 12)
