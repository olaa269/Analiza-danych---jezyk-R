# load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(ggcorrplot)
library(ggplot2)
library(randomForest)


# load the data
library(readr)
heart_data <- read_csv("heart_attack_prediction_jap_dataset.csv")


# view the data
view(heart_data)

# view 10 rows
heart_data %>% slice_head(n = 10)

#basic details for data
str(heart_data)
summary (heart_data)

# check for missing values
total_na <- sum(is.na(heart_data))
print(paste("Total NA values:", total_na))

# check for duplicates
total_duplicates <- sum(duplicated(heart_data))
print(paste("Total duplicates:", total_duplicates))

# identify categorical columns
cat_cols <- names(heart_data)[sapply(heart_data, function(x) is.factor(x) | is.character(x))]
unique_values <- lapply(heart_data[cat_cols], unique)

# print result
unique_values

# identify categorical columns
cat_cols <- names(heart_data)[sapply(heart_data, function(x) is.factor(x) | is.character(x))]
unique_values <- lapply(heart_data[cat_cols], unique)

# check negative figures in numeric columns 
num_cols <- sapply(heart_data, is.numeric)
negative_values <- colSums(heart_data[, num_cols] < 0)
print(negative_values)

#check letters in numeric columns
letter_check <- sapply(heart_data[, num_cols], function(col) any(grepl("[a-zA-Z]", col)))
print(letter_check)

# delete rows letters in numeric columns
heart_data <- heart_data %>% mutate_if(is.numeric, as.numeric)


#round figures in 2 decimals
heart_data <- heart_data %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

#view data
view(heart_data)

# cholesterol level distribution chart 
ggplot(heart_data, aes(x = Cholesterol_Level)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Cholesterol distribution", x = "Cholesterol level", y = "People number") +
  theme_minimal() 


# relationship between age and cholesterol
ggplot(heart_data, aes(x = Age, y = Cholesterol_Level)) +
  geom_point(color = "green") +
  labs(title = "Relationship between age and cholesterol levels", x = "Age", y = "Cholesterol") +
  theme_minimal()

# relationship between alcohol, age and cholesterol levels
ggplot(heart_data, aes(x = Age, y = Cholesterol_Level, color = Alcohol_Consumption)) +
  geom_point() +
  labs(title = "Alcohol, age and cholesterol levels", x = "Age", y = "Cholesterol level") +
  theme_minimal()

# boxplot for stress level and age in relation to heart attack
ggplot(heart_data, aes(x = Stress_Levels, y = Age, fill = Heart_Attack_Occurrence)) +
  geom_boxplot() +
  labs(title = "Stress level and age in relation to heart attack", x = "Poziom stresu", y = "Wiek") +
  theme_minimal()

# boxplot for smoking history in relation to age and heart attack
ggplot(heart_data, aes(x = Smoking_History, y = Age, fill = Heart_Attack_Occurrence)) +
  geom_boxplot() +
  labs(title = "Smoking history in relation to age and heart attack", x = "Smoking History", y = "Age") +
  theme_minimal()

#bar chart how many people had a heart attack
heart_data %>%
  count(Heart_Attack_Occurrence) %>%
  ggplot(aes(x = Heart_Attack_Occurrence, y = n, fill = Heart_Attack_Occurrence)) +
  geom_col() +
  theme_minimal()

# pie chart how many people had a heart attack
heart_data %>%
  count(Heart_Attack_Occurrence) %>%
  ggplot(aes(x = "", y = n, fill = Heart_Attack_Occurrence)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom")

# relationship between age and the occurrence of a heart attack
ggplot(heart_data, aes(x = Age, fill = Heart_Attack_Occurrence)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Relationship between age and the occurrence of a heart attack", x = "Age", y = "Number of people") +
  theme_minimal()

# density plot for age by heart attack occurrence
ggplot(heart_data, aes(x = Age, color = as.factor(Heart_Attack_Occurrence))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Age by Heart Attack Occurrence",
       x = "Age",
       y = "Density",
       color = "Heart Attack Occurrence") +
  theme_minimal()

library(dplyr)
heart_data <- heart_data %>% filter(between(Cholesterol_Level, quantile(Cholesterol_Level, 0.05), quantile(Cholesterol_Level, 0.95)))


# unique values for each categorical column
unique_values

# convert the columns that have "Yes or No" to 1 or 0
# Heart_Attack_Occurence is the target variable, so this is converted to a factor
heart_data$Smoking_History <- ifelse(heart_data$Smoking_History == 'Yes', 1, 0)
heart_data$Diabetes_History <- ifelse(heart_data$Diabetes_History == 'Yes', 1, 0)
heart_data$Hypertension_History <- ifelse(heart_data$Hypertension_History == 'Yes', 1, 0)
heart_data$Family_History <- ifelse(heart_data$Family_History == 'Yes', 1, 0)
heart_data$Heart_Attack_Occurrence <- as.factor(ifelse(heart_data$Heart_Attack_Occurrence == 'Yes', 1, 0))

# convert other variables
# Physical_Activity 
# physical activity is converted to 1, 0.5, 0
# High = 1, Moderate = 0.2, Low = 0

heart_data <- heart_data %>%
  mutate(Physical_Activity = case_when(
    Physical_Activity == "High" ~ 1,
    Physical_Activity == "Moderate" ~ 0.5,
    Physical_Activity == "Low" ~ 0
  ))

# view physical activity
heart_data$Physical_Activity


# Diet_Quality 
# Good = 1, Average = 0.5, Poor = 0
heart_data <- heart_data %>%
  mutate(Diet_Quality = case_when(
    Diet_Quality == "Good" ~ 1,
    Diet_Quality == "Average" ~ 0.5,
    Diet_Quality == "Poor" ~ 0
  ))


# Alcohol_Consumption
heart_data <- heart_data %>%
  mutate(Alcohol_Consumption = case_when(
    Alcohol_Consumption == "None" ~ 0,
    Alcohol_Consumption == "High" ~ 1,
    Alcohol_Consumption == "Moderate" ~ 0.4,
    Alcohol_Consumption == "Low" ~ 0.1
  ))

# Gender
heart_data$Gender <- ifelse(heart_data$Gender == "Male", 1, 0)

# Region
heart_data$Region <- ifelse(heart_data$Region == "Urban", 1, 0)

# view the data
head(heart_data)

#delete columns with name Extra_Columns
heart_data <- heart_data %>%
select(-Extra_Column_1, -Extra_Column_2, -Extra_Column_3, -Extra_Column_4, -Extra_Column_5, -Extra_Column_6, -Extra_Column_7, -Extra_Column_8, -Extra_Column_9, -Extra_Column_10, -Extra_Column_11, -Extra_Column_12, -Extra_Column_13,, -Extra_Column_14, -Extra_Column_15)
head(heart_data)

# load libraries
library(caret)
library(ROSE)
library(randomForest)
library(xgboost)

# split the data into training (80%) and testing sets (20%)
set.seed(123)
trainIndex <- createDataPartition(heart_data$Heart_Attack_Occurrence, p = 0.8, list = FALSE)
train_data <- heart_data[trainIndex, ]
test_data <- heart_data[-trainIndex, ]

# balance the training set using oversampling + undersampling 
balanced_data <- ovun.sample(Heart_Attack_Occurrence ~ ., data = train_data, method = "both", p = 0.5)$data

# train a Random Forest model with wagered classes
set.seed(123)
model_rf <- randomForest(Heart_Attack_Occurrence ~ ., data = balanced_data, ntree = 100, mtry = 3, classwt = c(1, 5))

# Random Forest Model Evaluation
predictions_rf <- predict(model_rf, test_data)
conf_matrix_rf <- confusionMatrix(predictions_rf, test_data$Heart_Attack_Occurrence, mode = "everything")
print(conf_matrix_rf)

# convert the data to numeric for XGBoost
balanced_data <- balanced_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

test_data <- test_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# convert the target variable to 0 and 1
balanced_data$Heart_Attack_Occurrence <- balanced_data$Heart_Attack_Occurrence - 1
test_data$Heart_Attack_Occurrence <- test_data$Heart_Attack_Occurrence - 1

# create the DMatrix for XGBoost model
train_matrix <- xgb.DMatrix(
  data = as.matrix(balanced_data[, -which(names(balanced_data) == "Heart_Attack_Occurrence")]), 
  label = as.numeric(balanced_data$Heart_Attack_Occurrence)
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(test_data[, -which(names(test_data) == "Heart_Attack_Occurrence")]), 
  label = as.numeric(test_data$Heart_Attack_Occurrence)
)

# define the parameters for XGBoost
param <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1
)

# train the XGBoost model
model_xgb <- xgboost(data = train_matrix, params = param, nrounds = 200, verbose = 0)

# predict the test data using the XGBoost model
predictions_xgb <- predict(model_xgb, test_matrix)
predictions_xgb <- ifelse(predictions_xgb > 0.5, 1, 0)

# evaluate the XGBoost model
conf_matrix_xgb <- confusionMatrix(factor(predictions_xgb), factor(test_data$Heart_Attack_Occurrence), mode = "everything")
print(conf_matrix_xgb)

# print the F1-score for both models
tp <- conf_matrix_xgb$table[2, 2]  # True Positives
fp <- conf_matrix_xgb$table[1, 2]  # False Positives
fn <- conf_matrix_xgb$table[2, 1]  # False Negatives

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
F1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-score (XGBoost):", round(F1_score, 2)))

# accuracy for both models
accuracy_rf <- conf_matrix_rf$overall['Accuracy']
accuracy_xgb <- conf_matrix_xgb$overall['Accuracy']

print(paste("Dokładność modelu Random Forest:", round(accuracy_rf * 100, 2), "%"))
print(paste("Dokładność modelu XGBoost:", round(accuracy_xgb * 100, 2), "%"))


