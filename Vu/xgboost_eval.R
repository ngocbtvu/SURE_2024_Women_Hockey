library(xgboost)
library(caret)
library(dplyr)
library(ROSE)

# Load data
powerplay_pass <- read.csv("pass_model.csv")

# Convert categorical variables to factors
powerplay_pass$pass_cluster <- as.factor(powerplay_pass$pass_cluster)

set.seed(1711)

# Create balanced dataset
resampled_data <- ovun.sample(event_successful ~ min_dist_from_pl + num_def_near_pl + 
                               pass_rushed + reciever_crowded + pass_distance + pass_cluster,
                             data = powerplay_pass, method = "both", p = 0.5, 
                             N = nrow(powerplay_pass))$data

# Split data into training and test sets
set.seed(1711)
train_index <- createDataPartition(resampled_data$event_successful, p = 0.7, list = FALSE)
train_data <- resampled_data[train_index, ]
test_data <- resampled_data[-train_index, ]
# Convert categorical variables to numeric factors
train_data$pass_cluster <- as.numeric(as.factor(train_data$pass_cluster))
test_data$pass_cluster <- as.numeric(as.factor(test_data$pass_cluster))

# Create numeric matrix for XGBoost
train_matrix <- as.matrix(train_data %>%
                            select(min_dist_from_pl, num_def_near_pl, pass_rushed, reciever_crowded, pass_distance, pass_cluster))
train_label <- as.numeric(train_data$event_successful) 

test_matrix <- as.matrix(test_data %>%
                           select(min_dist_from_pl, num_def_near_pl, pass_rushed, reciever_crowded, pass_distance, pass_cluster))
test_label <- as.numeric(test_data$event_successful) 

# Create XGBoost DMatrix
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix)

# Set XGBoost parameters and train model
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.3,
  max_depth = 6,
  gamma = 1
)

num_rounds <- 100
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = num_rounds)

# Make predictions
predictions <- predict(xgb_model, dtest)

# Evaluate the model by ROC and AUC
library(pROC)
roc_curve <- roc(test_label, predictions)
plot(roc_curve)
auc(roc_curve)

pred_labels <- ifelse(predictions > 0.5, 1, 0)

# Create confusion matrix
confusion_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(test_label))
print(confusion_matrix)
