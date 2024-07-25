library(caret)
library(dplyr)
library(ROSE)
library(xgboost)
library(ggplot2)

powerplay_pass <- read.csv("powerplay_pass_data.csv")
set.seed(1711)

# Convert categorical variables to factors
powerplay_pass$pass_cluster <- as.factor(powerplay_pass$pass_cluster)

# Resample dataset: oversampling minority class and undersampling majority class
resampled_data <- ovun.sample(event_successful ~ min_dist_from_pl + num_def_near_pl + 
                                pass_distance + pass_cluster,
                              data = powerplay_pass, method = "both", p = 0.5, 
                              N = nrow(powerplay_pass))$data

# Define predictors and response
predictors <- resampled_data %>%
  select(min_dist_from_pl, num_def_near_pl, pass_distance, pass_cluster)
response <- resampled_data$event_successful

# Convert factors to numeric
predictors <- data.frame(sapply(predictors, as.numeric))
predictors_matrix <- as.matrix(predictors)
response_vector <- as.numeric(response)

# Create XGBoost DMatrix
dtrain <- xgb.DMatrix(data = predictors_matrix, label = response_vector)

# Set Parameters for XGBoost
params <- list(
  objective = "binary:logistic",   
  eval_metric = "logloss",         
  eta = 0.3,                       
  max_depth = 6,                   
  gamma = 1                       
)

# Perform Cross-Validation
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 50,
  nfold = 5,                      
  stratified = TRUE,             
  verbose = 1,                    
  early_stopping_rounds = 10,    
  metrics = list("logloss")      
)

# Train the model using the best iteration
xgboost <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = cv_results$best_iteration
)

# Make Predictions
predictions <- predict(xgboost, predictors_matrix)
powerplay_pass$min_dist_from_pl_def_percent <- predictions

# Save results to CSV
write.csv(powerplay_pass, "pass_model.csv", row.names = FALSE)

# Extract log loss values from model result
log_loss_df <- data.frame(
  Iteration = 1:nrow(cv_results$evaluation_log),
  Train_Log_Loss = cv_results$evaluation_log$train_logloss_mean,
  Test_Log_Loss = cv_results$evaluation_log$test_logloss_mean
)

# Plot the log loss
ggplot(log_loss_df, aes(x = Iteration)) +
  geom_line(aes(y = Train_Log_Loss, color = "Train Log Loss")) +
  geom_line(aes(y = Test_Log_Loss, color = "Test Log Loss")) +
  labs(title = "XGBoost Cross-Validation Log Loss",
       x = "Iteration",
       y = "Log Loss",
       color = "Legend") +
  theme_minimal() +
  theme(legend.title = element_blank())

