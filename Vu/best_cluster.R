library(dplyr)
powerplay_pass <- read.csv("powerplay_pass_data.csv")

# Convert event_successful to binary
powerplay_pass$event_successful <- ifelse(powerplay_pass$event_successful == "t", 1, 0)

# Run logistic regression on event_successful
pass_completion_prob <- glm(event_successful ~ min_dist_from_pl + num_def_near_pl + pass_rushed 
                                          + reciever_crowded + pass_cluster,
                      data = powerplay_pass, 
                      family = binomial)

# Predict success probabilities for each pass cluster
powerplay_pass$success_prob <- predict(pass_completion_prob, type = "response")

# Calculate mean success probability by pass cluster
mean_probs <- powerplay_pass %>%
  group_by(pass_cluster) %>%
  summarize(mean_prob = mean(success_prob))

# Highest and lowest mean success probabilities
most_successful <- mean_probs[which.max(mean_probs$mean_prob), ]
most_successful
least_successful <- mean_probs[which.min(mean_probs$mean_prob), ]
least_successful
