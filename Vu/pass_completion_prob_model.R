library(dplyr)
library(car)

powerplay_pass <- read.csv("powerplay_pass_data.csv")

# Convert event_successful to binary
powerplay_pass$event_successful <- ifelse(powerplay_pass$event_successful == "t", 1, 0)

# Run logistic regression on event_successful
pass_completion_prob <- glm(event_successful ~ min_dist_from_pl + num_def_near_pl 
                            + as.factor(pass_rushed) 
                            + as.factor(reciever_crowded) + as.factor(pass_cluster),
                            data = powerplay_pass, 
                            family = binomial)

# Model results
summary(pass_completion_prob)

# Check multicollinearity
vif(pass_completion_prob)

