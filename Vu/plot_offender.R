library(tibble)
library(ggplot2)
library(ggimage)
library(grid)
library(png)
library(jpeg)
library(dplyr)

powerplay_pass <- read.csv("pass_model.csv")

# Data frames for top and bottom 5 offenders
top_5_offenders <- tibble(
  min_dist_from_pl_name = c("Blayre Turnbull", "Emily Clark", "Susanna Tapani", "Sarah Nurse", "Polina Luchnikova"),
  total_contribution_above_avg = c(0.926, 0.880, 0.769, 0.733, 0.721),
  #num_passes = c(10, 5, 6, 7, 7),
  image = paste0("player_image/", c("Blayre Turnbull.jpg", "Emily Clark.jpg", "Sarah Nurse.jpg", "Blayre Turnbull.jpg", "Polina Luchnikova.jpg"))
)

bot_5_offenders <- tibble(
  min_dist_from_pl_name = c("Noemi Ryhner", "Emma Maltais", "Sarah Forster", "Alina Muller", "Oxana Bratishcheva"),
  total_contribution_above_avg = c(0.213, 0.270, 0.335, 0.355, 0.470),
  #num_passes = c(8, 11, 8, 23, 22),
  image = paste0("player_image/", c("Noemi Ryhner.jpeg", "Emma Maltais.jpg", "Sarah Forster.jpg", "Alina Muller.jpg", "Oxana Bratishcheva.jpg"))
)

# Combine the top and bottom offenders for plotting
defender_data <- bind_rows(
  mutate(top_5_offenders, group = "Top 5"),
  mutate(bot_5_offenders, group = "Bottom 5")
)

# Create the plot
ggplot(defender_data, aes(x = reorder(min_dist_from_pl_name, total_contribution_above_avg), y = total_contribution_above_avg, fill = group)) +
  geom_col() +
  geom_image(aes(image = image), size = 0.08, position = position_dodge(width = 1)) +
  geom_text(aes(label = total_contribution_above_avg), position = position_stack(vjust = 0.5), color = "black") +
  geom_hline(yintercept = 0.523, linetype = "dashed", color = "darkblue") +
  annotate("text", x = Inf, y = 0.523, label = "Average", hjust = 9, vjust = -0.5, color = "darkblue", angle = 90) +
  coord_flip() +
  labs(x = "Passer", y = "Pass Completion Probabiliy", fill = NULL, title = "Defensive Team by Expected Pass Completion Probability") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  guides(fill = FALSE)


