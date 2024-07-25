library(tibble)
library(ggplot2)
library(ggimage)
library(grid)
library(png)
library(jpeg)
library(dplyr)

powerplay_pass <- read.csv("pass_model.csv")

# Data frames for team rank
rank_team <- tibble(
  team_name = c("United States", "Switzerland", "Canada", "Russia", "Finland"),
  team_contribution_above_avg = c(0.805, 0.628, 0.497, 0.438, 0.307),
  #num_passes = c(32, 60, 22),
  image = paste0("team_flag/", c("United States.jpg", "Switzerland.jpg", "Canada.jpg", "Russia.jpg", "Finland.jpg"))
)

# Create the plot
ggplot(rank_team, aes(x = reorder(team_name, team_contribution_above_avg), y = team_contribution_above_avg)) +
  geom_col(fill = "#0284c7") +
  geom_image(aes(image = image), size = 0.08, position = position_dodge(width = 1)) +
  geom_text(aes(label = team_contribution_above_avg), size = 3, position = position_stack(vjust = 0.5), color = "black") +
  geom_hline(yintercept = .535, linetype = "dashed", color = "darkblue") +
  annotate("text", x = Inf, y = .535, label = "Average", hjust = 9, vjust = -0.5, color = "darkblue", angle = 90) +
  coord_flip() +
  labs(x = "Team", y = "Pass Completion Probability", fill = NULL, title = "Average Pass Completion Probability by Team") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  guides(fill = FALSE)





