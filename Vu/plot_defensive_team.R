library(tibble)
library(ggplot2)
library(ggimage)
library(grid)
library(png)
library(jpeg)
library(dplyr)

powerplay_pass <- read.csv("pass_model.csv")

# Data frames for team rank
top_team <- tibble(
  team_name = c("Canada", "Switzerland", "Russia"),
  team_contribution_above_avg = c(0.504, 0.069, 0.062),
  num_passes = c(32, 60, 22),
  image = paste0("team_flag/", c("Canada.jpg", "Switzerland.jpg", "Russia.jpg"))
)

bot_team <- tibble(
  team_name = c("United States", "Finland"),
  team_contribution_above_avg = c(-0.208, -0.428),
  num_passes = c(38, 66),
  image = paste0("team_flag/", c("United States.jpg", "Finland.jpg"))
)

# Combine the top and bottom defenders for plotting
rank_defensive_team <- bind_rows(
  mutate(top_team, group = "Top"),
  mutate(bot_team, group = "Bottom")
)

# Create the plot
ggplot(rank_defensive_team, aes(x = reorder(team_name, team_contribution_above_avg), y = team_contribution_above_avg, fill = group)) +
  geom_col() +
  geom_image(aes(image = image), size = 0.08, position = position_dodge(width = 1)) +
  geom_text(aes(label = team_contribution_above_avg), size = 3, position = position_stack(vjust = 0.5), color = "black") +
  coord_flip() +
  labs(x = "Team", y = "Contribution Above Average", fill = NULL, title = "Team Defensive Contribution Above Average") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  guides(fill = FALSE)





