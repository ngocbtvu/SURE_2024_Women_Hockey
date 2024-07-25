library(tibble)
library(ggplot2)
library(ggimage)
library(grid)
library(png)
library(jpeg)
library(dplyr)

powerplay_pass <- read.csv("pass_model.csv")

# Data frames for top and bottom 5 defenders
top_5_defenders <- tibble(
  min_dist_from_pl_name = c("Michelle Karvinen", "Laura Stacey", "Amanda Kessel", "Blayre Turnbull", "Oxana Bratishcheva"),
  total_contribution_above_avg = c(0.265, 0.215, 0.194, 0.120, 0.118),
  num_passes = c(10, 5, 6, 7, 7),
  image = paste0("player_image/", c("Michelle Karvinen.jpg", "Laura Stacey.jpg", "Amanda Kessel.jpg", "Blayre Turnbull.jpg", "Oxana Bratishcheva.jpg"))
)

bot_5_defenders <- tibble(
  min_dist_from_pl_name = c("Lara Stalder", "Polina Luchnikova", "Veronika Korzhakova", "Noemi Ryhner", "Alina Muller"),
  total_contribution_above_avg = c(-0.337, -0.182, -0.166, -0.105, -0.0376),
  num_passes = c(8, 11, 8, 23, 22),
  image = paste0("player_image/", c("Lara Stalder.jpg", "Polina Luchnikova.jpg", "Veronika Korzhakova.jpg", "Noemi Ryhner.jpg", "Alina Muller.jpg"))
)

# Combine the top and bottom defenders for plotting
defender_data <- bind_rows(
  mutate(top_5_defenders, group = "Top 5"),
  mutate(bot_5_defenders, group = "Bottom 5")
)

# Create the plot
ggplot(defender_data, aes(x = reorder(min_dist_from_pl_name, total_contribution_above_avg), y = total_contribution_above_avg, fill = group)) +
  geom_col() +
  geom_image(aes(image = image), size = 0.08, position = position_dodge(width = 1)) +
  # geom_text(aes(label = total_contribution_above_avg), position = position_stack(vjust = 0.5), color = "black") +
  geom_text(data = subset(defender_data, min_dist_from_pl_name == "Alina Muller"),
            aes(label = total_contribution_above_avg),
            vjust = .5, hjust = -1.2, color = "black") +
  geom_text(data = subset(defender_data, min_dist_from_pl_name != "Alina Muller"),
            aes(label = total_contribution_above_avg),
            position = position_stack(vjust = 0.5), color = "black") +
  coord_flip() +
  labs(x = "Defender", y = "Contribution Above Average", fill = NULL, title = "Top 5 and Bottom 5 Defenders by Contribution Above Average") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  guides(fill = FALSE)


