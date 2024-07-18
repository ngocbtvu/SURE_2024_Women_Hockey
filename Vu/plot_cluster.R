library(dplyr)
library(sportyR)
library(ggplot2)

powerplay_pass <- read.csv("powerplay_pass_data.csv")

# Plot hockey rink
hockey_rink <- geom_hockey("nhl", xlims = c(-100, 100), ylims = c(-42.5, 42.5))

powerplay_cluster <- powerplay_pass %>%
  filter(pass_cluster == "10") 

# Plot the clusters
plot_cluster <- hockey_rink +
  geom_segment(data = powerplay_cluster, aes(
    x = 100 - x_coord,
    y = y_coord - 42.5,
    xend = 100 - x_coord_2,
    yend = y_coord_2 - 42.5,
    color = as.factor(pass_cluster)
  ), arrow = arrow(length = unit(0.1, "inches"))) +
  labs(title = "Most Successful Pass Clusters according to Pass Completion Probability Model") +
  theme_minimal() +
  scale_color_manual(name = "Pass Cluster No.", values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple", "5" = "orange", "6" = "darkgrey", "7" = "cyan", "8" = "brown", "9" = "darkgreen", "10" = "magenta"))
print(plot_cluster)

