library(dplyr)
library(sportyR)
library(ggplot2)

mirrored_pass_with_cluster_data_data  <- read.csv("mirrored_pass_with_cluster.csv")
powerplay_pass_data <- read.csv("powerplay_pass_data.csv")

# Plot hockey rink
hockey_rink <- geom_hockey("nhl", xlims = c(-100, 100), ylims = c(-42.5, 42.5))

# Join data
coordinate_data <- powerplay_pass_data %>%
  left_join(mirrored_pass_with_cluster_data %>% select(Period, Clock, Offense.Team,Player,Player.2, X.Coordinate, X.Coordinate.2),
            by = c("clock_seconds" = "Clock",
                   "period" = "Period",
                   "player_name" = "Player",
                   "player_name_2" = "Player.2",
                   "x_coord" = "X.Coordinate",
                   "x_coord_2" = "X.Coordinate.2"))

# Change team name
coordinate_data <- coordinate_data %>%
  mutate(team_name = case_when(
    team_name == "Olympic (Women) - United States" ~ "United States",
    team_name == "Olympic (Women) - Switzerland" ~ "Switzerland",
    team_name == "Olympic (Women) - Olympic Athletes from Russia" ~ "Russia",
    team_name == "Olympic (Women) - Canada" ~ "Canada",
    team_name == "Olympic (Women) - Finland" ~ "Finland",
    TRUE ~ team_name  
  ))

# Plot the clusters
plot_cluster <- hockey_rink +
  geom_segment(data = coordinate_data, aes(
    x = -(100 - x_coord),
    y = -(y_coord - 42.5),
    xend = -(100 - x_coord_2),
    yend = -(y_coord_2 - 42.5),
    color = as.factor(team_name),
  ), arrow = arrow(length = unit(0.1, "inches"))) +
  labs(title = "All Powerplay Passes by Team") +
  theme_minimal() + 
  theme(
    axis.title = element_blank(), 
    axis.text = element_blank(),  
    axis.ticks = element_blank(),  
    legend.position = "bottom",   
    legend.title = element_text(size = 0),  
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 20)
  ) 
print(plot_cluster)

