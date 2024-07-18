factor_variable_passes <- read.csv("factor_variable_passes_data.csv")
mirrored_pass_with_cluster <- read.csv("mirrored_pass_with_cluster.csv")

# Add cluster to powerplay data
powerplay_pass <- factor_variable_passes %>%
  left_join(mirrored_pass_with_cluster %>% select(Clock, Player, Player.2,
                                     X.Coordinate, X.Coordinate.2, pass_cluster),
            by = c("clock_seconds" = "Clock",
                   "player_name" = "Player",
                   "player_name_2" = "Player.2",
                   "x_coord" = "X.Coordinate",
                   "x_coord_2" = "X.Coordinate.2"))

# Import csv
write.csv(powerplay_pass, "powerplay_pass_data.csv", row.names = FALSE)
