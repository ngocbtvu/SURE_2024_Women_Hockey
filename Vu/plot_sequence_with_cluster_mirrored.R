library(dplyr)
library(sportyR)
library(ggplot2)

data <- read.csv("women_hockey_data_with_sequence.csv")

# Plot hockey rink
hockey_rink <- geom_hockey("nhl", xlims = c(-100, 100), ylims = c(-42.5, 42.5))

# Filter pass and mirror the coordinates
mirrored_pass_2 <- data %>%
  filter(Event == "Play" | Event == "Incomplete Play") %>%
  filter(Detail.1 != "Indirect") %>%
  mutate(Y.Coordinate = ifelse(Y.Coordinate > 42.5, Y.Coordinate, 42.5+Y.Coordinate),
         Y.Coordinate.2 = ifelse(Y.Coordinate.2 > 42.5, Y.Coordinate.2, 42.5+Y.Coordinate.2)) 

# k-means clustering with k = 10
set.seed(1711)
kmeans_result <- kmeans(mirrored_pass[, c("X.Coordinate", "Y.Coordinate", 
                                          "X.Coordinate.2", "Y.Coordinate.2")], centers = 10)

# Add cluster to the pass data
mirrored_pass_with_cluster <- mirrored_pass %>%
  mutate(pass_cluster = kmeans_result$cluster)

# Import csv
write.csv(mirrored_pass_with_cluster, "mirrored_pass_with_cluster.csv", row.names = FALSE)

# Filter sequence
sequence_data <- data %>%
  group_by(sequence) %>%
  mutate(sequence_events = paste(Event, collapse = " - ")) %>%
  filter(sequence_events == "Puck Recovery - Play") %>%
  filter(!is.na(X.Coordinate.2) & !is.na(Y.Coordinate.2)) %>%
  filter(Detail.1 != "Indirect") %>%
  ungroup()

# Add cluster to sequence data
coordinate_data <- sequence_data %>%
  left_join(mirrored_pass_with_cluster %>% select(Date, Tournament, Home.Team, Away.Team, Period, Clock, Offense.Team, Defense.Team, X.Coordinate, Y.Coordinate, pass_cluster),
            by = c("Date", "Tournament", "Home.Team", "Away.Team", "Period", "Clock" = "Clock", "Offense.Team", "Defense.Team", "X.Coordinate" = "X.Coordinate"))

# Plot the clusters
plot_cluster <- hockey_rink +
  geom_segment(data = coordinate_data, aes(
    x = 100 - X.Coordinate,
    y = Y.Coordinate.x - 42.5,
    xend = 100 - X.Coordinate.2,
    yend = Y.Coordinate.2 - 42.5,
    color = as.factor(pass_cluster)
  ), arrow = arrow(length = unit(0.1, "inches"))) +
  labs(title = "Pass from Sequence: Puck Recovery - Play") +
  theme_minimal() + 
  scale_color_manual(name = "Pass Type Cluster", values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple", "5" = "orange", "6" = "darkgrey", "7" = "cyan", "8" = "brown", "9" = "darkgreen", "10" = "magenta"))
print(plot_cluster)
