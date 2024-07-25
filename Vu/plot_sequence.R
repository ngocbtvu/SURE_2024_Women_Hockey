library(dplyr)
library(sportyR)
library(ggplot2)

data <- read.csv("women_hockey_data_with_sequence.csv")
mirrored_pass_with_cluster <- read.csv("mirrored_pass_with_cluster.csv")

# Plot hockey rink
hockey_rink <- geom_hockey("nhl", xlims = c(-100, 100), ylims = c(-42.5, 42.5))

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

cluster_type <- c(
  "1" = "Offensive Regroup",
  "2" = "Behind the net Breakout",
  "3" = "D to D",
  "4" = "Neutral Zone",
  "5" = "Reach out",
  "6" = "In front of the net Breakout",
  "7" = "Possession",
  "8" = "Cross Ice",
  "9" = "Defensive Regroup",
  "10" = "D to F"
)

coordinate_data <- coordinate_data %>%
  mutate(pass_cluster = recode(as.character(pass_cluster), !!!cluster_type))

# Plot the clusters
plot_cluster <- hockey_rink +
  geom_segment(data = coordinate_data, aes(
    # x = 100 - X.Coordinate,
    # y = Y.Coordinate.x - 42.5,
    # xend = 100 - X.Coordinate.2,
    # yend = Y.Coordinate.2 - 42.5,
    x = -(100 - X.Coordinate),
    y = -(Y.Coordinate.x - 42.5),
    xend = -(100 - X.Coordinate.2),
    yend = -(Y.Coordinate.2 - 42.5),
    color = as.factor(pass_cluster)
  ), arrow = arrow(length = unit(0.1, "inches"))) +
  labs(title = "Passes following a Puck Recovery") +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank(),  
    legend.position = "bottom",    
    legend.title = element_text(size = 0),  
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 20)
  ) +
  scale_color_manual(name = "Pass Type Cluster", values = c("Offensive Regroup" = "blue", "Behind the net Breakout" = "red", "D to D" = "darkgrey", "Neutral Zone" = "purple", "Reach out" = "orange", "In front of the net Breakout" = "green", "Possession" = "cyan", "Cross Ice" = "brown", "Defensive Regroup" = "darkgreen", "D to F" = "magenta"))
print(plot_cluster)
