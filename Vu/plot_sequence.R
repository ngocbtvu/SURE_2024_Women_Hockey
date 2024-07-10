library(dplyr)
library(ggplot2)
library(sportyR)

data <- read.csv("usa_can_hockey_sequence.csv")

# Filter sequence "Zone Entry - Play - Shot"
sequence_data <- data %>%
  # filter(Offense.Team == "Women - United States" | Offense.Team == "Olympic (Women) - United States") %>%
  filter(Event == "Zone Entry" | Event == "Puck Recovery"
         | Event == "Play" | Event == "Shot") %>%
  group_by(sequence) %>%
  mutate(sequence_events = paste(Event, collapse = " - ")) %>%
  filter(sequence_events == "Zone Entry - Play - Puck Recovery - Play - Shot") %>%
  filter(!is.na(X.Coordinate.2) & !is.na(Y.Coordinate.2)) %>%
  ungroup()

# Plot the sequences on the hockey rink
hockey_rink <- geom_hockey("nhl", xlims = c(-100, 100), ylims = c(-42.5, 42.5))

plot_sequences <- hockey_rink +
  geom_segment(data = sequence_data, aes(
    x = X.Coordinate - 100,
    y = Y.Coordinate - 42.5,
    xend = X.Coordinate.2 - 100,
    yend = Y.Coordinate.2 - 42.5,
    color = Detail.1
  ), arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(data = sequence_data, aes(
    x = X.Coordinate - 100,
    y = Y.Coordinate - 42.5,
  ), color = "red", size = 2) +
  labs(title = "Pass from Sequence: Zone Entry - Play - Puck Recovery - Play - Shot") +
  theme_minimal()
print(plot_sequences)
