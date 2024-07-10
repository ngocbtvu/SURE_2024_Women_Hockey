library(dplyr)
data <- read.csv("usa_can_hockey_sequence.csv")

# Group sequence and events
pass_sequences <- data %>%
  group_by(sequence) %>%
  summarise(pass_sequence = paste(Event, collapse = " - "))

# Count each pass sequence starting with Zone Entry
pass_sequence_counts <- pass_sequences %>%
  filter(grepl("^Zone Entry", pass_sequence) & grepl("Play - Shot", pass_sequence)) %>%
  group_by(pass_sequence) %>%
  summarise(count = n_distinct(sequence)) %>% 
  arrange(desc(count))

# Top 10 sequences starting with Zone Entry
head(pass_sequence_counts, 10)


