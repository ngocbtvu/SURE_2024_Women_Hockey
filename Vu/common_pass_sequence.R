library(dplyr)
data <- read.csv("women_hockey_data_with_sequence.csv")

# Group sequence and events
pass_sequences <- data %>%
  mutate(Event = case_when(
    Event %in% c("Play", "Incomplete Play") ~ "Pass",
    TRUE ~ Event)) %>%
  group_by(sequence) %>%
  summarise(pass_sequence = paste(Event, collapse = " - "), event_count = n())

# Count each pass sequence starting with Faceoff Win
pass_sequence_counts <- pass_sequences %>%
  filter((grepl("^Faceoff Win", pass_sequence) | grepl("^Puck Recovery", pass_sequence) 
          | grepl("^Takeaway", pass_sequence)) & grepl("Pass", pass_sequence)) %>%
  group_by(pass_sequence) %>%
  summarise(count = n_distinct(sequence)) %>% 
  arrange(desc(count))
head(pass_sequence_counts, 10)

# Count each pass sequence starting with Takeaway
pass_sequence_counts <- pass_sequences %>%
  filter(grepl("^Takeaway", pass_sequence) & grepl("Play", pass_sequence)) %>%
  group_by(pass_sequence) %>%
  summarise(count = n_distinct(sequence)) %>% 
  arrange(desc(count))
head(pass_sequence_counts, 3)

# Count each pass sequence starting with Puck Recovery
pass_sequence_counts <- pass_sequences %>%
  filter(grepl("^Puck Recovery", pass_sequence) & grepl("Play", pass_sequence)) %>%
  group_by(pass_sequence) %>%
  summarise(count = n_distinct(sequence)) %>% 
  arrange(desc(count))
head(pass_sequence_counts, 3)




