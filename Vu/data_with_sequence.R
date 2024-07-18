library(dplyr)

data <- read.csv("women_hockey_data.csv")

# Sort by Date, Period, and Clock
data <- data[order(data$Date, data$Period, data$Clock), ]

# Initialize sequence variables
current_sequence <- -1
previous_match <- ""
previous_event <- ""
data$sequence <- NA

# Loop to number sequences
for (i in 1:nrow(data)) {
  current_match <- paste(data$Date[i], data$Offense.Team[i], sep = " ")
  current_event <- data$Event[i]
  
  # Condition to start a new sequence based on match
  if (current_match != previous_match) {
    current_sequence <- current_sequence + 1
  }
  
  # Condition to start a new sequence based on events
  if (previous_event == "" | previous_event == "Incomplete Play" | previous_event == "Shot" | 
      previous_event == "Goal" | previous_event == "Dump In/Out" | previous_event == "Penalty" |
      current_event == "Faceoff Win" | current_event == "Takeaway" | current_event == "Puck Recovery") {
    current_sequence <- current_sequence + 1
  }
  
  # Assign sequence number
  data$sequence[i] <- current_sequence
  
  # Update previous event and match
  previous_event <- current_event
  previous_match <- current_match
}

# Export csv file
write.csv(data, "women_hockey_data_with_sequence.csv", row.names = FALSE)
