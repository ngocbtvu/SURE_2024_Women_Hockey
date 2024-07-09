data <- read.csv("usa_can_hockey.csv")

# Sort by 'Date', 'Period', and 'Clock'
data <- data[order(data$Date, data$Period, data$Clock), ]

# Initialize sequence numbering
current_sequence <- 1
current_match <- paste(data$Date, data$Offense.Team, sep = "_")
previous_event <- ""

# Condition to end a sequence
end_sequence_event <- function(Event) {
  Event %in% c("Incomplete Play", "Shot", "Goal", "Dump In/Out", 
               "Faceoff Win", "Zone Entry", "Takeaway")
}

# Initialize sequence column
data$sequence <- NA

# Loop 
for (i in 1:nrow(data)) {
  current_event <- data$Event[i]
  
  # Condition to start a sequence
  if (current_event %in% c("Faceoff Win", "Zone Entry", "Takeaway")) {
    if (previous_event == "" || end_sequence_event(previous_event)) {
      current_sequence <- current_sequence + 1
    }
  }
  
  # Assign sequence number 
  data$sequence[i] <- current_sequence
  
  # Update previous event
  previous_event <- current_event
}

# Export csv file
write.csv(data, "usa_can_hockey_sequence.csv", row.names = FALSE)
