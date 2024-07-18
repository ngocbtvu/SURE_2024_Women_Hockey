library(dplyr)
library(tidyr)
library(lubridate)

rivarlyseries <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2024/main/BDC_2024_Womens_Data.csv")
olympics18 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv") 
olympics22 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv")

# Convert clock time to seconds 
convert_clock_to_seconds <- function(clock) {
  parts <- strsplit(clock, ":")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

# Process olympics22
olympics22 <- olympics22 %>%
  separate(situation_type, into = c("Offense.Team.Skaters", "Defense.Team.Skaters"), sep = " on ") %>%
  mutate(
    Date = game_date,
    Tournament = "Olympics 2022",
    Home.Team = ifelse(venue == "home", team_name, opp_team_name),
    Away.Team = ifelse(venue == "away", team_name, opp_team_name),
    Period = period,
    Clock = clock_seconds,
    Offense.Team = team_name,
    Defense.Team = opp_team_name,
    Offense.Team.Skaters = as.numeric(Offense.Team.Skaters),
    Defense.Team.Skaters = as.numeric(Defense.Team.Skaters),
    Offense.Team.Goals = goals_for,
    Defense.Team.Goals = goals_against,
    Player = player_name,
    Event = case_when(
      event == "Play" & event_successful == "t" ~ "Play",
      event == "Play" & event_successful == "f" ~ "Incomplete Play",
      event == "Shot" & event_successful == "t" ~ "Goal",
      event == "Shot" & event_successful == "f" ~ "Shot",
      TRUE ~ event
    ),
    X.Coordinate = x_coord,
    Y.Coordinate = y_coord,
    Player.2 = player_name_2,
    X.Coordinate.2 = x_coord_2,
    Y.Coordinate.2 = y_coord_2,
    Detail.1 = case_when(
      event == "Dump In/Out" & event_successful == "t" ~ "Carried",
      event == "Dump In/Out" & event_successful == "f" ~ "Dumped",
      event == "Zone Entry" & event_successful == "t" ~ "Retained",
      event == "Zone Entry" & event_successful == "f" ~ "Lost",
      TRUE ~ ""
    ),
    Detail.2 = event_detail_1,
    Detail.3 = event_detail_2,
    Detail.4 = event_detail_3
  ) %>%
  select(Date, Tournament, Home.Team, Away.Team, Period, Clock, Offense.Team, Defense.Team, 
         Offense.Team.Skaters, Defense.Team.Skaters, Offense.Team.Goals, Defense.Team.Goals,
         Player, Event, X.Coordinate, Y.Coordinate, Detail.1, Detail.2, Detail.3, Detail.4, 
         Player.2, X.Coordinate.2, Y.Coordinate.2)

# Process olympics18
olympics18 <- olympics18 %>%
  mutate(
    Date = game_date,
    Tournament = "Olympics 2018",
    Clock = sapply(Clock, convert_clock_to_seconds),
    Offense.Team = Team,
    Defense.Team = ifelse(Team == Home.Team, Away.Team, Home.Team),
    Offense.Team.Skaters = ifelse(Team == Home.Team, as.numeric(Home.Team.Skaters), as.numeric(Away.Team.Skaters)),
    Defense.Team.Skaters = ifelse(Team != Home.Team, as.numeric(Home.Team.Skaters), as.numeric(Away.Team.Skaters)),
    Offense.Team.Goals = ifelse(Team == Home.Team, as.numeric(Home.Team.Goals), as.numeric(Away.Team.Goals)),
    Defense.Team.Goals = ifelse(Team != Home.Team, as.numeric(Home.Team.Goals), as.numeric(Away.Team.Goals)),
  ) %>%
  select(Date, Tournament, Home.Team, Away.Team, Period, Clock, Offense.Team, Defense.Team,
         Offense.Team.Skaters, Defense.Team.Skaters, Offense.Team.Goals, Defense.Team.Goals, 
         Player, Event, X.Coordinate, Y.Coordinate, Detail.1, Detail.2, Detail.3, Detail.4, 
         Player.2, X.Coordinate.2, Y.Coordinate.2)

# Process rivarlyseries
rivarlyseries <- rivarlyseries %>%
  mutate(
    Tournament = "Rivalry Series 2023",
    Clock = sapply(Clock, convert_clock_to_seconds),
    Offense.Team = Team,
    Defense.Team = ifelse(Team == Home.Team, Away.Team, Home.Team),
    Offense.Team.Skaters = ifelse(Team == Home.Team, as.numeric(Home.Team.Skaters), as.numeric(Away.Team.Skaters)),
    Defense.Team.Skaters = ifelse(Team != Home.Team, as.numeric(Home.Team.Skaters), as.numeric(Away.Team.Skaters)),
    Offense.Team.Goals = ifelse(Team == Home.Team, as.numeric(Home.Team.Goals), as.numeric(Away.Team.Goals)),
    Defense.Team.Goals = ifelse(Team != Home.Team, as.numeric(Home.Team.Goals), as.numeric(Away.Team.Goals))
  ) %>%
  select(Date, Tournament, Home.Team, Away.Team, Period, Clock, Offense.Team, Defense.Team,
         Offense.Team.Skaters, Defense.Team.Skaters, Offense.Team.Goals, Defense.Team.Goals, 
         Player, Event, X.Coordinate, Y.Coordinate, Detail.1, Detail.2, Detail.3, Detail.4, 
         Player.2, X.Coordinate.2, Y.Coordinate.2)

# Combine 3 dataset
combined_data <- bind_rows(olympics22, olympics18, rivarlyseries)

# Filter non-international matches
filtered_data <- combined_data %>%
  filter(!(Home.Team == "St. Lawrence Saints"))

# Export csv file
write.csv(filtered_data, "women_hockey_data.csv", row.names = FALSE)


