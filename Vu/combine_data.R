library(dplyr)
library(lubridate)

rivarlyseries <- read.csv("BDC_2024_Womens_Data.csv")
olympics18 <- read.csv("hackathon_womens.csv") 
olympics22 <- read.csv("pxp_womens_oly_2022_v2.csv")

# Convert clock time to seconds 
convert_clock_to_seconds <- function(clock) {
  parts <- strsplit(clock, ":")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

# Process olympics22
olympics22 <- olympics22 %>%
  separate(situation_type, into = c("Home.Team.Skaters", "Away.Team.Skaters"), sep = " on ") %>%
  mutate(
    Date = as.Date(game_date, format = "%m/%d/%Y"),
    Year = year(Date),
    Tournament = "Olympics 2022",
    Home.Team = ifelse(venue == "home", team_name, opp_team_name),
    Away.Team = ifelse(venue == "home", opp_team_name, team_name),
    Home.Team = gsub("Olympic \\(Women\\) - Canada", "Women - Canada", Home.Team),
    Home.Team = gsub("Olympic \\(Women\\) - United States", "Women - United States", Home.Team),
    Away.Team = gsub("Olympic \\(Women\\) - Canada", "Women - Canada", Away.Team),
    Away.Team = gsub("Olympic \\(Women\\) - United States", "Women - United States", Away.Team),
    Clock = clock_seconds,
    Home.Team.Skaters = as.numeric(Home.Team.Skaters),
    Away.Team.Skaters = as.numeric(Away.Team.Skaters),
    Home.Team.Goals = goals_for,
    Away.Team.Goals = goals_against,
    Player.Team = team_name,
    Player.Team = gsub("Olympic \\(Women\\) - Canada", "Women - Canada", Player.Team),
    Player.Team = gsub("Olympic \\(Women\\) - United States", "Women - United States", Player.Team),
    Away.Team = gsub("Olympic \\(Women\\) - Canada", "Women - Canada", Away.Team),
    Away.Team = gsub("Olympic \\(Women\\) - United States", "Women - United States", Away.Team),
    Player = player_name,
    Event = event,
    X.Coordinate = x_coord,
    Y.Coordinate = y_coord,
    Player.2 = player_name_2,
    X.Coordinate.2 = x_coord_2,
    Y.Coordinate.2 = y_coord_2,
    Detail.1 = event_type,
    Detail.2 = event_detail_1,
    Detail.3 = event_detail_2,
    Detail.4 = event_detail_3
  ) %>%
  select(Date,Year, Tournament, Home.Team, Away.Team, Period = period, Clock, Home.Team.Skaters, Away.Team.Skaters, 
         Home.Team.Goals, Away.Team.Goals, Player.Team, Player, Event, X.Coordinate, Y.Coordinate, 
         Detail.1, Detail.2, Detail.3, Detail.4, Player.2, X.Coordinate.2, Y.Coordinate.2)

# Process olympics18
olympics18 <- olympics18 %>%
  mutate(
    Date = as.Date(game_date, format = "%Y-%m-%d"),
    Year = year(Date),
    Tournament = "Olympics 2018",
    Clock = sapply(Clock, convert_clock_to_seconds),
    Player.Team = Team,
    Player.2 = `Player.2`,
    X.Coordinate.2 = `X.Coordinate.2`,
    Y.Coordinate.2 = `Y.Coordinate.2`,
    Home.Team = gsub("Olympic \\(Women\\) - Canada", "Women - Canada", Home.Team),
    Home.Team = gsub("Olympic \\(Women\\) - United States", "Women - United States", Home.Team),
    Away.Team = gsub("Olympic \\(Women\\) - Canada", "Women - Canada", Away.Team),
    Away.Team = gsub("Olympic \\(Women\\) - United States", "Women - United States", Away.Team),
    Away.Team = gsub("Olympic \\(Women\\) - Canada", "Women - Canada", Away.Team),
    Away.Team = gsub("Olympic \\(Women\\) - United States", "Women - United States", Away.Team)
  ) %>%
  select(Date, Year, Tournament, Home.Team = Home.Team, Away.Team = Away.Team, Period, Clock, 
         Home.Team.Skaters, Away.Team.Skaters, Home.Team.Goals, Away.Team.Goals, 
         Player.Team, Player, Event, X.Coordinate, Y.Coordinate, 
         Detail.1, Detail.2, Detail.3, Detail.4, Player.2, X.Coordinate.2, Y.Coordinate.2)

# Process rivarlyseries
rivarlyseries <- rivarlyseries %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Year = year(Date),
    Tournament = "Rivalry Series 2023",
    Clock = sapply(Clock, convert_clock_to_seconds),
    Player.Team = Team
  ) %>%
  select(Date, Year, Tournament, Home.Team, Away.Team, Period, Clock, Home.Team.Skaters, Away.Team.Skaters, 
         Home.Team.Goals, Away.Team.Goals, Player.Team, Player, Event, X.Coordinate, Y.Coordinate, 
         Detail.1, Detail.2, Detail.3, Detail.4, Player.2, X.Coordinate.2, Y.Coordinate.2)

# Combine data
combined_data <- bind_rows(olympics22, olympics18, rivarlyseries)

# Filter Canada and USA teams
filtered_data <- combined_data %>%
  filter(Home.Team %in% c("Women - Canada", "Women - United States") 
         & Away.Team %in% c("Women - Canada", "Women - United States"))

# Export csv file
write.csv(filtered_data, "usa_can_hockey.csv", row.names = FALSE)
