#The animation tutorial

## SET UP ##

# Load packages
library(tidyverse)
#install.packages('gganimate')
library(gganimate) # For turning ggplot's into animations


# Load the data directly from github
# To get this URL go to our GitHub repository then click "TrackingData/<game name>/<powerplay name.csv>", click "Raw", then copy the url once the raw data loads
tracking_data = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P3%20PP6.csv")

## DATA WRANGLING WITH NEST AND MAP ##

# The nest and map functions are a huge help in making sense of tracking data and performing frame-by-frame operations

# Using group_by and nest in R allows us to split data into smaller more manageable sub-data frames
tracking_data_nested = tracking_data %>%
  group_by(frame_id) %>%
  nest()


# We can perform transformations on the nested data using mutate and map
tracking_data_player_count = tracking_data_nested |> 
  mutate(num_players = map(.x = data, .f = ~nrow(.x)))




## ANIMATED PLOTS ##

# Set the specs for the gif we want to create (lower res to make it run quicker)
# options(gganimate.dev_args = list(width = 10, height = 6, units = 'in', res = 320))

# Source in the plot_rink function
source("Perez/plot_rink.R")

#install.packages('ggforce')
library(ggforce)

# Create a gif of this play
p = plot_rink(ggplot(tracking_data)) +
  geom_point(aes(x = x_ft, y = y_ft, fill = team_name), shape = 21, size = 6) +
  geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
  scale_colour_manual(values = c("USA" = "white", "Finland" = "navy")) +
  scale_fill_manual(values = c("USA" = "blue", "Finland" = "grey90")) +
  transition_time(frame_id) +
  labs(fill = "Team") +
  guides(colour = "none")


# Get the maximum and minimum frame number (so we know how long the gif should be)
max_frame = max(tracking_data$frame_id)
min_frame = min(tracking_data$frame_id)


# Render the animation
p2 = animate(p, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame)/30 + 1)


# Save as an mp4
#this mp4 does not open on quicktime
anim_save("Perez/demo2.mp4", p2)

######
#this one works
animate(p,renderer=av_renderer("Perez/demo3.mp4"), fps = 30, duration = (max_frame - min_frame)/30 + 1)

#-------------------------------------------------------------------
#Actually working with the tracking data and animation
library(tidyverse)
library(dplyr)
library(ggmosaic)
library(devtools)
library(ranger)
library(glmnet)
library(ggthemes)
library(ggplot2)
library(ggbeeswarm)
library(GGally)
library(tidyr)
library(ggimage)
library(hockeyR)
library(sportyR)
library(ggridges)
library(ggalluvial)


#getting the data that tracks power play number 

pp_numbers <- read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/pp_info.csv')

#filtering into USA games
pp_numbers_usa <- pp_numbers |> 
  filter(grepl('USA', game_name))

#filter to USA vs Canada games
pps_usa_can <- pp_numbers_usa |> 
  filter(grepl('Canada', game_name))

#filter to USA vs Finland games
pps_usa_fin <- pp_numbers_usa |> 
  filter(grepl('Finland', game_name))


#-------------------------------------------------------------------
#Now finding all the goals in these power plays

#First by loading in the play-by-play data
pbp_data = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv")

#Finding how power plays are listed, is it only '5 on 4' or also '4 on 5'?
pbp_data |> 
  select(situation_type) |>  
  distinct()

#Now finding the list of events to find how goals are listed
pbp_data |> 
  select(event) |>  
  distinct()

#Goals are not listed, just shots, but there is are 'event_successful',
# and 'event_type' and 'event_detail_1' columns
pbp_data |> 
  filter(event == 'Shot') |> 
  select(event_successful) |> 
  distinct()

pbp_data |> 
  filter(event == 'Shot') |> 
  select(event_type) |> 
  distinct()

pbp_data |> 
  filter(event == 'Shot') |> 
  select(event_detail_1) |> 
  distinct()

goal_nogoal_counts = pbp_data |> 
  filter(event == "Shot") |> 
  group_by(event_successful) |> 
  count()

#so only goals are classified as Successful shots

#now lets filter into just goals, and just power play goals, maybe the traditional 5v4

goals <- pbp_data |> 
  filter(event == 'Shot') |> 
  filter(event_successful == 't')

#are there 4v5 goals, or only 5v4 goals in the goal data
goals |> 
  filter(event == 'Shot') |> 
  filter(event_successful == 't') |> 
  select(situation_type) |> 
  distinct()

goal_situation_count <- goals |> 
  filter(event == 'Shot') |> 
  filter(event_successful == 't') |> 
  group_by(situation_type) |> 
  count()

#there is only 1 4v5 (penalty kill) goal 
  #also only 1 goal for each of: (4 on 6), (5 on 6), and (6 on 5)
#and there are 9 5v4 (power play) goals

pp_goals <- goals |> 
  filter(event == 'Shot') |> 
  filter(event_successful == 't') |> 
  filter(situation_type == '5 on 4')

#now filter to USA pp goals
usa_pp_goals <- pp_goals |> 
  filter(grepl('United States', team_name))
         
#now bring back the power play data that gives penalty count and game clock info
view(pp_numbers)
view(pp_numbers_usa)
view(pps_usa_can)
view(pps_usa_fin)

view(usa_pp_goals)

#match the two USA pp goals to power play info by time on clock

#i want to automate/calculate this later, 
#but the data across tables is not compatible 
#so for now I will do the matching manually

  
#now we know what player tracking CSVs to load in









#the goal vs canada seems to be a 5v4 situation but 
#the penalty (#4) closest to the time the goal was scored lasts 0 seconds and has no CSV file
usa_can_pp_goal_track <- read.csv()





#finland is pp1
usa_fin_pp_goal_track <- read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P2%20PP1.csv')

usa_fin_pp_goal_track |> 
  select(frame_id) |> 
  distinct()

max_frame = max(usa_fin_pp_goal_track$frame_id)
min_frame = max(usa_fin_pp_goal_track$frame_id)-300

usa_fin_pp_goal_track_cut <- usa_fin_pp_goal_track |> 
  filter(frame_id>= min_frame & frame_id<= max_frame)

#goal was scored with 982 seconds left in the game clock
#pp ends with 981 seconds left in the game clock
#so just use the (last 5 seconds = max frame - (5secs)*(30fps))

usa_fin_nested = usa_fin_pp_goal_track_cut |> 
  group_by(frame_id) |> 
  nest()

# We can perform transformations on the nested data using mutate and map
usa_fin_player_count = usa_fin_nested |> 
  mutate(num_players = map(.x = data, .f = ~nrow(.x)))


## ANIMATED PLOTS ##

# Set the specs for the gif we want to create (lower res to make it run quicker)
#options(gganimate.dev_args = list(width = 10, height = 6, units = 'in', res = 320))

# Source in the plot_rink function
source("Perez/plot_rink.R")

#install.packages('ggforce')
library(ggforce)


# Get the maximum and minimum frame number (so we know how long the gif should be)


# Create a gif of this play
goal_p = plot_rink(ggplot(usa_fin_pp_goal_track_cut)) +
  geom_point(aes(x = x_ft, y = y_ft, fill = team_name), shape = 21, size = 6) +
  geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
  scale_colour_manual(values = c("USA" = "white", "Finland" = "navy")) +
  scale_fill_manual(values = c("USA" = "blue", "Finland" = "grey90")) +
  labs(fill = "Team") +
  guides(colour = "none") +
  transition_time(frame_id) +
  ease_aes() +
  NULL


# Render the animation
#goal_p2 = animate(goal_p, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame)/30 + 1)

# Save as an mp4
#this mp4 does not open on quicktime
#anim_save("Perez/demo2.mp4", p2)

#this one works
#animate(goal_p,renderer=av_renderer("Perez/goal_demo.mp4"), fps = 30, duration = (max_frame - min_frame)/30 + 1)

animate(goal_p, renderer=av_renderer("Perez/goal_demo2.mp4"), duration = 10)


#animate 5 seconds leading to goal









