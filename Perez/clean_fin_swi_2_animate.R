#lets try again
#now we know that we have to check the 'videShotsInfo' table to see what seconds are included in the tracking data

library(tidyverse)
library(gganimate)
library(ggforce)
library(dplyr)
source("Perez/plot_rink.R")

#loading all the power plays
pp_numbers <- read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/pp_info.csv')

#-------------------------------------------------------------------------

#load all the play by play
pbp_data = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv")

#filter into just power play goals
pbp_pp_goals <- pbp_data |> 
  filter(event == 'Shot') |> 
  filter(event_successful == 't') |> 
  filter(situation_type == '5 on 4')


#-------------------------------------------------------------------------
#LOAD the data
fin_swi_2_track_clean <- read.csv('https://raw.githubusercontent.com/picagrad/Big-Data-Cup-2022/main/data/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P3%20PP8.csv')


#CUT the tracking ddata to just those 10 seconds
fin_swi_2_track_cut_clean <- fin_swi_2_track_clean |> 
  filter(game_seconds>= 56 & game_seconds<= 68)

#NEST the data (for the animation?)
fin_swi_2_nested_clean = fin_swi_2_track_clean |> 
  group_by(frame_id) |> 
  nest()

fin_swi_2_player_count_clean = fin_swi_2_nested_clean |> 
  mutate(num_players = map(.x = data, .f = ~nrow(.x)))

#-----------
source("Perez/plot_rink.R")

#Might need these libraries?
library(ggtext)

#install.packages('gapminder')
library(gapminder)

library(dplyr)


#---------------------------------------------------------------------------
#Now CUT the PBP DATA into just the 10 seconds leading to the goal

#First cut to just the game
pbp_fin_swi_2 <- subset(pbp_data, 
                        (grepl('Finland', team_name)) & 
                          (grepl('Switzerland', opp_team_name)))

#now cut it into the last 10 seconds before the goal
#period 2, leading up to 982 seconds left
fin_swi_2_pbp_goal <- pbp_fin_swi_2 |> 
  filter(period == 3) |> 
  filter(clock_seconds>56 & clock_seconds<68)

fin_swi_2_pbp_goal <- fin_swi_2_pbp_goal |> 
  mutate(game_seconds = clock_seconds)

#now fuzzy join the pbp and tracking data by the clock_seconds and seconds_remaining

#i think i want a half second each way, or 0.4 seconds each way, 'll mess with it
#i want the shots and passes ot actually remain on screen for a little bit
library(dplyr)

#install.packages("fuzzyjoin")

# Load the package
library(fuzzyjoin)

#------------------------------------------
#The pbp data is mirrored, so fix that
fin_swi_2_pbp_goal <- fin_swi_2_pbp_goal |> 
  mutate(y_coord = abs(y_coord-85)) |> 
  mutate(y_coord_2 = abs(y_coord_2-85))

#join the data, this still has the 'max_dist' line for a fuzzy join
#in case someone wants to calculate the decimal seconds in the tracking data 
#to make the animations a tiny bit more exact
final_fin_swi_2_goal_clean <- difference_left_join(
  fin_swi_2_track_cut_clean,
  fin_swi_2_pbp_goal,
  by = 'game_seconds',
  max_dist = 0.35,
  distance_col = NULL
)

#-----------------------------
#creating dataframes for each event
fin_swi_2_passes_clean <- final_fin_swi_2_goal_clean |> 
  filter(event == 'Play')

fin_swi_2_shots_clean <- final_fin_swi_2_goal_clean |> 
  filter(event == 'Shot')

fin_swi_2_recover_clean <- final_fin_swi_2_goal_clean |> 
  filter(event == 'Puck Recovery')

#don't use this, it messes up the legends, but could be useful in the future
fin_swi_2_points_clean <- final_fin_swi_2_goal_clean |> 
  filter(event %in% c('Shot', 'Puck Recovery', 'Takeaway'))
#-----------------------


#-------------------

fin_swi_2_goal_p_2_clean = plot_rink(ggplot(final_fin_swi_2_goal_clean)) +
  geom_point(aes(x = x_ft, y = y_ft, fill = team_name.x), shape = 21, size = 6) +
  geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name.x), size = 3) +
  scale_colour_manual(values = c("Switzerland" = "white", "Finland" = "navy")) +
  scale_fill_manual(values = c("Switzerland" = "red", "Finland" = "white")) +
  guides(colour = "none") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  transition_time(frame_id)+
  labs(title = 'Finland Power Play Goal',
       subtitle = 'Game Clock: {floor((192-(frame_time)/30)/60)}:{ceiling((192-(frame_time)/30)%%60)}',
       fill = "Team") +
  ease_aes()+
  geom_point(x = fin_swi_2_shots_clean$x_coord, y = fin_swi_2_shots_clean$y_coord, 
             fill = 'green3', data = fin_swi_2_shots_clean, shape = 21, size = 6)+
  geom_segment(x = final_fin_swi_2_goal_clean$x_coord, y = final_fin_swi_2_goal_clean$y_coord, 
               xend = final_fin_swi_2_goal_clean$x_coord_2, yend = final_fin_swi_2_goal_clean$y_coord_2, colour = "skyblue",
               linewidth = 1.2, arrow = arrow(length = unit(0.2, "inches")), data = final_fin_swi_2_goal_clean)+
  geom_label(aes(label=str_wrap(final_fin_swi_2_goal_clean$x_coord,12), 
                 x=((1/2)*(final_fin_swi_2_goal_clean$x_coord + final_fin_swi_2_goal_clean$x_coord_2)), 
                 y=((1/2)*(final_fin_swi_2_goal_clean$y_coord + final_fin_swi_2_goal_clean$y_coord_2))))+
  geom_point(x = fin_swi_2_recover_clean$x_coord, y = fin_swi_2_recover_clean$y_coord, 
             fill = 'pink', data = fin_swi_2_recover_clean, shape = 21, size = 6)

#------------------------------------
# possible substitution for the game clock subtitle
# subtitle = 'Finland vs Switzerland Bronze Medal Game, 02-16-2022, Goal with 57s left in 3rd Period'
# include whatever is deemed necessary/useful

#------------------------------------

#this one is real speed and saves the video
#animate(fin_swi_2_goal_p_2_clean, renderer=av_renderer('Perez/fin_swi_2.mp4'), duration = 10)

#slowed down a tiny bit, without saving the video
animate(fin_swi_2_goal_p_2_clean, renderer=av_renderer(), duration = 12)

#------------------------------------------


##THIS WAS FINLAND VS SWITZERLAND, POWER PLAY GOAL FOR FINLAND, P3 PP8


#------------------------
#------------------------









