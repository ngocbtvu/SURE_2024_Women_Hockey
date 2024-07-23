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
swi_can_track_clean <- read.csv('https://raw.githubusercontent.com/picagrad/Big-Data-Cup-2022/main/data/2022-02-14%20Switzerland%20at%20Canada/2022-02-14%20Switzerland%20at%20Canada%20P1%20PP2.csv')

#Calculate the seconds_remaninig from the frame_id
  #not doing this anymore because we'd have to manually check the number of frames and time and how they line up
  #maybe doable in the future when 'perfecting' this work

#CUT the tracking ddata to just those 10 seconds
swi_can_track_cut_clean <- swi_can_track_clean |> 
  filter(game_seconds>84 & game_seconds<95)

#NEST the data (for the animation?)
swi_can_nested_clean = swi_can_track_clean |> 
  group_by(frame_id) |> 
  nest()

swi_can_player_count_clean = swi_can_nested_clean |> 
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
pbp_swi_can <- subset(pbp_data, 
                      (grepl('Switzerland', team_name)) & 
                        (grepl('Canada', opp_team_name)))

#now cut it into the last 10 seconds before the goal
#period 2, leading up to 982 seconds left
swi_can_pbp_goal <- pbp_swi_can |> 
  filter(period == 1) |> 
  filter(clock_seconds>84 & clock_seconds<95)

swi_can_pbp_goal <- swi_can_pbp_goal |> 
  mutate(game_seconds = clock_seconds)

#now fuzzy join the pbp and tracking data by the clock_seconds and seconds_remaining

#i think i want a half second each way, or 0.4 seconds each way, 'll mess with it
#i want the shots and passes ot actually remain on screen for a little bit
library(dplyr)

#install.packages("fuzzyjoin")

# Load the package
library(fuzzyjoin)
#------------------------------------------
#There's some weird mirroring stuff going on, this fixes that

swi_can_pbp_goal <- swi_can_pbp_goal |> 
  mutate(x_coord = abs(x_coord-200)) |> 
  mutate(x_coord_2 = abs(x_coord_2-200))



#swi_can_track_cut_clean <- swi_can_track_cut_clean |> 
#  mutate(x_ft = abs(x_ft-200))
  

#------------------------------------------

#join the data, this still has the 'max_dist' line for a fuzzy join
#in case someone wants to calculate the decimal seconds in the tracking data 
#to make the animations a tiny bit more exact

final_swi_can_goal_clean <- difference_left_join(
  swi_can_track_cut_clean,
  swi_can_pbp_goal,
  by = 'game_seconds',
  max_dist = 0.35,
  distance_col = NULL
)
#------------------------------------------
swi_can_passes_clean <- final_swi_can_goal_clean |> 
  filter(event == 'Play')

swi_can_shots_clean <- final_swi_can_goal_clean |> 
  filter(event == 'Shot')

swi_can_recover_clean <- final_swi_can_goal_clean |> 
  filter(event == 'Takeaway')
#------------------------------------------
#actually animating stuff
#syntax is not the prettiest, but it works, can definitely follow convention better in the future

swi_can_goal_p_2_clean = plot_rink(ggplot(final_swi_can_goal_clean)) +
  geom_point(aes(x = x_ft, y = y_ft, fill = team_name.x), shape = 21, size = 6) +
  geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name.x), size = 3) +
  scale_colour_manual(values = c("Switzerland" = "white", "Canada" = "red")) +
  scale_fill_manual(values = c("Switzerland" = "red", "Canada" = "white")) +
  guides(colour = "none") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  transition_time(frame_id) +
  labs(title = 'Switzerland Power Play Goal',
       subtitle = 'Game Clock: {floor((219-(frame_time)/30)/60)}:{ceiling((219-(frame_time)/30)%%60)}',
       fill = "Team") +
  ease_aes()+
  NULL


swi_can_goal_p_2_clean <- swi_can_goal_p_2_clean + 
  geom_point(x = swi_can_shots_clean$x_coord, y = swi_can_shots_clean$y_coord, 
             fill = 'green3', data = swi_can_shots_clean, shape = 21, size = 6)+
  geom_segment(x = swi_can_passes_clean$x_coord, y = swi_can_passes_clean$y_coord, 
               xend = swi_can_passes_clean$x_coord_2, yend = swi_can_passes_clean$y_coord_2, colour = "skyblue",
               linewidth = 1.2, arrow = arrow(length = unit(0.2, "inches")), data = swi_can_passes_clean)+
  geom_label(aes(label=str_wrap(final_swi_can_goal_clean$x_coord,10), 
                 x=((1/2)*(final_swi_can_goal_clean$x_coord + final_swi_can_goal_clean$x_coord_2)), 
                 y=((1/2)*(final_swi_can_goal_clean$y_coord + final_swi_can_goal_clean$y_coord_2))))



swi_can_goal_p_2_clean <- swi_can_goal_p_2_clean +
  geom_point(x = swi_can_recover_clean$x_coord, y = swi_can_recover_clean$y_coord, 
             fill = 'pink', data = swi_can_recover_clean, shape = 21, size = 6)

#------------------------

#this one is real speed and saves the video
#animate(swi_can_goal_p_2_clean, renderer=av_renderer('Perez/swi_can.mp4'), duration = 10)

#slowed down a tiny bit, without saving the video
animate(swi_can_goal_p_2_clean, renderer=av_renderer(), duration = 12)



##THIS WAS SWITZERLAND VS CANADA, POWER PLAY GOAL FOR SWITZERLAND, P1 PP2

#------------------------
#------------------------
#------------------------
#------------------------









