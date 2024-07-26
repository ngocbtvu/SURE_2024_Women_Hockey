#lets try again
#now we know that we have to check the 'videShotsInfo' table to see what seconds are included in the tracking data

library(tidyverse)
library(gganimate)
library(ggforce)
library(dplyr)
source("Perez/plot_rink.R")

library(formattable)

pass_model = read.csv('Perez/pass_model copy.csv')

#loading all the power plays
pp_numbers <- read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/pp_info.csv')

#-------------------------------------------------------------------------

#load all the play by play
pbp_data = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv")

pbp_data <- pbp_data |> 
  left_join(select(pass_model, game_date, period, clock_seconds, event, x_coord, y_coord, min_dist_from_pl_def_percent, sec_dist_from_pl_def_percent), 
            by = c('game_date', 'period', 'clock_seconds', 'event', 'x_coord', 'y_coord'))

#filter into just power play goals
pbp_pp_goals <- pbp_data |> 
  filter(event == 'Shot') |> 
  filter(event_successful == 't') |> 
  filter(situation_type == '5 on 4')


#-------------------------------------------------------------------------
#LOAD the data
can_usa_track_clean <- read.csv('https://raw.githubusercontent.com/picagrad/Big-Data-Cup-2022/main/data/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P1%20PP1.csv')

#Calculate the seconds_remaninig from the frame_id


#CUT the tracking ddata to just those 10 seconds
can_usa_track_cut_clean <- can_usa_track_clean |> 
  filter(game_seconds>351 & game_seconds<362)

#NEST the data (for the animation?)
can_usa_nested_clean = can_usa_track_clean |> 
  group_by(frame_id) |> 
  nest()

can_usa_player_count_clean = can_usa_nested_clean |> 
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
pbp_can_usa <- subset(pbp_data, 
                      (grepl('Canada', team_name)) & 
                        (grepl('United States', opp_team_name)))

#now cut it into the last 10 seconds before the goal
#period 2, leading up to 982 seconds left
can_usa_pbp_goal <- pbp_can_usa |> 
  filter(period == 1) |> 
  filter(clock_seconds>351 & clock_seconds<362)

can_usa_pbp_goal <- can_usa_pbp_goal |> 
  mutate(game_seconds = clock_seconds)

#now fuzzy join the pbp and tracking data by the clock_seconds and seconds_remaining

#i think i want a half second each way, or 0.4 seconds each way, 'll mess with it
#i want the shots and passes ot actually remain on screen for a little bit
library(dplyr)

#install.packages("fuzzyjoin")

# Load the package
library(fuzzyjoin)
#------------------------------------------
final_can_usa_goal_clean <- difference_left_join(
  can_usa_track_cut_clean,
  can_usa_pbp_goal,
  by = 'game_seconds',
  max_dist = 0.35,
  distance_col = NULL
)

#------------------------------------------

can_usa_passes_clean <- final_can_usa_goal_clean |> 
  filter(event == 'Play')

can_usa_shots_clean <- final_can_usa_goal_clean |> 
  filter(event == 'Shot')

#can_usa_recover_clean <- final_can_usa_goal_clean |> 
#  filter(event == 'Takeaway')
#------------------------------------------

can_usa_goal_p_2_clean = plot_rink(ggplot(final_can_usa_goal_clean)) +
  geom_point(aes(x = x_ft, y = y_ft, fill = team_name.x), shape = 21, size = 6) +
  geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name.x), size = 3) +
  scale_colour_manual(values = c("USA" = "blue", "Canada" = "white")) +
  scale_fill_manual(values = c("USA" = "white", "Canada" = "red")) +
  guides(colour = "none") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  transition_time(frame_id) +
  labs(title = 'Canada Power Play Goal',
       subtitle = 'Game Clock: {floor((346-(frame_time)/30)/60)}:{ceiling((346-(frame_time)/30)%%60)}',
       fill = "Team") +
  ease_aes()+
  NULL

can_usa_goal_p_2_clean <- can_usa_goal_p_2_clean + 
  geom_point(x = can_usa_shots_clean$x_coord, y = can_usa_shots_clean$y_coord, 
             fill = 'green3', data = can_usa_shots_clean, shape = 21, size = 6)+
  geom_segment(x = can_usa_passes_clean$x_coord, y = can_usa_passes_clean$y_coord, 
               xend = can_usa_passes_clean$x_coord_2, yend = can_usa_passes_clean$y_coord_2, colour = "skyblue",
               linewidth = 1.2, arrow = arrow(length = unit(0.2, "inches")), data = can_usa_passes_clean)+
  geom_label(aes(label=str_wrap(percent((min_dist_from_pl_def_percent), accuracy = 0.1), 12)), 
                 x=((1/2)*(final_can_usa_goal_clean$x_coord + final_can_usa_goal_clean$x_coord_2)), 
                 y=((1/2)*(final_can_usa_goal_clean$y_coord + final_can_usa_goal_clean$y_coord_2)))


#can_usa_goal_p_2_clean <- can_usa_goal_p_2_clean +
 # geom_point(x = can_usa_recover_clean$x_coord, y = can_usa_recover_clean$y_coord, 
#             fill = 'pink', data = can_usa_recover_clean, shape = 21, size = 6)


#this one is real speed and saves the video
animate(can_usa_goal_p_2_clean, renderer=av_renderer('Perez/can_usa.mp4'), duration = 10)

#slowed down a tiny bit, without saving the video
animate(can_usa_goal_p_2_clean, renderer=av_renderer(), duration = 12)



##THIS WAS CANADA VS USA, POWER PLAY GOAL FOR CANADA, P1 PP1

#------------------------
#------------------------
#------------------------
#------------------------









