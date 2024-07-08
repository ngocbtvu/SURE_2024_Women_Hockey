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

#install.packages('ggimage')
library(ggimage)


library(ggalluvial)

hock <- read.csv('BDC_2024_Womens_Data copy.csv')

power_plays <- hock |> 
  filter((Home.Team.Skaters == 5 & Away.Team.Skaters ==4)|
           (Home.Team.Skaters == 4 & Away.Team.Skaters ==5))

power_shots <- filter(power_plays, Event %in% c("Shot","Goal"))

power_shots <- power_shots |> 
  mutate(X.Coordinate = X.Coordinate-100) |> 
  mutate(Y.Coordinate = Y.Coordinate-42.5)

power_plays <- power_plays |> 
  mutate(X.Coordinate = X.Coordinate-100) |> 
  mutate(Y.Coordinate = Y.Coordinate-42.5)


#devtools::install_github("danmorse314/hockeyR")
# get single game
library(hockeyR)
library(sportyR)

pbp <- load_pbp('2018-19')

#shot distance distribution by shot type
library(ggridges)
power_shots |> 
  ggplot(aes(X.Coordinate, Detail.1))+
  geom_density_ridges()

#-------------
#loading the full rink and offensive rink plots

rink <- geom_hockey("nhl")

rink

offense_rink = geom_hockey('nhl', 'offense')

#heatmaps of all shots during power plays

rink +
  geom_hex(data = power_shots, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(4,4)) + 
  scale_fill_gradient(low = "lightgreen", 
                      high = "forestgreen")

offense_rink +
  geom_hex(data = power_shots, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(4,4)) + 
  scale_fill_gradient(low = "lightgreen", 
                      high = "forestgreen")

#------------------------------------------------------------------------------
#heatmap of power play shots, color by shot type
#does not work, Error in `scale_fill_gradient()`:
#! Discrete values supplied to continuous scale.
#offense_rink +
#  geom_hex(data = power_shots, 
#           aes(x = X.Coordinate, y = Y.Coordinate, fill = Detail.1), 
#           binwidth = c(4,4))+
#  scale_fill_gradient()
#------------------------------------------------------------------------------

unique(power_shots$Detail.1)
  
#attempt 2 at heatmap color by shot type
shot_colors <- c('Snapshot' = 'red', 'Slapshot' = 'blue', 'Wristshot' = 'brown',
                'Deflection' = 'purple', 'Wrap Around' = 'deeppink1', 
                 'Bat' = 'aquamarine', 'Fan' = 'darkorange1')

#------------------------------------------------------------------------------
# Create a ggplot object for customization
#does not work, can't add ggplot to 'e2' type (rink)
#rink+
#  ggplot(power_shots, aes(x = X.Coordinate, y = Y.Coordinate)) +
#  theme_void() +  # Remove default ggplot theme
#  geom_hex(aes(fill = Detail.1), bins = 20) +  # Adjust bins for desired heatmap resolution
#  scale_fill_manual(values = shot_colors) +  # Set fill colors based on shot type# Add hockey rink background
#  coord_equal()  
#------------------------------------------------------------------------------

#creating aa df for all shots recorded
all_shots <- filter(hock, Event %in% c("Shot","Goal"))

all_shots <-  all_shots |> 
  mutate(X.Coordinate = X.Coordinate-100) |> 
  mutate(Y.Coordinate = Y.Coordinate-42.5)


offense_rink +
  geom_hex(data = all_shots, 
           aes(x = X.Coordinate, y = Y.Coordinate, color = Detail.1), 
           binwidth = c(7,7))+
  theme_void()+
  scale_color_manual(values = shot_colors)+
  scale_fill_gradient(low = "lightgreen", 
                      high = "forestgreen")+
  facet_wrap(~ Detail.1, nrow = 3)



top_shot_types |> 
  count(Detail.1)

top_shot_types <- filter(all_shots, Detail.1 %in% c('Snapshot', 'Deflection', 
                                                    'Slapshot','Wristshot'))

offense_rink +
  geom_hex(data = top_shot_types, 
           aes(x = X.Coordinate, y = Y.Coordinate, color = Detail.1), 
           binwidth = c(10,10),
           alpha=0.7)+
  theme_solarized()+
  scale_color_manual(values = shot_colors)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ Detail.1, nrow = 2)+
  labs(title = 'Shot Type Heatmaps',
       subtitle = 'minimum of 25 shot attempts',
       fill = 'Number of Shots') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  guides(color='none')+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


#-----------------------------------------------------------------------
#power play formations, attempt 1
#dont touch
#power_plays

#pp_player_1 <- rink +
#  geom_hex(data = power_plays, 
#           aes(x = X.Coordinate, y = Y.Coordinate), 
#           binwidth = c(4,4)) + 
#  scale_fill_gradient(low = "lightgreen", 
#                      high = "forestgreen")


#pp_player_1
#pp_player_2
#-----------------------------------------------------------------------


offense_rink +
  geom_hex(data = all_shots, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(6,6))+ 
  scale_fill_gradient(low = "lightgreen", 
                      high = "forestgreen")

rink +
  geom_hex(data = all_shots, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(6,6))+ 
  scale_fill_gradient(low = "lightgreen", 
                      high = "forestgreen")


#-----------------------------------------------------------------------
#start of set up for power play formation heatmaps

#calculating and plotting time between power play events

glimpse(power_plays)

convert_time_to_secs <- function(time_str) {
  time_parts <- as.numeric(strsplit(time_str, ":")[[1]])
  minutes <- time_parts[1]
  seconds <- time_parts[2]
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

power_plays <- power_plays |> 
  mutate(seconds_remaining = (sapply(Clock, convert_time_to_secs)) +(60*20*(3-Period)))


power_plays <- power_plays |> 
  mutate(time_elapsed = seconds_remaining - lag(seconds_remaining, default = first(seconds_remaining)))

power_plays <- power_plays |> 
  mutate(time_elapsed = -1*time_elapsed)

#-----------
power_plays |> 
  ggplot(aes(time_elapsed))+
  geom_histogram()

power_plays |> 
  ggplot(aes(time_elapsed))+
  geom_density()


#-----------------------------------------------------------------------
#numbering each power play

#trying to make a time difference threshold to use for 
#power play identifier/counter to group by power play

time_diff_threshold <- 15  
player_diff_threshold <- 1

power_plays <- power_plays |> 
  mutate(home_skaters_changed = Home.Team.Skaters - lag(Home.Team.Skaters, default = first(Home.Team.Skaters)))

power_plays <- power_plays |> 
  mutate(away_skaters_changed = Away.Team.Skaters - lag(Away.Team.Skaters, default = first(Away.Team.Skaters)))

power_plays$power_play_id <- cumsum(power_plays$time_elapsed > time_diff_threshold | 
                                      power_plays$home_skaters_changed >= player_diff_threshold |
                                      power_plays$away_skaters_changed >= player_diff_threshold)



#-------------------------------------------------------
#attempt at heatmap of each power play
#this function is useless right now
#plot_power_play_heatmap <- function(power_plays) {
#  heatmap <- rink +
#    geom_hex(data = power_plays, 
#             aes(x = X.Coordinate, y = Y.Coordinate), 
#             binwidth = c(3,3))+ 
#    scale_fill_gradient(low = "lightgreen", 
#                        high = "forestgreen")
#  return(heatmap)
#}
#also useless rn

#power_plays <- power_plays[order(power_plays$Period, power_plays$seconds_remaining), ]

# Group events by power_play_id
#also useless
#power_play_groups <- split(power_plays, power_plays$power_play_id)

# Initialize a list to store plots
#also useless

#heatmap_list <- list()

# Loop through each power play group
#also useless

#for (i in seq_along(power_play_groups)) {
#  current_power_play <- power_play_groups[[i]]
  # Plot heatmap for the current power play group
#  heatmap <- plot_power_play_heatmap(current_power_play)
  # Store the heatmap in the list
#  heatmap_list[[i]] <- heatmap
#}
# Display or save each heatmap
#for (i in seq_along(heatmap_list)) {
#  print(heatmap_list[[i]])
  # You can also save each heatmap using ggsave() or another function
  # ggsave(filename = paste0("power_play_", i, "_heatmap.png"), plot = heatmap_list[[i]], width = 8, height = 6)
#}



#----------------------------------

#heatmap of power play 1

pp_1 <- power_plays |> 
  filter(power_play_id == 1)

offense_rink +
  geom_hex(data = pp_1, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(6,6),
           alpha=0.9)+
  theme_void()+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 6)
#----------------------------------

#heatmaps of all 31 power plays
offense_rink +
  geom_hex(data = power_plays, 
           aes(x = X.Coordinate, y = Y.Coordinate, color = power_play_id), 
           binwidth = c(6,6),
           alpha=0.9)+
  theme_void()+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 6)

#heatmaps
#--------------------------------------------------------------------
#filter down power plays by relevant events (passes, shots, zone entries)
unique(power_plays$Event)

glimpse(power_plays)

power_plays_relevant <- power_plays |> 
  filter(Event %in% c("Play", "Incomplete Play", "Shot", "Zone Entry","Goal")) |> 
  mutate(X.Coordinate.2 = replace_na(X.Coordinate.2, 900),
         Y.Coordinate.2 = replace_na(Y.Coordinate.2, 900))


power_plays_relevant <- power_plays_relevant |> 
  mutate(X.Coordinate.2 = X.Coordinate.2-100) |> 
  mutate(Y.Coordinate.2 = Y.Coordinate.2-42.5)


#heatmap of each power play
#all 31 pp's but only relevant events, this time both players 1 and 2
#this one takes very long to run (for me)
offense_rink +
  geom_hex(data = power_plays_relevant, 
           aes(x = X.Coordinate, y = Y.Coordinate, color = power_play_id), 
           binwidth = c(6,6),
           alpha=0.9)+
  geom_hex(data = power_plays_relevant, 
           aes(x = X.Coordinate.2, y = Y.Coordinate.2, color = power_play_id), 
           binwidth = c(6,6),
           alpha=0.9)+
  theme_void()+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 6)


#just a few power play heatmaps
power_plays_plots_1 <- power_plays_relevant |> 
  filter(power_play_id %in% c(0,2,6,16,17,19,20,23,24,26,29))


offense_rink +
  geom_hex(data = power_plays_plots_1, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(11,11),
           alpha=0.9)+
  geom_hex(data = power_plays_plots_1, 
           aes(x = X.Coordinate.2, y = Y.Coordinate.2), 
           binwidth = c(11,11),
           alpha=0.9)+
  theme_void()+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 2)


#most useful pp heatmaps so far
power_plays_plots_2 <- power_plays_relevant |> 
  filter(power_play_id %in% c(0,6,16,17,19,20,23,29))

#6 and 19, 
#16 and 23

power_plays_plots_2 |> 
  count(power_play_id)

offense_rink +
  theme_solarized()+
  geom_hex(data = power_plays_plots_2, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(9,9),
           alpha=0.9)+
  geom_hex(data = power_plays_plots_2, 
           aes(x = X.Coordinate.2, y = Y.Coordinate.2), 
           binwidth = c(9,9),
           alpha=0.9)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 2)+
  labs(title = 'Power Play Heatmaps',
       subtitle = 'Minimum of 25 Events Recorded',
       fill = 'Number of Events') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

#just 2 pairs of plots
power_plays_plots_4 <- power_plays_relevant |> 
  filter(power_play_id %in% c(6,19))

power_plays_plots_5 <- power_plays_relevant |> 
  filter(power_play_id %in% c(16,23))

offense_rink +
  theme_solarized()+
  geom_hex(data = power_plays_plots_4, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(11,11),
           alpha=0.9)+
  geom_hex(data = power_plays_plots_4, 
           aes(x = X.Coordinate.2, y = Y.Coordinate.2), 
           binwidth = c(11,11),
           alpha=0.9)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 1)+
  labs(title = 'Power Play Heatmaps',
       subtitle = 'Minimum of 25 Events Recorded',
       fill = 'Number of Events') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

offense_rink +
  theme_solarized()+
  geom_hex(data = power_plays_plots_5, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(11,11),
           alpha=0.9)+
  geom_hex(data = power_plays_plots_5, 
           aes(x = X.Coordinate.2, y = Y.Coordinate.2), 
           binwidth = c(11,11),
           alpha=0.9)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 1)+
  labs(title = 'Power Play Heatmaps',
       subtitle = 'Minimum of 25 Events Recorded',
       fill = 'Number of Events') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())



#the most different power play heatmaps
power_plays_plots_6 <- power_plays_relevant |> 
  filter(power_play_id %in% c(0, 16, 17, 20, 29))

offense_rink +
  theme_solarized()+
  geom_hex(data = power_plays_plots_6, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(10,10),
           alpha=0.9)+
  geom_hex(data = power_plays_plots_6, 
           aes(x = X.Coordinate.2, y = Y.Coordinate.2), 
           binwidth = c(10,10),
           alpha=0.9)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 2)+
  labs(title = 'Power Play Heatmaps',
       subtitle = 'Minimum of 25 Events Recorded',
       fill = 'Number of Events') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


#most useful plots so far

#heatmap by shot type (most common shot types)
offense_rink +
  theme_solarized()+
  geom_hex(data = top_shot_types, 
           aes(x = X.Coordinate, y = Y.Coordinate, color = Detail.1), 
           binwidth = c(10,10),
           alpha=0.7)+
  scale_color_manual(values = shot_colors)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ Detail.1, nrow = 2)+
  labs(title = 'Shot Type Heatmaps',
       subtitle = 'minimum of 25 shot attempts',
       fill = 'Number of Shots') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  guides(color='none')+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

#shot heatmap with dots instead of hexagons
offense_rink +
  theme_solarized()+
  geom_point(data = top_shot_types, 
           aes(x = X.Coordinate, y = Y.Coordinate, color = Detail.1), 
           alpha = 0.6,
           size = 5)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ Detail.1, nrow = 2)+
  labs(title = 'Shot Type Heatmaps',
       subtitle = 'minimum of 25 shot attempts',
       fill = 'Number of Shots') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  guides(color='none')+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

#try with a typical heatmap
#i haven't figured this out yet

offense_rink +
  theme_solarized()+
  geom_hex(data = power_plays_plots_6, 
           aes(x = X.Coordinate, y = Y.Coordinate), 
           binwidth = c(6,6),
           alpha=0.9)+
  geom_hex(data = power_plays_plots_6, 
           aes(x = X.Coordinate.2, y = Y.Coordinate.2), 
           binwidth = c(6,6),
           alpha=0.9)+
  scale_fill_gradient(low = "lightgoldenrod", 
                      high = "darkcyan")+
  facet_wrap(~ power_play_id, nrow = 2)+
  labs(title = 'Power Play Heatmaps',
       subtitle = 'Minimum of 25 Events Recorded',
       fill = 'Number of Events') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())



#-----------------------------------------------------------------------