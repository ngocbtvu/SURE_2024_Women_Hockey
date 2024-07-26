
#-----------------------------------------------------------------------
#making passes df

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
library(sportyR)

hock <- read.csv('Perez/women_hockey_data_with_sequence.csv')

hock <- hock |> 
  mutate(X.Coordinate = X.Coordinate-100) |> 
  mutate(Y.Coordinate = Y.Coordinate-42.5)

hock <- hock |> 
  mutate(X.Coordinate.2 = X.Coordinate.2-100) |> 
  mutate(Y.Coordinate.2 = Y.Coordinate.2-42.5)


power_plays <- hock |> 
  filter(Offense.Team.Skaters == 5 & Defense.Team.Skaters ==4)

rink <- geom_hockey("nhl")

rink

offense_rink = geom_hockey('nhl', 'offense')

pp_passes <- power_plays |> 
  filter(Event %in% c('Play', 'Incomplete Play')) |> 
  mutate(success = if_else(Event == 'Play', 1, 0))

passes <- hock |> 
  filter(Event %in% c('Play', 'Incomplete Play')) |> 
  mutate(success = if_else(Event == 'Play', 1, 0))

#-----------------------------------------------------------------------
#Make heatmap of passes in rink

#shading is completion%
#hexmap

#first map is pass origin
#second map is pass target


#library(patchwork)


  
rink + 
  stat_summary_hex(data=pp_passes, aes(x = X.Coordinate.2, y = Y.Coordinate.2, 
                                    z = success, group = -1),
                   binwidth = c(4, 4), fun = mean, 
                   color = "black", alpha= 0.6) +
  scale_fill_gradient(low = "blue", 
                      high = "red")+
  theme(legend.position = "bottom") +
  coord_fixed()+
  labs(title = 'Pass Success by Rink Zone',
       subtitle = 'Minimum of 25 Passes Recorded',
       fill = 'Pass Success%')+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

rink + 
  stat_summary_hex(data=pp_passes, aes(x = X.Coordinate, y = Y.Coordinate, 
                                       z = success, group = -1),
                   binwidth = c(4, 4), fun = mean, 
                   color = "black", alpha= 0.7) +
  scale_fill_gradient(low = "blue", 
                      high = "red")+
  theme(legend.position = "bottom") +
  coord_fixed()+
  labs(title = 'Pass Success by Rink Zone',
       subtitle = 'Minimum of 25 Passes Recorded',
       fill = 'Pass Success%')+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
