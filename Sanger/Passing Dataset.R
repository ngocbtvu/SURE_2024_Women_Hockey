library(tidyverse)

## Game 1 CSV's

#All 2022 womens olympic games event data
event_data <- read.csv("pxp_womens_oly_2022_v2.csv")

################# Game 1
#NOTE:: 2022-02-08 Canada at USA == G1

#2022-02-08 Canada at USA event data
event_G1 = subset(event_data, (game_date == "8/2/2022") & 
                    (team_name == "Olympic (Women) - United States" |
                    team_name == "Olympic (Women) - Canada"))

rost_G1 = read.csv("2022-02-08 Canada at USA roster.csv")
rost_G1$jersey_number = rost_G1$jn
rost_G1$jn = NULL
###### Powerplay 1

#2022-02-08 Canada at USA Powerplay 1 Tracking Data
track_G1_PP1 <- read.csv("2022-02-08 Canada at USA P1 PP1.csv")

#Powerplay 1 event data (starts at 376 not 386)
event_G1_PP1 <- subset(event_G1, clock_seconds <= 376 & 
                                 clock_seconds >= 350 &
                         period == 1 & 
                         situation_type == "5 on 4")

G1_PP1_st = 386 #how to include period = 1??

better_track_G1_PP1 = merge(track_G1_PP1, rost_G1, by = "jersey_number")
better_track_G1_PP1$clock_seconds = G1_PP1_st - 
  better_track_G1_PP1$frame_id/30 + 1/30


event_G1_PP1$stP3 = c("Brianne Jenner")
#i.v. = unique(event_G1_PP1$clock_seconds)+2
#event_G1_PP1$stP3.Tplus60f = 
#(better_track_G1_PP1$x_ft, better_track_G1_PP1$clock_seconds == i.v.)


#unique(subset(better_track_G1_PP1, team_name == "Canada")$player)[1]

###### Powerplay 2

#2022-02-08 Canada at USA Powerplay 2 Tracking Data
track_G1_PP2 <- read.csv("2022-02-08 Canada at USA P1 PP2.csv")

#Powerplay 2 event data
event_G1_PP2 <- subset(event_G1, ((clock_seconds <= 217 & 
                         clock_seconds >= 143) | 
                         (clock_seconds <= 136 & 
                            clock_seconds >= 111) |
                         (clock_seconds <= 106 & 
                            clock_seconds >= 96)) &
                         (period == 1 & 
                         situation_type == "5 on 4"))

###### Powerplay 3

#2022-02-08 Canada at USA Powerplay 3 Tracking Data
track_G1_PP3 <- read.csv("2022-02-08 Canada at USA P2 PP3.csv")

#Powerplay 3 event data
event_G1_PP3 <- subset(event_G1, ((clock_seconds <= 991 & 
                                     clock_seconds >= 976) | 
                                    (clock_seconds <= 970 & 
                                       clock_seconds >= 878) |
                                    (clock_seconds <= 848 & 
                                       clock_seconds >= 838)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 5

#2022-02-08 Canada at USA Powerplay 5 Tracking Data
track_G1_PP5 <- read.csv("2022-02-08 Canada at USA P2 PP5.csv")

#Powerplay 5 event data
event_G1_PP5 <- subset(event_G1, ((clock_seconds <= 272 & 
                                     clock_seconds >= 250) | 
                                    (clock_seconds <= 229 & 
                                       clock_seconds >= 204) |
                                    (clock_seconds <= 195 & 
                                       clock_seconds >= 189) |
                                    (clock_seconds <= 180 & 
                                       clock_seconds >= 169) |
                                    (clock_seconds <= 167 & 
                                       clock_seconds >= 128)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 6

#2022-02-08 Canada at USA Powerplay 6 Tracking Data
track_G1_PP6 <- read.csv("2022-02-08 Canada at USA P3 PP6.csv")

#Powerplay 6 event data
event_G1_PP6 <- subset(event_G1, ((clock_seconds <= 545 & 
                                     clock_seconds >= 532) | 
                                    (clock_seconds <= 486 & 
                                       clock_seconds >= 480)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

###### Powerplay 7 -- This is 6 on 4

#2022-02-08 Canada at USA Powerplay 7 Tracking Data
track_G1_PP7 <- read.csv("2022-02-08 Canada at USA P3 PP7.csv")

#Powerplay 7 event data
event_G1_PP7 <- subset(event_G1, ((clock_seconds <= 157 & 
                                     clock_seconds >= 99) | 
                                    (clock_seconds <= 94 & 
                                       clock_seconds >= 37)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))


################# Game 2
#NOTE:: 2022-02-08 ROC at Finland == G2

#2022-02-08 ROC at Finland event data
event_G2 = subset(event_data, (game_date == "8/2/2022") & 
               (team_name == "Olympic (Women) - Olympic Athletes from Russia" |
                       team_name == "Olympic (Women) - Finland"))

###### Powerplay 8

#2022-02-08 ROC at Finland Powerplay 1 Tracking Data
track_G2_PP1 <- read.csv("2022-02-08 ROC at Finland P1 PP1.csv")

#Powerplay 8 event data
event_G2_PP1 <- subset(event_G2, (clock_seconds <= 189 & 
                                     clock_seconds >= 123) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

###### Powerplay 9

#2022-02-08 ROC at Finland Powerplay 2 Tracking Data
track_G2_PP2 <- read.csv("2022-02-08 ROC at Finland P2 PP2.csv")

#Powerplay 9 event data
event_G2_PP2 <- subset(event_G2, (clock_seconds <= 1047 & 
                                    clock_seconds >= 1021) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 10 -- no data

#2022-02-08 ROC at Finland Powerplay 3 Tracking Data
track_G2_PP3 <- read.csv("2022-02-08 ROC at Finland P2 PP3.csv")

#Powerplay 10 event data
event_G2_PP3 <- subset(event_G2, ((clock_seconds <= 905 & 
                                    clock_seconds >= 904) |
                                    (clock_seconds <= 903 & 
                                       clock_seconds >= 901)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 11

#2022-02-08 ROC at Finland Powerplay 4 Tracking Data
track_G2_PP4 <- read.csv("2022-02-08 ROC at Finland P2 PP4.csv")

#Powerplay 11 event data
event_G2_PP4 <- subset(event_G2, (clock_seconds <= 595 & 
                                     clock_seconds >= 584) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 12

#2022-02-08 ROC at Finland Powerplay 5 Tracking Data
track_G2_PP5 <- read.csv("2022-02-08 ROC at Finland P2 PP5.csv")

#Powerplay 12 event data
event_G2_PP5 <- subset(event_G2, ((clock_seconds <= 455 & 
                                    clock_seconds >= 449) | 
                                    (clock_seconds <= 426 & 
                                       clock_seconds >= 419) | 
                                    (clock_seconds <= 412 & 
                                       clock_seconds >= 338)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 13

#2022-02-08 ROC at Finland Powerplay 6 Tracking Data
track_G2_PP6 <- read.csv("2022-02-08 ROC at Finland P3 PP6.csv")

#Powerplay 13 event data
event_G2_PP6 <- subset(event_G2, ((clock_seconds <= 878 & 
                                     clock_seconds >= 858) | 
                                    (clock_seconds <= 853 & 
                                       clock_seconds >= 825) | 
                                    (clock_seconds <= 819 & 
                                       clock_seconds >= 764)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

################# Game 3
#NOTE:: 2022-02-12 Switzerland at ROC == G3

#2022-02-12 Switzerland at ROC event data
event_G3 = subset(event_data, (game_date == "12/2/2022") & 
                    (team_name == "Olympic (Women) - Olympic Athletes from Russia" |
                       team_name == "Olympic (Women) - Switzerland"))

###### Powerplay 14

#2022-02-12 Switzerland at ROC Powerplay 1 Tracking Data
track_G3_PP1 <- read.csv("2022-02-12 Switzerland at ROC P1 PP1.csv")

#Powerplay 14 event data
event_G3_PP1 <- subset(event_G3, ((clock_seconds <= 747 & 
                                     clock_seconds >= 706) | 
                                    (clock_seconds <= 695 & 
                                       clock_seconds >= 638) | 
                                    (clock_seconds <= 632 & 
                                       clock_seconds >= 629)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

###### Powerplay 15

#2022-02-12 Switzerland at ROC Powerplay 2 Tracking Data
track_G3_PP2 <- read.csv("2022-02-12 Switzerland at ROC P1 PP2.csv")

#Powerplay 15 event data
event_G3_PP2 <- subset(event_G3, ((clock_seconds <= 492 & 
                                     clock_seconds >= 461) | 
                                    (clock_seconds <= 458 & 
                                       clock_seconds >= 385) | 
                                    (clock_seconds <= 364 & 
                                       clock_seconds >= 346)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

###### Powerplay 16

#2022-02-12 Switzerland at ROC Powerplay 3 Tracking Data
track_G3_PP3 <- read.csv("2022-02-12 Switzerland at ROC P3 PP3.csv")

#Powerplay 16 event data
event_G3_PP3 <- subset(event_G3, ((clock_seconds <= 740 & 
                                     clock_seconds >= 702) | 
                                    (clock_seconds <= 664 & 
                                       clock_seconds >= 631) | 
                                    (clock_seconds <= 598 & 
                                       clock_seconds >= 564)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

###### Powerplay 17

#2022-02-12 Switzerland at ROC Powerplay 5 Tracking Data
track_G3_PP5 <- read.csv("2022-02-12 Switzerland at ROC P3 PP5.csv")

#Powerplay 17 event data
event_G3_PP5 <- subset(event_G3, ((clock_seconds <= 105 & 
                                     clock_seconds >= 87) | 
                                    (clock_seconds <= 76 & 
                                       clock_seconds >= 37)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

################# Game 4
#NOTE:: 2022-02-14 Switzerland at Canada == G4

#2022-02-14 Switzerland at Canada event data
event_G4 = subset(event_data, (game_date == "14/2/2022") & 
                    (team_name == "Olympic (Women) - Canada" |
                       team_name == "Olympic (Women) - Switzerland"))

###### Powerplay 18

#2022-02-14 Switzerland at Canada Powerplay 1 Tracking Data
track_G4_PP1 <- read.csv("2022-02-14 Switzerland at Canada P1 PP1.csv")

#Powerplay 18 event data
event_G4_PP1 <- subset(event_G4, ((clock_seconds <= 310 & 
                                     clock_seconds >= 297) | 
                                    (clock_seconds <= 268 & 
                                       clock_seconds >= 240) | 
                                    (clock_seconds <= 219 & 
                                       clock_seconds >= 205)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

###### Powerplay 19

#2022-02-14 Switzerland at Canada Powerplay 2 Tracking Data
track_G4_PP2 <- read.csv("2022-02-14 Switzerland at Canada P1 PP2.csv")

#Powerplay 19 event data
event_G4_PP2 <- subset(event_G4, ((clock_seconds <= 146 & 
                                     clock_seconds >= 142) | 
                                    (clock_seconds <= 133 & 
                                       clock_seconds >= 111) | 
                                    (clock_seconds <= 107 & 
                                       clock_seconds >= 83)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

###### Powerplay 20

#2022-02-14 Switzerland at Canada Powerplay 4 Tracking Data
track_G4_PP4 <- read.csv("2022-02-14 Switzerland at Canada P2 PP4.csv")

#Powerplay 20 event data
event_G4_PP4 <- subset(event_G4, ((clock_seconds <= 206 & 
                                     clock_seconds >= 101)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 21

#2022-02-14 Switzerland at Canada Powerplay 5 Tracking Data
track_G4_PP5 <- read.csv("2022-02-14 Switzerland at Canada P3 PP5.csv")

#Powerplay 21 event data
event_G4_PP5 <- subset(event_G4, ((clock_seconds <= 675 & 
                                     clock_seconds >= 634) | 
                                    (clock_seconds <= 622 & 
                                       clock_seconds >= 573) | 
                                    (clock_seconds <= 568 & 
                                       clock_seconds >= 557)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))



######## Combining Datasets

event_G1tG4 = rbind(event_G1_PP1, event_G1_PP2, event_G1_PP3, event_G1_PP5, event_G1_PP6,
      event_G2_PP1, event_G2_PP2, event_G2_PP4, event_G2_PP5, event_G2_PP6, 
      event_G3_PP1, event_G3_PP2, event_G3_PP3, event_G3_PP5, 
      event_G4_PP1, event_G4_PP2, event_G4_PP4, event_G4_PP5)

pass_G1tG4 = subset(event_G1tG4, event == "Play")

pass_G1tG4$pass_dist = sqrt((pass_G1tG4$x_coord-pass_G1tG4$x_coord_2)^2 + 
                             (pass_G1tG4$y_coord-pass_G1tG4$y_coord_2)^2)
#pass_G1tG4$event_detail_1 = NULL
#pass_G1tG4$event_detail_2 = NULL
#pass_G1tG4$event_detail_3 = NULL
