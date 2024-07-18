library(tidyverse)
library(dplyr)
library(tidyr)

## Game 1 CSV's

#All 2022 womens olympic games event data
event_data <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv")

################# Game 1
#NOTE:: 2022-02-08 Canada at USA == G1

#2022-02-08 Canada at USA event data
event_G1 = subset(event_data, (game_date == "8/2/2022") & 
                    (team_name == "Olympic (Women) - United States" |
                    team_name == "Olympic (Women) - Canada"))

rost_G1 = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20roster.csv")
rost_G1$team_name = ifelse(rost_G1$team == "home", "USA", "Canada")
colnames(rost_G1)[2] = "jersey_number"
rost_G1[41,] = c(NA, 100, NA, "home", "USA")
rost_G1[42,] = c(NA, 100, NA, "away", "Canada")

###### Powerplay 1

#2022-02-08 Canada at USA Powerplay 1 Tracking Data
track_G1_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P1%20PP1.csv")

#Powerplay 1 event data (starts at 376 not 386)
event_G1_PP1 <- subset(event_G1, clock_seconds <= 376 & 
                                 clock_seconds >= 350 &
                         period == 1 & 
                         situation_type == "5 on 4")

G1_PP1_st = 386 #how to include period = 1??

better_track_G1_PP1 = merge(track_G1_PP1, rost_G1, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G1_PP1$clock_seconds = G1_PP1_st - 
  better_track_G1_PP1$frame_id/30 + 1/30

event_G1_PP1.t = merge(event_G1_PP1, better_track_G1_PP1, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 2

#2022-02-08 Canada at USA Powerplay 2 Tracking Data
track_G1_PP2 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P1%20PP2.csv")

#Powerplay 2 event data
event_G1_PP2 <- subset(event_G1, ((clock_seconds <= 217 & 
                         clock_seconds >= 143) | 
                         (clock_seconds <= 136 & 
                            clock_seconds >= 111) |
                         (clock_seconds <= 106 & 
                            clock_seconds >= 96)) &
                         (period == 1 & 
                         situation_type == "5 on 4"))

G1_PP2_st = 216

better_track_G1_PP2 = merge(track_G1_PP2, rost_G1, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G1_PP2$clock_seconds = G1_PP2_st - 
  better_track_G1_PP2$frame_id/30 + 1/30

event_G1_PP2.t = merge(event_G1_PP2, better_track_G1_PP2, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 3

#2022-02-08 Canada at USA Powerplay 3 Tracking Data
track_G1_PP3 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P2%20PP3.csv")

#Powerplay 3 event data
event_G1_PP3 <- subset(event_G1, ((clock_seconds <= 991 & 
                                     clock_seconds >= 976) | 
                                    (clock_seconds <= 970 & 
                                       clock_seconds >= 878) |
                                    (clock_seconds <= 848 & 
                                       clock_seconds >= 838)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G1_PP3_st = 991

better_track_G1_PP3 = merge(track_G1_PP3, rost_G1, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G1_PP3$clock_seconds = G1_PP3_st - 
  better_track_G1_PP3$frame_id/30 + 1/30

event_G1_PP3.t = merge(event_G1_PP3, better_track_G1_PP3, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 5

#2022-02-08 Canada at USA Powerplay 5 Tracking Data
track_G1_PP5 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P2%20PP5.csv")

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

G1_PP5_st = 272

better_track_G1_PP5 = merge(track_G1_PP5, rost_G1, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G1_PP5$clock_seconds = G1_PP5_st - 
  better_track_G1_PP5$frame_id/30 + 1/30

event_G1_PP5.t = merge(event_G1_PP5, better_track_G1_PP5, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 6

#2022-02-08 Canada at USA Powerplay 6 Tracking Data
track_G1_PP6 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P3%20PP6.csv")

#Powerplay 6 event data
event_G1_PP6 <- subset(event_G1, ((clock_seconds <= 545 & 
                                     clock_seconds >= 532) | 
                                    (clock_seconds <= 486 & 
                                       clock_seconds >= 480)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G1_PP6_st = 545

better_track_G1_PP6 = merge(track_G1_PP6, rost_G1, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G1_PP6$clock_seconds = G1_PP6_st - 
  better_track_G1_PP6$frame_id/30 + 1/30

event_G1_PP6.t = merge(event_G1_PP6, better_track_G1_PP6, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 7 -- This is 6 on 4

#2022-02-08 Canada at USA Powerplay 7 Tracking Data
track_G1_PP7 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P3%20PP7.csv")

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

rost_G2 = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20roster.csv")
rost_G2$team_name = ifelse(rost_G2$team == "home", "Finland", "ROC")
colnames(rost_G2)[2] = "jersey_number"
rost_G2[42,] = c(NA, 100, NA, "home", "Finland")
rost_G2[43,] = c(NA, 100, NA, "away", "ROC")

###### Powerplay 8

#2022-02-08 ROC at Finland Powerplay 1 Tracking Data
track_G2_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P1%20PP1.csv")

#Powerplay 8 event data
event_G2_PP1 <- subset(event_G2, (clock_seconds <= 189 & 
                                     clock_seconds >= 123) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G2_PP1_st = 192

better_track_G2_PP1 = merge(track_G2_PP1, rost_G2, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G2_PP1$clock_seconds = G2_PP1_st - 
  better_track_G2_PP1$frame_id/30 + 1/30

event_G2_PP1.t = merge(event_G2_PP1, better_track_G2_PP1, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 9

#2022-02-08 ROC at Finland Powerplay 2 Tracking Data
track_G2_PP2 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P2%20PP2.csv")

#Powerplay 9 event data
event_G2_PP2 <- subset(event_G2, (clock_seconds <= 1047 & 
                                    clock_seconds >= 1021) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G2_PP2_st = 1059

better_track_G2_PP2 = merge(track_G2_PP2, rost_G2, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G2_PP2$clock_seconds = G2_PP2_st - 
  better_track_G2_PP2$frame_id/30 + 1/30

event_G2_PP2.t = merge(event_G2_PP2, better_track_G2_PP2, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 10 -- no data

#2022-02-08 ROC at Finland Powerplay 3 Tracking Data
track_G2_PP3 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P2%20PP3.csv")

#Powerplay 10 event data
event_G2_PP3 <- subset(event_G2, ((clock_seconds <= 905 & 
                                    clock_seconds >= 904) |
                                    (clock_seconds <= 903 & 
                                       clock_seconds >= 901)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

###### Powerplay 11

#2022-02-08 ROC at Finland Powerplay 4 Tracking Data
track_G2_PP4 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P2%20PP4.csv")

#Powerplay 11 event data
event_G2_PP4 <- subset(event_G2, (clock_seconds <= 595 & 
                                     clock_seconds >= 584) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G2_PP4_st = 597

better_track_G2_PP4 = merge(track_G2_PP4, rost_G2, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G2_PP4$clock_seconds = G2_PP4_st - 
  better_track_G2_PP4$frame_id/30 + 1/30

event_G2_PP4.t = merge(event_G2_PP4, better_track_G2_PP4, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 12

#2022-02-08 ROC at Finland Powerplay 5 Tracking Data
track_G2_PP5 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P2%20PP5.csv")

#Powerplay 12 event data
event_G2_PP5 <- subset(event_G2, ((clock_seconds <= 455 & 
                                    clock_seconds >= 449) | 
                                    (clock_seconds <= 426 & 
                                       clock_seconds >= 419) | 
                                    (clock_seconds <= 412 & 
                                       clock_seconds >= 338)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G2_PP5_st = 458

better_track_G2_PP5 = merge(track_G2_PP5, rost_G2, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G2_PP5$clock_seconds = G2_PP5_st - 
  better_track_G2_PP5$frame_id/30 + 1/30

event_G2_PP5.t = merge(event_G2_PP5, better_track_G2_PP5, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 13

#2022-02-08 ROC at Finland Powerplay 6 Tracking Data
track_G2_PP6 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P3%20PP6.csv")

#Powerplay 13 event data
event_G2_PP6 <- subset(event_G2, ((clock_seconds <= 878 & 
                                     clock_seconds >= 858) | 
                                    (clock_seconds <= 853 & 
                                       clock_seconds >= 825) | 
                                    (clock_seconds <= 819 & 
                                       clock_seconds >= 764)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G2_PP6_st = 880

better_track_G2_PP6 = merge(track_G2_PP6, rost_G2, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G2_PP6$clock_seconds = G2_PP6_st - 
  better_track_G2_PP6$frame_id/30 + 1/30

event_G2_PP6.t = merge(event_G2_PP6, better_track_G2_PP6, by = "clock_seconds", 
                       all.x = TRUE)

################# Game 3
#NOTE:: 2022-02-12 Switzerland at ROC == G3

#2022-02-12 Switzerland at ROC event data
event_G3 = subset(event_data, (game_date == "12/2/2022") & 
                    (team_name == "Olympic (Women) - Olympic Athletes from Russia" |
                       team_name == "Olympic (Women) - Switzerland"))

rost_G3 = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-12%20Switzerland%20at%20ROC/2022-02-12%20Switzerland%20at%20ROC%20roster.csv")
rost_G3$team_name = ifelse(rost_G3$team == "home", "ROC", "Switzerland")
colnames(rost_G3)[2] = "jersey_number"
rost_G3[42,] = c(NA, 100, NA, "home", "ROC")
rost_G3[43,] = c(NA, 100, NA, "away", "Switzerland")

###### Powerplay 14

#2022-02-12 Switzerland at ROC Powerplay 1 Tracking Data
track_G3_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-12%20Switzerland%20at%20ROC/2022-02-12%20Switzerland%20at%20ROC%20P1%20PP1.csv")

#Powerplay 14 event data
event_G3_PP1 <- subset(event_G3, ((clock_seconds <= 747 & 
                                     clock_seconds >= 706) | 
                                    (clock_seconds <= 695 & 
                                       clock_seconds >= 638) | 
                                    (clock_seconds <= 632 & 
                                       clock_seconds >= 629)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G3_PP1_st = 748

better_track_G3_PP1 = merge(track_G3_PP1, rost_G3, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G3_PP1$clock_seconds = G3_PP1_st - 
  better_track_G3_PP1$frame_id/30 + 1/30

event_G3_PP1.t = merge(event_G3_PP1, better_track_G3_PP1, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 15

#2022-02-12 Switzerland at ROC Powerplay 2 Tracking Data
track_G3_PP2 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-12%20Switzerland%20at%20ROC/2022-02-12%20Switzerland%20at%20ROC%20P1%20PP2.csv")

#Powerplay 15 event data
event_G3_PP2 <- subset(event_G3, ((clock_seconds <= 492 & 
                                     clock_seconds >= 461) | 
                                    (clock_seconds <= 458 & 
                                       clock_seconds >= 385) | 
                                    (clock_seconds <= 364 & 
                                       clock_seconds >= 346)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G3_PP2_st = 492

better_track_G3_PP2 = merge(track_G3_PP2, rost_G3, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G3_PP2$clock_seconds = G3_PP2_st - 
  better_track_G3_PP2$frame_id/30 + 1/30

event_G3_PP2.t = merge(event_G3_PP2, better_track_G3_PP2, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 16

#2022-02-12 Switzerland at ROC Powerplay 3 Tracking Data
track_G3_PP3 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-12%20Switzerland%20at%20ROC/2022-02-12%20Switzerland%20at%20ROC%20P3%20PP3.csv")

#Powerplay 16 event data
event_G3_PP3 <- subset(event_G3, ((clock_seconds <= 740 & 
                                     clock_seconds >= 702) | 
                                    (clock_seconds <= 664 & 
                                       clock_seconds >= 631) | 
                                    (clock_seconds <= 598 & 
                                       clock_seconds >= 564)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G3_PP3_st = 744

better_track_G3_PP3 = merge(track_G3_PP3, rost_G3, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G3_PP3$clock_seconds = G3_PP3_st - 
  better_track_G3_PP3$frame_id/30 + 1/30

event_G3_PP3.t = merge(event_G3_PP3, better_track_G3_PP3, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 17

#2022-02-12 Switzerland at ROC Powerplay 5 Tracking Data
track_G3_PP5 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-12%20Switzerland%20at%20ROC/2022-02-12%20Switzerland%20at%20ROC%20P3%20PP5.csv")

#Powerplay 17 event data
event_G3_PP5 <- subset(event_G3, ((clock_seconds <= 105 & 
                                     clock_seconds >= 87) | 
                                    (clock_seconds <= 76 & 
                                       clock_seconds >= 37)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G3_PP5_st = 107

better_track_G3_PP5 = merge(track_G3_PP5, rost_G3, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G3_PP5$clock_seconds = G3_PP5_st - 
  better_track_G3_PP5$frame_id/30 + 1/30

event_G3_PP5.t = merge(event_G3_PP5, better_track_G3_PP5, by = "clock_seconds", 
                       all.x = TRUE)

################# Game 4
#NOTE:: 2022-02-14 Switzerland at Canada == G4

#2022-02-14 Switzerland at Canada event data
event_G4 = subset(event_data, (game_date == "14/2/2022") & 
                    (team_name == "Olympic (Women) - Canada" |
                       team_name == "Olympic (Women) - Switzerland"))

rost_G4 = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Switzerland%20at%20Canada/2022-02-14%20Switzerland%20at%20Canada%20roster.csv")
rost_G4$team_name = ifelse(rost_G4$team == "home", "Canada", "Switzerland")
colnames(rost_G4)[2] = "jersey_number"
rost_G4[44,] = c(NA, 100, NA, "home", "Canada")
rost_G4[45,] = c(NA, 100, NA, "away", "Switzerland")

###### Powerplay 18

#2022-02-14 Switzerland at Canada Powerplay 1 Tracking Data
track_G4_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Switzerland%20at%20Canada/2022-02-14%20Switzerland%20at%20Canada%20P1%20PP1.csv")

#Powerplay 18 event data
event_G4_PP1 <- subset(event_G4, ((clock_seconds <= 310 & 
                                     clock_seconds >= 297) | 
                                    (clock_seconds <= 268 & 
                                       clock_seconds >= 240) | 
                                    (clock_seconds <= 219 & 
                                       clock_seconds >= 205)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G4_PP1_st = 312

better_track_G4_PP1 = merge(track_G4_PP1, rost_G4, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G4_PP1$clock_seconds = G4_PP1_st - 
  better_track_G4_PP1$frame_id/30 + 1/30

event_G4_PP1.t = merge(event_G4_PP1, better_track_G4_PP1, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 19

#2022-02-14 Switzerland at Canada Powerplay 2 Tracking Data
track_G4_PP2 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Switzerland%20at%20Canada/2022-02-14%20Switzerland%20at%20Canada%20P1%20PP2.csv")

#Powerplay 19 event data
event_G4_PP2 <- subset(event_G4, ((clock_seconds <= 146 & 
                                     clock_seconds >= 142) | 
                                    (clock_seconds <= 133 & 
                                       clock_seconds >= 111) | 
                                    (clock_seconds <= 107 & 
                                       clock_seconds >= 83)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G4_PP2_st = 172

better_track_G4_PP2 = merge(track_G4_PP2, rost_G4, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G4_PP2$clock_seconds = G4_PP2_st - 
  better_track_G4_PP2$frame_id/30 + 1/30

event_G4_PP2.t = merge(event_G4_PP2, better_track_G4_PP2, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 20

#2022-02-14 Switzerland at Canada Powerplay 4 Tracking Data
track_G4_PP4 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Switzerland%20at%20Canada/2022-02-14%20Switzerland%20at%20Canada%20P2%20PP4.csv")

#Powerplay 20 event data
event_G4_PP4 <- subset(event_G4, ((clock_seconds <= 206 & 
                                     clock_seconds >= 101)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G4_PP4_st = 209

better_track_G4_PP4 = merge(track_G4_PP4, rost_G4, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G4_PP4$clock_seconds = G4_PP4_st - 
  better_track_G4_PP4$frame_id/30 + 1/30

event_G4_PP4.t = merge(event_G4_PP4, better_track_G4_PP4, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 21

#2022-02-14 Switzerland at Canada Powerplay 5 Tracking Data
track_G4_PP5 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Switzerland%20at%20Canada/2022-02-14%20Switzerland%20at%20Canada%20P3%20PP5.csv")

#Powerplay 21 event data
event_G4_PP5 <- subset(event_G4, ((clock_seconds <= 675 & 
                                     clock_seconds >= 634) | 
                                    (clock_seconds <= 622 & 
                                       clock_seconds >= 573) | 
                                    (clock_seconds <= 568 & 
                                       clock_seconds >= 557)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G4_PP5_st = 677

better_track_G4_PP5 = merge(track_G4_PP5, rost_G4, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G4_PP5$clock_seconds = G4_PP5_st - 
  better_track_G4_PP5$frame_id/30 + 1/30

event_G4_PP5.t = merge(event_G4_PP5, better_track_G4_PP5, by = "clock_seconds", 
                       all.x = TRUE)

################# Game 5
#NOTE:: 2022-02-14 Finland at USA == G5

#2022-02-14 Finland at USA event data
event_G5 = subset(event_data, (game_date == "14/2/2022") & 
                    (team_name == "Olympic (Women) - Finland" |
                       team_name == "Olympic (Women) - United States"))

rost_G5 = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20roster.csv")
rost_G5$team_name = ifelse(rost_G5$team == "home", "USA", "Finland")
colnames(rost_G5)[2] = "jersey_number"
rost_G5[44,] = c(NA, 100, NA, "home", "USA")
rost_G5[45,] = c(NA, 100, NA, "away", "Finland")

###### Powerplay 22

#2022-02-14 Finland at USA Powerplay 1 Tracking Data
track_G5_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P2%20PP1.csv")

#Powerplay 22 event data
event_G5_PP1 <- subset(event_G5, ((clock_seconds <= 1068 & 
                                     clock_seconds >= 1027) | 
                                    (clock_seconds <= 1024 & 
                                       clock_seconds >= 994)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G5_PP1_st = 1068

better_track_G5_PP1 = merge(track_G5_PP1, rost_G5, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G5_PP1$clock_seconds = G5_PP1_st - 
  better_track_G5_PP1$frame_id/30 + 1/30

event_G5_PP1.t = merge(event_G5_PP1, better_track_G5_PP1, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 23

#2022-02-14 Finland at USA Powerplay 3 Tracking Data
track_G5_PP3 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P2%20PP3.csv")

#Powerplay 23 event data
event_G5_PP3 <- subset(event_G5, ((clock_seconds <= 814 & 
                                     clock_seconds >= 748) | 
                                    (clock_seconds <= 746 & 
                                       clock_seconds >= 738)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G5_PP3_st = 815

better_track_G5_PP3 = merge(track_G5_PP3, rost_G5, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G5_PP3$clock_seconds = G5_PP3_st - 
  better_track_G5_PP3$frame_id/30 + 1/30

event_G5_PP3.t = merge(event_G5_PP3, better_track_G5_PP3, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 24

#2022-02-14 Finland at USA Powerplay 4 Tracking Data
track_G5_PP4 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P3%20PP4.csv")

#Powerplay 24 event data
event_G5_PP4 <- subset(event_G5, ((clock_seconds <= 1151 & 
                                     clock_seconds >= 1092) | 
                                    (clock_seconds <= 1086 & 
                                       clock_seconds >= 1051)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G5_PP4_st = 1171

better_track_G5_PP4 = merge(track_G5_PP4, rost_G5, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G5_PP4$clock_seconds = G5_PP4_st - 
  better_track_G5_PP4$frame_id/30 + 1/30

event_G5_PP4.t = merge(event_G5_PP4, better_track_G5_PP4, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 25 -- 6 on 4

#2022-02-14 Finland at USA Powerplay 5 Tracking Data
track_G5_PP5 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P3%20PP5.csv")

#Powerplay 25 event data
event_G5_PP5 <- subset(event_G5, ((clock_seconds <= 238 & 
                                     clock_seconds >= 147) | 
                                    (clock_seconds <= 127 & 
                                       clock_seconds >= 118)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G5_PP5_st = 238

better_track_G5_PP5 = merge(track_G5_PP5, rost_G5, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G5_PP5$clock_seconds = G5_PP5_st - 
  better_track_G5_PP5$frame_id/30 + 1/30

event_G5_PP5.t = merge(event_G5_PP5, better_track_G5_PP5, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 26 -- 6 on 4

#2022-02-14 Finland at USA Powerplay 6 Tracking Data
track_G5_PP6 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P3%20PP6.csv")

#Powerplay 26 event data
event_G5_PP6 <- subset(event_G5, ((clock_seconds <= 21 & 
                                     clock_seconds >= 5)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G5_PP6_st = 21

better_track_G5_PP6 = merge(track_G5_PP6, rost_G5, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G5_PP6$clock_seconds = G5_PP6_st - 
  better_track_G5_PP6$frame_id/30 + 1/30

event_G5_PP6.t = merge(event_G5_PP6, better_track_G5_PP6, by = "clock_seconds", 
                       all.x = TRUE)

################# Game 6
#NOTE:: 2022-02-16 Switzerland at Finland == G6

#2022-02-16 Switzerland at Finland event data
event_G6 = subset(event_data, (game_date == "16/2/2022") & 
                    (team_name == "Olympic (Women) - Switzerland" |
                       team_name == "Olympic (Women) - Finland"))

rost_G6 = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20roster.csv")
rost_G6$team_name = ifelse(rost_G6$team == "home", "Finland", "Switzerland")
colnames(rost_G6)[2] = "jersey_number"
rost_G6[43,] = c(NA, 100, NA, "home", "Finland")
rost_G6[44,] = c(NA, 100, NA, "away", "Switzerland")

###### Powerplay 27

#2022-02-16 Switzerland at Finland Powerplay 1 Tracking Data
track_G6_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P1%20PP1.csv")

#Powerplay 27 event data
event_G6_PP1 <- subset(event_G6, ((clock_seconds <= 655 & 
                                     clock_seconds >= 629) |
                                    (clock_seconds <= 541 & 
                                       clock_seconds >= 538)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G6_PP1_st = 658

better_track_G6_PP1 = merge(track_G6_PP1, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP1$clock_seconds = G6_PP1_st - 
  better_track_G6_PP1$frame_id/30 + 1/30

event_G6_PP1.t = merge(event_G6_PP1, better_track_G6_PP1, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 28

#2022-02-16 Switzerland at Finland Powerplay 2 Tracking Data
track_G6_PP2 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P1%20PP2.csv")

#Powerplay 28 event data
event_G6_PP2 <- subset(event_G6, ((clock_seconds <= 417 & 
                                     clock_seconds >= 367) |
                                    (clock_seconds <= 354 & 
                                       clock_seconds >= 312) |
                                    (clock_seconds <= 309 & 
                                       clock_seconds >= 297)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G6_PP2_st = 417

better_track_G6_PP2 = merge(track_G6_PP2, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP2$clock_seconds = G6_PP2_st - 
  better_track_G6_PP2$frame_id/30 + 1/30

event_G6_PP2.t = merge(event_G6_PP2, better_track_G6_PP2, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 29

#2022-02-16 Switzerland at Finland Powerplay 3 Tracking Data
track_G6_PP3 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P2%20PP3.csv")

#Powerplay 29 event data
event_G6_PP3 <- subset(event_G6, ((clock_seconds <= 67 & 
                                     clock_seconds >= 0)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

G6_PP3_st = 67

better_track_G6_PP3 = merge(track_G6_PP3, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP3$clock_seconds = G6_PP3_st - 
  better_track_G6_PP3$frame_id/30 + 1/30

event_G6_PP3.t = merge(event_G6_PP3, better_track_G6_PP3, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 30

#2022-02-16 Switzerland at Finland Powerplay 4 Tracking Data
track_G6_PP4 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P2%20PP4.csv")

#Powerplay 30 event data
event_G6_PP4 <- subset(event_G6, ((clock_seconds <= 594 & 
                                     clock_seconds >= 538) |
                                    (clock_seconds <= 501 & 
                                       clock_seconds >= 474)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G6_PP4_st = 594

better_track_G6_PP4 = merge(track_G6_PP4, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP4$clock_seconds = G6_PP4_st - 
  better_track_G6_PP4$frame_id/30 + 1/30

event_G6_PP4.t = merge(event_G6_PP4, better_track_G6_PP4, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 31

#2022-02-16 Switzerland at Finland Powerplay 5 Tracking Data
track_G6_PP5 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P2%20PP5.csv")

#Powerplay 31 event data
event_G6_PP5 <- subset(event_G6, ((clock_seconds <= 219 & 
                                     clock_seconds >= 130) |
                                    (clock_seconds <= 115 & 
                                       clock_seconds >= 118)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

G6_PP5_st = 229

better_track_G6_PP5 = merge(track_G6_PP5, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP5$clock_seconds = G6_PP5_st - 
  better_track_G6_PP5$frame_id/30 + 1/30

event_G6_PP5.t = merge(event_G6_PP5, better_track_G6_PP5, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 32

#2022-02-16 Switzerland at Finland Powerplay 6 Tracking Data
track_G6_PP6 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P3%20PP6.csv")

#Powerplay 32 event data
event_G6_PP6 <- subset(event_G6, ((clock_seconds <= 1020 & 
                                     clock_seconds >= 996) |
                                    (clock_seconds <= 944 & 
                                       clock_seconds >= 908)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G6_PP6_st = 1028

better_track_G6_PP6 = merge(track_G6_PP6, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP6$clock_seconds = G6_PP6_st - 
  better_track_G6_PP6$frame_id/30 + 1/30

event_G6_PP6.t = merge(event_G6_PP6, better_track_G6_PP6, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 33

#2022-02-16 Switzerland at Finland Powerplay 7 Tracking Data
track_G6_PP7 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P3%20PP7.csv")

#Powerplay 33 event data
event_G6_PP7 <- subset(event_G6, ((clock_seconds <= 378 & 
                                     clock_seconds >= 337)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G6_PP7_st = 378

better_track_G6_PP7 = merge(track_G6_PP7, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP7$clock_seconds = G6_PP7_st - 
  better_track_G6_PP7$frame_id/30 + 1/30

event_G6_PP7.t = merge(event_G6_PP7, better_track_G6_PP7, by = "clock_seconds", 
                       all.x = TRUE)

###### Powerplay 34

#2022-02-16 Switzerland at Finland Powerplay 8 Tracking Data
track_G6_PP8 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P3%20PP8.csv")

#Powerplay 34 event data
event_G6_PP8 <- subset(event_G6, ((clock_seconds <= 107 & 
                                     clock_seconds >= 36)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

G6_PP8_st = 171

better_track_G6_PP8 = merge(track_G6_PP8, rost_G6, by = c("jersey_number", 
                                                          "team_name"), all.x = TRUE)

better_track_G6_PP8$clock_seconds = G6_PP8_st - 
  better_track_G6_PP8$frame_id/30 + 1/30

event_G6_PP8.t = merge(event_G6_PP8, better_track_G6_PP8, by = "clock_seconds", 
                       all.x = TRUE)

######## Combining Datasets

event_PP = rbind(event_G1_PP1.t, event_G1_PP2.t, event_G1_PP3.t, event_G1_PP5.t, event_G1_PP6.t,
      event_G2_PP1.t, event_G2_PP2.t, event_G2_PP4.t, event_G2_PP5.t, event_G2_PP6.t, 
      event_G3_PP1.t, event_G3_PP2.t, event_G3_PP3.t, event_G3_PP5.t, 
      event_G4_PP1.t, event_G4_PP2.t, event_G4_PP4.t, event_G4_PP5.t, 
      event_G5_PP1.t, event_G5_PP3.t, event_G5_PP4.t, event_G5_PP5.t, event_G5_PP6.t, 
      event_G6_PP1.t, event_G6_PP2.t, event_G6_PP3.t, event_G6_PP4.t, event_G6_PP5.t, event_G6_PP6.t, event_G6_PP7.t, event_G6_PP8.t)

#data set of all pass event data during the timeframes where we have tracking data
pass_PP = subset(event_PP, event == "Play")

pass_PP$pass_dist = sqrt((pass_PP$x_coord-pass_PP$x_coord_2)^2 + 
                             (pass_PP$y_coord-pass_PP$y_coord_2)^2)
pass_PP$event_detail_1 = NULL
pass_PP$event_detail_2 = NULL
pass_PP$event_detail_3 = NULL


################################
### Creation of new columns
################################

dist_from_pl_f = function(x1, y1, x2, y2, x3, y3){
  ABx = x1 - x2
  ABy = y1 - y2
  ACx = x3 - x1 #is this right??
  ACy = y3 - y1
  S = sqrt(ABx^2 + ABy^2)
  Num = ABx*ACx + ABy*ACy
  Px = (Num/(S^2)) * ABx
  Py = (Num/(S^2)) * ABy
  Qx = x1 + Px
  Qy = y1 + Py
  D = sqrt((x3 - Qx)^2 + (y3 - Qy)^2)
  return(D)
}

dist_from_passer_f = function(x1, y1, x2, y2){
  d = sqrt((x1 - x2)^2 + (y1 - y2)^2)
  return(d)
}

names(pass_PP)[4] = paste("team_name_passer")
pass_PP$period.y = NULL
pass_PP$frame_id = NULL
pass_PP$track_id = NULL
pass_PP$team_id = NULL
names(pass_PP)[7] = paste("period")
names(pass_PP)[20] = paste("jersey_number_t")
names(pass_PP)[21] = paste("team_name_t")
names(pass_PP)[22] = paste("x_coord_t")
names(pass_PP)[23] = paste("y_coord_t")
names(pass_PP)[24] = paste("player_name_t")
names(pass_PP)[25] = paste("position_t")
names(pass_PP)[26] = paste("venue_t")
pass_PP$team_name_passer = ifelse(pass_PP$team_name_passer == "Olympic (Women) - Canada", 
    "Canada", ifelse(pass_PP$team_name_passer == "Olympic (Women) - United States", 
    "USA", ifelse(pass_PP$team_name_passer == "Olympic (Women) - Finland", 
    "Finland", ifelse(pass_PP$team_name_passer == "Olympic (Women) - Olympic Athletes from Russia",
    "ROC", ifelse(pass_PP$team_name_passer == "Olympic (Women) - Switzerland", 
                  "Switzerland", "issue")))))

pass_PP$opp_team_name = ifelse(pass_PP$opp_team_name == "Olympic (Women) - Canada", 
     "Canada", ifelse(pass_PP$opp_team_name == "Olympic (Women) - United States", 
     "USA", ifelse(pass_PP$opp_team_name == "Olympic (Women) - Finland", 
     "Finland", ifelse(pass_PP$opp_team_name == "Olympic (Women) - Olympic Athletes from Russia",
     "ROC", ifelse(pass_PP$opp_team_name == "Olympic (Women) - Switzerland", 
     "Switzerland", "issue")))))

pass_PP$dist_from_pl = ifelse(pass_PP$team_name_passer == pass_PP$team_name_t, 
        NA, dist_from_pl_f(pass_PP$x_coord, pass_PP$y_coord, 
        pass_PP$x_coord_2, pass_PP$y_coord_2, pass_PP$x_coord_t, pass_PP$y_coord_t))

pass_PP$dist_from_passer = ifelse(pass_PP$team_name_passer == pass_PP$team_name_t, 
        NA, dist_from_passer_f(pass_PP$x_coord, pass_PP$y_coord, 
        pass_PP$x_coord_t, pass_PP$y_coord_t))

pass_PP$dist_from_reciever = ifelse(pass_PP$team_name_passer == pass_PP$team_name_t, 
        NA, dist_from_passer_f(pass_PP$x_coord_2, 
        pass_PP$y_coord_2, 
        pass_PP$x_coord_t, pass_PP$y_coord_t))

three_off_and_def_check = nest(pass_PP, .by = c("player_name", "clock_seconds", "x_coord", "y_coord"))

for_factor_pass_PP = subset(pass_PP, !is.na(jersey_number_t) & !is.na(dist_from_pl))
new_wider = nest(for_factor_pass_PP, .by = c("player_name", "clock_seconds", "x_coord", "y_coord"))

min_from_pl_f = function(new_wider){
  mini_list = list()
  n1 = nrow(new_wider)
  for(pass in 1:n1){
    mini_list = append(mini_list, min(unnest(new_wider[pass, 5])$dist_from_pl, na.rm = TRUE))
  }
  return(mini_list)
}

#used 7ft since 55" stick length (approximate) + 28.5" arm length (world woman average) ~= 7ft
#essentially, 7ft is their reach ability)
num_def_near_pl_f = function(new_wider){
  mini_list = list()
  n1 = nrow(new_wider)
  for(pass in 1:n1){
    mini_list = append(mini_list, length(which(unnest(new_wider[pass, 5])$dist_from_pl <= 7)))
  }
  return(mini_list)
}

pass_rushed_f = function(new_wider){
  mini_list = list()
  n1 = nrow(new_wider)
  for(pass in 1:n1){
    mini_list = append(mini_list, ifelse(min(unnest(new_wider[pass, 5])$dist_from_passer, na.rm = TRUE) <= 7, 1, 0))
  }
  return(mini_list)
}

reciever_crowded_f = function(new_wider){
  mini_list = list()
  n1 = nrow(new_wider)
  for(pass in 1:n1){
    mini_list = append(mini_list, ifelse(min(unnest(new_wider[pass, 5])$dist_from_reciever, na.rm = TRUE) <= 7, 1, 0))
  }
  return(mini_list)
}

###########
num_def_f = function(df){
  mini_list = list()
  n1 = nrow(df)
  for(pass in 1:n1){
    mini_list = append(mini_list, length(which(unnest(df[pass, 5])$venue_t != unnest(df[pass, 5])$venue)))
  }
  return(mini_list)
}

num_off_f = function(df){
  mini_list = list()
  n1 = nrow(df)
  for(pass in 1:n1){
    mini_list = append(mini_list, length(which(unnest(df[pass, 5])$venue_t == unnest(df[pass, 5])$venue)))
  }
  return(mini_list)
}
################



new_wider$min_dist_from_pl = min_from_pl_f(new_wider)

new_wider$num_def_near_pl = num_def_near_pl_f(new_wider)

new_wider$pass_rushed = pass_rushed_f(new_wider)

new_wider$reciever_crowded = reciever_crowded_f(new_wider)

################
#three_off_and_def_check$min_dist_from_pl = min_from_pl_f(three_off_and_def_check)
#three_off_and_def_check$num_def_near_pl = num_def_near_pl_f(three_off_and_def_check)
#three_off_and_def_check$pass_rushed = pass_rushed_f(three_off_and_def_check)
#three_off_and_def_check$reciever_crowded = reciever_crowded_f(three_off_and_def_check)
three_off_and_def_check$num_def_tr = num_def_f(three_off_and_def_check)
three_off_and_def_check$num_off_tr = num_off_f(three_off_and_def_check)

test1_df = subset(three_off_and_def_check, num_def_tr >= 3 & num_def_tr <= 4 &
                   num_off_tr >= 3 & num_off_tr <= 5)
test1_df$min_dist_from_pl = min_from_pl_f(test_df)
test1_df$num_def_near_pl = num_def_near_pl_f(test_df)
test1_df$pass_rushed = pass_rushed_f(test_df)
test1_df$reciever_crowded = reciever_crowded_f(test_df)

test1_df$data = NULL

test1_df = merge(test1_df, event_PP_passes, 
                               by = c("player_name", 
                                      "clock_seconds", "x_coord", "y_coord"), all.x = TRUE)

test1_df$event_detail_1 = NULL
test1_df$event_detail_2 = NULL
test1_df$event_detail_3 = NULL


#################

powerplay_pass = test1_df

# Convert event_successful to binary
powerplay_pass$event_successful <- ifelse(powerplay_pass$event_successful == "t", 1, 0)

# Run logistic regression on event_successful


powerplay_pass$min_dist_from_pl <- unlist(powerplay_pass$min_dist_from_pl)
powerplay_pass$num_def_near_pl <- unlist(powerplay_pass$num_def_near_pl)
powerplay_pass$pass_rushed <- unlist(powerplay_pass$pass_rushed)
powerplay_pass$reciever_crowded <- sapply(powerplay_pass$reciever_crowded, toString)


#pass_completion_prob <- glm(event_successful ~ min_dist_from_pl + num_def_near_pl 
                            #+ as.factor(pass_rushed) 
                            #+ as.factor(reciever_crowded) + as.factor(pass_cluster),
                            #data = powerplay_pass, 
                            #family = binomial)

# Model results
#summary(pass_completion_prob)



#factor_variable_passes = new_wider

#factor_variable_passes$data = NULL



event_PP_passes = rbind(event_G1_PP1, event_G1_PP2, event_G1_PP3, event_G1_PP5, event_G1_PP6,
                        event_G2_PP1, event_G2_PP2, event_G2_PP4, event_G2_PP5, event_G2_PP6, 
                        event_G3_PP1, event_G3_PP2, event_G3_PP3, event_G3_PP5, 
                        event_G4_PP1, event_G4_PP2, event_G4_PP4, event_G4_PP5, 
                        event_G5_PP1, event_G5_PP3, event_G5_PP4, event_G5_PP5, event_G5_PP6, 
                        event_G6_PP1, event_G6_PP2, event_G6_PP3, event_G6_PP4, event_G6_PP5, event_G6_PP6, event_G6_PP7, event_G6_PP8)
  
event_PP_passes = subset(event_PP_passes, event == "Play")

#factor_variable_passes = merge(factor_variable_passes, event_PP_passes, 
                               by = c("player_name", 
                               "clock_seconds", "x_coord", "y_coord"), all.x = TRUE)
#factor_variable_passes$event_detail_1 = NULL
#factor_variable_passes$event_detail_2 = NULL
#factor_variable_passes$event_detail_3 = NULL

