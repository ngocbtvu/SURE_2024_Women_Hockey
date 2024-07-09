library(tidyverse)

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
rost_G1$jersey_number = rost_G1$jn
rost_G1$jn = NULL
###### Powerplay 1

#2022-02-08 Canada at USA Powerplay 1 Tracking Data
track_G1_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20Canada%20at%20USA/2022-02-08%20Canada%20at%20USA%20P1%20PP1.csv")

#Powerplay 1 event data (starts at 376 not 386)
event_G1_PP1 <- subset(event_G1, clock_seconds <= 376 & 
                                 clock_seconds >= 350 &
                         period == 1 & 
                         situation_type == "5 on 4")

G1_PP1_st = 386 #how to include period = 1??

better_track_G1_PP1 = merge(track_G1_PP1, rost_G1, by = "jersey_number")
better_track_G1_PP1$clock_seconds = G1_PP1_st - 
  better_track_G1_PP1$frame_id/30 + 1/30


#event_G1_PP1$stP3 = c("Brianne Jenner")
#i.v. = unique(event_G1_PP1$clock_seconds)+2
#event_G1_PP1$stP3.Tplus60f = 
#(better_track_G1_PP1$x_ft, better_track_G1_PP1$clock_seconds == i.v.)


#unique(subset(better_track_G1_PP1, team_name == "Canada")$player)[1]

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

###### Powerplay 8

#2022-02-08 ROC at Finland Powerplay 1 Tracking Data
track_G2_PP1 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P1%20PP1.csv")

#Powerplay 8 event data
event_G2_PP1 <- subset(event_G2, (clock_seconds <= 189 & 
                                     clock_seconds >= 123) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

###### Powerplay 9

#2022-02-08 ROC at Finland Powerplay 2 Tracking Data
track_G2_PP2 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-08%20ROC%20at%20Finland/2022-02-08%20ROC%20at%20Finland%20P2%20PP2.csv")

#Powerplay 9 event data
event_G2_PP2 <- subset(event_G2, (clock_seconds <= 1047 & 
                                    clock_seconds >= 1021) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

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

################# Game 3
#NOTE:: 2022-02-12 Switzerland at ROC == G3

#2022-02-12 Switzerland at ROC event data
event_G3 = subset(event_data, (game_date == "12/2/2022") & 
                    (team_name == "Olympic (Women) - Olympic Athletes from Russia" |
                       team_name == "Olympic (Women) - Switzerland"))

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

################# Game 4
#NOTE:: 2022-02-14 Switzerland at Canada == G4

#2022-02-14 Switzerland at Canada event data
event_G4 = subset(event_data, (game_date == "14/2/2022") & 
                    (team_name == "Olympic (Women) - Canada" |
                       team_name == "Olympic (Women) - Switzerland"))

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

###### Powerplay 20

#2022-02-14 Switzerland at Canada Powerplay 4 Tracking Data
track_G4_PP4 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Switzerland%20at%20Canada/2022-02-14%20Switzerland%20at%20Canada%20P2%20PP4.csv")

#Powerplay 20 event data
event_G4_PP4 <- subset(event_G4, ((clock_seconds <= 206 & 
                                     clock_seconds >= 101)) &
                         (period == 2 & 
                            situation_type == "5 on 4"))

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


################# Game 5
#NOTE:: 2022-02-14 Finland at USA == G5

#2022-02-14 Finland at USA event data
event_G5 = subset(event_data, (game_date == "14/2/2022") & 
                    (team_name == "Olympic (Women) - Finland" |
                       team_name == "Olympic (Women) - United States"))

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

###### Powerplay 26 -- 6 on 4

#2022-02-14 Finland at USA Powerplay 6 Tracking Data
track_G5_PP6 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P3%20PP6.csv")

#Powerplay 26 event data
event_G5_PP6 <- subset(event_G5, ((clock_seconds <= 21 & 
                                     clock_seconds >= 5)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

################# Game 6
#NOTE:: 2022-02-16 Switzerland at Finland == G6

#2022-02-16 Switzerland at Finland event data
event_G6 = subset(event_data, (game_date == "16/2/2022") & 
                    (team_name == "Olympic (Women) - Switzerland" |
                       team_name == "Olympic (Women) - Finland"))

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

###### Powerplay 29

#2022-02-16 Switzerland at Finland Powerplay 3 Tracking Data
track_G6_PP3 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P2%20PP3.csv")

#Powerplay 29 event data
event_G6_PP3 <- subset(event_G6, ((clock_seconds <= 67 & 
                                     clock_seconds >= 0)) &
                         (period == 1 & 
                            situation_type == "5 on 4"))

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

###### Powerplay 33

#2022-02-16 Switzerland at Finland Powerplay 7 Tracking Data
track_G6_PP7 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P3%20PP7.csv")

#Powerplay 33 event data
event_G6_PP7 <- subset(event_G6, ((clock_seconds <= 378 & 
                                     clock_seconds >= 337)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

###### Powerplay 34

#2022-02-16 Switzerland at Finland Powerplay 8 Tracking Data
track_G6_PP8 <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-16%20Switzerland%20at%20Finland/2022-02-16%20Switzerland%20at%20Finland%20P3%20PP8.csv")

#Powerplay 34 event data
event_G6_PP8 <- subset(event_G6, ((clock_seconds <= 107 & 
                                     clock_seconds >= 36)) &
                         (period == 3 & 
                            situation_type == "5 on 4"))

######## Combining Datasets

event_PP = rbind(event_G1_PP1, event_G1_PP2, event_G1_PP3, event_G1_PP5, event_G1_PP6,
      event_G2_PP1, event_G2_PP2, event_G2_PP4, event_G2_PP5, event_G2_PP6, 
      event_G3_PP1, event_G3_PP2, event_G3_PP3, event_G3_PP5, 
      event_G4_PP1, event_G4_PP2, event_G4_PP4, event_G4_PP5, 
      event_G5_PP1, event_G5_PP3, event_G5_PP4, event_G5_PP5, event_G5_PP6, 
      event_G6_PP1, event_G6_PP2, event_G6_PP3, event_G6_PP4, event_G6_PP5, event_G6_PP6, event_G6_PP7, event_G6_PP8)

#data set of all pass event data during the timeframes where we have tracking data
pass_PP = subset(event_PP, event == "Play")

pass_PP$pass_dist = sqrt((pass_PP$x_coord-pass_PP$x_coord_2)^2 + 
                             (pass_PP$y_coord-pass_PP$y_coord_2)^2)
#pass_PP$event_detail_1 = NULL
#pass_PP$event_detail_2 = NULL
#pass_PP$event_detail_3 = NULL
