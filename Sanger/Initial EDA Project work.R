library(tidyverse)
nhl_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nhl_shots.csv")

head(nhl_shots)

#I can also do 4v4, and double power play if wanted

#gives home powerplay shots
nhl_shots_single_PP_home = subset(nhl_shots, 
                        homeSkatersOnIce == 5 & awaySkatersOnIce == 4 & 
                          isHomeTeam == 1 & homeEmptyNet == 0 & 
                          awayEmptyNet == 0)
#gives away powerplay shots
nhl_shots_single_PP_away = subset(nhl_shots, 
                        homeSkatersOnIce == 4 & awaySkatersOnIce == 5 & 
                          isHomeTeam == 0 & homeEmptyNet == 0 & 
                          awayEmptyNet == 0)
library(dplyr)
#gives total powerplay goals (no empty net)
nhl_shots_single_PP_total = bind_rows(nhl_shots_single_PP_home, 
                                      nhl_shots_single_PP_away)

#gives total 5v5 goals (no empty net)
nhl_shots_5v5 = subset(nhl_shots, 
                       homeSkatersOnIce == 5 & 
                         awaySkatersOnIce == 5 &
                         awayEmptyNet == 0 & homeEmptyNet == 0)

#dataframe with new column
nhl_shots_non_empty = nhl_shots |>
  subset(homeEmptyNet == 0 & 
           awayEmptyNet == 0 &
           ((isHomeTeam == 1 & homeSkatersOnIce == 5) | 
              (isHomeTeam == 0 & awaySkatersOnIce == 5)))
##
nhl_shots_non_empty_away = nhl_shots |>
  subset(homeEmptyNet == 0 & 
           awayEmptyNet == 0 &
           isHomeTeam == 0 & awaySkatersOnIce == 5 &
           homeSkatersOnIce <= 5)

nhl_shots_non_empty_home = nhl_shots |>
  subset(homeEmptyNet == 0 & 
           awayEmptyNet == 0 &
           isHomeTeam == 1 & homeSkatersOnIce == 5 &
           awaySkatersOnIce <= 5)
##
nhl_shots_non_empty_home$fiveVx = nhl_shots_non_empty_home$awaySkatersOnIce
nhl_shots_non_empty_away$fiveVx = nhl_shots_non_empty_away$homeSkatersOnIce

nhl_shots_non_empty = rbind(nhl_shots_non_empty_home, nhl_shots_non_empty_away)
######################################
#now have to figure out how to compare
##Can do this in terms of percentage of results, or expected goals


#BELOW ARE IN TERMS OF RESULTS
nhl_shots_5v5 |>
  ggplot(aes(x = event, fill = event)) + geom_bar()
#get rid of label and change to percentage rather than count
nhl_shots_single_PP_total |>
  ggplot(aes(x = event, fill = event)) + geom_bar()




#Have to do these with 5v etc and have #other team on ice as a variable


#BELOW ARE IN TERMS OF EXPECTED GOALS


#BELOW ARE IN TERMS OF SHOT DISTANCE
nhl_shots_5v5 |>
  ggplot(aes(x = event, fill = event)) + geom_bar()
#get rid of label and change to percentage rather than count
nhl_shots_single_PP_total |>
  ggplot(aes(x = event, fill = event)) + geom_bar()


#can also make a new col with 5v5, 5v4, 5v3 (just remove empty net)
nhl_shots_non_empty |>
  ggplot(aes(y = factor(fiveVx), x = shotDistance)) + 
  geom_violin() + geom_boxplot(width = 0.3)

nhl_shots_non_empty |>
  ggplot(aes(y = factor(fiveVx), x = xGoal)) + 
  geom_point(alpha = 0.1)

nhl_shots_non_empty |>
  ggplot(aes(x = factor(fiveVx), fill = factor(shotWasOnGoal))) + 
  geom_bar()

table(nhl_shots_non_empty$fiveVx, nhl_shots_non_empty$shotWasOnGoal)

######################################################
######################################################
######################################################
#this table in percentages is the perfect table
table(nhl_shots_non_empty$fiveVx, nhl_shots_non_empty$event)


#now do fct_relevel to change the levels 
#center the title
nhl_shots_non_empty |>
  count(fiveVx, event) |>
  group_by(fiveVx) |>
  mutate(prop = n/sum(n), event = fct_relevel(event, "GOAL", "SHOT", "MISS")) |>
  ggplot(aes(x = fiveVx, y = prop, fill = event)) + geom_col() + 
  labs(title = "Shooting Performance by # of Opposing Skaters",
       x = "Number of Opposing Skaters", y = "Percentage", 
       caption = "5 offensive skaters in all cases")

######################################################
######################################################
######################################################

######## Defender fatigue

nhl_shots_5v5 |>
  ggplot(aes(x = defendingTeamAverageTimeOnIce, y = xGoal, color = event)) + 
  geom_point(alpha = 0.2) + geom_smooth(method = "lm")
#maybe have to use difference in time on ice

nhl_shots_5v5$difTimeOnIce = 
  nhl_shots_5v5$defendingTeamAverageTimeOnIce - nhl_shots_5v5$shootingTeamAverageTimeOnIce
#higher the number, the more tired the defense

nhl_shots_5v5 |>
  ggplot(aes(x = difTimeOnIce, y = xGoal, color = event)) + 
  geom_point(alpha = 0.2) + geom_smooth(method = "lm")


