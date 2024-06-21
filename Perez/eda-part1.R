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

hock <- read.csv('BDC_2024_Womens_Data copy.csv')

power_plays <- hock |> 
  filter((Home.Team.Skaters == 5 & Away.Team.Skaters ==4)|
           (Home.Team.Skaters == 4 & Away.Team.Skaters ==5))

