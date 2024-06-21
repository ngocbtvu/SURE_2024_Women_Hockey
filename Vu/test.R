library(sportyR)

data <- read.csv("BDC_2024_Womens_Data.csv")
shifts <- read.csv("BDC_2024_Womens_Shifts.csv")

rink <- geom_hockey("iihf")
