# set working directory

setwd("C:/Users/linni/OneDrive/Desktop/Personal/Practice_Programming/Python_Practice_Stats")

# read data
data = read.csv("Data/BBALL_STATS.csv")

# summaries
summary(data)

library("car")

scatterplot(points_diff ~ fouls | result, data = data)


