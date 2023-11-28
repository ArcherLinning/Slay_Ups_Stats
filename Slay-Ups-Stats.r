# set working directory

setwd("C:/Users/linni/OneDrive/Desktop/Personal/Practice_Programming/Python_Practice_Stats")

# read data
data = read.csv("Data/BBALL_STATS.csv")
attach(data)

# summaries
summary(data)

library("car")

# scatter of points diff by number of fouls given the result
scatterplot(points_diff ~ fouls | result, data = data)

##################################################################
#               By quarter analysis - All Seasons               #
##################################################################

# create empty vectors
qtr_off_means = rep(0,4)
qtr_def_means = rep(0,4)

# create average points by quarter 
for (i in 1:4){
  qtr_for = c(qtr1_for, qtr2_for, qtr3_for, qtr4_for)
  qtr_against = c(qtr1_against, qtr2_against, qtr3_against, qtr4_against)
    
  qtr_off_means[i] = mean(qtr_for[(27*i-26):(27*i)], na.rm = TRUE)
  qtr_def_means[i] = mean(qtr_against[(27*i-26):(27*i)], na.rm = TRUE)
}

# plot mean points per quarter
plot(qtr_off_means, type = "b", xlab = "Quarters", xaxt = "n", ylab = "Average Points", col = "blue", ylim = c(8,17))
points(qtr_def_means, type = "b", col = "red")
axis(1, at = c(1,2,3,4))
legend("topleft", c("Points For", "Points Against"), col = c("blue", "red"), lty = 1)

barplot(qtr_off_means, xlab = "Quarters", names.arg = c("1st", "2nd", "3rd", "4th"),
        col = c("yellow", "blue", "red", "green"))

#################################################################




