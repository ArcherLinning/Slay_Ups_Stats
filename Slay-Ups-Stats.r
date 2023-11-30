# set working directory

setwd("C:/Users/linni/OneDrive/Desktop/Personal/Practice_Programming/Slay-Ups-Stats")

# read data
data = read.csv("Data/BBALL_STATS.csv")
attach(data)

# summaries
summary(data)

library("car")

# scatter of points diff by number of fouls given the result
scatterplot(points_diff ~ fouls | result, data = data, xaxt = 'n', yaxt = 'n', legend = FALSE, 
            col = c("red", 'blue'))
legend("bottomleft", c("W", "L"), pch = c(2, 1), col = c("blue", 'red'))
axis(1, at = c(4,6,8,10,12,14,16))
axis(2, at = c(-15,0,15,30,45,60))

################################################################################
#                    By quarter analysis - All Season                          #
################################################################################

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

par(mfrow = c(1,2))
barplot(qtr_off_means, xlab = "Quarters", ylab = "Points For", names.arg = c("1st", "2nd", "3rd", "4th"),
        col = c("yellow", "blue", "red", "green"), ylim = c(0,18), yaxt = 'n', xaxt = 'n')
axis(1, at = c(1,2,3,4))
axis(2, at = c(0,3,6,9,12,15,18))
barplot(qtr_def_means, xlab = "Quarters", ylab = "Points Agianst", names.arg = c("1st", "2nd", "3rd", "4th"),
        col = c("yellow", "blue", "red", "green"), ylim = c(0,12), xaxt = 'n')
axis(1, at = c(1,2,3,4))

# create matrix of plus-minus by quarter for each game

qtr_pm = matrix(0, nrow = 4, ncol = 27)

for (j in 1:4){
  for (k in 1:27){
    qtr_for = c(qtr1_for, qtr2_for, qtr3_for, qtr4_for)
    
    qtr_against = c(qtr1_against, qtr2_against, qtr3_against, qtr4_against)
    
    qtr_pm[j,k] = qtr_for[27*j - 27 + k] - qtr_against[27*j - 27 + k]
  }
}

# plot plus minus by quarter
par(mfrow = c(1,2))
matplot(c(1,2,3,4), qtr_pm, type = 'p', xlab = "Quarters", pch = 1, col = "black", xaxt = 'n', ylab = "Plus-Minus")
axis(1, at = c(1,2,3,4))

# create averaeg plus-minus by quarter
mean_qtr_pm = rep(0, 4)

for (l in 1:4){
  qtr_for = c(qtr1_for, qtr2_for, qtr3_for, qtr4_for)
  qtr_against = c(qtr1_against, qtr2_against, qtr3_against, qtr4_against)

  
  mean_qtr_pm[l] = mean(qtr_pm[l,], na.rm = TRUE)
}

# plot average plus-minus per quarter
plot(mean_qtr_pm, type = "b", xlab = "Quarters", xaxt = "n", ylab = "Plus-Minus")
axis(1, at = c(1,2,3,4))


################################################################################
#                             Predictive Modelling                            #
################################################################################

data$result_binary = as.integer(data$result == "W")

twoway_model = lm(result_binary ~ qtr1_for + qtr2_for + qtr3_for + qtr4_for +
     qtr1_against + qtr2_against + qtr3_against + qtr4_against + fouls, data = data)
summary(twoway_model)

off_model = lm(result_binary ~ qtr1_for + qtr2_for + qtr3_for + qtr4_for + fouls, data = data)
summary(off_model)

def_model = lm(result_binary ~ qtr1_against + qtr2_against + qtr3_against + qtr4_against + fouls, data = data)
summary(def_model)

firsthalf_model = lm(result_binary ~ qtr1_for + qtr2_for + qtr1_against + qtr2_against, data = data)
summary(firsthalf_model)

secondhalf_model = lm(result_binary ~ qtr3_for + qtr4_for + qtr3_against + qtr4_against, data = data)
summary(secondhalf_model)

firstqtr_model = lm(result_binary ~ qtr1_for + qtr1_against, data = data)
summary(firstqtr_model)

secondqtr_model = lm(result_binary ~ qtr2_for + qtr2_against, data = data)
summary(secondqtr_model)

thirdqtr_model = lm(result_binary ~ qtr3_for + qtr3_against, data = data)
summary(thirdqtr_model)

forthqtr_model = lm(result_binary ~ qtr4_for + qtr4_against, data = data)
summary(forthqtr_model)

best_model = lm(result_binary ~ qtr2_against + qtr3_against + fouls, data = data)
summary(best_model)

################################################################################

