# set working directory
setwd("C:/Users/linni/OneDrive/Desktop/Personal/Practice_Programming/Slay-Ups-Stats")

# read data
data = read.csv("Data/BBALL_STATS.csv")
attach(data)

# summaries
summary(data)

library("car")

# scatter of points diff by number of fouls given the result
scatterplot(points_diff ~ fouls | result, data = data, xaxt = 'n', yaxt = 'n',
            legend = FALSE, col = c("red", 'green'), xlab = "Fouls",
            ylab = "Points Differential", main = "Points Differential vs. Fouls",
            pch = c(4,3), smooth = FALSE)
legend("topright", c("L", "W"), pch = c(4,3), col = c("red", 'green'), cex = 1,
       bty = 'o', x.intersp = 0.4, y.intersp = 0.2)
axis(1, at = c(4,6,8,10,12,14,16))
axis(2, at = c(-15,0,15,30,45,60))

win = subset(data, result == "W")
win_fouls_mean = mean(win$fouls, na.rm = TRUE)

loss = subset(data, result == "L")
loss_fouls_mean = mean(loss$fouls, na.rm = TRUE)

barplot(c(win_fouls_mean, loss_fouls_mean), names.arg = c("Win", "Loss"),
        main = "Average Fouls for Wins and Losses", xlab = "Result",
        ylab = "Average Number of Fouls", col = c("green", "red"))

################################################################################
#                    By quarter analysis - All Season                          #
################################################################################

# create empty vectors
qtr_off_means = c(mean(qtr1_for, na.rm = TRUE),mean(qtr2_for, na.rm = TRUE),
                  mean(qtr3_for, na.rm = TRUE), mean(qtr4_for, na.rm = TRUE))
qtr_def_means = c(mean(qtr1_against, na.rm = TRUE),mean(qtr2_against,
                  na.rm = TRUE), mean(qtr3_against, na.rm = TRUE),
                  mean(qtr4_against, na.rm = TRUE))


# plot mean points per quarter
plot(qtr_off_means, type = "b", xlab = "Quarters", xaxt = "n",
     ylab = "Average Points", col = "green", ylim = c(8,17),
     main = "Average Points Scored For and Against by Quarter")
points(qtr_def_means, type = "b", col = "red")
axis(1, at = c(1,2,3,4))
legend("topleft", c("Points For", "Points Against"), col = c("green", "red"),
       lty = 1, bty = 'n', x.intersp = 0.4, y.intersp = 0.2)

par(mfrow = c(1,2))
barplot(qtr_off_means, xlab = "Quarters", ylab = "Points For", names.arg =
        c("1st", "2nd", "3rd", "4th"), col = c("yellow", "blue", "red",
        "green"), ylim = c(0,18), yaxt = 'n', xaxt = 'n',
        main = "Average Points Scored For by Quarter")
axis(1, at = c(1,2,3,4))
axis(2, at = c(0,3,6,9,12,15,18))
barplot(qtr_def_means, xlab = "Quarters", ylab = "Points Against", names.arg =
        c("1st", "2nd", "3rd", "4th"), col = c("yellow", "blue", "red",
        "green"), ylim = c(0,12), xaxt = 'n', yaxt = 'n',
        main = "Average Points Scored Against by Quarter")
axis(1, at = c(1,2,3,4))
axis(2, at = c(0,2,4,6,8,10,12))

# create matrix of plus-minus by quarter for each game

qtr_pm = matrix(0, nrow = 4, ncol = length(game))

for (j in 1:4){
  for (k in 1:length(game)){
    qtr_for = c(qtr1_for, qtr2_for, qtr3_for, qtr4_for)
    
    qtr_against = c(qtr1_against, qtr2_against, qtr3_against, qtr4_against)
    
    qtr_pm[j,k] = qtr_for[length(game)*(j-1)+k]-qtr_against[length(game)*(j-1)+k]
  }
}

# plot plus minus by quarter
matplot(c(1,2,3,4), qtr_pm, type = 'p', xlab = "Quarters", pch = 1, col = "black",
        xaxt = 'n', ylab = "Plus-Minus", main = "Plus-Minus by Quarter",
        ylim = c(-14,24))
axis(1, at = c(1,2,3,4))
points(c(1,2,3,4), mean_qtr_pm, col = "blue", pch = 16, type = 'b')

# create average plus-minus by quarter
mean_qtr_pm = rep(0,4)

for (j in 1:4){
  qtr_pm[j] = qtr_off_means[j] - qtr_def_means[j]
}

# plot plus minus by quarter
par(mfrow = c(1,2))
plot(qtr_pm, type = 'b', xlab = "Quarters", pch = 1, col = "black",
     xaxt = 'n', ylab = "Plus-Minus", main = "Plus-Minus by Quarter")
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

fourthqtr_model = lm(result_binary ~ qtr4_for + qtr4_against, data = data)
summary(forthqtr_model)

best_model = lm(result_binary ~ qtr2_against + qtr3_against + fouls, data = data)
summary(best_model)

################################################################################

