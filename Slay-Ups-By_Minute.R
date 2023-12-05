# set working directory
setwd("C:/Users/linni/OneDrive/Desktop/Personal/Practice_Programming/Slay-Ups-Stats")

# read data
data2 = read.csv("Data/Slay-Ups-By_Minute.csv")
attach(data2)

# offense - add new column when new data
offense = c(game1_off, game2_off, game3_off, game4_off, game5_off, game6_off,
            game7_off, game8_off, game9_off, game10_off, game11_off, game12_off,
            game13_off, game14_off, game15_off, game16_off, game17_off,
            game18_off, game19_off, game20_off, game21_off, game22_off,
            game23_off, game24_off, game25_off,game26_off, game27_off, game28_off)

# defense - - add new column when new data
defense = c(game1_def, game2_def, game3_def, game4_def, game5_def, game6_def,
            game7_def, game8_def, game9_def, game10_def, game11_def, game12_def,
            game13_def, game14_def, game15_def, game16_def, game17_def,
            game18_def, game19_def, game20_def, game21_def, game22_def,
            game23_def, game24_def, game25_def,game26_def, game27_def, game28_def)

# plus-minus
pm = offense - defense

# data columns - ADD NEW COLUMNS WHEN NEW DATA AVAILABLE
off_columns = c(2,4,6,8,10,12,14,16,24,26,28,56)
def_columns = c(3,5,7,9,11,13,15,17,25,27,29,57)

################################################################################
#                             Team Output by Minute                            #
################################################################################

# by minute offense
plot(minute, na.omit(offense[1:40]))
for (i in 2:length(off_columns)){
  points(minute, na.pass(offense[(40*i - 39):(40*i)]))}

# by minute defense
plot(minute, na.omit(defense[1:40]))
for (i in 2:length(def_columns)){
  points(minute, na.pass(defense[(40*i - 39):(40*i)]))}

# by minute plus-minus
plot(minute, na.omit(pm[1:40]))
for (i in 2:length(off_columns)){
  points(minute, na.pass(pm[(40*i - 39):(40*i)]))}

## Averages ##
mean_off_minute = rep(0,40)

for (j in 1:40){
    minute_subset = subset(data2, minute == j)
    mean_off_minute[j] = mean(colSums(minute_subset[,off_columns]))
}

mean_def_minute = rep(0,40)

for (k in 1:40){
  minute_subset = subset(data2, minute == k)
  mean_def_minute[k] = mean(colSums(minute_subset[,def_columns]))
}

mean_pm_minute = rep(0,40)

for (l in 1:40){
  minute_subset = subset(data2, minute == l)
  mean_pm_minute[l] = mean(colSums(minute_subset[,off_columns])) - mean(colSums(minute_subset[,def_columns]))
}

# plots
plot(minute, mean_off_minute, col = "green", pch = 3, type = 'b', ylim = c(-2.5, 3.5),
     ylab = "Average Points", xlab = "Minute")
points(minute, mean_def_minute, col = "red", pch = 4)
points(minute, mean_pm_minute, col = "yellow", pch = 1)
lines(minute, mean_def_minute, col = "red")
lines(minute, mean_pm_minute, col = "yellow")
legend(-1, 4, c("Points for", "Points against", "Plus-minus"),
       col = c("green", "red", "yellow"), pch = c(3,4,1), cex = 1, bty = 'n',
       x.intersp = 0.4, y.intersp = 0.4)

barplot(mean_pm_minute, names.arg = c('1','2','3','4','5','6','7','8','9','10',
        '11','12','13','14','15','16','17','18','19','20','21','22','23','24','25',
        '26', '27','28','29','30','31','32','33','34','35','36','37','38','39','40'),
        main = "Average Plus-Minus by Minute", axis.lty = 1, ylim = c(-2.5,2.5),
        ylab = "Average Points", xlab = "Minute")

################################################################################
#                               Output by person                               #
################################################################################

# set up each person's minutes
Archer = subset(data2, minute <= 4|(10 < minute & minute <= 28)|34 < minute)
Dan = subset(data2, minute <= 4|(10 < minute & minute <= 28)|34 < minute)
Booker = subset(data2, (4 < minute & minute <= 16)|22 < minute)
Chapman = subset(data2, minute <= 16|(22 < minute & minute <= 34))
Anton = subset(data2, minute <= 10|(16 < minute & minute <= 22)|28 < minute)
Joseph = subset(data2, (4 < minute & minute <= 22)|28 < minute)
Mitch = subset(data2, minute <= 10|(16 < minute & minute <= 34))

################################################################################
#                         Offensive Output by person                           #
################################################################################

# mean offensive output
Archer_off = mean(colSums(Archer[,off_columns], na.rm = TRUE))
Dan_off = mean(colSums(Dan[,off_columns], na.rm = TRUE))
Booker_off = mean(colSums(Booker[,off_columns], na.rm = TRUE))
Chapman_off = mean(colSums(Chapman[,off_columns], na.rm = TRUE))
Anton_off = mean(colSums(Anton[,off_columns], na.rm = TRUE))
Joseph_off = mean(colSums(Joseph[,off_columns], na.rm = TRUE))
Mitch_off = mean(colSums(Mitch[,off_columns], na.rm = TRUE))

barplot(c(Archer_off, Dan_off, Booker_off, Chapman_off, Anton_off, Joseph_off, Mitch_off),
        names.arg = c("Archer", "Dan", "Booker", "Chapman", "Anton", "Joseph",
        "Mitch"), col = c("yellow", "yellow", "lightgreen", "lightgreen","lightgreen","green",
        "lightgreen"), main = "Offense by Person", ylab = "Points", xlab = "Person")

################################################################################
#                         Defensive Output by person                           #
################################################################################

# mean defensive output
Archer_def = mean(colSums(Archer[,def_columns], na.rm = TRUE))
Dan_def = mean(colSums(Dan[,def_columns], na.rm = TRUE))
Booker_def = mean(colSums(Booker[,def_columns], na.rm = TRUE))
Chapman_def = mean(colSums(Chapman[,def_columns], na.rm = TRUE))
Anton_def = mean(colSums(Anton[,def_columns], na.rm = TRUE))
Joseph_def = mean(colSums(Joseph[,def_columns], na.rm = TRUE))
Mitch_def = mean(colSums(Mitch[,def_columns], na.rm = TRUE))

barplot(c(Archer_def,Dan_def, Booker_def, Chapman_def, Anton_def, Joseph_def, Mitch_def),
        names.arg = c("Archer", "Dan", "Booker", "Chapman", "Anton", "Joseph",
        "Mitch"), col = c("green", "green", "red", "lightgreen", "lightgreen",
        "yellow", "yellow"), main = "Defense by Person", ylab = "Points", xlab = "Person")

################################################################################
#                         Plus-Minus Output by person                          #
################################################################################

# mean plus-minus output
Archer_pm = Archer_off - Archer_def
Dan_pm = Dan_off - Dan_def
Booker_pm = Booker_off - Booker_def
Chapman_pm = Chapman_off - Chapman_def
Anton_pm = Anton_off - Anton_def
Joseph_pm = Joseph_off - Joseph_def
Mitch_pm = Mitch_off - Mitch_def

barplot(c(Archer_pm, Dan_pm, Booker_pm, Chapman_pm, Anton_pm, Joseph_pm, Mitch_pm),
        names.arg = c("Archer", "Dan", "Booker", "Chapman", "Anton", "Joseph",
        "Mitch"), col = c("red", "red", "lightgreen", "yellow","lightgreen","green",
        "yellow"), main = "Plus-Minus by Person", ylab = "Points", xlab = "Person")


plot(c(1,2,3,4,5,6,7), c(Archer_pm,Dan_pm, Booker_pm, Chapman_pm, Anton_pm, Joseph_pm,
        Mitch_pm), xaxt = 'n', xlab = "Person", ylab = "Plus-Minus", ylim = c(-1,15),
     pch = 4, main = "Plus-Minus by Person")
axis(1, at = c(1,2,3,4,5,6,7) , c("Archer", "Dan", "Booker", "Chapman", "Anton", "Joseph",
               "Mitch"))

################################################################################
#                               OFFENSE BY LINEUP                              #
################################################################################

starters = subset(data2, minute <= 4)
linn_sub1 = subset(data2, 4 < minute & minute <= 10)
AM_sub = subset(data2, 10 < minute & minute <= 16)
jamess_sub = subset(data2, 16 < minute & minute <= 22)
JOAN_sub = subset(data2, 22 < minute & minute <= 28)
linn_sub2 = subset(data2, 28 < minute & minute <= 34)
end = subset(data2, 34 < minute)

# raw
starters_off = mean(colSums(starters[,off_columns], na.rm = TRUE))
linn_sub1_off = mean(colSums(linn_sub1[,off_columns], na.rm = TRUE))
AM_sub_off = mean(colSums(AM_sub[,off_columns], na.rm = TRUE))
jamess_sub_off = mean(colSums(jamess_sub[,off_columns], na.rm = TRUE))
JOAN_sub_off = mean(colSums(JOAN_sub[,off_columns], na.rm = TRUE))
linn_sub2_off = mean(colSums(linn_sub2[,off_columns], na.rm = TRUE))
end_off = mean(colSums(end[,off_columns], na.rm = TRUE))

barplot(c(starters_off, linn_sub1_off, AM_sub_off, jamess_sub_off, JOAN_sub_off,
          linn_sub2_off, end_off), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Offense by Lineup", ylab = "Average Points", xlab = "Lineup")

# weighted - let 6 be index of 1 (i.e. multiply by 6/(number of minutes))
starters_off_w = (6/nrow(starters))*mean(colSums(starters[,off_columns], na.rm = TRUE))
linn_sub1_off_w = (6/nrow(linn_sub1))*mean(colSums(linn_sub1[,off_columns], na.rm = TRUE))
AM_sub_off_w = (6/nrow(AM_sub))*mean(colSums(AM_sub[,off_columns], na.rm = TRUE))
jamess_sub_off_w = (6/nrow(jamess_sub))*mean(colSums(jamess_sub[,off_columns], na.rm = TRUE))
JOAN_sub_off_w = (6/nrow(JOAN_sub))*mean(colSums(JOAN_sub[,off_columns], na.rm = TRUE))
linn_sub2_off_w = (6/nrow(linn_sub2))*mean(colSums(linn_sub2[,off_columns], na.rm = TRUE))
end_off_w = (6/nrow(end))*mean(colSums(end[,off_columns], na.rm = TRUE))

barplot(c(starters_off_w, linn_sub1_off_w, AM_sub_off_w, jamess_sub_off_w, JOAN_sub_off_w,
          linn_sub2_off_w, end_off_w), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Offense by Lineup (Weighted)", ylab = "Weighted Average", xlab = "Lineup")


################################################################################
#                               DEFENSE BY LINEUP                              #
################################################################################

# raw
starters_def = mean(colSums(starters[,def_columns], na.rm = TRUE))
linn_sub1_def = mean(colSums(linn_sub1[,def_columns], na.rm = TRUE))
AM_sub_def = mean(colSums(AM_sub[,def_columns], na.rm = TRUE))
jamess_sub_def = mean(colSums(jamess_sub[,def_columns], na.rm = TRUE))
JOAN_sub_def = mean(colSums(JOAN_sub[,def_columns], na.rm = TRUE))
linn_sub2_def = mean(colSums(linn_sub2[,def_columns], na.rm = TRUE))
end_def = mean(colSums(end[,def_columns], na.rm = TRUE))

barplot(c(starters_def, linn_sub1_def, AM_sub_def, jamess_sub_def, JOAN_sub_def,
          linn_sub2_def, end_def), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Defense by Lineup", ylab = "Average Points", xlab = "Lineup")

# weighted - let 6 be index of 1 (i.e. multiply by 6/(number of minutes))
starters_def_w = (6/nrow(starters))*mean(colSums(starters[,def_columns], na.rm = TRUE))
linn_sub1_def_w = (6/nrow(linn_sub1))*mean(colSums(linn_sub1[,def_columns], na.rm = TRUE))
AM_sub_def_w = (6/nrow(AM_sub))*mean(colSums(AM_sub[,def_columns], na.rm = TRUE))
jamess_sub_def_w = (6/nrow(jamess_sub))*mean(colSums(jamess_sub[,def_columns], na.rm = TRUE))
JOAN_sub_def_w = (6/nrow(JOAN_sub))*mean(colSums(JOAN_sub[,def_columns], na.rm = TRUE))
linn_sub2_def_w = (6/nrow(linn_sub2))*mean(colSums(linn_sub2[,def_columns], na.rm = TRUE))
end_def_w = (6/nrow(end))*mean(colSums(end[,def_columns], na.rm = TRUE))

barplot(c(starters_def_w, linn_sub1_def_w, AM_sub_def_w, jamess_sub_def_w, JOAN_sub_def_w,
          linn_sub2_def_w, end_def_w), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Defense by Lineup (Weighted)", ylab = "Weighted Average", xlab = "Lineup")

################################################################################
#                               PLUS-MINUS BY LINEUP                           #
################################################################################

# raw
starters_pm = starters_off - starters_def
linn_sub1_pm = linn_sub1_off - linn_sub1_def 
AM_sub_pm = AM_sub_off - AM_sub_def
jamess_sub_pm = jamess_sub_off - jamess_sub_def
JOAN_sub_pm = JOAN_sub_off - JOAN_sub_def
linn_sub2_pm = linn_sub2_off - linn_sub2_def 
end_pm = end_off - end_def

par(mfrow = c(1,2))

barplot(c(starters_pm, linn_sub1_pm, AM_sub_pm, jamess_sub_pm, JOAN_sub_pm,
          linn_sub2_pm, end_pm), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Plus-Minus by Lineup", ylab = "Average Points", xlab = "Lineup",
          ylim = c(-2,6))

# weighted
starters_pm_w = starters_off_w - starters_def_w
linn_sub1_pm_w = linn_sub1_off_w - linn_sub1_def_w
AM_sub_pm_w = AM_sub_off_w - AM_sub_def_w
jamess_sub_pm_w = jamess_sub_off_w - jamess_sub_def_w
JOAN_sub_pm_w = JOAN_sub_off_w - JOAN_sub_def_w
linn_sub2_pm_w = linn_sub2_off_w - linn_sub2_def_w
end_pm_w = end_off_w - end_def_w

barplot(c(starters_pm_w, linn_sub1_pm_w, AM_sub_pm_w, jamess_sub_pm_w, JOAN_sub_pm_w,
          linn_sub2_pm_w, end_pm_w), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Plus-Minus by Lineup (Weighted)", ylab = "Weighted Average", xlab = "Lineup",
          ylim = c(-2,6))
