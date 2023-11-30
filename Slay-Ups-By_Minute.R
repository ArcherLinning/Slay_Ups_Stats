# set working directory

setwd("C:/Users/linni/OneDrive/Desktop/Personal/Practice_Programming/Slay-Ups-Stats")

# read data
data2 = read.csv("Data/Slay-Ups-By_Minute.csv")
attach(data2)

# offense
offense = c(game1_off, game2_off, game3_off, game4_off, game5_off, game6_off,
            game7_off, game8_off, game9_off, game10_off, game11_off, game12_off,
            game13_off, game14_off, game15_off, game16_off, game17_off,
            game18_off, game19_off, game20_off, game21_off, game22_off,
            game23_off, game24_off, game25_off,game26_off, game27_off)

# defense
defense = c(game1_off, game2_off, game3_off, game4_off, game5_off, game6_off,
            game7_off, game8_off, game9_off, game10_off, game11_off, game12_off,
            game13_off, game14_off, game15_off, game16_off, game17_off,
            game18_off, game19_off, game20_off, game21_off, game22_off,
            game23_off, game24_off, game25_off,game26_off, game27_off)

# plus-minus
pm = offense - defense

# set up each person's minutes
Archer = subset(data2, minute <= 4|(10 < minute & minute <= 28)|minute > 34)
Dan = subset(data2, minute <= 4|(10 < minute & minute <= 28)|minute > 34)
Booker = subset(data2, (4 < minute & minute <= 16)|minute > 22)
Chapman = subset(data2, minute <= 16|(22 < minute & minute <= 34))
Anton = subset(data2, minute <= 10|(16 < minute & minute <= 22)|minute > 28)
Joseph = subset(data2, (4 < minute & minute <= 22)|minute > 28)
Mitch = subset(data2, minute < 10|(16 < minute & minute <= 34))

################################################################################
#                         Offensive Output by person                           #
################################################################################

## NOTE: MUST ADD COLUMNS IN COLSUMS WHEN NEW DATA AVAILABLE (for all calcs) ##

# mean offensive output
Archer_off = mean(colSums(Archer[,c(2,4,6,8,10)], na.rm = TRUE))
Dan_off = mean(colSums(Dan[,c(2,4,6,8,10)], na.rm = TRUE))
Booker_off = mean(colSums(Booker[,c(2,4,6,8,10)], na.rm = TRUE))
Chapman_off = mean(colSums(Chapman[,c(2,4,6,8,10)], na.rm = TRUE))
Anton_off = mean(colSums(Anton[,c(2,4,6,8,10)], na.rm = TRUE))
Joseph_off = mean(colSums(Joseph[,c(2,4,6,8,10)], na.rm = TRUE))
Mitch_off = mean(colSums(Mitch[,c(2,4,6,8,10)], na.rm = TRUE))

barplot(c(Archer_off, Dan_off, Booker_off, Chapman_off, Anton_off, Joseph_off, Mitch_off),
        names.arg = c("Archer", "Dan", "Booker", "Chapman", "Anton", "Joseph",
        "Mitch"), col = c("red", "red", "lightgreen", "yellow","lightgreen","green",
        "lightyellow"), main = "Offense by Person")

################################################################################
#                         Defensive Output by person                           #
################################################################################

# mean defensive output
Archer_def = mean(colSums(Archer[,c(3,5,7,9,11)], na.rm = TRUE))
Dan_def = mean(colSums(Dan[,c(3,5,7,9,11)], na.rm = TRUE))
Booker_def = mean(colSums(Booker[,c(3,5,7,9,11)], na.rm = TRUE))
Chapman_def = mean(colSums(Chapman[,c(3,5,7,9,11)], na.rm = TRUE))
Anton_def = mean(colSums(Anton[,c(3,5,7,9,11)], na.rm = TRUE))
Joseph_def = mean(colSums(Joseph[,c(3,5,7,9,11)], na.rm = TRUE))
Mitch_def = mean(colSums(Mitch[,c(3,5,7,9,11)], na.rm = TRUE))

barplot(c(Archer_def,Dan_def, Booker_def, Chapman_def, Anton_def, Joseph_def, Mitch_def),
        names.arg = c("Archer", "Dan", "Booker", "Chapman", "Anton", "Joseph",
        "Mitch"), col = c("green", "green", "red", "lightyellow", "lightyellow",
        "yellow", "lightgreen"), main = "Defense by Person")

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
        "Mitch"), col = c("red", "red", "yellow", "yellow","lightgreen","green",
        "lightgreen"), main = "Plus-Minus by Person")


plot(c(1,2,3,4,5,6,7), c(Archer_pm,Dan_pm, Booker_pm, Chapman_pm, Anton_pm, Joseph_pm,
        Mitch_pm), xaxt = 'n', xlab = "Person", ylab = "Plus-Minus", ylim = c(-1.5,10),
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


starters_off = mean(colSums(starters[,c(2,4,6,8,10)], na.rm = TRUE))
linn_sub1_off = mean(colSums(linn_sub1[,c(2,4,6,8,10)], na.rm = TRUE))
AM_sub_off = mean(colSums(AM_sub[,c(2,4,6,8,10)], na.rm = TRUE))
jamess_sub_off = mean(colSums(jamess_sub[,c(2,4,6,8,10)], na.rm = TRUE))
JOAN_sub_off = mean(colSums(JOAN_sub[,c(2,4,6,8,10)], na.rm = TRUE))
linn_sub2_off = mean(colSums(linn_sub2[,c(2,4,6,8,10)], na.rm = TRUE))
end_off = mean(colSums(end[,c(2,4,6,8,10)], na.rm = TRUE))

barplot(c(starters_off, linn_sub1_off, AM_sub_off, jamess_sub_off, JOAN_sub_off,
          linn_sub2_off, end_off), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Offense by Lineup")


################################################################################
#                               DEFENSE BY LINEUP                              #
################################################################################

starters_def = mean(colSums(starters[,c(3,5,7,9,11)], na.rm = TRUE))
linn_sub1_def = mean(colSums(linn_sub1[,c(3,5,7,9,11)], na.rm = TRUE))
AM_sub_def = mean(colSums(AM_sub[,c(3,5,7,9,11)], na.rm = TRUE))
jamess_sub_def = mean(colSums(jamess_sub[,c(3,5,7,9,11)], na.rm = TRUE))
JOAN_sub_def = mean(colSums(JOAN_sub[,c(3,5,7,9,11)], na.rm = TRUE))
linn_sub2_def = mean(colSums(linn_sub2[,c(3,5,7,9,11)], na.rm = TRUE))
end_def = mean(colSums(end[,c(3,5,7,9,11)], na.rm = TRUE))

barplot(c(starters_def, linn_sub1_def, AM_sub_def, jamess_sub_def, JOAN_sub_def,
          linn_sub2_def, end_def), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Defense by Lineup")

################################################################################
#                               PLUS-MINUS BY LINEUP                           #
################################################################################

starters_pm = starters_off - starters_def
linn_sub1_pm = linn_sub1_off - linn_sub1_def 
AM_sub_pm = AM_sub_off - AM_sub_def
jamess_sub_pm = jamess_sub_off - jamess_sub_def
JOAN_sub_pm = JOAN_sub_off - JOAN_sub_def
linn_sub2_pm = linn_sub2_off - linn_sub2_def 
end_pm = end_off - end_def

barplot(c(starters_pm, linn_sub1_pm, AM_sub_pm, jamess_sub_pm, JOAN_sub_pm,
          linn_sub2_pm, end_pm), names.arg = c("Starters", "Linnings Off (1)",
          "Anton/Mitch Off", "James' Off", "Jo/Anton Off", "Linnings Off (2)",
          "Endgame"), main = "Plus-Minus by Lineup")
