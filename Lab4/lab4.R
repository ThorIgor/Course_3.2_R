library(lubridate)
library(data.table)
library(dplyr)
library(utils)
library(AER)
library(stargazer)


DF <- read.csv("universities.csv")

#(A)

mod1 <- lm(teaching.score ~ research.score + citations.score + intl.outlook.score + gender.ratio, data = DF)
summary(mod1)

#(B)

#F-statistic:  1639 on 4 and 1521 DF,  p-value: < 2.2e-16
#a. k1 = 4
#b. k2 = 1521DF
#c. F = 2.37
#d. F-statistic = 1639
#e. Можем відхилити нульову гіпотезу про незначущість кофіцієнтів

#(C)

#a. k = 1521
#b.                 t value
#(Intercept)        36.648
#research.score     65.885
#citations.score    2.436
#intl.outlook.score -10.287
#gender.ratio       0.776
#c. kr = 1.96
#d.
linearHypothesis(mod1, c("gender.ratio=0"))
# Hypothesis:
#   gender.ratio = 0
# 
# Model 1: restricted model
# Model 2: teaching.score ~ research.score + citations.score + intl.outlook.score + 
#   gender.ratio
# 
# Res.Df   RSS Df Sum of Sq      F Pr(>F)
# 1   1522 53328                           
# 2   1521 53307  1    21.086 0.6016 0.4381

linearHypothesis(mod1, c("citations.score=0"))
# Linear hypothesis test
# 
# Hypothesis:
#   citations.score = 0
# 
# Model 1: restricted model
# Model 2: teaching.score ~ research.score + citations.score + intl.outlook.score + 
#   gender.ratio
# 
# Res.Df   RSS Df Sum of Sq      F  Pr(>F)  
# 1   1522 53515                              
# 2   1521 53307  1       208 5.9349 0.01496 *

#Можна зробити висновок що gender.ratio незначущий і від нього можна позбутись
#а citaions.score при alfa = 0.05 значуща, тому від нього не можна позбуватись

#(D)

#a = 0.1
confint(mod1, level = 0.9)
#                            5 %        95 %
#(Intercept)        12.743538769 13.94199739
#research.score      0.728392718  0.76571671
#citations.score     0.005766836  0.02978655
#intl.outlook.score -0.099445999 -0.07201244
#gender.ratio       -0.028838614  0.08024851

#a = 0.05
confint(mod1, level = 0.95)
#                          2.5 %      97.5 %
#(Intercept)        12.628608906 14.05692725
#research.score      0.724813419  0.76929601
#citations.score     0.003463392  0.03208999
#intl.outlook.score -0.102076824 -0.06938162
#gender.ratio       -0.039299859  0.09070976

#a = 0.01
confint(mod1, level = 0.99)
#                         0.5 %      99.5 %
#(Intercept)        12.40377243 14.28176373
#research.score      0.71781126  0.77629816
#citations.score    -0.00104282  0.03659621
#intl.outlook.score -0.10722349 -0.06423495
#gender.ratio       -0.05976512  0.11117502

#(E)

summary(predict(mod1, interval = "confidence"))
# Min.   :12.40   Min.   :11.49   Min.   :13.31  
# 1st Qu.:19.51   1st Qu.:19.00   1st Qu.:20.03  
# Median :23.58   Median :23.09   Median :24.16  
# Mean   :27.81   Mean   :27.23   Mean   :28.39  
# 3rd Qu.:31.48   3rd Qu.:30.99   3rd Qu.:32.02  
# Max.   :82.28   Max.   :80.91   Max.   :83.66 

#(F)
#a.
avgX <- data.frame(research.score = mean(DF$research.score),
                   citations.score = mean(DF$citations.score),
                   intl.outlook.score = mean(DF$intl.outlook.score),
                   gender.ratio = mean(DF$gender.ratio))
predict(mod1, newdata = avgX)
#       1 
#27.81134 

#b.
predict(mod1, newdata = avgX, interval = "prediction")
#       fit      lwr      upr
#1 27.81134 16.19514 39.42753

#c.
maxX <- data.frame(research.score = max(DF$research.score)*1.1,
                   citations.score = max(DF$citations.score)*1.1,
                   intl.outlook.score = max(DF$intl.outlook.score)*1.1,
                   gender.ratio = max(DF$gender.ratio)*1.1)
predict(mod1, newdata = maxX)
#       1 
#90.54285 

#d.
predict(mod1, newdata = maxX, interval = "prediction")
#       fit      lwr      upr
#1 90.54285 76.85025 104.2355