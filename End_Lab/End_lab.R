DF <- read.csv("universities_cor.csv")
DF <- subset(DF, select=-c(X))
DF$gender.ratio <- as.factor(DF$gender.ratio)

data <- subset(DF, select = -c(ranking, title, location))

m1 <- lm(overall.score~teaching.score, data=data)
summary(m1)
# Residual standard error: 9.481 on 1524 degrees of freedom
# Multiple R-squared:   0.68,	Adjusted R-squared:  0.6798 
# F-statistic:  3239 on 1 and 1524 DF,  p-value: < 2.2e-16
m2 <- lm(overall.score~research.score, data=data)
summary(m2)
# Residual standard error: 7.633 on 1524 degrees of freedom
# Multiple R-squared:  0.7926,	Adjusted R-squared:  0.7924 
# F-statistic:  5823 on 1 and 1524 DF,  p-value: < 2.2e-16
m3 <- lm(overall.score~citations.score, data=data)
summary(m3)
# Residual standard error: 8.495 on 1524 degrees of freedom
# Multiple R-squared:  0.7431,	Adjusted R-squared:  0.7429 
# F-statistic:  4408 on 1 and 1524 DF,  p-value: < 2.2e-16
m4 <- lm(overall.score~industry.income.score, data=data)
summary(m4)
# Residual standard error: 15.27 on 1524 degrees of freedom
# Multiple R-squared:  0.1704,	Adjusted R-squared:  0.1699 
# F-statistic: 313.1 on 1 and 1524 DF,  p-value: < 2.2e-16
m5 <- lm(overall.score~intl.outlook.score, data=data)
summary(m5)
# Residual standard error: 12.76 on 1524 degrees of freedom
# Multiple R-squared:  0.4202,	Adjusted R-squared:  0.4199 
# F-statistic:  1105 on 1 and 1524 DF,  p-value: < 2.2e-16
m6 <- lm(overall.score~number.students, data=data)
summary(m6)
plot(data$number.students, data$overall.score)
# Residual standard error: 16.76 on 1524 degrees of freedom
# Multiple R-squared:  6.132e-06,	Adjusted R-squared:  -0.00065 
# F-statistic: 0.009345 on 1 and 1524 DF,  p-value: 0.923
m6_a <- lm(overall.score~I(1/number.students), data=data)
summary(m6_a)
# Residual standard error: 16.75 on 1524 degrees of freedom
# Multiple R-squared:  0.001288,	Adjusted R-squared:  0.0006331 
# F-statistic: 1.966 on 1 and 1524 DF,  p-value: 0.1611
m7 <- lm(overall.score~students.staff.ratio, data=data)
summary(m7)
plot(data$students.staff.ratio, data$overall.score)
# Residual standard error: 16.76 on 1524 degrees of freedom
# Multiple R-squared:  0.0002334,	Adjusted R-squared:  -0.0004226 
# F-statistic: 0.3558 on 1 and 1524 DF,  p-value: 0.5509
m7_a <- lm(overall.score~I(1/students.staff.ratio), data=data)
summary(m7_a)
# Residual standard error: 16.76 on 1524 degrees of freedom
# Multiple R-squared:  0.0001327,	Adjusted R-squared:  -0.0005234 
# F-statistic: 0.2022 on 1 and 1524 DF,  p-value: 0.653
m8 <- lm(overall.score~perc.intl.students, data=data)
summary(m8)
# Residual standard error: 13.85 on 1524 degrees of freedom
# Multiple R-squared:  0.3175,	Adjusted R-squared:  0.3171 
# F-statistic: 709.1 on 1 and 1524 DF,  p-value: < 2.2e-16


plot(data$research.score, data$overall.score)
abline(m2)

hist(m2$residuals)
plot(m2, 1)
plot(m2, 2)
plot(m2, 3)
plot(m2$residuals, type="o")

data <- subset(DF, select = -c(ranking, title, location))
mod1 <- lm(overall.score~., data=data)
summary(mod1)
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       7.939e-01  3.084e-01   2.574 0.010137 *  
#   teaching.score                    2.699e-01  1.080e-02  24.994  < 2e-16 ***
#   research.score                    3.354e-01  9.804e-03  34.212  < 2e-16 ***
#   citations.score                   2.961e-01  2.951e-03 100.327  < 2e-16 ***
#   industry.income.score             1.456e-02  4.346e-03   3.351 0.000826 ***
#   intl.outlook.score                6.624e-02  5.294e-03  12.512  < 2e-16 ***
#   number.students                  -2.018e-07  2.501e-06  -0.081 0.935682    
#   students.staff.ratio             -7.579e-03  5.147e-03  -1.473 0.141084    
#   perc.intl.students                1.327e-02  9.170e-03   1.447 0.148073    
#   gender.ratiomore female students  2.408e-01  1.639e-01   1.470 0.141860    
#   gender.ratiomore male students    6.661e-02  1.682e-01   0.396 0.692072    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.323 on 1515 degrees of freedom
# Multiple R-squared:  0.9809,	Adjusted R-squared:  0.9808 
# F-statistic:  7783 on 10 and 1515 DF,  p-value: < 2.2e-16

mod2 <- lm(overall.score~.-number.students - gender.ratio - perc.intl.students - students.staff.ratio, data=data)
summary(mod2)
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)           0.503671   0.262171   1.921 0.054900 .  
#   teaching.score        0.278841   0.010132  27.520  < 2e-16 ***
#   research.score        0.329304   0.009458  34.818  < 2e-16 ***
#   citations.score       0.295114   0.002890 102.123  < 2e-16 ***
#   industry.income.score 0.014042   0.004204   3.340 0.000857 ***
#   intl.outlook.score    0.072966   0.003407  21.416  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.325 on 1520 degrees of freedom
# Multiple R-squared:  0.9808,	Adjusted R-squared:  0.9807 
# F-statistic: 1.554e+04 on 5 and 1520 DF,  p-value: < 2.2e-16

plot(mod2, 1)
plot(mod2, 2)
hist(mod2$residuals)
plot(mod2, 3)
plot(mod2$residuals, type="o")

car::ncvTest(mod2)

data <- subset(data, select = -c(gender.ratio))
corrplot::corrplot(cor(data), addCoef.col = "grey")


data <- subset(DF, select = -c(ranking, title, location, teaching.score, citations.score, intl.outlook.score))
mod3 <- lm(overall.score~.,data=data)
summary(mod3)
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       1.632e+01  6.086e-01  26.815  < 2e-16 ***
#   research.score                    8.106e-01  1.450e-02  55.915  < 2e-16 ***
#   industry.income.score            -4.062e-02  1.320e-02  -3.076  0.00213 ** 
#   number.students                  -5.298e-07  7.634e-06  -0.069  0.94468    
#   students.staff.ratio             -4.522e-02  1.506e-02  -3.003  0.00271 ** 
#   perc.intl.students                1.993e-01  1.797e-02  11.087  < 2e-16 ***
#   gender.ratiomore female students -2.428e-02  4.995e-01  -0.049  0.96125    
#   gender.ratiomore male students   -3.399e+00  5.020e-01  -6.771 1.83e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.118 on 1518 degrees of freedom
# Multiple R-squared:  0.8203,	Adjusted R-squared:  0.8195 
# F-statistic: 990.2 on 7 and 1518 DF,  p-value: < 2.2e-16

data <- subset(data, select = -c(number.students, gender.ratio))
mod4 <- lm(overall.score~., data=data)
summary(mod4)
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)           16.23244    0.58666  27.669  < 2e-16 ***
#   research.score         0.82526    0.01445  57.112  < 2e-16 ***
#   industry.income.score -0.06574    0.01285  -5.115 3.53e-07 ***
#   students.staff.ratio  -0.03468    0.01343  -2.583  0.00987 ** 
#   perc.intl.students     0.20315    0.01800  11.283  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.223 on 1521 degrees of freedom
# Multiple R-squared:  0.8146,	Adjusted R-squared:  0.8141 
# F-statistic:  1671 on 4 and 1521 DF,  p-value: < 2.2e-16


plot(mod4, 1)
plot(mod4, 2)
hist(mod4$residuals)
plot(mod4, 3)
plot(mod4$residuals, type="o")

car::ncvTest(mod4)

data <- subset(DF, select = -c(ranking, title, location))

n <- nrow(DF)
modZero <- lm(overall.score ~ 1, data=DF)
modAll <- lm(overall.score~., data=data)

mod_aic <- MASS::stepAIC(modAll, direction = "both", trace = 0, scope = list(lower = modZero, upper = modAll), k = log(n))
summary(mod_aic)


mod_intresting <- lm(number.students~gender.ratio, data=data)
summary(mod_intresting)
