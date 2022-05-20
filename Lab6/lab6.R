
DF <- read.csv("universities.csv")
data <- subset(DF, select=-c(ranking, title, location))

#------------------------------------------------------------------------------------

#(A)

mod1 <- lm(intl.outlook.score~., data)
summary(mod1)
# Call:
#   lm(formula = intl.outlook.score ~ ., data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -47.004  -7.769  -0.625   6.584  52.779 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            2.530e+01  1.268e+00  19.957  < 2e-16 ***
#   overall.score          1.462e+00  1.141e-01  12.822  < 2e-16 ***
#   teaching.score        -9.428e-01  5.492e-02 -17.165  < 2e-16 ***
#   research.score        -1.379e-02  6.084e-02  -0.227  0.82069    
# citations.score       -2.576e-01  3.749e-02  -6.871 9.26e-12 ***
#   industry.income.score -6.601e-02  1.967e-02  -3.356  0.00081 ***
#   number.students        3.040e-05  1.158e-05   2.626  0.00874 ** 
#   students.staff.ratio  -2.333e-02  2.406e-02  -0.969  0.33245    
# perc.intl.students     1.173e+00  3.023e-02  38.794  < 2e-16 ***
#   gender.ratio           1.239e-02  6.086e-02   0.204  0.83870    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.86 on 1516 degrees of freedom
# Multiple R-squared:  0.7814,	Adjusted R-squared:  0.7801 
# F-statistic: 602.2 on 9 and 1516 DF,  p-value: < 2.2e-16

mod2 <- lm(intl.outlook.score~.-gender.ratio, data)
summary(mod2)
# Call:
#   lm(formula = intl.outlook.score ~ . - gender.ratio, data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -47.014  -7.766  -0.626   6.578  52.767 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            2.532e+01  1.264e+00  20.036  < 2e-16 ***
#   overall.score          1.463e+00  1.140e-01  12.828  < 2e-16 ***
#   teaching.score        -9.426e-01  5.490e-02 -17.170  < 2e-16 ***
#   research.score        -1.396e-02  6.081e-02  -0.230 0.818506    
# citations.score       -2.577e-01  3.747e-02  -6.877 8.88e-12 ***
#   industry.income.score -6.606e-02  1.966e-02  -3.360 0.000798 ***
#   number.students        3.034e-05  1.157e-05   2.622 0.008828 ** 
#   students.staff.ratio  -2.324e-02  2.405e-02  -0.966 0.334032    
# perc.intl.students     1.173e+00  3.022e-02  38.806  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.86 on 1517 degrees of freedom
# Multiple R-squared:  0.7814,	Adjusted R-squared:  0.7803 
# F-statistic: 677.8 on 8 and 1517 DF,  p-value: < 2.2e-16

mod3 <- lm(intl.outlook.score~teaching.score+citations.score, data)
summary(mod3)
# Call:
#   lm(formula = intl.outlook.score ~ teaching.score + citations.score, 
#      data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -52.342 -13.360  -2.532  11.839  56.298 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     20.76422    1.17111   17.73  < 2e-16 ***
#   teaching.score   0.21659    0.04141    5.23 1.93e-07 ***
#   citations.score  0.42058    0.02043   20.58  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 18.94 on 1523 degrees of freedom
# Multiple R-squared:  0.3326,	Adjusted R-squared:  0.3317 
# F-statistic: 379.5 on 2 and 1523 DF,  p-value: < 2.2e-

#------------------------------------------------------------------------------------

#(B)

#mod3 is the worst(R-squared=0.3326, Adjusted R-squared=0.3317)
#mod1 is good(R-squared=0.7814, Adjusted R-squared=0.0.7801)
#but mod2 is slightly better then mod1(R-squared=0.7814, Adjusted R-squared=0.0.7803)

#mod2 is the best

#------------------------------------------------------------------------------------

#(C)

model0 <- lm(intl.outlook.score~0+teaching.score, data)
summary(mod0)
# Call:
#   lm(formula = intl.outlook.score ~ 0 + teaching.score, data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -90.878  -9.524   3.910  21.235  68.502 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# teaching.score  1.49008    0.02057   72.42   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 24.89 on 1525 degrees of freedom
# Multiple R-squared:  0.7747,	Adjusted R-squared:  0.7746 
# F-statistic:  5245 on 1 and 1525 DF,  p-value: < 2.2e-16


model1 <- lm(intl.outlook.score~teaching.score, data)
summary(model1)
# Call:
#   lm(formula = intl.outlook.score ~ teaching.score, data = data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -46.11 -16.94  -3.75  12.54  56.70 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    28.89446    1.24599   23.19   <2e-16 ***
#   teaching.score  0.65207    0.04024   16.21   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.41 on 1524 degrees of freedom
# Multiple R-squared:  0.147,	Adjusted R-squared:  0.1464 
# F-statistic: 262.6 on 1 and 1524 DF,  p-value: < 2.2e-16

plot(data$teaching.score, data$intl.outlook.score)
abline(model0, col="red")
abline(model1, col="blue")

#from the graphic we can see that model with intercept is obviously better

#------------------------------------------------------------------------------------

#(D)

datacen <- data.frame(scale(data, center = TRUE, scale = FALSE))

modelcen0 <- lm(intl.outlook.score~0+teaching.score, datacen)
summary(modelcen0)
# Call:
#   lm(formula = intl.outlook.score ~ 0 + teaching.score, data = datacen)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -46.11 -16.94  -3.75  12.54  56.70 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# teaching.score  0.65207    0.04022   16.21   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.4 on 1525 degrees of freedom
# Multiple R-squared:  0.147,	Adjusted R-squared:  0.1464 
# F-statistic: 262.8 on 1 and 1525 DF,  p-value: < 2.2e-16

modelcen1 <- lm(intl.outlook.score~teaching.score, datacen)
summary(modelcen1)
# Call:
#   lm(formula = intl.outlook.score ~ teaching.score, data = datacen)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -46.11 -16.94  -3.75  12.54  56.70 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -2.120e-15  5.480e-01    0.00        1    
# teaching.score  6.521e-01  4.024e-02   16.21   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.41 on 1524 degrees of freedom
# Multiple R-squared:  0.147,	Adjusted R-squared:  0.1464 
# F-statistic: 262.6 on 1 and 1524 DF,  p-value: < 2.2e-16

#R-squared and Adjusted R-squared of this two models are equal
#therefore we can use any of the models