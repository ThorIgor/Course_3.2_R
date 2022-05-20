DF <- read.csv("universities_cor.csv")

# Create factor variable
count <- 1
for(i in DF$gender.ratio) {
  if(i < 2/3) {
    DF$gender.ratio[count] <- "more male students"
  }
  else if(i < 3/2) {
    DF$gender.ratio[count] <- "middle"
  }
  else {
    DF$gender.ratio[count] <- "more female students"
  }
  count <- count+1
}
DF$gender.ratio <- as.factor(DF$gender.ratio)
DF$gender.ratio

# Delete chr variables
DF <- subset(DF, select = -ranking)
DF <- subset(DF, select = -title)
DF <- subset(DF, select = -location)
DF <- subset(DF, select = -X)

#(A)-----------------------------------------------------------

mod <- lm(number.students ~ gender.ratio+students.staff.ratio, data=DF)
summary(mod)

# Call:
#   lm(formula = number.students ~ gender.ratio + students.staff.ratio, 
#      data = DF)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -81651 -10745  -3973   6054 385780 
# 
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         8032.57    1179.89   6.808 1.42e-11 ***
#   gender.ratio more female students -4839.47    1667.90  -2.902  0.00377 ** 
#   gender.ratio more male students   -7954.49    1629.62  -4.881 1.17e-06 ***
#   students.staff.ratio               928.53      45.01  20.630  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 24150 on 1522 degrees of freedom
# Multiple R-squared:   0.24,	Adjusted R-squared:  0.2385 
# F-statistic: 160.2 on 3 and 1522 DF,  p-value: < 2.2e-16

#(B)-----------------------------------------------------------

mod_all <- lm(number.students ~ ., data=DF)
modBIC <- MASS::stepAIC(mod_all, k = log(nrow(DF)))

#                         Df  Sum of Sq       RSS   AIC
# <none>                               8.6672e+11 30804
# - teaching.score        1 7.8574e+09 8.7458e+11 30811
# - gender.ratio          2 1.6403e+10 8.8312e+11 30818
# - perc.intl.students    1 1.9850e+10 8.8657e+11 30832
# - students.staff.ratio  1 2.5026e+11 1.1170e+12 31184

summary(modBIC)

# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       6378.05    1832.94   3.480 0.000516 ***
#   teaching.score                     185.83      50.06   3.712 0.000213 ***
#   students.staff.ratio               939.22      44.83  20.950  < 2e-16 ***
#   perc.intl.students                -332.36      56.33  -5.900 4.47e-09 ***
#   gender.ratio more female students -4112.27    1662.01  -2.474 0.013459 *  
#   gender.ratio more male students   -8390.90    1615.48  -5.194 2.34e-07 ***

# The categorical variable gender.ratio remains in the optimal model
# It add to Y -4112.27 when more female students and -8390.90 when more male students  

#(C)-----------------------------------------------------------

Y <- DF$number.students
X <- DF$students.staff.ratio

#(1)-----------------------------------------------------------
mod = lm(Y ~ X)       
summary(mod)
# Residual standard error: 24350 on 1524 degrees of freedom
# Multiple R-squared:  0.2264,	Adjusted R-squared:  0.2259 
# F-statistic: 446.1 on 1 and 1524 DF,  p-value: < 2.2e-16
#(2)-----------------------------------------------------------
mod = lm(log(Y) ~ X)  
summary(mod)
# Residual standard error: 0.8273 on 1524 degrees of freedom
# Multiple R-squared:  0.1035,	Adjusted R-squared:  0.1029 
# F-statistic:   176 on 1 and 1524 DF,  p-value: < 2.2e-16
#(3)-----------------------------------------------------------
mod = lm(log(Y) ~ X)
summary(mod)
#Same as (2)
#(4)-----------------------------------------------------------
mod = lm(log(Y) ~ X)
summary(mod)
#Same as (2)
#(5)-----------------------------------------------------------
mod = lm(log(Y) ~ log(X)) 
summary(mod)
# Residual standard error: 0.7849 on 1524 degrees of freedom
# Multiple R-squared:  0.1931,	Adjusted R-squared:  0.1926 
# F-statistic: 364.8 on 1 and 1524 DF,  p-value: < 2.2e-16
#(6)-----------------------------------------------------------
mod = lm(Y ~ I(1/X))
summary(mod)
# Residual standard error: 27290 on 1524 degrees of freedom
# Multiple R-squared:  0.02833,	Adjusted R-squared:  0.0277 
# F-statistic: 44.44 on 1 and 1524 DF,  p-value: 3.657e-11
#(7)-----------------------------------------------------------
mod = lm(Y ~ I(X^2))  
summary(mod)
# Residual standard error: 23880 on 1524 degrees of freedom
# Multiple R-squared:  0.2562,	Adjusted R-squared:  0.2558 
# F-statistic: 525.1 on 1 and 1524 DF,  p-value: < 2.2e-16
#(8)-----------------------------------------------------------
mod = lm(Y ~ I(X^3))
summary(mod)
# Residual standard error: 24290 on 1524 degrees of freedom
# Multiple R-squared:  0.2303,	Adjusted R-squared:  0.2298 
# F-statistic:   456 on 1 and 1524 DF,  p-value: < 2.2e-16
#(9)-----------------------------------------------------------
mod = lm(Y ~ sqrt(X))
summary(mod)
# Residual standard error: 25550 on 1524 degrees of freedom
# Multiple R-squared:  0.1482,	Adjusted R-squared:  0.1476 
# F-statistic: 265.1 on 1 and 1524 DF,  p-value: < 2.2e-16
#(10)----------------------------------------------------------
mod = lm(Y ~ exp(X))
summary(mod)
# Residual standard error: 24670 on 1524 degrees of freedom
# Multiple R-squared:  0.2062,	Adjusted R-squared:  0.2057 
# F-statistic: 395.8 on 1 and 1524 DF,  p-value: < 2.2e-16
#(11)----------------------------------------------------------
mod = lm(Y ~ exp(-X))
summary(mod)
# Residual standard error: 27600 on 1524 degrees of freedom
# Multiple R-squared:  0.005819,	Adjusted R-squared:  0.005167 
# F-statistic:  8.92 on 1 and 1524 DF,  p-value: 0.002865
#(12)----------------------------------------------------------
mod = lm(Y ~ I(X^3 - log(abs(X)) + 2^X)) 
summary(mod)
# Residual standard error: 24670 on 1524 degrees of freedom
# Multiple R-squared:  0.2062,	Adjusted R-squared:  0.2057 
# F-statistic: 395.8 on 1 and 1524 DF,  p-value: < 2.2e-16

#(D)-----------------------------------------------------------

mod1 <- lm(Y~poly(X, degree = 1, raw = TRUE))
mod2 <- lm(Y~poly(X, degree = 2, raw = TRUE))
mod3 <- lm(Y~poly(X, degree = 3, raw = TRUE))
mod4 <- lm(Y~poly(X, degree = 4, raw = TRUE))
mod5 <- lm(Y~poly(X, degree = 5, raw = TRUE))
mod10 <- lm(Y~poly(X, degree = 10, raw = TRUE))

BIC(mod1, mod2, mod3, mod4, mod5, mod10)
#       df      BIC
# mod1   3 35176.53   the best
# mod2   4 35091.41
# mod3   5 35089.17
# mod4   6 35062.99
# mod5   7 35066.86
# mod10 11 35093.38

plot(X, Y, xlab="X", ylab="Y")
lines(X, mod1$fitted.values, col=1)
lines(X, mod2$fitted.values, col=2)
lines(X, mod3$fitted.values, col=3)
lines(X, mod4$fitted.values, col=4)
lines(X, mod5$fitted.values, col=5)
lines(X, mod10$fitted.values, col=6)

#(E)-----------------------------------------------------------

mod1 <- lm(Y~poly(X, degree = 1))
mod2 <- lm(Y~poly(X, degree = 2))
mod3 <- lm(Y~poly(X, degree = 3))
mod4 <- lm(Y~poly(X, degree = 4))
mod5 <- lm(Y~poly(X, degree = 5))
mod10 <- lm(Y~poly(X, degree = 10))

BIC(mod1, mod2, mod3, mod4, mod5, mod10)
#       df      BIC
# mod1   3 35176.53   the best
# mod2   4 35091.41
# mod3   5 35089.17
# mod4   6 35062.99
# mod5   7 35066.86
# mod10 12 35100.71

plot(X, Y, xlab="X", ylab="Y")
lines(X, mod1$fitted.values, col=1)
lines(X, mod2$fitted.values, col=2)
lines(X, mod3$fitted.values, col=3)
lines(X, mod4$fitted.values, col=4)
lines(X, mod5$fitted.values, col=5)
lines(X, mod10$fitted.values, col=6)

#(F)-----------------------------------------------------------

Y <- DF$number.students
X1 <- DF$students.staff.ratio
X2 <- DF$perc.intl.students
X3 <- DF$teaching.score

mod1 <- lm(Y ~ X1*X2)     
mod2 <- lm(Y ~ X1*X2*X3)

summary(mod1) #0.2737
summary(mod2) #0.3045

MASS::stepAIC(mod_all, scope = number.students ~ .^2, k = log(nobs(modBIC)), trace = 0)

#(G)-----------------------------------------------------------

D <- DF$gender.ratio

summary(lm(Y ~ X1+D))     #0.24
summary(lm(Y ~ X1*D))     #0.2569 the best
summary(lm(Y ~ X1+X1:D))  #0.2463


