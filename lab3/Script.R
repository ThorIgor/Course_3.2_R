load("assumptions.RData")

##----m1-----------------------------

m1 <- lm(y1 ~ x1, data = assumptions)
summ1<-summary(m1)
summary(m1)

plot(assumptions$x1, assumptions$y1, xlab="x", ylab = "y")
abline(m1, col=2)
summ1$r.squared #!
sum(summ1$residuals^2)
plot(summ1$residuals)
mean(summ1$residuals)
var(summ1$residuals)
hist(summ1$residuals)

var(assumptions$x1)
var(assumptions$y1)
hist(assumptions$x1)
hist(assumptions$y1)

plot(assumptions$x1, assumptions$y1, xlab="x", ylab = "y", xlim = c(-5, 5), ylim = c(-5, 5))

#----1. лінійність------
plot(m1, 1)
#----2. нормальність ------
plot(m1, 2)
#----3. гомоскедастичність------
plot(m1, 3)
#----4. незалежність похибок------
plot(m1$residuals, type = "o")

##----m2-----------------------------

m2 <- lm(y2 ~ x2, data = assumptions)

plot(assumptions$x2, assumptions$y2, xlab="x", ylab = "y")
abline(coef = m2$coefficients, col=2)

summ2<-summary(m2)
summ2$r.squared
sum(summ2$residuals^2)
plot(summ2$residuals) #!
var(summ2$residuals)
mean(summ2$residuals)
hist(summ2$residuals)

var(assumptions$x2)
var(assumptions$y2)
hist(assumptions$x2)
hist(assumptions$y2)

plot(assumptions$x2, assumptions$y2, xlab="x", ylab = "y", xlim = c(-10, 10), ylim = c(-10, 10))

plot(m2, 1)
plot(m2, 2)
plot(m2, 3)
plot(m2$residuals, type = "o")

##----m3-----------------------------

m3 <- lm(y3 ~ x3, data = assumptions)

plot(assumptions$x3, assumptions$y3, xlab="x", ylab = "y")
abline(coef = m3$coefficients, col=2)

summ3<-summary(m3)
summ3$r.squared
sum(summ3$residuals^2)
plot(summ3$residuals) #!
mean(summ3$residuals)
var(summ3$residuals)
hist(summ3$residuals)

var(assumptions$x3)
var(assumptions$y3)
hist(assumptions$x3)
hist(assumptions$y3)

plot(assumptions$x3, assumptions$y3, xlab="x", ylab = "y", xlim = c(-10, 10), ylim = c(-10, 10))

plot(m3, 1)
plot(m3, 2)
plot(m3, 3)
plot(m3$residuals, type = "o")
##----m4-----------------------------

m4 <- lm(y4 ~ x4, data = assumptions)

plot(assumptions$x4, assumptions$y4, xlab="x", ylab = "y")
abline(coef = m4$coefficients, col=2)

summ4<-summary(m4)
plot(assumptions$x4, assumptions$y4, xlab="x", ylab = "y", xlim = c(-10, 10), ylim = c(-10, 10))
summ4$r.squared
sum(summ4$residuals^2)
plot(summ4$residuals)
mean(summ4$residuals)
var(summ4$residuals)
hist(summ4$residuals)

var(assumptions$x4)
var(assumptions$y4)
hist(assumptions$x4)
hist(assumptions$y4)

plot(assumptions$x4, assumptions$y4, xlab="x", ylab = "y", xlim = c(-12, 12), ylim = c(-12, 12))

plot(m4, 1)
plot(m4, 2)
plot(m4, 3)
plot(m4$residuals, type = "o")
##----m5-----------------------------

m5 <- lm(y5 ~ x5, data = assumptions)

plot(assumptions$x5, assumptions$y5, xlab="x", ylab = "y")
abline(coef = m5$coefficients, col=2)

summ5<-summary(m5)
summ5$r.squared
sum(summ5$residuals^2)
plot(summ5$residuals)
mean(summ5$residuals)
var(summ5$residuals)
hist(summ5$residuals)

var(assumptions$x5)
var(assumptions$y5)
hist(assumptions$x5)
hist(assumptions$y5)

plot(assumptions$x5, assumptions$y5, xlab="x", ylab = "y", xlim = c(-5, 5), ylim = c(-5, 5))

plot(m5, 1)
plot(m5, 2)
plot(m5, 3)
plot(m5$residuals, type = "o")
##----m6-----------------------------

m6 <- lm(y6 ~ x6, data = assumptions)

plot(assumptions$x6, assumptions$y6, xlab="x", ylab = "y")
abline(coef = m6$coefficients, col=2)

summ6<-summary(m6)
summ6$r.squared
sum(summ6$residuals^2)
plot(summ6$residuals)
mean(summ6$residuals)
var(summ6$residuals)
hist(summ6$residuals)

var(assumptions$x6)
var(assumptions$y6)
hist(assumptions$x6)
hist(assumptions$y6)

plot(assumptions$x6, assumptions$y6, xlab="x", ylab = "y", xlim = c(-6, 6), ylim = c(-6, 6))

plot(m6, 1)
plot(m6, 2)
plot(m6, 3)
plot(m6$residuals, type = "o")
##----m7-----------------------------

m7 <- lm(y7 ~ x7, data = assumptions)

plot(assumptions$x7, assumptions$y7, xlab="x", ylab = "y")
abline(coef = m7$coefficients, col=2)

summ7<-summary(m7)
summ7$r.squared
sum(summ7$residuals^2)
plot(summ7$residuals)
mean(summ7$residuals)
var(summ7$residuals)
hist(summ7$residuals)

var(assumptions$x7)
var(assumptions$y7)
hist(assumptions$x7)
hist(assumptions$y7)

plot(assumptions$x7, assumptions$y7, xlab="x", ylab = "y", xlim = c(0, 22), ylim = c(-20, 20))

plot(m7, 1)
plot(m7, 2)
plot(m7, 3)
plot(m7$residuals, type = "o")
##----m8-----------------------------

m8 <- lm(y8 ~ x8, data = assumptions)

plot(assumptions$x8, assumptions$y8, xlab="x", ylab = "y")
abline(coef = m8$coefficients, col=2)

summ8<-summary(m8)
summ8$r.squared
sum(summ8$residuals^2)
plot(summ8$residuals)
mean(summ8$residuals)
var(summ8$residuals)
hist(summ8$residuals)

var(assumptions$x8)
var(assumptions$y8)
hist(assumptions$x8)
hist(assumptions$y8)

plot(assumptions$x8, assumptions$y8, xlab="x", ylab = "y", xlim = c(-100, 100), ylim = c(-200, 200))

plot(m8, 1)
plot(m8, 2)
plot(m8, 3)
plot(m8$residuals, type = "o")
##----m9-----------------------------

m9 <- lm(y9 ~ x9, data = assumptions)

plot(assumptions$x9, assumptions$y9, xlab="x", ylab = "y")
abline(coef = m9$coefficients, col=2)

summ9<-summary(m9)
summ9$r.squared
sum(summ9$residuals^2)
plot(summ9$residuals)
mean(summ9$residuals)
var(summ9$residuals)
hist(summ9$residuals)

var(assumptions$x9)
var(assumptions$y9)
hist(assumptions$x9)
hist(assumptions$y9)

plot(assumptions$x9, assumptions$y4, xlab="x", ylab = "y", xlim = c(-10, 10), ylim = c(-11, 11))

plot(m9, 1)
plot(m9, 2)
plot(m9, 3)
plot(m9$residuals, type = "o")
#=============================================


# Проаналізуємо множинну лінійну модель, 
#яку ми розглядали для набору вин, і розгянемо, 
#як зробити висновок про параметри моделі. 

#--------------------------------------------------------
par(mfrow = c(1, 1))
# Модель за всіма параметрами;
# Зчитування даних
wine <- read.table(file = "wine.csv", header = TRUE, sep = ",")

wine$Year <- NULL

mod_AGST <- lm(Price ~ AGST, data = wine)
summary(mod_AGST)
summod_AGST<-summary(mod_AGST)

plot(wine$AGST, wine$Price, xlab="AGST", ylab = "Price")
abline(mod_AGST, col=2)
summod_AGST$r.squared
sum(summod_AGST$residuals^2)
plot(summod_AGST$residuals)
mean(summod_AGST$residuals)
var(summod_AGST$residuals)
hist(summod_AGST$residuals)

var(wine$AGST)
var(wine$Price)

hist(wine$AGST)
hist(wine$Price)

plot(wine$AGST, wine$Price, xlab="x", ylab = "y", xlim = c(15, 25), ylim = c(5, 10))

# --------- Лінійність ------
plot(mod_AGST, 1)
# --------- Нормальність ----
plot(mod_AGST, 2)
#------Гомоскедастичність----
plot(mod_AGST, 3)
#------ Незалежність---------
plot(mod_AGST$residuals, type = "o")

#====================================================


mod <- lm(Price ~ Age + AGST + HarvestRain + WinterRain, data = wine)
summary(mod)

#===========припущення про лінійність==============

plot(mod, 1)# !!! перевірка припущення про лінійність
par(mfrow = c(2, 2)) # We have 4 predictors
termplot(mod, partial.resid = TRUE)

#---------------------------------
x <- c(-2, -1.9, -1.7, -1.6, -1.4, -1.3, -1.1, -1, -0.9, -0.7, -0.6,
       -0.4, -0.3, -0.1, 0, 0.1, 0.3, 0.4, 0.6, 0.7, 0.9, 1, 1.1, 1.3,
       1.4, 1.6, 1.7, 1.9, 2, 2.1, 2.3, 2.4, 2.6, 2.7, 2.9, 3, 3.1,
       3.3, 3.4, 3.6, 3.7, 3.9, 4, 4.1, 4.3, 4.4, 4.6, 4.7, 4.9, 5)
y <- c(1.4, 0.4, 2.4, 1.7, 2.4, 0, 0.3, -1, 1.3, 0.2, -0.7, 1.2, -0.1,
       -1.2, -0.1, 1, -1.1, -0.9, 0.1, 0.8, 0, 1.7, 0.3, 0.8, 1.2, 1.1,
       2.5, 1.5, 2, 3.8, 2.4, 2.9, 2.7, 4.2, 5.8, 4.7, 5.3, 4.9, 5.1,
       6.3, 8.6, 8.1, 7.1, 7.9, 8.4, 9.2, 12, 10.5, 8.7, 13.5)

# Data frame (a matrix with column names)
nonLinear <- data.frame(x = x, y = y)

par(mfrow = c(1, 2))
plot(x, y)
mod <- lm(y ~ x)
abline(mod, col = 2)

mod_nonLinear <- lm(y ~ I(x^2))
mod_nonLinear

#Call:
#  lm(formula = y ~ I(x^2))

#Coefficients:
#  (Intercept)       I(x^2)  
#0.05891      0.48659 
plot(x, y)
curve(0.05891 + 0.48659 * x^2, add = TRUE, col = 2)

par(mfrow = c(1, 2))
plot(lm(y ~ x, data = nonLinear), 1) # Nonlinear
plot(lm(y ~ I(x^2), data = nonLinear), 1) # Linear

#=========== нормальність ==============
mod <- lm(Price ~ Age + AGST + HarvestRain + WinterRain, data = wine)
summary(mod)

par(mfrow = c(1, 1))
plot(mod, 2)# !!! перевірка припущення про нормальність

#=========== Гомоскедастичність ==============

plot(mod, 3)# !!! перевірка припущення про Гомоскедастичність

#=========== Незалежність ==============

plot(mod$residuals, type = "o")# !!! перевірка припущення про незалежність

#=====================================

par(mfrow = c(1, 1))

modWine <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(modWine)
m_0 <- summary(modWine)


# R^2
m_0$r.squared
## [1] 0.6849893

# Fitted values
modWine$fitted.values

# Residuals
modWine$residuals
sum(modWine$residuals)^2

# Compute the minimized RSS
sum((wine$Price - modWine$coefficients[1] - modWine$coefficients[2] * wine$AGST - modWine$coefficients[3] * wine$HarvestRain)^2)
## [1] 3.298261
sum(modWine$residuals^2)
## [1] 3.298261

# sigma
sqrt(sum(modWine$residuals^2)/modWine$df.residual)
## [1] 0.3707122
m_0$sigma
## [1] 0.3707122


modWine1 <- lm(Price ~ ., data = wine)

# Summary
sumModWine1 <- summary(modWine1)
sumModWine1
## 
## Call:
## lm(formula = Price ~ ., data = wine)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.46541 -0.24133  0.00413  0.18974  0.52495 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.343e+00  7.697e+00  -0.304  0.76384    
## WinterRain   1.153e-03  4.991e-04   2.311  0.03109 *  
## AGST         6.144e-01  9.799e-02   6.270 3.22e-06 ***
## HarvestRain -3.837e-03  8.366e-04  -4.587  0.00016 ***
## Age          1.377e-02  5.821e-02   0.237  0.81531    
## FrancePop   -2.213e-05  1.268e-04  -0.175  0.86313    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.293 on 21 degrees of freedom
## Multiple R-squared:  0.8278, Adjusted R-squared:  0.7868 
## F-statistic: 20.19 on 5 and 21 DF,  p-value: 2.232e-07

# Contains the estimation of sigma ("Residual standard error")
sumModWine1$sigma
## [1] 0.2930287

# Access coefficients with $coefficients
modWine1$coefficients
## (Intercept)           x 
##  -0.6153744   1.3950973




#Висока частка предикторів не є значущою в modWine1: 
#FrancePop та Age не значущі. Це свідчить про надлишок 
#предикаторів, що додає мало інформації. Одне з пояснень
#- майже ідеальна кореляція між FrancePop та Age, 
#яке показано раніше: одне з них не додає жодної 
#додаткової інформації для пояснення ціни. Це надмірно 
#ускладнює модель і, що ще важливіше, має небажаний ефект, 
#що робить оцінки коефіцієнтів менш точними. 
#Ми вирішили видалити з моделі предиктор FrancePop, 
#оскільки він екзогенний для винного контексту. 
#Обґрунтуванням видалення цієї змінної на основі даних є 
#те, що вона є найменш значущою у modWine1.

#--------------------------------------------------
# Модель без параметра FrancePop

modWine2 <- lm(Price ~ . - FrancePop, data = wine)
summary(modWine2)
## 
## Call:
## lm(formula = Price ~ . - FrancePop, data = wine)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.46024 -0.23862  0.01347  0.18601  0.53443 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.6515703  1.6880876  -2.163  0.04167 *  
## WinterRain   0.0011667  0.0004820   2.420  0.02421 *  
## AGST         0.6163916  0.0951747   6.476 1.63e-06 ***
## HarvestRain -0.0038606  0.0008075  -4.781 8.97e-05 ***
## Age          0.0238480  0.0071667   3.328  0.00305 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2865 on 22 degrees of freedom
## Multiple R-squared:  0.8275, Adjusted R-squared:  0.7962 #---!!!---більший Adjusted R-squared:  0.7962 
## F-statistic: 26.39 on 4 and 22 DF,  p-value: 4.057e-08



#--------------------------------------------------
# Усі коефіцієнти значущі на рівні ?? = 0,05. 
# Тому чіткої надлишкової інформації немає. 
# Крім того, R2 дуже схожий на повну модель, але 
# "Налаштований R-квадрат", зважений R2 для 
# врахування кількості предикторів, які використовує модель,
# трохи більший. Це означає, що порівняно з кількістю 
# використовуваних предикторів, modWine2 пояснює більшу 
# мінливість ціни, ніж modWine1.
# Зручним способом порівняння коефіцієнтів обох моделей є car :: compareCoefs:

##Зверніть увагу, як коефіцієнти для modWine2 мають менші 
#похибки, ніж modWine1.


car::compareCoefs(modWine1, modWine2)

## Calls:
## 1: lm(formula = Price ~ ., data = wine)
## 2: lm(formula = Price ~ . - FrancePop, data = wine)
## 
##               Model 1   Model 2
## (Intercept)     -2.34     -3.65
## SE               7.70      1.69
##                                
## WinterRain   0.001153  0.001167
## SE           0.000499  0.000482
##                                
## AGST           0.6144    0.6164
## SE             0.0980    0.0952
##                                
## HarvestRain -0.003837 -0.003861
## SE           0.000837  0.000808
##                                
## Age           0.01377   0.02385
## SE            0.05821   0.00717
##                                
## FrancePop   -2.21e-05          
## SE           1.27e-04          
## 