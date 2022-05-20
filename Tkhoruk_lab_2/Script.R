# Numeric
x <- 27.5
# Integer
y <- 12L
# Complex
z <- 2 + 3i

typeof(x)
# "double"
typeof(y)
# "integer"
typeof(z)
# "complex"

x + y
# 39.5
x * z
# 23+36i
abs(x)
# 27.5
exp(x)
# 877199251319

#--------------------------

gender <- factor("male", levels = c("male", "female"))
flower_type <- factor("setosa", levels = c( "virginica", "setosa", "versicolor"))
print(gender)
# male
# Levels: male female
print(flower_type)
# setosa
# Levels: virginica setosa versicolor


gender_2 <- factor(c("male", "female", "female", "female", "male"), levels = c("male", "female"))
print(gender_2)
# male female female female male
# Levels: male female

table(gender_2)
# gender_2
# male   female
#    2        3   

nlevels(gender_2)
# 2
levels(gender_2)
# "male" "female"

#---------------------

character <- 'string'
print(character)
# "string"
text <- "This is also a string"
print(text)
# "This is also a string"

nchar(text)
# 21
# paste(character, text)
# "string This is also a string"

#------------------------------

library(lubridate)
sample_date <- ymd("2022-02-21")
print(sample_date)
# "2022-02-21"
sample_datetime <- mdy_hms("1/23/2022 2:53:22")
print(sample_datetime)
# "2022-01-23 02:53:22"

# Extract datetime components ----------------
month(sample_date)
# 2
second(sample_datetime)
# 22
# Add a period of time -----------------------
sample_date + years(50)
# "2072-02-21"
# Round a datetime down by month -------------
floor_date(sample_date, unit = "month")
# "2022-02-01"


#==========data cleaning=====================

library(data.table)
DT = data.table(x=c(1,NaN,NA,3,-1), y=c(NA_integer_, 1:3,2), z=c("a", NA_character_, "b", "c", "a"))
# default behaviour
na.omit(DT)
# omit rows where 'x' has a missing value
na.omit(DT, cols="x")
Dt_y <- na.omit(DT, cols="y")
# omit rows where either 'x' or 'y' have missing values
na.omit(DT, cols=c("x", "y"))

#---------------------------

data <- read.csv("TempByCity.csv")
head(data,5)

## dimension of the data
dim(data)
## structure of the data
str(data)
## summary of the data
summary(data)

sum(is.na(data))
# [1] 240

sum(is.na(data$AverageTemperature))
#[1] 120
## checking the mean and median values of Avg. Temperature

library(dplyr)

data %>%
  summarize(mean=mean(AverageTemperature,na.rm=TRUE),median=median(AverageTemperature,na.rm=TRUE))
#mean median
#1 16.84046 18.937

#----------------

data <- data %>%
  mutate(AverageTemperature=replace(AverageTemperature,is.na(AverageTemperature),median(AverageTemperature,na.rm = TRUE)))

## check again if any missing value is left
sum(is.na(data$AverageTemperature))
# [1] 0

#-----------------

# checking for na values
sum(is.na(data$AverageTemperatureUncertainty))
#[1] 120

data <- data %>%  
  mutate(AverageTemperatureUncertainty=replace(AverageTemperatureUncertainty,is.na(AverageTemperatureUncertainty),median(AverageTemperatureUncertainty,na.rm = TRUE)))

sum(is.na(data$AverageTemperatureUncertainty))
#[1] 0 

#------------------

library(tidyverse)
## checking for na values
sum(is.na(data$dt))
#[1] 0
data <- data %>%
  separate(dt,c("year","month","day"))
## changing them to integer
data$year <- as.integer(data$year)
data$month <- as.integer(data$month)
data$day <- as.integer(data$day)
head(data,3)

#------------------


## checking for NA values
sum(is.na(data$Latitude))
## filtering the rows with N values
dataN<- data %>%
  dplyr::filter(endsWith(Latitude,'N'))
head(dataN,5)

## removing n form dataN dataframe
dataN <- dataN %>%
  mutate(Latitude=gsub("N","",Latitude))
head(dataN$Latitude,5)
#[1] "10.45" "23.31" "29.74" "50.63" "40.99"

# changing the datatype of Latitude to numeric
dataN$Latitude <- as.numeric(dataN$Latitude)
sum(is.na(dataN$Latitude))
#[1] 0

## for S values
## filtering the rows with S values
dataS<- data %>%
  dplyr::filter(endsWith(Latitude,'S'))
## removing n form dataN dataframe
dataS <- dataS %>%
  mutate(Latitude=gsub("S","",Latitude))
head(dataS$Latitude,5)
#[1] "37.78" "7.23"  "20.09" "2.41"  "7.23"

dataS$Latitude <- as.numeric(dataS$Latitude)
sum(is.na(dataS$Latitude))
#[1] 0


#====================================

#---------------------------------------------------------
#Кейс: Рівняння Бордо

# Зчитування даних
wine <- read.table(file = "wine.csv", header = TRUE, sep = ",")

names(wine) #Як підписані змінні

#---------------------------------------------------------
# Числові - граничні розподіли
summary(wine)
##       Year          Price         WinterRain         AGST        HarvestRain         Age          FrancePop    
##  Min.   :1952   Min.   :6.205   Min.   :376.0   Min.   :14.98   Min.   : 38.0   Min.   : 3.00   Min.   :43184  
##  1st Qu.:1960   1st Qu.:6.508   1st Qu.:543.5   1st Qu.:16.15   1st Qu.: 88.0   1st Qu.: 9.50   1st Qu.:46856  
##  Median :1967   Median :6.984   Median :600.0   Median :16.42   Median :123.0   Median :16.00   Median :50650  
##  Mean   :1967   Mean   :7.042   Mean   :608.4   Mean   :16.48   Mean   :144.8   Mean   :16.19   Mean   :50085  
##  3rd Qu.:1974   3rd Qu.:7.441   3rd Qu.:705.5   3rd Qu.:17.01   3rd Qu.:185.5   3rd Qu.:22.50   3rd Qu.:53511  
##  Max.   :1980   Max.   :8.494   Max.   :830.0   Max.   :17.65   Max.   :292.0   Max.   :31.00   Max.   :55110

library(car)

# Графіки - попарні відношення з лінійною та "плавною" регресіями
car::scatterplotMatrix(wine, col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))
#-----------------------------------------------------------
#Видалиння предиктор Year і використання його для встановлення назв регістрів
row.names(wine) <- wine$Year
wine$Year <- NULL

#----------------------------------------------------------
# Price ~ AGST
modAGST <- lm(Price ~ AGST, data = wine)

# Короткий звіт моделі
summary(modAGST)
## 
## Call:
## lm(formula = Price ~ AGST, data = wine)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.78370 -0.23827 -0.03421  0.29973  0.90198 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -3.5469     2.3641  -1.500 0.146052    
## AGST          0.6426     0.1434   4.483 0.000143 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4819 on 25 degrees of freedom
## Multiple R-squared:  0.4456, Adjusted R-squared:  0.4234 
## F-statistic: 20.09 on 1 and 25 DF,  p-value: 0.0001425

# summary також є об’єктом
sumModAGST <- summary(modAGST)
names(sumModAGST)
##  [1] "call"          "terms"         "residuals"     "coefficients"  "aliased"       "sigma"         "df"           
##  [8] "r.squared"     "adj.r.squared" "fstatistic"    "cov.unscaled"

# R^2
sumModAGST$r.squared
## [1] 0.4455894

##_________________________________________________________
##leastSquares3D-------------------------------------------
load(file = "least-Squares-3D.RData")

#----------------------------------------------------------
# Вихід з lm
mod <- lm(yLin ~ x1 + x2, data = leastSquares3D)
mod$coefficients
## (Intercept)          x1          x2 
##  -0.5702694   0.4832624   0.3214894

# Матриця X
X <- cbind(1, X1, x2, x3, x4, x5)

# Вектор Y
Y <- leastSquares3D$yLin

# Коефіцієнти
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
# %*% множення матриць
# solve() обчислює обернену до матриці
# t() транспонує матрицю
beta
##            [,1]
## [1,] -0.5702694
## [2,]  0.4832624
## [3,]  0.3214894

# Оцінені значення Y за допомогою математичної моделі
mod$fitted.values

# Залишки
mod$residuals

#-------------------------------------------------
#------------------------------------------------
# Регресія щодо всіх предикторів
modWine1 <- lm(Price ~ AGST + FrancePop + HarvestRain + WinterRain + Age,
               data = wine)
modWine1

# Коротка форма
modWine1 <- lm(Price ~ . - Year, data = wine)
modWine1
## 
## Call:
## lm(formula = Price ~ ., data = wine)
## 
## Coefficients:
## (Intercept)   WinterRain         AGST  HarvestRain          Age    FrancePop  
##  -2.343e+00    1.153e-03    6.144e-01   -3.837e-03    1.377e-02   -2.213e-05

# Summary
summary(modWine1)
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

#-------------------------------------
wine <- read.table(file = "wine.csv", header = TRUE, sep = ",")

names(wine) #Як підписані змінні
attach(wine) #Переназвати змінні
names(wine)[1] <- "№"
names(wine)[2] <- "y"
names(wine)[4] <- "x"

names(wine) #Нова назва
#-------------------------------------

names(wine)
Y <- wine$y
X1 <- wine$x
x2 <- wine$WinterRain
x3 <- wine$HarvestRain
x4 <- wine$Age
x5 <- wine$FrancePop
n <- length(wine$x)
n

# Лінійна регресія 
mod <- lm(Y ~ X1+x2+x3+x4+x5)

# Коефіцієнти a, b з лінійної регресії
mod

summary(wine)

# Графік
plot(X, Y)
abline(mod)


# Середні значення
meanX <- mean(X)
meanY <- mean(Y)
meanX
meanY

# За допомогою формул

# Варіація Х
varX <- 0
for(i in 1:n){
  varX = varX + (X[i] - meanX)^2
}
varX = varX / n
varX

# Варіація У
varY <- 0
for(i in 1:n){
  varY = varY + (Y[i] - meanY)^2
}
varY = varY / n
varY
# Коваріація
covXY <- 0
for(i in 1:n){
  covXY = covXY + (X[i] - meanX)*(Y[i] - meanY)
}
covXY = covXY / n
covXY
# Параметр b
b <- covXY / varX
b
# Параметр a
a <- meanY - b*meanX
a

# За допомогою функцій
var(X)*(n-1)/n # Це еквівалент в Excel VAR.P(x) або ДИСП.Г
var(Y)*(n-1)/n
cov(X, Y)*(n-1)/n
var(X) #var(x)  не для генеральної сукупності. Це еквівалент в Excel VAR.S(x)

# Графік
plot(X, Y)
abline(mod)

mod_2 <- lm(Y ~ X, data = wine)
mod_2
summary(mod_2)
plot(wine$x, wine$y, xlab="x", ylab = "y")
abline(coef = mod_2$coefficients, col=2)



