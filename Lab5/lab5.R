#=============================================

# Ця функція обчислює спрощену anova для лінійної моделі
simpleAnova <- function(object, ...) {
  
  # Обчислити таблицю anova
  tab <- anova(object, ...)
  
  # Отримати кількість предикторів
  p <- nrow(tab) - 1
  
  # Додайте рядок предикторів
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
  
  # F-значення
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)
  
  # Спрощена таблиця
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)
  
}
#==============================

DF <- read.csv("universities.csv")

Y <- DF$teaching.score
X1 <- DF$research.score
X2 <- DF$citations.score
X3 <- DF$industry.income.score
X4 <- DF$intl.outlook.score
X5 <- DF$gender.ratio

#(A)

mod <- lm(Y ~ X1+X2+X3+X4+X5)
simpleAnova(mod)

#a. SSR = 52631
#b. SSE = 230378
#c. F = 1287

#(B)

anova(mod)

#a. 
#X1 SSE = 225897
#X2 SSE = 77
#X3 SSE = 424
#X4 SSE = 3963
#X5 SSE = 18

#b.
#X1 SSE = 6524
#X2 SSE = 2
#X3 SSE = 12
#X4 SSE = 114
#X5 SSE = 0.5

#c.
#X2 and X5 not fit F-statistic

#(C)

n <- nrow(DF)
DF$title <- NULL
DF$location <- NULL
modAll <- lm(formula = DF$teaching.score ~ ., data = DF)
modZero <- lm(formula = DF$teaching.score ~ 1, data = DF)

#a.

MASS::stepAIC(modAll, direction = "backward", k = log(n))
# Coefficients:
#   (Intercept)                ranking          overall.score         research.score        citations.score  
# 3.853276               0.004516               1.216689               0.136092              -0.300980  
# industry.income.score     intl.outlook.score   students.staff.ratio     perc.intl.students  
# -0.033763              -0.166111              -0.074521               0.072354 

#b.

MASS::stepAIC(modZero, direction = "forward",
              scope = list(lower = modZero, upper = modAll), k = log(n))
# Coefficients:
#   (Intercept)         research.score   students.staff.ratio     intl.outlook.score          overall.score  
# 3.853276               0.136092              -0.074521              -0.166111               1.216689  
# citations.score                ranking     perc.intl.students  industry.income.score  
# -0.300980               0.004516               0.072354              -0.033763 

#c.

MASS::stepAIC(modAll, direction = "both", trace = 0, 
              scope = list(lower = modZero, upper = modAll), k = log(n))
# Coefficients:
#   (Intercept)                ranking          overall.score         research.score        citations.score  
# 3.853276               0.004516               1.216689               0.136092              -0.300980  
# industry.income.score     intl.outlook.score   students.staff.ratio     perc.intl.students  
# -0.033763              -0.166111              -0.074521               0.072354  

#d.

MASS::stepAIC(modAll, trace = 0, k = 2)
# Coefficients:
#   (Intercept)                ranking          overall.score         research.score        citations.score  
# 3.827e+00              4.499e-03              1.212e+00              1.362e-01             -2.997e-01  
# industry.income.score     intl.outlook.score        number.students   students.staff.ratio     perc.intl.students  
# -3.285e-02             -1.673e-01              1.215e-05             -8.607e-02              7.832e-02  

#e.

MASS::stepAIC(modAll, trace = 0, k = log(n))
# Coefficients:
#   (Intercept)                ranking          overall.score         research.score        citations.score  
# 3.853276               0.004516               1.216689               0.136092              -0.300980  
# industry.income.score     intl.outlook.score   students.staff.ratio     perc.intl.students  
# -0.033763              -0.166111              -0.074521               0.072354 
