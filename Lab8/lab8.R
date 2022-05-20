DF <- read.csv("universities_cor.csv")
DF$gender.ratio <- as.factor(DF$gender.ratio)

y <- DF$overall.score
x1 <- DF$teaching.score
x2 <- DF$citations.score
x3 <- DF$industry.income.score
x4 <- DF$intl.outlook.score


data <- subset(DF, select = -c(X, ranking, title, location, gender.ratio))

#------------------------------(1)------------------------------

#--------------------(A)--------------------
round(cor(data), 2)
#--------------------(B)--------------------
corrplot::corrplot(cor(data), addCoef.col = "grey")
#--------------------(C)--------------------
modMulti <- lm(y ~ x1 + x2 + x3 + x4, data=data)

car::vif(modMulti)
#x2 has the highest value
# In next model we remove it

modClean <- lm(y ~ x1 + x3 + x4, data=data)

car::vif(modClean)
#--------------------(D)--------------------
car::compareCoefs(modMulti, modClean)

confint(modMulti)
confint(modClean)

summary(modMulti)
summary(modClean)

#--------------------(E)--------------------
# R-squared in modMulti is higher then in modClean, but
# F-statistic in modMUlti is much higher then in modClean.
# Standart error in modClean is higher in average than in modMulti

#------------------------------(2)------------------------------

#--------------------(A)--------------------
mod <- lm(y ~ x2, data=data)
plot(data$y, data$x1)

#--------------------(B)--------------------
car::ncvTest(mod)
# p = < 2.22e-16 -> heteroscedasticity

#--------------------(C)--------------------
plot(mod, 3) # -> heteroscedasticity

#--------------------(D)--------------------
mod1 <- lm(I(log(y)) ~ x2, data=data)
car::ncvTest(mod1)
#p = 5.0437e-07 -> heteroscedasticity

mod2 <- lm(I(sqrt(y)) ~ x1, data=data)
car::ncvTest(mod2)
#p = 0.15044 -> homoscedasticity

#--------------------(E)--------------------
delta <- 1 
m <- -min(data$y) + delta
plot(lm(I(log(data$y + m)) ~ data$x2), 3) 
mod3 <- lm(I(log(data$y + m)) ~ data$x2)
car::ncvTest(mod3)
#p = 5.9887e-14 -> heteroscedasticity

#--------------------(F)--------------------
YJ <- car::powerTransform(lm(data$y ~ data$x2), family = "yjPower")
(lambdaYJ <- YJ$lambda)
YTransf <- car::yjPower(U = data$y, lambda = lambdaYJ)
plot(lm(YTransf ~ data$x2), 3) 
mod4 <- lm(I(YTransf ~ data$x2))
car::ncvTest(mod4)
#p = 0.023793 -> heteroscedasticity

#------------------------------(3)------------------------------

#--------------------(A)--------------------
#--------------------(B)--------------------
pca_data <- princomp(data, fix_sign = TRUE)
summary(pca_data)
#--------------------(C)--------------------
plot(pca_data, type = "l")
# On this plot each comp is unique

#--------------------(D)--------------------
barplot(cumsum(pca_data$sdev^2) / sum(pca_data$sdev^2))
# Comp1, Comp2, Comp3 and Comp4 is unique. Comp5 is the same as Comp4

#--------------------(E)--------------------
head(sweep(pca_df$scores %*% t(pca_df$loadings), 2, pca_df$center, "+"))
#--------------------(F)--------------------
pca_data_std <- princomp(x = data, cor = TRUE, fix_sign = TRUE)
summary(pca_data_std)
#--------------------(G)--------------------
biplot(pca_data, cex = 0.75)
biplot(pca_data_std, cex = 0.75)
#--------------------(H)--------------------
data_x <- subset(data, select = -c(y))
pca_data_x <- princomp(x = data_x, cor = TRUE, fix_sign = TRUE)
datapca <- data.frame(y=data$y, pca_data_x$scores)
modpca <- lm(y~Comp.1+Comp.2, data=datapca)
summary(modpca)
