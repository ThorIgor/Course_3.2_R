library(lubridate)
library(data.table)
library(dplyr)
library(utils)

UR <- read.csv("universities_ranking.csv")
US <- read.csv("universities_scores.csv")

DF <- cbind(US, UR[c("number.students", "students.staff.ratio", "perc.intl.students", "gender.ratio")])

#������ overall.score
count <- 1
for(i in DF$overall.score) {
  DF$overall.score[count] <- as.character(mean(as.numeric(unlist(strsplit(i, "–")))))
  count <- count + 1
}
DF$overall.score <- as.numeric(DF$overall.score)


#������ number.students
DF <- DF %>%
  mutate(number.students = gsub(",", "", number.students))
DF$number.students <- as.numeric(DF$number.students)


#������ perc.intl.students
DF <- DF %>%
  mutate(perc.intl.students = gsub("%", "", perc.intl.students))
DF$perc.intl.students <- as.numeric(DF$perc.intl.students)
DF <- DF %>%
  mutate(perc.intl.students=replace(perc.intl.students,is.na(perc.intl.students),median(perc.intl.students,na.rm = TRUE)))

#������ gender.ratio
count <- 1
for(i in DF$gender.ratio) {
  if(!is.na(i)) {
    num <- as.numeric(unlist(strsplit(i, ":")))
    DF$gender.ratio[count] <- as.character(num[1]/num[2])
  }
  count <- count + 1
}
DF$gender.ratio <- as.numeric(DF$gender.ratio)
DF <- DF %>%
  mutate(gender.ratio=replace(gender.ratio,is.na(gender.ratio),median(gender.ratio,na.rm = TRUE)))
count <- 1
for(i in DF$gender.ratio) {
  if(i == Inf)
    DF$gender.ratio[count] <- 100
  count <- count + 1
}

#A
Y <- DF$perc.intl.students
X1 <- DF$teaching.score
X2 <- DF$research.score
X3 <- DF$intl.outlook.score
X4 <- DF$citations.score
X5 <- DF$students.staff.ratio


X <- cbind(1, X1, X2, X3, X4, X5)


beta <- solve(t(X) %*% X) %*% t(X) %*% Y
beta

mod <- lm(Y ~ X1 + X2 + X3 + X4 + X5)
mod$coefficients
summary(mod)

#B
carDF <- subset(DF, select = c(teaching.score, research.score, citations.score, intl.outlook.score, students.staff.ratio, perc.intl.students))

car::scatterplotMatrix(carDF, col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))

#C
rsq <- c(summary(lm(Y ~ X1))$r.squared, summary(lm(Y ~ X2))$r.squared, summary(lm(Y ~ X3))$r.squared, summary(lm(Y ~ X4))$r.squared, summary(lm(Y ~ X5))$r.squared)
rsq
max(rsq)
#X3 �������� R^2

#D
Y_ <- mod$fitted.values
e <- mod$residuals
summary(Y_)
summary(e)

#E ��������:
#� ������ ���� ����� ������ ������������ �� ��'����� �� � ����� ����� ���������� ����� ������������
#�������� � ���� ���� �� ��������� �� � ������ ����
#���� � ���� �������� �������� ������� ��������� �������� �����������  ����� ����������, ����������, ��������� �����������, ��������� �� ��������� ������� �������� �� ���������� ������������ 
#���� ������� ������� �� ����� ���������� beta � ����� ������� �� �� ��������� ������� lm(...)
#��� ������� �� � ���� �� ���� ��������� ���� ������� ���� � ������� �������������� ������
#������� ����� � ������ �� ��������� �������� �� ���������� �� ������ ���������� ��������� � �� ����� ��������
#��� � ����� ������� ������� ��� ����� ������������ �����
#���� � ����� R^2 ��� ����� � ��� ������� � �������� �� �������� �������� � ��������� �����������
#����� ����� ������ �������� �� ���� ����� ��������� ����������� �������� ������� ������� ��������� ��������
#��� � ������� ���������� �������� �� ������ �� ����������� ���������� �� ���������������

