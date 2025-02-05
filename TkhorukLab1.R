# ������ 39   ������ ����
x <- c(43.1, 45.2, 45.5, 47.8, 49.5, 50.3, 53.3, 53.7, 53.7, 55.5, 57.4)
y <- c(68.7, 73.3, 77.6, 80.1, 88.1, 91.0, 100.4, 102.2, 101.1, 103.9, 112.1)

# (A)
lm(y ~ x)
# b0 = -63.23, b1 = 3.05
plot(x, y)
curve( -63.230 + 3.052 * x, add = TRUE, col = 3)

# (B)
# �������
n <- length(x)

#�������
meanX <- mean(x)
meanY <- mean(y)
meanX
meanY

# ������� �
varX <- 0
for(i in 1:n){
  varX = varX + (x[i] - meanX)^2
}
varX = varX / n
varX

# ������� Y
varY <- 0
for(i in 1:n){
  varY = varY + (y[i] - meanY)^2
}
varY = varY / n
varY
  
# ��������� 
covXY <- 0
for(i in 1:n){
  covXY = covXY + (x[i] - meanX)*(y[i] - meanY)
}
covXY = covXY / n
covXY

# ����������
b1 <- covXY / varX
b1
b0 <- meanY - b1*meanX
b0

# ������
plot(x, y)
curve(b0 + b1 * x, add = TRUE, col = 2)

# (C)
# �� ��������� �������
varX <- var(x)*(n-1)/n 
varY <- var(y)*(n-1)/n
covXY <- cov(x, y)*(n-1)/n

b1 <- covXY / varX
b1
b0 <- meanY - b1*meanX
b0

# ������
plot(x, y)
curve(b0 + b1 * x, add = TRUE, col = 2)

# (D)(E)
# b0 + b1 * x^2
lm(y ~ I(x^2))
plot(x, y)
curve(12.84495 + 0.03038 * x^2, add = TRUE, col = 2)

# b0 + b1 * x + b2 * x^2
lm(y ~ x + I(x^2))
plot(x, y)
curve(-73.168532 + 3.451753 * x + -0.003982 * x^2, add = TRUE, col = 2)

# b0 + b1 * log(x)
lm(y ~ log(x))
plot(x, y)
curve(-505.3 + 152.2 * log(x), add = TRUE, col = 2)

# (F)
# ��������: 
# b0 = -63.23, b1 = 3.05
# y = -63.23 + 3.05 * x
# ���� x ���������� �� 1, �� y ���������� �� 3.05
# ������� ��������� ��� ���� ����� ���� ������ �������

