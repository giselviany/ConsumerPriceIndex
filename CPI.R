library(lmtest)
library(nortest)
library(car)
library(dplyr)
library(performance)

# Library untuk import file .xlsx
library(readxl)

# Untuk load dataset
gdp = read_excel("gisel/BINUS/Semester 5/Regression Analysis/AOL/Dataset Clean.xlsx")
summary(gdp$Inflation_Consumer_Prices_Annual)
summary(gdp$Households_Consumption_Expenditure_Annual_Growth)
summary(gdp$Age_Dependency_Ratio)
summary(gdp$Inflation_GDP)

# Untuk membuat model prediksi berdasarkan variabel independen yang diduga berpengaruh
model1 = lm(Inflation_Consumer_Prices_Annual ~ Households_Consumption_Expenditure_Annual_Growth +
              Age_Dependency_Ratio +
              Inflation_GDP, data = gdp)
summary(model1)

error1 = resid(model1)

# Untuk melakukan uji kolmogorov smirnov
lillie.test(error1)

# Untuk melakukan uji breusch pagan
bptest(model1)

# Untuk melakukan uji durbin watson
dwtest(model1)

# Untuk melakukan uji multikolinearitas
vif(model1)

# Untuk mendefinisikan weight yang digunakan pada WLS yang digunakan untuk menangani
# ketidaksetaraan varians residual atau memberikan penekanan lebih pada beberapa titik data tertentu
weight1 = 1 / lm(abs(model1$residuals) ~ model1$fitted.values)$fitted.values^2

# Untuk memuat model prediksi berdasarkan variabel independen yang diduga berpengaruh menggunakan metode WLS
model_wls1 <- lm(Inflation_Consumer_Prices_Annual ~ Households_Consumption_Expenditure_Annual_Growth +
                   Age_Dependency_Ratio + Inflation_GDP, data = gdp, weights = weight1)
summary(model_wls1)

# Untuk melakukan uji breusch pagan
bptest(model_wls1)

# AIC
AIC(model1)
AIC(model_wls1)

# AICc
library(gamlr)
AICc(model1)
AICc(model_wls1)

# BIC
BIC(model1)
BIC(model_wls1)

# RSE
summary(model1)$sigma
summary(model_wls1)$sigma
