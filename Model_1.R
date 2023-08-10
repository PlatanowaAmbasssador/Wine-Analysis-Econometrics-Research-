# install.packages("haven")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("fBasics")
# install.packages("lmtest")
# install.packages("olsrr")
# install.packages("MASS")
# install.packages("fastDummies")


library(haven)
library(data.table)
library(dplyr)
library(car)
library(fBasics)
library(lmtest)
library(olsrr)
library(MASS)
library(fastDummies)

#import data
wines <- read.csv("/Users/kamilkashif/Documents/Python/Econometrics/Research Paper/wines_SPA.csv")

# omiiting Nans
df_wines <- na.omit(wines)

# creating a column with todays year
today <- 2022
df_wines$today <- today

# calculating the age of the wines
df_wines$age <- (df_wines$today - strtoi(df_wines$year))

# removing unnecessary columns and dropping nans, if any
df_wines <- df_wines[, !names(df_wines) %in% c("year", "today", "country")]
df_wines <- na.omit(df_wines)

# Running OLS
regression_1 <- lm(price ~ factor(winery) + factor(wine) + rating + num_reviews + factor(region) + factor(type) + body + acidity + age, data = df_wines)
summary(regression_1)
                               
                               
                               
# DIAGNOSTIC TESTS
                               
# 1. Linearity test RESET
reset_test <- resettest(regression_1, power = 2:3, type = c("regressor"))
reset_test
                               
                               
# 2. Normality of residuals
                               
# Histogram + normal curve
# h <- hist(regression2$residuals, col = "blue", xlab = "Residuals", freq=F)
# xfit <- seq(min(regression2$residuals), max(regression2$residuals), length=40)
# yfit <- dnorm(xfit, mean=mean(regression2$residuals), sd=sd(regression2$residuals))
# lines(xfit, yfit, col="red", lwd=2)
                               
# q-q plot for standardized residuals
# plot(regression2, which=2)
# res.std <- rstandard(reg1)
# boxplot(res.std)
                               
# Let's look both on qqplot and boxplot:
# par(mfrow=c(1,2))
# plot(reg1, which=2)
# boxplot(res.std)
# par(mfrow=c(1,1))
                               
# Basic features
# summary(reg1$residuals)
                               
# Jarque-Berra test
jarquebera_test <- jarqueberaTest(regression_1$residuals)
jarquebera_test
                               
                               
# 3. Homoscedasticity
                               
# Residuals vs fitted - we should have randomly located points
# plot(reg1, which=1)
# or also which = 3
#  plot(reg1, which=3)
                               
# Breusch-Pagan test -> H0: homoscedastic residuals
braushpagan_test <- bptest(regression_1, studentize=FALSE)
braushpagan_test

# 5. Autocorrelation
# Durbin-Watson test
durbinwatson_test <- dwtest(regression_1)
durbinwatson_test

# Data Frame with all the tests
jarquebera_test_statistic <- 28787693.2953
jarquebera_test_p_value <- 2.2e-16
df_diagnostic_tests <- data.frame (
  TEST_NAME = c("RESET_TEST","JARQUE_BERA_TEST","PAGAN_TEST","WATSON_TEST"),
  TEST_STATISTIC = c(reset_test$statistic,jarquebera_test_statistic,braushpagan_test$statistic,durbinwatson_test$statistic),
  TEST_P_VALUE  = c(reset_test$p.value,jarquebera_test_p_value,braushpagan_test$p.value,durbinwatson_test$p.value)
)
df_diagnostic_tests

