library(haven)
library(data.table)
library(dplyr)
library(car)
library(fBasics)
library(lmtest)
library(olsrr)
library(MASS)
library(fastDummies)
library(stargazer)

#import data
wines <- read.csv("wines_SPA.csv")

# omiiting Nans
df_wines <- na.omit(wines)

# creating a column with todays year
today <- 2022
df_wines$today <- today

# calculating the age of the wines
df_wines$age <- (df_wines$today - strtoi(df_wines$year))

# Making binary 1 if region is among the popular 6 regions on wine in Spain and 0 otherwise
df_wines$region_dummy <- ifelse(df_wines$region %in% c('Rioja','Andalucia','Galicia', 'Ribera del Duero', 'Cataluna', 'Valencia'), 1, 0)

# removing unnecessary columns and dropping nans, if any
df_wines <- df_wines[, !names(df_wines) %in% c("year", "today", "country", "type", "wine", "region", "winery")]
df_wines <- na.omit(df_wines)

# Variables to convert to log
max(df_wines$price) - min(df_wines$price)
max(df_wines$num_reviews) - min(df_wines$num_reviews)
max(df_wines$age) - min(df_wines$age)



# THE BEST YET # based on R^2
regression_2 <- lm(log(price) ~ rating + region_dummy + num_reviews + factor(body) + factor(acidity) + poly(age,2,raw=T) , data = df_wines)
summary(regression_2)

# DIAGNOSTIC TESTS

# 1. Linearity test RESET
reset_test <- resettest(regression_2, power = 2:3, type = c("regressor"))
reset_test


# 2. Normality of residuals

# Histogram + normal curve
h <- hist(regression_2$residuals, col = "blue", xlab = "Residuals", freq=F)
xfit <- seq(min(regression_2$residuals), max(regression_2$residuals), length=40)
yfit <- dnorm(xfit, mean=mean(regression_2$residuals), sd=sd(regression_2$residuals))
lines(xfit, yfit, col="red", lwd=2)

# q-q plot for standardized residuals
plot(regression_2, which=2)

# Jarque-Berra test
jarquebera_test <- jarqueberaTest(regression_2$residuals)
jarquebera_test

# 3. Homoscedasticity

# Residuals vs fitted - we should have randomly located points
plot(regression_2, which=1)

# Breusch-Pagan test -> H0: homoscedastic residuals
braushpagan_test <- bptest(regression_2, studentize=FALSE)
braushpagan_test

# 5. Autocorrelation
# Durbin-Watson test
durbinwatson_test <- dwtest(regression_2)
durbinwatson_test

# Data Frame with all the tests
jarquebera_test_statistic <- 4937.7231
jarquebera_test_p_value <- 2.2e-16
df_diagnostic_tests <- data.frame (
  CLRM_CONDITION = c("Linear in parameter", "Zero mean of error term", "Homoscedasticity", "No autocorrelation"),
  TEST_NAME = c("RESET_TEST","JARQUE_BERA_TEST","BRAUSCH_PAGAN_TEST","DURBING_WATSON_TEST"),
  TEST_STATISTIC = c(reset_test$statistic,jarquebera_test_statistic,braushpagan_test$statistic,durbinwatson_test$statistic),
  TEST_P_VALUE  = c(reset_test$p.value,jarquebera_test_p_value,braushpagan_test$p.value,durbinwatson_test$p.value)
)
df_diagnostic_tests

