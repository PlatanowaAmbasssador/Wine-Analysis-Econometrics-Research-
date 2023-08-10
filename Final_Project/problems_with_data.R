library(car)
library(olsrr)
library(haven)

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

# ols regression summary
ols_regress(log(price) ~ rating + region_dummy + num_reviews + factor(body) + factor(acidity) + poly(age,2,raw=T) , data = df_wines)

# OLS
regression_2 <- lm(log(price) ~ rating + region_dummy + num_reviews + factor(body) + factor(acidity) + poly(age,2,raw=T) , data = df_wines)
summary(regression_2)

#residuals
df_wines$resids <- residuals(regression_2)

#leverages
# Leverage refers to the extent to which the coefficients 
# in the regression model would change if a particular observation 
# was removed from the dataset.
df_wines$lev <- hatvalues(regression_2)

# standardized residuals
# Standardized residuals refer to the standardized difference 
# between a predicted value for an observation and the actual 
# value of the observation.
df_wines$rstd <- rstandard(regression_2)

# Cook distance
df_wines$cookd <- cooks.distance(regression_2)

# fitted values
df_wines$yhat <- fitted(regression_2)

# For how many observations leverage is > 2k/n?
# length(reg1$coefficients) -- # of parameters, k
# nrow(laptops) -- # obs., n
# threshold = 0.003624382
length(df_wines$lev[df_wines$lev > (2*(length(regression_2$coefficients)/nrow(df_wines)))])
head(ols_leverage(regression_2))

# For how many observations |stand.residuals| >2?
length(df_wines$rstd[abs(df_wines$rstd)>2])

# For how many observation Cook's distance is > 4/n?
# 4/n = 0.0006589786
length(df_wines$cookd[df_wines$cookd > 4/nrow(df_wines)])

nontypical <- df_wines[df_wines$lev > 0.003624382 & abs(df_wines$rstd)>2 & df_wines$cookd > 0.0006589786, ]
nontypical
length(nontypical)

# Cook's distance plot for subsequent observations
# abline -- adds a threshold line
plot(regression_2, which=4, cook.level=(4/nrow(df_wines)))
abline(h=4/nrow(df_wines), lty=2, col= "red")
# or
ols_plot_cooksd_chart(regression_2)

# Leverage vs standardized residuals
plot(regression_2, which=5)
# or
ols_plot_resid_lev(regression_2)

# COLLINEARITY
vif <- ols_vif_tol(regression_2)
df_vif <- data.frame(
  VARIABLES = vif$Variables,
  TOLERANCE = vif$Tolerance,
  VIF = vif$VIF
)
df_vif

vif$Variables

# let's remind what were the coefficients in our model:
summary(regression_2)
avPlots(regression_2)
avPlot(regression_2, "inches") # for single variable

