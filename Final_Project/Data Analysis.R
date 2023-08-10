library(corrplot)
library(foreign)
library(PerformanceAnalytics)
library(haven)
library(fBasics)


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

#### DATA ANALYSIS ####

### PRICE VS RATING

# Scatter Plot
plot(price ~ rating, 
     data=df_wines, 
     pch=19, 
     col="blue", 
     main="Price vs Rating", 
     xlab="Rating", 
     ylab="Price")

# Histogram
hist(df_wines$rating, col = "blue", xlab = "Rating", main="Rating Frequency") # freq=T

# Boxplot
boxplot(df_wines$price ~ df_wines$rating, main= "Boxplot", ylab = "Price", xlab='Rating')

# correlation
# Shapiro-Wilk test: H0 - normality of data
jarqueberaTest(df_wines$rating) # not normal --> pearsman-correlation is not good for us.

# spearman correlation --> p-value > 0.05 -> true correlation is zero
cor(df_wines$rating, df_wines$price, method="spearman")

#
#
#
#
#

### PRICE VS NUM_REVIEWS

# Scatter Plot
plot(price ~ num_reviews, 
     data=df_wines, 
     pch=19, 
     col="blue", 
     main="Price vs num_reviews", 
     xlab="num_reviews", 
     ylab="Price")

# Histogram
hist(df_wines$num_reviews, col = "blue", xlab = "num_reviews", main = "num_reviews frequency") # freq=T

# Correlations

# Shapiro-Wilk test: H0 - normality of data
jarqueberaTest(df_wines$num_reviews) # not normal --> pearsman-correlation is not good for us.

# spearman correlation --> p-value > 0.05 -> true correlation is zero
cor(df_wines$price, df_wines$num_reviews, method="spearman")

#
#
#
#
#

### PRICE VS REGION_DUMMY

# unique values table
table(df_wines$region_dummy)
df_unique_region_dummy_len <- data.frame ( 
  REGION_TYPE = c("UNPOPULAR", "POPULAR"),
  VALUE = c(0,1),
  COUNT = c(2555, 3515)
)
df_unique_region_dummy_len
  
# Boxplot
boxplot(df_wines$price ~ df_wines$region_dummy, main= "Boxplot", xlab = "region_dummy", ylab = "price")
##########
#
#
#
#
#

### PRICE VS BODY
# Scatter Plot
plot(price ~ body, 
     data=df_wines, 
     pch=19, 
     col="blue", 
     main="Price vs body", 
     xlab="body", 
     ylab="Price")

# Histogram
hist(df_wines$body, col = "blue", xlab = "body", main = "body frequency") # freq=T

# Boxplot
boxplot(df_wines$price ~ df_wines$body, main= "Boxplot", xlab = "body", ylab = "price")

#
#
#
#
#

### PRICE VS ACIDITY

# Scatter Plot
plot(price ~ acidity, 
     data=df_wines, 
     pch=19, 
     col="blue", 
     main="Price vs acidity", 
     xlab="acidity", 
     ylab="Price")

# Histogram
hist(df_wines$acidity, col = "blue", xlab = "acidity", main = "acidity frequency") # freq=T

# Boxplot
boxplot(df_wines$price ~ df_wines$acidity, main= "Boxplot", xlab = "acidity", ylab = "price")

#
#
#
#
#

### PRICE VS AGE

# Scatter Plot
plot(price ~ age, 
     data=df_wines, 
     pch=19, 
     col="blue", 
     main="Price vs age", 
     xlab="age", 
     ylab="Price")

# Histogram
hist(df_wines$age, col = "blue", xlab = "age", main = "age frequency") # freq=T

# Correlations

# Shapiro-Wilk test: H0 - normality of data
jarqueberaTest(df_wines$age) # not normal --> pearsman-correlation is not good for us.

# spearman correlation --> p-value > 0.05 -> true correlation is zero
cor(df_wines$price, df_wines$age, method="spearman")

# mean, median, standard deviation, minimum and maximum
df_stats <- data.frame (
  VARIABLE = c("price","rating","num_reviews","body","acidity","age"),
  OBS. = c(length(df_wines$price),length(df_wines$rating),length(df_wines$num_reviews),length(df_wines$body),length(df_wines$acidity),length(df_wines$age)),
  MEAN = c(mean(df_wines$price),mean(df_wines$rating),mean(df_wines$num_reviews),mean(df_wines$body),mean(df_wines$acidity),mean(df_wines$age)),
  MEDIAN = c(median(df_wines$price),median(df_wines$rating),median(df_wines$num_reviews),median(df_wines$body),median(df_wines$acidity),median(df_wines$age)),
  STD.DEV. = c(stdev(df_wines$price),stdev(df_wines$rating),stdev(df_wines$num_reviews),stdev(df_wines$body),stdev(df_wines$acidity),stdev(df_wines$age)),
  MIN = c(min(df_wines$price),min(df_wines$rating),min(df_wines$num_reviews),min(df_wines$body),min(df_wines$acidity),min(df_wines$age)),
  MAX = c(max(df_wines$price),max(df_wines$rating),max(df_wines$num_reviews),max(df_wines$body),max(df_wines$acidity),max(df_wines$age))
)
df_stats
