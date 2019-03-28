##initializing the data set and summarizing it
data <- read.csv("kc_house_data.csv")
View(data)
summary(data)

##installing libraries
library(corrplot)
library(Hmisc)
library(car)
library(dplyr)
library(gbm)

#### data cleaning ####
## checking for NA value in the data ##
sapply(data, function(x) mean(is.na(data)))

##cleaning the date column by removing junk values
clean <- data$date
data$date <- gsub("T000000","",clean)
head(data)

#convering the data type from character to numeric
data$bedrooms <- as.numeric(as.character(data$bedrooms))

##testing for outliers
boxplot(data$bedrooms, data$bathrooms, data$floors)
boxplot(data$sqft_living, data$sqft_lot)
boxplot(data$condition, data$grade)
boxplot(data$price)

##correcting the typo
boxplot(data$bedrooms)
table(data$bedrooms)
data$bedrooms <- gsub("33", "3", data$bedrooms)
table(data$bedrooms)

#####removing unwanted columns
##year renovated columns contains many values as 0, if the percentage of '0' value
##is significant, the column is removed
count_year_re <- length(which(data$yr_renovated == 0))
count_year_re
##percentage of value
percent <- 20699/21613
percent
##as the percentage is 95.77% the column is removed
data$yr_renovated <- NULL


######## creating a correlation matrix ###########

##creating a data matrix
data_mat <- rcorr(as.matrix(data))
data_mat
##calling the coefficients of correlation
coeff <- cor(data_mat$r)
coeff
## plotting the values of coefficients of correlation
corrplot(coeff)

##removing additional columns
data$date <- NULL
data$sqft_lot <- NULL
data$id <- NULL
data$sqft_basement <- NULL
data$sqft_living15 <- NULL
data$sqft_lot15 <- NULL
 
data$bedrooms <- as.numeric(as.character(data$bedrooms))

#### linear modelling ####
##creating a training model and testing model
library(ISLR)
set.seed(152)
sample_size <- floor(0.80*nrow(data))
sample_size
train_data <- sample(seq_len(nrow(data)), size = sample_size)
train = data[train_data,]
test = data[-train_data,]

##training model
lm_modeltrain <- lm( price ~ ., data = train)
summary(lm_modeltrain)
alias(price ~., data = train)

vif(lm_modeltrain)

plot(lm_modeltrain)

##test model
lm_modeltest <- lm( price ~ ., data = test)
summary(lm_modeltest)
alias(price ~., data = test)

vif(lm_modeltest)

plot(lm_modeltest)


## creating predictive model
house1 <- data.frame(bedrooms = 3, bathrooms = 2, sqft_living = 1800, floors = 1, waterfront = 0, view = 1, grade = 8, condition = 4, sqft_above = 1800, yr_built = 1981, zipcode = 98178, lat = 47.5112, long = -122.257)
price <- predict(lm_modeltest, house1)
price

###### optimized linear modelling #######

### applying log transformation to price variable to normalize the data
hist(data$price)

data$price <- log(data$price)
hist(data$price)

###removing the variable floors from the data set
data$floors <- NULL

### creating a new linear model
set.seed(152)
sample_size <- floor(0.80*nrow(data))
sample_size
train_data <- sample(seq_len(nrow(data)), size = sample_size)
train = data[train_data,]
test = data[-train_data,]

## train model
lm2_train <- lm(price~., data = train)
summary(lm2_train)
vif(lm2_train)

##test model
lm2_test <- lm(price~., data = test)
summary(lm2_test)
vif(lm2_test)

## creating predictive model for new linear model
house1 <- data.frame(bedrooms = 3, bathrooms = 2, sqft_living = 1800, waterfront = 0, view = 1, grade = 8, condition = 4, sqft_above = 1800, yr_built = 1981, zipcode = 98178, lat = 47.5112, long = -122.257)
price <- predict(lm2_test, house1)
price

###### stepwise modelling ########

model3 <- step(lm2_test, direction = "backward", trace = TRUE)


######## gradient boosting model ########

### gbm for training data set
data.boost <- gbm(price~., data = train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
data.boost
summary(data.boost)

### checking the optimal number of iterations
gbm.perf(data.boost)

### predict the value of gbm model with obtained number of iterations
p1 <- predict(data.boost, n.trees = 2744)

### comparing the predicted values to the original value
summary(p1)

summary(data$price)

### gbm for test data set 
data.boost_test <- gbm(price~., data = test, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
data.boost_test
summary(data.boost_test)

### checking the optimal number of iterations
gbm.perf(data.boost_test)

### predicting the values of gbm based on optimal iterations
p2 <- predict(data.boost, n.trees = 1182)

### comparing the predicted values to the original values
summary(p2)

summary(data$price)

## creating predictive model for gradiant boosting model
house1 <- data.frame(lat = 47.5112, sqft_living = 1800, grade = 8, long = -122.257, view = 1, zipcode = 98178, yr_built = 1981, sqft_above = 1800, condition = 4, waterfront = 0, bathrooms = 2, bedrooms = 3)
price <- predict(data.boost_test, n.trees = 1182, house1)
price