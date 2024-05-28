# Using Elastic Net Regression and Least Absolute Deviation Regression on Wine Quality database.

# Uploading wine quality datasets

library(readr)
wine_red <- read_delim("C:/Users/tjf4x/Desktop/R projects/Wine quality/winequality-red.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
library(readr)
wine_white <- read_delim("C:/Users/tjf4x/Desktop/R projects/Wine quality/winequality-white.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Exploring the data -

library(tidyverse)
library(ggplot2)
install.packages("sfsmisc")
library(sfsmisc)

glimpse(wine_red)
glimpse(wine_white)

# Checking for missing values

missing_red <- !complete.cases(wine_red)
wine_red[missing_red, ]

missing_white <- !complete.cases(wine_white)
wine_white[missing_white, ]

# Checking the distribution of the Quality feature

hist(wine_red$quality)

hist(wine_white$quality)

# Comparing quality to independent variables

install.packages("psych")
library(psych)

pairs.panels(wine_red)

pairs.panels(wine_white)

# investigating outliers

install.packages("outliers")
library(outliers)
library(reshape2)
m1 <- melt(as.data.frame(wine_red))
ggplot(m1,aes(x = variable,y = value)) + facet_wrap(~variable) + geom_boxplot()

m1 <- melt(as.data.frame(wine_white))
ggplot(m1,aes(x = variable,y = value)) + facet_wrap(~variable) + geom_boxplot()

# Seperating testing and training data. Used 25% and 75% to mirror conditions 
# used by Cortez et al. 

redvalue <- nrow(wine_red) * 0.75
whitevalue <- nrow((wine_white)) * 0.75

red_train <- wine_red[1:floor(redvalue), ]
red_test <- wine_red[ceiling(redvalue):nrow(wine_red), ]

white_train <- wine_white[1:floor(whitevalue), ]
white_test <- wine_white[ceiling(whitevalue):nrow(wine_white), ]

# Red Wine Models
# ---------------

# Creating linear model baseline
# ------------------------------

linear_red_1 <- lm(quality ~ ., data=red_train)

summary(linear_red_1)

# R-squared = 0.33843

# Evaluating MAE on test set

linear_red_1_predict <- predict(linear_red_1, red_test)

MAE(linear_red_1_predict, red_test$quality)

# MAE linear model = 0.5281143

MAE(mean(red_train$quality), red_test$quality)

# For comparison the MAE by simply computing the mean = 0.6743573


# Second linear model including quadratic terms
# ---------------------------------------------

linear_red_2 <- lm(quality ~ . + I(`total sulfur dioxide`^2) + I(`free sulfur dioxide`^2) + I(`residual sugar`^2) + I(`citric acid`^2), data = red_train)

summary(linear_red_2)

# R-squared = 0.3874

# Calculating MAE
linear_red_2_predict <- predict(linear_red_2, red_test)

MAE(linear_red_2_predict, red_test$quality)

# MAE linear model with quadratic terms = 0.5327483


# Elastic Net Regression
# ----------------------

# Training elastic net regression

install.packages("glmnet")
library(glmnet)
library(caret)

# Tuning and results functions

custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = FALSE)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

#Red elastic regression without interaction terms

elastic_red <- train(quality~.,
                    red_train,
                    method='glmnet',
                    tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                          lambda = seq(0.0001,0.2,length=5)),
                    trControl=custom)

# Getting best results

get_best_result(elastic_red)

# alpha lambda      RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
#     0 0.050075 0.6529895 0.3741576 0.5098245 0.04871038 0.07088593 0.03668794

elastic_red_1_predict <- predict(elastic_red, red_test)

MAE(elastic_red_1_predict, red_test$quality)

# Elastic net MAE on test set 0.5284164

elastic_red_2 <- train(quality~.^2,
                 red_train,
                 method='glmnet',
                 tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                       lambda = seq(0.0001,0.5,length=5)),
                 trControl=custom)

# Getting best results

get_best_result(elastic_red_2)

# alpha lambda      RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
#    0 0.125075 0.6477815 0.3851634 0.5044805 0.04251387 0.06726575 0.0325463

elastic_red_2_predict <- predict(elastic_red_2, red_test)

MAE(elastic_red_2_predict, red_test$quality)

# Elastic net MAE on test set 0.0.5238079
# Best so far.

# Plotting residuals

elastic_red_2_resid <- resid(elastic_red_2)
plot(fitted(elastic_red_2), elastic_red_2_resid) 
abline(0,0) 

# rounding prediction to the nearest integer and comparing values as a confusion
# matrix to further investigate model deficiencies.

elastic_red_mae_rounded <- mean(abs(red_test$quality - round(elastic_red_2_predict)))

confusionMatrix(as.factor(round(elastic_red_2_predict)), as.factor(red_test$quality))

# Rounded elastic net MAE on test set = 0.434018
# Accuracy = 0.61
# Kappa 0.3364

# Least Absolute Deviation regression
# -----------------------------------

install.packages("L1pack")
library(L1pack)

lad_red <- lad(quality ~ ., data=red_train)

summary(lad_red)

lad_red_predict <- predict(lad_red, red_test)

MAE(lad_red_predict, red_test$quality)

# LAD MAE on test set 0.526982
# Still better

lad_red_mae_rounded <- mean(abs(red_test$quality - round(lad_red_predict)))

confusionMatrix(as.factor(round(lad_red_predict)), as.factor(red_test$quality))

# Rounded elastic net MAE on test set = 0.445748
# Accuracy = 0.61
# Kappa 0.3394


# Adding a interaction variable between alcohol and volatile acidity
# these appeared to have the strongest relationship with the high and low ratings

lad_red_2 <- lad(quality ~ . + alcohol:`volatile acidity`, data=red_train)

summary(lad_red_2)

lad_red_predict_2 <- predict(lad_red_2, red_test)

MAE(lad_red_predict_2, red_test$quality)

# LAD MAE on test set 0.52462

# Plotting residuals to observe patterns

res <- resid(lad_red_2) 

# produce residual vs. fitted plot 

plot(fitted(lad_red_2), res) 

abline(0,0) 

plot(density(res))

# The model still struggles to predict the highest and lowest ratings

lad_red_mae_rounded <- mean(abs(red_test$quality - round(lad_red_predict_2)))

confusionMatrix(as.factor(round(lad_red_predict_2)), as.factor(red_test$quality))

# Rounded LAD MAE on test set = 0.45161
# Accuracy = 0.60
# Kappa 0.3245

# MARS
# ----

install.packages("earth")
library(earth)

# Define values for x and y 

ncol(red_train)

x <- red_train[, -12]
y <- red_train$quality

# Create a parameter tuning grid
parameter_grid <- floor(expand.grid(degree = 1:4, nprune = seq(5, 25, by = 1)))

# Create new tuning function

custom1 <- trainControl(method = "repeatedcv",
                        repeats = 3,
                        verboseIter = FALSE)

# Perform cross-validation
red_mars <- train(x = x,
                  y = y,
                  method = "earth",
                  metric = "Rsquared",
                  trControl = custom1,
                  tuneGrid = parameter_grid)

# Visualize the results from the cross validation on the parameter tuning 

ggplot(red_mars) 

get_best_result(red_mars)

red_mars$results

# Use the best model to predict response variable for the test set 
mars_predict <- predict(object = red_mars$finalModel, 
                        newdata = red_test)

# Get the MAE (mean absolute error)
mars_mae <- mean(abs(red_test$quality - mars_predict))

mars_mae_rounded <- mean(abs(red_test$quality - round(mars_predict)))

confusionMatrix(as.factor(round(mars_predict)), as.factor(red_test$quality))

install.packages("vip")
library(vip)

red_vip <- vip(red_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")

plot(red_vip)

# White Wine Models
# -----------------


# Creating linear model baseline
# ------------------------------

linear_white_1 <- lm(quality ~ ., data=white_train)

summary(linear_white_1)

# R-squared = 0.3051

# Evaluating MAE on test set

linear_white_1_predict <- predict(linear_white_1, white_test)

MAE(linear_white_1_predict, white_test$quality)

# MAE linear model with no quadratic terms = 0.5685166

MAE(mean(white_train$quality), white_test$quality)

# For comparison the MAE by simply computing the mean = 0.5852432

# Second linear model including quadratic terms
# ---------------------------------------------

linear_white_2 <- lm(quality ~ . + I(`sulphates`^2) + I(`chlorides`^2) + I(`residual sugar`^2), data = white_train)

summary(linear_white_2)

# R-squared = 0.3066

# Calculating MAE
linear_white_2_predict <- predict(linear_white_2, white_test)

MAE(linear_white_2_predict, white_test$quality)

# MAE linear model with quadratic terms = 0.5698954


# Elastic Net Regression
# ----------------------

# Training elastic net regression

#Red elastic regression without interaction terms

elastic_white <- train(quality~.,
                    white_train,
                     method='glmnet',
                     tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                           lambda = seq(0.0001,0.2,length=5)),
                     trControl=custom)

# Getting best results

get_best_result(elastic_white)

# alpha lambda      RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
#      0  1e-04 0.7662902 0.2940911 0.5981886 0.01196175 0.01431521 0.007559827

elastic_white_1_predict <- predict(elastic_white, white_test)

MAE(elastic_white_1_predict, white_test$quality)

# Elastic net MAE on test set 0.5634004

elastic_white_2 <- train(quality~.^2,
                       white_train,
                       method='glmnet',
                       tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                             lambda = seq(0.0001,0.5,length=5)),
                       trControl=custom)

# Getting best results

get_best_result(elastic_white_2)

#    alpha lambda      RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
#        0  1e-04 0.7476503 0.3245182 0.5839565 0.01217412 0.01601512 0.009188031

elastic_white_2_predict <- predict(elastic_white_2, white_test)

MAE(elastic_white_2_predict, white_test$quality)

# Elastic net MAE on test set 0.5574479


# Least Absolute Deviation regression
# -----------------------------------

lad_white <- lad(quality ~ ., data=white_train)

summary(lad_white)

lad_white_predict <- predict(lad_white, white_test)

MAE(lad_white_predict, white_test$quality)

# LAD MAE on test set 0.5702528

# Similar to the red wine, I added a interaction function between alcohol and density

lad_white_2 <- lad(quality ~ . + alcohol:density, data=white_train)

summary(lad_white_2)

lad_white_predict_2 <- predict(lad_white_2, white_test)

MAE(lad_white_predict_2, white_test$quality)

# LAD MAE on test set 0.557702


# MARS on white wine dataset
# --------------------------

# Define values for x and y 

ncol(white_train)

xw <- white_train[, -12]
yw <- white_train$quality

# Perform cross-validation
white_mars <- train(x = xw,
                  y = yw,
                  method = "earth",
                  metric = "Rsquared",
                  trControl = custom1,
                  tuneGrid = parameter_grid)

# Visualize the results from the cross validation on the parameter tuning 

ggplot(white_mars) 

get_best_result(white_mars)

# Use the best model to predict response variable for the test set 
white_predict <- predict(object = white_mars$finalModel, 
                        newdata = white_test)

# Get the MAE (mean absolute error)
white_mars_mae <- mean(abs(white_test$quality - white_predict))

white_mars_mae_rounded <- mean(abs(white_test$quality - round(white_predict)))

confusionMatrix(as.factor(round(white_predict)), as.factor(white_test$quality))

white_vip <- vip(white_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")

plot(white_vip)
