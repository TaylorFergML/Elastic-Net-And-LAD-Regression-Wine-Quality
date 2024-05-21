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

red_train <- wine_red[1:1199, ]
red_test <- wine_red[1200:1599, ]

white_train <- wine_white[1:3750, ]
white_test <- wine_white[3751:4898, ]


# Red Wine Models
# ---------------

# Creating linear model baseline
# ------------------------------

linear_red_1 <- lm(quality ~ ., data=red_train)

summary(linear_red_1)

# R-squared = 0.3783

# Evaluating MAE on test set

# MAE function

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAE for model

linear_red_1_predict <- predict(linear_red_1, red_test)

MAE(linear_red_1_predict, red_test$quality)

# MAE linear model with no quadratic terms = 0.5123362


# Second linear model including quadratic terms
# ---------------------------------------------

linear_red_2 <- lm(quality ~ . + I(`total sulfur dioxide`^2) + I(`free sulfur dioxide`^2) + I(`residual sugar`^2) + I(`citric acid`^2), data = red_train)

summary(linear_red_2)

# R-squared = 0.3818

# Calculating MAE
linear_red_2_predict <- predict(linear_red_2, red_test)

MAE(linear_red_2_predict, red_test$quality)

# MAE linear model with quadratic terms = 0.5168531


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
                       verboseIter = TRUE)

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
#     0  1e-04 0.6425757 0.3741134 0.5057023 0.05288942 0.07281168 0.03786318

elastic_red_1_predict <- predict(elastic_red, red_test)

MAE(elastic_red_1_predict, red_test$quality)

# Elastic net MAE on test set 0.5128528

elastic_red_2 <- train(quality~.^2,
                 red_train,
                 method='glmnet',
                 tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                       lambda = seq(0.0001,0.5,length=5)),
                 trControl=custom)

# Getting best results

get_best_result(elastic_red_2)

# alpha lambda      RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
#    0 0.125075 0.6397919 0.380048 0.5021596 0.0481251 0.07036663 0.03356043

elastic_red_2_predict <- predict(elastic_red_2, red_test)

MAE(elastic_red_2_predict, red_test$quality)

# Elastic net MAE on test set 0.5105535 
# Best so far.

# Plotting residuals

elastic_red_2_resid <- resid(elastic_red_2)
plot(fitted(elastic_red_2), elastic_red_2_resid) 
abline(0,0) 

# as expected, the residuals are lower near the center of the quality data
# and greater near the edges.


# Least Absolute Deviation regression
# -----------------------------------

install.packages("L1pack")
library(L1pack)

lad_red <- lad(quality ~ ., data=red_train)

summary(lad_red)

lad_red_predict <- predict(lad_red, red_test)

MAE(lad_red_predict, red_test$quality)

# LAD MAE on test set 0.5081048
# Still better

# Adding a interaction variable between alcohol and volatile acidity
# these appeared to have the strongest relationship with the high and low ratings

lad_red_2 <- lad(quality ~ . + alcohol:`volatile acidity`, data=red_train)

summary(lad_red_2)

lad_red_predict_2 <- predict(lad_red_2, red_test)

MAE(lad_red_predict_2, red_test$quality)

# LAD MAE on test set 0.5063234

# Plotting residuals to observe patterns

res <- resid(lad_red_2) 

# produce residual vs. fitted plot 

plot(fitted(lad_red_2), res) 

abline(0,0) 

plot(density(res))

# The model still struggles to predict the highest and lowest ratings


# White Wine Models
# -----------------


# Creating linear model baseline
# ------------------------------

linear_white_1 <- lm(quality ~ ., data=white_train)

summary(linear_white_1)

# R-squared = 0.3018

# Evaluating MAE on test set

linear_white_1_predict <- predict(linear_white_1, white_test)

MAE(linear_white_1_predict, white_test$quality)

# MAE linear model with no quadratic terms = 0.5579922


# Second linear model including quadratic terms
# ---------------------------------------------

linear_white_2 <- lm(quality ~ . + I(`sulphates`^2) + I(`chlorides`^2) + I(`residual sugar`^2), data = white_train)

summary(linear_white_2)

# R-squared = 0.3031

# Calculating MAE
linear_white_2_predict <- predict(linear_white_2, white_test)

MAE(linear_white_2_predict, white_test$quality)

# MAE linear model with quadratic terms = 0.559131


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
#      0  1e-04 0.7687896 0.2959731 0.6011682 0.02381829 0.03845778 0.01658675

elastic_white_1_predict <- predict(elastic_white, white_test)

MAE(elastic_white_1_predict, white_test$quality)

# Elastic net MAE on test set 0.5532752

elastic_white_2 <- train(quality~.^2,
                       white_train,
                       method='glmnet',
                       tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                             lambda = seq(0.0001,0.5,length=5)),
                       trControl=custom)

# Getting best results

get_best_result(elastic_white_2)

#    alpha lambda      RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
# 0.1111111  1e-04 0.7476347 0.3372679 0.5781049 0.04349906 0.04593459 0.02053682

elastic_white_2_predict <- predict(elastic_white_2, white_test)

MAE(elastic_white_2_predict, white_test$quality)

# Elastic net MAE on test set 0.5567131


# Least Absolute Deviation regression
# -----------------------------------

lad_white <- lad(quality ~ ., data=white_train)

summary(lad_white)

lad_white_predict <- predict(lad_white, white_test)

MAE(lad_white_predict, white_test$quality)

# LAD MAE on test set 0.5584366

# Similar to the red wine, I added a interaction function between alcohol and density

lad_white_2 <- lad(quality ~ . + alcohol:density, data=white_train)

summary(lad_white_2)

lad_white_predict_2 <- predict(lad_white_2, white_test)

MAE(lad_white_predict_2, white_test$quality)

# LAD MAE on test set 0.557702