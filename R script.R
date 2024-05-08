# Using Elastic Net Regression on Wine Quality database.

# Exploring the data
library(tidyverse)

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

# Seperating testing and training data. Used 25% and 75% to mirror conditions 
# used by Cortez et al.
red_train <- wine_red[1:1199, ]
red_test <- wine_red[1200:1599, ]

white_train <- wine_white[1:3750, ]
white_test <- wine_white[3751:4898, ]

# Training elastic net regression
install.packages("glmnet")
library(glmnet)
