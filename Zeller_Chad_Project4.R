#######################################
# Project 4                           # 
# DATA 511 - Intro to Data Science    #
# Chad Zeller                         #
# November 30, 2021                   #
#######################################

# Set seed to 12345
set.seed(12345)

# Load libraries
library(plyr)
library(caret)
library(rattle)

# Import data sets 
proj4_te <- read.csv("/Users/chadzeller/Documents/proj3_te")
proj4_tr <- read.csv("/Users/chadzeller/Documents/proj3_tr")


##### 2. #####
# Set k-fold cross-validation parameters using trainControl with 10 folds
TC <- trainControl(method = "CV", number = 10)

# Set names for CART model
colnames(proj4_tr) <- make.names(colnames(proj4_tr))

# Perform CART with 10-fold cross-validation on proj4_tr data set
fit <- train(income ~ .,
             data = proj4_tr[],
             method = "rpart2",
             trControl = TC)

# Create decision tree with fancyRpartPlot
fancyRpartPlot(fit$finalModel)


##### 3. #####
# Check for overfitting using fit$resample
fit$resample


##### 4. #####
# Apply the model fit to the proj4_te test data set using predict
colnames(proj4_te) <- make.names(colnames(proj4_te))
tesetpred <- predict(fit, proj4_te)

# Create a contingency table of test set predictions against actual test set
table(proj4_te$income, tesetpred)
