#######################################
# Project 3                           # 
# DATA 511 - Intro to Data Science    #
# Chad Zeller                         #
# November 8, 2021                    #
#######################################

# Set seed to 12345
set.seed(12345)

# Load libraries
library(plyr)
library(caret)
library(rattle)

# Import data set 
proj3 <- read.csv("/Users/chadzeller/Documents/proj3_income")

# Delete the variable occupation
proj3$occupation <- NULL

# Capital gains missing values (99999) changed to NA
proj3$capital.gain[proj3$capital.gain == 99999] <- NA

# Mean of capital gains without missing values
cgm <- mean(proj3$capital.gain, na.rm=TRUE); cgm

# Standard deviation of capital gains without missing values
cgsd <- sd(proj3$capital.gain, na.rm=TRUE); cgsd

imputation_model <- 
  preProcess(proj3, method = c("knnImpute"))

# Use the knnImpute model to predict missing values
proj3.imp <- predict(imputation_model, proj3)

# De-standardize the imputed capital gains to original scale without missing values
proj3$cg.imp <- round(proj3.imp$capital.gain * cgsd + cgm, 5)

# Compute statistics (mean, standard deviation) for the imputed capital gains
cgm_imp <- mean(proj3$cg.imp, na.rm=TRUE); cgm_imp
cgsd_imp <- sd(proj3$cg.imp, na.rm=TRUE); cgsd_imp


##### 1. #####

## 1a. ##
# Summary of income
summary(proj3)

# Create table of income
income_table <- table(proj3$income); income_table

# Proportion of high income earners
round(prop.table(income_table),4)*100

# Income & Education table before reclassification
income_educ_table <- table(proj3$income, proj3$education); income_educ_table

# Reclassify education into educ with high/low categories
proj3$educ <- revalue(proj3$education, 
                      c("Preschool" = "low", "1st-4th" = "low",
                        "5th-6th" = "low", "7th-8th" = "low",
                        "9th" = "low", "10th" = "low",
                        "11th" = "low", "12th" = "low",
                        "HS-grad" = "low", "Some-college" = "low",
                        "Assoc-acdm" = "high", "Assoc-voc" = "high",
                        "Bachelors" = "high", "Masters" = "high",
                        "Prof-school" = "high", "Doctorate" = "high"))

# Delete the education variable
proj3$education <- NULL

# Income & Education table after reclassification
new_income_educ_table <- table(proj3$income, proj3$educ); new_income_educ_table

# Income & Education table of proportions
new_prop_income_educ_table <- round(prop.table(new_income_educ_table, margin = 2)*100, 2);new_prop_income_educ_table
                                  
                                    
## 1b. ##
# Relationship & Income table before reclassification
relation_income_table <- table(proj3$relationship, proj3$income); relation_income_table

# Reclassify relationship into rel with HusWife/Other categories
proj3$rel <- revalue(proj3$relationship, 
                      c("Husband" = "HusWife", "Wife" = "HusWife",
                        "Not-in-family" = "Other", "Other-relative" = "Other",
                        "Own-child" = "Other", "Unmarried" = "Other"))

# Delete the education variable
proj3$relationship <- NULL

# Relationship & Income table after reclassification
new_rel_income_table <- table(proj3$income, proj3$rel); new_rel_income_table

# Relationship & Income table of proportions
new_prop_rel_inc_table <- round(prop.table(new_rel_income_table, margin = 2)*100, 2);new_prop_rel_inc_table


##### 2. #####
#  Partition the data set into proj3.tr & proj3.te, splitting records 50/50%
inTrain <- createDataPartition(y = proj3$income, p = .5,
                               list = FALSE)

# Display structure of inTrain
str(inTrain)

# Select inTrain training set records
proj3.tr <- proj3[inTrain,]

# Select inTrain test set records
proj3.te <- proj3[-inTrain,]


## 2a. ##
# Summary and Table of inTrain training set's income variable
summary(proj3.tr$income)
table(proj3.tr$income)

# Summary and Table of inTrain test set's income variable
summary(proj3.te$income)
table(proj3.te$income)


## 2b. ##
# Append a new variable part (partition) to each training set 
proj3.tr$part <- rep("train", nrow(proj3.tr))
proj3.te$part <- rep("test", nrow(proj3.te))

# Merge the two data sets using rbind
proj3.all <- rbind(proj3.tr, proj3.te)

# Create boxplot for capital gains & capital losses
boxplot(proj3.all$capital.gain ~ as.factor(part), data = proj3.all)
boxplot(proj3.all$capital.loss ~ as.factor(part), data = proj3.all)

# Run Kruskal-Wallis test for capital gains & capital losses
kruskal.test(proj3.all$capital.gain ~ as.factor(part), data = proj3.all)
kruskal.test(proj3.all$capital.loss ~ as.factor(part), data = proj3.all)


## 2c. ##

# Table of training & test set's educ variable for counts
table(proj3.tr$educ)
table(proj3.te$educ)

# Create matrix based on counts for educ variable
educ_pt_table <- matrix( c(5258, 5258, 11023, 11022), ncol = 2); educ_pt_table

# Chi-squared test for educ variable
prop.test(educ_pt_table, correct = FALSE)

# Table of training & test set's rel variable for counts
table(proj3.tr$rel)
table(proj3.te$rel)

# Create matrix based on counts for rel variable
rel_pt_table <- matrix( c(7378, 7383, 8903, 8897), ncol = 2); rel_pt_table

# Chi-squared test for rel variable
prop.test(rel_pt_table, correct = FALSE)


##### 3. #####
# Obtain training set data counts for target variable income
table(proj3.tr$income)

