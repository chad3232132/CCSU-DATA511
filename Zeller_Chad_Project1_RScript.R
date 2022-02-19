#######################################
# Project 1                           # 
# DATA 511 - Intro to Data Science    #
# Chad Zeller                         #
# September 28, 2021                  #
#######################################

# Set seed to 12345
set.seed(12345)

# Load libraries
library(plyr)
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(RANN)


##### 2. #####

# Create capital gains histogram
hist(proj1$capital.gain,
     main = "Capital Gains Histogram",
     xlab = "Capital Gains",
     ylab = "Count")

## 2a. ##
# Capital gains missing values (99999) changed to NA
proj1$capital.gain[proj1$capital.gain == 99999] <- NA

# Capital gains histogram without missing values
hist(proj1$capital.gain,
     main = "Capital Gains Histogram",
     xlab = "Capital Gains",
     ylab = "Count")

# Mean of capital gains without missing values
cgm <- mean(proj1$capital.gain, na.rm=TRUE); cgm

# Standard deviation of capital gains without missing values
cgsd <- sd(proj1$capital.gain, na.rm=TRUE); cgsd

## 2b. ##
# Sets the imputation model used for prediction
imputation_model <- 
  preProcess(proj1, method = c("knnImpute"))

# Use the knnImpute model to predict missing values
proj1.imp <- predict(imputation_model, proj1)

# De-standardize the imputed capital gains to original scale without missing values
proj1$cg.imp <- round(proj1.imp$capital.gain * cgsd + cgm, 5)

# Compute statistics (mean, standard deviation) for the imputed capital gains
cgm_imp <- mean(proj1$cg.imp, na.rm=TRUE); cgm_imp
cgsd_imp <- sd(proj1$cg.imp, na.rm=TRUE); cgsd_imp


##### 3. #####

# Constructs flag variable cg.miss; assign value 1 when capital gain missing, 0 otherwise
proj1$cg.miss <- ifelse(is.na(proj1$capital.gain), 1, 0)

# Constructs contingency table - missing vs. non-missing capital gains
ct.income.miss <- table(proj1$income, proj1$cg.miss)
colnames(ct.income.miss) <- c("Not Missing", "Missing"); ct.income.miss


##### 4. #####

# Create new ID field (proj$id)
proj1$id <- 1:nrow(proj1)

# Select data for record number 2001
proj1[2001,c(1, 3, 6, 2)]


##### 5. #####

# Rename marital-status as marital-status-old
(names(proj1)[names(proj1)=="marital.status"] <- "marital.status.old")

# Create new variable, marital.status, combining married categories into one, else Other
proj1$marital.status <- revalue(proj1$marital.status.old, 
                                c("Married-civ-spouse" = "Married",
                                  "Married-AF-spouse" = "Married",
                                  "Married-spouse-absent" = "Married",
                                  "Divorced" = "Other",
                                  "Never-married" = "Other",
                                  "Separated" = "Other",
                                  "Widowed" = "Other"))

# Create contingency table of counts with income for rows, marital.status for columns.
inc.marital.table <- table(proj1$income, proj1$marital.status)

# Converts table of counts into table of proportions, rounded to 2 decimals
round(prop.table(inc.marital.table, margin = 2)*100, 2)

##### 6. #####

# Creates new flag variable (capgl), equal to 1 when customer has imputed cap gains or
# any capital losses, else equal to 0
proj1$capgl <- ifelse(proj1$cg.imp != 0  | proj1$capital.loss != 0, 1, 0)

# Create contingency table with income as rows, capgl as columns with column proportions
ct.capgain.loss <- table(proj1$income, proj1$capgl)
colnames(ct.capgain.loss) <- c("No Capital Gains or Losses", "Has Capital Gains or Losses")
round(prop.table(ct.capgain.loss, margin = 2)*100, 2)

##### 7. #####

## 7a. ##
# Creates table of proportions of records with high income in data set
ct.highincome <- table(proj1$income); ct.highincome
ct.highincome <- round(prop.table(ct.highincome)*100, 2); ct.highincome

## 7b. ##
# Calculates capital loss mean
caploss.mean <- mean(proj1$capital.loss); caploss.mean
# Calculates capital loss standard deviation
caploss.sd <- sd(proj1$capital.loss); caploss.sd
# Calculates cutoff point for identifying outliers
caploss.cutoff <- (caploss.mean + 3 *caploss.sd); caploss.cutoff

## 7c. ##
# Finds records with values of capital.loss greater than cut-off
caploss.cutofflist <- proj1[(proj1$capital.loss > caploss.cutoff),]
# Number of outlier records with capital.loss values greater than cut-off
caploss.cutoffcount <- nrow(caploss.cutofflist); caploss.cutoffcount

## 7d. ##
# Table of number of records by income category among outlier records
ct.caploss.greater <- table(caploss.cutofflist$income); ct.caploss.greater
# Table of proportions of records by income category among outlier records
ct.caploss.greater <- round(prop.table(ct.caploss.greater)*100, 2); ct.caploss.greater


##### 8. #####

## 8a. ##
# Uses rpart() for predicting income based on education
income_ed_dt <- rpart(formula = proj1$income ~ proj1$education,
                      data = proj1,
                      control = rpart.control(
                        minbucket = .01 * nrow(proj1),
                        maxdepth = 2))

# Uses rpart.plot to plot the decision tree
rpart.plot(income_ed_dt)

# Uses cut function, with split thresholds from tree to construct new variable educ.bin
educ.bin <- cut(proj1$education,breaks = c(0, 13, 14, 16))


# Constructs contingency table of counts, with income for rows, educ.bin for columns
ct.dec.tree <- table(proj1$income, educ.bin); ct.dec.tree


## 8b. ##
# Converts contingency table from 8a. to column proportions, rounded to 2 decimals
ct.dec.tree <- round(prop.table(ct.dec.tree, 2)*100, 2); ct.dec.tree


##### 9. #####

## 9a. ##
# Constructs non-normalized stacked bar graph of educ.bin with overlay of income
ggplot(proj1, aes(educ.bin)) + geom_bar(aes(fill = income), position = "stack") + xlab("Education by Bin") + ylab("Frequency") + ggtitle("Education by Income Level")

## 9b. ##
# Constructs normalized stacked bar graph
ggplot(proj1, aes(educ.bin)) + geom_bar(aes(fill = income), position = "fill") + xlab("Education by Bin") + ylab("Frequency") + ggtitle("Education Based on Income Level")


##### 10. #####

## 10a. ##
# Table of counts, with sex as columns, income as rows, column totals included
inc.sex.table <- table(proj1$income, proj1$sex)
inc.sex.table <- addmargins(inc.sex.table); inc.sex.table

## 10b. ##
# Table of column proportions, with totals for columns only, rounded to 2 decimals
inc.sex.table.prop <- table(proj1$income, proj1$sex)
inc.sex.table.prop <- prop.table(inc.sex.table.prop, 2)
inc.sex.table.prop <- round(inc.sex.table.prop * 100, 2)
inc.sex.table.prop <- addmargins(inc.sex.table.prop, 1); inc.sex.table.prop
