#######################################
# Project 2                           # 
# DATA 511 - Intro to Data Science    #
# Chad Zeller                         #
# October 18, 2021                    #
#######################################

# Load libraries (ggplot2)
library(ggplot2)

########## Part 1: Simple Linear Regression ##########

##### 1. #####
#  Regress Score on Price for Cameras data set
cameras_reg <- lm(Score~Price, data = Cameras); cameras_reg


##### 2. #####
## 2a. ##
# Estimate score for a camera costing $70
46.6688 + .05525 * 70

## 2b & 2c. ##
# Calculate r squared & s (standard error of estimate)
summary(cameras_reg)


##### 3. #####
# Calculate standardized residuals
cameras.rstand <- rstandard(cameras_reg); round(cameras.rstand, 4)
cameras.residual <- residuals(cameras_reg); round(cameras.residual, 4)

#Calculate predicted scores for cameras #17 & 28
Cameras$Score[17] - cameras.residual[17]
Cameras$Score[28] - cameras.residual[28]


##### 4. #####
# Calculate leverage values (hatvalues) for cameras
round(hatvalues(cameras_reg), 4)

# Plot the leverage values (hatvalues) for cameras
plot(hatvalues(cameras_reg), type = "h", main = "Leverage Values", xlab = "Camera Number", 
     ylab = "Hat Values")


##### 5. #####
# Calculate median of F-Distribution to identify cutoff for influential observations
round(qf(0.5, 1, 26), 4)

# Calculate 25th percentile of F-Distribution to identify cutoff  for non-influential observations
round(qf(0.25, 1, 26), 4)

# Calculate Cook's Distance
sort(cooks.distance(cameras_reg))

##### 6. #####
# Display Residuals vs. Fits Plot & Normal Q-Q Plot
plot(cameras_reg, pch = 19)


##### 7. #####
# Calculate p-value to determine linear relationship between Score & Price 
summary(cameras_reg)


##### 8. #####
# Assign the score value for a new camera with $250 price
new_camera <- data.frame(Price = 250)

# Calculate the 99% prediction interval for a new camera with $250 price
round(predict(cameras_reg, new_camera, interval = "prediction", level = 0.99), 4)


########## Part 2: Analysis of Variance ##########

##### 9. #####
# Summary statistics of Facebook data set
summary(Facebook)

# Create boxplot of student motivation, by level of instructor
boxplot(Facebook$S.M ~ Facebook$Level, main = "Student Motivation by Instructor Self-Disclosure", 
        xlab = "Instructor Self-Disclosure", ylab = "Student Motivation")

##### 10. #####
# Verify ANOVA normality assumption using a quick plot
qplot(sample = S.M,
      data = Facebook, color = Level)

# Check equal variances assumption using Bartlett's Test
bartlett.test(S.M ~ Level, data = Facebook)


##### 11. #####
# Create variable ANOVAfit for analysis of variance (ANOVA)
ANOVAfit <- aov(S.M ~ Level, data = Facebook); ANOVAfit

# Display ANOVA results
summary(ANOVAfit)


##### 12. #####
# Run Tukey's HSD Method on the ANOVAfit variable
TukeyHSD(ANOVAfit)

