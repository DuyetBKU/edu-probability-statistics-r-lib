library(readr)
setwd("C:/Users/ThinkPro/OneDrive/Documents/R/win-library/4.1")

Diet.data <- read.csv("Diet.csv", header = TRUE)
attach(Diet.data)

# Create data frame with selected variables
Diet.data.cau1a <- data.frame(gender, Age, Height, pre.weight, weight6weeks, Diet)
by(Diet.data.cau1a, Diet, summary)

# Standard deviation by Diet group
tapply(gender, Diet, sd, na.rm = TRUE)
tapply(Age, Diet, sd, na.rm = TRUE)
tapply(Height, Diet, sd, na.rm = TRUE)
tapply(pre.weight, Diet, sd, na.rm = TRUE)
tapply(weight6weeks, Diet, sd, na.rm = TRUE)

# Variance by Diet group
tapply(gender, Diet, var, na.rm = TRUE)
tapply(Age, Diet, var, na.rm = TRUE)
tapply(Height, Diet, var, na.rm = TRUE)
tapply(pre.weight, Diet, var, na.rm = TRUE)
tapply(weight6weeks, Diet, var, na.rm = TRUE)

# Remove missing values
na.omit(Diet.data)

# Calculate weight loss and add as new column
weightloss <- pre.weight - weight6weeks
Diet.data$weight.loss <- weightloss

# Boxplot for weight loss by diet type
boxplot(weightloss ~ Diet, 
        main = "Weight loss according to diet type", 
        ylab = "kg")

# Paired t-test for pre.weight and weight6weeks
t.test(pre.weight, weight6weeks, paired = TRUE)

# Check normality for each diet group
Diet1 <- subset(Diet.data, Diet == "1")
weight.loss1 <- pre.weight - weight6weeks
qqnorm(weight.loss1)
qqline(weight.loss1)

Diet2 <- subset(Diet.data, Diet == "2")
weight.loss2 <- pre.weight - weight6weeks
qqnorm(weight.loss2)
qqline(weight.loss2)

Diet3 <- subset(Diet.data, Diet == "3")
weight.loss3 <- pre.weight - weight6weeks
qqnorm(weight.loss3)
qqline(weight.loss3)

# Shapiro-Wilk normality test
attach(Diet1)
weight.loss1 <- pre.weight - weight6weeks
shapiro.test(weight.loss1)

attach(Diet2)
weight.loss2 <- pre.weight - weight6weeks
shapiro.test(weight.loss2)

attach(Diet3)
weight.loss3 <- pre.weight - weight6weeks
shapiro.test(weight.loss3)

# Bartlett's test for homogeneity of variances
bartlett.test(list(weight.loss1, weight.loss2, weight.loss3))

# ANOVA for weight loss by diet group
group <- c(rep("1", 24), rep("2", 27), rep("3", 27))
group <- as.factor(group)

library(dplyr)
fto <- Diet
ft01 <- arrange(fto, Diet)

weight.loss <- pre.weight - weight6weeks
dat <- data.frame(group, weight.loss)
analysis <- lm(weight.loss ~ group)
anova(analysis)
