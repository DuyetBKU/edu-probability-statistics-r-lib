# 1. Import data
library(readr)
setwd("C:/Users/ThinkPro/Documents/R/test") # Change working directory
heatdata <- read.csv("heat_data.csv", header = TRUE) # Read CSV file
View(heatdata) # View dataset

# 2. Data Cleaning - Remove missing values
is.na(heatdata) # Check missing values
heatdata <- na.omit(heatdata) # Remove rows with NA
heatdata # Display cleaned data

# 3. Data Visualization

# (a) Variable assignment
X1 <- heatdata$X1 # compactness
X2 <- heatdata$X2 # surface area
X3 <- heatdata$X3 # wall area
X4 <- heatdata$X4 # roof area
X5 <- heatdata$X5 # overall height
X6 <- heatdata$X6 # orientation
X7 <- heatdata$X7 # glazing area
X8 <- heatdata$X8 # glazing area distribution
Y1 <- heatdata$Y1 # heating load
Y2 <- heatdata$Y2 # cooling load

# (b) Descriptive statistics
summary(data.frame(X1, X2, X3, X4, X5, X6, X7, X8, Y1, Y2)) # Summary statistics
data.frame(
  sd(X1), sd(X2), sd(X3), sd(X4), sd(X5), sd(X6), sd(X7), sd(X8), sd(Y1), sd(Y2)
) # Standard deviations

# Histograms for each variable
hist(X1, main = "Histogram of Compactness", xlab = "Compactness", ylab = "Frequency")
hist(X2, main = "Histogram of Surface Area", xlab = "Surface Area", ylab = "Frequency")
hist(X3, main = "Histogram of Wall Area", xlab = "Wall Area", ylab = "Frequency")
hist(X4, main = "Histogram of Roof Area", xlab = "Roof Area", ylab = "Frequency")
hist(X5, main = "Histogram of Overall Height", xlab = "Overall Height", ylab = "Frequency")
hist(X6, main = "Histogram of Orientation", xlab = "Orientation", ylab = "Frequency")
hist(X7, main = "Histogram of Glazing Area", xlab = "Glazing Area", ylab = "Frequency")
hist(X8, main = "Histogram of Glazing Area Distribution", xlab = "Glazing Area Distribution", ylab = "Frequency")
hist(Y1, main = "Histogram of Heating Load", xlab = "Heating Load", ylab = "Frequency")
hist(Y2, main = "Histogram of Cooling Load", xlab = "Cooling Load", ylab = "Frequency")

# 4. Linear Regression Model
analysis <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8) # Fit model
anova(analysis) # ANOVA table
step(analysis, direction = "both") # Stepwise model selection

# 5. t-test to compare Y1 and Y2
t.test(Y1, Y2) # Two-sample t-test (different variance)
t.test(Y1, Y2, var.equal = TRUE) # Two-sample t-test (equal variance)
