library(readr)

# Set working directory
setwd("C:/Users/ThinkPro/Documents/R/win-library/4.1")

# Read CSV (renamed to English)
house_data <- read.csv("house_prices.csv", header = TRUE)
View(house_data)

# Select relevant columns
new_DF <- house_data[c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")]

# Remove missing values
is.na(new_DF)
new_DF <- na.omit(new_DF)
new_DF

# Log transformation
price <- log(new_DF$price)
sqft_living15 <- log(new_DF$sqft_living15)
sqft_above <- log(new_DF$sqft_above)
sqft_living <- log(new_DF$sqft_living)

# Summary statistics
Mean <- data.frame(t(apply(new_DF[c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")], 2, mean)), row.names = "Mean")
Median <- data.frame(t(apply(new_DF[c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")], 2, median)), row.names = "Median")
Standard_deviation <- data.frame(t(apply(new_DF[c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")], 2, sd)), row.names = "Standard deviation")
Max <- data.frame(t(apply(new_DF[c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")], 2, max)), row.names = "Max")
Min <- data.frame(t(apply(new_DF[c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")], 2, min)), row.names = "Min")
df <- data.frame(rbind(Mean, Median, Standard_deviation, Max, Min))
df

# Plots
hist(price, xlab = "Price", ylab = "Frequency", main = "Histogram of Price")
boxplot(price ~ new_DF$floors, main = "Price by Number of Floors", col = "red")
boxplot(price ~ new_DF$condition, main = "Price by Condition", col = "blue")
pairs(price ~ sqft_living15, main = "Price vs sqft_living15")
pairs(price ~ sqft_above, main = "Price vs sqft_above")
pairs(price ~ sqft_living, main = "Price vs sqft_living")

# Linear regression model
linear_model <- lm(price ~ sqft_living15 + floors + condition + sqft_above + sqft_living, data = new_DF)
summary(linear_model)

# Residuals plot
plot(fitted(linear_model), resid(linear_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Plot of Residuals vs Fitted Values")

# Diagnostic plots
op <- par(mfrow = c(2, 2))
plot(linear_model)

# Prediction for given values
x1 <- data.frame(
  sqft_living15 = mean(sqft_living15),
  sqft_above = mean(sqft_above),
  sqft_living = mean(sqft_living),
  floors = 2,
  condition = 3
)

prediction_x1 <- predict(linear_model, x1)
prediction_x1
