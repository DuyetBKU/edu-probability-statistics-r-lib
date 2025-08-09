library(readr)
setwd("C:/Users/ThinkPro/Documents/R/win-library/4.1")

# Load dataset
grade <- read.csv("student_scores.csv", header = TRUE)
View(grade)

# Remove missing values
is.na(grade)
grade <- na.omit(grade)
grade

# Extract variables
G1 <- grade$G1
G2 <- grade$G2
G3 <- grade$G3
studytime <- grade$studytime
failures <- grade$failures
absences <- grade$absences
paid <- grade$paid
sex <- grade$sex

# Summary statistics
Mean <- data.frame(t(apply(grade[c("G1","G2","G3","studytime","failures","absences")], 2, mean)), row.names = "Mean")
Median <- data.frame(t(apply(grade[c("G1","G2","G3","studytime","failures","absences")], 2, median)), row.names = "Median")
Standard_deviation <- data.frame(t(apply(grade[c("G1","G2","G3","studytime","failures","absences")], 2, sd)), row.names = "Standard deviation")
Max <- data.frame(t(apply(grade[c("G1","G2","G3","studytime","failures","absences")], 2, max)), row.names = "Max")
Min <- data.frame(t(apply(grade[c("G1","G2","G3","studytime","failures","absences")], 2, min)), row.names = "Min")

df <- data.frame(rbind(Mean, Median, Standard_deviation, Max, Min))
df

# Frequency tables
data.frame(table(sex))
data.frame(table(paid))

# Histograms
hist(G1, main = "Distribution of First Semester Grades", xlab = "Grade", ylab = "Frequency")
hist(G2, main = "Distribution of Second Semester Grades", xlab = "Grade", ylab = "Frequency")
hist(G3, main = "Distribution of Final Grades", xlab = "Grade", ylab = "Frequency")
hist(studytime, main = "Distribution of Study Time", xlab = "Hours", ylab = "Frequency")
hist(failures, main = "Distribution of Number of Failures", xlab = "Number of Failures", ylab = "Frequency")
hist(absences, main = "Distribution of Number of Absences", xlab = "Number of Absences", ylab = "Frequency")

# Pie chart
pie(table(sex), main = "Number of Male and Female Students")

# Boxplots
boxplot(G3 ~ studytime, main = "Final Grade by Study Time", col = "red")
boxplot(G3 ~ failures, main = "Final Grade by Number of Failures", col = "orange")
boxplot(G3 ~ absences, main = "Final Grade by Number of Absences", col = "yellow")
boxplot(G3 ~ paid, main = "Final Grade by Extra Class Participation", col = "green")
boxplot(G3 ~ sex, main = "Final Grade by Gender", col = "blue")

# Regression analysis
analysis <- lm(G3 ~ G1 + G2 + studytime + failures + absences + paid + sex)
anova(analysis)
summary(analysis)

# Residual plots
plot(fitted(analysis), resid(analysis), 
     xlab = "Fitted values", 
     ylab = "Residuals", 
     main = "Plot of Residuals vs Fitted Values")

op = par(mfrow = c(2, 2))
plot(analysis)

# Reduced model
M1 <- lm(G3 ~ G1 + G2 + absences)
anova(M1)

# Prediction for average values
x1 <- data.frame(G1 = mean(G1), G2 = mean(G2), absences = mean(absences))
predicted_x1 <- predict(M1, x1)
predicted_x1
