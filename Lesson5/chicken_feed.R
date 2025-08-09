# Question 1
# a) Set working directory
setwd("C:/Users/ThinkPro/Documents/R/test")

# Read CSV file
chicken_data <- read.csv("chicken_feed.csv", header = TRUE)
attach(chicken_data)
chicken_data

# Create a new data frame with weight and feed type
feed_data <- data.frame(weight, feed)

# Summary statistics by feed type
by(feed_data, feed, summary)

# Standard deviation and variance by feed type
tapply(weight, feed, sd, na.rm = TRUE)
tapply(weight, feed, var, na.rm = TRUE)

# b) Remove rows with missing values
chicken_data_clean <- na.omit(chicken_data)
chicken_data_clean

# c) Boxplot with ggplot2
library(ggplot2)
weight_clean <- chicken_data_clean$weight

ggplot(chicken_data_clean, aes(x = feed, y = weight_clean)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(fill = "blue", outlier.color = "black") +
  theme_classic() +
  xlab("Feed Types") +
  ylab("Chicken Weight") +
  theme(axis.text.x = element_text(colour = "black", size = 12)) +
  theme(axis.text.y = element_text(colour = "black", size = 12)) +
  theme(axis.title.x = element_text(colour = "black", size = 15)) +
  theme(axis.title.y = element_text(colour = "black", size = 15))

ggsave("Boxplot.tiff", scale = 1, width = 20, height = 8, units = "cm", dpi = 600)

# Question 2
# c) QQ plots for each feed type

# Function to create QQ plot for each feed type
create_qq_plot <- function(data, feed_type) {
  subset_data <- subset(data, feed == feed_type)
  tiff(paste0("QQ_plot_", feed_type, ".tiff"), res = 600, units = "cm", width = 20, height = 20)
  par(mar = c(5, 5, 4, 3))
  qqnorm(subset_data$weight, lwd = 3, cex.axis = 1.25, cex.lab = 1.5)
  qqline(subset_data$weight, lwd = 1.5, col = "red")
  dev.off()
  return(subset_data$weight)
}

# Generate QQ plots and store weight data
casein_wt     <- create_qq_plot(chicken_data, "casein")
horsebean_wt  <- create_qq_plot(chicken_data, "horsebean")
linseed_wt    <- create_qq_plot(chicken_data, "linseed")
meatmeal_wt   <- create_qq_plot(chicken_data, "meatmeal")
soybean_wt    <- create_qq_plot(chicken_data, "soybean")
sunflower_wt  <- create_qq_plot(chicken_data, "sunflower")

# Shapiro-Wilk normality test
shapiro.test(casein_wt)
shapiro.test(horsebean_wt)
shapiro.test(linseed_wt)
shapiro.test(meatmeal_wt)
shapiro.test(soybean_wt)
shapiro.test(sunflower_wt)

# Bartlett's test for homogeneity of variances
bartlett.test(list(casein_wt, horsebean_wt, linseed_wt, 
                   meatmeal_wt, soybean_wt, sunflower_wt))

# d) ANOVA
anova_model <- lm(weight ~ feed, data = chicken_data_clean)
anova(anova_model)

# e) Tukey's HSD post-hoc test
anova_res <- aov(weight ~ feed, data = chicken_data_clean)
TukeyHSD(anova_res)
