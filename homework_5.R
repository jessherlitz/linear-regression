# Homework 5

# loading data
crime_data <- read.table(
  "uscrime.txt", 
  header = TRUE
)

View(crime_data)

# fitting model
model <- lm(Crime ~ ., data = crime_data)
summary(model)

# creating a test data frame
test_data <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,
  LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120,
  U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)

pred <- predict(
  model, 
  newdata = test_data
)
pred

summary(crime_data$Crime)

#library(ggplot2)
#library(tidyr)

# visualization to compare dataset to test data
comparison <- data.frame(
  Variable = names(crime_data[,-16]),
  Min = sapply(crime_data[,-16], min),
  Mean = sapply(crime_data[,-16], mean),
  Max = sapply(crime_data[,-16], max),
  Test = as.numeric(test_data)
)

comparison$Min_scaled <- 0
comparison$Max_scaled <- 1
comparison$Mean_scaled <- (comparison$Mean - comparison$Min) / (comparison$Max - comparison$Min)
comparison$Test_scaled <- (comparison$Test - comparison$Min) / (comparison$Max - comparison$Min)

ggplot(comparison, aes(x = Variable)) +
  geom_linerange(aes(ymin = Min_scaled, ymax = Max_scaled), color = "gray70", linewidth = 3) +
  geom_point(aes(y = Mean_scaled), color = "green4", size = 3, shape = 16) +
  geom_point(aes(y = Test_scaled), color = "red", size = 3, shape = 17) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Test Data vs Dataset Range",
    subtitle = "Gray = dataset range, Green = dataset mean, Red = test point",
    x = "Predictor",
    y = "Scaled Value (0 = Min, 1 = Max)"
  ) +
  theme_minimal()

# visualizing the coefficients 
crime_scaled <- as.data.frame(scale(crime_data))
model_scaled <- lm(Crime ~ ., data = crime_scaled)
coefs_s <- summary(model_scaled)$coefficients[-1, ]

coef_df2 <- data.frame(
  Variable = rownames(coefs_s),
  Estimate = coefs_s[, 1],
  Pvalue = coefs_s[, 4]
)

coef_df2$Significant <- ifelse(coef_df2$Pvalue < 0.05, "Significant",
                               ifelse(coef_df2$Pvalue < 0.1, "Borderline", "Not Significant"))

ggplot(coef_df2, aes(x = reorder(Variable, Estimate), y = Estimate, fill = Significant)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Standardized Regression Coefficients", x = "Predictor", y = "Standardized Coefficient") +
  theme_minimal()


