# Homework 5

# loading data
crime_data <- read.table(
  "uscrime.txt", 
  header = TRUE
)

View(crime_data)

model <- lm(Crime ~ ., data = crime_data)
