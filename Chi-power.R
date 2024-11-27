# Load necessary library
library(datasets)

# Load the dataset
data(mtcars)

# Inspect the dataset
str(mtcars)

# Convert 'am' to a factor (automatic = 0, manual = 1)
mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))

# Fit logistic regression model
# Predicting 'am' using 'mpg' (miles per gallon) and 'hp' (horsepower)
logistic_model <- glm(am ~ mpg + hp, data = mtcars, family = binomial)

# View model summary
summary(logistic_model)

# Make predictions
predicted_probabilities <- predict(logistic_model, type = "response")  # Predicted probabilities

# Create a confusion matrix
predicted_classes <- ifelse(predicted_probabilities > 0.5, "Manual", "Automatic")
table(Predicted = predicted_classes, Actual = mtcars$am)

# Visualize the results (optional)
plot(mtcars$mpg, predicted_probabilities, col = ifelse(mtcars$am == "Manual", "blue", "red"),
     main = "Logistic Regression Predictions", xlab = "Miles Per Gallon", ylab = "Probability of Manual")

