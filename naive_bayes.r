# -------------------------------
# Install & load library
# -------------------------------
install.packages("naivebayes")
library(naivebayes)

# -------------------------------
# Create sample dataset
# -------------------------------
# Example: Buying decision
data <- data.frame(
  Age = c("Young","Young","Middle","Old","Old","Old","Middle","Young","Young","Old"),
  Income = c("High","High","High","Medium","Low","Low","Low","Medium","Low","Medium"),
  Student = c("No","No","No","No","Yes","Yes","Yes","No","Yes","Yes"),
  Credit = c("Fair","Excellent","Fair","Fair","Fair","Excellent","Excellent","Fair","Fair","Fair"),
  Buy = c("No","No","Yes","Yes","Yes","No","Yes","No","Yes","Yes")
)

# Convert to factors
data[] <- lapply(data, as.factor)

# -------------------------------
# Train Naive Bayes model
# -------------------------------
model <- naive_bayes(Buy ~ ., data = data)

# View model
print(model)

# -------------------------------
# Make predictions
# -------------------------------
new_data <- data.frame(
  Age = "Young",
  Income = "Medium",
  Student = "Yes",
  Credit = "Fair"
)

prediction <- predict(model, new_data)
prediction

# -------------------------------
# Predict probabilities
# -------------------------------
predict(model, new_data, type = "prob")

