---
title: "Decision Tree ID3"
author:
date: "2026-03-12"
output: pdf_document
---


Ouestion:

Take any dataset

Apply the ID3 algorithm

Your results should contain 

1. All the attribute's Entrophy and Gain values

2. Visualization of intermediate steps with values, tree

3. Final tree
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
About Dataset:

The dataset used in the code is a Telecom Customer Churn dataset. It contains information about telecom customers and whether they stopped using the service (churned) or continued using it. The goal of the analysis is to predict churn using customer attributes.

The dataset represents individual customers as rows and their characteristics as columns. Each row describes one customer and the final column (Churn) indicates whether the customer left the service.

Typical columns used in the model include:

• Contract – Type of contract the customer has with the telecom company. Examples: Month-to-month, One year, Two year. Customers with shorter contracts tend to leave more often.

• InternetService – The type of internet connection provided. Examples: DSL, Fiber optic, No internet. Different service types may influence customer satisfaction and churn.

• tenure – The number of months a customer has stayed with the company. Customers with longer tenure usually show lower churn probability.

• MonthlyCharges – The amount the customer pays every month. Higher charges sometimes correlate with churn if customers feel the price is too high.

• PaymentMethod – How the customer pays their bill (credit card, electronic check, etc.). Certain payment methods may be associated with higher churn patterns.

• Churn (Target Variable) – Indicates whether the customer left the service.
Values:

Yes → customer left

No → customer stayed

General characteristics of the dataset:

• It is a supervised classification dataset because the output variable (Churn) is known.
• Most features are categorical, though some are numerical (tenure, MonthlyCharges).
• The dataset is commonly used in machine learning tasks such as decision trees, logistic regression, and clustering.

In this analysis, the dataset is used to build a decision tree model that learns patterns such as:

customers with month-to-month contracts are more likely to churn

customers with longer tenure tend to remain with the company

service type and charges may influence churn behavior

The decision tree helps convert these patterns into clear decision rules, making the results easy to interpret for business decision-making.
```{r}
# Install packages (run once if not installed)
#install.packages("infotheo")
#install.packages("RWeka")
#install.packages("partykit")
#install.packages("dplyr")

# Load libraries
library(infotheo)     # used to compute entropy and information gain
library(rpart)
library(rpart.plot)
library(dplyr)
```

```{r}
# Load dataset
data <- read.csv("churn.csv")

# Convert target variable to categorical
data$Churn <- as.factor(data$Churn)

print("Dataset loaded successfully.")
print("Goal: Predict whether a customer will churn (leave) or not.")

# Display dataset structure
str(data)
```

```{r}
head(data)
```
```{r}
# Entropy of Target Variable


churn_entropy <- entropy(discretize(data$Churn))

print(paste("Entropy of Churn:", churn_entropy))
print("Interpretation: Higher entropy means churn outcomes are mixed.")

```


```{r}
# Entropy of Target Variable

contract_entropy <- entropy(discretize(data$Contract))
internet_entropy <- entropy(discretize(data$InternetService))
tenure_entropy <- entropy(discretize(data$tenure))
charges_entropy <- entropy(discretize(data$MonthlyCharges))
payment_entropy <- entropy(discretize(data$PaymentMethod))

print(paste("Entropy of Contract:", contract_entropy))
print(paste("Entropy of InternetService:", internet_entropy))
print(paste("Entropy of tenure:", tenure_entropy))
print(paste("Entropy of MonthlyCharges:", charges_entropy))
print(paste("Entropy of PaymentMethod:", payment_entropy))

print("Interpretation: These values describe how varied each attribute is.")
```
```{r}
gain_contract <- mutinformation(discretize(data$Contract), discretize(data$Churn))
gain_internet <- mutinformation(discretize(data$InternetService), discretize(data$Churn))
gain_tenure <- mutinformation(discretize(data$tenure), discretize(data$Churn))
gain_charges <- mutinformation(discretize(data$MonthlyCharges), discretize(data$Churn))
gain_payment <- mutinformation(discretize(data$PaymentMethod), discretize(data$Churn))

print(paste("Information Gain - Contract:", gain_contract))
print(paste("Information Gain - InternetService:", gain_internet))
print(paste("Information Gain - tenure:", gain_tenure))
print(paste("Information Gain - MonthlyCharges:", gain_charges))
print(paste("Information Gain - PaymentMethod:", gain_payment))

print("Interpretation: The attribute with highest gain will become the root node.")

```

```{r}
# Split the dataset (70% training, 30% testing)

set.seed(123)

index <- sample(1:nrow(data), 0.7 * nrow(data))

train_data <- data[index, ]
test_data <- data[-index, ]

print("Dataset has been split into training and testing sets.")
print(paste("Training rows:", nrow(train_data)))
print(paste("Testing rows:", nrow(test_data)))
```

```{r}
# Train the decision tree using rpart
model <- rpart(
  Churn ~ Contract + InternetService + tenure + MonthlyCharges + PaymentMethod,
  data = train_data,
  method = "class"
)

print("Decision tree model has been trained.")
print("The algorithm automatically chooses the best attribute to split the data.")
```

```{r}
# Plot the decision tree
rpart.plot(model)

print("Decision tree visualization displayed.")
print("The root node represents the most important attribute for predicting churn.")
```

```{r}
# Display the structure of the tree
print(model)

print("This output shows the decision rules generated by the tree.")
print("Attributes near the top influence churn predictions more strongly.")
```

```{r}
# Predict churn for the testing dataset
predictions <- predict(model, test_data, type = "class")

print("Predictions have been generated for the testing dataset.")
print(predictions )
```

```{r}
# Confusion matrix
conf_matrix <- table(predictions, test_data$Churn)

print("Confusion Matrix:")
print(conf_matrix)

print("This matrix compares predicted values with the actual churn values.")
```

```{r}
# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

print(paste("Model Accuracy:", accuracy))

print("Accuracy shows the proportion of correctly predicted customers.")
```

```{r}
print("Final Interpretation:")
print("The decision tree helps identify which factors influence customer churn.")
print("Contract type often appears near the root because it strongly affects churn.")
print("Customers with month-to-month contracts generally have a higher chance of leaving.")


```