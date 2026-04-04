# Load required libraries
library(RWeka)   # For the JRip algorithm
library(caret)   # For evaluation metrics
library(dplyr)   # For data manipulation

# Step 1: Load the dataset (in this example, we'll use the "iris" dataset)
data(iris)

# Step 2: Discretize the target variable into 3 bins (low, medium, high)
# You can adjust the bins and labels as needed for your specific problem
iris <- iris %>%
  mutate(Species_class = cut(Sepal.Length, breaks = c(0, 5.5, 6.5, Inf),
                             labels = c("low", "medium", "high")))

# Step 3: Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- createDataPartition(iris$Species_class, p = 0.7, list = FALSE)
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Step 4: Train the JRip classifier
model <- JRip(Species_class ~ Sepal.Width + Petal.Length + Petal.Width, data = train_data)

# Step 5: Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Step 6: Evaluate the classifier using classification metrics
confusion_matrix <- confusionMatrix(predictions, test_data$Species_class)
print(confusion_matrix)

# Step 7: Get accuracy and other metrics from the confusion matrix
accuracy <- confusion_matrix$overall["Accuracy"]
precision <- confusion_matrix$byClass["Precision"]
recall <- confusion_matrix$byClass["Recall"]
f1_score <- confusion_matrix$byClass["F1"]

# Print the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
