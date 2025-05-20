# Rename loaded datasets for consistency
train <- train_set
valid <- valid_set
test  <- test_set

# Check column names to find the complaints column
names(train)

# Calculate threshold using correct column name
threshold <- median(train$Total.Complaints, na.rm = TRUE)

# Use correct column name with underscore
threshold <- median(train$Total_Complaints, na.rm = TRUE)

# Create binary classification target
train$high_reporter <- ifelse(train$Total_Complaints > threshold, 1, 0)
valid$high_reporter <- ifelse(valid$Total_Complaints > threshold, 1, 0)
test$high_reporter  <- ifelse(test$Total_Complaints > threshold, 1, 0)

#LOGIT MODEL
library(caret)    # for confusionMatrix / accuracy
library(pROC)     # for AUC and ROC

logit_model <- glm(high_reporter ~ Race + Religion + Population, 
                   data = train, family = "binomial")

logit_probs_valid <- predict(logit_model, newdata = valid, type = "response")
logit_pred_valid <- ifelse(logit_probs_valid > 0.5, 1, 0)

# Accuracy
confusionMatrix(as.factor(logit_pred_valid), as.factor(valid$high_reporter))

# ROC and AUC
roc_valid <- roc(valid$high_reporter, logit_probs_valid)
plot(roc_valid, col = "blue", main = "ROC Curve - Logistic Model (Validation)")
auc(roc_valid)

#PROBIT

probit_model <- glm(high_reporter ~ Race + Religion + Population,
                    data = train, family = binomial(link = "probit"))
probit_probs_valid <- predict(probit_model, newdata = valid, type = "response")
probit_pred_valid <- ifelse(probit_probs_valid > 0.5, 1, 0)
confusionMatrix(as.factor(probit_pred_valid), as.factor(valid$high_reporter))
roc_probit_valid <- roc(valid$high_reporter, probit_probs_valid)
plot(roc_probit_valid, col = "purple", main = "ROC Curve - Probit Model (Validation)")
auc(roc_probit_valid)


logit_model <- glm(high_reporter ~ Race + Religion + Population, 
                   data = train, family = "binomial")

# Predict on validation set
logit_probs_valid <- predict(logit_model, newdata = valid, type = "response")
logit_pred_valid <- ifelse(logit_probs_valid > 0.5, 1, 0)

# Accuracy
library(caret)
confusionMatrix(as.factor(logit_pred_valid), as.factor(valid$high_reporter))

# AUC and ROC
library(pROC)
roc_valid_logit <- roc(valid$high_reporter, logit_probs_valid)
plot(roc_valid_logit, col = "blue", main = "ROC Curve - Logistic Model (Validation)")
auc(roc_valid_logit)


probit_model <- glm(high_reporter ~ Race + Religion + Population, 
                    data = train, family = binomial(link = "probit"))

# Predict on validation set
probit_probs_valid <- predict(probit_model, newdata = valid, type = "response")
probit_pred_valid <- ifelse(probit_probs_valid > 0.5, 1, 0)

# Accuracy
confusionMatrix(as.factor(probit_pred_valid), as.factor(valid$high_reporter))

# AUC and ROC
roc_valid_probit <- roc(valid$high_reporter, probit_probs_valid)
plot(roc_valid_probit, col = "purple", main = "ROC Curve - Probit Model (Validation)")
auc(roc_valid_probit)

library(e1071)
svm_model <- svm(high_reporter ~ Race + Religion + Population, 
                 data = train, probability = TRUE)
svm_pred_valid <- predict(svm_model, newdata = valid, probability = TRUE)

# Extract probabilities for ROC
svm_probs_valid <- attr(svm_pred_valid, "probabilities")[, "1"]
svm_pred_valid <- predict(svm_model, newdata = valid, probability = TRUE)
length(svm_pred_valid)         # Should be 215
nrow(valid)                    # Should be 215

valid_clean <- na.omit(valid)
nrow(valid_clean)  # Should now be 200
svm_pred_valid <- predict(svm_model, newdata = valid_clean, probability = TRUE)
svm_probs_valid <- attr(svm_pred_valid, "probabilities")[, "1"]


# Recalculate binary outcome after cleaning
threshold <- median(train$Total_Complaints, na.rm = TRUE)

valid_clean$high_reporter <- ifelse(valid_clean$Total_Complaints > threshold, 1, 0)

svm_model <- svm(high_reporter ~ Race + Religion + Population,
                 data = train, probability = TRUE)

svm_pred_valid <- predict(svm_model, newdata = valid_clean, probability = TRUE)
svm_probs_valid <- attr(svm_pred_valid, "probabilities")[, "1"]
confusionMatrix(factor(svm_pred_valid, levels = c(0, 1)),
                factor(valid_clean$high_reporter, levels = c(0, 1)))

roc_svm <- roc(valid_clean$high_reporter, svm_probs_valid)
plot(roc_svm, col = "orange", main = "ROC Curve - SVM (Validation)")
auc(roc_svm)

str(attr(svm_pred_valid, "probabilities"))
svm_model <- svm(high_reporter ~ Race + Religion + Population,
                 data = train, probability = TRUE)
svm_pred_valid <- predict(svm_model, newdata = valid_clean, probability = TRUE)
svm_probs_valid <- attr(svm_pred_valid, "probabilities")[, "1"]

.rs.restartR()
library(e1071)
library(pROC)
library(caret)
train <- train_set
valid <- valid_set
valid_clean <- na.omit(valid)

# Recreate binary outcome for valid_clean
threshold <- median(train$Total_Complaints, na.rm = TRUE)
valid_clean$high_reporter <- ifelse(valid_clean$Total_Complaints > threshold, 1, 0)
train$high_reporter <- ifelse(train$Total_Complaints > threshold, 1, 0)
svm_model <- svm(high_reporter ~ Race + Religion + Population,
                 data = train,
                 probability = TRUE)
svm_pred_valid <- predict(svm_model, newdata = valid_clean, probability = TRUE)
svm_probs_valid <- attr(svm_pred_valid, "probabilities")[, "1"]
library(caret)

confusionMatrix(factor(svm_pred_valid, levels = c(0, 1)),
                factor(valid_clean$high_reporter, levels = c(0, 1)))
table(svm_pred_valid)
train$high_reporter <- as.factor(ifelse(train$Total_Complaints > threshold, 1, 0))
valid_clean$high_reporter <- as.factor(ifelse(valid_clean$Total_Complaints > threshold, 1, 0))
library(e1071)

svm_model <- svm(high_reporter ~ Race + Religion + Population,
                 data = train,
                 probability = TRUE)
svm_pred_valid <- predict(svm_model, newdata = valid_clean, probability = TRUE)
svm_probs_valid <- attr(svm_pred_valid, "probabilities")[, "1"]
table(svm_pred_valid)  # Should show a mix of 0s and 1s
library(caret)

confusionMatrix(svm_pred_valid, valid_clean$high_reporter)
library(pROC)

roc_svm <- roc(valid_clean$high_reporter, svm_probs_valid)
plot(roc_svm, col = "orange", main = "ROC Curve - SVM (Validation)")
auc(roc_svm)

library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
tree_model <- rpart(high_reporter ~ Race + Religion + Population,
                    data = train,
                    method = "class",
                    control = rpart.control(cp = 0.01))  # You can tune cp if needed
rpart.plot(tree_model, type = 3, extra = 104,
           fallen.leaves = TRUE,
           main = "Classification Tree - High Reporter")

# Predict on validation data
tree_pred_valid <- predict(tree_model, newdata = valid_clean, type = "class")
tree_probs_valid <- predict(tree_model, newdata = valid_clean, type = "prob")[, 2]

# Confusion Matrix
confusionMatrix(tree_pred_valid, valid_clean$high_reporter)

# ROC and AUC
roc_tree <- roc(valid_clean$high_reporter, tree_probs_valid)
plot(roc_tree, col = "darkgreen", main = "ROC Curve - Classification Tree")
auc(roc_tree)

library(randomForest)
install.packages("randomForest")
set.seed(123)  # for reproducibility
rf_model <- randomForest(as.factor(high_reporter) ~ Race + Religion + Population,
                         data = train,
                         ntree = 100,
                         importance = TRUE)
install.packages("randomForest", type = "source")
library(randomForest)

set.seed(123)
rf_model <- randomForest(as.factor(high_reporter) ~ Race + Religion + Population,
                         data = train,
                         ntree = 100,
                         importance = TRUE)
library(caret)
# Confusion matrix
confusionMatrix(rf_pred_valid, valid_clean$high_reporter)

# ROC Curve and AUC
library(pROC)
roc_rf <- roc(valid_clean$high_reporter, rf_probs_valid)
plot(roc_rf, col = "darkred", main = "ROC Curve - Random Forest")
auc(roc_rf)


test_clean <- na.omit(test)
test_clean$high_reporter <- ifelse(test_clean$Total_Complaints > threshold, 1, 0)
rf_pred_test <- predict(rf_model, newdata = test_clean)
rf_probs_test <- predict(rf_model, newdata = test_clean, type = "prob")[, 2]
library(caret)
confusionMatrix(rf_pred_test, as.factor(test_clean$high_reporter))
library(pROC)
roc_rf_test <- roc(test_clean$high_reporter, rf_probs_test)
plot(roc_rf_test, col = "darkred", # Create a summary data frame for model performance
     model_perf <- data.frame(
       Model = c("Logit", "Probit", "SVM", "Classification Tree", "Random Forest"),
       Accuracy_Validation = c(0.8419, 0.8419, 0.8250, 0.7600, 0.8650),
       AUC_Validation = c(0.8967, 0.8419, 0.8950, 0.6990, 0.8592),
       Accuracy_Test = c(NA, NA, NA, NA, 0.8564),  # Only RF used on test set
       AUC_Test = c(NA, NA, NA, NA, 0.8994)
     )
     
     print(model_perf)
     = "ROC Curve - Final Test Set")
auc(roc_rf_test)


#ACCURACY CHARTS IN AND OUT OF SAMPLE
# Create a summary data frame for model performance
model_perf <- data.frame(
  Model = c("Logit", "Probit", "SVM", "Classification Tree", "Random Forest"),
  Accuracy_Validation = c(0.8419, 0.8419, 0.8250, 0.7600, 0.8650),
  AUC_Validation = c(0.8967, 0.8419, 0.8950, 0.6990, 0.8592),
  Accuracy_Test = c(NA, NA, NA, NA, 0.8564),  # Only RF used on test set
  AUC_Test = c(NA, NA, NA, NA, 0.8994)
)

print(model_perf)

library(ggplot2)

# Melt the table to long format for ggplot (optional)
library(reshape2)
perf_long <- melt(model_perf, id.vars = "Model")

# Plot AUC only (filtering)
ggplot(subset(perf_long, grepl("AUC", variable)), aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "AUC Comparison (Validation & Test Sets)", y = "AUC", x = "Model") +
  theme_minimal()

# Use unique quantiles and handle ties
q_breaks <- unique(quantile(train$Total_Complaints, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))

# Check that we have at least 3 distinct break points
if (length(q_breaks) < 4) {
  stop("Not enough unique values to form three categories. Try using fewer bins or inspecting your data.")
}

# Proceed with clean breaks
train$report_level <- cut(train$Total_Complaints,
                          breaks = q_breaks,
                          labels = c("Low", "Medium", "High"),
                          include.lowest = TRUE)
train$report_level <- as.factor(train$report_level)

# Repeat on validation using training breaks
valid$report_level <- cut(valid$Total_Complaints,
                          breaks = q_breaks,
                          labels = c("Low", "Medium", "High"),
                          include.lowest = TRUE)
valid$report_level <- as.factor(valid$report_level)

multi_tree_model <- rpart(report_level ~ Race + Religion + Population,
                          data = train,
                          method = "class")

rpart.plot(multi_tree_model, type = 3, extra = 104, main = "Multi-Class Classification Tree")

# Proceed with clean breaks
train$report_level <- cut(train$Total_Complaints,
                          breaks = q_breaks,
                          labels = c("Low", "Medium", "High"),
                          include.lowest = TRUE)
train$report_level <- as.factor(train$report_level)

# Repeat on validation using training breaks
valid$report_level <- cut(valid$Total_Complaints,
                          breaks = q_breaks,
                          labels = c("Low", "Medium", "High"),
                          include.lowest = TRUE)
valid$report_level <- as.factor(valid$report_level)


# Accuracy values (replace with your actual results)
model_perf_multi <- data.frame(
  Model = c("Random Forest", "Random Forest", "Classification Tree", "Classification Tree"),
  Dataset = c("Validation", "Test", "Validation", "Test"),
  Accuracy = c(0.80, 0.78, 0.75, 0.71)
)

# Load required libraries
library(ggplot2)

# Plot
ggplot(model_perf_multi, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Multi-Class Accuracy: Random Forest vs Classification Tree") +
  ylim(0, 1) +
  theme_minimal()

# Load required libraries
library(ggplot2)
library(reshape2)

# Create performance data frame
model_perf <- data.frame(
  Model = c("Logit", "Probit", "SVM", "Classification Tree", "Random Forest"),
  Accuracy_Validation = c(0.8419, 0.8419, 0.8250, 0.7600, 0.8650),
  Accuracy_Test = c(NA, NA, NA, NA, 0.8564)  # Only Random Forest was tested
)

# Convert to long format
perf_long <- melt(model_perf, id.vars = "Model", variable.name = "Dataset", value.name = "Accuracy")

# Remove NA values (test accuracies for models not evaluated)
perf_long <- na.omit(perf_long)

# Rename dataset labels
perf_long$Dataset <- gsub("Accuracy_", "", perf_long$Dataset)

# Plot
ggplot(perf_long, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylim(0, 1) +
  labs(title = "In-Sample vs Out-of-Sample Accuracy",
       x = "Model", y = "Accuracy") +
  theme_minimal()



# Load required libraries
library(randomForest)
library(ggplot2)

# Generate breaks using quantiles (we need 4 points for 3 groups)
q_breaks <- unique(quantile(train$Total_Complaints, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))

# Check for sufficient unique breakpoints
if (length(q_breaks) < 4) {
  stop("❌ Not enough unique values to form three categories. Try fewer bins or inspect Total_Complaints distribution.")
}

# Proceed with multi-class labels if enough breaks exist
train$report_level <- cut(train$Total_Complaints, breaks = q_breaks,
                          labels = c("Low", "Medium", "High"), include.lowest = TRUE)
valid$report_level <- cut(valid$Total_Complaints, breaks = q_breaks,
                          labels = c("Low", "Medium", "High"), include.lowest = TRUE)
test$report_level <- cut(test$Total_Complaints, breaks = q_breaks,
                         labels = c("Low", "Medium", "High"), include.lowest = TRUE)

# Convert to factors
train$report_level <- as.factor(train$report_level)
valid$report_level <- as.factor(valid$report_level)
test$report_level  <- as.factor(test$report_level)

# Train multi-class Random Forest
set.seed(123)
rf_multi_model <- randomForest(report_level ~ Race + Religion + Population,
                               data = train, ntree = 100)

# Predict
rf_pred_valid <- predict(rf_multi_model, newdata = valid)
rf_pred_test <- predict(rf_multi_model, newdata = test)

# Calculate Accuracy
acc_valid <- mean(rf_pred_valid == valid$report_level)
acc_test <- mean(rf_pred_test == test$report_level)

# Summary table
model_perf_multi <- data.frame(
  Model = c("Random Forest", "Random Forest"),
  Dataset = c("Validation", "Test"),
  Accuracy = c(acc_valid, acc_test)
)

# Plot accuracy
ggplot(model_perf_multi, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0, 1) +
  labs(title = "Multi-Class Accuracy: Random Forest",
       x = "Model", y = "Accuracy") +
  theme_minimal()


# Check the range to pick sensible breakpoints
summary(train$Total_Complaints)

# Set manual breaks based on your data distribution
# Example: complaints 0–2 → Low, 3–5 → Medium, 6+ → High
train$report_level <- cut(train$Total_Complaints,
                          breaks = c(-Inf, 2, 5, Inf),
                          labels = c("Low", "Medium", "High"),
                          right = TRUE)

valid$report_level <- cut(valid$Total_Complaints,
                          breaks = c(-Inf, 2, 5, Inf),
                          labels = c("Low", "Medium", "High"),
                          right = TRUE)

test$report_level <- cut(test$Total_Complaints,
                         breaks = c(-Inf, 2, 5, Inf),
                         labels = c("Low", "Medium", "High"),
                         right = TRUE)

# Convert to factors
train$report_level <- as.factor(train$report_level)
valid$report_level <- as.factor(valid$report_level)
test$report_level  <- as.factor(test$report_level)

# Train multi-class Random Forest
library(randomForest)
set.seed(123)
rf_multi_model <- randomForest(report_level ~ Race + Religion + Population,
                               data = train, ntree = 100)

# Predict on validation and test
rf_pred_valid <- predict(rf_multi_model, newdata = valid)
rf_pred_test <- predict(rf_multi_model, newdata = test)

# Accuracy
acc_valid <- mean(rf_pred_valid == valid$report_level)
acc_test <- mean(rf_pred_test == test$report_level)

# Summary for chart
model_perf_multi <- data.frame(
  Model = c("Random Forest", "Random Forest"),
  Dataset = c("Validation", "Test"),
  Accuracy = c(acc_valid, acc_test)
)

# Plot
library(ggplot2)
ggplot(model_perf_multi, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0, 1) +
  labs(title = "Multi-Class Accuracy: Random Forest",
       x = "Model", y = "Accuracy") +
  theme_minimal()