# STEP 1.1: INSTALLING DATA

df <- table13

#STEP 1.2: CLEANING THE DATA: we are adding a variable called Total Complaints.
library(dplyr)

# Start from your original dataframe
df_clean <- df

# 1. Convert 'Population' to numeric
df_clean <- df_clean %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))

# 2. Create 'Total_Complaints' by summing quarterly columns
df_clean <- df_clean %>%
  mutate(
    Total_Complaints = rowSums(select(., X1st.quarter, X2nd.quarter, X3rd.quarter, X4th.quarter), na.rm = TRUE)
  )

# 3. Drop rows with missing Population or Total_Complaints
df_clean <- df_clean %>%
  filter(!is.na(Population) & !is.na(Total_Complaints))

# 4. Convert 'Gender' to a factor
df_clean <- df_clean %>%
  mutate(Gender = as.factor(Gender))

# 5. (Optional) Reset row names to default
rownames(df_clean) <- NULL

# STEP 1.3: SPLITTING DATA

# 6. Split the data into training (70%), validation (15%), and testing (15%) sets
set.seed(123)  # for reproducibility
n <- nrow(df_clean)
indices <- sample(1:n)
train_size <- floor(0.7 * n)
valid_size <- floor(0.15 * n)

train_indices <- indices[1:train_size]
valid_indices <- indices[(train_size + 1):(train_size + valid_size)]
test_indices  <- indices[(train_size + valid_size + 1):n]

train_set <- df_clean[train_indices, ]
valid_set <- df_clean[valid_indices, ]
test_set  <- df_clean[test_indices, ]

# 7. Save the datasets to CSV files (optional)
write.csv(train_set, "train_set.csv", row.names = FALSE)
write.csv(valid_set, "valid_set.csv", row.names = FALSE)
write.csv(test_set, "test_set.csv", row.names = FALSE)


#Extra Step --> Initial exploratory research about our data
install.packages("ggplot2") 
            

library(ggplot2)

ggplot(train_set, aes(x = Total_Complaints)) +
  geom_histogram(binwidth = 5, fill = "#0073C2FF") +
  coord_cartesian(xlim = c(0, quantile(train_set$Total_Complaints, 0.99))) +
  labs(
    title = "Distribution of Total Hate Crime Complaints",
    x = "Total Complaints",
    y = "Frequency"
  ) +
  theme_minimal()



# EXPLORATORY RESEARCH 2

library(dplyr)


# Define cutoffs for both variables
cutoff_total <- quantile(train_set$Total_Complaints, 0.993)
cutoff_so <- quantile(train_set$Sexual.orientation, 0.996)

# Filter dataset to remove extreme outliers
filtered_data <- train_set %>%
  filter(Total_Complaints <= cutoff_total,
         Sexual.orientation <= cutoff_so)

# Plot without outliers
ggplot(filtered_data, aes(x = Sexual.orientation, y = Total_Complaints)) +
  geom_point(alpha = 0.5, color = "brown3") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Total Complaints vs. Sexual Orientation (Without Outliers)",
    x = "Sexual Orientation Complaints",
    y = "Total Complaints"
  ) +
  theme_minimal()



# STEP 3: Correlation matrix

# Select only numeric columns from the training set
numeric_vars <- train_set %>%
  select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# View correlation of all variables with the target 'Total_Complaints'
cor_matrix["Total_Complaints", ]

# EXTRA PLOT FOR VISUALS
install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix,
         method = "color",         # color-coded tiles
         type = "lower",           # lower triangle only
         addCoef.col = "black",    # add correlation values
         tl.col = "black",         # label color
         tl.cex = 0.8,             # label size
         number.cex = 0.7,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "Correlation Matrix of Numeric Variables",
         mar = c(0,0,1,0))          # space for title



#STEP 4: Bivariate Regression Modeling (Sexual Orentiation)

# Step 4.1: Simpler Regression
# Linear model
model_lin <- lm(Total_Complaints ~ Sexual.orientation, data = train_set)

# Predict on validation set
pred_lin <- predict(model_lin, newdata = valid_set)

# Compute validation RMSE
rmse_lin <- sqrt(mean((valid_set$Total_Complaints - pred_lin)^2))
print(paste("Validation RMSE (Linear):", round(rmse_lin, 2)))

# Summary of the model
summary(model_lin)

# Step 4.2: Polynomial (Quadratic) Model
# Polynomial model
model_poly <- lm(Total_Complaints ~ Sexual.orientation + I(Sexual.orientation^2), data = train_set)

# Predict on validation set
pred_poly <- predict(model_poly, newdata = valid_set)

# Compute validation RMSE
rmse_poly <- sqrt(mean((valid_set$Total_Complaints - pred_poly)^2))
print(paste("Validation RMSE (Polynomial):", round(rmse_poly, 2)))

# Summary
summary(model_poly)


# Step 4.3:  Plot Both Models

library(ggplot2)

ggplot(train_set, aes(x = Sexual.orientation, y = Total_Complaints)) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +
  labs(title = "Bivariate Regression Models",
       subtitle = "Blue = Linear | Red = Polynomial",
       x = "Sexual Orientation (counts)",
       y = "Total Hate Crime Complaints")

# INTERPRETATION: "We began with a bivariate regression using Sexual.orientation as the sole predictor of total reported hate crimes. The variable demonstrated a very strong positive linear relationship with the target (R² = 0.9287). Adding a quadratic term slightly improved in-sample fit (R² = 0.9314), but out-of-sample RMSE increased, indicating minor overfitting. The linear model was ultimately preferred due to its simplicity and better validation performance."

# I have redo the graph since it had some outliers, so what I did I have created a filtered version of the train data without the outliers and create the same plot from this data.

# Define an upper threshold (e.g., 99th percentile)
threshold <- quantile(train_set$Sexual.orientation, 0.99)

# Create a filtered version of the training set just for plotting
train_filtered <- train_set %>%
  filter(Sexual.orientation <= threshold)


ggplot(train_filtered, aes(x = Sexual.orientation, y = Total_Complaints)) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +
  labs(title = "Bivariate Regression (Zoomed In)",
       subtitle = "Blue = Linear | Red = Polynomial",
       x = "Sexual Orientation (counts, filtered)",
       y = "Total Hate Crime Complaints")


#STEP 4.B: Bivariate Regression Modeling (Religion)

# Step 4.1: Simpler Regression

# Linear model: Religion as predictor
model_lin_religion <- lm(Total_Complaints ~ Religion, data = train_set)

# Predictions
pred_lin_religion <- predict(model_lin_religion, newdata = valid_set)

# RMSE
rmse_lin_religion <- sqrt(mean((valid_set$Total_Complaints - pred_lin_religion)^2))
print(paste("Validation RMSE (Linear, Religion):", round(rmse_lin_religion, 2)))

# Summary
summary(model_lin_religion)


# Step 4.2: Polynomial (Quadratic) Model

# Polynomial model: Religion + Religion²
model_poly_religion <- lm(Total_Complaints ~ Religion + I(Religion^2), data = train_set)

# Predictions
pred_poly_religion <- predict(model_poly_religion, newdata = valid_set)

# RMSE
rmse_poly_religion <- sqrt(mean((valid_set$Total_Complaints - pred_poly_religion)^2))
print(paste("Validation RMSE (Polynomial, Religion):", round(rmse_poly_religion, 2)))

# Summary
summary(model_poly_religion)

# Step 4.2.2: I am going to create again a filtered data without outliers so the same thing doesn't happen again

# Set 99th percentile threshold for Religion
threshold_religion <- quantile(train_set$Religion, 0.99)

# Filter training set for plotting
train_filtered_religion <- train_set %>%
  filter(Religion <= threshold_religion)


# Step 4.3:  Plot Both Models

ggplot(train_filtered_religion, aes(x = Religion, y = Total_Complaints)) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +
  labs(title = "Bivariate Regression Models (Religion)",
       subtitle = "Blue = Linear | Red = Polynomial",
       x = "Religion (counts, filtered)",
       y = "Total Hate Crime Complaints")


# INTERPRETATION: "Using Religion as the predictor for total hate crime complaints produced a good linear model (R² = 0.7861), but a second-order polynomial significantly improved both in-sample fit (R² = 0.8377) and validation performance (RMSE reduced from 9.96 to 9.00). The quadratic term was statistically significant (p < 0.001), and visually captured the curvature observed in the data at higher values of religion-related incidents."

# STEP 4 - C: Regularization of the Bivariate Model

# STEP 4.C.1: Prepare the Design Matrix with Linear and Polynomial Terms

# Make sure glmnet is installed and loaded
install.packages("glmnet")  # only if not already installed
library(glmnet)

# Design matrix with x and x^2 (remove intercept column)
x_train_bi <- model.matrix(~ Sexual.orientation + I(Sexual.orientation^2), data = train_set)[, -1]
x_valid_bi <- model.matrix(~ Sexual.orientation + I(Sexual.orientation^2), data = valid_set)[, -1]

# Response variable
y_train_bi <- train_set$Total_Complaints
y_valid_bi <- valid_set$Total_Complaints

# STEP 4.C.2: Ridge Regression (alpha = 0)

# Ridge regression with cross-validation
ridge_bi_cv <- cv.glmnet(x_train_bi, y_train_bi, alpha = 0)

# Best lambda
best_lambda_ridge_bi <- ridge_bi_cv$lambda.min
print(paste("Best lambda (Ridge):", best_lambda_ridge_bi))

# Predict on validation
pred_ridge_bi <- predict(ridge_bi_cv, s = best_lambda_ridge_bi, newx = x_valid_bi)

# RMSE
rmse_ridge_bi <- sqrt(mean((y_valid_bi - pred_ridge_bi)^2))
print(paste("Validation RMSE (Ridge - bivariate):", round(rmse_ridge_bi, 2)))


# STEP 4.C.2: Lasso Regression (alpha = 1)

# Lasso regression with cross-validation
lasso_bi_cv <- cv.glmnet(x_train_bi, y_train_bi, alpha = 1)

# Best lambda
best_lambda_lasso_bi <- lasso_bi_cv$lambda.min
print(paste("Best lambda (Lasso):", best_lambda_lasso_bi))

# Predict on validation
pred_lasso_bi <- predict(lasso_bi_cv, s = best_lambda_lasso_bi, newx = x_valid_bi)

# RMSE
rmse_lasso_bi <- sqrt(mean((y_valid_bi - pred_lasso_bi)^2))
print(paste("Validation RMSE (Lasso - bivariate):", round(rmse_lasso_bi, 2)))


# INTERPREATION: "To regularize the bivariate polynomial model, we applied ridge and lasso regression using cross-validation to tune the penalty parameter λ. The lasso model outperformed ridge (Validation RMSE: 6.60 vs. 8.26), showing that it effectively reduced overfitting by shrinking less useful coefficients. Lasso likely reduced or zeroed out the polynomial term, improving generalization."

# STEP 4.D.1: Preparing 

install.packages("mgcv")  # Run this only once
library(mgcv)             # Run this every time you open RStudio


# STEP 4.D.2: Fit the GAM Model

# Fit a Generalized Additive Model using a spline on Sexual.orientation
model_gam <- gam(Total_Complaints ~ s(Sexual.orientation), data = train_set)

# View model summary
summary(model_gam)


# STEP 4.D.3: Predict and Compute RMSE on Validation Set

# Predict on validation set
pred_gam <- predict(model_gam, newdata = valid_set)

# Compute RMSE
rmse_gam <- sqrt(mean((valid_set$Total_Complaints - pred_gam)^2))
print(paste("Validation RMSE (GAM):", round(rmse_gam, 2)))


#STEP 4.D.4: Plot the Smooth Term

plot(model_gam, shade = TRUE, main = "GAM: Spline for Sexual.orientation")


# STEP 4.D.5: Optional: Try Quasi-Poisson (if counts are skewed and overdispersed)

model_qp <- glm(Total_Complaints ~ Sexual.orientation, family = quasipoisson, data = train_set)
summary(model_qp)

pred_qp <- predict(model_qp, newdata = valid_set, type = "response")
rmse_qp <- sqrt(mean((valid_set$Total_Complaints - pred_qp)^2))
print(paste("Validation RMSE (Quasi-Poisson):", round(rmse_qp, 2)))


# INTERPRETATION: "We modeled the bivariate relationship using a spline-based GAM and a quasi-Poisson regression. The GAM outperformed the quasi-Poisson model with a lower RMSE (6.06 vs. 11.78) and higher adjusted R² (94.3%), indicating a highly flexible and accurate fit. The quasi-Poisson model, while appropriate for count data, underperformed due to the strong linear and nonlinear structure already captured by the GAM."

# STEP 4 - E

# STEP 4.E.1: Create a sequence of values for prediction

# Step 1: Create a sequence of values for prediction
x_seq <- data.frame(Sexual.orientation = seq(min(train_set$Sexual.orientation),
                                             max(train_set$Sexual.orientation),
                                             length.out = 300))

# Step 2: Add squared term for polynomial and regularized models
x_seq$Sexual.orientation_sq <- x_seq$Sexual.orientation^2



# STEP 4.E.2.1: Get predictions from each model

# Linear
y_lin <- predict(model_lin, newdata = x_seq)

# Polynomial
y_poly <- predict(model_poly, newdata = x_seq)

# STEP 4.E.2.2

library(glmnet)

# Create model matrix just like in training
x_seq_matrix <- model.matrix(~ Sexual.orientation + I(Sexual.orientation^2), data = x_seq)[, -1]

# Ridge prediction
y_ridge <- predict(ridge_bi_cv, s = best_lambda_ridge_bi, newx = x_seq_matrix)

# Lasso prediction
y_lasso <- predict(lasso_bi_cv, s = best_lambda_lasso_bi, newx = x_seq_matrix)

# STEP 4.E.2.3

y_gam <- predict(model_gam, newdata = x_seq)



# STEP 4.E.3: Combine predictions into one dataframe

plot_data <- data.frame(
  Sexual.orientation = x_seq$Sexual.orientation,
  Linear = y_lin,
  Polynomial = y_poly,
  Ridge = as.numeric(y_ridge),
  Lasso = as.numeric(y_lasso),
  GAM = y_gam
)


# STEP 4.E.4: Plot all curves on one graph

library(ggplot2)
library(tidyr)

# 1. Reshape data for plotting
plot_data_long <- plot_data %>%
  pivot_longer(cols = -Sexual.orientation, names_to = "Model", values_to = "Predicted")

# 2. Plot with training points and all model curves
ggplot(train_set, aes(x = Sexual.orientation, y = Total_Complaints)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_line(data = plot_data_long, aes(x = Sexual.orientation, y = Predicted, color = Model), size = 1.2) +
  labs(
    title = "Comparison of Bivariate Models",
    subtitle = "Fitted values of all models predicting Total Complaints from Sexual.orientation",
    x = "Sexual Orientation (incident count)",
    y = "Total Hate Crime Complaints",
    color = "Model"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Linear" = "blue",
    "Polynomial" = "red",
    "Ridge" = "purple",
    "Lasso" = "orange",
    "GAM" = "green"
  ))

# # STEP 4.E.4: Plot all curves on one graph (again to improve plot)

# Define an upper y-axis limit based on the 95th percentile
y_limit <- quantile(train_set$Total_Complaints, 0.999)

# Create the plot
ggplot(train_set, aes(x = Sexual.orientation, y = Total_Complaints)) +
  geom_point(alpha = 0.25, color = "gray40") +
  geom_line(data = plot_data_long, aes(x = Sexual.orientation, y = Predicted, color = Model), size = 1.2) +
  labs(
    title = "Comparison of Bivariate Models",
    subtitle = "Fitted values of all models predicting Total Complaints from Sexual.orientation",
    x = "Sexual Orientation (incident count)",
    y = "Total Hate Crime Complaints",
    color = "Model"
  ) +
  coord_cartesian(ylim = c(0, y_limit)) +  # <-- This zooms into the informative y range
  theme_minimal() +
  scale_color_manual(values = c(
    "Linear" = "blue",
    "Polynomial" = "red",
    "Ridge" = "purple",
    "Lasso" = "orange",
    "GAM" = "green"
  ))

# INTERPREATION: "To compare model performance and flexibility, we plotted the fitted values from five bivariate models that predict Total_Complaints using Sexual.orientation.
#The simple linear model (blue) captured the overall trend well, but slightly underfit the curvature present at higher values. The polynomial model (red) improved the fit by introducing a quadratic term, but still imposed a rigid global shape.
#Ridge regression (purple) applied L2 regularization to the same polynomial features. While it helped reduce overfitting, it also slightly underfit the upper range of the predictor, pulling the curve downward due to penalty shrinkage.
#In contrast, lasso regression (orange) found an effective balance between flexibility and parsimony. It achieved a lower validation RMSE (6.6) than both ridge and polynomial models, suggesting that it potentially removed the weaker squared term altogether.
#Finally, the GAM spline model (green) allowed the smoothest and most flexible fit. It adapted to local patterns in the data—especially at low and mid-range values—and achieved the lowest validation RMSE (6.06), confirming its superior generalization.
#To improve visualization and highlight this comparison, we adjusted the y-axis range using the 95th percentile of Total_Complaints. This removed distortion caused by extreme outliers and helped visualize differences across models more clearly.#

# STEP 4 - F : building a clear performance comparison table

# STEP 4.F.1:Compute In-Sample RMSEs

# Helper function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# In-sample predictions
rmse_train_lin <- rmse(train_set$Total_Complaints, predict(model_lin, newdata = train_set))
rmse_train_poly <- rmse(train_set$Total_Complaints, predict(model_poly, newdata = train_set))

x_train_bi <- model.matrix(~ Sexual.orientation + I(Sexual.orientation^2), data = train_set)[, -1]
rmse_train_ridge <- rmse(train_set$Total_Complaints, predict(ridge_bi_cv, s = best_lambda_ridge_bi, newx = x_train_bi))
rmse_train_lasso <- rmse(train_set$Total_Complaints, predict(lasso_bi_cv, s = best_lambda_lasso_bi, newx = x_train_bi))

rmse_train_gam <- rmse(train_set$Total_Complaints, predict(model_gam, newdata = train_set))


# STEP 4.F.2:Recall Your Validation RMSEs

# Based on your earlier results: linear = 6.28; Polynomial = 6.53; Ridge = 8.26; Lasso = 6.60; GAM = 6.06

# STEP 4.F.3:Create the Summary Table in R

performance_table <- data.frame(
  Model = c("Linear", "Polynomial", "Ridge", "Lasso", "GAM"),
  In_Sample_RMSE = c(
    round(rmse_train_lin, 2),
    round(rmse_train_poly, 2),
    round(rmse_train_ridge, 2),
    round(rmse_train_lasso, 2),
    round(rmse_train_gam, 2)
  ),
  Validation_RMSE = c(
    6.28,
    6.53,
    8.26,
    6.60,
    6.06
  )
)

print(performance_table)


# Interpretation: "Based on the validation RMSEs, the GAM model achieved the best generalization performance (6.06), followed by the linear model (6.28). Although the polynomial and lasso models performed competitively, the ridge model over-regularized, resulting in the highest validation RMSE (8.26). While some models performed slightly better in-sample, validation results guided our final model selection to avoid overfitting."

# EXTRA F STEP, creatin a plot comparing RMSEs

library(ggplot2)

# Create a data frame with just the validation RMSEs
rmse_df <- data.frame(
  Model = c("Linear", "Polynomial", "Ridge", "Lasso", "GAM"),
  Validation_RMSE = c(6.28, 6.53, 8.26, 6.60, 6.06)
)

# Sort for better visual ordering (lowest RMSE on top)
rmse_df <- rmse_df[order(rmse_df$Validation_RMSE), ]

# Plot
ggplot(rmse_df, aes(x = reorder(Model, Validation_RMSE), y = Validation_RMSE, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Validation_RMSE, 2)), vjust = -0.3, size = 4.2) +
  labs(
    title = "Validation RMSE Comparison by Model",
    x = "Model",
    y = "Validation RMSE"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(rmse_df$Validation_RMSE) + 1)

# STEP NEW: CREATING PLOTS FOR DOC

# PLOT 1: Bar Plot Comparing Validation RMSEs

library(ggplot2)

# Create dataframe
rmse_df <- data.frame(
  Model = c("Linear", "Polynomial", "Ridge", "Lasso", "GAM"),
  Validation_RMSE = c(6.28, 6.53, 8.26, 6.60, 6.06)
)

# Sort by RMSE (lower is better)
rmse_df <- rmse_df[order(rmse_df$Validation_RMSE), ]

# Plot
ggplot(rmse_df, aes(x = reorder(Model, Validation_RMSE), y = Validation_RMSE, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Validation_RMSE, 2)), vjust = -0.3, size = 4.2) +
  labs(
    title = "Validation RMSE Comparison by Model",
    x = "Model",
    y = "Validation RMSE"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(rmse_df$Validation_RMSE) + 1)

# PLOT 2: Lasso & Ridge Coefficients at Best Lambda

# Lasso coefficients at best lambda
coef(lasso_bi_cv, s = best_lambda_lasso_bi)

# Ridge coefficients at best lambda
coef(ridge_bi_cv, s = best_lambda_ridge_bi)

# PLOT 3:  Spline Plot for GAM Model

# GAM spline plot
plot(model_gam, shade = TRUE, 
     main = "GAM: Spline for Sexual.orientation",
     xlab = "Sexual Orientation (incident count)",
     ylab = "Smoothed Contribution to Total Complaints")

# Print GAM model summary
summary(model_gam)

#-----------------------------------------------------------------

#STEP 5: Multivariate Regression Modeling

# step 5.a: Multivariate Linear Regression (No Transformations)

# Ensure categorical variables are factors
train_set$Gender <- as.factor(train_set$Gender)
train_set$Agency.type <- as.factor(train_set$Agency.type)

valid_set$Gender <- as.factor(valid_set$Gender)
valid_set$Agency.type <- as.factor(valid_set$Agency.type)

# Fit multivariate linear regression
model_multi <- lm(Total_Complaints ~ Sexual.orientation + Religion + Race + Population + Gender + Agency.type, data = train_set)

# In-sample RMSE
rmse_train_multi <- sqrt(mean((train_set$Total_Complaints - predict(model_multi, newdata = train_set))^2))

# Out-of-sample RMSE
rmse_valid_multi <- sqrt(mean((valid_set$Total_Complaints - predict(model_multi, newdata = valid_set))^2))

# Print results
print(paste("Train RMSE:", round(rmse_train_multi, 2)))
print(paste("Validation RMSE:", round(rmse_valid_multi, 2)))

# Show model summary
summary(model_multi)


# STEP 5.b: Regularization of Multivariate Model

# STEP 5.b.1: Preparation

install.packages("glmnet")  # Only run if not installed
library(glmnet)

# Create a formula for consistency
formula_full <- Total_Complaints ~ Sexual.orientation + Religion + Race + Population + Gender + Agency.type

# Subset training data to complete cases only
train_set_complete <- train_set[complete.cases(train_set[, all.vars(formula_full)]), ]
valid_set_complete <- valid_set[complete.cases(valid_set[, all.vars(formula_full)]), ]

# Create model matrices
x_train_full <- model.matrix(formula_full, data = train_set_complete)[, -1]
x_valid_full <- model.matrix(formula_full, data = valid_set_complete)[, -1]

# Response vectors
y_train <- train_set_complete$Total_Complaints
y_valid <- valid_set_complete$Total_Complaints


# STEP 5.b.3: Ridge Regression (L2 Penalty)
# Ridge
ridge_cv <- cv.glmnet(x_train_full, y_train, alpha = 0)
best_lambda_ridge <- ridge_cv$lambda.min
pred_ridge <- predict(ridge_cv, s = best_lambda_ridge, newx = x_valid_full)
rmse_ridge <- sqrt(mean((y_valid - pred_ridge)^2))
print(paste("Validation RMSE (Ridge):", round(rmse_ridge, 2)))

# Lasso
lasso_cv <- cv.glmnet(x_train_full, y_train, alpha = 1)
best_lambda_lasso <- lasso_cv$lambda.min
pred_lasso <- predict(lasso_cv, s = best_lambda_lasso, newx = x_valid_full)
rmse_lasso <- sqrt(mean((y_valid - pred_lasso)^2))
print(paste("Validation RMSE (Lasso):", round(rmse_lasso, 2)))


# STEP 5.C: Non-Linear Features (No Regularization)

# STEP 5.C.1: Add Nonlinear Terms to Your Training Data

library(dplyr)


# Create squared and log-transformed features
train_set_nl <- train_set_complete %>%
  mutate(
    Sexual.orientation_sq = Sexual.orientation^2,
    Religion_sq = Religion^2,
    Race_sq = Race^2,
    Population_log = log(Population + 1)
  )

valid_set_nl <- valid_set_complete %>%
  mutate(
    Sexual.orientation_sq = Sexual.orientation^2,
    Religion_sq = Religion^2,
    Race_sq = Race^2,
    Population_log = log(Population + 1)
  )


# STEP 5.C.2: 

model_nl <- lm(Total_Complaints ~ Sexual.orientation + Sexual.orientation_sq +
                 Religion + Religion_sq +
                 Race + Race_sq +
                 Population_log +
                 Gender + Agency.type,
               data = train_set_nl)


# In-sample RMSE
rmse_train_nl <- sqrt(mean((train_set_nl$Total_Complaints - predict(model_nl, newdata = train_set_nl))^2))

# Out-of-sample RMSE
rmse_valid_nl <- sqrt(mean((valid_set_nl$Total_Complaints - predict(model_nl, newdata = valid_set_nl))^2))

# Output results
print(paste("Train RMSE (Nonlinear):", round(rmse_train_nl, 2)))
print(paste("Validation RMSE (Nonlinear):", round(rmse_valid_nl, 2)))

# View model summary
summary(model_nl)

# view part

print(paste("Train RMSE (Nonlinear):", round(rmse_train_nl, 2)))
print(paste("Validation RMSE (Nonlinear):", round(rmse_valid_nl, 2)))

# STEP 5.D: Support Vector Machine Regression

# STEP. 5.D.1: 

install.packages("e1071")  # Run only if not installed
library(e1071)

# STEP 5.D.2:

length(pred_svm_train)
nrow(train_set_nl)

length(pred_svm_valid)
nrow(valid_set_nl)

# Unify factor levels
valid_set_nl$Gender <- factor(valid_set_nl$Gender, levels = levels(train_set_nl$Gender))
valid_set_nl$Agency.type <- factor(valid_set_nl$Agency.type, levels = levels(train_set_nl$Agency.type))

svm_model <- svm(Total_Complaints ~ Sexual.orientation + Sexual.orientation_sq +
                   Religion + Religion_sq +
                   Race + Race_sq +
                   Population_log +
                   Gender + Agency.type,
                 data = train_set_nl)

pred_svm_train <- predict(svm_model, newdata = train_set_nl)
pred_svm_valid <- predict(svm_model, newdata = valid_set_nl)

rmse_train_svm <- sqrt(mean((train_set_nl$Total_Complaints - pred_svm_train)^2))
rmse_valid_svm <- sqrt(mean((valid_set_nl$Total_Complaints - pred_svm_valid)^2))

print(paste("Train RMSE (SVM):", round(rmse_train_svm, 2)))
print(paste("Validation RMSE (SVM):", round(rmse_valid_svm, 2)))


# STEP 5.E:  Regression Tree

# STEP 5.E.1

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# STEP 5.E.2: 

tree_model <- rpart(
  Total_Complaints ~ Sexual.orientation + Religion + Race + Population + Gender + Agency.type,
  data = train_set,
  method = "anova",        # For regression
  control = rpart.control(cp = 0.01)  # Start with default complexity
)


# STEP 5.E.3

rpart.plot(tree_model, type = 2, extra = 101, under = TRUE, main = "Regression Tree for Total Complaints")

# STEP 5.E.4

# Predict on training and validation
tree_pred_train <- predict(tree_model, newdata = train_set)
tree_pred_valid <- predict(tree_model, newdata = valid_set)

# RMSE
rmse_tree_train <- sqrt(mean((train_set$Total_Complaints - tree_pred_train)^2))
rmse_tree_valid <- sqrt(mean((valid_set$Total_Complaints - tree_pred_valid)^2))

print(paste("Train RMSE (Tree):", round(rmse_tree_train, 2)))
print(paste("Validation RMSE (Tree):", round(rmse_tree_valid, 2)))


# STEP 5.F: Tree-Based Ensemble Model

# Install and load randomForest package if not already
install.packages("randomForest")  # run only once
library(randomForest)

# Filter complete cases for the variables in the formula
formula_rf <- Total_Complaints ~ Sexual.orientation + Religion + Race + Population + Gender + Agency.type
train_set_rf <- train_set[complete.cases(train_set[, all.vars(formula_rf)]), ]
valid_set_rf <- valid_set[complete.cases(valid_set[, all.vars(formula_rf)]), ]

# Fit the random forest model
library(randomForest)
set.seed(123)
rf_model <- randomForest(
  formula_rf,
  data = train_set_rf,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

# Predictions
rf_pred_train <- predict(rf_model, newdata = train_set_rf)
rf_pred_valid <- predict(rf_model, newdata = valid_set_rf)

# Compute RMSE
rmse_train_rf <- sqrt(mean((train_set_rf$Total_Complaints - rf_pred_train)^2))
rmse_valid_rf <- sqrt(mean((valid_set_rf$Total_Complaints - rf_pred_valid)^2))

# Print RMSEs
print(paste("Train RMSE (Random Forest):", round(rmse_train_rf, 2)))
print(paste("Validation RMSE (Random Forest):", round(rmse_valid_rf, 2)))

# Plot variable importance
varImpPlot(rf_model, main = "Variable Importance (Random Forest)")


# FINAL STEPS - FOR THE TEST DATA

# 1. TEST DATASET

# Apply same cleaning as before
test_set$Gender <- as.factor(test_set$Gender)
test_set$Agency.type <- as.factor(test_set$Agency.type)

# Drop rows with missing values in the same variables used in the final model
formula_final <- Total_Complaints ~ Sexual.orientation + Religion + Race + Population + Gender + Agency.type
test_set_clean <- test_set[complete.cases(test_set[, all.vars(formula_final)]), ]

# 2. TEST SET

# Predict using the final multivariate model
pred_test_final <- predict(model_multi, newdata = test_set_clean)

# Compute RMSE on test set
rmse_test_final <- sqrt(mean((test_set_clean$Total_Complaints - pred_test_final)^2))
print(paste("Test RMSE (Final Model):", round(rmse_test_final, 2)))










































