
cat("\n==================================================================================\n")
data <- read.csv("C:\\Users\\DELL\\Downloads\\23i2622_23i2618data(Sheet2).csv")
data <- as.data.frame(data)


cat("\n=========== MULTIPLE LINEAR REGRESSION MODEL ===========\n")
MLRM <- lm(Inflation ~ ., data = train_data[, !(names(train_data) %in% c("Year"))])

# Show model summary
summary(MLRM)

summary(data)
colnames(data)

boxplot_vars <- c("Year", "Inflation", "pop_growth", "CPI", "Trade..ofGDP.",
                  "Imports_of_goods_and_services.annual.growth.", 
                  "Gross_national_expenditure..ofGDP.",
                  "External._debt._stocks..ofGNI.",
                  "Wholesale_price_index", "Food_Production_index",
                  "World_Crude_oil_rates...", "UnemploymentRate..oftotallaborforce.",
                  "AdolescentFertilityRate", "AgeDependencyRatio..ofWorking.agePopulation.",
                  "UrbanPopulationGrowth.annual..", "TotalReserves..ofTotalExternalDebt.",
                  "ExportsOfGoodsAndServices.annual.growth.",
                  "AdjustedSavingsGrossSavings..ofGNI.")

boxplot_data_1 <- data[, boxplot_vars[1:9]]
boxplot_data_2 <- data[, boxplot_vars[10:18]]

# Boxplot: Variables 1 to 9
cat("\n=====================\n")
par(mfrow = c(3, 3), mar = c(5, 4, 2, 1))
for (i in 1:ncol(boxplot_data_1)) {
  boxplot(boxplot_data_1[, i],
          horizontal = TRUE,
          col = rainbow(9)[i],
          main = names(boxplot_data_1)[i],
          xlab = "Values",
          border = "black",
          outline = TRUE,
          notch = TRUE,
          cex.axis = 0.8)
}

# Boxplot: Variables 10 to 18
cat("\n=====================\n")
par(mfrow = c(3, 3), mar = c(5, 4, 2, 1))
for (i in 1:ncol(boxplot_data_2)) {
  boxplot(boxplot_data_2[, i],
          horizontal = TRUE,
          col = rainbow(9)[i],
          main = names(boxplot_data_2)[i],
          xlab = "Values",
          border = "black",
          outline = TRUE,
          notch = TRUE,
          cex.axis = 0.8)
}

# Scatter Plots
cat("\n=====================\n")
scatter_vars <- c("pop_growth", "CPI", "Trade..ofGDP.",
                  "Imports_of_goods_and_services.annual.growth.",
                  "Gross_national_expenditure..ofGDP.",
                  "External._debt._stocks..ofGNI.",
                  "Wholesale_price_index", "Food_Production_index",
                  "World_Crude_oil_rates...", "UnemploymentRate..oftotallaborforce.",
                  "AdolescentFertilityRate", "AgeDependencyRatio..ofWorking.agePopulation.",
                  "UrbanPopulationGrowth.annual..", "TotalReserves..ofTotalExternalDebt.",
                  "ExportsOfGoodsAndServices.annual.growth.",
                  "AdjustedSavingsGrossSavings..ofGNI.")

par(mfrow = c(4, 5), mar = c(4, 4, 2, 1))
for (var in scatter_vars) {
  plot(data$Inflation, data[[var]],
       main = var,
       xlab = "Inflation", ylab = "Value",
       pch = 16, col = "blue",
       cex = 0.6,
       cex.main = 0.8,
       cex.lab = 0.7,
       cex.axis = 0.7)
}


# Subset data with selected variables
LDF <- data[, c("CPI", 
                "Trade..ofGDP.", 
                "Imports_of_goods_and_services.annual.growth.", 
                "Food_Production_index", 
                "UnemploymentRate..oftotallaborforce.")]

# Scatter Plot Matrix (pairwise plots)
pairs(LDF, 
      main = "Pairwise Scatter Plots of Significant Variables",
      pch = 19, 
      col = rgb(0.2, 0.4, 0.6, 0.5))  # Optional styling

# Correlation Matrix
cat("Correlation Matrix:\n")
print(cor(LDF, use = "complete.obs"))


cat("\n==================================================================================\n")
#ARIMA
# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)
library(caret)
library(corrplot)


data <- read.csv("C:\\Users\\DELL\\Downloads\\23i2622_23i2618data(Sheet2).csv")
data <- as.data.frame(data)



# Split data
train_data <- subset(data, Year >= 1980 & Year <= 2020)
test_data  <- subset(data, Year >= 2021 & Year <= 2024)

#  Run linear regression
predictors <- names(train_data)[!names(train_data) %in% c("Year", "Inflation")]
formula <- as.formula(paste("Inflation ~", paste(predictors, collapse = " + ")))
lm_model <- lm(formula, data = train_data)

summary_lm <- summary(lm_model)

# Extract p-values
pvalues <- summary_lm$coefficients[-1, 4]  # Exclude intercept
significant_vars_p <- names(pvalues)[pvalues < 0.05]
insignificant_vars_p <- names(pvalues)[pvalues >= 0.05]

cat("Significant variables based on p-value < 0.05:\n")
print(significant_vars_p)

cat("Insignificant variables (p-value >= 0.05):\n")
print(insignificant_vars_p)
# Use significant variables only to reduce noise/multicollinearity

#training time series for inflation and regressors
y_train <- ts(train_data$Inflation, start = 1980, frequency = 1)
xreg_train <- as.matrix(train_data[, significant_vars])
xreg_test  <- as.matrix(test_data[, significant_vars])

# Plot the inflation series
autoplot(y_train) + ggtitle("Inflation Time Series (1980–2020)") + ylab("Inflation") + xlab("Year")

# stationarity
adf <- adf.test(y_train)
print(adf)
if (adf$p.value > 0.05) {
  y_train <- diff(y_train)
  xreg_train <- xreg_train[-1, ]  # Adjust due to differencing
  xreg_test <- xreg_test  # Test set remains the same
  cat("Differenced Inflation Series (Non-stationary detected)\n")
}

#  Fit ARIMAX Model
model <- auto.arima(y_train, xreg = xreg_train)
summary(model)

#  Forecast for 2021–2024
forecast_result <- forecast(model, xreg = xreg_test, h = 4)

# Plot forecast
autoplot(forecast_result) + ggtitle("Inflation Forecast (2021–2024)") + ylab("Inflation")

#  Compare predicted vs actual
predicted <- as.numeric(forecast_result$mean)
actual <- test_data$Inflation
comparison <- data.frame(Year = 2021:2024, Actual = actual, Predicted = predicted)
print(comparison)

# Calculate Error Metrics
mse <- mean((comparison$Actual - comparison$Predicted)^2)
mae <- mean(abs(comparison$Actual - comparison$Predicted))
rmse <- sqrt(mse)

cat("MSE:", round(mse, 4), "\n")
cat("MAE:", round(mae, 4), "\n")
cat("RMSE:", round(rmse, 4), "\n")

# Step 10: Residual Diagnostics
checkresiduals(model)


cat("\n==================================================================================\n")

#LASSO
library(caret)
library(ggplot2)

# Load data
data <- read.csv("C:\\Users\\DELL\\Downloads\\23i2622_23i2618data(Sheet2).csv")
data <- as.data.frame(data)

# Filter for train/test years
train_data <- subset(data, Year >= 1980 & Year <= 2020)
test_data  <- subset(data, Year >= 2021 & Year <= 2024)

# Identify significant variables using correlation (you can also use p-values separately)
cor_matrix <- cor(train_data[, -which(names(train_data) == "Year")], use = "complete.obs")
inflation_corr <- cor_matrix["Inflation", -which(colnames(cor_matrix) == "Inflation")]
significant_vars <- names(inflation_corr)[abs(inflation_corr) > 0.3]

# Prepare matrices
x_train <- train_data[, significant_vars]
y_train <- train_data$Inflation
x_test <- test_data[, significant_vars]
y_test <- test_data$Inflation

# Train LASSO model
train_control <- trainControl(method = "cv", number = 10)
lasso_model <- train(
  x = x_train, y = y_train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, length = 100))
)

# Best lambda
best_lambda <- lasso_model$bestTune$lambda
cat("Best lambda:", best_lambda, "\n")

# Predict
lasso_pred <- predict(lasso_model, newdata = x_test)

# Evaluation
mse <- mean((y_test - lasso_pred)^2)
rmse <- sqrt(mse)
mae <- mean(abs(y_test - lasso_pred))

cat("\nEvaluation Metrics (2021–2024):\n")
cat("MSE:", round(mse, 3), "\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE:", round(mae, 3), "\n\n")

# Actual vs Predicted Table
comparison <- data.frame(Year = 2021:2024, Actual = y_test, Predicted = as.numeric(lasso_pred))
print(comparison)

# --- Plots ---

# 1. Actual vs Predicted Line Plot
ggplot(comparison, aes(x = Year)) +
  geom_line(aes(y = Actual), color = "black", size = 1.2) +
  geom_line(aes(y = Predicted), color = "blue", linetype = "dashed", size = 1.2) +
  ggtitle("LASSO: Actual vs Predicted Inflation") +
  ylab("Inflation")

# 2. Residual Plot
residuals <- y_test - lasso_pred
res_df <- data.frame(Year = 2021:2024, Residuals = residuals)
ggplot(res_df, aes(x = Year, y = Residuals)) +
  geom_point(color = "red", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residual Plot for LASSO Model")

# 3. Predicted vs Actual Scatter Plot
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggtitle("LASSO: Predicted vs Actual Scatter Plot")

# 4. Coefficient Plot
coef_df <- as.data.frame(as.matrix(coef(lasso_model$finalModel, s = best_lambda)))
coef_df <- coef_df[coef_df$`s1` != 0, , drop = FALSE]
coef_df$Variable <- rownames(coef_df)
colnames(coef_df)[1] <- "Coefficient"

ggplot(coef_df[-1, ], aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Non-Zero Coefficients from LASSO Model") +
  xlab("Variables") +
  ylab("Coefficient")

cat("\n==================================================================================\n")
# RIDGE 

library(caret)
library(ggplot2)

data <- read.csv("C:\\Users\\DELL\\Downloads\\23i2622_23i2618data(Sheet2).csv")
data <- as.data.frame(data)

train_data <- subset(data, Year >= 1980 & Year <= 2020)
test_data  <- subset(data, Year >= 2021 & Year <= 2024)


cor_matrix <- cor(train_data[, -which(names(train_data) == "Year")], use = "complete.obs")
inflation_corr <- cor_matrix["Inflation", -which(colnames(cor_matrix) == "Inflation")]
significant_vars <- names(inflation_corr)[abs(inflation_corr) > 0.3]

# Prepare matrices
x_train <- train_data[, significant_vars]
y_train <- train_data$Inflation
x_test <- test_data[, significant_vars]
y_test <- test_data$Inflation

# Define cross-validation control
train_control <- trainControl(method = "cv", number = 10)

# --- Ridge Model --- #
ridge_model <- train(
  x = x_train, y = y_train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, length = 100))
)

# Best lambda for Ridge
best_lambda_ridge <- ridge_model$bestTune$lambda
cat("Best lambda for Ridge:", best_lambda_ridge, "\n")

# Predict using Ridge model
ridge_pred <- predict(ridge_model, newdata = x_test)

# Evaluation for Ridge model
mse_ridge <- mean((y_test - ridge_pred)^2)
rmse_ridge <- sqrt(mse_ridge)
mae_ridge <- mean(abs(y_test - ridge_pred))

cat("\nRidge Model Evaluation Metrics (2021–2024):\n")
cat("MSE:", round(mse_ridge, 3), "\n")
cat("RMSE:", round(rmse_ridge, 3), "\n")
cat("MAE:", round(mae_ridge, 3), "\n\n")

# --- Actual vs Predicted Table for Ridge ---
comparison_ridge <- data.frame(Year = 2021:2024, Actual = y_test, Predicted = as.numeric(ridge_pred))
print(comparison_ridge)

# --- Actual vs Predicted Line Plot for Ridge ---
ggplot(comparison_ridge, aes(x = Year)) +
  geom_line(aes(y = Actual), color = "black", size = 1.2) +
  geom_line(aes(y = Predicted), color = "blue", linetype = "dashed", size = 1.2) +
  ggtitle("Ridge: Actual vs Predicted Inflation") +
  ylab("Inflation")

# --- Residual Plot for Ridge ---
residuals_ridge <- y_test - ridge_pred
res_df_ridge <- data.frame(Year = 2021:2024, Residuals = residuals_ridge)
ggplot(res_df_ridge, aes(x = Year, y = Residuals)) +
  geom_point(color = "red", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residual Plot for Ridge Model")

# --- Predicted vs Actual Scatter Plot for Ridge ---
ggplot(comparison_ridge, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggtitle("Ridge: Predicted vs Actual Scatter Plot")

# --- Coefficient Plot for Ridge ---
coef_df_ridge <- as.data.frame(as.matrix(coef(ridge_model$finalModel, s = best_lambda_ridge)))
coef_df_ridge <- coef_df_ridge[coef_df_ridge$`s1` != 0, , drop = FALSE]
coef_df_ridge$Variable <- rownames(coef_df_ridge)
colnames(coef_df_ridge)[1] <- "Coefficient"

ggplot(coef_df_ridge[-1, ], aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Non-Zero Coefficients from Ridge Model") +
  xlab("Variables") +
  ylab("Coefficient")


cat("\n==================================================================================\n")
#ELASTIC NET

library(caret)
library(ggplot2)


data <- read.csv("C:\\Users\\DELL\\Downloads\\23i2622_23i2618data(Sheet2).csv")
data <- as.data.frame(data)


train_data <- subset(data, Year >= 1980 & Year <= 2020)
test_data  <- subset(data, Year >= 2021 & Year <= 2024)

# Identify significant variables using correlation (optional)
cor_matrix <- cor(train_data[, -which(names(train_data) == "Year")], use = "complete.obs")
inflation_corr <- cor_matrix["Inflation", -which(colnames(cor_matrix) == "Inflation")]
significant_vars <- names(inflation_corr)[abs(inflation_corr) > 0.3]


x_train <- train_data[, significant_vars]
y_train <- train_data$Inflation
x_test <- test_data[, significant_vars]
y_test <- test_data$Inflation

# Define cross-validation control
train_control <- trainControl(method = "cv", number = 10)

# --- Elastic Net Model --- #
elastic_net_model <- train(
  x = x_train, y = y_train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 0.5, lambda = seq(0.001, 1, length = 100))
)

# Best lambda for Elastic Net
best_lambda_enet <- elastic_net_model$bestTune$lambda
cat("Best lambda for Elastic Net:", best_lambda_enet, "\n")

# Predict using Elastic Net model
enet_pred <- predict(elastic_net_model, newdata = x_test)

# Evaluation for Elastic Net model
mse_enet <- mean((y_test - enet_pred)^2)
rmse_enet <- sqrt(mse_enet)
mae_enet <- mean(abs(y_test - enet_pred))

cat("\nElastic Net Model Evaluation Metrics (2021–2024):\n")
cat("MSE:", round(mse_enet, 3), "\n")
cat("RMSE:", round(rmse_enet, 3), "\n")
cat("MAE:", round(mae_enet, 3), "\n\n")

# --- Actual vs Predicted Table for Elastic Net ---
comparison_enet <- data.frame(Year = 2021:2024, Actual = y_test, Predicted = as.numeric(enet_pred))
print(comparison_enet)

# --- Actual vs Predicted Line Plot for Elastic Net ---
ggplot(comparison_enet, aes(x = Year)) +
  geom_line(aes(y = Actual), color = "black", size = 1.2) +
  geom_line(aes(y = Predicted), color = "blue", linetype = "dashed", size = 1.2) +
  ggtitle("Elastic Net: Actual vs Predicted Inflation") +
  ylab("Inflation")

# --- Residual Plot for Elastic Net ---
residuals_enet <- y_test - enet_pred
res_df_enet <- data.frame(Year = 2021:2024, Residuals = residuals_enet)
ggplot(res_df_enet, aes(x = Year, y = Residuals)) +
  geom_point(color = "red", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residual Plot for Elastic Net Model")

# --- Predicted vs Actual Scatter Plot for Elastic Net ---
ggplot(comparison_enet, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggtitle("Elastic Net: Predicted vs Actual Scatter Plot")

# --- Coefficient Plot for Elastic Net ---
coef_df_enet <- as.data.frame(as.matrix(coef(elastic_net_model$finalModel, s = best_lambda_enet)))
coef_df_enet <- coef_df_enet[coef_df_enet$`s1` != 0, , drop = FALSE]
coef_df_enet$Variable <- rownames(coef_df_enet)
colnames(coef_df_enet)[1] <- "Coefficient"

ggplot(coef_df_enet[-1, ], aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Non-Zero Coefficients from Elastic Net Model") +
  xlab("Variables") +
  ylab("Coefficient")

cat("\n==================================================================================\n")
comparison_arima <- data.frame(
  Year = 2021:2024,
  Actual = actual,
  Predicted = as.numeric(forecast_result$mean)
)
comparison_lasso <- data.frame(
  Year = 2021:2024,
  Actual = y_test,
  Predicted = as.numeric(lasso_pred)
)
comparison_ridge <- data.frame(Year = 2021:2024, 
                               Actual = y_test, 
                               Predicted = as.numeric(ridge_pred))
comparison_enet <- data.frame(Year = 2021:2024, 
                              Actual = y_test, 
                              Predicted = as.numeric(enet_pred)) 

arima_df <- comparison_arima %>% mutate(Model = "ARIMA")
lasso_df <- comparison_lasso %>% mutate(Model = "LASSO")
ridge_df <- comparison_ridge %>% mutate(Model = "Ridge")
enet_df  <- comparison_enet  %>% mutate(Model = "Elastic Net")

combined_predictions <- bind_rows(arima_df, lasso_df, ridge_df, enet_df)

library(ggplot2)

ggplot(combined_predictions, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
  geom_line(aes(y = Predicted, color = Model, linetype = Model), size = 1.1) +
  scale_color_manual(values = c("Actual" = "black", 
                                "ARIMA" = "red", 
                                "LASSO" = "blue",
                                "Ridge" = "darkgreen", 
                                "Elastic Net" = "purple")) +
  scale_linetype_manual(values = c("ARIMA" = "dashed", 
                                   "LASSO" = "dotted",
                                   "Ridge" = "dotdash", 
                                   "Elastic Net" = "twodash")) +
  ggtitle("Actual vs Predicted Inflation (2021–2024) for All Models") +
  ylab("Inflation") + 
  xlab("Year") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
