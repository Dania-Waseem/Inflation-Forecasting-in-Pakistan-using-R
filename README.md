# 📊 Inflation Forecasting in Pakistan

This project applies statistical and machine learning models to forecast inflation in Pakistan using macroeconomic data from 1980 to 2025.

## 📁 Dataset
- Source: Historical economic indicators for Pakistan (1980–2025)
- Target variable: Inflation rate (%)
- Features: CPI, WPI, Oil Prices, External Debt, Population Growth, Trade %, etc.

## 🧠 Models Used
- **ARIMA**: Captures temporal dependencies in the univariate inflation series.
- **Ridge Regression**: Handles multicollinearity by applying L2 regularization.
- **Lasso Regression**: Performs variable selection via L1 regularization.
- **Elastic Net**: Combines L1 and L2 penalties for balanced feature selection and multicollinearity control.

## ⚙️ Methodology
1. Data was split into:
   - **Training Set**: 1980–2020
   - **Testing Set**: 2021–2025
2. Each model was trained and evaluated using **Mean Squared Error (MSE)**.
3. Predicted vs. actual inflation values were visualized and compared.

## 📈 Results

| Model        | MSE      |
|--------------|----------|
| ARIMA        | 245.37   |
| Ridge        | 37.58    |
| Elastic Net  | 35.52    |
| **LASSO**     | **30.42** |

✅ **Best Model**: **LASSO Regression**
- Lowest MSE
- Best fit to actual values
- Automatically selects significant economic indicators

## 🔚 Conclusion
LASSO outperformed other models due to its ability to reduce overfitting, discard irrelevant variables, and produce more interpretable forecasts. ARIMA lagged due to limitations in handling multivariate relationships.

## 📌 Future Work
- Combine ARIMA with LASSO for hybrid time series modeling
- Test performance with quarterly or monthly data granularity
- Explore ensemble techniques for further accuracy gains
