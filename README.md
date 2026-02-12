# 📊 Statistical Data Analysis Project: MLB Hitters

An in-depth statistical analysis of Major League Baseball (MLB) player performance and salary metrics. This project leverages the **Hitters** dataset to explore the relationship between player statistics and their annual compensation through advanced econometric modeling.

---

## 🚀 Overview

This repository contains a full data science pipeline implemented in **R**. The goal is to build a predictive model for player salaries (`Salary`) while addressing common real-world data issues such as multicollinearity, non-normality, and influential outliers.

## 🛠️ Key Features & Methodology

### 1. Data Preprocessing & Cleaning
- **Handling Missing Data**: Removal of incomplete observations to ensure model integrity.
- **Feature Selection**: Filtering out irrelevant categorical variables and focusing on performance metrics.
- **Normality Enhancement**: Application of **Log Transformations** to skewed variables (Salary, Home Runs, etc.) to satisfy OLS assumptions.

### 2. Exploratory Data Analysis (EDA)
- **Correlation Analysis**: Using `corrplot` to visualize relationships between performance metrics.
- **Multicollinearity Detection**: Identifying highly correlated predictors that could bias the model.
- **Visualization**: Detailed boxplots and histograms for univariate analysis.

### 3. Advanced Model Selection
The project compares different statistical techniques to find the optimal set of predictors:
- **Stepwise Selection (BIC)**: A conservative approach to minimize the Bayesian Information Criterion.
- **LASSO Regression**: Using $L_1$ regularization for automatic variable selection and shrinkage.

### 4. Robustness & Diagnostics
To ensure the model is reliable, we perform:
- **VIF (Variance Inflation Factor)**: Checking for multicollinearity issues.
- **Influence Measures**: Using **Cook's Distance** and **DFBETAS** to identify points disproportionately affecting coefficients.
- **Residual Analysis**: Detecting extreme outliers using **Studentized Residuals**.

### 5. Regularization Methods
- **Ridge Regression ($L_2$)**: Applied to stabilize the model when predictors are highly correlated, comparing coefficients against traditional OLS.
- **Cross-Validation**: Using `cv.glmnet` to find the optimal $\lambda$ parameter.

## 📂 Project Structure

- `project.R`: The complete R script containing all data cleaning, visualizations, and modeling logic.
- `README.md`: This documentation.

## 📋 Requirements

Ensure you have the following R packages installed:

```R
install.packages(c("MASS", "ISLR2", "corrplot", "e1071", "glmnet", "car", "boot", "leaps"))
```

## 📈 Results Summary

The analysis demonstrates that:
- Talent metrics (Hits, Home Runs) and career longevity (Years) are the strongest predictors of salary.
- Log-transformations significantly improve the normality of residuals.
- Regularization (Ridge/Lasso) provides more stable coefficient estimates compared to a simple full OLS model.

---
*Created as part of a Statistical Data Analysis study.*
