# Canterra Employee Attrition Analysis

## Overview
This repository contains a comprehensive analysis of employee turnover at Canterra. The project identifies key factors driving attrition and offers actionable recommendations to reduce turnover rates. By leveraging data-driven insights and predictive modeling, this project aims to improve employee satisfaction, reduce costs associated with high turnover, and foster a stable and high-performing workforce.

---

## Features

### **Exploratory Data Analysis**
- **Correlation Heatmaps**: Visualizing relationships between age, income, job satisfaction, tenure, and attrition.
- **Key Trends**:
  - Higher turnover among employees under 25.
  - Increased attrition linked to income disparities and job dissatisfaction.
  - Frequent business travel correlates with higher attrition rates.

### **Predictive Models**
- **Logistic Regression**:
  - Focused on binary classification of employee attrition.
  - Identified top predictors: job satisfaction, income, age, and tenure.
- **Decision Tree**:
  - Provided interpretable insights into decision paths.
  - Highlighted tenure, income, and age as key drivers of attrition.
- **Bagged Tree**:
  - Reduced variance and improved predictive stability.
  - High ROC AUC score of 0.943.
- **Random Forest**:
  - Best-performing model with ROC AUC of 0.972.
  - Accurately identified high-risk employees while minimizing overfitting.

### **Model Evaluation**
- **ROC Curves**:
  - Evaluated models’ performance with both training and testing datasets.
- **Sensitivity and Specificity**:
  - Assessed true positives and negatives for each model.
- **Feature Importance**:
  - Analyzed key factors like income, job satisfaction, and age.

---

## Key Visualizations
### Attrition by Income
![Attrition by Income](images/attrition_by_income.png)

### Attrition by Business Travel
![Attrition by Business Travel](images/attrition_by_travel.png)

### ROC Curve for Random Forest Model
![ROC Curve](images/roc_curve_random_forest.png)

---

## Workflow

1. **Data Preparation**
   - Cleaned and transformed the dataset.
   - Addressed class imbalance with stratified sampling.
   - Converted categorical variables to numerical formats.

2. **Model Development**
   - Built and compared Logistic Regression, Decision Tree, Bagged Tree, and Random Forest models.
   - Conducted hyperparameter tuning and cross-validation.

3. **Insights and Recommendations**
   - Identified high-risk groups (e.g., younger employees, frequent travelers).
   - Provided actionable strategies to reduce turnover.

---

## Recommendations
1. **Enhance Career Development for Younger Employees**
   - Create mentorship programs and clear growth paths.
2. **Improve Job Satisfaction**
   - Introduce recognition programs and flexible work arrangements.
3. **Offer Competitive Compensation**
   - Benchmark salaries and implement performance-based incentives.
4. **Prevent Burnout in Long-Term Employees**
   - Provide leadership opportunities and skill rotations.

---

## Tools and Technologies
- **Data Cleaning and Preparation**: Python (pandas, numpy)
- **Visualization**: Tableau, ggplot2
- **Modeling**: R (caret, randomForest)


