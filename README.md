# Project 1 — Independent t-test (Heart Dataset)

## 📌 Project Goal
Examine whether there is a difference in **mean cholesterol levels** between males and females.

## 📊 Dataset
- Source: UCI / Kaggle Heart Disease dataset
- Variables:
  - `sex` (Male / Female)
  - `chol` (Serum cholesterol)

## 🧪 Analysis Overview
- Descriptive statistics (mean and standard deviation)
- Distribution check using QQ-plots
- Welch two-sample t-test
- Effect size calculation (Cohen’s d)
- Visualization using a boxplot

## 📈 Key Findings
- Females show **higher average cholesterol levels** than males
- The difference is **statistically significant**
- Effect size indicates a **small-to-moderate difference**

## 🖼️ Visualization
Boxplot illustrating cholesterol levels by sex:

![Cholesterol by Sex](plots/qq_females.png)

## 🧠 Conclusion
In this dataset, females have higher mean cholesterol levels compared to males.  
This project demonstrates a basic application of hypothesis testing and data visualization in R for learning purposes.
