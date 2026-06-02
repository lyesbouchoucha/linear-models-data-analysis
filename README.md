# 📊 Predictive & Interpretable Linear Modeling: Student Performance Analysis

### 🎯 Project Overview
This repository contains a reproducible end-to-end multiple linear regression analysis designed to study the factors influencing student exam performance. 

Developed as part of the **M1 MIDO (Linear Models)** curriculum, the dual objective of this project was to construct a highly interpretable inferential model while optimizing predictive performance on a dataset of 5,000 students, under strict constraints (no data leakage, parsimony, and reproducibility).

---

### 🛠️ Key Methodological Highlights

* **Data Integrity & Feature Management:** Cleaned and transformed raw categorical data into properly encoded factors. Successfully resolved the "duplicate representation" problem (e.g., `age` vs `agecat`, `attend_pct` vs `attend_pct_cat`) by selecting the continuous/categorical form that minimized multicollinearity while preserving linear assumptions.
* **Rigorous Model Selection:** Developed and compared multiple candidate models using distinct criteria: (Nested F-tests) and information-theoretic/predictive selection metrics (AIC, BIC). 
* **Gauss-Markov Assumptions & Diagnostics:** Conducted exhaustive residual diagnostics including Heteroskedasticity checks (Breusch-Pagan / NCV Test), Independence (Durbin-Watson), Normality tests (Shapiro-Wilk & Q-Q plots), and Influence analysis (Cook's Distance threshold $4/n$).
* **Strict Validation Protocol:** Implemented a robust train/test split validation framework (70/30) with a fixed seed (`set.seed(42)`) to ensure perfect reproducibility and zero data leakage during model training.

---

### 📉 Core Results & Performance

#### 1. Model Selection & Predictive Power
The final selected model (M4) includes an interaction term and significantly outperforms the baseline mean-prediction model, reducing the Mean Squared Error (MSE) by 72.7%:

| Model Specification | Training $R^2$ | Test/Validation MSE | Test $R^2$ | Test MedAE |
| :--- | :---: | :---: | :---: | :---: |
| **Baseline Model** (Mean) | 0.000 | 222.38 | 0.000 | 10.21 |
| **Full Model** (No Interaction) | 0.710 | 63.90 | 0.712 | 5.21 |
| **Final Selected Model** | **0.728** | **60.81** | **0.726** | **5.16** |

*Note: The model shows no sign of overfitting, as the Training $R^2$ (0.728) and Test $R^2$ (0.726) are nearly identical, and the Test RMSE (7.80) is actually slightly lower than the Training RMSE (7.92).*

#### 2. Key Statistical Insights (Inference)
* **Top Predictor (Attendance):** Class attendance has the strongest main effect. Each additional percentage point of attendance is associated with a 0.45-point increase in the exam score ($p < 0.001$).
* **Crucial Interaction Effect (Study Hours $\times$ Sleep Quality):** The benefit of studying is heavily moderated by sleep quality. Well-rested students gain $+3.05$ points per additional hour of study, whereas sleep-deprived students gain only $+1.16$ points. 
* **Scenario-Based Predictions:** When simulating profiles, the model predicts a massive 50-point gap between a "Struggling Student" profile (30.6/100) and an "Engaged Student" profile (80.1/100).

---

### 📂 Repository Structure

* 📄 `project.csv`: The raw observational dataset containing student characteristics and exam scores.
* 📝 `report.qmd`: The complete, reproducible Quarto source code containing data cleaning, visual EDA, model estimation, and diagnostic pipelines.
* 🌐 `report.html`: The fully rendered production report featuring interactive tables, data visualizations, and detailed mathematical interpretations (Download to view).

---

### 🚀 Reproducibility
To replicate the entire analysis and ensure all figures, models, and tables compute identically from a fresh R session:

1. Clone this repository.
2. Ensure packages such as `tidyverse`, `car`, `performance`, and reporting dependencies are installed.
3. Render the Quarto document:
```R
quarto::render("report.qmd")
