#########################################################################################################
########################### Linear Models Project - Student Performance #################################
#########################################################################################################


# ==============================================================================
# 0. SETUP
# ==============================================================================

## R Code 1------------------------------------------------------------------------------------------

options(scipen = 999, digits = 5)

## R Code 2------------------------------------------------------------------------------------------

library(tidyverse)
library(broom)
library(car)
library(performance)
library(parameters)
library(GGally)
library(rstatix)
library(gtsummary)
library(kableExtra)
library(patchwork)
library(collapse)
library(correlation)
library(qqplotr)
library(ggpubr)
library(janitor)
library(scales)
library(moments)


# ==============================================================================
# 1. DATA IMPORT AND MANAGEMENT
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Data import
# ------------------------------------------------------------------------------

## R Code 3------------------------------------------------------------------------------------------

project <- read_csv("data/project.csv", show_col_types = FALSE)


## R Code 4------------------------------------------------------------------------------------------

# Check data integrity
cat("=== DATA INTEGRITY CHECK ===\n")
cat("Number of observations:", nrow(project), "\n")
cat("Number of variables:", ncol(project), "\n")
cat("Unique IDs:", n_distinct(project$id), "\n")
cat("Duplicate IDs:", nrow(project) - n_distinct(project$id), "\n")
cat("Duplicate rows:", sum(duplicated(project)), "\n")
cat("Total missing values:", sum(is.na(project)), "\n")


## R Code 5------------------------------------------------------------------------------------------

glimpse(project)


# ------------------------------------------------------------------------------
# 1.2 Convert to factors with labels
# ------------------------------------------------------------------------------

## R Code 6------------------------------------------------------------------------------------------

# Following codebook specifications
data_clean <- project |>
  
  mutate(
    # Gender (ref: Female)
    sexe = factor(sexe, 
                  levels = c(1, 2, 3),
                  labels = c("Female", "Male", "Other")),
    
    # School type (ref: Public)
    school_type = factor(school_type, 
                         levels = c(1, 2),
                         labels = c("Public", "Private")),
    
    # Parental education - ordinal (ref: No Formal)
    parent_educ = factor(parent_educ, 
                         levels = 1:6,
                         labels = c("No Formal", "High School", "Graduate",
                                    "Post Grad 1", "Post Grad 2", "PhD")),
    
    # Sleep quality - ordinal (ref: Poor)
    sleep_qual = factor(sleep_qual, 
                        levels = c(1, 2, 3),
                        labels = c("Poor", "Average", "Good")),
    
    # Commute time - ordinal (ref: <15 Min)
    trav_time = factor(trav_time, 
                       levels = 1:4,
                       labels = c("<15 Min", "15-30 Min", "30-60 Min", ">60 Min")),
    
    # Internet access (ref: No)
    web_access = factor(web_access, 
                        levels = c(1, 2),
                        labels = c("No", "Yes")),
    
    # Extracurricular activities (ref: No)
    extra_act = factor(extra_act, 
                       levels = c(1, 2),
                       labels = c("No", "Yes")),
    
    # Study method (ref: Online Videos)
    study_method = factor(study_method, 
                          levels = 1:6,
                          labels = c("Online Videos", "Coaching", "Notes",
                                     "Textbook", "Group Study", "Mixed")),
    
    # Age categories (for sensitivity analysis)
    agecat = factor(agecat,
                    levels = 1:5,
                    labels = c("[14-15.1[", "[15.1-16.1[", "[16.1-17.1[",
                               "[17.1-18.1[", "[18.1-19]")),
    
    # Attendance categories (for sensitivity analysis)
    attend_pct_cat = factor(attend_pct_cat,
                            levels = 1:4,
                            labels = c("[50-62[", "[62-72[", "[72-81[", "[81-100]"))
  )


# ------------------------------------------------------------------------------
# 1.3 Add descriptive labels
# ------------------------------------------------------------------------------

## R Code 7------------------------------------------------------------------------------------------

data_clean <- data_clean |>
  relabel(
    id = "Student ID",
    y = "Exam Score",
    age = "Age (years)",
    agecat = "Age category",
    sexe = "Gender",
    school_type = "School Type",
    parent_educ = "Parental Education",
    study_hrs = "Weekly Study Hours",
    sleep_hrs = "Sleep Duration (hours)",
    sleep_qual = "Sleep Quality",
    attend_pct = "Attendance Rate (%)",
    attend_pct_cat = "Attendance category",
    web_access = "Internet Access",
    trav_time = "Commute Time",
    extra_act = "Extracurricular Activities",
    study_method = "Study Method"
  )


# ------------------------------------------------------------------------------
# 1.4 Verify final structure
# ------------------------------------------------------------------------------

## R Code 8------------------------------------------------------------------------------------------

# Check factor levels
cat("\n=== FACTOR LEVELS CHECK ===\n")
data_clean |>
  select(where(is.factor)) |>
  map(levels)


## R Code 9------------------------------------------------------------------------------------------

namlab(data_clean, N = TRUE, Ndistinct = TRUE, class = TRUE)


# ------------------------------------------------------------------------------
# 1.5 Identify extreme values
# ------------------------------------------------------------------------------

## R Code 10------------------------------------------------------------------------------------------

cat("\n=== EXTREME VALUES IDENTIFICATION ===\n")

# Summary statistics for y
mean_y <- mean(data_clean$y)
sd_y <- sd(data_clean$y)

cat("Variable y:\n")
cat("  Mean:", round(mean_y, 2), "\n")
cat("  SD:", round(sd_y, 2), "\n")
cat("  Min:", min(data_clean$y), "\n")
cat("  Max:", max(data_clean$y), "\n")


## R Code 11------------------------------------------------------------------------------------------

# Observations with y beyond 3 SD from mean
outliers_y <- data_clean |>
  filter(abs(y - mean_y) > 3 * sd_y)

cat("\nObservations with |y - mean| > 3*SD:", nrow(outliers_y), "\n")


## R Code 12------------------------------------------------------------------------------------------

if (nrow(outliers_y) > 0) {
  cat("Outlier details:\n")
  print(outliers_y |> select(id, y, study_hrs, attend_pct, sleep_hrs))
}


## R Code 13------------------------------------------------------------------------------------------

# Count outliers per continuous variable
cat("\nNumber of outliers (> 3 SD) per variable:\n")
data_clean |>
  summarise(
    y = sum(abs(y - mean(y)) > 3 * sd(y)),
    study_hrs = sum(abs(study_hrs - mean(study_hrs)) > 3 * sd(study_hrs)),
    sleep_hrs = sum(abs(sleep_hrs - mean(sleep_hrs)) > 3 * sd(sleep_hrs)),
    attend_pct = sum(abs(attend_pct - mean(attend_pct)) > 3 * sd(attend_pct)),
    age = sum(abs(age - mean(age)) > 3 * sd(age))
  ) |>
  print()


# ------------------------------------------------------------------------------
# 1.6 Define variable lists for EDA
# ------------------------------------------------------------------------------

## R Code 14------------------------------------------------------------------------------------------

# Continuous variables
vars_continuous <- c("study_hrs", "sleep_hrs", "attend_pct", "age")

# Categorical variables
vars_categorical <- c("sexe", "school_type", "parent_educ", "sleep_qual",
                      "web_access", "trav_time", "extra_act", "study_method")

# Ordinal variables (subset of categorical)
vars_ordinal <- c("parent_educ", "sleep_qual", "trav_time")


## R Code 15------------------------------------------------------------------------------------------

cat("\n=== DATA MANAGEMENT COMPLETE ===\n")
cat("Continuous variables:", paste(vars_continuous, collapse = ", "), "\n")
cat("Categorical variables:", paste(vars_categorical, collapse = ", "), "\n")










# ==============================================================================
# 2. EXPLORATORY DATA ANALYSIS (EDA)
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.A Descriptive statistics - Continuous variables
# ------------------------------------------------------------------------------

## R Code 16------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.A DESCRIPTIVE STATISTICS - CONTINUOUS VARIABLES\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 17------------------------------------------------------------------------------------------

tab_continuous <- data_clean |>
  select(y, all_of(vars_continuous)) |>
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",
      "{median} ({p25}, {p75})",
      "{min}, {max}"
    ),
    digits = ~ 2
  ) |>
  bold_labels()

tab_continuous


# ------------------------------------------------------------------------------
# 2.B Descriptive statistics - Categorical variables
# ------------------------------------------------------------------------------

## R Code 18------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.B DESCRIPTIVE STATISTICS - CATEGORICAL VARIABLES\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 19------------------------------------------------------------------------------------------

for (var in vars_categorical) {
  cat("\n", var, ":\n", sep = "")
  data_clean |>
    count(!!sym(var)) |>
    mutate(pct = round(100 * n / sum(n), 1)) |>
    print()
}


# ------------------------------------------------------------------------------
# 2.C Distribution of y (response variable)
# ------------------------------------------------------------------------------

## R Code 20------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.C DISTRIBUTION OF y (RESPONSE VARIABLE)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 21------------------------------------------------------------------------------------------

# Shape statistics
cat("\nShape statistics for y:\n")
cat("  Skewness:", round(moments::skewness(data_clean$y), 3), "\n")
cat("  Excess Kurtosis:", round(moments::kurtosis(data_clean$y) - 3, 3), "\n")


## R Code 22------------------------------------------------------------------------------------------

# Histogram with density
p1_hist <- ggplot(data_clean, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "dodgerblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_clean$y), sd = sd(data_clean$y)),
                color = "darkgreen", linetype = 2, linewidth = 1) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(title = "Distribution of Exam Scores",
       subtitle = "Red = observed density, Green dashed = normal",
       x = "Exam Score (y)", y = "Density") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 23------------------------------------------------------------------------------------------

# Boxplot
p2_box <- ggplot(data_clean, aes(y = y)) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.shape = 1,
               linewidth = 0.25, width = 0.3) +
  labs(title = "Boxplot of y", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


## R Code 24------------------------------------------------------------------------------------------

# Q-Q plot
p3_qq <- ggplot(data_clean, aes(sample = y)) +
  stat_qq_band(alpha = 0.2, fill = "blue") +
  stat_qq_line(color = "red") +
  stat_qq_point(size = 0.5) +
  labs(title = "Q-Q Plot of y",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 25------------------------------------------------------------------------------------------

# Combined plot
p1_hist + p2_box + p3_qq + plot_layout(ncol = 3, widths = c(2, 1, 2))


# ------------------------------------------------------------------------------
# 2.D Correlations with y
# ------------------------------------------------------------------------------

## R Code 26------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.D CORRELATIONS WITH y (RESPONSE VARIABLE)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 27------------------------------------------------------------------------------------------

# Pearson correlations between y and continuous variables
cor_with_y <- data_clean |>
  select(y, all_of(vars_continuous)) |>
  correlation(method = "pearson") |>
  filter(Parameter1 == "y" | Parameter2 == "y") |>
  filter(Parameter1 != Parameter2) |>
  mutate(
    variable = if_else(Parameter1 == "y", Parameter2, Parameter1),
    r = round(r, 3)
  ) |>
  select(variable, r, p) |>
  arrange(desc(abs(r)))

cat("\nPearson correlations with y:\n")
print(cor_with_y)


# ------------------------------------------------------------------------------
# 2.E y vs Continuous predictors (scatterplots)
# ------------------------------------------------------------------------------

## R Code 28------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.E y vs CONTINUOUS PREDICTORS (SCATTERPLOTS)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 29------------------------------------------------------------------------------------------

p_scatter <- data_clean |>
  select(y, all_of(vars_continuous)) |>
  pivot_longer(cols = all_of(vars_continuous), 
               names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, 
                           levels = vars_continuous,
                           labels = vlabels(data_clean[vars_continuous]))) |>
  ggplot(aes(x = value, y = y)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  geom_point(alpha = 0.3, size = 1, color = "grey30") +
  geom_smooth(method = "loess", color = "red", se = TRUE, linewidth = 1) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, 
              linetype = 2, linewidth = 0.8) +
  labs(x = "Predictor value", y = "Exam Score (y)",
       title = "Exam Score vs Continuous Predictors",
       subtitle = "Red = LOESS smooth, Blue dashed = Linear fit") +
  theme_bw(base_size = 12) +
  labs_pubr() +
  theme(strip.text = element_text(size = 11, face = "bold"))

print(p_scatter)


# ------------------------------------------------------------------------------
# 2.F y vs Categorical predictors (boxplots)
# ------------------------------------------------------------------------------

## R Code 30------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.F y vs CATEGORICAL PREDICTORS (BOXPLOTS)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 31------------------------------------------------------------------------------------------

p_boxplots <- data_clean |>
  select(y, all_of(vars_categorical)) |>
  pivot_longer(cols = all_of(vars_categorical), 
               names_to = "variable", values_to = "level") |>
  mutate(variable = factor(variable, 
                           levels = vars_categorical,
                           labels = vlabels(data_clean[vars_categorical]))) |>
  ggplot(aes(x = level, y = y)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  geom_boxplot(fill = "lightblue", linewidth = 0.25, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(x = NULL, y = "Exam Score (y)",
       title = "Exam Score by Categorical Predictors",
       subtitle = "Red diamond = mean") +
  theme_bw(base_size = 11) +
  labs_pubr() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

print(p_boxplots)


# ------------------------------------------------------------------------------
# 2.G Mean y by group (raw effects)
# ------------------------------------------------------------------------------

## R Code 32------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.G MEAN y BY GROUP (RAW EFFECTS)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 33------------------------------------------------------------------------------------------

# Function to calculate means by group
mean_by_group <- function(data, y_var, group_var) {
  data |>
    group_by(level = !!sym(group_var)) |>
    summarise(
      n = n(),
      mean_y = round(mean(!!sym(y_var)), 1),
      sd_y = round(sd(!!sym(y_var)), 1),
      .groups = "drop"
    ) |>
    mutate(
      variable = group_var,
      diff_vs_ref = round(mean_y - first(mean_y), 1)
    ) |>
    select(variable, level, n, mean_y, sd_y, diff_vs_ref)
}


## R Code 34------------------------------------------------------------------------------------------

# Calculate for all categorical variables
means_all <- map_dfr(vars_categorical, ~ mean_by_group(data_clean, "y", .x))

print(means_all, n = 40)


# ------------------------------------------------------------------------------
# 2.H Monotone trends for ordinal variables
# ------------------------------------------------------------------------------

## R Code 35------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.H MONOTONE TRENDS (ORDINAL VARIABLES)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 36------------------------------------------------------------------------------------------

# Calculate means for ordinal variables
ordinal_means <- map_dfr(vars_ordinal, function(var) {
  data_clean |>
    group_by(level = !!sym(var)) |>
    summarise(
      n = n(),
      mean_y = mean(y),
      se_y = sd(y) / sqrt(n()),
      .groups = "drop"
    ) |>
    mutate(variable = var, .before = 1)
})

print(ordinal_means, n = 20)


## R Code 37------------------------------------------------------------------------------------------

# Trend plot
p_ordinal <- ordinal_means |>
  mutate(variable = factor(variable, 
                           levels = vars_ordinal,
                           labels = vlabels(data_clean[vars_ordinal]))) |>
  ggplot(aes(x = level, y = mean_y, group = 1)) +
  facet_wrap(~ variable, scales = "free_x") +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbar(aes(ymin = mean_y - 1.96*se_y, ymax = mean_y + 1.96*se_y),
                width = 0.2, color = "darkred") +
  labs(x = NULL, y = "Mean Exam Score",
       title = "Monotone Trend Check for Ordinal Predictors",
       subtitle = "Error bars = 95% CI") +
  theme_bw(base_size = 12) +
  labs_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_ordinal)


# ------------------------------------------------------------------------------
# 2.I Correlations between predictors (collinearity)
# ------------------------------------------------------------------------------

## R Code 38------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.I CORRELATIONS BETWEEN PREDICTORS (COLLINEARITY)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 39------------------------------------------------------------------------------------------

# Pairs plot (shows correlations, scatterplots, and densities)
p_pairs <- data_clean |>
  select(y, all_of(vars_continuous)) |>
  ggpairs(
    lower = list(continuous = wrap("points", size = 0.5, alpha = 0.2)),
    diag = list(continuous = wrap("densityDiag", fill = "dodgerblue", alpha = 0.5)),
    upper = list(continuous = wrap("cor", size = 4))
  ) +
  theme_bw(base_size = 10) +
  labs(title = "Pairs Plot: y and Continuous Predictors")

print(p_pairs)


# ------------------------------------------------------------------------------
# 2.J Associations between categorical predictors
# ------------------------------------------------------------------------------

## R Code 40------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.J ASSOCIATIONS BETWEEN CATEGORICAL PREDICTORS\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 41------------------------------------------------------------------------------------------

# Cross-table: school_type vs parent_educ
cat("\nCross-table: school_type vs parent_educ\n")
tabyl(data_clean, school_type, parent_educ) |>
  adorn_totals(c("row", "col")) |>
  adorn_percentages("row") |>
  adorn_pct_formatting(digits = 1) |>
  adorn_ns(position = "front") |>
  print()


## R Code 42------------------------------------------------------------------------------------------

# Stacked barplots for confounding check
vars_cat_check <- c("parent_educ", "web_access", "extra_act")

p_confound <- data_clean |>
  select(school_type, all_of(vars_cat_check)) |>
  pivot_longer(cols = vars_cat_check, names_to = "variable", values_to = "value") |>
  ggplot(aes(x = school_type, fill = value)) +
  geom_bar(position = "fill", width = 0.7) +
  facet_wrap(vars(variable), scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Structural Confounding Check",
       subtitle = "Does school composition vary by background?",
       y = "Proportion", x = "School Type") +
  theme_bw(base_size = 12) +
  labs_pubr() +
  theme(legend.position = "bottom")

print(p_confound)


# ------------------------------------------------------------------------------
# 2.J-bis Mixed associations (numeric vs categorical)
# ------------------------------------------------------------------------------

## R Code 43------------------------------------------------------------------------------------------

cat("\n=== MIXED ASSOCIATIONS: NUMERIC vs CATEGORICAL ===\n")


## R Code 44------------------------------------------------------------------------------------------

# Boxplots: numeric variables by categorical variables
p_mixed <- data_clean |>
  select(study_hrs, attend_pct, school_type, parent_educ, extra_act) |>
  pivot_longer(cols = c(study_hrs, attend_pct), 
               names_to = "numeric_var", values_to = "numeric_value") |>
  pivot_longer(cols = c(school_type, parent_educ, extra_act),
               names_to = "cat_var", values_to = "cat_value") |>
  ggplot(aes(x = cat_value, y = numeric_value)) +
  geom_boxplot(fill = "lightgreen") +
  facet_grid(numeric_var ~ cat_var, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

print(p_mixed)


# ------------------------------------------------------------------------------
# 2.K Linearity check (detailed)
# ------------------------------------------------------------------------------

## R Code 45------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("2.K LINEARITY CHECK (QUARTILE ANALYSIS)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 46------------------------------------------------------------------------------------------

# Mean y by quartile of study_hrs
cat("\nMean y by quartile of study_hrs:\n")
data_clean |>
  mutate(quartile = ntile(study_hrs, 4)) |>
  group_by(quartile) |>
  summarise(
    range = paste0("[", round(min(study_hrs), 1), "-", round(max(study_hrs), 1), "]"),
    mean_y = round(mean(y), 1),
    n = n()
  ) |>
  mutate(increment = mean_y - lag(mean_y)) |>
  print()


## R Code 47------------------------------------------------------------------------------------------

# Mean y by quartile of attend_pct
cat("\nMean y by quartile of attend_pct:\n")
data_clean |>
  mutate(quartile = ntile(attend_pct, 4)) |>
  group_by(quartile) |>
  summarise(
    range = paste0("[", round(min(attend_pct), 1), "-", round(max(attend_pct), 1), "]"),
    mean_y = round(mean(y), 1),
    n = n()
  ) |>
  mutate(increment = mean_y - lag(mean_y)) |>
  print()


## R Code 48------------------------------------------------------------------------------------------

# Mean y by quartile of sleep_hrs
cat("\nMean y by quartile of sleep_hrs:\n")
data_clean |>
  mutate(quartile = ntile(sleep_hrs, 4)) |>
  group_by(quartile) |>
  summarise(
    range = paste0("[", round(min(sleep_hrs), 1), "-", round(max(sleep_hrs), 1), "]"),
    mean_y = round(mean(y), 1),
    n = n()
  ) |>
  mutate(increment = mean_y - lag(mean_y)) |>
  print()


# ==============================================================================
# END OF EDA
# ==============================================================================

## R Code 49------------------------------------------------------------------------------------------

cat("\n=== END OF EDA (SECTIONS 1-2) ===\n")















# ==============================================================================
# 3. BUILDING REGRESSION MODELS
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.A Validation Strategy (Train/Test Split)
# ------------------------------------------------------------------------------

## R Code 50------------------------------------------------------------------------------------------

set.seed(42)  # Required for reproducibility

data_clean <- data_clean |>
  mutate(split_tag = rbinom(n(), size = 1, prob = 0.70))

data_train <- data_clean |> filter(split_tag == 1)
data_test  <- data_clean |> filter(split_tag == 0)


## R Code 51------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.A TRAIN/TEST SPLIT\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("Train size:", nrow(data_train), "obs (", 
    round(nrow(data_train)/nrow(data_clean)*100, 1), "%)\n")
cat("Test size :", nrow(data_test), "obs (", 
    round(nrow(data_test)/nrow(data_clean)*100, 1), "%)\n")


# ------------------------------------------------------------------------------
# 3.B Baseline Model (Null Model)
# ------------------------------------------------------------------------------

## R Code 52------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.B BASELINE MODEL (NULL MODEL)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")

mod_null <- lm(y ~ 1, data = data_train)
cat("\nNull Model: y ~ 1 (mean only)\n")
cat("Mean of y (train):", round(coef(mod_null), 2), "\n")
cat("RSS (Residual Sum of Squares):", round(deviance(mod_null), 1), "\n")


# ------------------------------------------------------------------------------
# 3.C Simple Regressions (Intuition Building)
# ------------------------------------------------------------------------------

## R Code 53------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.C SIMPLE REGRESSIONS (INTUITION)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 54------------------------------------------------------------------------------------------

# Top 3 continuous predictors (by correlation with y)
mod_simple_attend <- lm(y ~ attend_pct, data = data_train)
mod_simple_study  <- lm(y ~ study_hrs, data = data_train)
mod_simple_sleep  <- lm(y ~ sleep_hrs, data = data_train)


## R Code 55------------------------------------------------------------------------------------------

# Summary table of simple regressions
simple_models <- tibble(
  Predictor = c("attend_pct", "study_hrs", "sleep_hrs"),
  R2 = c(summary(mod_simple_attend)$r.squared,
         summary(mod_simple_study)$r.squared,
         summary(mod_simple_sleep)$r.squared),
  Slope = c(coef(mod_simple_attend)[2],
            coef(mod_simple_study)[2],
            coef(mod_simple_sleep)[2]),
  p_value = c(summary(mod_simple_attend)$coefficients[2, 4],
              summary(mod_simple_study)$coefficients[2, 4],
              summary(mod_simple_sleep)$coefficients[2, 4])
) |>
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\nSimple regressions - Summary:\n")
print(simple_models)


# ------------------------------------------------------------------------------
# 3.D Candidate Models Construction
# ------------------------------------------------------------------------------

## R Code 56------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.D CANDIDATE MODELS\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 57------------------------------------------------------------------------------------------

# Model 1: Core (key continuous variables only)
mod1_core <- lm(y ~ attend_pct + study_hrs + sleep_hrs, 
                data = data_train)

cat("\n--- Model 1: Core (3 continuous variables) ---\n")
cat("Formula: y ~ attend_pct + study_hrs + sleep_hrs\n")
cat("Adjusted R²:", round(summary(mod1_core)$adj.r.squared, 4), "\n")


## R Code 58------------------------------------------------------------------------------------------

# Model 2: Core + Strong categorical variables
mod2_extended <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                      trav_time + sleep_qual + parent_educ,
                    data = data_train)

cat("\n--- Model 2: Extended (+ strong categorical) ---\n")
cat("Formula: y ~ attend_pct + study_hrs + sleep_hrs + trav_time + sleep_qual + parent_educ\n")
cat("Adjusted R²:", round(summary(mod2_extended)$adj.r.squared, 4), "\n")


## R Code 59------------------------------------------------------------------------------------------

# Model 3: Full (all relevant variables, WITHOUT age/sexe)
mod3_full <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                  trav_time + sleep_qual + parent_educ +
                  extra_act + web_access + school_type + study_method,
                data = data_train)

cat("\n--- Model 3: Full (all relevant variables, WITHOUT age/sexe) ---\n")
cat("Formula: y ~ attend_pct + study_hrs + sleep_hrs + trav_time + sleep_qual +\n")
cat("             parent_educ + extra_act + web_access + school_type + study_method\n")
cat("Adjusted R²:", round(summary(mod3_full)$adj.r.squared, 4), "\n")


# ------------------------------------------------------------------------------
# 3.E Systematic Interaction Testing
# ------------------------------------------------------------------------------

## R Code 60------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.E SYSTEMATIC INTERACTION TESTING\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")

## R Code 61------------------------------------------------------------------------------------------

# Base model: mod3_full (no interactions)
# We test plausible interactions based on EDA findings

cat("\n--- Testing candidate interactions ---\n\n")

# Candidate 1: study_hrs × sleep_qual
# Hypothesis: Sleep quality moderates the effectiveness of study hours
mod4_interact <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                      trav_time + sleep_qual + parent_educ +
                      extra_act + web_access + school_type + study_method +
                      study_hrs:sleep_qual,
                    data = data_train)

# Candidate 2: study_hrs × attend_pct
# Hypothesis: Attendance and study hours have synergistic effect
mod_int_B <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                  trav_time + sleep_qual + parent_educ +
                  extra_act + web_access + school_type + study_method +
                  study_hrs:attend_pct,
                data = data_train)

# Candidate 3: attend_pct × sleep_qual
# Hypothesis: Sleep quality moderates the effect of attendance
mod_int_C <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                  trav_time + sleep_qual + parent_educ +
                  extra_act + web_access + school_type + study_method +
                  attend_pct:sleep_qual,
                data = data_train)

# Candidate 4: study_hrs × parent_educ
# Hypothesis: Parental education moderates study effectiveness
mod_int_D <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                  trav_time + sleep_qual + parent_educ +
                  extra_act + web_access + school_type + study_method +
                  study_hrs:parent_educ,
                data = data_train)

# F-tests for each interaction (vs base model mod3_full)
cat("--- F-tests: Each interaction vs Base model (mod3_full) ---\n\n")

cat("Interaction 1: study_hrs × sleep_qual\n")
anova(mod3_full, mod4_interact) |> print()

cat("\nInteraction 2: study_hrs × attend_pct\n")
anova(mod3_full, mod_int_B) |> print()

cat("\nInteraction 3: attend_pct × sleep_qual\n")
anova(mod3_full, mod_int_C) |> print()

cat("\nInteraction 4: study_hrs × parent_educ\n")
anova(mod3_full, mod_int_D) |> print()

# AIC/BIC comparison
cat("\n--- AIC/BIC Comparison of all interaction models ---\n\n")

compare_performance(mod3_full, mod4_interact, mod_int_B, mod_int_C, mod_int_D,
                    metrics = c("AIC", "BIC", "R2_adj")) |>
  print()

# Summary table
cat("\n--- INTERACTION SELECTION SUMMARY ---\n\n")

interaction_summary <- data.frame(
  Interaction = c("None (mod3_full)", "study_hrs:sleep_qual", "study_hrs:attend_pct", 
                  "attend_pct:sleep_qual", "study_hrs:parent_educ"),
  AIC = c(AIC(mod3_full), AIC(mod4_interact), AIC(mod_int_B), AIC(mod_int_C), AIC(mod_int_D)),
  BIC = c(BIC(mod3_full), BIC(mod4_interact), BIC(mod_int_B), BIC(mod_int_C), BIC(mod_int_D)),
  Adj_R2 = c(summary(mod3_full)$adj.r.squared, 
             summary(mod4_interact)$adj.r.squared,
             summary(mod_int_B)$adj.r.squared,
             summary(mod_int_C)$adj.r.squared,
             summary(mod_int_D)$adj.r.squared)
) |>
  mutate(across(c(AIC, BIC), \(x) round(x, 1)),
         Adj_R2 = round(Adj_R2, 4)) |>
  arrange(AIC)

print(interaction_summary, row.names = FALSE)

cat("\n→ SELECTED: study_hrs:sleep_qual (lowest AIC, highest Adj R², significant F-test)\n")

# Clean up temporary models
rm(mod_int_B, mod_int_C, mod_int_D)

## R Code 62------------------------------------------------------------------------------------------

# F-test for interaction significance
cat("\nF-test for interaction study_hrs:sleep_qual:\n")
anova_interact <- anova(mod3_full, mod4_interact)
print(anova_interact)


# ------------------------------------------------------------------------------
# 3.F Model Comparison (multiple criteria)
# ------------------------------------------------------------------------------

## R Code 63------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.F MODEL COMPARISON\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 64------------------------------------------------------------------------------------------

# Model comparison table using compare_performance()
compare_performance(mod_null, mod1_core, mod2_extended, mod3_full, mod4_interact,
                    metrics = c("AIC", "BIC", "R2", "R2_adj", "RMSE", "SIGMA")) |>
  print()


# ------------------------------------------------------------------------------
# 3.G Nested F-tests (inferential criterion)
# ------------------------------------------------------------------------------

## R Code 65------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.G NESTED F-TESTS\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 66------------------------------------------------------------------------------------------

# Test 1: Core vs Extended
cat("\n--- Test 1: M1_Core vs M2_Extended ---\n")
anova(mod1_core, mod2_extended) |> print()


## R Code 67------------------------------------------------------------------------------------------

# Test 2: Extended vs Full
cat("\n--- Test 2: M2_Extended vs M3_Full ---\n")
anova(mod2_extended, mod3_full) |> print()


## R Code 68------------------------------------------------------------------------------------------

# Test 3: Full vs Full+Interaction
cat("\n--- Test 3: M3_Full vs M4_Interact ---\n")
anova(mod3_full, mod4_interact) |> print()


# ------------------------------------------------------------------------------
# 3.H Final Model Selection
# ------------------------------------------------------------------------------

## R Code 69------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("3.H FINAL MODEL SELECTION\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 70------------------------------------------------------------------------------------------

# Final model: mod4_interact (Full + interaction study_hrs:sleep_qual)
final_model <- mod4_interact

cat("\n--- FINAL MODEL SELECTED: mod4_interact ---\n")
cat("Formula: y ~ attend_pct + study_hrs + sleep_hrs + trav_time + sleep_qual +\n")
cat("             parent_educ + extra_act + web_access + school_type + study_method +\n")
cat("             study_hrs:sleep_qual\n\n")
cat("Adjusted R²:", round(summary(final_model)$adj.r.squared, 4), "\n")
cat("AIC:", round(AIC(final_model), 2), "\n")
cat("BIC:", round(BIC(final_model), 2), "\n")
cat("Number of parameters:", length(coef(final_model)), "\n")


## R Code 71------------------------------------------------------------------------------------------

# Coefficients with 95% CI
cat("\n--- Coefficients with 95% confidence intervals ---\n")
model_parameters(final_model, ci = 0.95, ci_method = "residual") |>
  print()







# ==============================================================================
# 4. DIAGNOSTICS, ASSUMPTIONS AND ROBUSTNESS
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.A Helper functions for diagnostics
# ------------------------------------------------------------------------------

## R Code 72------------------------------------------------------------------------------------------

# Function 1: Residuals vs Fitted
resid_vs_fitted <- function(model) {
  augment(model) |>
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.3, size = 1, color = "steelblue") +
    geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 1) +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values", y = "Residuals") +
    theme_bw(base_size = 12) +
    labs_pubr()
}


## R Code 73------------------------------------------------------------------------------------------

# Function 2: Scale-Location plot
scale_location_plot <- function(model) {
  augment(model) |>
    mutate(sqrt_std_resid = sqrt(abs(.std.resid))) |>
    ggplot(aes(x = .fitted, y = sqrt_std_resid)) +
    geom_point(alpha = 0.3, size = 1, color = "steelblue") +
    geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 1) +
    labs(title = "Scale-Location",
         x = "Fitted values", y = expression(sqrt("|Standardized residuals|"))) +
    theme_bw(base_size = 12) +
    labs_pubr()
}


## R Code 74------------------------------------------------------------------------------------------

# Function 3: Q-Q plot
qq_plot <- function(model) {
  tibble(std_resid = rstandard(model)) |>
    ggplot(aes(sample = std_resid)) +
    stat_qq_band(alpha = 0.2, fill = "blue") +
    stat_qq_line(color = "red", linewidth = 1) +
    stat_qq_point(size = 0.5, alpha = 0.5) +
    labs(title = "Normal Q-Q Plot",
         x = "Theoretical quantiles", y = "Standardized residuals") +
    theme_bw(base_size = 12) +
    labs_pubr()
}


## R Code 75------------------------------------------------------------------------------------------

# Function 4: Cook's Distance
cooks_distance_plot <- function(model) {
  n <- nobs(model)
  threshold <- 4 / n
  
  augment(model) |>
    mutate(obs = row_number(),
           influential = .cooksd > threshold) |>
    ggplot(aes(x = obs, y = .cooksd)) +
    geom_bar(stat = "identity", width = 0.5, 
             aes(fill = influential), show.legend = FALSE) +
    geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +
    scale_fill_manual(values = c("steelblue", "red")) +
    labs(title = "Cook's Distance",
         subtitle = paste0("Threshold = 4/n = ", round(threshold, 4)),
         x = "Observation", y = "Cook's Distance") +
    theme_bw(base_size = 12) +
    labs_pubr()
}


## R Code 76------------------------------------------------------------------------------------------

# Function 5: Residuals vs Predictor
resid_vs_predictor <- function(model, data, predictor) {
  tibble(
    residuals = residuals(model),
    predictor_value = data[[predictor]]
  ) |>
    ggplot(aes(x = predictor_value, y = residuals)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.3, size = 1, color = "steelblue") +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste("Residuals vs", predictor),
         x = predictor, y = "Residuals") +
    theme_bw(base_size = 12) +
    labs_pubr()
}


# ------------------------------------------------------------------------------
# 4.B Main diagnostic plots
# ------------------------------------------------------------------------------

## R Code 77------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("4.B MAIN DIAGNOSTIC PLOTS\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 78------------------------------------------------------------------------------------------

# Create the 4 main diagnostic plots
p_resid_fitted <- resid_vs_fitted(final_model)
p_scale_loc <- scale_location_plot(final_model)
p_qq <- qq_plot(final_model)
p_cooks <- cooks_distance_plot(final_model)


## R Code 79------------------------------------------------------------------------------------------

# Combined display
(p_resid_fitted + p_scale_loc) / (p_qq + p_cooks) +
  plot_annotation(title = "Diagnostic Plots for Final Model")


# ------------------------------------------------------------------------------
# 4.C Residuals vs Predictors (in model)
# ------------------------------------------------------------------------------

## R Code 80------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("4.C RESIDUALS vs PREDICTORS (IN MODEL)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 81------------------------------------------------------------------------------------------

# Residuals vs continuous predictors
p_resid_attend <- resid_vs_predictor(final_model, data_train, "attend_pct")
p_resid_study <- resid_vs_predictor(final_model, data_train, "study_hrs")
p_resid_sleep <- resid_vs_predictor(final_model, data_train, "sleep_hrs")

(p_resid_attend + p_resid_study + p_resid_sleep) +
  plot_annotation(title = "Residuals vs Continuous Predictors")


# ------------------------------------------------------------------------------
# 4.D Residuals vs Omitted Variables
# ------------------------------------------------------------------------------

## R Code 82------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("4.D RESIDUALS vs OMITTED VARIABLES\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 83------------------------------------------------------------------------------------------

# Residuals vs age (omitted continuous variable)
p_resid_age <- resid_vs_predictor(final_model, data_train, "age") +
  labs(title = "Residuals vs Age (OMITTED)")

print(p_resid_age)


## R Code 84------------------------------------------------------------------------------------------

# Residuals vs sexe (omitted categorical variable)
p_resid_sexe <- tibble(
  residuals = residuals(final_model),
  sexe = data_train$sexe
) |>
  ggplot(aes(x = sexe, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Residuals vs Gender (OMITTED)",
       x = "Gender", y = "Residuals") +
  theme_bw(base_size = 12) +
  labs_pubr()

print(p_resid_sexe)


## R Code 85------------------------------------------------------------------------------------------

# Combined omitted variables plot
(p_resid_age + p_resid_sexe) +
  plot_annotation(title = "Residuals vs Omitted Variables")


# ------------------------------------------------------------------------------
# 4.E Formal hypothesis tests
# ------------------------------------------------------------------------------

## R Code 86------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("4.E FORMAL HYPOTHESIS TESTS\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 87------------------------------------------------------------------------------------------

# Test 1: Homoscedasticity (Breusch-Pagan / NCV)
cat("\n--- Test 1: Homoscedasticity (Breusch-Pagan) ---\n")
cat("H0: Constant variance of residuals\n\n")
car::ncvTest(final_model)


## R Code 88------------------------------------------------------------------------------------------

# Test 2: Independence (Durbin-Watson)
cat("\n--- Test 2: Independence (Durbin-Watson) ---\n")
cat("H0: No autocorrelation of residuals\n")
cat("Expected value under H0 ≈ 2\n\n")
car::durbinWatsonTest(final_model)


## R Code 89------------------------------------------------------------------------------------------

# Test 3: Normality (Shapiro-Wilk)
cat("\n--- Test 3: Normality (Shapiro-Wilk) ---\n")
cat("H0: Residuals follow normal distribution\n\n")

std_resid <- rstandard(final_model)
if (length(std_resid) > 5000) {
  set.seed(42)
  sample_resid <- sample(std_resid, 5000)
  cat("Note: Test on sample of 5000 obs (Shapiro limit)\n\n")
} else {
  sample_resid <- std_resid
}

shapiro.test(sample_resid)


# ------------------------------------------------------------------------------
# 4.F Influential observations (Cook's Distance)
# ------------------------------------------------------------------------------

## R Code 90------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("4.F INFLUENTIAL OBSERVATIONS (COOK'S DISTANCE)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 91------------------------------------------------------------------------------------------

# Identify influential points
n_train <- nobs(final_model)
cooks_threshold <- 4 / n_train

cat("Threshold: 4/n =", round(cooks_threshold, 5), "\n\n")

influential_obs <- augment(final_model) |>
  mutate(obs_id = row_number()) |>
  filter(.cooksd > cooks_threshold) |>
  arrange(desc(.cooksd))

cat("Number of influential points:", nrow(influential_obs), "\n")
cat("Proportion:", round(100 * nrow(influential_obs) / n_train, 2), "%\n")


## R Code 92------------------------------------------------------------------------------------------

# Top 10 most influential points
cat("\nTop 10 most influential points:\n")
influential_obs |>
  select(obs_id, y, .fitted, .resid, .cooksd) |>
  head(10) |>
  print()


# ------------------------------------------------------------------------------
# 4.G End of diagnostics
# ------------------------------------------------------------------------------

## R Code 93------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("END OF DIAGNOSTICS (SECTION 4)\n")






# ==============================================================================
# 5. INTERPRETATION AND INFERENCE
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.A Model equation and reference categories
# ------------------------------------------------------------------------------

## R Code 94------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("5.A MODEL EQUATION AND REFERENCE CATEGORIES\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 95------------------------------------------------------------------------------------------

# Model formula
cat("\n--- Model formula ---\n")
print(formula(final_model))


## R Code 96------------------------------------------------------------------------------------------

# Reference categories
cat("\n--- Reference categories (absorbed in Intercept) ---\n\n")

ref_categories <- data.frame(
  Variable = c("school_type", "parent_educ", "sleep_qual", "trav_time", 
               "extra_act", "web_access", "study_method"),
  Reference = c(
    levels(data_train$school_type)[1],
    levels(data_train$parent_educ)[1],
    levels(data_train$sleep_qual)[1],
    levels(data_train$trav_time)[1],
    levels(data_train$extra_act)[1],
    levels(data_train$web_access)[1],
    levels(data_train$study_method)[1]
  )
)
print(ref_categories, row.names = FALSE)


# ------------------------------------------------------------------------------
# 5.B Coefficient table with SE and 95% CI
# ------------------------------------------------------------------------------

## R Code 97------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("5.B COEFFICIENT TABLE (Estimates, SE, 95% CI)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 98------------------------------------------------------------------------------------------

# Extract coefficients and CI
model_summary <- summary(final_model)
coefficients_table <- model_summary$coefficients
ci <- confint(final_model, level = 0.95)

# Create complete table
coeffs_df <- data.frame(
  Term = rownames(coefficients_table),
  Estimate = round(coefficients_table[, "Estimate"], 4),
  Std.Error = round(coefficients_table[, "Std. Error"], 4),
  CI_2.5 = round(ci[, 1], 4),
  CI_97.5 = round(ci[, 2], 4),
  t.value = round(coefficients_table[, "t value"], 3),
  p.value = coefficients_table[, "Pr(>|t|)"],
  Signif = ifelse(coefficients_table[, "Pr(>|t|)"] < 0.001, "***",
                  ifelse(coefficients_table[, "Pr(>|t|)"] < 0.01, "**",
                         ifelse(coefficients_table[, "Pr(>|t|)"] < 0.05, "*",
                                ifelse(coefficients_table[, "Pr(>|t|)"] < 0.1, ".", ""))))
)
rownames(coeffs_df) <- NULL

cat("\n--- Complete coefficient table ---\n\n")
print(coeffs_df, row.names = FALSE)

cat("\nSignif. codes: '***' p<0.001, '**' p<0.01, '*' p<0.05, '.' p<0.1\n")


## R Code 99------------------------------------------------------------------------------------------

# Significant variables (p < 0.05)
cat("\n--- Significant variables (p < 0.05) ---\n\n")
signif_vars <- coeffs_df[coeffs_df$p.value < 0.05, c("Term", "Estimate", "p.value", "Signif")]
print(signif_vars, row.names = FALSE)


## R Code 100------------------------------------------------------------------------------------------

# Non-significant variables (p >= 0.05)
cat("\n--- Non-significant variables (p >= 0.05) ---\n\n")
non_signif_vars <- coeffs_df[coeffs_df$p.value >= 0.05, c("Term", "Estimate", "p.value")]
print(non_signif_vars, row.names = FALSE)


# ------------------------------------------------------------------------------
# 5.C Interaction effect: study_hrs by sleep_qual
# ------------------------------------------------------------------------------

## R Code 101------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("5.C INTERACTION EFFECT: study_hrs × sleep_qual\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 102------------------------------------------------------------------------------------------

# Extract relevant coefficients
coefs <- coef(final_model)
beta_study <- coefs["study_hrs"]
beta_interact_avg <- coefs["study_hrs:sleep_qualAverage"]
beta_interact_good <- coefs["study_hrs:sleep_qualGood"]

# Total effect of +1h study by sleep quality level
effet_poor <- beta_study
effet_avg <- beta_study + beta_interact_avg
effet_good <- beta_study + beta_interact_good

effects_table <- data.frame(
  sleep_qual = c("Poor (reference)", "Average", "Good"),
  effect_per_hour = round(c(effet_poor, effet_avg, effet_good), 3)
)

cat("\n--- Effect of +1h study by sleep quality level ---\n\n")
print(effects_table, row.names = FALSE)


# ------------------------------------------------------------------------------
# 5.D Interaction plot
# ------------------------------------------------------------------------------

## R Code 103------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("5.D INTERACTION PLOT\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 104------------------------------------------------------------------------------------------

# Create prediction grid
study_vals <- seq(0, 15, by = 0.5)
sleep_levels <- levels(data_train$sleep_qual)

grid_list <- list()
for (sq in sleep_levels) {
  temp_df <- data.frame(
    study_hrs = study_vals,
    sleep_qual = factor(sq, levels = sleep_levels),
    attend_pct = median(data_train$attend_pct),
    sleep_hrs = median(data_train$sleep_hrs),
    school_type = factor("Public", levels = levels(data_train$school_type)),
    parent_educ = factor("No Formal", levels = levels(data_train$parent_educ)),
    trav_time = factor("<15 Min", levels = levels(data_train$trav_time)),
    extra_act = factor("No", levels = levels(data_train$extra_act)),
    web_access = factor("No", levels = levels(data_train$web_access)),
    study_method = factor("Online Videos", levels = levels(data_train$study_method))
  )
  temp_df$predicted <- predict(final_model, newdata = temp_df)
  grid_list[[sq]] <- temp_df
}

grid_interact <- do.call(rbind, grid_list)


## R Code 105------------------------------------------------------------------------------------------

# Interaction plot
plot_interaction <- ggplot(grid_interact, 
                           aes(x = study_hrs, y = predicted, 
                               color = sleep_qual, linetype = sleep_qual)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("Poor" = "#E41A1C", "Average" = "#FF7F00", "Good" = "#4DAF4A"),
                     name = "Sleep Quality") +
  scale_linetype_manual(values = c("Poor" = "dashed", "Average" = "dotdash", "Good" = "solid"),
                        name = "Sleep Quality") +
  labs(
    title = "Interaction: Study Hours × Sleep Quality",
    subtitle = "Effect of study hours depends on sleep quality",
    x = "Study hours per day",
    y = "Predicted score"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

print(plot_interaction)


# ------------------------------------------------------------------------------
# 5.E Scenario-based predictions
# ------------------------------------------------------------------------------

## R Code 106------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("5.E SCENARIO-BASED PREDICTIONS\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 107------------------------------------------------------------------------------------------

# Profile A: Struggling student
scenario_A <- data.frame(
  attend_pct = 60, study_hrs = 2, sleep_hrs = 5,
  school_type = factor("Public", levels = levels(data_train$school_type)),
  parent_educ = factor("No Formal", levels = levels(data_train$parent_educ)),
  sleep_qual = factor("Poor", levels = levels(data_train$sleep_qual)),
  trav_time = factor(">60 Min", levels = levels(data_train$trav_time)),
  extra_act = factor("No", levels = levels(data_train$extra_act)),
  web_access = factor("No", levels = levels(data_train$web_access)),
  study_method = factor("Online Videos", levels = levels(data_train$study_method))
)

# Profile B: Engaged student
scenario_B <- data.frame(
  attend_pct = 95, study_hrs = 10, sleep_hrs = 8,
  school_type = factor("Public", levels = levels(data_train$school_type)),
  parent_educ = factor("PhD", levels = levels(data_train$parent_educ)),
  sleep_qual = factor("Good", levels = levels(data_train$sleep_qual)),
  trav_time = factor("<15 Min", levels = levels(data_train$trav_time)),
  extra_act = factor("Yes", levels = levels(data_train$extra_act)),
  web_access = factor("Yes", levels = levels(data_train$web_access)),
  study_method = factor("Mixed", levels = levels(data_train$study_method))
)


## R Code 108------------------------------------------------------------------------------------------

# Predictions with 95% CI
pred_A <- predict(final_model, newdata = scenario_A, interval = "confidence", level = 0.95)
pred_B <- predict(final_model, newdata = scenario_B, interval = "confidence", level = 0.95)

cat("\n--- Predictions with 95% confidence intervals ---\n")

cat("\nProfile A (Struggling student):\n")
cat("  Predicted score:", round(pred_A[1, "fit"], 2), "\n")
cat("  95% CI: [", round(pred_A[1, "lwr"], 2), ";", round(pred_A[1, "upr"], 2), "]\n")

cat("\nProfile B (Engaged student):\n")
cat("  Predicted score:", round(pred_B[1, "fit"], 2), "\n")
cat("  95% CI: [", round(pred_B[1, "lwr"], 2), ";", round(pred_B[1, "upr"], 2), "]\n")

cat("\nDifference (B - A):", round(pred_B[1, "fit"] - pred_A[1, "fit"], 2), "points\n")


# ------------------------------------------------------------------------------
# 5.F Overall model fit
# ------------------------------------------------------------------------------

## R Code 109------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("5.F OVERALL MODEL FIT\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 110------------------------------------------------------------------------------------------

model_summary <- summary(final_model)

metrics_df <- data.frame(
  Metric = c("R²", "Adjusted R²", "Residual Std. Error", "F-statistic", 
             "Nb parameters", "Nb observations"),
  Value = c(
    round(model_summary$r.squared, 4),
    round(model_summary$adj.r.squared, 4),
    round(model_summary$sigma, 4),
    round(model_summary$fstatistic[1], 2),
    length(coef(final_model)),
    nobs(final_model)
  )
)

cat("\n--- Model fit metrics ---\n\n")
print(metrics_df, row.names = FALSE)


# ==============================================================================
# 6. PREDICTIVE PERFORMANCE
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.A Predictions on test set
# ------------------------------------------------------------------------------

## R Code 111------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("6.A PREDICTIONS ON TEST SET\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 112------------------------------------------------------------------------------------------

# Predictions for different models
pred_null  <- predict(mod_null, newdata = data_test)
pred_full  <- predict(mod3_full, newdata = data_test)
pred_final <- predict(final_model, newdata = data_test)

cat("\nTest set size:", nrow(data_test), "observations\n")
cat("Observed y range: [", round(min(data_test$y), 1), ",", round(max(data_test$y), 1), "]\n")
cat("Predicted ŷ range: [", round(min(pred_final), 1), ",", round(max(pred_final), 1), "]\n")


# ------------------------------------------------------------------------------
# 6.B Performance metrics (MSE, RMSE, MedAE, R²)
# ------------------------------------------------------------------------------

## R Code 113------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("6.B PERFORMANCE METRICS (MSE, RMSE, MedAE, R²)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 114------------------------------------------------------------------------------------------

# Function to calculate metrics
calc_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  mse   <- mean(residuals^2)
  rmse  <- sqrt(mse)
  mae   <- mean(abs(residuals))
  medae <- median(abs(residuals))
  ss_res <- sum(residuals^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2    <- 1 - (ss_res / ss_tot)
  return(c(MSE = mse, RMSE = rmse, MAE = mae, MedAE = medae, R2 = r2))
}

m_null  <- calc_metrics(data_test$y, pred_null)
m_full  <- calc_metrics(data_test$y, pred_full)
m_final <- calc_metrics(data_test$y, pred_final)


## R Code 115------------------------------------------------------------------------------------------

# Performance comparison table
cat("\n--- Performance on TEST set ---\n\n")

perf_table <- data.frame(
  Model = c("Null (baseline)", "Full (no interaction)", "Final (with interaction)"),
  MSE = round(c(m_null["MSE"], m_full["MSE"], m_final["MSE"]), 3),
  RMSE = round(c(m_null["RMSE"], m_full["RMSE"], m_final["RMSE"]), 3),
  MAE = round(c(m_null["MAE"], m_full["MAE"], m_final["MAE"]), 3),
  MedAE = round(c(m_null["MedAE"], m_full["MedAE"], m_final["MedAE"]), 3),
  R2 = round(c(m_null["R2"], m_full["R2"], m_final["R2"]), 4)
)
print(perf_table, row.names = FALSE)


# ------------------------------------------------------------------------------
# 6.C Comparison with baseline
# ------------------------------------------------------------------------------

## R Code 116------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("6.C COMPARISON WITH BASELINE\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 117------------------------------------------------------------------------------------------

improvement_rmse <- (m_null["RMSE"] - m_final["RMSE"]) / m_null["RMSE"] * 100

cat("\n--- Improvement over Null model ---\n")
cat("RMSE Null :", round(m_null["RMSE"], 2), "\n")
cat("RMSE Final:", round(m_final["RMSE"], 2), "\n")
cat("Reduction :", round(improvement_rmse, 1), "%\n")
cat("R² gain   :", round(m_final["R2"] - m_null["R2"], 4), "\n")


# ------------------------------------------------------------------------------
# 6.D Calibration plot
# ------------------------------------------------------------------------------

## R Code 118------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("6.D CALIBRATION PLOT\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 119------------------------------------------------------------------------------------------

plot_data <- data.frame(
  Observed = data_test$y,
  Predicted = pred_final
)

plot_calibration <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 1) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", alpha = 0.2) +
  labs(
    title = "Calibration: Predicted vs Observed",
    subtitle = paste0("Test Set (n=", nrow(data_test), ") | RMSE = ", 
                      round(m_final["RMSE"], 2), " | R² = ", round(m_final["R2"], 3)),
    x = "Observed values (y)",
    y = "Predicted values (ŷ)"
  ) +
  theme_bw(base_size = 12) +
  coord_fixed(ratio = 1)

print(plot_calibration)


# ------------------------------------------------------------------------------
# 6.E Residuals distribution (test set)
# ------------------------------------------------------------------------------

## R Code 120------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("6.E RESIDUALS DISTRIBUTION (TEST SET)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 121------------------------------------------------------------------------------------------

residuals_test <- data_test$y - pred_final

plot_resid <- ggplot(data.frame(residuals = residuals_test), aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, 
                 fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Residuals Distribution (Test Set)",
    subtitle = paste0("Mean = ", round(mean(residuals_test), 3), 
                      " | SD = ", round(sd(residuals_test), 2)),
    x = "Residuals (Observed - Predicted)",
    y = "Density"
  ) +
  theme_bw(base_size = 12)

print(plot_resid)


## R Code 122------------------------------------------------------------------------------------------

# Residuals summary statistics
cat("\n--- Residuals summary (test set) ---\n")
cat("Mean  :", round(mean(residuals_test), 4), "\n")
cat("Median:", round(median(residuals_test), 4), "\n")
cat("SD    :", round(sd(residuals_test), 4), "\n")
cat("Min   :", round(min(residuals_test), 2), "\n")
cat("Max   :", round(max(residuals_test), 2), "\n")


# ------------------------------------------------------------------------------
# 6.F Overfitting check
# ------------------------------------------------------------------------------

## R Code 123------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("6.F OVERFITTING CHECK\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")


## R Code 124------------------------------------------------------------------------------------------

r2_train <- summary(final_model)$adj.r.squared
rmse_train <- summary(final_model)$sigma
r2_test <- m_final["R2"]
rmse_test <- m_final["RMSE"]

overfit_table <- data.frame(
  Metric = c("R²", "RMSE"),
  Train = c(round(r2_train, 4), round(rmse_train, 3)),
  Test = c(round(r2_test, 4), round(rmse_test, 3)),
  Difference = c(round(r2_train - r2_test, 4), round(rmse_test - rmse_train, 3))
)

cat("\n--- Train vs Test comparison ---\n\n")
print(overfit_table, row.names = FALSE)


# ------------------------------------------------------------------------------
# 6.G End of analysis
# ------------------------------------------------------------------------------

## R Code 125------------------------------------------------------------------------------------------

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("END OF ANALYSIS (SECTIONS 5-6)\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")

cat("\n--- Final model summary ---\n")
cat("Formula: y ~ predictors + study_hrs:sleep_qual\n")
cat("R² (test):", round(m_final["R2"], 4), "\n")
cat("RMSE (test):", round(m_final["RMSE"], 2), "\n")
cat("MedAE (test):", round(m_final["MedAE"], 2), "\n")
