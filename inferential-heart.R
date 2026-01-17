# --------------------------------------------------------
# Project 1: Independent t-test on Heart Dataset
# Research Question:
# Is there a statistically significant difference in mean cholesterol levels
# between male and female participants?
#
# IV: sex (Male, Female)
# DV: chol (Cholesterol level, mg/dL)
# Test: Welch Two-Sample t-test
# --------------------------------------------------------

# 0) Load required libraries
library(tidyverse)
library(rstatix)
library(ggpubr)

# 1) Read data
data <- read_csv("heart.csv")

# 2) Quick structure check
glimpse(data)

# 3) Convert binary sex to factor
data <- data %>%
  mutate(sex = factor(sex, labels = c("Female", "Male")))

# 4) Verify conversion
summary(data$sex)

# 5) Descriptive statistics by sex
descriptive_by_sex <- data %>%
  group_by(sex) %>%
  summarise(
    n         = n(),
    mean_chol = mean(chol, na.rm = TRUE),
    sd_chol   = sd(chol, na.rm = TRUE),
    .groups   = "drop"
  )

descriptive_by_sex

# 6) QQ plots (normality check)
ggqqplot(data$chol[data$sex == "Female"])
ggqqplot(data$chol[data$sex == "Male"])

# 7) Welch two-sample t-test
t_test_result <- t.test(chol ~ sex, data = data)
t_test_result

# 8) Effect size (Cohen's d)
effect_size <- cohens_d(chol ~ sex, data = data)
effect_size

# 9) Boxplot visualization
p_box <- ggboxplot(
  data,
  x = "sex",
  y = "chol",
  color = "sex",
  palette = "jco",
  add = "jitter"
)

# Save boxplot
ggsave(
  "plots/boxplot_chol_by_sex.png",
  p_box,
  width = 6,
  height = 4,
  dpi = 300
)

# Save QQ-plot for females
p_qq_f <- ggqqplot(data$chol[data$sex == "Female"])
ggsave(
  "plots/qq_female.png",
  p_qq_f,
  width = 5,
  height = 4,
  dpi = 300
)

# Save QQ-plot for males
p_qq_m <- ggqqplot(data$chol[data$sex == "Male"])
ggsave(
  "plots/qq_male.png",
  p_qq_m,
  width = 5,
  height = 4,
  dpi = 300
)
