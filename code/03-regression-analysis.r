# Regression analyses

# Load packages

library(tidyverse)
library(here)

# Load clustering data

add_pro_regression <- read_rds(
  here("data/add_pro_clustering.rds")
)

# 1. Define regression analysis dataset

# Exclude participants with missing data on variables required for the regression analyses:

# visceral adipose tissue, subcutaneous adipose tissue, body fat percentage,

# physical activity energy expenditure, and smoking status.

n_before <- nrow(add_pro_regression)

add_pro_regression <- add_pro_regression %>%
  drop_na(
    visceral_adipose_tissue,
    subcutaneous_adipose_tissue,
    body_fat_percentage,
    physical_activity_energy_expenditure,
    smoking_status
  )

n_excluded <- n_before - nrow(add_pro_regression)
n_analysis <- nrow(add_pro_regression)

# Number of participants excluded from the regression analyses

n_excluded

# Number of participants included in the regression analyses

n_analysis

# 2. Set reference cluster

# Set the reference cluster for the regression models.

# Change this value if a different cluster should be used as reference.

reference_cluster <- "Cluster 2"

add_pro_regression <- add_pro_regression %>%
  mutate(
    cluster = relevel(
      factor(cluster),
      ref = reference_cluster
    )
  )

# 3. Create transformed and standardized outcomes

add_pro_regression <- add_pro_regression %>%
  mutate(
    # Log-transform skewed outcomes
    log_vat = log(visceral_adipose_tissue),
    log_sat = log(subcutaneous_adipose_tissue),
    log_vsratio = log(vat_sat_ratio),


    # Standardize outcomes used in the regression models
    z_log_vat = as.numeric(scale(log_vat)),
    z_log_sat = as.numeric(scale(log_sat)),
    z_log_vsratio = as.numeric(scale(log_vsratio)),
    z_body_fat = as.numeric(scale(body_fat_percentage))

  )

# 4. Main regression models

# VAT

model_vat_1 <- lm(
  z_log_vat ~ cluster + age_at_screening + sex,
  data = add_pro_regression
)

model_vat_2 <- lm(
  z_log_vat ~ cluster + age_at_screening + sex +
    physical_activity_energy_expenditure + smoking_status,
  data = add_pro_regression
)

# SAT

model_sat_1 <- lm(
  z_log_sat ~ cluster + age_at_screening + sex,
  data = add_pro_regression
)

model_sat_2 <- lm(
  z_log_sat ~ cluster + age_at_screening + sex +
    physical_activity_energy_expenditure + smoking_status,
  data = add_pro_regression
)

# VAT/SAT ratio

model_vsratio_1 <- lm(
  z_log_vsratio ~ cluster + age_at_screening + sex,
  data = add_pro_regression
)

model_vsratio_2 <- lm(
  z_log_vsratio ~ cluster + age_at_screening + sex +
    physical_activity_energy_expenditure + smoking_status,
  data = add_pro_regression
)

# Body fat %

model_bf_1 <- lm(
  z_body_fat ~ cluster + age_at_screening + sex,
  data = add_pro_regression
)

model_bf_2 <- lm(
  z_body_fat ~ cluster + age_at_screening + sex +
    physical_activity_energy_expenditure + smoking_status,
  data = add_pro_regression
)

# 5. Sex-stratified regression models

dat_men <- add_pro_regression %>%
  filter(sex == "Men")

dat_women <- add_pro_regression %>%
  filter(sex == "Women")

# VAT: men

model_vat_1_m <- lm(
  z_log_vat ~ cluster + age_at_screening,
  data = dat_men
)

model_vat_2_m <- lm(
  z_log_vat ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_men
)

# VAT: women

model_vat_1_f <- lm(
  z_log_vat ~ cluster + age_at_screening,
  data = dat_women
)

model_vat_2_f <- lm(
  z_log_vat ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_women
)

# SAT: men

model_sat_1_m <- lm(
  z_log_sat ~ cluster + age_at_screening,
  data = dat_men
)

model_sat_2_m <- lm(
  z_log_sat ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_men
)

# SAT: women

model_sat_1_f <- lm(
  z_log_sat ~ cluster + age_at_screening,
  data = dat_women
)

model_sat_2_f <- lm(
  z_log_sat ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_women
)

# VAT/SAT ratio: men

model_vsratio_1_m <- lm(
  z_log_vsratio ~ cluster + age_at_screening,
  data = dat_men
)

model_vsratio_2_m <- lm(
  z_log_vsratio ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_men
)

# VAT/SAT ratio: women

model_vsratio_1_f <- lm(
  z_log_vsratio ~ cluster + age_at_screening,
  data = dat_women
)

model_vsratio_2_f <- lm(
  z_log_vsratio ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_women
)

# Body fat %: men

model_bf_1_m <- lm(
  z_body_fat ~ cluster + age_at_screening,
  data = dat_men
)

model_bf_2_m <- lm(
  z_body_fat ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_men
)

# Body fat %: women

model_bf_1_f <- lm(
  z_body_fat ~ cluster + age_at_screening,
  data = dat_women
)

model_bf_2_f <- lm(
  z_body_fat ~ cluster + age_at_screening +
    physical_activity_energy_expenditure + smoking_status,
  data = dat_women
)
