# Load required packages

library(tidyverse)
library(haven)
library(lubridate)
library(labelled)
library(gtsummary)
library(here)

# Data management

# Load and merge datasets

add_pro_raw <- read_dta(here("Rawdata/MES_addition_pro.dta")) %>%
  left_join(
    read_dta(here("Rawdata/MES_Additionpro_insulin_adiponektin_crp.dta")),
    by = "new_id"
  ) %>%
  left_join(
    read_dta(here("Rawdata/MES_addition_pro_4apr2018.dta")),
    by = "new_id"
  ) %>%
  left_join(
    read_dta(here("Rawdata/MES_baseline_dm_risk_dataset_pia_dei.dta")),
    by = "new_id"
  ) %>%
  left_join(
    read_dta(here("Rawdata/MES_addition_pro_2009_quest.dta")),
    by = "new_id"
  )


# Select and rename variables

add_pro_vars_selected <- add_pro_raw %>%
  select(
    -sbp_av_r,
    -dbp_av_r,
    -p_gq_hyp_any,
    -base_fam_dm_sc
  ) %>%
  rename(
    sex = p_gv_sex,
    age_at_screening = age_scr,
    age_at_followup = age_fup_r,
    bmi = bmi_r,
    body_fat_percentage = fat_pc,
    waist_circumference = waist_av,
    hip_circumference = hip_av,
    smoking_status = p_gq_smoke,
    glp1_0 = p_lab_glp1_0,
    glp1_30 = p_lab_glp1_30,
    glp1_120 = p_lab_glp1_120,
    glucagon_0 = p_lab_glucagon_0,
    glucagon_30 = p_lab_glucagon_30,
    glucagon_120 = p_lab_glucagon_120,
    gip_0 = p_lab_gip_0,
    gip_30 = p_lab_gip_30,
    gip_120 = p_lab_gip_120,
    hba1c = p_lab_hba1c,
    insulin_0 = p_lab_insu0,
    insulin_30 = p_lab_insu30,
    insulin_120 = p_lab_insu120,
    plasma_glucose_0 = p_lab_pglu0_r,
    plasma_glucose_30 = p_lab_pglu30_r,
    plasma_glucose_120 = p_lab_pglu120_r,
    hdl = p_lab_hdlc_r,
    ldl = p_lab_ldl_r,
    cholesterol = p_lab_chol_r,
    triglycerides = p_lab_trig_r,
    homa_insulin_sensitivity = homa_s_r,
    homa_insulin_resistance = homa_ir_r,
    homa_betacell_function = homa_b_r,
    insulin_sensitivity_index_0_120 = isi_r,
    visceral_adipose_tissue = vat,
    subcutaneous_adipose_tissue = sat,
    physical_activity_energy_expenditure = PAEE_kj_kg_day_r,
    adiponectin_ng_ml = Adiponectinngml,
    crp_mg_l = CRPmgl
  )


# Recode variables and create derived variables

add_pro_vars_selected <- add_pro_vars_selected %>%
  mutate(
    smoking_status = case_when(
      as.numeric(smoking_status) %in% c(1, 2) ~ "Current smoker",
      as.numeric(smoking_status) == 3 ~ "Never smoked"
    ),
    smoking_status = factor(
      smoking_status,
      levels = c("Current smoker", "Never smoked")
    ),
    sex = factor(
      as.numeric(sex),
      levels = c(0, 1),
      labels = c("Women", "Men")
    ),
    incident_diabetes = if_else(
      CRF_DM == "1" |
        diabetes_q23 == "1" |
        p_gq_diabetes_treatment %in% c("1", "2", "3", "4", "5") |
        treadment_q23 %in% c("1", "2", "3", "4", "5"),
      "yes",
      "no"
    ),
    vat_sat_ratio = visceral_adipose_tissue / subcutaneous_adipose_tissue,
    hba1c_mmol = 10.929 * (hba1c - 2.15)
  )


# Calculate Gutt's insulin sensitivity index for participants with
# missing values but complete OGTT measurements and available BMI and height
# Formula based on Færch et al.

add_pro_vars_selected <- add_pro_vars_selected %>%
  mutate(
    weight = bmi * (height / 100)^2,
    insulin_sensitivity_index_0_120 = ifelse(
      is.na(insulin_sensitivity_index_0_120),
      (75000 + 180 * (plasma_glucose_0 - plasma_glucose_120) * 0.19 * weight) /
        (120 * ((plasma_glucose_0 + plasma_glucose_120) / 2) *
           log(((insulin_0 + insulin_120) / 6.945) / 2)),
      insulin_sensitivity_index_0_120
    )
  )


# Define variables used for clustering

clustering_variables <- c(
  "plasma_glucose_0",
  "plasma_glucose_30",
  "plasma_glucose_120",
  "insulin_0",
  "insulin_30",
  "insulin_120",
  "glp1_0",
  "glp1_30",
  "glp1_120",
  "gip_0",
  "gip_30",
  "gip_120",
  "glucagon_0",
  "glucagon_30",
  "glucagon_120"
)

# Exclude participants receiving medication for type 2 diabetes

n_before <- nrow(add_pro_vars_selected)

add_pro_filtered <- add_pro_vars_selected %>%
  filter(
    !(
      p_gq_diabetes_treatment %in% c("2", "3", "4") |
        treadment_q23 %in% c("2", "3", "4")
    )
  )

n_excluded_diabetes_medication <- n_before - nrow(add_pro_filtered)

# Exclude participants fasting for less than 8 hours before the OGTT

n_before <- nrow(add_pro_filtered)

add_pro_filtered <- add_pro_filtered %>%
  filter(hour(hours_fasting) >= 8)

n_excluded_short_fasting <- n_before - nrow(add_pro_filtered)

# Exclude participants with missing values on any clustering variable

n_before <- nrow(add_pro_filtered)

add_pro_filtered <- add_pro_filtered %>%
  filter(if_all(all_of(clustering_variables), ~ !is.na(.x)))

n_excluded_missing_clustering_variables <- n_before - nrow(add_pro_filtered)

# Exclude participants with an insulin sensitivity index <= 0

n_before <- nrow(add_pro_filtered)

add_pro_filtered <- add_pro_filtered %>%
  filter(insulin_sensitivity_index_0_120 > 0)

n_excluded_invalid_insulin_sensitivity <- n_before - nrow(add_pro_filtered)

# Summarize exclusions

exclusion_summary <- tibble(
  exclusion_criterion = c(
    "Medication for type 2 diabetes",
    "Fasting less than 8 hours",
    "Missing clustering variables",
    "Insulin sensitivity index <= 0"
  ),
  n_excluded = c(
    n_excluded_diabetes_medication,
    n_excluded_short_fasting,
    n_excluded_missing_clustering_variables,
    n_excluded_invalid_insulin_sensitivity
  )
)

# Create derived variables for analysis

add_pro_analysis <- add_pro_filtered %>%
  mutate(
    # AUC glucose
    auc_glucose =
      ((plasma_glucose_0 + plasma_glucose_30) / 2 * 30) +
      ((plasma_glucose_30 + plasma_glucose_120) / 2 * 90),

    # AUC insulin
    auc_insulin =
      ((insulin_0 + insulin_30) / 2 * 30) +
      ((insulin_30 + insulin_120) / 2 * 90),

    # AUC GLP-1
    auc_glp1 =
      ((glp1_0 + glp1_30) / 2 * 30) +
      ((glp1_30 + glp1_120) / 2 * 90),

    # AUC GIP
    auc_gip =
      ((gip_0 + gip_30) / 2 * 30) +
      ((gip_30 + gip_120) / 2 * 90),

    # Glucagon suppression
    glucagon_supp_0_30 =
      (1 - (glucagon_30 / glucagon_0)) * 100,

    glucagon_supp_30_120 =
      (1 - (glucagon_120 / glucagon_30)) * 100,

    glucagon_supp_0_120 =
      (1 - (glucagon_120 / glucagon_0)) * 100,

    # AUC glucagon
    auc_glucagon =
      ((glucagon_0 + glucagon_30) / 2 * 30) +
      ((glucagon_30 + glucagon_120) / 2 * 90)
  )

# Add variable labels

add_pro_analysis <- add_pro_analysis %>%
  set_variable_labels(
    auc_glucose = "AUC Glucose (mmol/l x min)",
    auc_insulin = "AUC Insulin (pmol/l x min)",
    auc_glp1 = "AUC GLP-1 (pmol/L × min)",
    auc_gip = "AUC GIP (pmol/L × min)",
    glucagon_supp_0_30 = "Glucagon suppression 0-30 min (%)",
    glucagon_supp_30_120 = "Glucagon suppression 30-120 min (%)",
    glucagon_supp_0_120 = "Glucagon suppression 0-120 min (%)"
  )

# Check that the calculated variables have no missing values.
# There should be 0 missing values, as participants with missing OGTT measurements
# were excluded in the previous steps.

add_pro_analysis %>%
  select(
    auc_glucose,
    auc_insulin,
    auc_glp1,
    auc_gip,
    glucagon_supp_0_30,
    glucagon_supp_30_120,
    glucagon_supp_0_120
  ) %>%
  tbl_summary() %>%
  modify_footnote(
    everything() ~ "Unknown = Missing values"
  )

# Create glycemic status categories according to the ADDITION-PRO protocol

add_pro_analysis <- add_pro_analysis %>%
  mutate(
    glycemic_status = case_when(
      plasma_glucose_0 >= 7.0 |
        plasma_glucose_120 >= 11.1 ~ "Type 2 diabetes",

      plasma_glucose_0 >= 6.1 &
        plasma_glucose_0 < 7.0 &
        plasma_glucose_120 >= 7.8 &
        plasma_glucose_120 < 11.1 ~ "IFG+IGT",

      plasma_glucose_0 >= 6.1 &
        plasma_glucose_0 < 7.0 &
        plasma_glucose_120 < 7.8 ~ "iIFG",

      plasma_glucose_0 < 6.1 &
        plasma_glucose_120 >= 7.8 &
        plasma_glucose_120 < 11.1 ~ "iIGT",

      plasma_glucose_0 < 6.1 &
        plasma_glucose_120 < 7.8 ~ "NGT"
    ),
    glycemic_status = factor(
      glycemic_status,
      levels = c(
        "NGT",
        "iIFG",
        "iIGT",
        "IFG+IGT",
        "Type 2 diabetes"
      )
    )
  )

# Check distribution of glycemic status

add_pro_analysis %>%
  select(glycemic_status) %>%
  tbl_summary() %>%
  modify_footnote(
    everything() ~ "Unknown = Missing values"
  )

# Save analysis dataset

saveRDS(
  add_pro_analysis,
  here("data/add_pro_analysis.rds")
)
