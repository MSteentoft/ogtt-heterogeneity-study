# Sex-stratified analyses

# Load packages

library(tidyverse)
library(here)

# Load main regression models

source(
  here("code/03-regression.R")
)

# 1. Sex-stratified clustering

# Load analysis data

add_pro_sex_stratified <- read_rds(
  here("data/add_pro_analysis.rds")
)

## Women

add_pro_clustering_w <- add_pro_sex_stratified %>%
  filter(sex == "Women") %>%
  mutate(
    log_auc_glucose = log(auc_glucose),
    log_ISI = log(insulin_sensitivity_index_0_120),
    log_auc_glp1 = log(auc_glp1),
    log_auc_gip = log(auc_gip)
  )

# Select and standardize clustering variables

add_pro_clusters_w <- add_pro_clustering_w %>%
  select(
    log_auc_glucose,
    log_ISI,
    log_auc_glp1,
    log_auc_gip,
    glucagon_supp_0_120
  ) %>%
  scale() %>%
  as.data.frame()

# Run K-means clustering

set.seed(123)

add_pro_kmeans_w <- kmeans(
  add_pro_clusters_w,
  centers = 3,
  nstart = 25
)

# Add cluster membership to the dataset

add_pro_clustering_w$cluster <- NA

add_pro_clustering_w$cluster[
  as.integer(rownames(add_pro_clusters_w))
] <- add_pro_kmeans_w$cluster

# Describe the clusters

add_pro_kmeans_w$size

add_pro_kmeans_w$centers

# Calculate geometric means for log-transformed variables

aggregate(
  add_pro_clustering_w[
    ,
    c(
      "log_auc_glucose",
      "log_ISI",
      "log_auc_glp1",
      "log_auc_gip"
    )
  ],
  by = list(cluster = add_pro_clustering_w$cluster),
  FUN = function(x) exp(mean(x, na.rm = TRUE))
)

# Calculate arithmetic means for glucagon suppression

aggregate(
  add_pro_clustering_w[
    ,
    "glucagon_supp_0_120",
    drop = FALSE
  ],
  by = list(cluster = add_pro_clustering_w$cluster),
  FUN = mean,
  na.rm = TRUE
)

# Convert cluster membership to a factor

add_pro_clustering_w$cluster <- factor(
  add_pro_clustering_w$cluster,
  labels = c("Cluster 1", "Cluster 2", "Cluster 3")
)

## Men

add_pro_clustering_m <- add_pro_sex_stratified %>%
  filter(sex == "Men") %>%
  mutate(
    log_auc_glucose = log(auc_glucose),
    log_ISI = log(insulin_sensitivity_index_0_120),
    log_auc_glp1 = log(auc_glp1),
    log_auc_gip = log(auc_gip)
  )

# Select and standardize clustering variables

add_pro_clusters_m <- add_pro_clustering_m %>%
  select(
    log_auc_glucose,
    log_ISI,
    log_auc_glp1,
    log_auc_gip,
    glucagon_supp_0_120
  ) %>%
  scale() %>%
  as.data.frame()

# Run K-means clustering

set.seed(123)

add_pro_kmeans_m <- kmeans(
  add_pro_clusters_m,
  centers = 3,
  nstart = 25
)

# Add cluster membership to the dataset

add_pro_clustering_m$cluster <- NA

add_pro_clustering_m$cluster[
  as.integer(rownames(add_pro_clusters_m))
] <- add_pro_kmeans_m$cluster

# Describe the clusters

add_pro_kmeans_m$size

add_pro_kmeans_m$centers

# Calculate geometric means for log-transformed variables

aggregate(
  add_pro_clustering_m[
    ,
    c(
      "log_auc_glucose",
      "log_ISI",
      "log_auc_glp1",
      "log_auc_gip"
    )
  ],
  by = list(cluster = add_pro_clustering_m$cluster),
  FUN = function(x) exp(mean(x, na.rm = TRUE))
)

# Calculate arithmetic means for glucagon suppression

aggregate(
  add_pro_clustering_m[
    ,
    "glucagon_supp_0_120",
    drop = FALSE
  ],
  by = list(cluster = add_pro_clustering_m$cluster),
  FUN = mean,
  na.rm = TRUE
)

# Convert cluster membership to a factor

add_pro_clustering_m$cluster <- factor(
  add_pro_clustering_m$cluster,
  labels = c("Cluster 1", "Cluster 2", "Cluster 3")
)

# 2. Interaction analyses

# Fit models including cluster-by-sex interaction terms

model_vat_1_int <- lm(
  z_log_vat ~ cluster * sex + age_at_screening,
  data = add_pro_regression
)

model_vat_2_int <- lm(
  z_log_vat ~ cluster * sex +
    age_at_screening +
    physical_activity_energy_expenditure +
    smoking_status,
  data = add_pro_regression
)

model_sat_1_int <- lm(
  z_log_sat ~ cluster * sex + age_at_screening,
  data = add_pro_regression
)

model_sat_2_int <- lm(
  z_log_sat ~ cluster * sex +
    age_at_screening +
    physical_activity_energy_expenditure +
    smoking_status,
  data = add_pro_regression
)

model_vsratio_1_int <- lm(
  z_log_vsratio ~ cluster * sex + age_at_screening,
  data = add_pro_regression
)

model_vsratio_2_int <- lm(
  z_log_vsratio ~ cluster * sex +
    age_at_screening +
    physical_activity_energy_expenditure +
    smoking_status,
  data = add_pro_regression
)

model_bf_1_int <- lm(
  z_body_fat ~ cluster * sex + age_at_screening,
  data = add_pro_regression
)

model_bf_2_int <- lm(
  z_body_fat ~ cluster * sex +
    age_at_screening +
    physical_activity_energy_expenditure +
    smoking_status,
  data = add_pro_regression
)

# Compare models with and without the cluster-by-sex interaction
# to obtain a global test of the interaction.

get_interaction_p <- function(main_model, interaction_model) {
  anova(main_model, interaction_model)$`Pr(>F)`[2]
}

interaction_p_values <- tibble(
  outcome = c(
    "VAT",
    "SAT",
    "VAT/SAT ratio",
    "Body fat %"
  ),
  model_1 = c(
    get_interaction_p(model_vat_1, model_vat_1_int),
    get_interaction_p(model_sat_1, model_sat_1_int),
    get_interaction_p(model_vsratio_1, model_vsratio_1_int),
    get_interaction_p(model_bf_1, model_bf_1_int)
  ),
  model_2 = c(
    get_interaction_p(model_vat_2, model_vat_2_int),
    get_interaction_p(model_sat_2, model_sat_2_int),
    get_interaction_p(model_vsratio_2, model_vsratio_2_int),
    get_interaction_p(model_bf_2, model_bf_2_int)
  )
)
