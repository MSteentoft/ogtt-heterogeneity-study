# K-means clustering

# Load packages

library(tidyverse)
library(here)
library(cluster)
library(NbClust)

# Load analysis data

add_pro_clustering <- read_rds(
  here("data/add_pro_analysis.rds")
)

# 1. Prepare variables for clustering

# Create log-transformed variables

add_pro_clustering <- add_pro_clustering %>%
  mutate(
    log_auc_glucose = log(auc_glucose),
    log_ISI = log(insulin_sensitivity_index_0_120),
    log_auc_glp1 = log(auc_glp1),
    log_auc_gip = log(auc_gip)
  )

# Select clustering variables and standardize them

add_pro_clusters <- add_pro_clustering %>%
  select(
    log_auc_glucose,
    log_ISI,
    log_auc_glp1,
    log_auc_gip,
    glucagon_supp_0_120
  ) %>%
  scale() %>%
  as.data.frame()

# 2. Pre-analysis: assess the number of clusters

# Assess the optimal number of clusters using complementary methods.
# The elbow method evaluates within-cluster variation, while NbClust
# compares several clustering criteria. Silhouette width assesses
# how well observations fit within their assigned cluster.

# Elbow method

wssplot <- function(data, nc = 10, seed = 123) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))

  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }

  plot(
    1:nc,
    wss,
    type = "b",
    xlab = "Number of clusters",
    ylab = "Within-cluster sum of squares"
  )
}

wssplot(add_pro_clusters)

# NbClust

set.seed(123)

number_of_clusters <- NbClust(
  data = add_pro_clusters,
  diss = NULL,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "kmeans",
  index = "all",
  alphaBeale = 0.1
)

barplot(
  table(number_of_clusters$Best.n[1, ]),
  xlab = "Number of clusters",
  ylab = "Number of criteria",
  main = "Number of clusters chosen by clustering criteria"
)

# Silhouette width

sil_width <- numeric(10)

for (i in 2:10) {
  set.seed(123)

  km <- kmeans(
    add_pro_clusters,
    centers = i,
    nstart = 25
  )

  sil <- silhouette(
    km$cluster,
    dist(add_pro_clusters)
  )

  sil_width[i] <- mean(sil[, 3])
}

plot(
  2:10,
  sil_width[2:10],
  type = "b",
  xlab = "Number of clusters",
  ylab = "Average silhouette width"
)

# 3. Run K-means clustering

set.seed(123)

add_pro_kmeans <- kmeans(
  add_pro_clusters,
  centers = 3,
  nstart = 25
)

# 4. Add cluster membership to the dataset

add_pro_clustering$cluster <- NA

add_pro_clustering$cluster[
  as.integer(rownames(add_pro_clusters))
] <- add_pro_kmeans$cluster

# 5. Describe the clusters

# Number of participants in each cluster

add_pro_kmeans$size

# Cluster centers in the standardized clustering variables

add_pro_kmeans$centers

# Calculate geometric means for the log-transformed variables.
# The geometric mean is obtained by back-transforming the mean of the log values.

aggregate(
  add_pro_clustering[
    ,
    c(
      "log_auc_glucose",
      "log_ISI",
      "log_auc_glp1",
      "log_auc_gip"
    )
  ],
  by = list(cluster = add_pro_clustering$cluster),
  FUN = function(x) exp(mean(x, na.rm = TRUE))
)

# Calculate arithmetic means for glucagon suppression.

aggregate(
  add_pro_clustering[
    ,
    "glucagon_supp_0_120",
    drop = FALSE
  ],
  by = list(cluster = add_pro_clustering$cluster),
  FUN = mean,
  na.rm = TRUE
)

# Convert cluster membership to a factor with readable labels

add_pro_clustering$cluster <- factor(
  add_pro_clustering$cluster,
  labels = c("Cluster 1", "Cluster 2", "Cluster 3")
)

# 6. Save dataset with cluster membership

saveRDS(
  add_pro_clustering,
  here("data/add_pro_clustering.rds")
)
