# Figures

# Load packages

library(tidyverse)
library(here)
library(ggtext)
library(patchwork)
library(scales)
library(broom)

# Load analysis data and results

add_pro_final <- read_rds(
  here("data/add_pro_clustering.rds")
)


# Run the sex-stratified analyses to access the regression models
# used for Supplementary Figure 1.

source(
  here("code/04-sex-stratified-analysis.r")
)

# 1. Figure 1: Spider plots

cluster_colors <- c(
  "Cluster 1" = "#D55E00",
  "Cluster 2" = "#56B4E9",
  "Cluster 3" = "#009E73"
)

spider_vars <- c(
  "log_auc_glucose",
  "log_ISI",
  "log_auc_glp1",
  "log_auc_gip",
  "glucagon_supp_0_120"
)

spider_labels <- c(
  "log_auc_glucose" = "AUC\nGlucose",
  "log_ISI" = "Insulin\nSensitivity",
  "log_auc_glp1" = "AUC\nGLP-1",
  "log_auc_gip" = "AUC\nGIP",
  "glucagon_supp_0_120" = "Glucagon\nSuppression"
)

# Set a common z-score scale for all panels

z_limits <- c(-2, 2)
z_breaks <- c(-2, -1, 0, 1, 2)

n_vars <- length(spider_vars)

# Start at the top and go clockwise

angles <- seq(
  pi / 2,
  pi / 2 - 2 * pi,
  length.out = n_vars + 1
)[1:n_vars]

radial_map <- function(x) {
  scales::rescale(
    x,
    from = z_limits,
    to = c(0, 1),
    oob = scales::squish
  )
}

# Calculate cluster means in z-scores

spider_data <- add_pro_final %>%
  mutate(
    across(
      all_of(spider_vars),
      ~ scale(.x)[, 1]
    )
  ) %>%
  group_by(cluster) %>%
  summarise(
    across(
      all_of(spider_vars),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -cluster,
    names_to = "variable",
    values_to = "z"
  ) %>%
  mutate(
    cluster = factor(
      cluster,
      levels = names(cluster_colors)
    ),
    var_idx = match(variable, spider_vars),
    angle = angles[var_idx],
    radius = radial_map(z),
    x = radius * cos(angle),
    y = radius * sin(angle)
  )

# Close polygons

spider_closed <- spider_data %>%
  group_by(cluster) %>%
  arrange(var_idx, .by_group = TRUE) %>%
  slice(c(seq_len(n()), 1)) %>%
  ungroup()

# Create polygon grid rings

grid_df <- expand.grid(
  z = z_breaks,
  var_idx = seq_len(n_vars)
) %>%
  mutate(
    angle = angles[var_idx],
    radius = radial_map(z),
    x = radius * cos(angle),
    y = radius * sin(angle)
  ) %>%
  group_by(z) %>%
  arrange(var_idx, .by_group = TRUE) %>%
  slice(c(seq_len(n()), 1)) %>%
  ungroup()

# Create axis spokes

axis_df <- tibble(
  angle = angles,
  xend = 1.02 * cos(angles),
  yend = 1.02 * sin(angles)
)

# Create outer labels

label_df <- tibble(
  label = unname(spider_labels[spider_vars]),
  angle = angles,
  x = 1.10 * cos(angles),
  y = 1.10 * sin(angles),
  hjust = case_when(
    cos(angle) > 0.15 ~ 0,
    cos(angle) < -0.15 ~ 1,
    TRUE ~ 0.5
  ),
  vjust = case_when(
    sin(angle) > 0.15 ~ 0,
    sin(angle) < -0.15 ~ 1,
    TRUE ~ 0.5
  )
)

# Create ring labels

ring_label_df <- tibble(
  z = z_breaks,
  x = 0.03,
  y = radial_map(z_breaks),
  label = z_breaks
)

figure_1 <- ggplot() +
  geom_path(
    data = grid_df,
    aes(
      x = x,
      y = y,
      group = z
    ),
    color = "grey82",
    linewidth = 0.4
  ) +
  geom_segment(
    data = axis_df,
    aes(
      x = 0,
      y = 0,
      xend = xend,
      yend = yend
    ),
    color = "grey82",
    linewidth = 0.4
  ) +
  geom_polygon(
    data = spider_closed,
    aes(
      x = x,
      y = y,
      group = cluster,
      fill = cluster
    ),
    alpha = 0.28,
    color = NA
  ) +
  geom_path(
    data = spider_closed,
    aes(
      x = x,
      y = y,
      group = cluster,
      color = cluster
    ),
    linewidth = 1.1
  ) +
  geom_text(
    data = label_df,
    aes(
      x = x,
      y = y,
      label = label,
      hjust = hjust,
      vjust = vjust
    ),
    family = "Arial",
    size = 3.1,
    lineheight = 0.9,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = ring_label_df,
    aes(
      x = x,
      y = y,
      label = label
    ),
    color = "grey45",
    family = "Arial",
    size = 2.6,
    inherit.aes = FALSE
  ) +
  facet_wrap(
    ~ cluster,
    nrow = 1
  ) +
  scale_color_manual(
    values = cluster_colors
  ) +
  scale_fill_manual(
    values = cluster_colors
  ) +
  coord_equal(
    xlim = c(-1.30, 1.30),
    ylim = c(-1.20, 1.20),
    clip = "off"
  ) +
  labs(
    caption = paste0(
      "<b>Figure 1:</b> OGTT-characteristics across clusters. ",
      "Each plot shows the cluster mean standardized levels (z scores) ",
      "of <br>AUC glucose, AUC GLP-1, AUC GIP, Gutt's insulin ",
      "sensitivity index and glucagon suppression. <br>",
      "Higher z scores indicate higher values for the respective measure."
    )
  ) +
  theme_void(
    base_family = "Arial"
  ) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "none",
    strip.text = element_text(
      face = "bold",
      size = 11,
      margin = margin(b = 10)
    ),
    plot.caption = ggtext::element_markdown(
      family = "Arial",
      size = 10,
      hjust = 0,
      lineheight = 1.05,
      margin = margin(t = 10)
    ),
    plot.caption.position = "plot",
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(8, 16, 14, 16)
  )

figure_1

cairo_pdf(
  here("figures/figure_1.pdf"),
  width = 35 / 2.54,
  height = 17 / 2.54
)

print(figure_1)

dev.off()

# 2. Figure 2: Mean response curves

df_long <- add_pro_final %>%
  select(
    cluster,
    starts_with("plasma_glucose_"),
    starts_with("insulin_"),
    starts_with("glp1_"),
    starts_with("gip_"),
    starts_with("glucagon_")
  ) %>%
  pivot_longer(
    cols = -cluster,
    names_to = c("marker", "time"),
    names_pattern = "(.*)_(0|30|120)",
    values_to = "value"
  ) %>%
  mutate(
    time = as.numeric(time)
  )

make_response_plot <- function(
    marker_name,
    y_label,
    title_text
) {
  df_plot <- df_long %>%
    filter(marker == marker_name) %>%
    group_by(cluster, time)

  if (marker_name == "glucagon") {
    df_res <- df_plot %>%
      summarise(
        n = sum(!is.na(value)),
        mean_val = mean(value, na.rm = TRUE),
        se = sd(value, na.rm = TRUE) / sqrt(n),
        lower_ci = mean_val - 1.96 * se,
        upper_ci = mean_val + 1.96 * se,
        .groups = "drop"
      )
  } else {
    df_res <- df_plot %>%
      summarise(
        n = sum(!is.na(value)),
        log_val = mean(
          log(value + 0.1),
          na.rm = TRUE
        ),
        log_se = sd(
          log(value + 0.1),
          na.rm = TRUE
        ) / sqrt(n),
        mean_val = exp(log_val) - 0.1,
        lower_ci = exp(
          log_val - 1.96 * log_se
        ) - 0.1,
        upper_ci = exp(
          log_val + 1.96 * log_se
        ) - 0.1,
        .groups = "drop"
      )
  }

  ggplot(
    df_res,
    aes(
      x = time,
      y = mean_val,
      colour = cluster,
      group = cluster
    )
  ) +
    geom_line(
      linewidth = 1.1,
      linejoin = "round"
    ) +
    geom_point(
      size = 2.2
    ) +
    geom_errorbar(
      aes(
        ymin = lower_ci,
        ymax = upper_ci
      ),
      width = 6,
      linewidth = 0.5,
      alpha = 0.75
    ) +
    scale_x_continuous(
      breaks = c(0, 30, 120)
    ) +
    scale_y_continuous(
      limits = c(
        min(0, min(df_res$lower_ci)),
        NA
      ),
      expand = expansion(
        mult = c(0, 0.15)
      )
    ) +
    scale_color_manual(
      values = cluster_colors
    ) +
    labs(
      title = title_text,
      x = "Time (min)",
      y = y_label
    ) +
    theme_bw(
      base_size = 11
    ) +
    theme(
      plot.title = element_text(
        size = 13,
        hjust = 0.5
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(
        color = "grey90"
      ),
      panel.grid.major.y = element_line(
        color = "grey90"
      ),
      panel.border = element_blank(),
      axis.line = element_line(
        color = "black"
      ),
      legend.position = "bottom"
    )
}

p1 <- make_response_plot(
  "plasma_glucose",
  "mmol/L",
  "Glucose"
)

p2 <- make_response_plot(
  "insulin",
  "pmol/L",
  "Insulin"
)

p3 <- make_response_plot(
  "glp1",
  "pmol/L",
  "GLP-1"
)

p4 <- make_response_plot(
  "gip",
  "pmol/L",
  "GIP"
)

p5 <- make_response_plot(
  "glucagon",
  "pmol/L",
  "Glucagon"
)

figure_2 <- (p1 | p2) / (p3 | p4 | p5) +
  plot_annotation(
    tag_levels = "A",
    caption = paste0(
      "**Figure 2:** Mean response curves of Glucose (A), ",
      "Insulin (B), GLP-1 (C), GIP (D) and Glucagon (E) ",
      "during the OGTT (0-120 minutes) by clusters. ",
      "Data are geometric <br>means for glucose, insulin, ",
      "GLP-1 and GIP and arithmetic means for glucagon. ",
      "95% confidence intervals are shown around the mean ",
      "for each time point."
    )
  ) +
  plot_layout(
    guides = "collect"
  ) &
  theme(
    text = element_text(
      family = "Arial",
      size = 10
    ),
    legend.position = "bottom",
    plot.tag = element_text(
      face = "bold"
    ),
    plot.caption = ggtext::element_markdown(
      family = "Arial",
      size = 10,
      hjust = 0
    )
  )

figure_2

cairo_pdf(
  here("figures/figure_2.pdf"),
  width = 35 / 2.54,
  height = 19 / 2.54
)

print(figure_2)

dev.off()

# 3. Supplementary Figure 1: Sex-stratified associations

# Extract sex-stratified estimates from the regression models

clean_cluster_term <- function(x) {
  x <- stringr::str_remove(
    x,
    "^cluster"
  )

  x <- stringr::str_squish(x)

  case_when(
    stringr::str_detect(x, "^\d+$") ~ paste("Cluster", x),
    stringr::str_detect(x, "^Cluster") ~ x,
    TRUE ~ x
  )
}

extract_sex_std <- function(
    model_m,
    model_f,
    outcome_name,
    model_name
) {
  tidy_m <- broom::tidy(
    model_m,
    conf.int = TRUE
  ) %>%
    filter(
      stringr::str_detect(term, "^cluster")
    ) %>%
    mutate(
      sex = "Men"
    )

  tidy_f <- broom::tidy(
    model_f,
    conf.int = TRUE
  ) %>%
    filter(
      stringr::str_detect(term, "^cluster")
    ) %>%
    mutate(
      sex = "Women"
    )

  bind_rows(
    tidy_m,
    tidy_f
  ) %>%
    mutate(
      term = clean_cluster_term(term),
      outcome = outcome_name,
      model = model_name
    )
}

forest_data <- bind_rows(
  extract_sex_std(
    model_vat_1_m,
    model_vat_1_f,
    "Visceral fat (VAT)",
    "Model 1"
  ),
  extract_sex_std(
    model_vat_2_m,
    model_vat_2_f,
    "Visceral fat (VAT)",
    "Model 2"
  ),
  extract_sex_std(
    model_sat_1_m,
    model_sat_1_f,
    "Subcutaneous fat (SAT)",
    "Model 1"
  ),
  extract_sex_std(
    model_sat_2_m,
    model_sat_2_f,
    "Subcutaneous fat (SAT)",
    "Model 2"
  ),
  extract_sex_std(
    model_vsratio_1_m,
    model_vsratio_1_f,
    "VAT/SAT ratio",
    "Model 1"
  ),
  extract_sex_std(
    model_vsratio_2_m,
    model_vsratio_2_f,
    "VAT/SAT ratio",
    "Model 2"
  ),
  extract_sex_std(
    model_bf_1_m,
    model_bf_1_f,
    "Body fat %",
    "Model 1"
  ),
  extract_sex_std(
    model_bf_2_m,
    model_bf_2_f,
    "Body fat %",
    "Model 2"
  )
) %>%
  mutate(
    model = factor(
      model,
      levels = c("Model 1", "Model 2")
    ),
    sex = factor(
      sex,
      levels = c("Men", "Women")
    ),
    term = factor(
      term,
      levels = c("Cluster 1", "Cluster 3")
    )
  )

# Set common x-axis limits across panels

x_limits <- range(
  c(
    forest_data$conf.low,
    forest_data$conf.high
  ),
  na.rm = TRUE
)

x_pad <- 0.08 * diff(x_limits)

x_limits <- c(
  x_limits[1] - x_pad,
  x_limits[2] + x_pad
)

# Create forest plot

sex_colors <- c(
  "Men" = "#56B4E9",
  "Women" = "#D55E00"
)

plot_forest_std <- function(
    data,
    outcome_name,
    label
) {
  data %>%
    filter(
      outcome == outcome_name
    ) %>%
    ggplot(
      aes(
        x = estimate,
        y = term,
        colour = sex,
        shape = model
      )
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = "grey65"
    ) +
    geom_pointrange(
      aes(
        xmin = conf.low,
        xmax = conf.high
      ),
      position = position_dodge(
        width = 0.5
      ),
      size = 0.65,
      linewidth = 0.7
    ) +
    scale_colour_manual(
      values = sex_colors
    ) +
    scale_shape_manual(
      values = c(
        "Model 1" = 16,
        "Model 2" = 17
      )
    ) +
    coord_cartesian(
      xlim = x_limits,
      clip = "off"
    ) +
    labs(
      title = outcome_name,
      tag = label,
      x = "Difference in standardized outcome (SD units)",
      y = NULL,
      colour = NULL,
      shape = NULL
    ) +
    theme_classic(
      base_family = "Arial"
    ) +
    theme(
      plot.title = element_text(
        size = 13,
        hjust = 0.5
      ),
      plot.tag = element_text(
        face = "bold",
        size = 13
      ),
      axis.text = element_text(
        size = 10
      ),
      panel.grid.major.y = element_line(
        colour = "grey90"
      ),
      legend.position = "bottom"
    )
}

p1 <- plot_forest_std(
  forest_data,
  "Visceral fat (VAT)",
  "A"
)

p2 <- plot_forest_std(
  forest_data,
  "Subcutaneous fat (SAT)",
  "B"
)

p3 <- plot_forest_std(
  forest_data,
  "VAT/SAT ratio",
  "C"
)

p4 <- plot_forest_std(
  forest_data,
  "Body fat %",
  "D"
)

supplementary_figure_1_forest_plot <- (p1 | p2) / (p3 | p4) +
  plot_layout(
    guides = "collect"
  ) +
  plot_annotation(
    caption = paste0(
      "**Supplementary figure 1:** Sex-stratified associations ",
      "between clusters and standardized VAT (A), SAT (B), ",
      "VAT/SAT ratio (C) and body fat percentage (D). ",
      "VAT, SAT and VAT/SAT ratio were log-transformed<br>",
      "and standardized before regression analyses and body fat ",
      "percentage was standardized on the original scale. ",
      "Estimates are shown relative to Cluster 2. Model 1 is ",
      "adjusted for age<br>and Model 2 is further adjusted for ",
      "smoking status and physical activity. Dots and triangles ",
      "represent point estimates and horizontal lines 95% ",
      "confidence intervals.<br>No statistically significant ",
      "(p > 0.05) sex-by-cluster interactions were observed."
    ),
    theme = theme(
      plot.caption = ggtext::element_markdown(
        family = "Arial",
        size = 10,
        hjust = 0,
        lineheight = 1.05
      )
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

supplementary_figure_1_forest_plot

cairo_pdf(
  here("figures/supplementary_figure_1_forest_plot.pdf"),
  width = 35 / 2.54,
  height = 19 / 2.54
)

print(supplementary_figure_1_forest_plot)

dev.off()
