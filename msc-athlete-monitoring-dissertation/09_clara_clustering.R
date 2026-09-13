# 08_clara_clustering.R
# CLARA clustering of daily athlete-monitoring profiles.
#
# Input:
#   feature_engineered_readiness_v2.rds
#
# Outputs:
#   clara_clustering_outputs/
#
# Clusters are formed from wellness and training-load variables only.
# Next-day readiness is used after clustering to describe the resulting
# monitoring profiles and is not used to form the clusters.

library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)
library(forcats)

set.seed(2026)

# 2. Load Daily Readiness Data

daily_state_data <- readRDS(
  "feature_engineered_readiness_v2.rds"
)

daily_state_data <- daily_state_data %>%
  
  mutate(
    player_name =
      as.character(
        player_name
      ),
    
    date =
      as.Date(
        date
      )
  ) %>%
  
  filter(
    !is.na(
      readiness_next_day
    )
  ) %>%
  
  arrange(
    player_name,
    date
  )


# 3. Define Clustering Variables 

state_variables <- c(
  # Wellness
  "readiness",
  "fatigue",
  "mood",
  "sleep_duration",
  "sleep_quality",
  "soreness",
  "stress",
  
  # Training load
  "daily_load",
  "weekly_load",
  "acwr",
  "atl",
  "ctl28",
  "ctl42",
  "monotony",
  "strain"
)


missing_state_variables <- setdiff(
  state_variables,
  names(
    daily_state_data
  )
)


if (
  length(
    missing_state_variables
  ) > 0
) {
  
  stop(
    paste(
      "Missing clustering variables:",
      paste(
        missing_state_variables,
        collapse = ", "
      )
    )
  )
}


# 4. Optional Gps Variables For Cluster Profiling

gps_profile_variables <- intersect(
  c(
    "total_distance_m",
    "high_speed_distance_m",
    "moving_time_seconds",
    "duration_minutes",
    "mean_speed",
    "max_speed",
    "mean_acc_impulse",
    "sprint_pct",
    "gps_values_available"
  ),
  names(
    daily_state_data
  )
)


# 5. Missingness Audit

state_missingness <- daily_state_data %>%
  
  summarise(
    across(
      all_of(
        state_variables
      ),
      ~ sum(
        is.na(.x)
      )
    )
  ) %>%
  
  pivot_longer(
    cols =
      everything(),
    
    names_to =
      "Variable",
    
    values_to =
      "Missing"
  ) %>%
  
  mutate(
    Missing_Percentage =
      100 *
      Missing /
      nrow(
        daily_state_data
      )
  ) %>%
  
  arrange(
    desc(
      Missing_Percentage
    )
  )


# 6. Create Next-Day Readiness Class

player_readiness_reference <- daily_state_data %>%
  
  group_by(
    player_name
  ) %>%
  
  summarise(
    player_mean_next_day_readiness =
      mean(
        readiness_next_day,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


daily_state_data <- daily_state_data %>%
  
  left_join(
    player_readiness_reference,
    by =
      "player_name",
    relationship =
      "many-to-one"
  ) %>%
  
  mutate(
    readiness_class_next_day =
      if_else(
        readiness_next_day <
          player_mean_next_day_readiness,
        
        "Under_Ready",
        
        "Ready"
      ),
    
    readiness_class_next_day =
      factor(
        readiness_class_next_day,
        levels = c(
          "Ready",
          "Under_Ready"
        )
      )
  )


# 7. Player-Median Imputation 

state_imputed <- daily_state_data %>%
  
  group_by(
    player_name
  )


for (
  variable_name in
  state_variables
) {
  
  state_imputed <- state_imputed %>%
    
    mutate(
      !!variable_name :=
        if_else(
          is.na(
            .data[[variable_name]]
          ),
          
          median(
            .data[[variable_name]],
            na.rm = TRUE
          ),
          
          .data[[variable_name]]
        )
    )
}


state_imputed <- state_imputed %>%
  ungroup()


for (
  variable_name in
  state_variables
) {
  
  overall_median <- median(
    state_imputed[[variable_name]],
    na.rm = TRUE
  )
  
  
  if (
    is.na(
      overall_median
    ) ||
    is.infinite(
      overall_median
    )
  ) {
    
    overall_median <- 0
  }
  
  
  missing_rows <- is.na(
    state_imputed[[variable_name]]
  )
  
  
  state_imputed[[variable_name]][missing_rows] <-
    overall_median
}


stopifnot(
  sum(
    is.na(
      state_imputed[
        state_variables
      ]
    )
  ) ==
    0
)


# 8. Standardise Clustering Variables

state_matrix <- state_imputed %>%
  
  select(
    all_of(
      state_variables
    )
  ) %>%
  
  as.data.frame()


state_scaled <- scale(
  state_matrix
)


state_scaled <- as.matrix(
  state_scaled
)


if (
  any(
    !is.finite(
      state_scaled
    )
  )
) {
  
  stop(
    "Non-finite values were found after scaling."
  )
}


# 9. Select A Representative Sample For Choosing K 

selection_sample_size <- min(
  3000,
  nrow(
    state_scaled
  )
)


set.seed(2026)

selection_rows <- sample(
  seq_len(
    nrow(
      state_scaled
    )
  ),
  
  size =
    selection_sample_size,
  
  replace =
    FALSE
)


selection_scaled <- state_scaled[
  selection_rows,
  ,
  drop = FALSE
]


selection_distance <- dist(
  selection_scaled
)


# 10. Test Candidate Numbers Of Clusters 

candidate_k <- 2:8


cluster_selection_results <- lapply(
  candidate_k,
  function(k_value) {
    
    set.seed(
      2026 +
        k_value
    )
    
    
    current_clara <- cluster::clara(
      x =
        selection_scaled,
      
      k =
        k_value,
      
      metric =
        "euclidean",
      
      samples =
        50,
      
      sampsize =
        min(
          1000,
          nrow(
            selection_scaled
          )
        ),
      
      pamLike =
        TRUE,
      
      rngR =
        TRUE
    )
    
    
    current_silhouette <- cluster::silhouette(
      current_clara$clustering,
      selection_distance
    )
    
    
    tibble(
      Clusters =
        k_value,
      
      Mean_Silhouette =
        mean(
          current_silhouette[
            ,
            "sil_width"
          ]
        ),
      
      Minimum_Silhouette =
        min(
          current_silhouette[
            ,
            "sil_width"
          ]
        ),
      
      Negative_Silhouette_Percentage =
        100 *
        mean(
          current_silhouette[
            ,
            "sil_width"
          ] <
            0
        ),
      
      Objective =
        current_clara$objective
    )
  }
) %>%
  
  bind_rows() %>%
  
  arrange(
    desc(
      Mean_Silhouette
    )
  )


best_k <- cluster_selection_results %>%
  
  slice_max(
    order_by =
      Mean_Silhouette,
    
    n =
      1,
    
    with_ties =
      FALSE
  ) %>%
  
  pull(
    Clusters
  )


# 11. Cluster-Selection Plot

cluster_selection_plot <- cluster_selection_results %>%
  
  arrange(
    Clusters
  ) %>%
  
  ggplot(
    aes(
      x =
        Clusters,
      
      y =
        Mean_Silhouette
    )
  ) +
  
  geom_line() +
  
  geom_point(
    size =
      3
  ) +
  
  geom_vline(
    xintercept =
      best_k,
    
    linetype =
      "dashed"
  ) +
  
  scale_x_continuous(
    breaks =
      candidate_k
  ) +
  
  labs(
    title =
      "Selection of Daily Monitoring-State Clusters",
    
    subtitle =
      paste(
        "CLARA silhouette analysis using",
        selection_sample_size,
        "sampled player-days"
      ),
    
    x =
      "Number of clusters",
    
    y =
      "Mean silhouette width"
  ) +
  
  theme_minimal(
    base_size =
      12
  )


print(
  cluster_selection_plot
)


# 12. Fit Final Clara Model To All Player-Days

set.seed(2026)

final_state_clara <- cluster::clara(
  x =
    state_scaled,
  
  k =
    best_k,
  
  metric =
    "euclidean",
  
  samples =
    100,
  
  sampsize =
    min(
      2000,
      nrow(
        state_scaled
      )
    ),
  
  pamLike =
    TRUE,
  
  rngR =
    TRUE,
  
  keep.data =
    TRUE
)


daily_state_clustered <- state_imputed %>%
  
  mutate(
    State_Cluster =
      factor(
        final_state_clara$clustering
      )
  )

# Calculate Distance To Assigned Medoid

medoid_matrix <- as.matrix(
  final_state_clara$medoids
)

assigned_cluster_numbers <- as.integer(
  daily_state_clustered$State_Cluster
)

distance_to_assigned_medoid <- vapply(
  seq_len(
    nrow(state_scaled)
  ),
  function(row_index) {
    
    assigned_cluster <-
      assigned_cluster_numbers[[row_index]]
    
    sqrt(
      sum(
        (
          state_scaled[row_index, ] -
            medoid_matrix[assigned_cluster, ]
        )^2
      )
    )
  },
  numeric(1)
)

daily_state_clustered$Distance_to_Medoid <-
  distance_to_assigned_medoid

# 13. Raw Cluster Profiles 

cluster_profile_raw <- daily_state_clustered %>%
  
  group_by(
    State_Cluster
  ) %>%
  
  summarise(
    Player_Days =
      n(),
    
    Players =
      n_distinct(
        player_name
      ),
    
    across(
      all_of(
        state_variables
      ),
      ~ mean(
        .x,
        na.rm = TRUE
      )
    ),
    
    .groups =
      "drop"
  )


# 14. Standardised Cluster Profiles 

cluster_profile_scaled <- as.data.frame(
  state_scaled
) %>%
  
  mutate(
    State_Cluster =
      factor(
        final_state_clara$clustering
      )
  ) %>%
  
  group_by(
    State_Cluster
  ) %>%
  
  summarise(
    across(
      all_of(
        state_variables
      ),
      mean
    ),
    
    .groups =
      "drop"
  )


# 15. Long Profile Table For Visualisation 

cluster_profile_long <- cluster_profile_scaled %>%
  
  pivot_longer(
    cols = -State_Cluster,
    names_to = "Variable",
    values_to = "Standardised_Mean"
  )


cluster_heatmap <- ggplot(
  cluster_profile_long,
  
  aes(
    x = Variable,
    y = factor(State_Cluster),
    fill = Standardised_Mean
  )
) +
  
  geom_tile() +
  
  geom_text(
    aes(
      label = sprintf(
        "%.2f",
        Standardised_Mean
      )
    ),
    size = 4
  ) +
  
  scale_fill_gradient2(
    midpoint = 0,
    name = "Standardised\nmean"
  ) +
  
  labs(
    title = "Profiles of Daily Athlete Monitoring States",
    subtitle = "Cluster means expressed as standard deviations from the overall mean",
    x = NULL,
    y = "State cluster"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    axis.title.y = element_text(
      size = 11
    ),
    
    legend.title = element_text(
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    panel.grid = element_blank(),
    
    plot.margin = ggplot2::margin(
      10, 15, 10, 10
    )
  )


print(
  cluster_heatmap
)


# 17. Next-Day Readiness Outcomes By Cluster

cluster_readiness_outcomes <- daily_state_clustered %>%
  
  group_by(
    State_Cluster
  ) %>%
  
  summarise(
    Player_Days =
      n(),
    
    Players =
      n_distinct(
        player_name
      ),
    
    Mean_Next_Day_Readiness =
      mean(
        readiness_next_day
      ),
    
    SD_Next_Day_Readiness =
      sd(
        readiness_next_day
      ),
    
    Under_Ready_Days =
      sum(
        readiness_class_next_day ==
          "Under_Ready"
      ),
    
    Under_Ready_Rate =
      mean(
        readiness_class_next_day ==
          "Under_Ready"
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    Under_Ready_Percentage =
      100 *
      Under_Ready_Rate
  ) %>%
  
  arrange(
    desc(
      Under_Ready_Rate
    )
  )


# 18. Chi-Squared Association Test

cluster_readiness_table <- table(
  daily_state_clustered$State_Cluster,
  daily_state_clustered$readiness_class_next_day
)


cluster_readiness_chisq <- chisq.test(
  cluster_readiness_table
)


# 19. Cluster Readiness-Risk Plot

cluster_readiness_plot <- cluster_readiness_outcomes %>%
  
  mutate(
    State_Cluster =
      fct_reorder(
        State_Cluster,
        Under_Ready_Percentage
      )
  ) %>%
  
  ggplot(
    aes(
      x =
        State_Cluster,
      
      y =
        Under_Ready_Percentage
    )
  ) +
  
  geom_col() +
  
  geom_text(
    aes(
      label =
        paste0(
          round(
            Under_Ready_Percentage,
            1
          ),
          "%"
        )
    ),
    
    vjust =
      -0.4
  ) +
  
  coord_cartesian(
    ylim = c(
      0,
      max(
        cluster_readiness_outcomes$
          Under_Ready_Percentage
      ) *
        1.12
    )
  ) +
  
  labs(
    title =
      "Next-Day Under-Readiness by Monitoring State",
    
    x =
      "Daily monitoring-state cluster",
    
    y =
      "Under-ready player-days (%)"
  ) +
  
  theme_minimal(
    base_size =
      12
  )


print(
  cluster_readiness_plot
)


# 23. Cluster Medoids 

medoid_rows <- final_state_clara$i.med


cluster_medoids <- daily_state_clustered[
  medoid_rows,
  c(
    "player_name",
    "date",
    "State_Cluster",
    state_variables
  )
]

# Cluster-readiness effect summary

cluster_readiness_cramers_v <- sqrt(
  as.numeric(cluster_readiness_chisq$statistic) /
    sum(cluster_readiness_table)
)

cluster_readiness_risk_summary <- cluster_readiness_outcomes %>%
  arrange(State_Cluster) %>%
  summarise(
    Risk_Ratio =
      Under_Ready_Rate[State_Cluster == "2"] /
      Under_Ready_Rate[State_Cluster == "1"],
    Risk_Difference =
      Under_Ready_Rate[State_Cluster == "2"] -
      Under_Ready_Rate[State_Cluster == "1"]
  )

cluster_readiness_effect_summary <- tibble(
  Cramers_V = cluster_readiness_cramers_v,
  Risk_Ratio = cluster_readiness_risk_summary$Risk_Ratio,
  Risk_Difference = cluster_readiness_risk_summary$Risk_Difference,
  Percentage_Point_Difference =
    100 * cluster_readiness_risk_summary$Risk_Difference
)

# Save outputs

output_directory <- "clara_clustering_outputs"

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

saveRDS(
  final_state_clara,
  file.path(output_directory, "final_clara_model.rds")
)

saveRDS(
  daily_state_clustered,
  file.path(output_directory, "daily_monitoring_clusters.rds")
)

write.csv(
  state_missingness,
  file.path(output_directory, "clustering_variable_missingness.csv"),
  row.names = FALSE
)

write.csv(
  cluster_selection_results,
  file.path(output_directory, "cluster_selection_results.csv"),
  row.names = FALSE
)

write.csv(
  cluster_profile_raw,
  file.path(output_directory, "cluster_profile_raw.csv"),
  row.names = FALSE
)

write.csv(
  cluster_profile_scaled,
  file.path(output_directory, "cluster_profile_scaled.csv"),
  row.names = FALSE
)

write.csv(
  cluster_readiness_outcomes,
  file.path(output_directory, "cluster_readiness_outcomes.csv"),
  row.names = FALSE
)

write.csv(
  cluster_readiness_effect_summary,
  file.path(output_directory, "cluster_readiness_effect_summary.csv"),
  row.names = FALSE
)

write.csv(
  cluster_medoids,
  file.path(output_directory, "cluster_medoids.csv"),
  row.names = FALSE
)

capture.output(
  cluster_readiness_chisq,
  file = file.path(output_directory, "cluster_readiness_chisq.txt")
)

ggsave(
  file.path(output_directory, "cluster_selection_plot.png"),
  cluster_selection_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(output_directory, "cluster_profile_heatmap.png"),
  cluster_heatmap,
  width = 13,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(output_directory, "cluster_under_readiness.png"),
  cluster_readiness_plot,
  width = 9,
  height = 6,
  dpi = 300
)

cat(
  "CLARA clustering complete.\n",
  "Selected clusters:", best_k, "\n",
  "Player-days:", nrow(daily_state_clustered), "\n",
  "Outputs saved to:", output_directory, "\n"
)
