# 07_readiness_performance_bridge.R
# Link out-of-fold readiness predictions to subsequent match-performance records.
#
# Inputs:
#   readiness_model_outputs/readiness_predictions.csv
#   engineered_match_dataset_audited.rds
#
# Output:
#   readiness_performance_bridge_outputs/performance_readiness_bridge.rds
#
# The readiness prediction attached to each match comes from the pre-match
# monitoring record already linked to that player-match in 05_performance_data_build.R.

library(tidyverse)

# Load readiness predictions

readiness_predictions_file <- file.path(
  "readiness_model_outputs",
  "readiness_predictions.csv"
)

if (!file.exists(readiness_predictions_file)) {
  stop(
    "Cannot find 'readiness_model_outputs/readiness_predictions.csv'. ",
    "Run 03_readiness_model.R first."
  )
}

readiness_predictions <- read.csv(
  readiness_predictions_file,
  stringsAsFactors = FALSE
) %>%
  mutate(
    date = as.Date(date),
    player_name = as.character(player_name)
  )

required_readiness_columns <- c(
  "outer_fold",
  "Feature_Set",
  "Model",
  "player_name",
  "date",
  "Actual",
  "Predicted",
  "Under_Ready_Probability"
)

missing_readiness_columns <- setdiff(
  required_readiness_columns,
  names(readiness_predictions)
)

if (length(missing_readiness_columns) > 0) {
  stop(
    paste(
      "Missing readiness-prediction columns:",
      paste(missing_readiness_columns, collapse = ", ")
    )
  )
}

# Keep out-of-fold predictions from the final readiness feature set

readiness_bridge_lookup <- readiness_predictions %>%
  filter(
    Feature_Set == "Engineered Core + wider GPS set",
    Model == "Engineered Ranger RF"
  ) %>%
  transmute(
    player_name,
    monitoring_date = date,
    predicted_under_ready_probability = Under_Ready_Probability,
    predicted_readiness_class = Predicted,
    actual_readiness_class = Actual,
    readiness_outer_fold = outer_fold
  ) %>%
  distinct(
    player_name,
    monitoring_date,
    .keep_all = TRUE
  )

if (nrow(readiness_bridge_lookup) == 0) {
  stop(
    "No final-model readiness predictions were found. ",
    "Check the feature-set and model labels in readiness_predictions.csv."
  )
}

# Load match-level data

match_file <- "engineered_match_dataset_audited.rds"

if (!file.exists(match_file)) {
  stop(
    "Cannot find 'engineered_match_dataset_audited.rds'. ",
    "Run 05_performance_data_build.R first."
  )
}

match_data <- readRDS(match_file) %>%
  mutate(
    player_name = as.character(player_name),
    monitoring_date = as.Date(monitoring_date),
    match_date = as.Date(match_date)
  )

required_match_columns <- c(
  "player_name",
  "monitoring_date",
  "match_date",
  "performance_rating"
)

missing_match_columns <- setdiff(
  required_match_columns,
  names(match_data)
)

if (length(missing_match_columns) > 0) {
  stop(
    paste(
      "Missing match-data columns:",
      paste(missing_match_columns, collapse = ", ")
    )
  )
}

# Link readiness risk to the corresponding pre-match monitoring record

performance_readiness_bridge <- match_data %>%
  left_join(
    readiness_bridge_lookup,
    by = c(
      "player_name",
      "monitoring_date"
    ),
    relationship = "many-to-one"
  )

bridge_coverage <- performance_readiness_bridge %>%
  summarise(
    Total_Player_Matches = n(),
    Readiness_Probability_Available =
      sum(!is.na(predicted_under_ready_probability)),
    Missing_Readiness_Probability =
      sum(is.na(predicted_under_ready_probability)),
    Coverage_Percentage =
      100 * Readiness_Probability_Available / Total_Player_Matches
  )

# Dataset used by the mixed-effects analysis

bridge_complete_data <- performance_readiness_bridge %>%
  filter(
    !is.na(predicted_under_ready_probability),
    !is.na(performance_rating)
  )

bridge_summary <- bridge_complete_data %>%
  summarise(
    Observations = n(),
    Players = n_distinct(player_name),
    Matches = n_distinct(match_date),
    Mean_Predicted_Under_Ready_Probability =
      mean(predicted_under_ready_probability),
    SD_Predicted_Under_Ready_Probability =
      sd(predicted_under_ready_probability),
    Mean_Performance =
      mean(performance_rating),
    SD_Performance =
      sd(performance_rating)
  )

# Descriptive association
# These summaries are retained as supporting diagnostics. The inferential
# player-level analysis is conducted in the mixed-effects script.

readiness_performance_association <- bridge_complete_data %>%
  summarise(
    Pearson_Correlation = cor(
      predicted_under_ready_probability,
      performance_rating,
      method = "pearson"
    ),
    Spearman_Correlation = cor(
      predicted_under_ready_probability,
      performance_rating,
      method = "spearman"
    )
  )

# Save outputs

output_directory <- "readiness_performance_bridge_outputs"

if (!dir.exists(output_directory)) {
  dir.create(
    output_directory,
    recursive = TRUE
  )
}

saveRDS(
  performance_readiness_bridge,
  file.path(
    output_directory,
    "performance_readiness_bridge.rds"
  )
)

write.csv(
  bridge_coverage,
  file.path(
    output_directory,
    "readiness_bridge_coverage.csv"
  ),
  row.names = FALSE
)

write.csv(
  bridge_summary,
  file.path(
    output_directory,
    "readiness_bridge_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  readiness_performance_association,
  file.path(
    output_directory,
    "readiness_performance_association.csv"
  ),
  row.names = FALSE
)

cat(
  "Readiness-performance bridge complete.\n",
  "Matched player-matches:", nrow(bridge_complete_data), "\n",
  "Players:", n_distinct(bridge_complete_data$player_name), "\n",
  "Outputs saved to:", output_directory, "\n"
)
