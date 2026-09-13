# 02_feature_engineering.R
# Create calendar-aware longitudinal features for next-day readiness modelling.
# Input:  monitoring_daily_unique.rds
# Output: feature_engineered_monitoring_full.rds
#         feature_engineered_readiness_v2.rds
#         feature_engineered_readiness_v2.csv
#         feature_engineered_readiness_v2_missingness.csv

library(tidyverse)
library(slider)

# Load and validate daily monitoring data

input_file <- "monitoring_daily_unique.rds"

if (!file.exists(input_file)) {
  stop("Cannot find 'monitoring_daily_unique.rds' in the working directory.")
}

daily_data <- readRDS(input_file) %>%
  mutate(
    player_name = as.character(player_name),
    date = as.Date(date)
  ) %>%
  arrange(player_name, date)

required_columns <- c("player_name", "date")
missing_columns <- setdiff(required_columns, names(daily_data))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

duplicate_days <- daily_data %>%
  count(player_name, date) %>%
  filter(n > 1)

if (nrow(duplicate_days) > 0) {
  stop("Duplicate player-date rows remain in the daily monitoring data.")
}

# Variables used for temporal feature engineering

wellness_variables <- c(
  "readiness", "fatigue", "mood", "sleep_duration",
  "sleep_quality", "soreness", "stress"
)

training_load_variables <- c(
  "acwr", "atl", "ctl28", "ctl42",
  "daily_load", "weekly_load", "monotony", "strain"
)

gps_variables <- c(
  "total_distance_m", "high_speed_distance_m", "sprint_time_seconds",
  "moving_time_seconds", "duration_minutes", "mean_speed", "max_speed",
  "moving_percentage", "mean_accel_mag", "max_accel_mag",
  "acceleration_cv", "mean_acc_impulse", "max_acc_impulse",
  "running_pct", "sprint_pct", "gps_outlier_pct"
)

wellness_variables <- intersect(wellness_variables, names(daily_data))
training_load_variables <- intersect(training_load_variables, names(daily_data))
gps_variables <- intersect(gps_variables, names(daily_data))

engineered_variables <- unique(
  c(wellness_variables, training_load_variables, gps_variables)
)

# Helper functions

safe_mean_min <- function(x, minimum_observations = 1) {
  x <- x[!is.na(x)]
  if (length(x) < minimum_observations) return(NA_real_)
  mean(x)
}

safe_sd_min <- function(x, minimum_observations = 2) {
  x <- x[!is.na(x)]
  if (length(x) < minimum_observations) return(NA_real_)
  sd(x)
}

safe_minimum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_maximum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_ratio <- function(numerator, denominator) {
  ifelse(
    is.na(numerator) | is.na(denominator) | denominator == 0,
    NA_real_,
    numerator / denominator
  )
}

# Exact calendar-day lags
# Date-shifted joins ensure that lag1 is exactly one calendar day earlier.

lag_days <- c(1, 2, 3, 5, 7)
feature_data <- daily_data

for (current_lag in lag_days) {
  lag_lookup <- daily_data %>%
    select(player_name, date, all_of(engineered_variables)) %>%
    mutate(date = date + current_lag) %>%
    rename_with(
      ~ paste0(.x, "_lag", current_lag),
      all_of(engineered_variables)
    )

  feature_data <- feature_data %>%
    left_join(
      lag_lookup,
      by = c("player_name", "date"),
      relationship = "one-to-one"
    )
}

# Calendar-aware rolling summaries

rolling_windows <- c(3, 5, 7)

feature_data <- feature_data %>%
  group_by(player_name) %>%
  arrange(date, .by_group = TRUE)

for (variable_name in engineered_variables) {
  for (window_size in rolling_windows) {
    minimum_observations <- case_when(
      window_size == 3 ~ 2,
      window_size == 5 ~ 3,
      TRUE ~ 4
    )

    feature_data <- feature_data %>%
      mutate(
        !!paste0(variable_name, "_mean", window_size) :=
          slider::slide_index_dbl(
            .x = .data[[variable_name]],
            .i = date,
            .f = ~ safe_mean_min(
              .x,
              minimum_observations = minimum_observations
            ),
            .before = window_size - 1,
            .complete = FALSE
          ),

        !!paste0(variable_name, "_sd", window_size) :=
          slider::slide_index_dbl(
            .x = .data[[variable_name]],
            .i = date,
            .f = ~ safe_sd_min(
              .x,
              minimum_observations = minimum_observations
            ),
            .before = window_size - 1,
            .complete = FALSE
          )
      )
  }
}

# Seven-day range, daily changes and recent deviations

for (variable_name in engineered_variables) {
  feature_data <- feature_data %>%
    mutate(
      !!paste0(variable_name, "_min7") :=
        slider::slide_index_dbl(
          .x = .data[[variable_name]],
          .i = date,
          .f = safe_minimum,
          .before = 6,
          .complete = FALSE
        ),

      !!paste0(variable_name, "_max7") :=
        slider::slide_index_dbl(
          .x = .data[[variable_name]],
          .i = date,
          .f = safe_maximum,
          .before = 6,
          .complete = FALSE
        )
    )

  lag1_name <- paste0(variable_name, "_lag1")
  lag3_name <- paste0(variable_name, "_lag3")
  lag7_name <- paste0(variable_name, "_lag7")
  mean3_name <- paste0(variable_name, "_mean3")
  mean7_name <- paste0(variable_name, "_mean7")

  if (lag1_name %in% names(feature_data)) {
    feature_data <- feature_data %>%
      mutate(
        !!paste0(variable_name, "_delta1") :=
          .data[[variable_name]] - .data[[lag1_name]]
      )
  }

  if (mean3_name %in% names(feature_data)) {
    feature_data <- feature_data %>%
      mutate(
        !!paste0(variable_name, "_vs_mean3") :=
          .data[[variable_name]] - .data[[mean3_name]]
      )
  }

  if (mean7_name %in% names(feature_data)) {
    feature_data <- feature_data %>%
      mutate(
        !!paste0(variable_name, "_vs_mean7") :=
          .data[[variable_name]] - .data[[mean7_name]]
      )
  }

  if (lag3_name %in% names(feature_data)) {
    feature_data <- feature_data %>%
      mutate(
        !!paste0(variable_name, "_trend3") :=
          .data[[variable_name]] - .data[[lag3_name]]
      )
  }

  if (lag7_name %in% names(feature_data)) {
    feature_data <- feature_data %>%
      mutate(
        !!paste0(variable_name, "_trend7") :=
          .data[[variable_name]] - .data[[lag7_name]]
      )
  }
}

# Derived monitoring features

feature_data <- feature_data %>%
  mutate(
    recovery_balance = readiness - fatigue,
    recovery_balance_lag1 = readiness_lag1 - fatigue_lag1,
    recovery_balance_delta = recovery_balance - recovery_balance_lag1,

    training_stress_balance = ctl42 - atl,
    training_stress_balance_lag1 = ctl42_lag1 - atl_lag1,
    training_stress_balance_delta =
      training_stress_balance - training_stress_balance_lag1,

    acute_load_ratio_3 = safe_ratio(daily_load, daily_load_mean3),
    acute_load_ratio_7 = safe_ratio(daily_load, daily_load_mean7),

    gps_high_speed_ratio =
      safe_ratio(high_speed_distance_m, total_distance_m),
    gps_sprint_time_ratio =
      safe_ratio(sprint_time_seconds, moving_time_seconds),

    sleep_fatigue_balance = sleep_duration - fatigue,
    readiness_soreness_balance = readiness - soreness,

    wellness_pressure =
      rowMeans(cbind(fatigue, soreness, stress), na.rm = TRUE),
    positive_wellness =
      rowMeans(cbind(readiness, mood, sleep_quality), na.rm = TRUE)
  ) %>%
  mutate(
    wellness_pressure =
      if_else(is.nan(wellness_pressure), NA_real_, wellness_pressure),
    positive_wellness =
      if_else(is.nan(positive_wellness), NA_real_, positive_wellness)
  )

# Data-coverage indicators ----

available_wellness_variables <- intersect(
  wellness_variables,
  names(feature_data)
)

available_training_variables <- intersect(
  training_load_variables,
  names(feature_data)
)

available_gps_variables <- intersect(
  gps_variables,
  names(feature_data)
)

feature_data <- feature_data %>%
  mutate(
    wellness_values_available =
      rowSums(
        !is.na(
          as.data.frame(across(all_of(available_wellness_variables)))
        )
      ),

    training_values_available =
      rowSums(
        !is.na(
          as.data.frame(across(all_of(available_training_variables)))
        )
      ),

    gps_values_available =
      rowSums(
        !is.na(
          as.data.frame(across(all_of(available_gps_variables)))
        )
      ),

    wellness_data_available =
      as.integer(wellness_values_available > 0),

    gps_data_available =
      as.integer(gps_values_available > 0)
  )

# Next-day readiness target
# The target is retained only when the next record is exactly one calendar day later.

feature_data <- feature_data %>%
  arrange(player_name, date) %>%
  group_by(player_name) %>%
  mutate(
    next_record_date = lead(date),
    days_to_next_record = as.numeric(next_record_date - date),
    readiness_next_day = if_else(
      days_to_next_record == 1,
      lead(readiness),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    across(
      where(is.numeric),
      ~ if_else(is.infinite(.x), NA_real_, .x)
    )
  )

# Save the full engineered monitoring dataset for match-performance modelling.

saveRDS(
  feature_data,
  "feature_engineered_monitoring_full.rds"
)

# Readiness modelling dataset

feature_engineered_readiness_v2 <- feature_data %>%
  filter(!is.na(readiness_next_day)) %>%
  arrange(player_name, date)

list_columns <- names(feature_engineered_readiness_v2)[
  vapply(feature_engineered_readiness_v2, is.list, logical(1))
]

if (length(list_columns) > 0) {
  stop(
    paste(
      "Unexpected list-columns:",
      paste(list_columns, collapse = ", ")
    )
  )
}

remaining_duplicates <- feature_engineered_readiness_v2 %>%
  count(player_name, date) %>%
  filter(n > 1)

if (nrow(remaining_duplicates) > 0) {
  stop("Duplicate player-date rows exist in the readiness dataset.")
}

feature_missingness <- feature_engineered_readiness_v2 %>%
  summarise(
    across(everything(), ~ sum(is.na(.x)))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing"
  ) %>%
  mutate(
    Missing_Percentage =
      100 * Missing / nrow(feature_engineered_readiness_v2)
  ) %>%
  arrange(desc(Missing_Percentage))

saveRDS(
  feature_engineered_readiness_v2,
  "feature_engineered_readiness_v2.rds"
)

write.csv(
  feature_engineered_readiness_v2,
  "feature_engineered_readiness_v2.csv",
  row.names = FALSE
)

write.csv(
  feature_missingness,
  "feature_engineered_readiness_v2_missingness.csv",
  row.names = FALSE
)

cat(
  "Feature engineering complete.\n",
  "Readiness rows:", nrow(feature_engineered_readiness_v2), "\n",
  "Players:", n_distinct(feature_engineered_readiness_v2$player_name), "\n",
  "Columns:", ncol(feature_engineered_readiness_v2), "\n"
)
