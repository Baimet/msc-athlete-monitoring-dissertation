# 01_data_preparation.R
# Build the daily monitoring dataset used for longitudinal feature engineering.
#
# Inputs:
#   game-performance.csv
#   fatigue.csv
#   mood.csv
#   readiness.csv
#   sleep_duration.csv
#   sleep_quality.csv
#   soreness.csv
#   stress.csv
#   acwr.csv
#   atl.csv
#   ctl28.csv
#   ctl42.csv
#   daily_load.csv
#   monotony.csv
#   strain.csv
#   weekly_load.csv
#   objective_processed/objective-TeamA-2020_summary_v3.csv
#   objective_processed/objective-TeamA-2021_summary_v3.csv
#   objective_processed/objective-TeamB-2020_summary_v3.csv
#   objective_processed/objective-TeamB-2021_summary_v3.csv
#
# Output:
#   monitoring_daily_unique.rds
#   monitoring_daily_unique.csv
#   data_preparation_summary.csv

library(tidyverse)
library(lubridate)

# Helper functions

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_sum <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

parse_date_safely <- function(x) {
  suppressWarnings(
    lubridate::parse_date_time(
      as.character(x),
      orders = c(
        "ymd",
        "dmy",
        "dmY",
        "Ymd",
        "dmy HMS",
        "ymd HMS"
      ),
      quiet = TRUE
    ) %>%
      as_date()
  )
}

standardise_player_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("\\.", "-") %>%
    stringr::str_trim()
}

process_monitoring_file <- function(
    file_name,
    date_column,
    value_name
) {
  if (!file.exists(file_name)) {
    stop("Cannot find input file: ", file_name)
  }

  raw_data <- read.csv(
    file_name,
    check.names = FALSE
  )

  if (!date_column %in% names(raw_data)) {
    stop(
      "Date column '",
      date_column,
      "' was not found in ",
      file_name
    )
  }

  raw_data %>%
    pivot_longer(
      cols = -all_of(date_column),
      names_to = "player_name",
      values_to = value_name
    ) %>%
    transmute(
      date = parse_date_safely(.data[[date_column]]),
      player_name = standardise_player_id(player_name),
      !!value_name := suppressWarnings(
        as.numeric(.data[[value_name]])
      )
    ) %>%
    filter(
      !is.na(date),
      !is.na(player_name),
      player_name != ""
    )
}

# Performance roster
# Match performance is not merged into the daily monitoring table here.
# It is used only to identify the players included in the analytical cohort.

performance_file <- "game-performance.csv"

if (!file.exists(performance_file)) {
  stop("Cannot find 'game-performance.csv'.")
}

performance <- read.csv(
  performance_file,
  check.names = FALSE
) %>%
  mutate(
    player_name = standardise_player_id(player_name)
  ) %>%
  filter(
    !is.na(player_name),
    player_name != ""
  )

performance_players <- unique(
  performance$player_name
)

# Wellness data

fatigue_long <- process_monitoring_file(
  "fatigue.csv",
  "Fatigue.Data",
  "fatigue"
)

mood_long <- process_monitoring_file(
  "mood.csv",
  "Mood.Data",
  "mood"
)

readiness_long <- process_monitoring_file(
  "readiness.csv",
  "Readiness.Data",
  "readiness"
)

sleep_duration_long <- process_monitoring_file(
  "sleep_duration.csv",
  "SleepDurH.Data",
  "sleep_duration"
)

sleep_quality_long <- process_monitoring_file(
  "sleep_quality.csv",
  "SleepQuality.Data",
  "sleep_quality"
)

soreness_long <- process_monitoring_file(
  "soreness.csv",
  "Soreness.Data",
  "soreness"
)

stress_long <- process_monitoring_file(
  "stress.csv",
  "Date",
  "stress"
)

wellness_daily <- list(
  fatigue_long,
  mood_long,
  readiness_long,
  sleep_duration_long,
  sleep_quality_long,
  soreness_long,
  stress_long
) %>%
  reduce(
    full_join,
    by = c(
      "date",
      "player_name"
    )
  ) %>%
  arrange(
    player_name,
    date
  )

# Training-load data

acwr_long <- process_monitoring_file(
  "acwr.csv",
  "Date",
  "acwr"
)

atl_long <- process_monitoring_file(
  "atl.csv",
  "Date",
  "atl"
)

ctl28_long <- process_monitoring_file(
  "ctl28.csv",
  "Date",
  "ctl28"
)

ctl42_long <- process_monitoring_file(
  "ctl42.csv",
  "Date",
  "ctl42"
)

daily_load_long <- process_monitoring_file(
  "daily_load.csv",
  "Date",
  "daily_load"
)

monotony_long <- process_monitoring_file(
  "monotony.csv",
  "Date",
  "monotony"
)

strain_long <- process_monitoring_file(
  "strain.csv",
  "Date",
  "strain"
)

weekly_load_long <- process_monitoring_file(
  "weekly_load.csv",
  "Date",
  "weekly_load"
)

training_load_daily <- list(
  acwr_long,
  atl_long,
  ctl28_long,
  ctl42_long,
  daily_load_long,
  monotony_long,
  strain_long,
  weekly_load_long
) %>%
  reduce(
    full_join,
    by = c(
      "date",
      "player_name"
    )
  ) %>%
  arrange(
    player_name,
    date
  )

# Objective GNSS data

objective_directory <- "objective_processed"

objective_files <- file.path(
  objective_directory,
  c(
    "objective-TeamA-2020_summary_v3.csv",
    "objective-TeamA-2021_summary_v3.csv",
    "objective-TeamB-2020_summary_v3.csv",
    "objective-TeamB-2021_summary_v3.csv"
  )
)

missing_objective_files <- objective_files[
  !file.exists(objective_files)
]

if (length(missing_objective_files) > 0) {
  stop(
    paste(
      "Missing processed GNSS files:",
      paste(
        missing_objective_files,
        collapse = ", "
      ),
      "\nRun 00_gnss_processing.R first."
    )
  )
}

objective_raw <- objective_files %>%
  map_dfr(
    ~ read.csv(
      .x,
      check.names = FALSE
    )
  ) %>%
  mutate(
    date = parse_date_safely(date),
    player_name = standardise_player_id(player_name)
  ) %>%
  filter(
    !is.na(date),
    !is.na(player_name),
    player_name != ""
  )

required_objective_columns <- c(
  "player_name",
  "date",
  "duration_minutes",
  "total_distance_m",
  "high_speed_distance_m",
  "sprint_time_seconds",
  "moving_time_seconds",
  "mean_speed",
  "max_speed",
  "moving_percentage",
  "mean_moving_speed",
  "mean_accel_mag",
  "max_accel_mag",
  "acceleration_cv",
  "mean_acc_impulse",
  "max_acc_impulse",
  "running_pct",
  "sprint_pct",
  "gps_outlier_pct"
)

missing_objective_columns <- setdiff(
  required_objective_columns,
  names(objective_raw)
)

if (length(missing_objective_columns) > 0) {
  stop(
    paste(
      "Missing GNSS summary columns:",
      paste(
        missing_objective_columns,
        collapse = ", "
      )
    )
  )
}

# Several sessions can occur on the same day. Distance/time measures are
# accumulated across sessions, while intensity measures are summarised daily.

objective_daily <- objective_raw %>%
  group_by(
    player_name,
    date
  ) %>%
  summarise(
    gps_session_count = n(),

    total_distance_m =
      safe_sum(total_distance_m),

    high_speed_distance_m =
      safe_sum(high_speed_distance_m),

    sprint_time_seconds =
      safe_sum(sprint_time_seconds),

    moving_time_seconds =
      safe_sum(moving_time_seconds),

    duration_minutes =
      safe_sum(duration_minutes),

    mean_speed =
      safe_mean(mean_speed),

    max_speed =
      safe_max(max_speed),

    moving_percentage =
      safe_mean(moving_percentage),

    mean_moving_speed =
      safe_mean(mean_moving_speed),

    mean_accel_mag =
      safe_mean(mean_accel_mag),

    max_accel_mag =
      safe_max(max_accel_mag),

    acceleration_cv =
      safe_mean(acceleration_cv),

    mean_acc_impulse =
      safe_mean(mean_acc_impulse),

    max_acc_impulse =
      safe_max(max_acc_impulse),

    running_pct =
      safe_mean(running_pct),

    sprint_pct =
      safe_mean(sprint_pct),

    gps_outlier_pct =
      safe_mean(gps_outlier_pct),

    .groups = "drop"
  )

# Integrate the three monitoring domains

monitoring_daily <- wellness_daily %>%
  full_join(
    training_load_daily,
    by = c(
      "player_name",
      "date"
    )
  ) %>%
  full_join(
    objective_daily,
    by = c(
      "player_name",
      "date"
    )
  ) %>%
  filter(
    player_name %in% performance_players
  ) %>%
  arrange(
    player_name,
    date
  )

# Ensure one row per player-day
# This is retained as a final safeguard in case any source contains
# duplicate records for the same calendar date.

identifier_columns <- c(
  "player_name",
  "date"
)

numeric_monitoring_variables <- monitoring_daily %>%
  select(
    -all_of(identifier_columns)
  ) %>%
  select(
    where(is.numeric)
  ) %>%
  names()

monitoring_daily_unique <- monitoring_daily %>%
  group_by(
    player_name,
    date
  ) %>%
  summarise(
    across(
      all_of(numeric_monitoring_variables),
      safe_mean
    ),
    .groups = "drop"
  ) %>%
  arrange(
    player_name,
    date
  )

duplicate_player_days <- monitoring_daily_unique %>%
  count(
    player_name,
    date
  ) %>%
  filter(
    n > 1
  )

if (nrow(duplicate_player_days) > 0) {
  stop(
    "Duplicate player-date observations remain after daily aggregation."
  )
}

# Data-quality summary

data_preparation_summary <- monitoring_daily_unique %>%
  summarise(
    Player_Days = n(),
    Players = n_distinct(player_name),
    First_Date = min(date),
    Last_Date = max(date),

    Wellness_Days = sum(
      !is.na(fatigue) |
        !is.na(readiness) |
        !is.na(soreness)
    ),

    Training_Load_Days = sum(
      !is.na(daily_load)
    ),

    GNSS_Days = sum(
      !is.na(total_distance_m)
    )
  )

missingness_summary <- monitoring_daily_unique %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing"
  ) %>%
  mutate(
    Missing_Percentage =
      100 * Missing /
      nrow(monitoring_daily_unique)
  ) %>%
  arrange(
    desc(Missing_Percentage)
  )

# Save outputs

saveRDS(
  monitoring_daily_unique,
  "monitoring_daily_unique.rds"
)

write.csv(
  monitoring_daily_unique,
  "monitoring_daily_unique.csv",
  row.names = FALSE
)

write.csv(
  data_preparation_summary,
  "data_preparation_summary.csv",
  row.names = FALSE
)

write.csv(
  missingness_summary,
  "data_preparation_missingness.csv",
  row.names = FALSE
)

cat(
  "Daily monitoring data preparation complete.\n",
  "Player-days:", nrow(monitoring_daily_unique), "\n",
  "Players:", n_distinct(monitoring_daily_unique$player_name), "\n"
)
