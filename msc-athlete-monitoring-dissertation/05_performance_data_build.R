# 05_performance_data_build.R
# Construct the audited player-match dataset used for match-performance modelling.
#
# Inputs:
#   game-performance.csv
#   feature_engineered_monitoring_full.rds
#
# Output:
#   engineered_match_dataset_audited.rds
#   engineered_match_dataset_audited.csv
#
# Each row represents one player in one match. Monitoring information is taken
# from the closest available record 1-7 calendar days before the match.

# Packages 

library(tidyverse)
library(lubridate)

# 2. Load Performance Data 

performance_raw <- readr::read_csv(
  "./game-performance.csv",
  show_col_types = FALSE
)

# 3. Inspect The Timestamp Format

# 4. Parse Match Timestamps

performance <- performance_raw %>%
  
  mutate(
    timestamp =
      as.character(
        timestamp
      ),
    
    parsed_timestamp =
      lubridate::parse_date_time(
        timestamp,
        orders = c(
          "ymd HMS",
          "ymd HM",
          "ymd",
          "dmy HMS",
          "dmy HM",
          "dmy",
          "mdy HMS",
          "mdy HM",
          "mdy"
        ),
        quiet = TRUE
      ),
    
    match_date =
      as.Date(
        parsed_timestamp
      ),
    
    team_performance =
      as.numeric(
        team_performance
      ),
    
    offensive_performance =
      as.numeric(
        offensive_performance
      ),
    
    defensive_performance =
      as.numeric(
        defensive_performance
      ),
    
    performance_rating =
      rowMeans(
        cbind(
          offensive_performance,
          defensive_performance
        ),
        na.rm = FALSE
      )
  ) %>%
  
  select(
    player_name,
    match_date,
    timestamp,
    parsed_timestamp,
    team_performance,
    offensive_performance,
    defensive_performance,
    performance_rating
  ) %>%
  
  arrange(
    player_name,
    match_date
  )


# 5. Check Timestamp Parsing

timestamp_parsing_summary <- performance %>%
  
  summarise(
    Rows =
      n(),
    
    Parsed_Timestamps =
      sum(
        !is.na(parsed_timestamp)
      ),
    
    Missing_Parsed_Timestamps =
      sum(
        is.na(parsed_timestamp)
      ),
    
    First_Match_Date =
      min(
        match_date,
        na.rm = TRUE
      ),
    
    Last_Match_Date =
      max(
        match_date,
        na.rm = TRUE
      )
  )

print(
  timestamp_parsing_summary
)


if (
  any(
    is.na(
      performance$match_date
    )
  )
) {
  
  cat(
    "\nUnparsed timestamp examples:\n"
  )
  
  print(
    performance %>%
      
      filter(
        is.na(match_date)
      ) %>%
      
      distinct(
        timestamp
      ) %>%
      
      head(
        30
      )
  )
  
  stop(
    "Some performance timestamps could not be parsed."
  )
}


# 6. Check Performance Target

performance_target_summary <- performance %>%
  
  summarise(
    Rows =
      n(),
    
    Players =
      n_distinct(
        player_name
      ),
    
    Match_Dates =
      n_distinct(
        match_date
      ),
    
    Mean_Performance =
      mean(
        performance_rating,
        na.rm = TRUE
      ),
    
    SD_Performance =
      sd(
        performance_rating,
        na.rm = TRUE
      ),
    
    Minimum_Performance =
      min(
        performance_rating,
        na.rm = TRUE
      ),
    
    Maximum_Performance =
      max(
        performance_rating,
        na.rm = TRUE
      ),
    
    Missing_Performance =
      sum(
        is.na(
          performance_rating
        )
      )
  )

print(
  performance_target_summary
)


# 7. Remove Invalid Performance Rows 

performance <- performance %>%
  
  filter(
    !is.na(player_name),
    !is.na(match_date),
    !is.na(performance_rating)
  )


# 8. Create Unique Player-Match Identifier

performance <- performance %>%
  
  mutate(
    player_match_id =
      paste(
        player_name,
        match_date,
        sep = "__"
      )
  )


duplicate_player_matches <- performance %>%
  
  count(
    player_match_id
  ) %>%
  
  filter(
    n > 1
  )


####
# 9. HANDLE DUPLICATE PLAYER-MATCH REPORTS
#
# If multiple reports exist for the same player and date,
# average the subjective ratings to retain one player-match.
###

performance_unique <- performance %>%
  
  group_by(
    player_name,
    match_date,
    player_match_id
  ) %>%
  
  summarise(
    team_performance =
      mean(
        team_performance,
        na.rm = TRUE
      ),
    
    offensive_performance =
      mean(
        offensive_performance,
        na.rm = TRUE
      ),
    
    defensive_performance =
      mean(
        defensive_performance,
        na.rm = TRUE
      ),
    
    performance_rating =
      mean(
        performance_rating,
        na.rm = TRUE
      ),
    
    reports_combined =
      n(),
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    player_name,
    match_date
  )


# 10. Load The Full Engineered Monitoring Data

monitoring_file <-
  "feature_engineered_monitoring_full.rds"


if (
  !file.exists(
    monitoring_file
  )
) {
  
  stop(
    paste0(
      "The file '",
      monitoring_file,
      "' was not found. ",
      "Run 02_feature_engineering.R first."
    )
  )
}


engineered_monitoring_full <- readRDS(
  monitoring_file
)


engineered_monitoring_full <- engineered_monitoring_full %>%
  
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
  
  arrange(
    player_name,
    date
  )


# 11. Remove Variables That Would Leak Future Information

future_or_target_columns <- c(
  "readiness_next_day",
  "next_record_date",
  "days_to_next_record"
)


monitoring_predictor_data <- engineered_monitoring_full %>%
  
  select(
    -any_of(
      future_or_target_columns
    )
  ) %>%
  
  rename(
    monitoring_date =
      date
  )


# 12. Check Player Overlap

player_overlap_summary <- tibble(
  Performance_Players =
    n_distinct(
      performance_unique$player_name
    ),
  
  Monitoring_Players =
    n_distinct(
      monitoring_predictor_data$player_name
    ),
  
  Shared_Players =
    length(
      intersect(
        unique(
          performance_unique$player_name
        ),
        unique(
          monitoring_predictor_data$player_name
        )
      )
    )
)


print(
  player_overlap_summary
)


unmatched_performance_players <- setdiff(
  unique(
    performance_unique$player_name
  ),
  unique(
    monitoring_predictor_data$player_name
  )
)


if (
  length(
    unmatched_performance_players
  ) > 0
) {
  
  cat(
    "\nPerformance players absent from monitoring data:\n"
  )
  
  print(
    unmatched_performance_players
  )
}


####
# 13. GENERATE POSSIBLE PRE-MATCH MONITORING MATCHES
#
# We join by player, then retain monitoring records from
# exactly 1–7 calendar days before the match.
#
# We intentionally exclude match-day monitoring because the
# data does not contain reliable collection times. A same-day
# record might have been submitted after the match.
###

candidate_match_monitoring <- performance_unique %>%
  
  inner_join(
    monitoring_predictor_data,
    by = "player_name",
    relationship = "many-to-many"
  ) %>%
  
  mutate(
    days_before_match =
      as.numeric(
        match_date -
          monitoring_date
      )
  ) %>%
  
  filter(
    days_before_match >= 1,
    days_before_match <= 7
  )


###
# 14. SELECT THE LATEST AVAILABLE PRE-MATCH RECORD
#
# Minimum days_before_match means the closest observation
# before the match.
###

engineered_match_dataset <- candidate_match_monitoring %>%
  
  group_by(
    player_match_id
  ) %>%
  
  arrange(
    days_before_match,
    desc(
      monitoring_date
    ),
    .by_group = TRUE
  ) %>%
  
  slice(
    1
  ) %>%
  
  ungroup() %>%
  
  arrange(
    match_date,
    player_name
  )


# 15. Check Match Coverage

match_linkage_summary <- tibble(
  Total_Player_Matches =
    nrow(
      performance_unique
    ),
  
  Linked_Player_Matches =
    nrow(
      engineered_match_dataset
    ),
  
  Unlinked_Player_Matches =
    nrow(
      performance_unique
    ) -
    nrow(
      engineered_match_dataset
    ),
  
  Linkage_Percentage =
    100 *
    nrow(
      engineered_match_dataset
    ) /
    nrow(
      performance_unique
    )
)


print(
  match_linkage_summary
)


days_before_match_summary <- engineered_match_dataset %>%
  
  count(
    days_before_match,
    name =
      "Player_Matches"
  ) %>%
  
  mutate(
    Percentage =
      100 *
      Player_Matches /
      sum(
        Player_Matches
      )
  ) %>%
  
  arrange(
    days_before_match
  )


print(
  days_before_match_summary,
  n = Inf
)


# 16. Identify Unlinked Player-Matches

unlinked_player_matches <- performance_unique %>%
  
  anti_join(
    engineered_match_dataset %>%
      
      select(
        player_match_id
      ),
    
    by =
      "player_match_id"
  )


####
# 17. ADD LEAKAGE-SAFE HISTORICAL PERFORMANCE FEATURES
#
# These are calculated only from each player's previous
# match performances.
####

engineered_match_dataset <- engineered_match_dataset %>%
  
  arrange(
    player_name,
    match_date
  ) %>%
  
  group_by(
    player_name
  ) %>%
  
  mutate(
    previous_performance =
      lag(
        performance_rating,
        1
      ),
    
    previous_3_mean =
      lag(
        slider::slide_dbl(
          performance_rating,
          ~ mean(
            .x,
            na.rm = TRUE
          ),
          .before = 2,
          .complete = TRUE
        ),
        1
      ),
    
    previous_5_mean =
      lag(
        slider::slide_dbl(
          performance_rating,
          ~ mean(
            .x,
            na.rm = TRUE
          ),
          .before = 4,
          .complete = TRUE
        ),
        1
      ),
    
    player_historical_mean =
      lag(
        cummean(
          performance_rating
        ),
        1
      ),
    
    days_since_previous_match =
      as.numeric(
        match_date -
          lag(
            match_date
          )
      )
  ) %>%
  
  ungroup()


# 18. Final Quality Checks

duplicate_final_rows <- engineered_match_dataset %>%
  
  count(
    player_match_id
  ) %>%
  
  filter(
    n > 1
  )


list_columns <- names(
  engineered_match_dataset
)[
  vapply(
    engineered_match_dataset,
    is.list,
    logical(1)
  )
]


final_match_summary <- engineered_match_dataset %>%
  
  summarise(
    Rows =
      n(),
    
    Players =
      n_distinct(
        player_name
      ),
    
    Match_Dates =
      n_distinct(
        match_date
      ),
    
    First_Match =
      min(
        match_date
      ),
    
    Last_Match =
      max(
        match_date
      ),
    
    Performance_Mean =
      mean(
        performance_rating
      ),
    
    Performance_SD =
      sd(
        performance_rating
      ),
    
    Previous_Performance_Available =
      sum(
        !is.na(
          previous_performance
        )
      ),
    
    Previous_3_Available =
      sum(
        !is.na(
          previous_3_mean
        )
      ),
    
    Previous_5_Available =
      sum(
        !is.na(
          previous_5_mean
        )
      )
  )


print(
  final_match_summary
)


cat(
  "\nFinal dataset dimensions:\n"
)

print(
  dim(
    engineered_match_dataset
  )
)


cat(
  "\nDuplicate final player-matches:",
  nrow(
    duplicate_final_rows
  ),
  "\n"
)


cat(
  "List-columns:",
  length(
    list_columns
  ),
  "\n"
)


if (
  nrow(
    duplicate_final_rows
  ) > 0
) {
  
  stop(
    "Duplicate player-match rows remain."
  )
}


if (
  length(
    list_columns
  ) > 0
) {
  
  stop(
    paste(
      "List-columns detected:",
      paste(
        list_columns,
        collapse = ", "
      )
    )
  )
}


# 21. Final Performance-Data Audit 


# A. Identify The Duplicate That Was Combined ----

combined_performance_reports <- performance_unique %>%
  filter(
    reports_combined > 1
  )

print(
  combined_performance_reports,
  n = Inf
)


# B. Create Team And Match-Event Identifiers

engineered_match_dataset <- engineered_match_dataset %>%
  mutate(
    team_name =
      sub(
        "-.*$",
        "",
        player_name
      ),
    
    match_event_id =
      paste(
        team_name,
        match_date,
        sep = "__"
      )
  )


match_event_summary <- engineered_match_dataset %>%
  summarise(
    Player_Matches =
      n(),
    
    Calendar_Dates =
      n_distinct(
        match_date
      ),
    
    Team_Match_Events =
      n_distinct(
        match_event_id
      ),
    
    Players =
      n_distinct(
        player_name
      )
  )

print(
  match_event_summary
)


# C. Check Actual Pre-Match Data Coverage

pre_match_coverage_summary <- engineered_match_dataset %>%
  summarise(
    Rows =
      n(),
    
    Readiness_Available =
      sum(
        !is.na(readiness)
      ),
    
    Fatigue_Available =
      sum(
        !is.na(fatigue)
      ),
    
    Sleep_Duration_Available =
      sum(
        !is.na(sleep_duration)
      ),
    
    Daily_Load_Available =
      sum(
        !is.na(daily_load)
      ),
    
    GPS_Total_Distance_Available =
      sum(
        !is.na(total_distance_m)
      ),
    
    GPS_Moving_Time_Available =
      sum(
        !is.na(moving_time_seconds)
      ),
    
    Any_Wellness_Available =
      sum(
        wellness_values_available > 0,
        na.rm = TRUE
      ),
    
    Any_GPS_Available =
      sum(
        gps_values_available > 0,
        na.rm = TRUE
      )
  ) %>%
  
  mutate(
    Readiness_Percentage =
      100 * Readiness_Available / Rows,
    
    Fatigue_Percentage =
      100 * Fatigue_Available / Rows,
    
    Sleep_Percentage =
      100 * Sleep_Duration_Available / Rows,
    
    Daily_Load_Percentage =
      100 * Daily_Load_Available / Rows,
    
    GPS_Distance_Percentage =
      100 * GPS_Total_Distance_Available / Rows,
    
    GPS_Moving_Time_Percentage =
      100 * GPS_Moving_Time_Available / Rows,
    
    Any_Wellness_Percentage =
      100 * Any_Wellness_Available / Rows,
    
    Any_GPS_Percentage =
      100 * Any_GPS_Available / Rows
  )

print(
  pre_match_coverage_summary
)


# D. Check Whether Monitoring Availability Differs By Season

pre_match_coverage_by_year <- engineered_match_dataset %>%
  mutate(
    match_year =
      lubridate::year(
        match_date
      )
  ) %>%
  
  group_by(
    match_year
  ) %>%
  
  summarise(
    Player_Matches =
      n(),
    
    Readiness_Available =
      mean(
        !is.na(readiness)
      ),
    
    Daily_Load_Available =
      mean(
        !is.na(daily_load)
      ),
    
    GPS_Available =
      mean(
        gps_values_available > 0,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    across(
      c(
        Readiness_Available,
        Daily_Load_Available,
        GPS_Available
      ),
      ~ 100 * .x
    )
  )

print(
  pre_match_coverage_by_year
)


# F. Target And Baseline Availability

performance_baseline_coverage <- engineered_match_dataset %>%
  summarise(
    Observations =
      n(),
    
    Previous_Performance =
      sum(
        !is.na(previous_performance)
      ),
    
    Previous_3_Mean =
      sum(
        !is.na(previous_3_mean)
      ),
    
    Previous_5_Mean =
      sum(
        !is.na(previous_5_mean)
      ),
    
    Historical_Mean =
      sum(
        !is.na(player_historical_mean)
      )
  )

print(
  performance_baseline_coverage
)


# G. Save The Audited Version

saveRDS(
  engineered_match_dataset,
  "engineered_match_dataset_audited.rds"
)

write.csv(
  engineered_match_dataset,
  "engineered_match_dataset_audited.csv",
  row.names = FALSE
)

write.csv(
  pre_match_coverage_summary,
  "performance_pre_match_coverage.csv",
  row.names = FALSE
)

write.csv(
  pre_match_coverage_by_year,
  "performance_pre_match_coverage_by_year.csv",
  row.names = FALSE
)

write.csv(
  match_event_summary,
  "performance_match_event_summary.csv",
  row.names = FALSE
)

write.csv(
  performance_baseline_coverage,
  "performance_historical_feature_coverage.csv",
  row.names = FALSE
)


cat(
  "Performance data preparation complete.\n",
  "Player-match rows:", nrow(engineered_match_dataset), "\n",
  "Players:", n_distinct(engineered_match_dataset$player_name), "\n",
  "Match events:", n_distinct(engineered_match_dataset$match_event_id), "\n"
)
