# 00_gnss_processing.R
# Process raw SoccerMon GNSS/IMU Parquet files into session-level summaries.
#
# Expected folder structure:
#   objective_raw/
#     objective-TeamA-2020/2020/...
#     objective-TeamA-2021/2021/...
#     objective-TeamB-2020/2020/...
#     objective-TeamB-2021/2021/...
#
# Outputs:
#   objective_processed/objective-TeamA-2020_summary_v3.csv
#   objective_processed/objective-TeamA-2021_summary_v3.csv
#   objective_processed/objective-TeamB-2020_summary_v3.csv
#   objective_processed/objective-TeamB-2021_summary_v3.csv
#
# Distances are calculated from consecutive latitude-longitude coordinates.
# GNSS displacements of 20 m or more are excluded as implausible segments.

library(arrow)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(geosphere)

processed_dir <- "objective_processed"

if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

datasets <- c(
  "objective-TeamA-2020",
  "objective-TeamA-2021",
  "objective-TeamB-2020",
  "objective-TeamB-2021"
)

summarise_objective_file_v3 <- function(file_path) {
  
  file_name <- basename(file_path)
  
  data <- read_parquet(file_path)
  
  data2 <- data %>%
    mutate(
      time_seconds = period_to_seconds(hms(time)),
      speed = as.numeric(speed),
      inst_acc_impulse = as.numeric(inst_acc_impulse),
      accel_mag = sqrt(accl_x^2 + accl_y^2 + accl_z^2),
      gyro_mag = sqrt(gyro_x^2 + gyro_y^2 + gyro_z^2)
    ) %>%
    arrange(time_seconds)
  
  gps_data <- data2 %>%
    distinct(time_seconds, .keep_all = TRUE) %>%
    arrange(time_seconds)
  
  gps_coords <- gps_data %>%
    select(lon, lat)

  if (nrow(gps_coords) < 2) {
    return(tibble())
  }

  gps_segment_distance <- geosphere::distHaversine(
    gps_coords[-nrow(gps_coords), ],
    gps_coords[-1, ]
  )
  
  valid_segments <- gps_segment_distance < 20
  sample_interval <- mean(diff(data2$time_seconds), na.rm = TRUE)

  if (!is.finite(sample_interval) || sample_interval <= 0) {
    sample_interval <- 0
  }

  moving_rows <- data2$speed > 1
  
  data2 %>%
    summarise(
      date = ymd(str_sub(file_name, 1, 10)),
      player_name = first(player_name),
      n_records = n(),
      duration_minutes = (max(time_seconds, na.rm = TRUE) - min(time_seconds, na.rm = TRUE)) / 60,
      mean_speed = mean(speed, na.rm = TRUE),
      max_speed = max(speed, na.rm = TRUE),
      sd_speed = sd(speed, na.rm = TRUE),
      total_distance_m = sum(gps_segment_distance[valid_segments], na.rm = TRUE),
      high_speed_distance_m = sum(gps_segment_distance[valid_segments & gps_data$speed[-1] >= 5.5], na.rm = TRUE),
      sprint_time_seconds = sum(speed >= 7, na.rm = TRUE) * sample_interval,
      moving_time_seconds = sum(moving_rows, na.rm = TRUE) * sample_interval,
      moving_percentage = {
        session_duration_seconds <- max(time_seconds, na.rm = TRUE) -
          min(time_seconds, na.rm = TRUE)
        if (session_duration_seconds > 0) {
          moving_time_seconds / session_duration_seconds * 100
        } else {
          NA_real_
        }
      },
      mean_moving_speed = mean(speed[moving_rows], na.rm = TRUE),
      mean_accel_mag = mean(accel_mag, na.rm = TRUE),
      max_accel_mag = max(accel_mag, na.rm = TRUE),
      sd_accel_mag = sd(accel_mag, na.rm = TRUE),
      acceleration_cv = sd(accel_mag, na.rm = TRUE) / mean(accel_mag, na.rm = TRUE),
      mean_gyro_mag = mean(gyro_mag, na.rm = TRUE),
      max_gyro_mag = max(gyro_mag, na.rm = TRUE),
      sd_gyro_mag = sd(gyro_mag, na.rm = TRUE),
      mean_acc_impulse = mean(inst_acc_impulse, na.rm = TRUE),
      max_acc_impulse = max(inst_acc_impulse, na.rm = TRUE),
      mean_hdop = mean(hdop, na.rm = TRUE),
      mean_satellites = mean(num_satellites, na.rm = TRUE),
      mean_signal_quality = mean(signal_quality, na.rm = TRUE),
      stationary_pct = mean(speed < 0.5, na.rm = TRUE) * 100,
      walking_pct = mean(speed >= 0.5 & speed < 2, na.rm = TRUE) * 100,
      running_pct = mean(speed >= 2 & speed < 5.5, na.rm = TRUE) * 100,
      sprint_pct = mean(speed >= 5.5, na.rm = TRUE) * 100,
      gps_outlier_segments = sum(gps_segment_distance >= 20, na.rm = TRUE),
      gps_outlier_pct = mean(gps_segment_distance >= 20, na.rm = TRUE) * 100
    )
}

for (dataset_name in datasets) {
  
  year <- sub(".*-(\\d{4})$", "\\1", dataset_name)
  
  raw_dir <- file.path(
    "objective_raw",
    dataset_name,
    year
  )
  
  message("Processing: ", dataset_name)
  
  objective_files_all <- list.files(
    path = raw_dir,
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  message("Files found: ", length(objective_files_all))
  
  objective_summary <- map_dfr(
    objective_files_all,
    summarise_objective_file_v3
  )
  
  output_file <- file.path(
    processed_dir,
    paste0(dataset_name, "_summary_v3.csv")
  )
  
  write.csv(
    objective_summary,
    output_file,
    row.names = FALSE
  )
  

  
  message("Saved: ", output_file)
}

cat(
  "GNSS processing complete.\n",
  "Session-level summaries saved to:", processed_dir, "\n"
)
