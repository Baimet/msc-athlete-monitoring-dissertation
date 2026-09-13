# 03_readiness_model.R
# Random Forest classification of next-day readiness.
# Input:  feature_engineered_readiness_v2.rds
# Output: readiness_model_outputs/
#
# Validation uses four expanding chronological folds.
# Under_Ready is treated as the positive class.

# Packages

library(tidyverse)
library(ranger)

set.seed(2026)


# 3. Load Engineered Data

input_file <- "feature_engineered_readiness_v2.rds"


if (
  !file.exists(input_file)
) {
  
  stop(
    paste0(
      "The file '",
      input_file,
      "' was not found in:\n",
      getwd()
    )
  )
}


readiness_data <- readRDS(
  input_file
)


cat(
  "\nDataset loaded:\n"
)

print(
  dim(readiness_data)
)


# 4. Validate Data

required_columns <- c(
  "player_name",
  "date",
  "readiness_next_day"
)


missing_required_columns <- setdiff(
  required_columns,
  names(readiness_data)
)


if (
  length(missing_required_columns) > 0
) {
  
  stop(
    paste(
      "Required columns missing:",
      paste(
        missing_required_columns,
        collapse = ", "
      )
    )
  )
}


readiness_data <- readiness_data %>%
  
  mutate(
    player_name =
      as.character(player_name),
    
    date =
      as.Date(date)
  ) %>%
  
  filter(
    !is.na(readiness_next_day)
  ) %>%
  
  arrange(
    player_name,
    date
  )


# 5. Identify Candidate Predictors

excluded_columns <- c(
  "player_name",
  "date",
  "next_record_date",
  "days_to_next_record",
  "readiness_next_day"
)


candidate_predictors <- setdiff(
  names(readiness_data),
  excluded_columns
)


candidate_predictors <- candidate_predictors[
  vapply(
    readiness_data[candidate_predictors],
    is.numeric,
    logical(1)
  )
]


cat(
  "\nCandidate numeric predictors:",
  length(candidate_predictors),
  "\n"
)


# 6. Identify Gps Features

gps_keywords <- c(
  "total_distance",
  "high_speed_distance",
  "sprint_time",
  "moving_time",
  "duration_minutes",
  "mean_speed",
  "max_speed",
  "moving_percentage",
  "mean_accel",
  "max_accel",
  "acceleration_cv",
  "mean_acc_impulse",
  "max_acc_impulse",
  "running_pct",
  "sprint_pct",
  "gps_outlier",
  "gps_high_speed_ratio",
  "gps_sprint_time_ratio",
  "gps_values_available",
  "gps_data_available",
  "gps_session_count"
)


gps_pattern <- paste(
  gps_keywords,
  collapse = "|"
)


gps_candidate_predictors <- candidate_predictors[
  grepl(
    gps_pattern,
    candidate_predictors,
    ignore.case = TRUE
  )
]


core_candidate_predictors <- setdiff(
  candidate_predictors,
  gps_candidate_predictors
)


feature_set_definitions <- list(
  
  "Engineered Core" =
    core_candidate_predictors,
  
  "Engineered Core + wider GPS set" =
    candidate_predictors
)


cat(
  "Core candidates:",
  length(core_candidate_predictors),
  "\n"
)

cat(
  "GPS candidates:",
  length(gps_candidate_predictors),
  "\n"
)

cat(
  "Integrated candidates:",
  length(candidate_predictors),
  "\n"
)


# 7. Prepare Chronological Index

readiness_data <- readiness_data %>%
  
  group_by(
    player_name
  ) %>%
  
  arrange(
    date,
    .by_group = TRUE
  ) %>%
  
  mutate(
    player_observation_number =
      row_number(),
    
    player_observation_count =
      n(),
    
    player_time_fraction =
      player_observation_number /
      player_observation_count
  ) %>%
  
  ungroup()


chronological_folds <- tibble(
  
  outer_fold =
    1:4,
  
  training_end =
    c(
      0.60,
      0.70,
      0.80,
      0.90
    ),
  
  testing_end =
    c(
      0.70,
      0.80,
      0.90,
      1.00
    )
)


# 8. Classification Metrics

evaluate_classification <- function(
    actual,
    predicted
) {
  
  actual <- factor(
    actual,
    levels = c(
      "Ready",
      "Under_Ready"
    )
  )
  
  
  predicted <- factor(
    predicted,
    levels = c(
      "Ready",
      "Under_Ready"
    )
  )
  
  
  true_positive <- sum(
    actual == "Under_Ready" &
      predicted == "Under_Ready",
    na.rm = TRUE
  )
  
  
  false_positive <- sum(
    actual == "Ready" &
      predicted == "Under_Ready",
    na.rm = TRUE
  )
  
  
  true_negative <- sum(
    actual == "Ready" &
      predicted == "Ready",
    na.rm = TRUE
  )
  
  
  false_negative <- sum(
    actual == "Under_Ready" &
      predicted == "Ready",
    na.rm = TRUE
  )
  
  
  accuracy <-
    (
      true_positive +
        true_negative
    ) /
    length(actual)
  
  
  sensitivity <- if (
    true_positive +
    false_negative ==
    0
  ) {
    
    NA_real_
    
  } else {
    
    true_positive /
      (
        true_positive +
          false_negative
      )
  }
  
  
  specificity <- if (
    true_negative +
    false_positive ==
    0
  ) {
    
    NA_real_
    
  } else {
    
    true_negative /
      (
        true_negative +
          false_positive
      )
  }
  
  
  precision <- if (
    true_positive +
    false_positive ==
    0
  ) {
    
    NA_real_
    
  } else {
    
    true_positive /
      (
        true_positive +
          false_positive
      )
  }
  
  
  f1 <- if (
    is.na(precision) ||
    is.na(sensitivity) ||
    precision +
    sensitivity ==
    0
  ) {
    
    NA_real_
    
  } else {
    
    2 *
      precision *
      sensitivity /
      (
        precision +
          sensitivity
      )
  }
  
  
  balanced_accuracy <- mean(
    c(
      sensitivity,
      specificity
    ),
    na.rm = TRUE
  )
  
  
  tibble(
    Accuracy =
      accuracy,
    
    Balanced_Accuracy =
      balanced_accuracy,
    
    Sensitivity =
      sensitivity,
    
    Specificity =
      specificity,
    
    Precision =
      precision,
    
    F1 =
      f1,
    
    TP =
      true_positive,
    
    FP =
      false_positive,
    
    TN =
      true_negative,
    
    FN =
      false_negative
  )
}


# 9. Roc Auc ----

calculate_auc <- function(
    actual,
    probability
) {
  
  actual_binary <- ifelse(
    actual == "Under_Ready",
    1,
    0
  )
  
  
  valid_rows <- complete.cases(
    actual_binary,
    probability
  )
  
  
  actual_binary <- actual_binary[
    valid_rows
  ]
  
  
  probability <- probability[
    valid_rows
  ]
  
  
  positive_count <- sum(
    actual_binary == 1
  )
  
  
  negative_count <- sum(
    actual_binary == 0
  )
  
  
  if (
    positive_count == 0 ||
    negative_count == 0
  ) {
    
    return(
      NA_real_
    )
  }
  
  
  probability_ranks <- rank(
    probability,
    ties.method = "average"
  )
  
  
  (
    sum(
      probability_ranks[
        actual_binary == 1
      ]
    ) -
      positive_count *
      (
        positive_count +
          1
      ) /
      2
  ) /
    (
      positive_count *
        negative_count
    )
}


# 10. Fold-Specific Feature Screening

screen_training_features <- function(
    training_data,
    predictor_names,
    maximum_missing_percentage = 70
) {
  
  predictor_names <- predictor_names[
    predictor_names %in%
      names(training_data)
  ]
  
  
  screening_table <- tibble(
    
    Variable =
      predictor_names,
    
    Missing_Percentage =
      vapply(
        training_data[predictor_names],
        function(x) {
          100 * mean(is.na(x))
        },
        numeric(1)
      ),
    
    Unique_Observed_Values =
      vapply(
        training_data[predictor_names],
        function(x) {
          length(
            unique(
              x[
                !is.na(x)
              ]
            )
          )
        },
        numeric(1)
      )
  ) %>%
    
    mutate(
      Retain =
        Missing_Percentage <=
        maximum_missing_percentage &
        Unique_Observed_Values > 1
    )
  
  
  retained_predictors <- screening_table %>%
    
    filter(
      Retain
    ) %>%
    
    pull(
      Variable
    )
  
  
  list(
    screening_table =
      screening_table,
    
    retained_predictors =
      retained_predictors
  )
}


# 11. Training-Only Median Imputation

fit_median_imputation <- function(
    training_x
) {
  
  medians <- vapply(
    training_x,
    function(x) {
      
      median_value <- median(
        x,
        na.rm = TRUE
      )
      
      
      if (
        is.na(median_value) ||
        is.infinite(median_value)
      ) {
        
        median_value <- 0
      }
      
      
      median_value
    },
    numeric(1)
  )
  
  
  list(
    medians =
      medians
  )
}


apply_median_imputation <- function(
    data_x,
    imputation_object
) {
  
  processed_x <- data_x
  
  
  for (
    variable_name in
    names(processed_x)
  ) {
    
    missing_rows <- is.na(
      processed_x[[variable_name]]
    )
    
    
    processed_x[[variable_name]][missing_rows] <-
      imputation_object$medians[[variable_name]]
  }
  
  
  processed_x
}


# 12. Threshold Evaluation

evaluate_probability_threshold <- function(
    actual,
    probability,
    threshold
) {
  
  predicted <- ifelse(
    probability >= threshold,
    "Under_Ready",
    "Ready"
  )
  
  
  evaluate_classification(
    actual =
      actual,
    
    predicted =
      predicted
  ) %>%
    
    mutate(
      Threshold =
        threshold,
      .before =
        1
    )
}


select_probability_threshold <- function(
    actual,
    probability
) {
  
  threshold_grid <- seq(
    0.30,
    0.70,
    by = 0.01
  )
  
  
  threshold_results <- map_dfr(
    threshold_grid,
    function(current_threshold) {
      
      evaluate_probability_threshold(
        actual =
          actual,
        
        probability =
          probability,
        
        threshold =
          current_threshold
      )
    }
  ) %>%
    
    mutate(
      Distance_From_050 =
        abs(
          Threshold -
            0.50
        )
    ) %>%
    
    arrange(
      desc(
        Balanced_Accuracy
      ),
      desc(
        F1
      ),
      Distance_From_050
    )
  
  
  threshold_results %>%
    slice(1)
}


# 13. Rank Features Using Training Data Only

rank_training_features <- function(
    training_x,
    training_y,
    seed
) {
  
  ranking_frame <- bind_cols(
    readiness_class =
      training_y,
    
    training_x
  )
  
  
  set.seed(seed)
  
  
  ranking_model <- ranger::ranger(
    
    formula =
      readiness_class ~ .,
    
    data =
      ranking_frame,
    
    probability =
      TRUE,
    
    num.trees =
      500,
    
    mtry =
      max(
        1,
        floor(
          sqrt(
            ncol(training_x)
          )
        )
      ),
    
    min.node.size =
      10,
    
    importance =
      "permutation",
    
    seed =
      seed,
    
    num.threads =
      max(
        1,
        parallel::detectCores() -
          1
      )
  )
  
  
  importance_table <- tibble(
    
    Variable =
      names(
        ranking_model$variable.importance
      ),
    
    Importance =
      as.numeric(
        ranking_model$variable.importance
      )
  ) %>%
    
    arrange(
      desc(
        Importance
      )
    )
  
  
  importance_table
}


# 14. Fit One Tuning Configuration

fit_oob_configuration <- function(
    training_x,
    training_y,
    mtry_value,
    minimum_node_size,
    seed
) {
  
  modelling_frame <- bind_cols(
    readiness_class =
      training_y,
    
    training_x
  )
  
  
  set.seed(seed)
  
  
  model <- ranger::ranger(
    
    formula =
      readiness_class ~ .,
    
    data =
      modelling_frame,
    
    probability =
      TRUE,
    
    num.trees =
      750,
    
    mtry =
      mtry_value,
    
    min.node.size =
      minimum_node_size,
    
    importance =
      "none",
    
    seed =
      seed,
    
    num.threads =
      max(
        1,
        parallel::detectCores() -
          1
      )
  )
  
  
  oob_probability <-
    model$predictions[
      ,
      "Under_Ready"
    ]
  
  
  best_threshold <- select_probability_threshold(
    
    actual =
      training_y,
    
    probability =
      oob_probability
  )
  
  
  tibble(
    mtry =
      mtry_value,
    
    min_node_size =
      minimum_node_size,
    
    OOB_AUC =
      calculate_auc(
        actual =
          training_y,
        
        probability =
          oob_probability
      ),
    
    OOB_Balanced_Accuracy =
      best_threshold$Balanced_Accuracy,
    
    OOB_Accuracy =
      best_threshold$Accuracy,
    
    OOB_Sensitivity =
      best_threshold$Sensitivity,
    
    OOB_Specificity =
      best_threshold$Specificity,
    
    OOB_F1 =
      best_threshold$F1,
    
    Threshold =
      best_threshold$Threshold
  )
}


# 15. Storage

outer_predictions <- list()

outer_tuning_results <- list()

outer_selected_features <- list()

outer_screening_results <- list()

outer_fold_information <- list()


prediction_result_number <- 1

tuning_result_number <- 1

feature_result_number <- 1

screening_result_number <- 1


# 16. Outer Chronological Validation

candidate_feature_counts <- c(
  20,
  40,
  60,
  100
)


minimum_node_sizes <- c(
  5,
  10,
  20
)


for (
  fold_row in
  seq_len(
    nrow(chronological_folds)
  )
) {
  
  current_fold <-
    chronological_folds$outer_fold[
      fold_row
    ]
  
  
  training_end <-
    chronological_folds$training_end[
      fold_row
    ]
  
  
  testing_end <-
    chronological_folds$testing_end[
      fold_row
    ]
  
  
  cat(
    "OUTER FOLD:",
    current_fold,
    "\n"
  )
  
  cat(
    "Training through:",
    training_end,
    "| Testing through:",
    testing_end,
    "\n"
  )
  
  cat(
    "====================================================\n"
  )
  
  
  outer_training_data <- readiness_data %>%
    
    filter(
      player_time_fraction <=
        training_end
    )
  
  
  outer_testing_data <- readiness_data %>%
    
    filter(
      player_time_fraction >
        training_end,
      
      player_time_fraction <=
        testing_end
    )
  
  
  ###
  # TRAINING-ONLY PLAYER READINESS BASELINES
  ###
  
  player_training_baselines <- outer_training_data %>%
    
    group_by(
      player_name
    ) %>%
    
    summarise(
      player_training_mean_readiness =
        mean(
          readiness_next_day,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    )
  
  
  outer_training_data <- outer_training_data %>%
    
    left_join(
      player_training_baselines,
      by =
        "player_name",
      relationship =
        "many-to-one"
    ) %>%
    
    mutate(
      readiness_class =
        if_else(
          readiness_next_day <
            player_training_mean_readiness,
          
          "Under_Ready",
          
          "Ready"
        )
    )
  
  
  outer_testing_data <- outer_testing_data %>%
    
    left_join(
      player_training_baselines,
      by =
        "player_name",
      relationship =
        "many-to-one"
    ) %>%
    
    filter(
      !is.na(
        player_training_mean_readiness
      )
    ) %>%
    
    mutate(
      readiness_class =
        if_else(
          readiness_next_day <
            player_training_mean_readiness,
          
          "Under_Ready",
          
          "Ready"
        )
    )
  
  
  outer_training_y <- factor(
    outer_training_data$readiness_class,
    levels = c(
      "Ready",
      "Under_Ready"
    )
  )
  
  
  outer_testing_y <- factor(
    outer_testing_data$readiness_class,
    levels = c(
      "Ready",
      "Under_Ready"
    )
  )
  
  
  ####
  # MAJORITY-CLASS BASELINE
  ####
  
  majority_class <- names(
    which.max(
      table(
        outer_training_y
      )
    )
  )
  
  
  majority_prediction <- rep(
    majority_class,
    nrow(
      outer_testing_data
    )
  )
  
  
  outer_predictions[[prediction_result_number]] <- tibble(
    
    outer_fold =
      current_fold,
    
    Feature_Set =
      "Baseline",
    
    Model =
      "Majority-Class Baseline",
    
    player_name =
      outer_testing_data$player_name,
    
    date =
      outer_testing_data$date,
    
    Actual =
      outer_testing_y,
    
    Predicted =
      majority_prediction,
    
    Under_Ready_Probability =
      ifelse(
        majority_class == "Under_Ready",
        1,
        0
      ),
    
    Selected_Feature_Count =
      0,
    
    Threshold =
      NA_real_
  )
  
  
  prediction_result_number <-
    prediction_result_number +
    1
  
  
  ####
  # ENGINEERED FEATURE SETS
  ####
  
  for (
    feature_set_name in
    names(feature_set_definitions)
  ) {
    
    cat(
      "\nFeature set:",
      feature_set_name,
      "\n"
    )
    
    
    current_candidates <-
      feature_set_definitions[[feature_set_name]]
    
    
    screening <- screen_training_features(
      
      training_data =
        outer_training_data,
      
      predictor_names =
        current_candidates,
      
      maximum_missing_percentage =
        70
    )
    
    
    retained_predictors <-
      screening$retained_predictors
    
    
    cat(
      "Predictors retained after fold screening:",
      length(retained_predictors),
      "\n"
    )
    
    
    outer_screening_results[[screening_result_number]] <-
      screening$screening_table %>%
      
      mutate(
        outer_fold =
          current_fold,
        
        Feature_Set =
          feature_set_name,
        
        .before =
          1
      )
    
    
    screening_result_number <-
      screening_result_number +
      1
    
    
    training_x_raw <- outer_training_data %>%
      
      select(
        all_of(
          retained_predictors
        )
      )
    
    
    testing_x_raw <- outer_testing_data %>%
      
      select(
        all_of(
          retained_predictors
        )
      )
    
    
    imputation_object <- fit_median_imputation(
      training_x_raw
    )
    
    
    training_x <- apply_median_imputation(
      training_x_raw,
      imputation_object
    )
    
    
    testing_x <- apply_median_imputation(
      testing_x_raw,
      imputation_object
    )
    
    
    feature_ranking <- rank_training_features(
      
      training_x =
        training_x,
      
      training_y =
        outer_training_y,
      
      seed =
        40000 +
        current_fold *
        100 +
        match(
          feature_set_name,
          names(
            feature_set_definitions
          )
        )
    )
    
    
    maximum_available_features <-
      nrow(
        feature_ranking
      )
    
    
    valid_feature_counts <- candidate_feature_counts[
      candidate_feature_counts <=
        maximum_available_features
    ]
    
    
    if (
      maximum_available_features <
      min(candidate_feature_counts)
    ) {
      
      valid_feature_counts <-
        maximum_available_features
    }
    
    
    current_tuning_rows <- list()
    
    current_tuning_number <- 1
    
    
    for (
      selected_feature_count in
      valid_feature_counts
    ) {
      
      selected_features <- feature_ranking %>%
        
        slice_head(
          n =
            selected_feature_count
        ) %>%
        
        pull(
          Variable
        )
      
      
      selected_training_x <- training_x %>%
        
        select(
          all_of(
            selected_features
          )
        )
      
      
      base_mtry <- max(
        1,
        floor(
          sqrt(
            selected_feature_count
          )
        )
      )
      
      
      candidate_mtry_values <- unique(
        pmax(
          1,
          pmin(
            selected_feature_count,
            c(
              floor(
                base_mtry /
                  2
              ),
              base_mtry,
              base_mtry *
                2
            )
          )
        )
      )
      
      
      for (
        current_mtry in
        candidate_mtry_values
      ) {
        
        for (
          current_node_size in
          minimum_node_sizes
        ) {
          
          tuning_metrics <- fit_oob_configuration(
            
            training_x =
              selected_training_x,
            
            training_y =
              outer_training_y,
            
            mtry_value =
              current_mtry,
            
            minimum_node_size =
              current_node_size,
            
            seed =
              50000 +
              current_fold *
              1000 +
              selected_feature_count *
              10 +
              current_mtry +
              current_node_size
          )
          
          
          current_tuning_rows[[current_tuning_number]] <-
            tuning_metrics %>%
            
            mutate(
              outer_fold =
                current_fold,
              
              Feature_Set =
                feature_set_name,
              
              Selected_Feature_Count =
                selected_feature_count,
              
              .before =
                1
            )
          
          
          current_tuning_number <-
            current_tuning_number +
            1
        }
      }
    }
    
    
    feature_set_tuning_results <- bind_rows(
      current_tuning_rows
    ) %>%
      
      arrange(
        desc(
          OOB_Balanced_Accuracy
        ),
        desc(
          OOB_AUC
        ),
        desc(
          OOB_F1
        ),
        Selected_Feature_Count
      )
    
    
    best_configuration <-
      feature_set_tuning_results %>%
      
      slice(1)
    
    
    outer_tuning_results[[tuning_result_number]] <-
      feature_set_tuning_results
    
    
    tuning_result_number <-
      tuning_result_number +
      1
    
    
    best_feature_count <-
      best_configuration$Selected_Feature_Count
    
    
    best_features <- feature_ranking %>%
      
      slice_head(
        n =
          best_feature_count
      ) %>%
      
      pull(
        Variable
      )
    
    
    outer_selected_features[[feature_result_number]] <-
      feature_ranking %>%
      
      mutate(
        outer_fold =
          current_fold,
        
        Feature_Set =
          feature_set_name,
        
        Rank =
          row_number(),
        
        Selected =
          Variable %in%
          best_features,
        
        .before =
          1
      )
    
    
    feature_result_number <-
      feature_result_number +
      1
    
    
    final_training_x <- training_x %>%
      
      select(
        all_of(
          best_features
        )
      )
    
    
    final_testing_x <- testing_x %>%
      
      select(
        all_of(
          best_features
        )
      )
    
    
    final_training_frame <- bind_cols(
      readiness_class =
        outer_training_y,
      
      final_training_x
    )
    
    
    final_seed <-
      60000 +
      current_fold *
      100 +
      match(
        feature_set_name,
        names(
          feature_set_definitions
        )
      )
    
    
    set.seed(
      final_seed
    )
    
    
    final_model <- ranger::ranger(
      
      formula =
        readiness_class ~ .,
      
      data =
        final_training_frame,
      
      probability =
        TRUE,
      
      num.trees =
        1500,
      
      mtry =
        best_configuration$mtry,
      
      min.node.size =
        best_configuration$min_node_size,
      
      importance =
        "permutation",
      
      seed =
        final_seed,
      
      num.threads =
        max(
          1,
          parallel::detectCores() -
            1
        )
    )
    
    
    testing_probability <- predict(
      final_model,
      data =
        final_testing_x
    )$predictions[
      ,
      "Under_Ready"
    ]
    
    
    selected_threshold <-
      best_configuration$Threshold
    
    
    testing_prediction <- ifelse(
      testing_probability >=
        selected_threshold,
      
      "Under_Ready",
      
      "Ready"
    )
    
    
    outer_predictions[[prediction_result_number]] <-
      tibble(
        
        outer_fold =
          current_fold,
        
        Feature_Set =
          feature_set_name,
        
        Model =
          "Engineered Ranger RF",
        
        player_name =
          outer_testing_data$player_name,
        
        date =
          outer_testing_data$date,
        
        Actual =
          outer_testing_y,
        
        Predicted =
          testing_prediction,
        
        Under_Ready_Probability =
          testing_probability,
        
        Selected_Feature_Count =
          best_feature_count,
        
        Threshold =
          selected_threshold
      )
    
    
    prediction_result_number <-
      prediction_result_number +
      1
    
    
    cat(
      "Selected features:",
      best_feature_count,
      "\n"
    )
    
    cat(
      "Best OOB balanced accuracy:",
      round(
        best_configuration$OOB_Balanced_Accuracy,
        3
      ),
      "\n"
    )
    
    cat(
      "Test threshold:",
      round(
        selected_threshold,
        2
      ),
      "\n"
    )
  }
  
  
  outer_fold_information[[current_fold]] <- tibble(
    
    outer_fold =
      current_fold,
    
    Training_Rows =
      nrow(
        outer_training_data
      ),
    
    Testing_Rows =
      nrow(
        outer_testing_data
      ),
    
    Training_Players =
      n_distinct(
        outer_training_data$player_name
      ),
    
    Testing_Players =
      n_distinct(
        outer_testing_data$player_name
      ),
    
    Training_Under_Ready =
      sum(
        outer_training_y ==
          "Under_Ready"
      ),
    
    Testing_Under_Ready =
      sum(
        outer_testing_y ==
          "Under_Ready"
      )
  )
}


# 17. Combine Results

readiness_predictions <- bind_rows(
  outer_predictions
)


readiness_tuning_results <- bind_rows(
  outer_tuning_results
)


readiness_selected_features <- bind_rows(
  outer_selected_features
)


readiness_screening_results <- bind_rows(
  outer_screening_results
)


readiness_fold_information <- bind_rows(
  outer_fold_information
)


# 18. Overall Results

readiness_model_results <- readiness_predictions %>%
  
  group_by(
    Feature_Set,
    Model
  ) %>%
  
  group_modify(
    ~ {
      
      metrics <- evaluate_classification(
        
        actual =
          .x$Actual,
        
        predicted =
          .x$Predicted
      )
      
      
      metrics %>%
        
        mutate(
          Observations =
            nrow(.x),
          
          ROC_AUC =
            calculate_auc(
              
              actual =
                .x$Actual,
              
              probability =
                .x$Under_Ready_Probability
            ),
          
          Mean_Selected_Features =
            mean(
              .x$Selected_Feature_Count
            ),
          
          Mean_Threshold =
            if (
              all(
                is.na(
                  .x$Threshold
                )
              )
            ) {
              
              NA_real_
              
            } else {
              
              mean(
                .x$Threshold,
                na.rm = TRUE
              )
            },
          
          .before =
            1
        )
    }
  ) %>%
  
  ungroup() %>%
  
  arrange(
    desc(
      Balanced_Accuracy
    )
  )


print(
  readiness_model_results,
  n = Inf
)


# 19. Fold-Level Results

readiness_fold_metrics <- readiness_predictions %>%
  
  group_by(
    outer_fold,
    Feature_Set,
    Model
  ) %>%
  
  group_modify(
    ~ {
      
      metrics <- evaluate_classification(
        
        actual =
          .x$Actual,
        
        predicted =
          .x$Predicted
      )
      
      
      metrics %>%
        
        mutate(
          ROC_AUC =
            calculate_auc(
              
              actual =
                .x$Actual,
              
              probability =
                .x$Under_Ready_Probability
            ),
          
          Selected_Features =
            unique(
              .x$Selected_Feature_Count
            )[1],
          
          Threshold =
            unique(
              .x$Threshold
            )[1]
        )
    }
  ) %>%
  
  ungroup()


readiness_fold_summary <- readiness_fold_metrics %>%
  
  group_by(
    Feature_Set,
    Model
  ) %>%
  
  summarise(
    Mean_Accuracy =
      mean(
        Accuracy,
        na.rm = TRUE
      ),
    
    SD_Accuracy =
      sd(
        Accuracy,
        na.rm = TRUE
      ),
    
    Mean_Balanced_Accuracy =
      mean(
        Balanced_Accuracy,
        na.rm = TRUE
      ),
    
    SD_Balanced_Accuracy =
      sd(
        Balanced_Accuracy,
        na.rm = TRUE
      ),
    
    Mean_Sensitivity =
      mean(
        Sensitivity,
        na.rm = TRUE
      ),
    
    Mean_Specificity =
      mean(
        Specificity,
        na.rm = TRUE
      ),
    
    Mean_F1 =
      mean(
        F1,
        na.rm = TRUE
      ),
    
    Mean_ROC_AUC =
      mean(
        ROC_AUC,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    desc(
      Mean_Balanced_Accuracy
    )
  )


print(
  readiness_fold_summary,
  n = Inf
)


# 20. Feature-Selection Stability

readiness_feature_frequency <- readiness_selected_features %>%
  
  filter(
    Selected
  ) %>%
  
  group_by(
    Feature_Set,
    Variable
  ) %>%
  
  summarise(
    Folds_Selected =
      n_distinct(
        outer_fold
      ),
    
    Mean_Rank =
      mean(
        Rank
      ),
    
    Mean_Importance =
      mean(
        Importance,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    Selection_Percentage =
      100 *
      Folds_Selected /
      nrow(
        chronological_folds
      )
  ) %>%
  
  arrange(
    Feature_Set,
    desc(
      Folds_Selected
    ),
    Mean_Rank
  )


readiness_feature_frequency %>%
  
  group_by(
    Feature_Set
  ) %>%
  
  slice_head(
    n =
      30
  ) %>%
  
  ungroup() %>%
  
  print(
    n = 60
  )


# 21. Best Model

best_readiness_model <- readiness_model_results %>%
  
  filter(
    Model !=
      "Majority-Class Baseline"
  ) %>%
  
  arrange(
    desc(
      Balanced_Accuracy
    ),
    desc(
      ROC_AUC
    ),
    desc(
      F1
    )
  ) %>%
  
  slice(1)


print(
  best_readiness_model
)


best_feature_set <-
  best_readiness_model$Feature_Set


best_model_predictions <- readiness_predictions %>%
  
  filter(
    Feature_Set ==
      best_feature_set,
    
    Model ==
      "Engineered Ranger RF"
  )


best_v2_confusion_matrix <- with(
  best_model_predictions,
  table(
    Actual,
    Predicted
  )
)


print(
  best_v2_confusion_matrix
)


# 22. Plots

model_comparison_plot <- readiness_model_results %>%
  
  filter(
    Model != "Majority-Class Baseline"
  ) %>%
  
  ggplot(
    aes(
      x = reorder(Feature_Set, Balanced_Accuracy),
      y = Balanced_Accuracy
    )
  ) +
  
  geom_col(width = 0.6) +
  
  geom_text(
    aes(
      label = scales::percent(Balanced_Accuracy, accuracy = 0.01)
    ),
    hjust = -0.15,
    size = 4
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.75),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    title = "Comparison of Final Readiness Classification Models",
    x = NULL,
    y = "Balanced Accuracy"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    axis.title.y = element_blank()
  )


print(
  model_comparison_plot
)


best_feature_plot_data <- readiness_feature_frequency %>%
  
  filter(
    Feature_Set ==
      best_feature_set
  ) %>%
  
  slice_head(
    n =
      25
  )


best_feature_plot <- ggplot(
  best_feature_plot_data,
  
  aes(
    x =
      reorder(
        Variable,
        Mean_Importance
      ),
    
    y =
      Mean_Importance
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title =
      paste(
        "Stable Predictors:",
        best_feature_set
      ),
    
    subtitle =
      "Training-fold permutation importance",
    
    x =
      NULL,
    
    y =
      "Mean permutation importance"
  ) +
  
  theme_minimal(
    base_size =
      12
  )


print(
  best_feature_plot
)


# 23. Save Outputs

output_directory <- "readiness_model_outputs"


if (
  !dir.exists(
    output_directory
  )
) {
  
  dir.create(
    output_directory
  )
}


write.csv(
  readiness_model_results,
  file.path(
    output_directory,
    "readiness_model_results.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_fold_metrics,
  file.path(
    output_directory,
    "readiness_fold_metrics.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_fold_summary,
  file.path(
    output_directory,
    "readiness_fold_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_predictions,
  file.path(
    output_directory,
    "readiness_predictions.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_tuning_results,
  file.path(
    output_directory,
    "readiness_tuning_results.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_selected_features,
  file.path(
    output_directory,
    "readiness_selected_features.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_feature_frequency,
  file.path(
    output_directory,
    "readiness_feature_frequency.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_screening_results,
  file.path(
    output_directory,
    "readiness_screening_results.csv"
  ),
  row.names = FALSE
)


write.csv(
  readiness_fold_information,
  file.path(
    output_directory,
    "readiness_fold_information.csv"
  ),
  row.names = FALSE
)


ggsave(
  filename =
    file.path(
      output_directory,
      "readiness_model_comparison.png"
    ),
  
  plot =
    model_comparison_plot,
  
  width =
    10,
  
  height =
    6,
  
  dpi =
    300
)


ggsave(
  filename =
    file.path(
      output_directory,
      "readiness_feature_importance.png"
    ),
  
  plot =
    best_feature_plot,
  
  width =
    10,
  
  height =
    8,
  
  dpi =
    300
)


# 24. Final Output

cat(
  "READINESS RANDOM FOREST V2 COMPLETE\n"
)

cat(
  "====================================================\n"
)

cat(
  "Best feature set:",
  best_feature_set,
  "\n"
)

cat(
  "Accuracy:",
  round(
    100 *
      best_readiness_model$Accuracy,
    2
  ),
  "%\n"
)

cat(
  "Balanced accuracy:",
  round(
    100 *
      best_readiness_model$Balanced_Accuracy,
    2
  ),
  "%\n"
)

cat(
  "Sensitivity:",
  round(
    100 *
      best_readiness_model$Sensitivity,
    2
  ),
  "%\n"
)

cat(
  "Specificity:",
  round(
    100 *
      best_readiness_model$Specificity,
    2
  ),
  "%\n"
)

cat(
  "F1:",
  round(
    best_readiness_model$F1,
    3
  ),
  "\n"
)

cat(
  "ROC AUC:",
  round(
    best_readiness_model$ROC_AUC,
    3
  ),
  "\n"
)

cat(
  "Outputs:",
  output_directory,
  "\n"
)

cat(
  "====================================================\n"
)

readiness_model_results
readiness_fold_summary
best_v2_confusion_matrix
readiness_feature_frequency %>%
  group_by(Feature_Set) %>%
  slice_head(n = 20) %>%
  ungroup()


