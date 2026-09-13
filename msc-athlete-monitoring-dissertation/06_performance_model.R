# 06_performance_model.R
# Random Forest regression of subsequent match performance.
#
# Input:
#   engineered_match_dataset_audited.rds
#
# Outputs:
#   performance_model_outputs/
#
# Two predictor sets are compared:
#   1. Monitoring Only
#   2. Monitoring + Historical Performance
#
# Evaluation uses four expanding chronological folds by match event.
# Reported metrics are RMSE, MAE and predictive R-squared.

# Packages

library(tidyverse)
library(ranger)

set.seed(2026)

# 3. Load Final Audited Match Data

input_file <- "engineered_match_dataset_audited.rds"

if (!file.exists(input_file)) {
  stop(
    paste0(
      "\nCannot find: ",
      input_file,
      "\n\nPlace this script in the same working directory ",
      "as the final performance dataset.\n"
    )
  )
}

performance_data <- readRDS(input_file)

performance_data <- performance_data %>%
  mutate(
    match_date = as.Date(match_date)
  ) %>%
  arrange(
    match_date,
    match_event_id,
    player_name
  )

cat(
  "Performance dataset loaded.\n",
  "Rows:", nrow(performance_data), "\n",
  "Players:", n_distinct(performance_data$player_name), "\n",
  "Match events:", n_distinct(performance_data$match_event_id), "\n"
)

# 4. Metric Functions

safe_rmse <- function(actual, predicted) {
  
  valid <- complete.cases(actual, predicted)
  
  sqrt(
    mean(
      (
        actual[valid] -
          predicted[valid]
      )^2
    )
  )
}


safe_mae <- function(actual, predicted) {
  
  valid <- complete.cases(actual, predicted)
  
  mean(
    abs(
      actual[valid] -
        predicted[valid]
    )
  )
}


###
# Conventional predictive R-squared
#
# R2 = 1 - SSE / SST
#
# Negative values are possible when the model performs
# worse than simply predicting the test-set mean.
###

safe_r2 <- function(actual, predicted) {
  
  valid <- complete.cases(actual, predicted)
  
  actual <- actual[valid]
  predicted <- predicted[valid]
  
  if (length(actual) < 2) {
    return(NA_real_)
  }
  
  sse <- sum(
    (actual - predicted)^2
  )
  
  sst <- sum(
    (
      actual -
        mean(actual)
    )^2
  )
  
  if (sst == 0) {
    return(NA_real_)
  }
  
  1 - sse / sst
}


evaluate_regression <- function(
    actual,
    predicted
) {
  
  tibble(
    Observations =
      sum(
        complete.cases(
          actual,
          predicted
        )
      ),
    
    RMSE =
      safe_rmse(
        actual,
        predicted
      ),
    
    MAE =
      safe_mae(
        actual,
        predicted
      ),
    
    R2 =
      safe_r2(
        actual,
        predicted
      )
  )
}


# 5. Identify Columns That Must Never Be Predictors

metadata_columns <- c(
  "player_name",
  "team_name",
  "match_date",
  "match_event_id",
  "player_match_id",
  "monitoring_date",
  "days_before_match",
  "reports_combined"
)

###
# These contain CURRENT-match outcome information.
# They must never enter the model.
###

current_performance_columns <- c(
  "performance_rating",
  "offensive_performance",
  "defensive_performance",
  "team_performance"
)


# 6. Valid Historical Performance Variables

historical_performance_candidates <- c(
  "previous_performance",
  "previous_3_mean",
  "previous_5_mean",
  "player_historical_mean"
)

historical_performance_features <-
  intersect(
    historical_performance_candidates,
    names(performance_data)
  )

# 7. Identify Numeric Monitoring Predictors

all_numeric_predictors <- names(
  performance_data
)[
  vapply(
    performance_data,
    is.numeric,
    logical(1)
  )
]

all_numeric_predictors <- setdiff(
  all_numeric_predictors,
  c(
    current_performance_columns,
    metadata_columns
  )
)

####
# MONITORING ONLY:
# excludes previous performance / form variables.
###

monitoring_predictors <- setdiff(
  all_numeric_predictors,
  historical_performance_features
)

### Monitoring + Valid Performance History 

monitoring_plus_history_predictors <-
  unique(
    c(
      monitoring_predictors,
      historical_performance_features
    )
  )

# 8. Feature Set Definitions 

feature_sets <- list(
  
  "Monitoring Only" =
    monitoring_predictors,
  
  "Monitoring + Historical Performance" =
    monitoring_plus_history_predictors
  
)


# 9. Training-Only Feature Screening

screen_predictors <- function(
    training_data,
    predictor_names,
    maximum_missing_percentage = 70
) {
  
  predictor_names <- intersect(
    predictor_names,
    names(training_data)
  )
  
  screening_table <- map_dfr(
    predictor_names,
    function(variable) {
      
      x <- training_data[[variable]]
      
      missing_percentage <-
        100 * mean(is.na(x))
      
      non_missing <- x[
        !is.na(x)
      ]
      
      unique_values <-
        length(
          unique(
            non_missing
          )
        )
      
      variance_value <-
        if (
          length(non_missing) > 1
        ) {
          var(non_missing)
        } else {
          NA_real_
        }
      
      retain <-
        missing_percentage <=
        maximum_missing_percentage &&
        unique_values > 1 &&
        !is.na(variance_value) &&
        variance_value > 1e-12
      
      tibble(
        Variable = variable,
        Missing_Percentage =
          missing_percentage,
        Unique_Values =
          unique_values,
        Variance =
          variance_value,
        Retain =
          retain
      )
    }
  )
  
  retained_predictors <-
    screening_table %>%
    filter(Retain) %>%
    pull(Variable)
  
  list(
    retained_predictors =
      retained_predictors,
    
    screening_table =
      screening_table
  )
}


# 10. Median Imputation

fit_median_imputation <- function(x) {
  
  medians <- vapply(
    x,
    function(column) {
      
      result <- median(
        column,
        na.rm = TRUE
      )
      
      if (
        !is.finite(result)
      ) {
        result <- 0
      }
      
      result
    },
    numeric(1)
  )
  
  medians
}


apply_median_imputation <- function(
    x,
    medians
) {
  
  x <- as.data.frame(x)
  
  for (
    variable in names(medians)
  ) {
    
    missing_rows <-
      is.na(
        x[[variable]]
      )
    
    x[
      missing_rows,
      variable
    ] <- medians[[variable]]
  }
  
  as_tibble(x)
}


# 11. Training-Only Random-Forest Feature Ranking

rank_features <- function(
    training_x,
    training_y,
    seed = 2026
) {
  
  if (ncol(training_x) == 1) {
    
    return(
      tibble(
        Variable =
          names(training_x),
        Importance =
          1
      )
    )
  }
  
  set.seed(seed)
  
  ranking_model <-
    ranger::ranger(
      x =
        training_x,
      y =
        training_y,
      num.trees =
        800,
      mtry =
        max(
          1,
          floor(
            sqrt(
              ncol(
                training_x
              )
            )
          )
        ),
      min.node.size =
        5,
      importance =
        "permutation",
      seed =
        seed,
      num.threads =
        max(
          1,
          parallel::detectCores() - 1
        )
    )
  
  tibble(
    Variable =
      names(
        ranking_model$
          variable.importance
      ),
    
    Importance =
      as.numeric(
        ranking_model$
          variable.importance
      )
  ) %>%
    
    arrange(
      desc(
        Importance
      )
    )
}


# 12. Random Forest Tuning Using Oob Error

tune_random_forest <- function(
    training_x,
    training_y,
    feature_ranking,
    seed = 2026
) {
  
  maximum_features <-
    nrow(feature_ranking)
  
  candidate_feature_counts <-
    unique(
      pmin(
        maximum_features,
        c(
          10,
          20,
          30,
          50,
          75,
          100
        )
      )
    )
  
  candidate_feature_counts <-
    candidate_feature_counts[
      candidate_feature_counts > 0
    ]
  
  tuning_results <- list()
  
  result_number <- 1
  
  for (
    feature_count in
    candidate_feature_counts
  ) {
    
    selected_features <-
      feature_ranking %>%
      slice_head(
        n =
          feature_count
      ) %>%
      pull(
        Variable
      )
    
    current_x <-
      training_x %>%
      select(
        all_of(
          selected_features
        )
      )
    
    base_mtry <-
      max(
        1,
        floor(
          sqrt(
            feature_count
          )
        )
      )
    
    candidate_mtry <- unique(
      pmax(
        1,
        pmin(
          feature_count,
          c(
            floor(
              base_mtry / 2
            ),
            base_mtry,
            base_mtry * 2
          )
        )
      )
    )
    
    for (
      mtry_value in
      candidate_mtry
    ) {
      
      for (
        node_size in
        c(
          3,
          5,
          8,
          12,
          20
        )
      ) {
        
        current_seed <-
          seed +
          feature_count * 100 +
          mtry_value * 10 +
          node_size
        
        set.seed(
          current_seed
        )
        
        current_model <-
          ranger::ranger(
            x =
              current_x,
            y =
              training_y,
            num.trees =
              1200,
            mtry =
              mtry_value,
            min.node.size =
              node_size,
            importance =
              "none",
            seed =
              current_seed,
            num.threads =
              max(
                1,
                parallel::detectCores() -
                  1
              )
          )
        
        oob_prediction <-
          current_model$
          predictions
        
        tuning_results[[
          result_number
        ]] <-
          evaluate_regression(
            training_y,
            oob_prediction
          ) %>%
          
          mutate(
            Feature_Count =
              feature_count,
            
            mtry =
              mtry_value,
            
            min_node_size =
              node_size
          )
        
        result_number <-
          result_number + 1
      }
    }
  }
  
  tuning_table <-
    bind_rows(
      tuning_results
    ) %>%
    
    arrange(
      RMSE,
      MAE
    )
  
  list(
    tuning_table =
      tuning_table,
    
    best =
      tuning_table %>%
      slice(1)
  )
}


# 13. Fit Final Random Forest For One Outer Fold 

fit_rf_fold <- function(
    training_data,
    testing_data,
    predictor_names,
    feature_set_name,
    fold_number,
    seed = 2026
) {
  
  screening <-
    screen_predictors(
      training_data =
        training_data,
      predictor_names =
        predictor_names
    )
  
  retained_predictors <-
    screening$
    retained_predictors
  
  if (
    length(
      retained_predictors
    ) < 2
  ) {
    
    stop(
      paste(
        "Too few predictors retained for",
        feature_set_name,
        "fold",
        fold_number
      )
    )
  }
  
  training_x_raw <-
    training_data %>%
    select(
      all_of(
        retained_predictors
      )
    )
  
  testing_x_raw <-
    testing_data %>%
    select(
      all_of(
        retained_predictors
      )
    )
  
  medians <-
    fit_median_imputation(
      training_x_raw
    )
  
  training_x <-
    apply_median_imputation(
      training_x_raw,
      medians
    )
  
  testing_x <-
    apply_median_imputation(
      testing_x_raw,
      medians
    )
  
  training_y <-
    training_data$
    performance_rating
  
  testing_y <-
    testing_data$
    performance_rating
  
  ranking <-
    rank_features(
      training_x =
        training_x,
      training_y =
        training_y,
      seed =
        seed +
        fold_number * 100
    )
  
  tuning <-
    tune_random_forest(
      training_x =
        training_x,
      training_y =
        training_y,
      feature_ranking =
        ranking,
      seed =
        seed +
        fold_number * 1000
    )
  
  best_parameters <-
    tuning$best
  
  selected_features <-
    ranking %>%
    slice_head(
      n =
        best_parameters$
        Feature_Count
    ) %>%
    pull(
      Variable
    )
  
  final_training_x <-
    training_x %>%
    select(
      all_of(
        selected_features
      )
    )
  
  final_testing_x <-
    testing_x %>%
    select(
      all_of(
        selected_features
      )
    )
  
  final_seed <-
    seed +
    50000 +
    fold_number
  
  set.seed(
    final_seed
  )
  
  final_model <-
    ranger::ranger(
      x =
        final_training_x,
      y =
        training_y,
      num.trees =
        2000,
      mtry =
        best_parameters$mtry,
      min.node.size =
        best_parameters$
        min_node_size,
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
  
  predictions <-
    predict(
      final_model,
      data =
        final_testing_x
    )$predictions
  
  prediction_table <-
    testing_data %>%
    
    transmute(
      outer_fold =
        fold_number,
      
      Feature_Set =
        feature_set_name,
      
      player_name,
      
      match_date,
      
      match_event_id,
      
      Actual =
        performance_rating,
      
      Predicted =
        predictions,
      
      Selected_Features =
        length(
          selected_features
        ),
      
      mtry =
        best_parameters$mtry,
      
      min_node_size =
        best_parameters$
        min_node_size
    )
  
  feature_table <-
    tibble(
      outer_fold =
        fold_number,
      
      Feature_Set =
        feature_set_name,
      
      Variable =
        selected_features,
      
      Rank =
        seq_along(
          selected_features
        )
    )
  
  list(
    predictions =
      prediction_table,
    
    features =
      feature_table,
    
    screening =
      screening$
      screening_table,
    
    tuning =
      tuning$
      tuning_table,
    
    model =
      final_model
  )
}


# 14. Create Chronological Expanding-Window Folds

event_order <- performance_data %>%
  
  distinct(
    match_event_id,
    match_date
  ) %>%
  
  arrange(
    match_date,
    match_event_id
  ) %>%
  
  mutate(
    event_number =
      row_number(),
    
    event_fraction =
      event_number /
      n()
  )

performance_data <-
  performance_data %>%
  
  left_join(
    event_order,
    by =
      c(
        "match_event_id",
        "match_date"
      ),
    relationship =
      "many-to-one"
  )


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

print(
  chronological_folds
)


# 15. Run Strict Chronological Random Forest Models

chronological_predictions <-
  list()

chronological_features <-
  list()

chronological_tuning <-
  list()

prediction_number <- 1
feature_number <- 1
tuning_number <- 1


for (
  fold_row in
  seq_len(
    nrow(
      chronological_folds
    )
  )
) {
  
  fold_number <-
    chronological_folds$
    outer_fold[
      fold_row
    ]
  
  training_end <-
    chronological_folds$
    training_end[
      fold_row
    ]
  
  testing_end <-
    chronological_folds$
    testing_end[
      fold_row
    ]
  
  cat("Running chronological fold", fold_number, "
")
  
  training_data <-
    performance_data %>%
    filter(
      event_fraction <=
        training_end
    )
  
  testing_data <-
    performance_data %>%
    filter(
      event_fraction >
        training_end,
      event_fraction <=
        testing_end
    )
  
  ####
  # TRAINING-MEAN BASELINE
  ###
  
  training_mean <-
    mean(
      training_data$
        performance_rating,
      na.rm = TRUE
    )
  
  baseline_prediction <-
    rep(
      training_mean,
      nrow(
        testing_data
      )
    )
  
  chronological_predictions[[
    prediction_number
  ]] <-
    testing_data %>%
    
    transmute(
      outer_fold =
        fold_number,
      
      Feature_Set =
        "Baseline",
      
      player_name,
      
      match_date,
      
      match_event_id,
      
      Actual =
        performance_rating,
      
      Predicted =
        baseline_prediction,
      
      Selected_Features =
        0,
      
      mtry =
        NA_real_,
      
      min_node_size =
        NA_real_
    )
  
  prediction_number <-
    prediction_number + 1
  
  
  ####
  # PREVIOUS PERFORMANCE BASELINE
  ###
  
  if (
    "previous_performance" %in%
    names(
      testing_data
    )
  ) {
    
    chronological_predictions[[
      prediction_number
    ]] <-
      testing_data %>%
      
      transmute(
        outer_fold =
          fold_number,
        
        Feature_Set =
          "Previous Performance Baseline",
        
        player_name,
        
        match_date,
        
        match_event_id,
        
        Actual =
          performance_rating,
        
        Predicted =
          previous_performance,
        
        Selected_Features =
          1,
        
        mtry =
          NA_real_,
        
        min_node_size =
          NA_real_
      )
    
    prediction_number <-
      prediction_number + 1
  }
  
  
  ####
  # ENGINEERED FEATURE SETS
  ###
  
  for (
    feature_set_name in
    names(
      feature_sets
    )
  ) {
    
    current_result <-
      fit_rf_fold(
        training_data =
          training_data,
        
        testing_data =
          testing_data,
        
        predictor_names =
          feature_sets[[
            feature_set_name
          ]],
        
        feature_set_name =
          feature_set_name,
        
        fold_number =
          fold_number,
        
        seed =
          2026
      )
    
    chronological_predictions[[
      prediction_number
    ]] <-
      current_result$
      predictions
    
    prediction_number <-
      prediction_number + 1
    
    chronological_features[[
      feature_number
    ]] <-
      current_result$
      features
    
    feature_number <-
      feature_number + 1
    
    chronological_tuning[[
      tuning_number
    ]] <-
      current_result$
      tuning %>%
      
      mutate(
        outer_fold =
          fold_number,
        
        Feature_Set =
          feature_set_name,
        
        .before =
          1
      )
    
    tuning_number <-
      tuning_number + 1
  }
}


# 16. Combine Chronological Results

chronological_predictions <-
  bind_rows(
    chronological_predictions
  )

chronological_selected_features <-
  bind_rows(
    chronological_features
  )

chronological_tuning_results <-
  bind_rows(
    chronological_tuning
  )


# 17. Overall Strict Chronological Results

chronological_results <-
  chronological_predictions %>%
  
  group_by(
    Feature_Set
  ) %>%
  
  group_modify(
    ~ evaluate_regression(
      .x$Actual,
      .x$Predicted
    )
  ) %>%
  
  ungroup() %>%
  
  arrange(
    RMSE
  )

# 18. Chronological Fold-Level Results

chronological_fold_results <-
  chronological_predictions %>%
  
  group_by(
    outer_fold,
    Feature_Set
  ) %>%
  
  group_modify(
    ~ evaluate_regression(
      .x$Actual,
      .x$Predicted
    )
  ) %>%
  
  ungroup()

# 19. Feature-Selection Stability

feature_stability <-
  chronological_selected_features %>%
  
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
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    Feature_Set,
    desc(
      Folds_Selected
    ),
    Mean_Rank
  )

# Save final outputs

output_directory <- "performance_model_outputs"

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

write.csv(
  chronological_results,
  file.path(output_directory, "performance_model_results.csv"),
  row.names = FALSE
)

write.csv(
  chronological_fold_results,
  file.path(output_directory, "performance_fold_results.csv"),
  row.names = FALSE
)

write.csv(
  chronological_predictions,
  file.path(output_directory, "performance_predictions.csv"),
  row.names = FALSE
)

write.csv(
  chronological_selected_features,
  file.path(output_directory, "performance_selected_features.csv"),
  row.names = FALSE
)

write.csv(
  feature_stability,
  file.path(output_directory, "performance_feature_stability.csv"),
  row.names = FALSE
)

write.csv(
  chronological_tuning_results,
  file.path(output_directory, "performance_tuning_results.csv"),
  row.names = FALSE
)

performance_comparison_plot <- chronological_results %>%
  mutate(
    Feature_Set = factor(
      Feature_Set,
      levels = Feature_Set[order(RMSE, decreasing = TRUE)]
    )
  ) %>%
  ggplot(
    aes(
      x = Feature_Set,
      y = RMSE
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Chronological Match-Performance Model Comparison",
    x = NULL,
    y = "RMSE"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    output_directory,
    "performance_model_comparison.png"
  ),
  plot = performance_comparison_plot,
  width = 9,
  height = 6,
  dpi = 300
)

cat(
  "Performance modelling complete.\n",
  "Results saved to:", output_directory, "\n"
)
