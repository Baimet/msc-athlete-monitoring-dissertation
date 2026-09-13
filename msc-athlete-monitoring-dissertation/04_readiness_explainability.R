# 04_readiness_explainability.R
# Post interpretation of the final next-day readiness model using SHAP.
#
# Inputs:
#   feature_engineered_readiness_v2.rds
#   readiness_model_outputs/readiness_feature_frequency.csv
#
# Outputs:
#   readiness_model_outputs/explainability/

library(tidyverse)
library(ranger)
library(fastshap)

set.seed(2026)

# Load data and selected-feature information

readiness_file <- "feature_engineered_readiness_v2.rds"
feature_file <- file.path(
  "readiness_model_outputs",
  "readiness_feature_frequency.csv"
)

if (!file.exists(readiness_file)) {
  stop("Cannot find 'feature_engineered_readiness_v2.rds'.")
}

if (!file.exists(feature_file)) {
  stop(
    "Cannot find 'readiness_model_outputs/readiness_feature_frequency.csv'. ",
    "Run 03_readiness_model.R first."
  )
}

readiness_data <- readRDS(readiness_file) %>%
  mutate(
    player_name = as.character(player_name),
    date = as.Date(date)
  ) %>%
  filter(!is.na(readiness_next_day)) %>%
  arrange(player_name, date)

readiness_feature_frequency <- read.csv(
  feature_file,
  stringsAsFactors = FALSE
)

# Select stable predictors 
# The interpretation model uses predictors that were selected most consistently
# across the chronological validation folds.

explanation_feature_count <- 100

stable_explanation_features <- readiness_feature_frequency %>%
  filter(Feature_Set == "Engineered Core + wider GPS set") %>%
  arrange(
    desc(Folds_Selected),
    Mean_Rank,
    desc(Mean_Importance)
  ) %>%
  slice_head(n = explanation_feature_count) %>%
  pull(Variable)

stable_explanation_features <- stable_explanation_features[
  stable_explanation_features %in% names(readiness_data)
]

if (length(stable_explanation_features) == 0) {
  stop("No stable predictors were available for the explainability analysis.")
}

# Prepare the interpretation dataset
# This is a post-hoc interpretation model fitted to the full analytical sample.
# Readiness status is defined relative to each player's observed mean readiness.

explanation_data <- readiness_data %>%
  group_by(player_name) %>%
  mutate(
    player_mean_readiness = mean(
      readiness_next_day,
      na.rm = TRUE
    ),
    readiness_class = if_else(
      readiness_next_day < player_mean_readiness,
      "Under_Ready",
      "Ready"
    )
  ) %>%
  ungroup() %>%
  mutate(
    readiness_class = factor(
      readiness_class,
      levels = c("Ready", "Under_Ready")
    )
  )

explanation_x_raw <- explanation_data %>%
  select(all_of(stable_explanation_features))

# Median values are estimated from the interpretation dataset because this
# model is used for explanation rather than out-of-sample performance testing.
explanation_medians <- vapply(
  explanation_x_raw,
  function(x) {
    value <- median(x, na.rm = TRUE)
    if (is.na(value) || is.infinite(value)) 0 else value
  },
  numeric(1)
)

explanation_x <- explanation_x_raw

for (variable_name in names(explanation_x)) {
  missing_rows <- is.na(explanation_x[[variable_name]])
  explanation_x[[variable_name]][missing_rows] <-
    explanation_medians[[variable_name]]
}

stopifnot(sum(is.na(explanation_x)) == 0)

# Fit interpretation Random Forest

explanation_training_frame <- bind_cols(
  readiness_class = explanation_data$readiness_class,
  explanation_x
)

set.seed(2026)

final_explanation_model <- ranger::ranger(
  readiness_class ~ .,
  data = explanation_training_frame,
  probability = TRUE,
  num.trees = 1500,
  mtry = max(1, floor(sqrt(ncol(explanation_x)))),
  min.node.size = 10,
  importance = "permutation",
  seed = 2026,
  num.threads = max(1, parallel::detectCores() - 1)
)

# Permutation importance 

global_permutation_importance <- tibble(
  Variable = names(final_explanation_model$variable.importance),
  Permutation_Importance =
    as.numeric(final_explanation_model$variable.importance)
) %>%
  arrange(desc(Permutation_Importance))

top_permutation_features <- global_permutation_importance %>%
  slice_head(n = 20)

permutation_importance_plot <- ggplot(
  top_permutation_features,
  aes(
    x = reorder(Variable, Permutation_Importance),
    y = Permutation_Importance
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Global Importance for Next-Day Readiness Classification",
    subtitle = "Permutation importance from the final readiness interpretation model",
    x = NULL,
    y = "Permutation importance"
  ) +
  theme_minimal(base_size = 12)

# SHAP sample and prediction function

shap_sample_size <- min(750, nrow(explanation_x))

set.seed(2026)

shap_sample_rows <- sample(
  seq_len(nrow(explanation_x)),
  size = shap_sample_size,
  replace = FALSE
)

shap_x <- explanation_x[
  shap_sample_rows,
  ,
  drop = FALSE
]

shap_metadata <- explanation_data[
  shap_sample_rows,
  c("player_name", "date", "readiness_class")
]

predict_under_ready_probability <- function(object, newdata) {
  predictions <- predict(
    object,
    data = newdata
  )$predictions

  as.numeric(predictions[, "Under_Ready"])
}

# SHAP values

set.seed(2026)

shap_values <- fastshap::explain(
  object = final_explanation_model,
  X = explanation_x,
  pred_wrapper = predict_under_ready_probability,
  newdata = shap_x,
  nsim = 50,
  adjust = TRUE,
  .parallel = FALSE
) %>%
  as.data.frame()

# Global SHAP importance

shap_global_importance <- tibble(
  Variable = names(shap_values),
  Mean_Absolute_SHAP = vapply(
    shap_values,
    function(x) mean(abs(x), na.rm = TRUE),
    numeric(1)
  )
) %>%
  arrange(desc(Mean_Absolute_SHAP))

top_shap_features <- shap_global_importance %>%
  slice_head(n = 20)

shap_importance_plot <- ggplot(
  top_shap_features,
  aes(
    x = reorder(Variable, Mean_Absolute_SHAP),
    y = Mean_Absolute_SHAP
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Predictor Importance for Next-Day Readiness",
    x = NULL,
    y = "Mean absolute SHAP value"
  ) +
  theme_minimal(base_size = 12)

# SHAP contribution distribution

top_shap_variable_names <- top_shap_features %>%
  slice_head(n = 15) %>%
  pull(Variable)

shap_long <- shap_values %>%
  mutate(SHAP_Row = row_number()) %>%
  select(
    SHAP_Row,
    all_of(top_shap_variable_names)
  ) %>%
  pivot_longer(
    cols = -SHAP_Row,
    names_to = "Variable",
    values_to = "SHAP_Value"
  )

feature_value_long <- shap_x %>%
  mutate(SHAP_Row = row_number()) %>%
  select(
    SHAP_Row,
    all_of(top_shap_variable_names)
  ) %>%
  pivot_longer(
    cols = -SHAP_Row,
    names_to = "Variable",
    values_to = "Feature_Value"
  )

shap_plot_data <- shap_long %>%
  left_join(
    feature_value_long,
    by = c("SHAP_Row", "Variable"),
    relationship = "one-to-one"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Standardised_Feature_Value =
      if (sd(Feature_Value, na.rm = TRUE) == 0) {
        0
      } else {
        as.numeric(scale(Feature_Value))
      }
  ) %>%
  ungroup() %>%
  mutate(
    Variable = factor(
      Variable,
      levels = rev(top_shap_variable_names)
    )
  )

shap_distribution_plot <- ggplot(
  shap_plot_data,
  aes(
    x = SHAP_Value,
    y = Variable,
    alpha = abs(Standardised_Feature_Value)
  )
) +
  geom_jitter(
    height = 0.22,
    width = 0,
    size = 1.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  guides(alpha = "none") +
  labs(
    title = "Distribution of SHAP Contributions",
    subtitle = "Positive values increase predicted Under-Readiness probability",
    x = "SHAP contribution to Under-Readiness probability",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

# Dependence plots for the four highest-ranking predictors

dependence_variables <- shap_global_importance %>%
  slice_head(n = 4) %>%
  pull(Variable)

shap_dependence_data <- map_dfr(
  dependence_variables,
  function(variable_name) {
    tibble(
      Variable = variable_name,
      Feature_Value = shap_x[[variable_name]],
      SHAP_Value = shap_values[[variable_name]]
    )
  }
)

shap_dependence_plot <- ggplot(
  shap_dependence_data,
  aes(
    x = Feature_Value,
    y = SHAP_Value
  )
) +
  geom_point(alpha = 0.35) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free_x"
  ) +
  labs(
    title = "SHAP Dependence for Key Readiness Predictors",
    subtitle = "Positive SHAP values indicate increased Under-Readiness probability",
    x = "Feature value",
    y = "SHAP contribution"
  ) +
  theme_minimal(base_size = 12)

# Save outputs ----

explanation_output_directory <- file.path(
  "readiness_model_outputs",
  "explainability"
)

if (!dir.exists(explanation_output_directory)) {
  dir.create(
    explanation_output_directory,
    recursive = TRUE
  )
}

write.csv(
  global_permutation_importance,
  file.path(
    explanation_output_directory,
    "global_permutation_importance.csv"
  ),
  row.names = FALSE
)

write.csv(
  shap_global_importance,
  file.path(
    explanation_output_directory,
    "shap_global_importance.csv"
  ),
  row.names = FALSE
)

write.csv(
  shap_plot_data,
  file.path(
    explanation_output_directory,
    "shap_plot_data.csv"
  ),
  row.names = FALSE
)

write.csv(
  shap_dependence_data,
  file.path(
    explanation_output_directory,
    "shap_dependence_data.csv"
  ),
  row.names = FALSE
)

saveRDS(
  shap_values,
  file.path(
    explanation_output_directory,
    "shap_values.rds"
  )
)

saveRDS(
  shap_metadata,
  file.path(
    explanation_output_directory,
    "shap_sample_metadata.rds"
  )
)

ggsave(
  filename = file.path(
    explanation_output_directory,
    "permutation_importance.png"
  ),
  plot = permutation_importance_plot,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    explanation_output_directory,
    "shap_importance.png"
  ),
  plot = shap_importance_plot,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    explanation_output_directory,
    "shap_distribution.png"
  ),
  plot = shap_distribution_plot,
  width = 11,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    explanation_output_directory,
    "shap_dependence.png"
  ),
  plot = shap_dependence_plot,
  width = 12,
  height = 8,
  dpi = 300
)

cat(
  "Explainability analysis complete.\n",
  "Outputs saved to:", explanation_output_directory, "\n"
)
