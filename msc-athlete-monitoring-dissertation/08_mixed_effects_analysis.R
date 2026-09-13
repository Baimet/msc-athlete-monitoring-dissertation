# 07_mixed_effects_analysis.R
# Mixed-effects analysis of predicted match-day under-readiness and match performance.
#
# Input:
#   readiness_performance_bridge_outputs/performance_readiness_bridge.rds
#
# Outputs:
#   mixed_effects_outputs/
#
# The analysis compares a pooled regression, a player random-intercept model,
# and random-slope specifications to assess population-level readiness effects
# and between-player variation in typical match performance.

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)

bridge_file <- file.path(
  "readiness_performance_bridge_outputs",
  "performance_readiness_bridge.rds"
)

if (!file.exists(bridge_file)) {
  stop(
    "Cannot find the readiness-performance bridge file. ",
    "Create the bridge dataset before running this analysis."
  )
}

performance_readiness_bridge <- readRDS(bridge_file)

bridge_complete_data <- performance_readiness_bridge %>%
  filter(
    !is.na(predicted_under_ready_probability),
    !is.na(performance_rating)
  )

# Prepare analysis data 

mixed_bridge_data <- bridge_complete_data %>%
  
  transmute(
    player_name =
      factor(
        player_name
      ),
    
    match_date =
      as.Date(
        match_date
      ),
    
    performance_rating =
      as.numeric(
        performance_rating
      ),
    
    readiness_risk =
      as.numeric(
        predicted_under_ready_probability
      ),
    
    readiness_risk_centred =
      as.numeric(
        predicted_under_ready_probability -
          mean(
            predicted_under_ready_probability,
            na.rm = TRUE
          )
      )
  ) %>%
  
  filter(
    complete.cases(
      player_name,
      match_date,
      performance_rating,
      readiness_risk
    )
  ) %>%
  
  arrange(
    player_name,
    match_date
  )


# 3. Sample-Size Audit 

mixed_sample_summary <- mixed_bridge_data %>%
  
  summarise(
    Observations =
      n(),
    
    Players =
      n_distinct(
        player_name
      ),
    
    Mean_Matches_Per_Player =
      n() /
      n_distinct(
        player_name
      ),
    
    Readiness_Risk_Mean =
      mean(
        readiness_risk
      ),
    
    Readiness_Risk_SD =
      sd(
        readiness_risk
      ),
    
    Performance_Mean =
      mean(
        performance_rating
      ),
    
    Performance_SD =
      sd(
        performance_rating
      )
  )


player_bridge_counts <- mixed_bridge_data %>%
  
  count(
    player_name,
    name =
      "Matches"
  ) %>%
  
  arrange(
    Matches
  )


player_count_summary <- player_bridge_counts %>%
  
  summarise(
    Minimum =
      min(
        Matches
      ),
    
    First_Quartile =
      quantile(
        Matches,
        0.25
      ),
    
    Median =
      median(
        Matches
      ),
    
    Mean =
      mean(
        Matches
      ),
    
    Third_Quartile =
      quantile(
        Matches,
        0.75
      ),
    
    Maximum =
      max(
        Matches
      ),
    
    Players_With_One_Match =
      sum(
        Matches == 1
      ),
    
    Players_With_At_Least_Three =
      sum(
        Matches >= 3
      ),
    
    Players_With_At_Least_Five =
      sum(
        Matches >= 5
      )
  )


# 4. Models 

# Ordinary pooled regression
pooled_model <- lm(
  performance_rating ~
    readiness_risk_centred,
  
  data =
    mixed_bridge_data
)


# Player random intercept only
random_intercept_model <- lmer(
  performance_rating ~
    readiness_risk_centred +
    (
      1 |
        player_name
    ),
  
  data =
    mixed_bridge_data,
  
  REML =
    FALSE,
  
  control =
    lmerControl(
      optimizer =
        "bobyqa",
      
      optCtrl =
        list(
          maxfun =
            200000
        )
    )
)


# Player random intercept and random readiness slope
random_slope_model <- lmer(
  performance_rating ~
    readiness_risk_centred +
    (
      1 +
        readiness_risk_centred |
        player_name
    ),
  
  data =
    mixed_bridge_data,
  
  REML =
    FALSE,
  
  control =
    lmerControl(
      optimizer =
        "bobyqa",
      
      optCtrl =
        list(
          maxfun =
            200000
        )
    )
)


# A less demanding random-slope specification that does not
# estimate the intercept–slope correlation
uncorrelated_slope_model <- lmer(
  performance_rating ~
    readiness_risk_centred +
    (
      1 |
        player_name
    ) +
    (
      0 +
        readiness_risk_centred |
        player_name
    ),
  
  data =
    mixed_bridge_data,
  
  REML =
    FALSE,
  
  control =
    lmerControl(
      optimizer =
        "bobyqa",
      
      optCtrl =
        list(
          maxfun =
            200000
        )
    )
)


# 6. Singularity And Convergence Checks

mixed_model_diagnostics <- tibble(
  Model =
    c(
      "Random intercept",
      "Random intercept + correlated slope",
      "Random intercept + uncorrelated slope"
    ),
  
  Singular =
    c(
      lme4::isSingular(
        random_intercept_model,
        tol = 1e-4
      ),
      
      lme4::isSingular(
        random_slope_model,
        tol = 1e-4
      ),
      
      lme4::isSingular(
        uncorrelated_slope_model,
        tol = 1e-4
      )
    ),
  
  Convergence_Message =
    c(
      paste(
        random_intercept_model@optinfo$
          conv$lme4$messages,
        collapse = "; "
      ),
      
      paste(
        random_slope_model@optinfo$
          conv$lme4$messages,
        collapse = "; "
      ),
      
      paste(
        uncorrelated_slope_model@optinfo$
          conv$lme4$messages,
        collapse = "; "
      )
    )
) %>%
  
  mutate(
    Convergence_Message =
      if_else(
        is.na(
          Convergence_Message
        ) |
          Convergence_Message ==
          "",
        
        "No convergence message",
        
        Convergence_Message
      )
  )


# 7. Model Comparison

# Test Whether The Player Random Intercept Contributes 

random_intercept_test <- lmerTest::ranova(
  random_intercept_model
)

random_slope_comparison <- anova(
  random_intercept_model,
  uncorrelated_slope_model,
  random_slope_model
)

# Descriptive Fit Comparison 

pooled_vs_random_intercept_fit <- tibble(
  Model = c(
    "Pooled regression",
    "Player random intercept"
  ),
  
  AIC = c(
    AIC(pooled_model),
    AIC(random_intercept_model)
  ),
  
  BIC = c(
    BIC(pooled_model),
    BIC(random_intercept_model)
  ),
  
  Log_Likelihood = c(
    as.numeric(
      logLik(pooled_model)
    ),
    as.numeric(
      logLik(random_intercept_model)
    )
  )
)

# 8. Fixed-Effect Results Without Broom.Mixed 

extract_lmer_fixed_effects <- function(
    model,
    model_name
) {
  
  coefficient_table <- coef(
    summary(model)
  )
  
  confidence_intervals <- confint(
    model,
    parm = "beta_",
    method = "Wald"
  )
  
  tibble(
    Model = model_name,
    term = rownames(coefficient_table),
    estimate = coefficient_table[, "Estimate"],
    std.error = coefficient_table[, "Std. Error"],
    statistic = coefficient_table[, "t value"],
    p.value = coefficient_table[, "Pr(>|t|)"],
    conf.low = confidence_intervals[, 1],
    conf.high = confidence_intervals[, 2]
  )
}


pooled_coefficient_table <- coef(
  summary(
    pooled_model
  )
)


pooled_confidence_intervals <- confint(
  pooled_model
)


pooled_fixed_effects <- tibble(
  Model = "Pooled regression",
  term = rownames(pooled_coefficient_table),
  estimate = pooled_coefficient_table[, "Estimate"],
  std.error = pooled_coefficient_table[, "Std. Error"],
  statistic = pooled_coefficient_table[, "t value"],
  p.value = pooled_coefficient_table[, "Pr(>|t|)"],
  conf.low = pooled_confidence_intervals[, 1],
  conf.high = pooled_confidence_intervals[, 2]
)


mixed_fixed_effects <- bind_rows(
  
  pooled_fixed_effects,
  
  extract_lmer_fixed_effects(
    random_intercept_model,
    "Player random intercept"
  ),
  
  extract_lmer_fixed_effects(
    uncorrelated_slope_model,
    "Player random intercept and slope"
  )
)


# 9. Variance Components And Icc 

random_intercept_variance <- as.data.frame(
  VarCorr(
    random_intercept_model
  )
)


player_intercept_variance <-
  random_intercept_variance %>%
  
  filter(
    grp ==
      "player_name"
  ) %>%
  
  pull(
    vcov
  )


residual_variance <-
  random_intercept_variance %>%
  
  filter(
    grp ==
      "Residual"
  ) %>%
  
  pull(
    vcov
  )


player_icc <-
  player_intercept_variance /
  (
    player_intercept_variance +
      residual_variance
  )


mixed_variance_summary <- tibble(
  Player_Intercept_Variance =
    player_intercept_variance,
  
  Residual_Variance =
    residual_variance,
  
  Player_ICC =
    player_icc,
  
  Player_Intercept_SD =
    sqrt(
      player_intercept_variance
    ),
  
  Residual_SD =
    sqrt(
      residual_variance
    )
)


# 10. Marginal And Conditional R-Squared 

mixed_r2_summary <- bind_rows(
  
  performance::r2_nakagawa(
    random_intercept_model
  ) %>%
    
    as.data.frame() %>%
    
    transmute(
      Model =
        "Player random intercept",
      
      Marginal_R2 =
        R2_marginal,
      
      Conditional_R2 =
        R2_conditional
    ),
  
  
  performance::r2_nakagawa(
    uncorrelated_slope_model
  ) %>%
    
    as.data.frame() %>%
    
    transmute(
      Model =
        "Player random intercept and slope",
      
      Marginal_R2 =
        R2_marginal,
      
      Conditional_R2 =
        R2_conditional
    )
)

# Model comparison summary

model_comparison_summary <- tibble(
  Model = c(
    "Pooled regression",
    "Player random intercept",
    "Player random intercept + uncorrelated slope",
    "Player random intercept + correlated slope"
  ),
  AIC = c(
    AIC(pooled_model),
    AIC(random_intercept_model),
    AIC(uncorrelated_slope_model),
    AIC(random_slope_model)
  ),
  BIC = c(
    BIC(pooled_model),
    BIC(random_intercept_model),
    BIC(uncorrelated_slope_model),
    BIC(random_slope_model)
  ),
  Log_Likelihood = c(
    as.numeric(logLik(pooled_model)),
    as.numeric(logLik(random_intercept_model)),
    as.numeric(logLik(uncorrelated_slope_model)),
    as.numeric(logLik(random_slope_model))
  )
)

# Save outputs

output_directory <- "mixed_effects_outputs"

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

write.csv(
  mixed_sample_summary,
  file.path(output_directory, "mixed_effects_sample_summary.csv"),
  row.names = FALSE
)

write.csv(
  player_count_summary,
  file.path(output_directory, "player_match_count_summary.csv"),
  row.names = FALSE
)

write.csv(
  mixed_fixed_effects,
  file.path(output_directory, "mixed_effects_fixed_effects.csv"),
  row.names = FALSE
)

write.csv(
  mixed_variance_summary,
  file.path(output_directory, "mixed_effects_variance_icc.csv"),
  row.names = FALSE
)

write.csv(
  mixed_r2_summary,
  file.path(output_directory, "mixed_effects_r2.csv"),
  row.names = FALSE
)

write.csv(
  mixed_model_diagnostics,
  file.path(output_directory, "mixed_effects_diagnostics.csv"),
  row.names = FALSE
)

write.csv(
  model_comparison_summary,
  file.path(output_directory, "mixed_effects_model_comparison.csv"),
  row.names = FALSE
)

capture.output(
  random_intercept_test,
  file = file.path(output_directory, "random_intercept_test.txt")
)

capture.output(
  random_slope_comparison,
  file = file.path(output_directory, "random_slope_comparison.txt")
)

cat(
  "Mixed-effects analysis complete.\n",
  "Observations:", nrow(mixed_bridge_data), "\n",
  "Players:", n_distinct(mixed_bridge_data$player_name), "\n",
  "Outputs saved to:", output_directory, "\n"
)
