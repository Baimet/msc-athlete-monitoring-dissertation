# SoccerMon Dissertation Analysis

R code for an MSc dissertation investigating longitudinal athlete-monitoring data, next-day readiness, and subsequent match performance in elite women's football.

This repository contains the final analytical workflow used in the dissertation. The scripts are numbered in the order in which they should be run.

## Software

The analysis was conducted in **R**.

Main packages used across the workflow include:

- `tidyverse`
- `lubridate`
- `arrow`
- `geosphere`
- `slider`
- `ranger`
- `fastshap`
- `lme4`
- `lmerTest`
- `performance`
- `cluster`
- `ggplot2`

Each script loads the packages required for its own analysis.

## Script Order

### `00_gnss_processing.R`

Processes the raw high-frequency SoccerMon GNSS/IMU Parquet files one session at a time.

The script derives session-level measures including:

- total distance
- high-speed running distance
- sprint duration
- moving time
- speed measures
- acceleration measures
- gyroscope measures
- GNSS quality indicators

Expected input folder:

```text
objective_raw/
```

Output folder:

```text
objective_processed/
```

### `01_data_preparation.R`

Combines subjective wellness, training-load, and processed GNSS information into a common player-date structure.

Where multiple GNSS sessions occur on the same day, these are aggregated to daily level.

Main output:

```text
monitoring_daily_unique.rds
```

### `02_feature_engineering.R`

Creates the temporal predictors used in modelling.

These include:

- exact calendar-day lags
- rolling summaries
- recent variability
- day-to-day changes
- deviations from recent averages
- derived workload and recovery indicators

Features are constructed separately within each player's chronological record.

Main outputs:

```text
feature_engineered_monitoring_full.rds
feature_engineered_readiness_v2.rds
```

### `03_readiness_model.R`

Builds and evaluates the next-day readiness classification models using chronological validation.

The script compares:

- Engineered Core
- Engineered Core + wider GPS set
- majority-class baseline

The main model is a Random Forest classifier evaluated using expanding chronological folds.

Main output folder:

```text
readiness_model_outputs/
```

### `04_readiness_explainability.R`

Uses SHAP values to examine the predictors contributing most strongly to the final readiness model.

Outputs include:

- global SHAP importance
- permutation importance
- SHAP contribution distributions
- dependence plots

This script should be run after `03_readiness_model.R`.

### `05_performance_data_build.R`

Constructs the player-match analytical dataset used for match-performance prediction.

Historical monitoring information is linked to subsequent match-performance records while retaining only information available before the corresponding match.

Main output:

```text
engineered_match_dataset_audited.rds
```

### `06_performance_model.R`

Evaluates subsequent match-performance regression using chronological validation.

The script compares:

- Monitoring Only
- Monitoring + Historical Performance
- chronological training-mean baseline
- previous-performance baseline

Evaluation metrics include:

- RMSE
- MAE
- predictive R-squared

Main output folder:

```text
performance_model_outputs/
```

### `07_readiness_performance_bridge.R`

Links out-of-fold readiness predictions to the corresponding pre-match monitoring record and subsequent match-performance observation.

Main output:

```text
readiness_performance_bridge_outputs/performance_readiness_bridge.rds
```

### `08_mixed_effects_analysis.R`

Examines the relationship between readiness and subsequent match performance while accounting for repeated observations within players.

The analysis compares:

- pooled regression
- player random-intercept model
- random-slope alternatives

Reported outputs include:

- fixed effects
- player-level variance
- residual variance
- ICC
- marginal and conditional R-squared
- model-fit comparisons

### `09_clara_clustering.R`

Uses CLARA clustering to identify recurring multidimensional monitoring profiles from wellness and training-load variables.

Candidate cluster solutions are compared using silhouette width. The selected profiles are then described and compared with next-day readiness status.

## Expected Input Data

The analysis uses files from the publicly available **SoccerMon** dataset.

### Subjective wellness files

```text
fatigue.csv
mood.csv
readiness.csv
sleep_duration.csv
sleep_quality.csv
soreness.csv
stress.csv
```

### Training-load files

```text
acwr.csv
atl.csv
ctl28.csv
ctl42.csv
daily_load.csv
monotony.csv
strain.csv
weekly_load.csv
```

### Match-performance file

```text
game-performance.csv
```

### Objective GNSS/IMU data

The raw SoccerMon Parquet files should be organised under:

```text
objective_raw/
```

The raw objective repository is large, so it is not duplicated in the analysis-output folders. `00_gnss_processing.R` processes the files sequentially and creates smaller session-level CSV summaries.

## Suggested Project Structure

```text
msc-athlete-monitoring-dissertation/
│
├── 00_gnss_processing.R
├── 01_data_preparation.R
├── 02_feature_engineering.R
├── 03_readiness_model.R
├── 04_readiness_explainability.R
├── 05_performance_data_build.R
├── 06_performance_model.R
├── 07_readiness_performance_bridge.R
├── 08_mixed_effects_analysis.R
├── 09_clara_clustering.R
├── README.md
│
├── fatigue.csv
├── mood.csv
├── readiness.csv
├── sleep_duration.csv
├── sleep_quality.csv
├── soreness.csv
├── stress.csv
├── acwr.csv
├── atl.csv
├── ctl28.csv
├── ctl42.csv
├── daily_load.csv
├── monotony.csv
├── strain.csv
├── weekly_load.csv
├── game-performance.csv
│
├── objective_raw/
└── objective_processed/
```

## Temporal and Leakage Controls

The analytical workflow is designed so that predictors precede the outcome being predicted.

For readiness classification:

- monitoring information available up to the current observation is used to predict readiness on the following day
- validation is chronological
- preprocessing parameters are derived from training data where applicable

For match-performance prediction:

- only monitoring information recorded before the competitive match is used
- historical-performance variables are constructed from earlier matches only
- match events are kept chronologically ordered during validation

Where preprocessing is part of model fitting, imputation values, feature screening, and feature ranking are derived from the training data rather than from future evaluation observations.

## Reproducibility Notes

The repository contains the final analytical workflow used in the dissertation.

Intermediate exploratory scripts, superseded model versions, debugging files, and temporary diagnostics are not required to reproduce the reported analyses.

Because the raw objective-data repository is approximately 100 GB, processing time for `00_gnss_processing.R` will depend on the computer used. The remaining scripts operate on substantially smaller derived datasets.

All submitted scripts use relative file paths rather than machine-specific user directories.

## Data Source

The dataset used in this dissertation is:

**SoccerMon: A large-scale multivariate soccer athlete health, performance, and position monitoring dataset**

The source dataset is publicly available through Zenodo and is described in the corresponding Scientific Data publication.

Dataset: https://doi.org/10.5281/zenodo.10033832
