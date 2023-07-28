# Lasso Regression for Pre gestational Period
# Kazi Tanvir Hasan
# 2023-07-17

# Load Preprocess data (contains train and test datasets)
source("preprocess.R")


# Use the tidymodels Framework for Predictions

## Make a recipe
data_recipe <- recipe(anomalies ~ ., data = analysis_train[c(1:20, 59)]) %>%
  update_role(BIRTHID, new_role = "ID") %>%  # Update the role of the 'BIRTHID' variable to 'ID'
  step_nzv() %>%  # Remove near-zero variance predictors
  step_normalize(all_numeric_predictors()) %>%  # Normalize numeric predictors
  step_impute_median(all_numeric_predictors()) %>%  # Impute missing values in numeric predictors with median
  step_impute_mode(all_factor_predictors()) %>%  # Impute missing values in factor predictors with mode
  step_corr(all_numeric_predictors(), threshold = 0.7, method = "spearman") %>%  # Remove highly correlated predictors
  step_dummy(all_factor_predictors())


# Create a specification for the LASSO model
lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# Create a workflow
lasso_wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(lasso_spec)

# Define the tuning grid
lasso_grid <- data.frame(penalty = 10^seq(-6, -1, length.out = 20))

# Tune the LASSO model using the defined grid and cross-validation
lasso_tune <- tune_grid(
  lasso_wf,
  resamples = cv_folds,
  grid = lasso_grid,
  control = control_grid(verbose = TRUE)
)

# Select the best model based on one standard error rule
favorite <- select_by_one_std_err(lasso_tune, penalty, metric = "roc_auc")

# Print the selected model
favorite

# Update the workflow with the selected amount of shrinkage
final_wf <- finalize_workflow(lasso_wf, favorite)


# Fit the Lasso model using the training data
lasso_fit_pre <- final_wf %>% fit(data = analysis_train[c(1:20, 59)])

# Print the fitted model
lasso_fit_pre

## Review Fit on the Training Data

# Make predictions on the training data
anomalieslasso_pred <- predict(lasso_fit_pre, analysis_train[c(1:20, 59)], type = "prob") %>%
  bind_cols(analysis_train[c(1:20, 59)] %>% select(anomalies))

# Print outcomes for the first 10 people
head(anomalieslasso_pred, n = 10)

# Plot the prediction
Pred_plot_lasso_pre <- anomalieslasso_pred %>%
  ggplot() +
  geom_density(
    aes(x = .pred_0, fill = anomalies),
    alpha = 0.5
  ) +
  labs(
    title = "True Anomalies vs Prediction Probability",
    x = "Probability Of Anomalies",
    y = "Density"
  ) +
  ggthemes::theme_few() +
  theme(
    legend.title = element_blank()
  )

# Print the prediction plot
print(Pred_plot_lasso_pre)


# Plot ROC curve for the Lasso model on the training data
roc_lasso_plot_pre <- anomalieslasso_pred %>%
  roc_curve(truth = anomalies, .pred_0) %>%
  autoplot()


roc_lasso_plot_pre

# Calculate AUC for the Lasso model on the training data
lasso_auc <- anomalieslasso_pred %>%
  roc_auc(truth = anomalies, .pred_0)

# Print the AUC value
print(lasso_auc)

# Evaluate model metrics
last_lasso_fit <- final_wf %>%
  last_fit(
    analysis_split, # use the test data
    metrics = metric_set(
      accuracy, roc_auc, sens, spec
    )
  )

lasso_matrices_pre <- collect_metrics(last_lasso_fit, summarize = TRUE)

# Generate confusion matrix
lasso_conf_mat_pre <- last_lasso_fit %>%
  collect_predictions() %>%
  conf_mat(anomalies, .pred_class)


## Look at Variable Importance

# Use vip() function from the 'vip' package to calculate variable importance
library(vip)

vip_lasso_pre <- lasso_fit_pre %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

# Print the variable importance
vip_lasso_pre


# Create a directory for saving data if it doesn't exist
if (!dir.exists(paste0(here::here(), "/data"))) {
  dir.create(paste0(here::here(), "/data"))
}

# Save the coefficients to a file
saveRDS(vip_lasso_pre, "./data/vip_lasso_pre.rds")
saveRDS(Pred_plot_lasso_pre, file = "./data/Pred_plot_lasso_pre.rds")
saveRDS(roc_lasso_plot_pre, file = "./data/roc_lasso_plot_pre.rds")
saveRDS(lasso_matrices_pre, file = "./data/lasso_matrices_pre.rds")
saveRDS(lasso_conf_mat_pre, file = "./data/lasso_conf_mat_pre.rds")
