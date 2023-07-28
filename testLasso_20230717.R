# Lasso Regression on test data
# Kazi Tanvir Hasan
# 2023-07-17


# Load Preprocess data (contains train and test datasets)
source("preprocess.R")

# *****Test Data*****


lasso_pred <-
  predict(lasso_fit_trim1, analysis_test[c(1, 21:39, 59)], type = "prob") %>%
  bind_cols(analysis_test[c(1, 21:39, 59)] %>% select(anomalies))

test_plot <- lasso_pred %>%
  ggplot() +
  geom_density(
    aes(x = .pred_0,
        fill = relapse
    ),
    alpha = 0.5
  ) +
  labs(
    title = "True Anomalies vs Prediction Probability",
    x = "Probability Of Anomalies",
    y = "Density"
  ) +
  ggthemes::theme_few() +
  theme(legend.title = element_blank())

test_roc_plot <- lasso_pred %>%
  roc_curve(truth = anomalies, .pred_0) %>%
  autoplot()

test_roc_plot

test_roc <- lasso_pred %>%
  roc_auc(truth = anomalies, .pred_0)

test_roc

test_vip <-lasso_fit_trim1 %>%
  fit(analysis_test[c(1, 21:39, 59)]) %>%
  extract_fit_parsnip() %>%
  vip()

test_vip

# Create a directory for saving data if it doesn't exist
if (!dir.exists(paste0(here::here(), "/data"))) {
  dir.create(paste0(here::here(), "/data"))
}

# Save the coefficients to a file
saveRDS(test_vip, "./data/test_vip.rds")
saveRDS(test_plot, file = "./data/test_plot.rds")
saveRDS(test_roc_plot, file = "./data/test_roc_plot.rds")
saveRDS(test_roc, file = "./data/test_roc.rds")


