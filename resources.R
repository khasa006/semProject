library(tidyverse)

# Lasso pre

vip_lasso_pre_rds <- read_rds(file = "./data/vip_lasso_pre.rds")
Pred_plot_lasso_pre_rds <- read_rds(file = "./data/Pred_plot_lasso_pre.rds")
roc_lasso_plot_pre_rds <- read_rds(file = "./data/roc_lasso_plot_pre.rds")
lasso_matrices_pre_rds <- read_rds(file = "./data/lasso_matrices_pre.rds")
lasso_conf_mat_pre_rds <- read_rds(file = "./data/lasso_conf_mat_pre.rds") %>%
  autoplot(type = "heatmap")


# Lasso trim1

vip_lasso_trim1_rds <- read_rds(file = "./data/vip_lasso_trim1.rds")
Pred_plot_lasso_trim1_rds <- read_rds(file = "./data/Pred_plot_lasso_trim1.rds")
roc_lasso_plot_trim1_rds <- read_rds(file = "./data/roc_lasso_plot_trim1.rds")
lasso_matrices_trim1_rds <- read_rds(file = "./data/lasso_matrices_trim1.rds")
lasso_conf_mat_trim1_rds <- read_rds(file = "./data/lasso_conf_mat_trim1.rds") %>%
  autoplot(type = "heatmap")

# Lasso trim2

vip_lasso_trim2_rds <- read_rds(file = "./data/vip_lasso_trim2.rds")
Pred_plot_lasso_pr_rds <- read_rds(file = "./data/Pred_plot_lasso_trim2.rds")
roc_lasso_plot_trim2_rds <- read_rds(file = "./data/roc_lasso_plot_trim2.rds")
lasso_matrices_trim2_rds <- read_rds(file = "./data/lasso_matrices_trim2.rds")
lasso_conf_mat_trim2_rds <- read_rds(file = "./data/lasso_conf_mat_trim2.rds") %>%
  autoplot(type = "heatmap")

# Comparison Table

compare_table <-
  left_join(lasso_matrices_pre_rds, lasso_matrices_trim1_rds, by =".metric") %>%
  left_join(.,lasso_matrices_trim2_rds, by =".metric") %>%
  select(".metric", ".estimate.x", ".estimate.y", ".estimate") %>%
  rename(
    Metric =".metric",
    Pre = ".estimate.x",
    Trim1 = ".estimate.y",
    "Trim2" =".estimate"
  )


# Testing Results

test_vip_rds <- read_rds(file = "./data/test_vip.rds")
test_plot_rds <- read_rds(file = "./data/test_plot.rds")
test_roc_plot_rds <- read_rds(file = "./data/test_roc_plot.rds")
test_roc_rds <- read_rds(file = "./data/test_roc.rds")


# Descriptive  Results

health_descrip <- read_rds(file = "./data/health_descrip.rds")


# SEM Results

cfaFit_summary_rds <- read_rds(file = "./data/cfaFit_summary.rds")
semFit_summary_rds <- read_rds(file = "./data/semFit_summary.rds")
semPlot_rds <- read_rds(file = "./data/semPlot.rds")
