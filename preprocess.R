# Preprocess for lasso Regression
# Kazi Tanvir Hasan
# 2023-07-17


# Packages to install if not already installed
packages <- c(
  "lavaan", "conflicted","ggthemes", "ggpubr", "table1", "styler", "glmnet",
  "tidySEM", "gmodels", "janitor", "bkmr", "doParallel", "tidyverse", "tidymodels"
)

# Check if packages are installed
installed_packages <- packages %in%
  rownames(
    installed.packages()
  )

# Install packages if any are not installed
if (any(!installed_packages)) {
  packages_to_install <- packages[!installed_packages]
  remotes::install_github(packages_to_install)
}

# Loading the libraries
suppressPackageStartupMessages(
  {
    library(lavaan)
    library(janitor)
    library(ggthemes)
    library(ggpubr)
    library(table1)
    library(styler)
    library(tidySEM)
    library(gmodels)
    library(bkmr)
    library(doParallel)
    library(conflicted)
    library(tidyverse)
    library(tidymodels)
  }
)

# set preference
suppressMessages(conflict_prefer("filter", "dplyr"))
suppressMessages(conflict_prefer("spec", "yardstick"))
suppressMessages(conflicts_prefer(tidySEM::conf_int))
tidymodels_prefer()

# suppress "`summarise()` has grouped output by " messages
options(dplyr.summarise.inform = FALSE)




BR_MI_2021 <- read_csv("data/BR_MI_2021.csv")

birthMetalData <- BR_MI_2021 %>%
  select(
    BIRTHID, MOTHER_AGE, PrePregnancy_BMI, weight_gain, MR_DIAB, MR_HYPERT_CHRONIC, PE,
    MR_HYPERT_ECLAMPSIA, ANOM_CHROM, ANOM_DOWNS, ANOM_HEART, ANOM_DIAPH_HERNIA,
    pregest_Aluminum, pregest_Arsenic, pregest_Calcium, pregest_Chromium,
    pregest_Copper, pregest_Iron, pregest_Lead, pregest_Magnesium,
    pregest_Manganese, pregest_Nickel, pregest_Potassium, pregest_Rubidium,
    pregest_Selenium, pregest_Sodium, pregest_Strontium, pregest_Titanium,
    pregest_Vanadium, pregest_Zinc, pregest_Zirconium,
    trim1_Aluminum, trim1_Arsenic, trim1_Calcium, trim1_Chromium, trim1_Copper,
    trim1_Iron, trim1_Lead, trim1_Magnesium, trim1_Manganese, trim1_Nickel,
    trim1_Potassium, trim1_Rubidium, trim1_Selenium, trim1_Sodium,
    trim1_Strontium, trim1_Titanium, trim1_Vanadium, trim1_Zinc, trim1_Zirconium,
    trim2_Aluminum, trim2_Arsenic, trim2_Calcium, trim2_Chromium, trim2_Copper,
    trim2_Iron, trim2_Lead, trim2_Magnesium, trim2_Manganese, trim2_Nickel,
    trim2_Potassium, trim2_Rubidium, trim2_Selenium, trim2_Sodium,
    trim2_Strontium, trim2_Titanium, trim2_Vanadium, trim2_Zinc, trim2_Zirconium
  ) %>%
  mutate(
    PrePregnancy_BMI = replace(
      PrePregnancy_BMI,
      PrePregnancy_BMI == "Null", NA
    )
  ) %>%
  mutate(
    PrePregnancy_BMI = as.numeric(PrePregnancy_BMI),
    weight_gain = as.numeric(weight_gain),
    MR_DIAB = factor(if_else(MR_DIAB == "U", NA_character_, MR_DIAB), ordered = TRUE),
    MR_HYPERT_CHRONIC = factor(if_else(MR_HYPERT_CHRONIC == "U", NA_character_, MR_HYPERT_CHRONIC), ordered = TRUE),
    PE = factor(if_else(PE == "U", NA_character_, PE), ordered = TRUE),
    MR_HYPERT_ECLAMPSIA = factor(if_else(MR_HYPERT_ECLAMPSIA == "U", NA_character_, MR_HYPERT_ECLAMPSIA), ordered = TRUE),
    ANOM_CHROM = factor(if_else(ANOM_CHROM == "U", NA_character_, ANOM_CHROM), ordered = TRUE),
    ANOM_DOWNS = factor(if_else(ANOM_DOWNS == "U", NA_character_, ANOM_DOWNS), ordered = TRUE),
    ANOM_HEART = factor(if_else(ANOM_HEART == "U", NA_character_, ANOM_HEART), ordered = TRUE),
    ANOM_DIAPH_HERNIA = factor(if_else(ANOM_DIAPH_HERNIA == "U", NA_character_, ANOM_DIAPH_HERNIA), ordered = TRUE)
  ) %>%
  mutate(across(
    c(
      pregest_Aluminum, pregest_Arsenic, pregest_Calcium, pregest_Chromium,
      pregest_Copper, pregest_Iron, pregest_Lead, pregest_Magnesium,
      pregest_Manganese, pregest_Nickel, pregest_Potassium, pregest_Rubidium,
      pregest_Selenium, pregest_Sodium, pregest_Strontium, pregest_Titanium,
      pregest_Vanadium, pregest_Zinc, pregest_Zirconium,
      trim1_Aluminum, trim1_Arsenic, trim1_Calcium, trim1_Chromium, trim1_Copper,
      trim1_Iron, trim1_Lead, trim1_Magnesium, trim1_Manganese, trim1_Nickel,
      trim1_Potassium, trim1_Rubidium, trim1_Selenium, trim1_Sodium,
      trim1_Strontium, trim1_Titanium, trim1_Vanadium, trim1_Zinc, trim1_Zirconium,
      trim2_Aluminum, trim2_Arsenic, trim2_Calcium, trim2_Chromium, trim2_Copper,
      trim2_Iron, trim2_Lead, trim2_Magnesium, trim2_Manganese, trim2_Nickel,
      trim2_Potassium, trim2_Rubidium, trim2_Selenium, trim2_Sodium,
      trim2_Strontium, trim2_Titanium, trim2_Vanadium, trim2_Zinc, trim2_Zirconium
    ),
    ~ scale(.) %>% as.vector()
  ))

# Set the random seed for replicability
set.seed(230717)

# Make Modeling Data

analysis_data <- birthMetalData %>%
  mutate(
    anomalies = case_when(
      ANOM_CHROM == "Y" | ANOM_DOWNS == "Y" ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  data.frame() %>%
  mutate(anomalies = case_when(
    anomalies == "N" ~ 1,
    anomalies == "Y" ~ 0,
    TRUE ~ NA_integer_
  )) %>%
  mutate(anomalies = as.factor(anomalies)) %>%
  select(
    BIRTHID, pregest_Aluminum, pregest_Arsenic, pregest_Calcium, pregest_Chromium,
    pregest_Copper, pregest_Iron, pregest_Lead, pregest_Magnesium,
    pregest_Manganese, pregest_Nickel, pregest_Potassium, pregest_Rubidium,
    pregest_Selenium, pregest_Sodium, pregest_Strontium, pregest_Titanium,
    pregest_Vanadium, pregest_Zinc, pregest_Zirconium,
    trim1_Aluminum, trim1_Arsenic, trim1_Calcium, trim1_Chromium, trim1_Copper,
    trim1_Iron, trim1_Lead, trim1_Magnesium, trim1_Manganese, trim1_Nickel,
    trim1_Potassium, trim1_Rubidium, trim1_Selenium, trim1_Sodium,
    trim1_Strontium, trim1_Titanium, trim1_Vanadium, trim1_Zinc, trim1_Zirconium,
    trim2_Aluminum, trim2_Arsenic, trim2_Calcium, trim2_Chromium, trim2_Copper,
    trim2_Iron, trim2_Lead, trim2_Magnesium, trim2_Manganese, trim2_Nickel,
    trim2_Potassium, trim2_Rubidium, trim2_Selenium, trim2_Sodium,
    trim2_Strontium, trim2_Titanium, trim2_Vanadium, trim2_Zinc, trim2_Zirconium,
    anomalies
  )


analysis_split <- analysis_data %>%
  initial_split(strata = anomalies)

analysis_train <- training(analysis_split)
analysis_test <- testing(analysis_split)



# Make a Cross Validate Object
cv_folds <- vfold_cv(
  analysis_train,
  v = 5  # Number of sets for cross-validation
)

# Double Check Modeling Data

analysis_data %>%
  tabyl(anomalies) %>%
  adorn_pct_formatting(0) %>%
  adorn_totals()

analysis_train %>%
  tabyl(anomalies) %>%
  adorn_pct_formatting(0) %>%
  adorn_totals()

analysis_test %>%
  tabyl(anomalies) %>%
  adorn_pct_formatting(0) %>%
  adorn_totals()



