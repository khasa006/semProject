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
tidymodels_prefer()

# suppress "`summarise()` has grouped output by " messages
options(dplyr.summarise.inform = FALSE)




BR_MI_2021 <- read_csv("data/BR_MI_2021.csv")

birthMetalData <- BR_MI_2021 %>%
  select(
    BIRTHID, MOTHER_AGE, PrePregnancy_BMI, weight_gain, MR_DIAB, MR_HYPERT_CHRONIC, PE,
    MR_HYPERT_ECLAMPSIA, ANOM_CHROM, ANOM_DOWNS, ANOM_HEART, ANOM_DIAPH_HERNIA,
    pregest_Aluminum, pregest_Arsenic, pregest_Bromine, pregest_Calcium,
    pregest_Chloride, pregest_Chlorine, pregest_Chromium, pregest_Copper,
    pregest_Iron, pregest_Lead, pregest_Magnesium, pregest_Manganese,
    pregest_Nickel, pregest_Phosphorus, pregest_Potassium, pregest_Rubidium,
    pregest_Selenium, pregest_Silicon, pregest_Sodium, pregest_Soil,
    pregest_Strontium, pregest_Sulfate, pregest_Sulfur, pregest_Titanium,
    pregest_Vanadium, pregest_Zinc, pregest_Zirconium
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
      pregest_Aluminum, pregest_Arsenic, pregest_Bromine, pregest_Calcium,
      pregest_Chloride, pregest_Chlorine, pregest_Chromium, pregest_Copper,
      pregest_Iron, pregest_Lead, pregest_Magnesium, pregest_Manganese,
      pregest_Nickel, pregest_Phosphorus, pregest_Potassium, pregest_Rubidium,
      pregest_Selenium, pregest_Silicon, pregest_Sodium, pregest_Soil,
      pregest_Strontium, pregest_Sulfate, pregest_Sulfur, pregest_Titanium,
      pregest_Vanadium, pregest_Zinc, pregest_Zirconium
    ),
    ~ scale(.) %>% as.vector()
  )) %>%
  na.omit()

# Set the random seed for replicability
set.seed(0711)

# Make Modeling Data
data_split <- birthMetalData %>%
  mutate(
    anomalies = case_when(
      ANOM_CHROM == "Y" | ANOM_DIAPH_HERNIA == "Y" | ANOM_HEART == "Y" | ANOM_DOWNS == "Y" ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  initial_split(strata = anomalies)

train_data <- training(data_split)
test_data <- testing(data_split)


# Double Check Modeling Data
train_data %>%
  tabyl(anomalies) %>%
  adorn_pct_formatting(0) %>%
  adorn_totals()

test_data %>%
  tabyl(anomalies) %>%
  adorn_pct_formatting(0) %>%
  adorn_totals()

# Select variables for LASSO modeling
analysis_columns <- train_data %>%
  select(
    pregest_Aluminum, pregest_Arsenic, pregest_Bromine, pregest_Calcium,
    pregest_Chloride, pregest_Chlorine, pregest_Chromium, pregest_Copper,
    pregest_Iron, pregest_Lead, pregest_Magnesium, pregest_Manganese,
    pregest_Nickel, pregest_Phosphorus, pregest_Potassium, pregest_Rubidium,
    pregest_Selenium, pregest_Silicon, pregest_Sodium, pregest_Soil,
    pregest_Strontium, pregest_Sulfate, pregest_Sulfur, pregest_Titanium,
    pregest_Vanadium, pregest_Zinc, pregest_Zirconium, anomalies
  ) %>%
  mutate(anomalies = as.factor(anomalies)) %>%
  mutate(anomalies = as.numeric(anomalies))

# Register parallel processing
registerDoParallel()

# Prepare variables for the BKMR model

# dependent variable
y <- as.numeric(unlist(analysis_columns[, 28]))

# scale exposure variable
expos <- data.matrix(analysis_columns[, 1:27])


# Fit the model
fitkm <- kmbayes(
  y = y,
  Z = expos,
  X = NULL,
  iter = 10000,
  family = "binomial",
  verbose = FALSE,
  varsel = TRUE
)



# Stop implicit cluster for parallel processing
stopImplicitCluster()
