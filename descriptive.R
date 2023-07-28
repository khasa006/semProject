# Descriptive Table
# Kazi Tanvir Hasan
# 2023-07-28


birthMetalData %>%
  select(MOTHER_AGE, PrePregnancy_BMI, weight_gain, MR_DIAB, MR_HYPERT_CHRONIC, PE,
         MR_HYPERT_ECLAMPSIA, ANOM_CHROM, ANOM_DOWNS, ANOM_HEART, ANOM_DIAPH_HERNIA) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)"
  ) %>%
  modify_caption("**Table 1. Mother Health Characteristics**") %>%
  bold_labels()

birthMetalData %>%
  select(
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
  pivot_longer(cols = starts_with("pregest_"),
               names_to = "metal_pregest",
               values_to = "value_pregest") %>%
  pivot_longer(cols = starts_with("trim1_"),
               names_to = "metal_trim1",
               values_to = "value_trim1") %>%
  pivot_longer(cols = starts_with("trim2_"),
               names_to = "metal_trim2",
               values_to = "value_trim2")
