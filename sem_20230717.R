# SEM Analysis
# Kazi Tanvir Hasan
# 2023-07-17


# Load Preprocess data (contains train and test datasets)
source("preprocess.R")



# Define the CFA model
cfaModel <- '
  # Define latent variables (Measurement model)
  trim1 =~ trim1_Zirconium + trim1_Vanadium + trim1_Zinc


  maternal_health =~  MOTHER_AGE + PrePregnancy_BMI +  MR_DIAB + MR_HYPERT_CHRONIC + MR_HYPERT_ECLAMPSIA

  child_anom =~ ANOM_CHROM + ANOM_DOWNS

'

# Fit the CFA model using lavaan
cfaFit <- cfa(cfaModel, data = birthMetalData, estimator = 'DWLS')

# Print CFA model results
cfaFit_summary <- summary(cfaFit, fit.measures = TRUE, standardized = TRUE)

cfaFit_summary

# Define the SEM model
semModel <- '
  # Define latent variables (Measurement model)
  trim1 =~ trim1_Zirconium + trim1_Vanadium + trim1_Zinc

  maternal_health =~  MOTHER_AGE + PrePregnancy_BMI +  MR_DIAB + MR_HYPERT_CHRONIC + MR_HYPERT_ECLAMPSIA

  child_anom =~ ANOM_CHROM + ANOM_DOWNS

  # Specify structural paths
  ## Direct effects
  child_anom ~ trim1

  ## Indirect effects
  child_anom ~ maternal_health
  maternal_health ~ trim1
'

# Fit the SEM model using lavaan
semFit <- sem(semModel, data = birthMetalData, estimator = 'DWLS')

# Print SEM model results
semFit_summary <-  summary(semFit, fit.measures = TRUE, standardized = TRUE)

semFit_summary

# Plot the model
semPlot <- graph_sem(model = semFit)

# Create a directory for saving data if it doesn't exist
if (!dir.exists(paste0(here::here(), "/data"))) {
  dir.create(paste0(here::here(), "/data"))
}

# Save the coefficients to a file
saveRDS(cfaFit_summary, "./data/cfaFit_summary.rds")
saveRDS(semFit_summary, file = "./data/semFit_summary.rds")
saveRDS(semPlot, file = "./data/semPlot.rds")
