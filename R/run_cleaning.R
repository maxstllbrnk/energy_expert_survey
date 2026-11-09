################################################################################
# run_cleaning.R — from the raw exports to the cleaned data sets and the report
# Inputs : raw exports (RAW_DIR, secure); batch overview, design matrix, postcode
#          lookups and degree days (PUBLIC_DATA_DIR)
# Outputs: CLEAN_DATA_DIR/survey_main.*, vignettes_long.* (one row per started
#          questionnaire, nothing decided yet); experts.rds, vignettes.rds,
#          reference_values.rds (what every analysis starts from);
#          OUTPUT_DIR/cleaning/ (codebook; cleaning_report.xlsx and .rds)
# Checks : see the scripts; afterwards run tests/selftest.R
################################################################################
# In RStudio: open energy_expert_survey.Rproj, open this file, click "Source".
# In a terminal, from the project folder: Rscript R/run_cleaning.R
# On a new computer, first install the recorded package versions: renv::restore().
# Raw data is only ever read, and nothing is written into the project folder.
#
# A synthetic run (Claude Code, see config.R) has no raw exports: it starts at step
# 06 from what tests/simulate_data.R wrote. The report leaves out the sheets of the
# raw stage.

source(file.path("R", "setup.R"))
source(file.path("R", "cleaning", "00_functions.R"), encoding = "UTF-8")
source(file.path("R", "cleaning", "00_preferences.R"), encoding = "UTF-8")

if (!USE_SYNTHETIC_DATA) {
  versand <- load_versand()
  source(file.path("R", "cleaning", "01_codebook.R"), encoding = "UTF-8")    # codebook
  source(file.path("R", "cleaning", "02_read_stack.R"), encoding = "UTF-8")  # read and stack the 12 waves
  source(file.path("R", "cleaning", "03_recode.R"), encoding = "UTF-8")      # postcode, "Sonstiges", flags
  source(file.path("R", "cleaning", "04_vignettes.R"), encoding = "UTF-8")   # vignettes_long
  source(file.path("R", "cleaning", "05_main.R"), encoding = "UTF-8")        # survey_main, data quality
}
source(file.path("R", "cleaning", "06_experts_vignettes.R"), encoding = "UTF-8")  # experts, vignettes
source(file.path("R", "cleaning", "07_report.R"), encoding = "UTF-8")             # the cleaning report

message("\nDone.")
