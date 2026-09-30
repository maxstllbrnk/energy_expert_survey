# =============================================================================
# run_cleaning.R  -  build the three clean data sets from the raw exports
# =============================================================================
# In RStudio: open energy_expert_survey.Rproj, open this file, click "Source".
# In a terminal, from the project folder:   Rscript R/run_cleaning.R
#
# First time on a new computer, install the recorded package versions:
#   renv::restore()
#
# Reads the raw exports from RAW_DIR (secure) and the two non-sensitive inputs
# from Dropbox; writes the cleaned data sets to CLEAN_DATA_DIR (secure) and the
# codebook and diagnostics to REPORT_DIR (Dropbox). All six paths come from
# config.R. Raw data is only ever read, never modified, and nothing is written
# into this project folder.
#
# Each analysis belongs in a folder of its own under R/ (R/summary_statistics/,
# R/first_policy_brief/, ...) - this file is only about turning the raw exports
# into clean data.
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source("config.R")
source("R/cleaning/00_functions.R")

versand <- load_versand()

source("R/cleaning/01_codebook.R")    # DATA SET 1  -> codebook.xlsx
source("R/cleaning/02_read_stack.R")  #               read + merge all 12 waves
source("R/cleaning/03_recode.R")      #               postcode, "Sonstiges", flags
source("R/cleaning/04_vignettes.R")   # DATA SET 2  -> vignettes_long.*
source("R/cleaning/05_main.R")        # DATA SET 3  -> survey_main.*

message("\nDone.")
