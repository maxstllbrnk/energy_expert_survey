################################################################################
# config.R — the two roots, every path derived from them, and the constants of
#            the survey
# Inputs : config.local.R (git-ignored): SECURE_DIR and DROPBOX_DIR of this
#          machine
# Outputs: creates the cleaned-data and output folders if they are missing
# Checks : the roots are set, the secure folder and the public inputs exist
################################################################################
# The project lives in three places:
#   SECURE   SECURE_DIR, one folder on the secure drive. Personal data: the raw
#            exports, the cleaned data, the sampling frame and all output, as
#            the fitted models contain the data. Claude Code has no access.
#            SECURE_DIR/
#              260909_raw_energy_expert_survey_data/      raw LimeSurvey exports
#                limesurvey-syntax/                       and the syntax files
#              260909_cleaned_energy_expert_survey_data/  R/run_cleaning.R
#              260915_scraped_data/                       the sampling frame
#              output/                                    all output
#              output_quick/                              a quick run's output
#   DROPBOX  DROPBOX_DIR. Non-sensitive inputs only (public_data_dropbox/):
#            design matrix, batch overview, geodata, degree days.
#   PROJECT  this folder: code only, shared through GitHub. Nothing is written
#            here.

# 1. The roots -----------------------------------------------------------------
# The only two settings that differ between machines. Set both in config.local.R
# in the project root, with forward slashes (R reads "\" in a string as an
# escape character):
#   SECURE_DIR <- "I:/.../energy_experts_survey/wave_1_2026"
#   DROPBOX_DIR <- "C:/Users/<you>/Dropbox/Heating Transition/analysis_survey"
SECURE_DIR <- NA
DROPBOX_DIR <- NA
if (file.exists("config.local.R")) {
  source("config.local.R")
}

# A first run on the real data to see that everything runs: few draws, no
# cross-validation, the output in output_quick/ so that it is never taken for the
# real one. A run with FALSE estimates every model again.
QUICK_RUN <- FALSE

# 2. Secure paths --------------------------------------------------------------
# A session of Claude Code (CLAUDECODE=1) may not touch the real data (project
# profile). It reads and writes synthetic data in a temporary folder instead, set
# by EES_SYNTHETIC_DIR, which tests/simulate_data.R fills with the same folders
# as the secure drive. A run on the real data never takes this branch.
USE_SYNTHETIC_DATA <- Sys.getenv("CLAUDECODE") == "1"
if (USE_SYNTHETIC_DATA) {
  SECURE_DIR <- Sys.getenv("EES_SYNTHETIC_DIR",
                           unset = file.path(Sys.getenv("TEMP"), "ees", "synthetic"))
}
if (is.na(SECURE_DIR) || is.na(DROPBOX_DIR)) {
  stop("Set SECURE_DIR and DROPBOX_DIR in config.local.R (see config.R, section 1).")
}
# A mistyped root would otherwise create the folders below in the wrong place and
# write personal data there
if (!USE_SYNTHETIC_DATA && !dir.exists(SECURE_DIR)) {
  stop("SECURE_DIR does not exist: ", SECURE_DIR,
       "\nFix it in config.local.R, or connect the secure drive.")
}
RAW_DIR <- file.path(SECURE_DIR, "260909_raw_energy_expert_survey_data")
SYNTAX_DIR <- file.path(RAW_DIR, "limesurvey-syntax")
CLEAN_DATA_DIR <- file.path(SECURE_DIR, "260909_cleaned_energy_expert_survey_data")
# The sampling frame: every expert and firm that was contacted, with e-mail,
# postcode and the list they were drawn from. The denominator of the response
# rates.
FRAME_FILE <- file.path(SECURE_DIR, "260915_scraped_data",
                        "260513_allexperts_unique_PLZ.rds")

# All output, one folder per estimation script (R/estimation/), plus the cleaning
# and the summary statistics of the full sample
OUTPUT_DIR <- file.path(SECURE_DIR, if (QUICK_RUN) "output_quick" else "output")
REPORT_DIR <- file.path(OUTPUT_DIR, "cleaning")
CODEBOOK_FILE <- file.path(REPORT_DIR, "codebook.csv")

# 3. Public inputs (read only) -------------------------------------------------
PUBLIC_DATA_DIR <- file.path(DROPBOX_DIR, "public_data_dropbox")
VERSAND_FILE <- file.path(PUBLIC_DATA_DIR, "LimeSurvey_Versanduebersicht_010726.xlsx")
VIGNETTE_FILE <- file.path(PUBLIC_DATA_DIR, "vignetten_kombinationen.xlsx")
# Eurostat: postcode to NUTS3 (GISCO; postcodes 2025, NUTS 2024), and heating
# and cooling degree days by NUTS3 region (nrg_chddr2_a)
PLZ_NUTS3_FILE <- file.path(PUBLIC_DATA_DIR, "pc2025_DE_NUTS-2024_v1.0.csv")
HDD_FILE <- file.path(PUBLIC_DATA_DIR, "hdd_cdd_nuts3.csv")
# Built once by R/geodata/build_plz_bundesland.R
GEODATA_DIR <- file.path(PUBLIC_DATA_DIR, "geodata")
PLZ_LOOKUP_FILE <- file.path(GEODATA_DIR, "plz_bundesland.csv")
BKG_DIR <- file.path(GEODATA_DIR, "bkg_vg2500")
BUNDESLAND_SHP <- file.path(BKG_DIR, "vg2500", "VG2500_LAN.shp")
KREIS_SHP <- file.path(BKG_DIR, "vg2500", "VG2500_KRS.shp")

# 4. Constants of the survey ---------------------------------------------------
REFERENCE_WAVE <- "161853"   # supplies the question wording for the codebook
N_VIGNETTES <- 6             # vignettes shown per expert
N_ATTRIBUTES <- 9            # design attributes per vignette
SURVEY_YEAR <- 2026          # age is SURVEY_YEAR minus the year of birth
HDD_YEAR <- 2025             # the year of the heating degree days

# The outlier rule of R/cleaning/06_experts_vignettes.R trims each answer at these
# quantiles. Here because the notes of the tables quote them.
TRIM_QUANTILES <- c(0.005, 0.995)
# How unanimous the surrounding postcodes have to be before the first three digits
# stand in for a postcode the lookup does not have. Used for the respondents
# (R/cleaning/03_recode.R) and the contacts (load_frame()), so that one postcode
# lands in one state on both sides of a response rate.
PLZ_PREFIX_MIN_PURITY <- 0.9

# The minimum cell of any output (project profile): every statistic rests on at
# least 10 answers, every response rate on at least 30 contacted persons. Counts
# of how many answers a cleaning step changed may be smaller.
MIN_N <- 10
MIN_CONTACTS <- 30

# 5. Draws and workers ---------------------------------------------------------
# Halton draws per expert in the estimation; prediction draws for probabilities
# and AMEs (5,000 Halton draws are at least as accurate as the 100,000
# pseudo-random draws used before, benchmark of 2026-11-09); Krinsky-Robb draws,
# so that each 2.5% tail rests on 125 draws. Fewer in a synthetic run, which only
# tests the code, and in a quick run. N_WORKERS: the R sessions for the Hessian
# and the Krinsky-Robb loop; lower it if something else runs on this computer.
if (USE_SYNTHETIC_DATA || QUICK_RUN) {
  N_DRAWS_MODEL <- 100
  N_DRAWS_PREDICTION <- 1000
  N_DRAWS_KR <- 200
} else {
  N_DRAWS_MODEL <- 1000
  N_DRAWS_PREDICTION <- 5000
  N_DRAWS_KR <- 5000
}
N_WORKERS <- parallel::detectCores()
if (USE_SYNTHETIC_DATA) {
  N_WORKERS <- 2
}
# The primes of the Halton draws, one per random constant, in mlogit's order. They
# start again at 2 for each group of constants.
HALTON_PRIMES <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)

# 6. Create and check ----------------------------------------------------------
dir.create(REPORT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CLEAN_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
required_inputs <- c(VERSAND_FILE, VIGNETTE_FILE, HDD_FILE)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop("These inputs do not exist: ", paste(missing_inputs, collapse = ", "),
       "\nFix the roots at the top of config.R, or in config.local.R.")
}
message("Data: ", if (USE_SYNTHETIC_DATA) "SYNTHETIC, " else "", CLEAN_DATA_DIR,
        "\nOutput: ", OUTPUT_DIR,
        if (QUICK_RUN) "\nQUICK RUN: few draws, no cross-validation" else "")
