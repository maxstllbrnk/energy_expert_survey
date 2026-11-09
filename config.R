################################################################################
# config.R — the roots, every path derived from them, and the constants of the
#            survey. The only file to edit on a new machine.
# Inputs : config.local.R, if it exists (per-machine roots, git-ignored)
# Outputs: creates the output folder if it is missing
# Checks : the inputs exist
################################################################################
# The project lives in three places:
#   SECURE   raw exports, cleaned data, sampling frame and all output. Personal
#            data, and the fitted models contain them. Claude Code has no access.
#   DROPBOX  non-sensitive inputs only (public_data_dropbox/): design matrix,
#            batch overview, geodata, degree days.
#   PROJECT  this folder: code only, shared through GitHub. Nothing is written
#            here.

# 1. The roots -----------------------------------------------------------------
# Forward slashes: R reads "\" in a string as an escape character.
RAW_DIR <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/energy_experts_survey/wave_1_2026/260909_raw_energy_expert_survey_data"
CLEAN_DATA_DIR <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/energy_experts_survey/wave_1_2026/260909_cleaned_energy_expert_survey_data"
DROPBOX_DIR <- "C:/Users/msk/Dropbox/Heating Transition/analysis_survey"

# config.local.R holds only the assignments above that differ on this machine
if (file.exists("config.local.R")) {
  source("config.local.R")
}

# 2. Secure paths --------------------------------------------------------------
# A session of Claude Code (CLAUDECODE=1) may not touch the real data (project
# profile). It reads and writes synthetic data in a temporary folder instead, set
# by EES_SYNTHETIC_DIR, which tests/simulate_data.R fills. A run on the real data
# never takes this branch.
USE_SYNTHETIC_DATA <- Sys.getenv("CLAUDECODE") == "1"
if (USE_SYNTHETIC_DATA) {
  SECURE_DIR <- Sys.getenv("EES_SYNTHETIC_DIR",
                           unset = file.path(Sys.getenv("TEMP"), "ees", "synthetic"))
  RAW_DIR <- file.path(SECURE_DIR, "raw")
  CLEAN_DATA_DIR <- file.path(SECURE_DIR, "clean")
  FRAME_FILE <- file.path(SECURE_DIR, "frame", "frame.rds")
} else {
  SECURE_DIR <- dirname(CLEAN_DATA_DIR)
  # The sampling frame: every expert and firm that was contacted, with e-mail,
  # postcode and the list they were drawn from. The denominator of the response
  # rates.
  FRAME_FILE <- file.path(dirname(RAW_DIR), "260915_scraped_data",
                          "260513_allexperts_unique_PLZ.rds")
}
SYNTAX_DIR <- file.path(RAW_DIR, "limesurvey-syntax")

# All output, one folder per estimation script (R/estimation/), plus the cleaning
# and the summary statistics of the full sample
OUTPUT_DIR <- file.path(SECURE_DIR, "output")
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
# tests the code.
if (USE_SYNTHETIC_DATA) {
  N_DRAWS_MODEL <- 100
  N_DRAWS_PREDICTION <- 1000
  N_DRAWS_KR <- 200
} else {
  N_DRAWS_MODEL <- 1000
  N_DRAWS_PREDICTION <- 5000
  N_DRAWS_KR <- 5000
}
# R sessions for the Hessian and the Krinsky-Robb loop
N_WORKERS <- parallel::detectCores()

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
        "\nOutput: ", OUTPUT_DIR)
