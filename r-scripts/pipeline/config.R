# =============================================================================
# config.R
# -----------------------------------------------------------------------------
# Central configuration for the survey-cleaning pipeline.
#
# I have updated the location of the raw survey data as well as the location of 
# where the cleaned data should be saved to the folder 
# "I:\Projekte\Stiftungs- und Industrieprojekte\SFB UniMA\Daten\Umfrage Energieexperten"
# If the data location is different for you, you need to update the RAW_DIR,
# OUTPUT_DIR variables.
#
#
# Loaded first by run_pipeline.R. It does three things:
#   1. Loads the R packages every step needs.
#   2. Defines the file/folder paths used across the pipeline (relative to the
#      project root - so run the pipeline from the project root).
#   3. Defines a small shared helper (get_single_column) used by the cleaning
#      step.
#
# Nothing here reads data or changes files; it only sets things up.
# =============================================================================

# --- Packages ----------------------------------------------------------------
# The pipeline loads its packages from its OWN folder (r-packages/) rather than
# the system library, so system-wide package updates cannot break it. This line
# points R's search path at that folder (see pipeline/library_path.R).
source("r-scripts/library_path.R", encoding = "UTF-8")

# dplyr / tidyr / stringr do the data wrangling; readxl reads the Excel files.
# (The original Test.R used readxl but never loaded it - fixed here.)
required_pkgs <- c("dplyr", "tidyr", "stringr", "readxl")

# Friendly check before loading anything: the packages must be present in the
# project folder AND have been built for THIS computer (see PKG_BUILD_STAMP in
# library_path.R). If the folder is empty, or was copied from another computer
# (e.g. a coauthor's Mac onto a Windows PC), we stop with a clear instruction
# instead of failing cryptically or crashing when a package is loaded.
setup_hint <- paste0(
  "  Please run this once on THIS computer first:  r-scripts/setup_packages.R"
)

installed_here <- rownames(installed.packages(lib.loc = PKG_LIB))
missing_pkgs   <- setdiff(required_pkgs, installed_here)
built_here     <- file.exists(PKG_BUILD_FILE) &&
  identical(readLines(PKG_BUILD_FILE, warn = FALSE)[1], PKG_BUILD_STAMP)

if (length(missing_pkgs) > 0L) {
  stop(
    "Required packages are not installed in '", PKG_LIB, "': ",
    paste(missing_pkgs, collapse = ", "), ".\n", setup_hint,
    call. = FALSE
  )
}
if (!built_here) {
  stop(
    "The packages in '", PKG_LIB, "' were installed on a different computer or ",
    "R version and will not work here.\n", setup_hint,
    call. = FALSE
  )
}

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# --- Paths -------------------------------------------------------------------
# All paths are relative to the project root (the folder that contains this
# "pipeline" directory, raw-survey-data/, and the Excel files).
RAW_DIR      <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/raw-survey-data"                         # folder with the raw CSV exports
VERSAND_FILE <- "data/LimeSurvey_Versanduebersicht_010726.xlsx" # batch overview (id -> target group)
VIGNETTE_FILE <- "data/vignetten_kombinationen.xlsx"          # lookup table of vignette combinations
OUTPUT_DIR   <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/cleaned-survey-data"                                  # where the cleaned data set is written

# --- Shared helper -----------------------------------------------------------
# Finds exactly one column of `df` whose name matches `pattern`. Used by the
# cleaning step to locate LimeSurvey timing columns (whose names are not renamed
# during labelling). It deliberately stops with an informative error if zero or
# more than one column matches, so a change in the export structure is caught
# early rather than producing silently wrong results.
# (`df` is passed explicitly so the helper works from inside clean_one_survey.)
get_single_column <- function(df, pattern, description) {
  result <- names(df)[str_detect(names(df), pattern)]

  if (length(result) != 1L) {
    stop(
      description,
      ": expected exactly one matching column, but found ",
      length(result),
      "."
    )
  }

  result
}
