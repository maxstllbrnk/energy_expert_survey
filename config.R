# =============================================================================
# config.R  -  the only file you need to edit
# =============================================================================
# The project lives in three places, with three different levels of access:
#
#   1. SECURE   Raw LimeSurvey exports, the matching syntax files, and the
#               cleaned respondent-level data. Personal data, so it cannot be
#               shared freely. Currently a local folder; it moves to an
#               external server later - when it does, only RAW_DIR and
#               CLEAN_DATA_DIR below change.
#
#   2. DROPBOX  Shared with the coauthors. Holds the non-sensitive inputs
#               (public_data_dropbox/) and every output the pipeline produces
#               (output_dropbox/): codebook, diagnostics, and later tables and
#               figures. None of it contains respondent-level data.
#
#   3. PROJECT  This folder - code only, shared via GitHub. The pipeline never
#               writes here, and no raw or cleaned data is ever stored here.
#
# On a new machine, edit the three roots below and nothing else. Coauthors who
# would rather not touch a tracked file can put their own roots in
# config.local.R instead; it is git-ignored and overrides what is set here.
# =============================================================================

library(tidyverse)
library(readxl)
library(writexl)

# =============================================================================
# the three roots  <- EDIT THESE
# =============================================================================
# Use forward slashes. R reads "\" in a string as an escape character, so a
# pasted Windows path ("C:\Users\...") is a syntax error.

# --- 1. SECURE ---------------------------------------------------------------
# INPUT: the twelve raw exports (survey_<id>_R_data_file.csv, soft-launch waves
# included) plus a limesurvey-syntax/ sub-folder with the twelve syntax files.
RAW_DIR <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/260909_raw_energy_expert_survey_data"

# OUTPUT: the cleaned analysis data sets. One row per respondent and one per
# respondent x vignette, so this is respondent-level data and stays out of
# Dropbox. Created automatically if it does not exist.
CLEAN_DATA_DIR <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/260909_cleaned_energy_expert_survey_data"

# --- 2. DROPBOX --------------------------------------------------------------
# Everything the coauthors need. The path differs per machine - this is the one
# line most likely to need changing.
DROPBOX_DIR <- "C:/Users/msk/Dropbox/Heating Transition/analysis_survey"

# --- local override (git-ignored) --------------------------------------------
# config.local.R holds ONLY root assignments - the lines above that differ on
# this machine. Everything below is then derived from whatever it sets.
#
# It must not be a copy of this file: the copy would reach this same line and
# source itself forever, which R reports as "evaluation nested too deeply:
# infinite recursion". Catch that here, where it can be explained.
if (file.exists("config.local.R")) {
  if (any(grepl('source("config.local.R")', readLines("config.local.R"), fixed = TRUE)))
    stop("config.local.R sources itself, so it looks like a copy of config.R.\n",
         "It should contain only the roots that differ on this machine, e.g.\n",
         '  DROPBOX_DIR <- "C:/Users/you/Dropbox/Heating Transition/analysis_survey"\n',
         "Delete it if this machine does not need an override at all.",
         call. = FALSE)
  source("config.local.R")
}

# =============================================================================
# derived paths  -  no need to edit below here
# =============================================================================

# --- secure ------------------------------------------------------------------
SYNTAX_DIR <- file.path(RAW_DIR, "limesurvey-syntax")  # 12 LimeSurvey R syntax exports

# --- Dropbox -----------------------------------------------------------------
PUBLIC_DATA_DIR <- file.path(DROPBOX_DIR, "public_data_dropbox")  # non-sensitive inputs
OUTPUT_DIR      <- file.path(DROPBOX_DIR, "output_dropbox")       # everything we produce

VERSAND_FILE  <- file.path(PUBLIC_DATA_DIR, "LimeSurvey_Versanduebersicht_010726.xlsx")  # batch overview
VIGNETTE_FILE <- file.path(PUBLIC_DATA_DIR, "vignetten_kombinationen.xlsx")              # vignette design matrix

# --- geodata (Dropbox, downloaded once) --------------------------------------
# Built by R/geodata/build_plz_bundesland.R, which downloads the two open data
# sets once and leaves them here. Nothing in the pipeline ever downloads: the
# cleaning run only reads PLZ_LOOKUP_FILE, a plain CSV.
#
# These paths are deliberately NOT in the required list below. Before the build
# script has been run they simply do not exist, and 03_recode.R then sets
# `bundesland` to NA with a message rather than failing the whole run.
GEODATA_DIR     <- file.path(PUBLIC_DATA_DIR, "geodata")
PLZ_LOOKUP_FILE <- file.path(GEODATA_DIR, "plz_bundesland.csv")   # postcode -> federal state
BKG_DIR         <- file.path(GEODATA_DIR, "bkg_vg2500")           # official boundaries, for maps
BUNDESLAND_SHP  <- file.path(BKG_DIR, "vg2500", "VG2500_LAN.shp") # the 16 state polygons
GEONAMES_FILE   <- file.path(GEODATA_DIR, "geonames", "DE.txt")    # postcode coordinates, for maps

# Codebook and build diagnostics, written by R/run_cleaning.R.
REPORT_DIR    <- file.path(OUTPUT_DIR, "reports_cleaning")
CODEBOOK_FILE <- file.path(REPORT_DIR, "codebook.csv")

# Figures and tables describing the data, written by R/run_summary_statistics.R.
# Aggregates only - no respondent-level data - so they can live in Dropbox.
SUMMARY_DIR <- file.path(OUTPUT_DIR, "summary_statistics")

# The report and figures of the first policy brief, written by the scripts in
# R/first_policy_brief/. The figures are copied from SUMMARY_DIR.
FIRST_POLICY_BRIEF_DIR <- file.path(OUTPUT_DIR, "first_policy_brief")

# =============================================================================
# survey design constants
# =============================================================================

# --- reference wave ----------------------------------------------------------
# All ten main waves are structurally identical; this one supplies the question
# wording and answer options used for the codebook and for value labels.
REFERENCE_WAVE <- "161853"

N_VIGNETTES  <- 6   # vignettes shown per respondent
N_ATTRIBUTES <- 9   # design attributes per vignette (attribute_1..54)

# =============================================================================
# check the paths before anything tries to use them
# =============================================================================
dir.create(CLEAN_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(REPORT_DIR,     showWarnings = FALSE, recursive = TRUE)

.required <- c(
  "RAW_DIR (secure)"        = RAW_DIR,
  "SYNTAX_DIR (secure)"     = SYNTAX_DIR,
  "CLEAN_DATA_DIR (secure)" = CLEAN_DATA_DIR,
  "VERSAND_FILE (Dropbox)"  = VERSAND_FILE,
  "VIGNETTE_FILE (Dropbox)" = VIGNETTE_FILE,
  "REPORT_DIR (Dropbox)"    = REPORT_DIR
)
.missing <- .required[!file.exists(.required)]
if (length(.missing))
  stop("These paths from config.R do not exist:\n",
       str_c("  ", names(.missing), ": ", .missing, collapse = "\n"),
       "\n\nFix the roots at the top of config.R (or in config.local.R).",
       call. = FALSE)
rm(.required, .missing)

message("secure : ", RAW_DIR, "\n",
        "         ", CLEAN_DATA_DIR, "\n",
        "dropbox: ", DROPBOX_DIR)
