# =============================================================================
# run_summary_statistics.R  -  figures and tables describing the survey data
# =============================================================================
# In RStudio: open energy_expert_survey.Rproj, open this file, click "Source".
# In a terminal, from the project folder:   Rscript R/run_summary_statistics.R
#
# Run R/run_cleaning.R first. This reads the cleaned data from CLEAN_DATA_DIR
# (secure) and the codebook from REPORT_DIR, and writes only figures and
# aggregate tables to SUMMARY_DIR in Dropbox - nothing respondent-level.
# What is plotted, the outlier rule, labels and colours are all set in
# R/summary_statistics/00_settings.R.
#
# Every figure is produced for each sample x group:
#
#   SUMMARY_DIR/
#     alle_befragten/                 every expert, including break-offs
#       tabellen.xlsx                 the numbers behind the figures, all groups
#       alle_berufsgruppen/
#         01_stichprobe/              Berufsgruppe, completion, drop-out
#         02_bearbeitungszeit/        time for the questionnaire and its sections
#         03_karten/                  maps by federal state and by postcode
#         04_unternehmen/             Q0a-Q6a
#         05_vignetten/               recommendations, response times, costs
#         06_energiepreise/           Q15-Q18b
#         07_heiztechnologien/        Q11b-Q11c (SHK firms only)
#         08_praeferenzen/            time and risk preferences
#         09_dienstleistungen_markt/  Q8-Q10b, Q19a-c, Q7, Q28
#         10_person/                  Q20-Q24
#       shk_handwerk/                 the same, per Berufsgruppe
#       schornsteinfeger/
#       energieberater/
#     completer/                      the same for submitted questionnaires only
#                                     (without the drop-out figures)
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source("R/prepare_analysis_data.R")     # also loads config.R and R/add_preferences.R
source("R/summary_statistics/00_settings.R")
source("R/summary_statistics/01_plot_functions.R")
library(sf)

# --- inputs that are the same for every subset ------------------------------------
# The official state polygons (GF 9 = land area, one row per state) and one
# coordinate per postcode - the median over the places sharing it.
map_states <- st_read(BUNDESLAND_SHP, quiet = TRUE) %>%
  filter(GF == 9)

map_postcodes <- read_tsv(GEONAMES_FILE, col_names = FALSE, quote = "",
                          col_select = c(plz = 2, lat = 10, lon = 11),
                          col_types = cols(.default = col_character()), progress = FALSE) %>%
  mutate(across(c(lat, lon), as.numeric)) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  summarise(lat = median(lat), lon = median(lon), .by = plz)

# --- one pass per sample x group ------------------------------------------------------
for (s in seq_len(nrow(SAMPLES))) {
  sample_spec <- SAMPLES[s, ]
  sample_dir  <- file.path(SUMMARY_DIR, sample_spec$folder)

  # Output of an earlier run is removed first, so that a figure which is no
  # longer produced cannot sit among the current ones looking current.
  unlink(sample_dir, recursive = TRUE)
  dir.create(sample_dir, recursive = TRUE)

  dat <- load_analysis_data(completers_only = sample_spec$completers_only)

  for (group_label in GROUPS) {
    sm <- dat$main
    vl <- dat$vignettes
    if (group_label != ALL_GROUP) {
      sm <- sm %>% filter(berufsgruppe == group_label)
      vl <- vl %>% filter(berufsgruppe == group_label)
    }
    out_dir <- file.path(sample_dir, folder_name(group_label))
    message(sample_spec$label, " / ", group_label, ": ", nrow(sm), " experts")

    source("R/summary_statistics/02_sample.R")
    source("R/summary_statistics/03_questions.R")
    source("R/summary_statistics/04_vignettes.R")
    source("R/summary_statistics/05_preferences.R")
    source("R/summary_statistics/06_maps.R")
  }

  source("R/summary_statistics/07_tables.R")
}

message("\nSummary statistics -> ", normalizePath(SUMMARY_DIR))
