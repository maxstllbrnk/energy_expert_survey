# =============================================================================
# run_ruecklauf.R  -  how many of the contacted experts answered?
# =============================================================================
# In RStudio: open energy_expert_survey.Rproj, open this file, click "Source".
# In a terminal, from the project folder:   Rscript R/run_ruecklauf.R
#
# Run R/run_cleaning.R first. This reads the cleaned data from CLEAN_DATA_DIR
# and the sampling frame from FRAME_FILE (both secure) and writes only figures
# and aggregate tables to OUTPUT_DIR/ruecklauf in Dropbox - nothing
# respondent-level, and no e-mail address ever leaves the secure location.
#
# The response rate is  answered / contacted. The numerator is survey_main, the
# denominator is the scraped list of every expert and firm that was written to.
# Both are counted as PEOPLE, not as rows: a person who started the
# questionnaire three times counts once, and an address that appears twice in
# the frame counts once.
#
# Every figure is produced for each combination of
#
#   t-online variant      all contacts  /  without t-online addresses
#   response definition   all answers   /  only submitted questionnaires
#
#   OUTPUT_DIR/ruecklauf/
#     bericht.html, bericht.pdf         all figures on one page
#     tabellen_ruecklauf.xlsx           the numbers, all four combinations
#     mit_tonline/ , ohne_tonline/
#       alle_antworten/ , vollstaendige_antworten/
#         01_ueberblick/                funnel, and the t-online comparison
#         02_karten/                    rate per federal state, and break-offs
#         03_berufsgruppen/             rate per sample the lists were drawn from
#
# What is plotted, the t-online rule and the minimum cell size are set in
# R/ruecklauf/00_settings.R.
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source("R/prepare_analysis_data.R")              # also loads config.R
source("R/summary_statistics/00_settings.R")     # colours, figure size, MIN_N
source("R/summary_statistics/01_plot_functions.R")  # theme, save_figure(), fmt_n()
source("R/ruecklauf/00_settings.R")
source("R/ruecklauf/01_frame.R")
source("R/ruecklauf/02_link.R")
library(sf)

# --- inputs that are the same for every subset -------------------------------
map_states <- st_read(BUNDESLAND_SHP, quiet = TRUE) %>%
  filter(GF == 9)

frame    <- load_frame()
resp_all <- load_respondents() %>% apply_token_bridge(frame)

NUMERATOR_SOURCE <- attr(resp_all, "numerator_source")

# The dispatch log's own total, as a cross-check on the frame: the two count
# different things - invitations sent vs. people on the list - and a large gap
# between them would mean the frame is not the list that was mailed.
versand_gesamt <- load_versand_total()

message("Frame ", fmt_n(nrow(frame)), " contacted · Versandübersicht ",
        fmt_n(versand_gesamt), " invitations sent · ",
        fmt_n(nrow(resp_all)), " respondents (",
        percent_rate(nrow(resp_all) / nrow(frame)), " response rate)")

# Output of an earlier run is removed first, so a figure that is no longer
# produced cannot sit among the current ones looking current.
unlink(RUECKLAUF_DIR, recursive = TRUE)
dir.create(RUECKLAUF_DIR, recursive = TRUE, showWarnings = FALSE)

# --- one pass per t-online variant x response definition ---------------------
for (v in seq_len(nrow(TONLINE_VARIANTS))) {
  rl_variant <- TONLINE_VARIANTS[v, ]
  frame_v    <- if (rl_variant$drop_tonline) frame %>% filter(!is_tonline) else frame

  for (s in seq_len(nrow(RUECKLAUF_SAMPLES))) {
    rl_sample <- RUECKLAUF_SAMPLES[s, ]
    resp_v    <- if (rl_sample$completers_only) resp_all %>% filter(is_complete) else resp_all

    out_dir <- file.path(RUECKLAUF_DIR, rl_variant$folder, rl_sample$folder)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    message(rl_variant$label, " / ", rl_sample$label, ": ",
            fmt_n(nrow(resp_v)), " of ", fmt_n(nrow(frame_v)), " = ",
            percent_rate(nrow(resp_v) / nrow(frame_v)))

    source("R/ruecklauf/05_ueberblick.R")
    source("R/ruecklauf/03_karten.R")
    source("R/ruecklauf/04_berufsgruppen.R")
  }
}

# --- tables and the report, once over everything -----------------------------
source("R/ruecklauf/06_tabellen.R")
source("R/ruecklauf/07_report.R")

message("\nRücklaufquote -> ", normalizePath(RUECKLAUF_DIR))
