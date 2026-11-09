################################################################################
# run_summary_statistics.R — the summary statistics of the full sample: all tables,
#                            figures and maps, and the deck that shows them
# Inputs : CLEAN_DATA_DIR/experts.rds, vignettes.rds; FRAME_FILE (all secure)
# Outputs: OUTPUT_DIR/full_sample/summary_statistics/: tables/, figures/, maps/,
#          exhibits.csv, summary_statistics.pdf and .tex
# Checks : in the sections
################################################################################
# Run R/run_cleaning.R first. In RStudio: open energy_expert_survey.Rproj, open this
# file, click "Source". In a terminal, from the project folder:
#   Rscript R/run_summary_statistics.R

source(file.path("R", "setup.R"))

experts <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds"))
vignettes <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds"))
frame <- load_frame()

run_summary_statistics(
  experts, vignettes,
  file.path(OUTPUT_DIR, "full_sample", "summary_statistics"),
  "everyone who started the questionnaire",
  frame = frame
)
