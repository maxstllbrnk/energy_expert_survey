################################################################################
# run_descriptives.R — English tables and figures describing the survey, for
#                      talks: who was invited and who answered, who the experts
#                      are, the topics of the policy brief, the vignettes and
#                      the response times
# Inputs : the cleaned data (CLEAN_DATA_DIR) and the sampling frame
#          (FRAME_FILE), both secure; the codebook, geodata and degree days;
#          the tests of the price expectations (first_policy_brief/) and the
#          saved results of a model with prices (rc_logit_heterogeneity/)
# Outputs: in DESCRIPTIVES_DIR (Dropbox), aggregates only:
#            tables/   tab_desc_*.tex, complete LaTeX table floats, and
#                      numbers_desc.tex, numbers and figure notes for the text
#            figures/  fig_desc_*.pdf
#            excel/    descriptives.xlsx, every number unrounded
# Checks : see the scripts
################################################################################
# In RStudio: open energy_expert_survey.Rproj, open this file, click "Source".
# In a terminal, from the project folder:   Rscript R/run_descriptives.R
#
# Run R/run_cleaning.R and R/first_policy_brief/02_kl_divergence_prices.R
# first. Everything is sized for the 16:9 slides of the economics-latex
# layout and in English. The tables need booktabs and threeparttable.
#
#   02_data.R             the sample, the price rules, the outlier rule
#   03_recruitment.R      response rate by state, invited and responses by
#                         group, from the invitation to the estimation sample
#   04_experts.R          summary statistics of the experts, and the
#                         characteristics that enter the models; the cleaning
#   05_policy_brief.R     price expectations (with the tests), barriers,
#                         what matters to the customers
#   06_vignettes.R        the design, the recommendations by arm and group,
#                         who is in each arm
#   07_response_times.R   the questionnaire, its sections and the vignettes
#   08_excel_numbers.R    Excel, and the numbers for the slides

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

# load_analysis_data(), question_times(), the price rules (also loads
# config.R)
source(file.path("R", "prepare_analysis_data.R"))
# VALID_RANGE, TRIM_QUANTILES, MIN_N; outliers_to_na(), ranking_weights(),
# folder_name()
source(file.path("R", "summary_statistics", "00_settings.R"))
source(file.path("R", "summary_statistics", "01_plot_functions.R"))
# table_float(), latex_number(), latex_text(), theme_rcl(), save_figure_pdf()
source(file.path("R", "analysis", "00_functions_output.R"))
# MIN_KONTAKTE; load_frame(), load_respondents(), ruecklauf_by()
source(file.path("R", "ruecklauf", "00_settings.R"))
source(file.path("R", "ruecklauf", "01_frame.R"))
source(file.path("R", "ruecklauf", "02_link.R"))
library(sf)
source(file.path("R", "descriptives", "00_settings.R"))
source(file.path("R", "descriptives", "01_functions.R"))

# Output of an earlier run is removed first, so that a file which is no longer
# produced cannot sit among the current ones. Only these three folders, so
# that slides next to them stay.
tables_dir <- file.path(DESCRIPTIVES_DIR, "tables")
figures_dir <- file.path(DESCRIPTIVES_DIR, "figures")
excel_dir <- file.path(DESCRIPTIVES_DIR, "excel")
for (folder in c(tables_dir, figures_dir, excel_dir)) {
  # unlink(): deletes the folder and everything in it
  unlink(folder, recursive = TRUE)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# Filled by the scripts: the sheets of the workbook, and the numbers and notes
# of the slides (08_excel_numbers.R)
excel_sheets <- list()
numbers <- character(0)

source(file.path("R", "descriptives", "02_data.R"))
source(file.path("R", "descriptives", "03_recruitment.R"))
source(file.path("R", "descriptives", "04_experts.R"))
source(file.path("R", "descriptives", "05_policy_brief.R"))
source(file.path("R", "descriptives", "06_vignettes.R"))
source(file.path("R", "descriptives", "07_response_times.R"))
source(file.path("R", "descriptives", "08_excel_numbers.R"))

message("\nDescriptives -> ", normalizePath(DESCRIPTIVES_DIR))
