# =============================================================================
# build_report_completers.R  -  The summary report, completed questionnaires only
# -----------------------------------------------------------------------------
# Produces the same summary report as the normal chain, but restricted to the
# experts who reached the end of the questionnaire and submitted it.
#
# WHY A WRAPPER RATHER THAN A COPY
# It sets two switches and then runs the ORIGINAL three scripts. Nothing is
# duplicated, so a change to a plot or to the report layout shows up in both
# versions automatically - a forked copy would drift out of step within a week.
#
# WHAT "COMPLETED" MEANS
# `submitdate` is filled in only when the questionnaire is submitted. Every one
# of the 2,165 submitters ends on lastpage 121 (the final page) and everyone else
# stops at 120 or earlier, so this is exactly "finished the survey", not a proxy.
#
# WHAT IT CHANGES, AND WHAT IT DOES NOT
# Missingness caused by people BREAKING OFF disappears here. Missingness built
# into the questionnaire does not: Q11b is only shown to Sanitaer/Heizung/Klima
# firms, and Q15 / Q10b explicitly tell respondents to leave a field blank when
# it does not apply to their customers. Those stay just as high.
#
# HOW TO RUN (after run_pipeline.R):
#   - In RStudio: open this file and click "Source"; or
#   - In a terminal, from the project folder:  Rscript build_report_completers.R
#
# WRITES (alongside the full-sample versions, never overwriting them):
#   output/summary_completers/            the plots
#   output/variable_summary_completers.csv
#   output/maps_completers/               the maps
#   output/summary_report_completers.html the report
# =============================================================================

# --- 0. Always run from the project folder -----------------------------------
# Locate this script, then walk up to the project root (the folder containing
# the marker file/dir listed in PROJECT_MARKERS). Works under Rscript, source(),
# the RStudio "Source" button, and Ctrl+Enter in the editor.

PROJECT_MARKERS <- c(".Rproj.user", "data")  # adjust: any file/dir unique to the root

find_this_script <- function() {
  # (a) Rscript / R CMD BATCH: read the --file= argument. Rscript encodes
  #     spaces in the path as "~+~", so decode them.
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) >= 1L) {
    return(gsub("~+~", " ", sub("^--file=", "", file_arg[[1L]]), fixed = TRUE))
  }
  # (b) source() / RStudio "Source" button: the file name is recorded in the
  #     call stack. Search inside out so nested source() calls resolve to the
  #     file actually running.
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frames()[[i]]$ofile
    if (!is.null(of) && is.character(of) && nzchar(of)) return(of)
  }
  # (c) open in the RStudio editor and run another way (e.g. Ctrl+Enter).
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getSourceEditorContext()
    if (!is.null(ctx) && nzchar(ctx$path)) return(ctx$path)
  }
  ""  # could not detect
}

find_project_root <- function(start_dir, markers = PROJECT_MARKERS) {
  d <- start_dir
  repeat {
    if (any(file.exists(file.path(d, markers)))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) return(NA_character_)  # reached filesystem root
    d <- parent
  }
}

.script_path <- find_this_script()
if (nzchar(.script_path) && file.exists(.script_path)) {
  .script_dir  <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
  .project_dir <- find_project_root(.script_dir)
  if (is.na(.project_dir)) {
    stop("Found this script at ", .script_dir, ", but no project root above it ",
         "(looked for: ", paste(PROJECT_MARKERS, collapse = ", "), ").")
  }
  setwd(.project_dir)
  message("Working directory: ", getwd())
} else {
  message(
    "NOTE: could not detect this script's location automatically.\n",
    "      Please set your working directory to the project folder before running."
  )
}

# The two switches every one of the three scripts reads. They are plain globals
# because that is what `source()` makes visible to the scripts being run.
COMPLETERS_ONLY <- TRUE
OUTPUT_TAG      <- "_completers"

message("\n=========================================================")
message(" Building the summary report for COMPLETED questionnaires ")
message("=========================================================\n")

message(">>> 1/3  summarize_data.R  (plots and the summary table)")
source("r-scripts/summarize_data.R", encoding = "UTF-8")

message("\n>>> 2/3  map_experts.R  (the two maps)")
source("r-scripts/map_experts.R", encoding = "UTF-8")

message("\n>>> 3/3  build_report.R  (the HTML report)")
source("r-scripts/build_report.R", encoding = "UTF-8")

# Clear the switches again. Without this they would linger in the session, and
# running summarize_data.R by hand afterwards would quietly produce a
# completers-only result under the completers name - with nothing on screen to
# say the sample had changed.
rm(COMPLETERS_ONLY, OUTPUT_TAG)

message("\n=========================================================")
message(" Done. Full-sample outputs are untouched; the restricted   ")
message(" versions carry the '_completers' suffix.                  ")
message("=========================================================")
