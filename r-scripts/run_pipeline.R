# =============================================================================
# run_pipeline.R  -  MAIN SCRIPT (this is the only file you need to run)
# -----------------------------------------------------------------------------
# Produces one clean, combined data set from all raw survey batches.
#
# For every raw CSV in raw-survey-data/ it:
#   1. labels the raw export            (label_raw_csv,      01_label_raw_csv.R)
#   2. reshapes it to per-vignette rows (clean_one_survey,   02_clean_one_survey.R)
#   3. adds the batch id + target group + unique id
#                                       (add_batch_metadata, 03_batch_metadata.R)
# and finally stacks all batches together and writes the result to output/.
#
# HOW TO RUN (either way works, on Windows or macOS):
#   - In RStudio: open the project (Clean_survey_data_pipeline.Rproj), open this
#     file, and click "Source"; or
#   - In a terminal, from the project folder:  Rscript run_pipeline.R
# The FIRST time on a new computer, run pipeline/setup_packages.R once first.
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

# --- 1. Load configuration and the pipeline functions ------------------------
# config.R must be sourced first: it loads the packages and defines the paths
# and helpers that the other scripts rely on.
# encoding = "UTF-8" is important on Windows: these files contain German text
# (e.g. "Wärmepumpe", "Ölheizung", "€") that must be read correctly, otherwise
# the factor labels would not match the data.
source("r-scripts/pipeline/config.R",            encoding = "UTF-8")
source("r-scripts/pipeline/01_label_raw_csv.R",  encoding = "UTF-8")
source("r-scripts/pipeline/02_clean_one_survey.R", encoding = "UTF-8")
source("r-scripts/pipeline/03_batch_metadata.R", encoding = "UTF-8")

# --- 2. Read the batch overview (id -> target group) -------------------------
versand <- load_versand()
message("Loaded batch overview: ", nrow(versand), " batch(es) known.")

# Load and extract column names from the survey batch 196938
df.196938  <- label_raw_csv(file.path(RAW_DIR, 
                                        "survey_196938_R_data_file.csv"))
column_names.196938 <- colnames(df.196938)
rm(df.196938)

# --- 3. Find all raw survey files --------------------------------------------
# Files are named survey_<Umfrage-ID>_R_data_file.csv.
raw_files <- list.files(
  path       = RAW_DIR,
  pattern    = "^survey_\\d+_R_data_file\\.csv$",
  full.names = TRUE
)

if (length(raw_files) == 0L) {
  stop("No raw survey files found in '", RAW_DIR, "'.")
}
message("Found ", length(raw_files), " raw file(s) to process.\n")

# --- 4. Process each file ----------------------------------------------------
# We collect the cleaned batches in a list and remember how many we skipped.
cleaned_batches <- list()
n_skipped       <- 0L

for (file_path in raw_files) {

  file_name <- basename(file_path)

  # Pull the batch id out of the file name (the digits in survey_<id>_...).
  survey_id <- as.numeric(
    str_match(file_name, "^survey_(\\d+)_R_data_file\\.csv$")[, 2]
  )

  # If this batch is not listed in the overview we cannot attach a target group,
  # so we warn and skip it (the rest of the files are still processed).
  if (!(survey_id %in% versand$survey_id)) {
    warning(
      "Skipping '", file_name, "': survey_id ", survey_id,
      " is not in the batch overview.",
      call. = FALSE, immediate. = TRUE
    )
    n_skipped <- n_skipped + 1L
    next
  }

  message("Processing '", file_name, "' (batch ", survey_id, ") ...")

  # The three cleaning steps for one file.
  labelled  <- label_raw_csv(file_path)
  colnames(labelled) <- column_names.196938
  long_data <- clean_one_survey(labelled)
  stamped   <- add_batch_metadata(long_data, survey_id, versand)

  cleaned_batches[[file_name]] <- stamped
}

if (length(cleaned_batches) == 0L) {
  stop("No files could be processed (all were skipped).")
}

# --- 5. Combine all batches into one data set --------------------------------
combined <- bind_rows(cleaned_batches)

# --- 6. Write the output -----------------------------------------------------
# .rds keeps R types/factor labels exactly; .csv is portable (opens in Excel).
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}
rds_path <- file.path(OUTPUT_DIR, "cleaned_survey_data.rds")
csv_path <- file.path(OUTPUT_DIR, "cleaned_survey_data.csv")

saveRDS(combined, rds_path)
write.csv(combined, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

# --- 7. Summary --------------------------------------------------------------
message("\n==================== Pipeline finished ====================")
message("Batches processed : ", length(cleaned_batches))
message("Files skipped     : ", n_skipped)
message("Total rows        : ", nrow(combined))
message("Respondents       : ", dplyr::n_distinct(combined$unique_id))
message("Output written to : ", rds_path)
message("                    ", csv_path)
message("===========================================================")
