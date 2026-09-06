# =============================================================================
# setup_packages.R  -  ONE-TIME package installation
# -----------------------------------------------------------------------------
# Installs every package required and installs it in the project's OWN package
# folder (r-packages/, defined in library_path.R). You only need to run this
# once per computer (and again if you move the project to a new machine, or want
# to add a package). After that, all scripts load the packages from that
# folder directly and keeps working even if system packages are updated.
#
# If you add or change a script that needs an additional package, please make sure
# to update this file.
#
# HOW TO RUN (either way works, on Windows or macOS):
#   - In RStudio: open the project (Clean_survey_data_pipeline.Rproj), open this
#     file, and click "Source"; or
#   - In a terminal, from the project folder:  Rscript r-scripts/setup_packages.R
# =============================================================================

# --- Always run from the project folder --------------------------------------
# This script lives in the "pipeline" sub-folder, so the project root is one
# level up. Find this script's own location and switch into the project root, so
# it works whether launched from RStudio or a terminal, on Windows or macOS.
find_this_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)                 # (a) Rscript --file=
  file_arg <- grep("^--file=", args, value = TRUE)          #     (spaces encoded as ~+~)
  if (length(file_arg) == 1L) {
    return(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))
  }
  for (i in seq_len(sys.nframe())) {                        # (b) source()/RStudio
    of <- sys.frames()[[i]]$ofile
    if (!is.null(of)) return(of)
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path        # (c) RStudio editor
    if (nzchar(p)) return(p)
  }
  ""
}
.script_path <- find_this_script()
if (nzchar(.script_path)) {
  setwd(dirname(dirname(normalizePath(.script_path))))      # up from pipeline/ to root
} else {
  message(
    "NOTE: could not detect this script's location automatically.\n",
    "      Please set your working directory to the project folder before running."
  )
}

# Point R's library path at the project folder (defines PKG_LIB).
source("r-scripts/library_path.R", encoding = "UTF-8")

# --- Start from a clean, machine-specific library ----------------------------
# Installed packages are specific to the operating system and R version. If this
# project folder was copied from another computer (e.g. a coauthor's Mac), the
# r-packages/ folder it brought along will NOT work here. Empty it first so the
# install below produces packages built for THIS computer.
existing <- list.files(PKG_LIB, all.files = TRUE, no.. = TRUE)
if (length(existing) > 0L) {
  message("Clearing ", length(existing),
          " existing item(s) from '", PKG_LIB, "' for a clean install ...")
  unlink(file.path(PKG_LIB, existing), recursive = TRUE, force = TRUE)
}

# --- Packages the pipeline needs ---------------------------------------------
required_pkgs <- c("dplyr", "tidyr", "stringr", "readxl", "sf")

# --- Where to download the packages from -------------------------------------
# By default we use the main CRAN mirror. For MAXIMUM reproducibility you can
# instead pin to a fixed daily snapshot of CRAN, so re-installing always gives
# the exact same package versions. To do that, replace the line below with a
# dated Posit Public Package Manager URL, e.g.:
#   repos <- "https://packagemanager.posit.co/cran/2026-07-01"
repos <- "https://cloud.r-project.org"

# --- Install ------------------------------------------------------------------
# `lib = PKG_LIB` forces the packages (and their dependencies) into the project
# folder rather than the system library.
message("Installing packages into: ", normalizePath(PKG_LIB))
message("This can take a few minutes the first time ...\n")

install.packages(required_pkgs, lib = PKG_LIB, repos = repos)

# --- Confirm ------------------------------------------------------------------
installed_here <- rownames(installed.packages(lib.loc = PKG_LIB))
still_missing  <- setdiff(required_pkgs, installed_here)

if (length(still_missing) > 0L) {
  stop(
    "These packages did not install correctly: ",
    paste(still_missing, collapse = ", "),
    ". Check the messages above."
  )
}

# Record which computer / R version these packages were built for, so the
# pipeline can confirm they are usable here (and warn if the folder is later
# copied to a different machine). See PKG_BUILD_STAMP in library_path.R.
writeLines(PKG_BUILD_STAMP, PKG_BUILD_FILE)

message("\nAll packages installed successfully into '", PKG_LIB, "'.")
message("You can now run:  run_pipeline.R")
