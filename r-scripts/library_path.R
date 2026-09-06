# =============================================================================
# library_path.R  -  project-local package library
# -----------------------------------------------------------------------------
# The scripts use their OWN package folder instead of R's system-wide library.
# This makes it robust: once the packages are installed here, updating (or
# breaking) packages elsewhere on the computer can no longer affect the pipeline
# - it always loads the exact copies stored in this project.
#
# This tiny script does two things and NOTHING else (it never loads a package,
# so it is safe to run before anything is installed):
#   1. defines PKG_LIB, the folder that holds the pipeline's packages, and
#   2. puts that folder FIRST on R's search path, so `library(...)` loads from
#      it before looking at the system library.
#
# It is sourced first by both setup_packages.R (which installs into PKG_LIB) and
# config.R (which loads from PKG_LIB).
# =============================================================================

# The package folder lives inside the project (relative to the project root, so
# run scripts from the project root). Change this line if you want it elsewhere.
PKG_LIB <- "r-packages"

# Create the folder the first time it is needed.
if (!dir.exists(PKG_LIB)) {
  dir.create(PKG_LIB, recursive = TRUE)
}

# Load from (and install into) this folder before the system library.
.libPaths(c(normalizePath(PKG_LIB), .libPaths()))

# A short fingerprint of THIS computer's R build (operating system + R version).
# Installed packages only work under a matching build, so setup_packages.R writes
# this fingerprint into r-packages/ and config.R checks it before loading. This
# is what turns "someone copied a Mac r-packages/ onto Windows" from a crash into
# a clear "please run setup" message.
PKG_BUILD_STAMP <- paste(
  R.version$platform,
  paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = "."),
  sep = " | "
)
PKG_BUILD_FILE <- file.path(PKG_LIB, ".build_info")
