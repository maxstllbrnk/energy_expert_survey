# =============================================================================
# map_experts.R  -  Where are the surveyed experts?
# -----------------------------------------------------------------------------
# Maps how many experts responded from each postcode area, using the postcode of
# their company's registered office (Q0).
#
# SELF-CONTAINED POSTCODE STEP: this script no longer requires
# pipeline/06_postcode.R. If the cleaned data already carries `plz` and
# `plz_valid`, those columns are used unchanged; otherwise section 4 derives
# them from the raw Q0 answer.
#
# It produces two maps, because they answer two different questions:
#   experts_by_plz_points.png    one dot per postcode, area proportional to the
#                                number of experts - shows the actual geographic
#                                spread and the individual clusters
#   experts_by_bundesland.png    a choropleth by federal state - shows regional
#                                coverage at a glance, which the dot map cannot
#                                because single dots are hard to compare
#
# WHY NOT A POSTCODE CHOROPLETH: shading the 5-digit postcode polygons
# themselves would be the obvious third option, but the boundary files are not
# reachable from this network, and with ~2,300 distinct postcodes for ~3,300
# experts most areas would hold one or two people - a near-empty map. Dots sized
# by count carry the same information and stay readable.
#
# HOW TO RUN (after run_pipeline.R):
#   - In RStudio: open this file and click "Source"; or
#   - In a terminal, from the project folder:  Rscript map_experts.R
#
# The first run downloads two small open geodata files into data/geodata/ (see
# GEODATA below) and reuses them afterwards, so it needs an internet connection
# once. Requires the `sf` package (pipeline/setup_packages.R installs it).
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

source("r-scripts/pipeline/config.R", encoding = "UTF-8")
library(sf)
sf_use_s2(FALSE)   # planar geometry is fine at this scale and avoids s2 edge errors

# --- 1. Settings ---------------------------------------------------------------
# Optional sample restriction, set by a wrapper before sourcing this file (see
# build_report_completers.R). Run on its own, this script behaves as before.
if (!exists("COMPLETERS_ONLY")) COMPLETERS_ONLY <- FALSE
if (!exists("OUTPUT_TAG"))      OUTPUT_TAG      <- ""

# Column in the cleaned data holding the raw postcode answer (Q0). Leave NULL to
# detect it automatically; set it to the exact column name if detection fails or
# picks the wrong one. Ignored when the data already has `plz`/`plz_valid`.
if (!exists("PLZ_SOURCE_COL")) PLZ_SOURCE_COL <- NULL

GEODATA_DIR <- "data/geodata"
MAP_DIR     <- file.path("output", paste0("maps", OUTPUT_TAG))

# Open data, both fetched once and then cached in data/geodata/:
#   - federal state boundaries (simplified) from deutschlandGeoJSON (CC0)
#   - postcode -> coordinates from the zauberware postal-code collection
#     (derived from GeoNames, CC-BY)
GEODATA <- list(
  bundeslaender = list(
    file = file.path(GEODATA_DIR, "bundeslaender.geojson"),
    url  = paste0("https://raw.githubusercontent.com/isellsoap/deutschlandGeoJSON/",
                  "main/2_bundeslaender/4_niedrig.geo.json")
  ),
  postcodes = list(
    file = file.path(GEODATA_DIR, "DE.zip"),
    url  = paste0("https://raw.githubusercontent.com/zauberware/",
                  "postal-codes-json-xml-csv/master/data/DE.zip")
  )
)

# Sequential single-hue ramp (light = few experts, dark = many). A sequential
# scale must be one hue getting darker, never a rainbow: the reader has to be
# able to order the colors at a glance, which only lightness does reliably.
SEQ_BLUE <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
INK_PRIMARY   <- "#0b0b0b"
INK_SECONDARY <- "#52514e"
INK_MUTED     <- "#898781"
SURFACE       <- "#fcfcfb"
HAIRLINE      <- "#c3c2b7"

# --- 2. Fetch the geodata once -------------------------------------------------
if (!dir.exists(GEODATA_DIR)) dir.create(GEODATA_DIR, recursive = TRUE)

fetch_once <- function(spec, what) {
  if (file.exists(spec$file)) return(invisible(TRUE))
  message("Downloading ", what, " (first run only) ...")
  ok <- tryCatch({
    utils::download.file(spec$url, spec$file, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(spec$file)) {
    stop("Could not download ", what, " from:\n  ", spec$url,
         "\nThe maps need this file. Download it manually into '", GEODATA_DIR,
         "/' and run again.", call. = FALSE)
  }
  invisible(TRUE)
}
fetch_once(GEODATA$bundeslaender, "federal state boundaries")
fetch_once(GEODATA$postcodes,     "postcode coordinates")

postcode_csv <- file.path(GEODATA_DIR, "DE", "zipcodes.de.csv")
if (!file.exists(postcode_csv)) {
  utils::unzip(GEODATA$postcodes$file, exdir = file.path(GEODATA_DIR, "DE"))
}

# The postcode file lists one row per place (and sometimes per large recipient,
# e.g. a company with its own postcode), so a postcode can appear many times.
# The MEDIAN coordinate per postcode is used: it is unaffected by the occasional
# outlying entry in a way the mean would not be.
pc <- utils::read.csv(postcode_csv, stringsAsFactors = FALSE, encoding = "UTF-8",
                      colClasses = c(zipcode = "character"))
pc <- pc[!is.na(pc$latitude) & !is.na(pc$longitude), ]
PLZ_REFERENCE <- unique(pc$zipcode)
coords <- stats::aggregate(cbind(latitude, longitude) ~ zipcode, data = pc,
                           FUN = stats::median)

# --- 3. The cleaned survey data ------------------------------------------------
data_path <- file.path(OUTPUT_DIR, "cleaned_survey_data.rds")
if (!file.exists(data_path)) {
  stop("Could not find '", data_path, "'. Run run_pipeline.R first.", call. = FALSE)
}
d <- readRDS(data_path)

if (!"unique_id" %in% names(d)) {
  stop("The cleaned data has no `unique_id` column; this script needs it to ",
       "reduce the vignette-level rows to one row per expert.", call. = FALSE)
}

if (COMPLETERS_ONLY) {
  d <- d[!is.na(d$submitdate), , drop = FALSE]
  message("Restricted to completed questionnaires: ",
          length(unique(d$unique_id)), " experts.")
}

# One row per expert: the cleaned data holds one row per vignette, so counting
# rows would multiply every expert by their number of vignettes.
experts <- d[!duplicated(d$unique_id), , drop = FALSE]

# --- 4. Postcodes (does the work of pipeline/06_postcode.R) --------------------
# Two columns are needed downstream:
#   plz        the cleaned 5-digit postcode, NA when none could be read
#   plz_valid  TRUE / FALSE for answered questions, NA when Q0 was left blank
# If the pipeline already produced them, they are used as they are.

# Answers that mean "no answer" rather than a postcode. Treated as missing, not
# as an invalid postcode, so the two counts below stay interpretable.
NO_ANSWER_RX <- "^(na|n\\.?a\\.?|n/a|k\\.?a\\.?|keine angaben?|unbekannt|[-.?]+)$"

is_blank_answer <- function(s) {
  is.na(s) | !nzchar(s) | grepl(NO_ANSWER_RX, s, ignore.case = TRUE)
}

# Free-text answer -> 5-digit PLZ, or NA when there is none to extract.
extract_plz <- function(x) {
  s <- trimws(as.character(x))
  s[is_blank_answer(s)] <- NA_character_
  s <- sub("\\.0+$", "", s)          # "68159.0", from a numeric round-trip
  out <- rep(NA_character_, length(s))
  
  # (i) plain numbers. Four digits almost always means a leading zero was lost
  #     in a spreadsheet (01067 -> 1067), so pad rather than discard.
  pure <- !is.na(s) & grepl("^[0-9]{1,5}$", s)
  out[pure] <- formatC(as.integer(s[pure]), width = 5, format = "d", flag = "0")
  
  # (ii) everything else: the first stand-alone run of five digits, which covers
  #      "D-68159", "68159 Mannheim", "PLZ 68159".
  idx <- which(!is.na(s) & is.na(out))
  if (length(idx)) {
    m <- regexpr("(?<![0-9])[0-9]{5}(?![0-9])", s[idx], perl = TRUE)
    out[idx[m > 0]] <- regmatches(s[idx], m)
  }
  out
}

# Format check only - whether the postcode actually exists is reported separately
# in section 5, so a typo that happens to look like a PLZ is not silently hidden.
is_plz_format <- function(p) {
  ok <- !is.na(p) & grepl("^[0-9]{5}$", p)
  ok[ok] <- as.integer(p[ok]) >= 1000     # no German PLZ begins with 00
  ok
}

# Find the raw postcode column. Name first, then content. The content test
# requires values to be postcodes IN THE REFERENCE FILE, not merely five digits:
# a housing survey has other numeric columns (construction years, rents) that
# would otherwise match, and a year like 1990 pads to "01990", which is not a
# real PLZ.
detect_plz_column <- function(df, reference) {
  score <- function(col) {
    v <- df[[col]]
    if (!(is.character(v) || is.factor(v) || is.numeric(v))) return(0)
    s <- trimws(as.character(v))
    s <- s[!is_blank_answer(s)]
    if (length(s) < 20) return(0)
    p <- extract_plz(s)
    mean(!is.na(p) & p %in% reference)
  }
  by_name <- grep("^(q0($|[^0-9])|plz|postleitzahl|zip|postal|postcode)",
                  names(df), ignore.case = TRUE, value = TRUE)
  for (col in by_name) if (score(col) >= 0.5) return(col)
  scores <- vapply(names(df), score, numeric(1))
  best <- names(scores)[which.max(scores)]
  if (length(best) == 1L && scores[[best]] >= 0.8) return(best)
  NA_character_
}

if (!all(c("plz", "plz_valid") %in% names(experts))) {
  src <- PLZ_SOURCE_COL
  if (is.null(src) || !nzchar(src)) {
    src <- detect_plz_column(experts, PLZ_REFERENCE)
    if (is.na(src)) {
      stop("Could not work out which column holds the postcode answer (Q0).\n",
           "Set PLZ_SOURCE_COL near the top of this script to the right name.\n",
           "Columns available:\n  ", paste(names(experts), collapse = ", "),
           call. = FALSE)
    }
    message("Postcode column detected automatically: `", src, "`.")
  }
  if (!src %in% names(experts)) {
    stop("PLZ_SOURCE_COL is `", src, "`, which is not a column of the cleaned ",
         "data.", call. = FALSE)
  }
  
  raw      <- trimws(as.character(experts[[src]]))
  answered <- !is_blank_answer(raw)
  cand     <- extract_plz(raw)
  
  experts$plz_valid <- ifelse(answered, is_plz_format(cand), NA)
  experts$plz       <- ifelse(!is.na(experts$plz_valid) & experts$plz_valid,
                              cand, NA_character_)
  
  # Show what was thrown away, so a systematic parsing failure is visible rather
  # than just quietly shrinking the map.
  rejected <- unique(raw[answered & !is.na(experts$plz_valid) & !experts$plz_valid])
  if (length(rejected)) {
    message("Answers not usable as a postcode (", length(rejected),
            " distinct), first few: ",
            paste(utils::head(rejected, 10), collapse = " | "))
  }
} else {
  message("Using the `plz` / `plz_valid` columns already present in the data.")
}

experts <- experts[, c("unique_id", "plz", "plz_valid")]

n_experts   <- nrow(experts)
n_no_pc     <- sum(is.na(experts$plz_valid))
n_bad_pc    <- sum(!is.na(experts$plz_valid) & !experts$plz_valid)
n_with_pc   <- sum(!is.na(experts$plz))

message("Experts total              : ", n_experts)
message("  with a usable postcode   : ", n_with_pc)
message("  postcode not answered    : ", n_no_pc)
message("  postcode not a valid PLZ : ", n_bad_pc)

# --- 5. Expert counts per postcode, with coordinates ---------------------------
per_plz <- as.data.frame(table(experts$plz[!is.na(experts$plz)]),
                         stringsAsFactors = FALSE)
names(per_plz) <- c("plz", "n_experts")

per_plz <- merge(per_plz, coords, by.x = "plz", by.y = "zipcode", all.x = TRUE)
n_unmatched <- sum(is.na(per_plz$latitude))
if (n_unmatched > 0) {
  message("NOTE: ", n_unmatched, " postcode(s) covering ",
          sum(per_plz$n_experts[is.na(per_plz$latitude)]),
          " expert(s) are not in the postcode reference and cannot be placed.")
}
placed <- per_plz[!is.na(per_plz$latitude), ]

if (nrow(placed) == 0) {
  stop("No expert could be placed on the map: every postcode is missing or ",
       "unmatched. Check the postcode column (see PLZ_SOURCE_COL).", call. = FALSE)
}

# --- 6. Geometry ----------------------------------------------------------------
states <- sf::st_read(GEODATA$bundeslaender$file, quiet = TRUE)

pts <- sf::st_as_sf(placed, coords = c("longitude", "latitude"), crs = 4326)

# Assign each postcode to a federal state by LOCATION rather than by the state
# column in the postcode file: that column mixes German and English names
# ("Bayern" and "Bavaria" both appear) and is wrong for company postcodes, so
# joining on it would silently mis-assign respondents.
pts <- sf::st_join(pts, states[, "name"], join = sf::st_within)

# A postcode centroid lying exactly on a state border matches BOTH polygons and
# comes back as two rows, which would count those experts twice in the
# choropleth. Keep the first match per postcode; which of two adjacent states a
# border postcode lands in is arbitrary either way.
n_border <- sum(duplicated(pts$plz))
if (n_border > 0) {
  pts <- pts[!duplicated(pts$plz), ]
  message("NOTE: ", n_border, " postcode(s) sit on a state border and matched ",
          "two states; each is counted once.")
}

# The state outlines are the simplified version, so a postcode centroid right on
# the coast or a national border can fall just outside every polygon. Those
# points still appear on the dot map but cannot be counted in the choropleth, so
# report the gap rather than letting the two maps quietly disagree.
n_outside <- sum(is.na(pts$name))
if (n_outside > 0) {
  message("NOTE: ", n_outside, " postcode(s) covering ",
          sum(pts$n_experts[is.na(pts$name)]),
          " expert(s) fall outside the simplified state outlines; they appear ",
          "on the dot map but not in the per-state totals.")
}

# Equal-area projection for Europe, so state sizes are comparable and Germany
# is not stretched the way plotting raw latitude/longitude would.
states_p <- sf::st_transform(states, 3035)
pts_p    <- sf::st_transform(pts, 3035)

# --- 7. Map 1: one dot per postcode --------------------------------------------
# Dot AREA is proportional to the expert count - area is what the eye compares,
# so scaling the radius by sqrt(n) keeps a dot for 4 experts looking four times
# a dot for 1, not sixteen times.
if (!dir.exists(MAP_DIR)) dir.create(MAP_DIR, recursive = TRUE)

counts <- pts_p$n_experts
radius <- 1.6 * sqrt(counts / max(counts))

png(file.path(MAP_DIR, "experts_by_plz_points.png"),
    width = 1500, height = 1750, res = 170)
par(mar = c(0, 0, 3.4, 0), bg = SURFACE)
plot(sf::st_geometry(states_p), col = "#f2f1ee", border = HAIRLINE, lwd = 0.6,
     reset = FALSE)
plot(sf::st_geometry(pts_p), add = TRUE, pch = 21,
     bg = grDevices::adjustcolor(SEQ_BLUE[5], alpha.f = 0.55),
     col = grDevices::adjustcolor(SEQ_BLUE[7], alpha.f = 0.7),
     lwd = 0.5, cex = radius)
title(main = "Surveyed experts by postcode area", col.main = INK_PRIMARY,
      cex.main = 1.15, font.main = 2, line = 1.9, adj = 0.02)
mtext(sprintf("%s experts with a usable postcode, in %s distinct postcode areas | dot area = number of experts",
              format(sum(counts), big.mark = ","), format(nrow(placed), big.mark = ",")),
      side = 3, line = 0.55, adj = 0.02, cex = 0.72, col = INK_SECONDARY)

# Size legend: a few round numbers spanning the observed range.
legend_vals <- unique(c(1, round(max(counts) / 4), round(max(counts) / 2), max(counts)))
legend_vals <- legend_vals[legend_vals > 0]
legend("bottomleft", inset = c(0.02, 0.03), bty = "n",
       legend = legend_vals, pt.cex = 1.6 * sqrt(legend_vals / max(counts)),
       pch = 21, pt.bg = grDevices::adjustcolor(SEQ_BLUE[5], alpha.f = 0.55),
       col = grDevices::adjustcolor(SEQ_BLUE[7], alpha.f = 0.7),
       title = "Experts", title.col = INK_SECONDARY, text.col = INK_SECONDARY,
       y.intersp = 1.45, cex = 0.78)
dev.off()

# --- 8. Map 2: choropleth by federal state --------------------------------------
by_state <- as.data.frame(table(pts_p$name), stringsAsFactors = FALSE)
names(by_state) <- c("name", "n_postcodes")
agg <- stats::aggregate(n_experts ~ name, data = sf::st_drop_geometry(pts_p), FUN = sum)
by_state <- merge(by_state, agg, by = "name", all = TRUE)

states_p <- merge(states_p, by_state, by = "name", all.x = TRUE)
states_p$n_experts[is.na(states_p$n_experts)] <- 0

# Equal-count (quantile) breaks: expert numbers are heavily skewed towards the
# big states, so equal-width bins would put almost every state in the lightest
# class and show nothing.
brk <- unique(stats::quantile(states_p$n_experts, probs = seq(0, 1, length.out = 6),
                              na.rm = TRUE))
if (length(brk) < 3) brk <- unique(range(states_p$n_experts))
bin <- cut(states_p$n_experts, breaks = brk, include.lowest = TRUE)
ramp <- grDevices::colorRampPalette(SEQ_BLUE)(nlevels(bin))
fill <- ramp[as.integer(bin)]

png(file.path(MAP_DIR, "experts_by_bundesland.png"),
    width = 1500, height = 1750, res = 170)
par(mar = c(0, 0, 3.4, 0), bg = SURFACE)
plot(sf::st_geometry(states_p), col = fill, border = SURFACE, lwd = 1.4, reset = FALSE)
title(main = "Surveyed experts by federal state", col.main = INK_PRIMARY,
      cex.main = 1.15, font.main = 2, line = 1.9, adj = 0.02)
mtext("Number of experts whose company is registered in each state",
      side = 3, line = 0.55, adj = 0.02, cex = 0.72, col = INK_SECONDARY)

# Direct labels: with only 16 areas the number fits on the map, which beats
# making the reader match a colour back to a legend swatch.
#
# Each label is drawn with a thin halo in the opposite ink. The city states and
# Bremen are far smaller than the text that sits on them, so a label placed
# correctly inside its own polygon still visually overlaps the neighbouring
# state - Bremen's dark number lands on dark-blue Niedersachsen and becomes
# unreadable. The halo makes every label legible whatever it ends up over.
ctr <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(states_p)))
lab_col  <- ifelse(as.integer(bin) > nlevels(bin) / 2, "#ffffff", INK_PRIMARY)
halo_col <- ifelse(lab_col == "#ffffff", INK_PRIMARY, "#ffffff")
labels   <- format(states_p$n_experts, big.mark = ",")

offset <- max(diff(par("usr")[1:2]), diff(par("usr")[3:4])) * 0.0016
for (dx in c(-1, 0, 1)) {
  for (dy in c(-1, 0, 1)) {
    if (dx == 0 && dy == 0) next
    text(ctr[, 1] + dx * offset, ctr[, 2] + dy * offset, labels = labels,
         cex = 0.72, col = halo_col, font = 2)
  }
}
text(ctr[, 1], ctr[, 2], labels = labels, cex = 0.72, col = lab_col, font = 2)

legend("bottomleft", inset = c(0.02, 0.03), bty = "n",
       legend = levels(bin), fill = ramp, border = SURFACE,
       title = "Experts", title.col = INK_SECONDARY, text.col = INK_SECONDARY,
       cex = 0.72, y.intersp = 1.15)
dev.off()

# --- 9. The numbers behind the maps ---------------------------------------------
out_state <- sf::st_drop_geometry(states_p)[, c("name", "n_experts", "n_postcodes")]
out_state <- out_state[order(-out_state$n_experts), ]
utils::write.csv(out_state, file.path(MAP_DIR, "experts_by_bundesland.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

out_plz <- per_plz[order(-per_plz$n_experts), c("plz", "n_experts", "latitude", "longitude")]
utils::write.csv(out_plz, file.path(MAP_DIR, "experts_by_plz.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

message("\n==================== Maps finished ====================")
message("Experts mapped     : ", sum(counts))
message("Postcode areas     : ", nrow(placed))
message("Maps written to    : ", MAP_DIR)
message("=======================================================")

