# =============================================================================
# build_plz_bundesland.R  -  run ONCE, then never again
# =============================================================================
# Downloads the two open data sets that a postcode -> federal state lookup needs,
# stores them in Dropbox (GEODATA_DIR), and builds `plz_bundesland.csv` from
# them. After this has run, nothing in the pipeline downloads anything: the
# cleaning run reads that one small CSV and does a left join.
#
#   Rscript R/geodata/build_plz_bundesland.R          # skips what is already there
#   Rscript R/geodata/build_plz_bundesland.R refresh  # re-download everything
#
# =============================================================================
# THE TWO SOURCES
# =============================================================================
# 1. BKG "Verwaltungsgebiete 1:2 500 000" (VG2500), from the Bundesamt fuer
#    Kartographie und Geodaesie - the federal mapping agency, i.e. the official
#    source for German administrative boundaries. Licence: Datenlizenz
#    Deutschland - Namensnennung 2.0 (dl-de/by-2-0). BKG publishes an MD5
#    alongside each archive and this script checks the download against it.
#
#    Two things are taken from it: the authoritative table of the sixteen
#    federal states (their official two-digit key SN_L and their official
#    German name GEN), and the state polygons themselves, kept for maps.
#
# 2. GeoNames postal codes for Germany. Licence: CC-BY 4.0. One row per
#    postcode x place with coordinates and, crucially, `admin3_code` - the
#    official five-digit AGS (Amtlicher Gemeindeschluessel) of the district.
#
# =============================================================================
# WHY THE AGS AND NOT A SPATIAL JOIN
# =============================================================================
# The old pipeline placed each postcode at its coordinates and asked which state
# polygon contained the point. That works, but it needs `sf`, it needs the
# polygons at run time, and near a border it depends on how finely the boundary
# was drawn.
#
# The first two digits of the AGS ARE the official federal state key, so the
# state can be read straight off the postcode's district - no geometry at all.
# Checked against a point-in-polygon join on the BKG polygons, the two agree on
# 99.80% of the 23,297 GeoNames rows, and the exceptions are all border cases
# (most of them on the Hamburg / Schleswig-Holstein boundary, where the 1:2.5m
# generalisation cuts corners). The AGS route is the more accurate of the two
# AND the one with no dependencies, so it is the one used here.
#
# What is deliberately NOT used is GeoNames' own `admin1_name` column - the same
# trap the old 07_bundesland.R warned about. It holds 23 distinct spellings for
# 16 states ("Bayern" and "Bavaria", "Sachsen" and "Saxony", "Land Berlin",
# "Lower Saxony", ...) and agrees with the AGS-derived state only 90.3% of the
# time. Every state name in the output comes from the BKG table instead.
#
# =============================================================================
# POSTCODES THAT STRADDLE A BORDER
# =============================================================================
# Postcodes are Deutsche Post's routing areas, not administrative units, so a
# few of them cross a state line. 32 of 10,813 do (0.30%). Each is assigned to
# the state holding most of its places, and the output records `n_states` and
# `share` so those rows can be found and treated differently. Ties are broken by
# the lower state key, which is arbitrary but at least reproducible.
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).", call. = FALSE)

source("config.R")

refresh <- "refresh" %in% commandArgs(trailingOnly = TRUE)

BKG_BASE     <- "https://daten.gdz.bkg.bund.de/produkte/vg/vg2500/aktuell/"
BKG_ARCHIVE  <- "vg2500_01-01-2026.utm32s.shape.zip"
GEONAMES_URL <- "https://download.geonames.org/export/zip/DE.zip"

RAW_GEO_DIR  <- file.path(GEODATA_DIR, "raw")       # the archives, exactly as downloaded
GEONAMES_DIR <- file.path(GEODATA_DIR, "geonames")  # DE.txt, unpacked

dir.create(RAW_GEO_DIR, recursive = TRUE, showWarnings = FALSE)

# --- download helpers --------------------------------------------------------
# Nothing is fetched twice: a file that is already on disk is left alone unless
# the script was started with "refresh".
fetch <- function(url, dest) {
  if (file.exists(dest) && !refresh) {
    message("  have   ", basename(dest))
    return(invisible(dest))
  }
  message("  get    ", basename(dest), "  <- ", url)
  ok <- try(download.file(url, dest, mode = "wb", quiet = TRUE), silent = TRUE)
  if (inherits(ok, "try-error") || !file.exists(dest))
    stop("Could not download ", url, "\n",
         "Check the network, or open the URL in a browser and save the file to\n  ",
         dest, call. = FALSE)
  invisible(dest)
}

# =============================================================================
# 1. the official boundaries
# =============================================================================
message("BKG VG2500 (official state boundaries)")
bkg_zip <- fetch(paste0(BKG_BASE, BKG_ARCHIVE), file.path(RAW_GEO_DIR, BKG_ARCHIVE))
bkg_md5 <- fetch(paste0(BKG_BASE, BKG_ARCHIVE, ".md5"),
                 file.path(RAW_GEO_DIR, paste0(BKG_ARCHIVE, ".md5")))

# BKG publishes the checksum next to the archive, so a truncated or tampered
# download is caught here rather than turning into a puzzling result later.
published <- str_trim(str_split_1(readLines(bkg_md5, warn = FALSE)[1], "\\s+")[1])
observed  <- unname(tools::md5sum(bkg_zip))
if (!identical(tolower(published), tolower(observed)))
  stop("MD5 mismatch for ", BKG_ARCHIVE, "\n  published: ", published,
       "\n  downloaded: ", observed,
       "\nDelete the file and run again.", call. = FALSE)
message("  md5 ok ", observed)

if (refresh || !file.exists(BUNDESLAND_SHP)) {
  unlink(BKG_DIR, recursive = TRUE)
  unzip(bkg_zip, exdir = BKG_DIR)
}

# The attribute table alone is enough for the state names, and reading it with
# `foreign` means this script - and therefore the whole postcode chain - needs
# no spatial package at all. `sf` is only needed later, to draw maps.
lan <- foreign::read.dbf(sub("\\.shp$", ".dbf", BUNDESLAND_SHP), as.is = TRUE)

# GF is the BKG "Geofaktor": 9 marks the land area of a state, the other rows
# are its share of the sea and of Lake Constance. Keeping only GF 9 gives
# exactly the sixteen states, once each, under their official names.
land_key <- lan |>
  filter(GF == 9) |>
  transmute(bl_code = as.character(SN_L), bundesland = as.character(GEN)) |>
  distinct() |>
  arrange(bl_code)

if (nrow(land_key) != 16 || !identical(land_key$bl_code, sprintf("%02d", 1:16)))
  stop("Expected the sixteen federal states with keys 01-16 in ", basename(BUNDESLAND_SHP),
       ", got ", nrow(land_key), ": ", str_c(land_key$bl_code, collapse = ", "),
       "\nBKG may have changed the file layout.", call. = FALSE)
message("  ", nrow(land_key), " federal states read from VG2500_LAN.dbf")

# =============================================================================
# 2. the postcodes
# =============================================================================
message("GeoNames postal codes DE")
gn_zip <- fetch(GEONAMES_URL, file.path(RAW_GEO_DIR, "DE.zip"))
if (refresh || !file.exists(file.path(GEONAMES_DIR, "DE.txt"))) {
  dir.create(GEONAMES_DIR, recursive = TRUE, showWarnings = FALSE)
  unzip(gn_zip, exdir = GEONAMES_DIR)
}

# Tab-separated, no header, and no quoting at all - place names contain
# apostrophes and quotes, so read_tsv must be told not to look for them.
GEONAMES_COLS <- c("country", "plz", "place", "admin1_name", "admin1_code",
                   "admin2_name", "admin2_code", "admin3_name", "admin3_code",
                   "lat", "lon", "accuracy")

gn <- read_tsv(file.path(GEONAMES_DIR, "DE.txt"), col_names = GEONAMES_COLS,
               quote = "", na = c("", "NA"),
               col_types = cols(.default = col_character(),
                                lat = col_double(), lon = col_double(),
                                accuracy = col_integer()),
               locale = locale(encoding = "UTF-8"), progress = FALSE)

n_read <- nrow(gn)
gn <- gn |> filter(str_detect(plz, "^[0-9]{5}$"), !is.na(admin3_code))
message("  ", n_read, " rows, ", n_read - nrow(gn), " without a usable postcode or AGS, ",
        n_distinct(gn$plz), " distinct postcodes")

# =============================================================================
# 3. the lookup
# =============================================================================
# One row per postcode: the state most of its places sit in, plus enough
# information to see when that was not unanimous.
lookup <- gn |>
  mutate(bl_code = str_sub(admin3_code, 1, 2)) |>
  count(plz, bl_code, name = "n_places") |>
  mutate(n_states = n_distinct(bl_code),
         share    = n_places / sum(n_places),
         .by      = plz) |>
  arrange(plz, desc(n_places), bl_code) |>
  slice(1, .by = plz) |>
  left_join(land_key, by = "bl_code") |>
  select(plz, bl_code, bundesland, n_states, share, n_places)

unknown_key <- lookup |> filter(is.na(bundesland))
if (nrow(unknown_key))
  stop("AGS prefixes with no federal state: ",
       str_c(unique(unknown_key$bl_code), collapse = ", "), call. = FALSE)

stopifnot(!anyDuplicated(lookup$plz))

dir.create(GEODATA_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(lookup, PLZ_LOOKUP_FILE, na = "")

# =============================================================================
# 4. provenance
# =============================================================================
# Written next to the data so that in two years' time it is still clear where
# these numbers came from, which vintage, and under what licence - without
# having to find this script.
writeLines(c(
  "plz_bundesland.csv  -  German postcode -> federal state",
  str_c("built ", format(Sys.time(), "%Y-%m-%d %H:%M"), " by R/geodata/build_plz_bundesland.R"),
  "",
  "SOURCE 1  federal state names and boundaries",
  str_c("  ", BKG_BASE, BKG_ARCHIVE),
  "  Bundesamt fuer Kartographie und Geodaesie (BKG), Verwaltungsgebiete 1:2 500 000",
  str_c("  vintage ", readLines(file.path(BKG_DIR, "dokumentation", "aktualitaet.txt"),
                                warn = FALSE)[1]),
  str_c("  md5 ", observed, "  (matched the published checksum)"),
  "  licence: Datenlizenz Deutschland - Namensnennung 2.0 (dl-de/by-2-0)",
  "  cite as: (c) GeoBasis-DE / BKG (year), dl-de/by-2-0",
  "",
  "SOURCE 2  postcodes",
  str_c("  ", GEONAMES_URL),
  "  GeoNames postal codes, Germany",
  str_c("  downloaded file dated ", format(file.mtime(gn_zip), "%Y-%m-%d")),
  "  licence: CC-BY 4.0  -  https://www.geonames.org",
  "",
  "METHOD",
  "  state = first two digits of admin3_code (the official AGS district key),",
  "  resolved to a name through the BKG table. GeoNames' own admin1_name column",
  "  is NOT used: it mixes German and English spellings.",
  "  A postcode covering more than one state is assigned to the state holding",
  "  most of its places; n_states and share record that.",
  "",
  "RESULT",
  str_c("  ", nrow(lookup), " postcodes"),
  str_c("  ", sum(lookup$n_states > 1), " covering more than one federal state"),
  "",
  "REBUILD",
  "  Rscript R/geodata/build_plz_bundesland.R refresh"
), file.path(GEODATA_DIR, "provenance.txt"))

message("\nplz_bundesland.csv: ", nrow(lookup), " postcodes, ",
        sum(lookup$n_states > 1), " spanning more than one state")
message("-> ", normalizePath(GEODATA_DIR))
message("   plz_bundesland.csv   the lookup the cleaning pipeline reads")
message("   provenance.txt       where it came from")
message("   bkg_vg2500/          official state polygons, for maps")
message("   geonames/, raw/      the downloads, kept so this need not run again")
