# =============================================================================
# 01_frame.R  -  the sampling frame: one row per person contacted
# =============================================================================
# Turns the scraped expert list into the denominator of every response rate:
#
#   load_frame()   one row per contacted person, with berufsgruppe, bundesland
#                  and is_tonline
#
# Three things happen here, and each of them changes the denominator, so each
# is counted and reported in the build message and in tabellen_ruecklauf.xlsx:
#
#   1. encoding   the .rds holds latin1 strings without a declared encoding, so
#                 "Jörg" arrives as invalid UTF-8 and any regex over Email or
#                 Name fails. Declared and converted here, once.
#   2. one row per person   the list contains the same e-mail more than once.
#                 A person who was contacted twice must still count once, so
#                 the frame is deduplicated on the e-mail address.
#   3. bundesland  derived from the postcode with exactly the same rule the
#                 cleaning pipeline applies to the respondents - otherwise
#                 numerator and denominator would not be comparable.
# =============================================================================


# --- postcode -> federal state -----------------------------------------------
# The same two-step lookup as R/cleaning/03_recode.R: the postcode itself, and
# where it is unknown the first three digits, provided they are effectively
# unanimous. Kept identical on purpose - a respondent and a frame contact with
# the same postcode must land in the same state, or the rate is wrong.
PLZ_PREFIX_MIN_PURITY <- 0.9

add_bundesland_from_plz <- function(data, plz_col = "plz") {
  plz <- data[[plz_col]]

  plz_lookup <- read_csv(PLZ_LOOKUP_FILE, col_types = cols(
    plz = col_character(), bl_code = col_character(), bundesland = col_character(),
    n_states = col_integer(), share = col_double(), n_places = col_integer()))

  bundeslaender <- plz_lookup %>% distinct(bl_code, bundesland) %>% arrange(bl_code)

  plz3_lookup <- plz_lookup %>%
    count(plz3 = str_sub(plz, 1, 3), bundesland) %>%
    mutate(purity = n / sum(n), .by = plz3) %>%
    slice_max(n, n = 1, by = plz3, with_ties = FALSE) %>%
    filter(purity >= PLZ_PREFIX_MIN_PURITY)

  exact  <- match(plz, plz_lookup$plz)
  prefix <- match(str_sub(plz, 1, 3), plz3_lookup$plz3)

  data %>%
    mutate(
      bundesland = factor(
        coalesce(plz_lookup$bundesland[exact], plz3_lookup$bundesland[prefix]),
        levels = bundeslaender$bundesland),
      bundesland_source = case_when(
        is.na(plz)                                    ~ "no_postcode",
        !is.na(exact) & plz_lookup$n_states[exact] > 1 ~ "plz_border",
        !is.na(exact)                                 ~ "plz",
        !is.na(prefix)                                ~ "plz_prefix",
        TRUE                                          ~ "unmatched")
    )
}


# --- the frame ---------------------------------------------------------------
# Returns one row per contacted person:
#   frame_id      the id of the row that was kept
#   email         lowercased, trimmed - the identity of a contact
#   berufsgruppe  the sample the contact was drawn from, labelled as in the survey
#   plz           5-digit postcode, or NA
#   bundesland    derived from plz, with bundesland_source
#   is_tonline    the invitation went to a t-online address
#
# The counts that were dropped on the way are attached as attributes, so
# 05_tabellen.R can report them without reading the file a second time.
load_frame <- function(path = FRAME_FILE) {
  raw <- readRDS(path)

  # 1. encoding. The file was written on a latin1 locale; without this every
  #    umlaut is invalid UTF-8 and sub()/grepl() abort on it.
  chr <- names(raw)[map_lgl(raw, is.character)]
  for (v in chr) {
    Encoding(raw[[v]]) <- "latin1"
    raw[[v]] <- enc2utf8(raw[[v]])
  }

  frame <- raw %>%
    as_tibble() %>%
    transmute(
      frame_id = id,
      email    = str_to_lower(str_squish(Email)),
      type     = type,
      plz      = if_else(str_detect(replace_na(PLZ, ""), "^[0-9]{5}$"), PLZ, NA_character_)
    )

  n_raw <- nrow(frame)

  # 2. one row per person. Contacts that share an e-mail are the same mailbox
  #    and were invited once, so they count once. Where the duplicates disagree
  #    about the sample they were drawn from, the row is still collapsed, but
  #    the conflict is counted - it is a property of the frame worth knowing.
  n_type_conflicts <- frame %>%
    summarise(n_types = n_distinct(type), .by = email) %>%
    filter(n_types > 1) %>%
    nrow()

  frame <- frame %>%
    arrange(frame_id) %>%
    distinct(email, .keep_all = TRUE)

  n_duplicate_contacts <- n_raw - nrow(frame)

  # 3. labels, t-online and the federal state.
  frame <- frame %>%
    mutate(
      berufsgruppe = factor(unname(FRAME_TYPE_LABELS[type]),
                            levels = unname(FRAME_TYPE_LABELS)),
      is_tonline   = str_detect(email, TONLINE_PATTERN)
    ) %>%
    add_bundesland_from_plz()

  if (any(is.na(frame$berufsgruppe)))
    stop("The frame holds a `type` that FRAME_TYPE_LABELS does not know: ",
         str_c(setdiff(unique(frame$type), names(FRAME_TYPE_LABELS)), collapse = ", "),
         "\nAdd it in R/ruecklauf/00_settings.R.", call. = FALSE)

  attr(frame, "n_raw")                <- n_raw
  attr(frame, "n_duplicate_contacts") <- n_duplicate_contacts
  attr(frame, "n_type_conflicts")     <- n_type_conflicts
  attr(frame, "frame_file")           <- path

  message("Frame: ", fmt_n(nrow(frame)), " contacted persons (",
          fmt_n(n_duplicate_contacts), " duplicate e-mail addresses collapsed",
          if (n_type_conflicts > 0) str_c(", ", fmt_n(n_type_conflicts),
                                          " of them with conflicting samples"), ")\n",
          "       ", fmt_n(sum(frame$is_tonline)), " t-online, ",
          fmt_n(sum(is.na(frame$bundesland))), " without a usable postcode")

  frame
}


# --- the dispatch log, as a cross-check --------------------------------------
# The Versandübersicht records one row per sending event, with the number of
# recipients of each participant list. Summed, that is how many invitations went
# out - a different quantity from the number of people on the list (a person
# invited in two waves received two), but close enough that a large gap would
# mean the frame is not the list that was actually mailed.
#
# R/cleaning/00_functions.R has load_versand(), which collapses the same sheet
# to one row per wave for the cleaning pipeline. Only the total is needed here,
# so the sheet is read directly rather than sourcing the cleaning code.
load_versand_total <- function(path = VERSAND_FILE) {
  read_excel(path, sheet = "Versandübersicht", skip = 7, na = c("", "NA"),
             .name_repair = "unique_quiet") %>%
    filter(!is.na(`Umfrage-ID`)) %>%
    pull(`Anzahl Empfänger je csv`) %>%
    sum(na.rm = TRUE)
}
