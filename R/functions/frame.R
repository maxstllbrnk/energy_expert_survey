################################################################################
# frame.R — the sampling frame and the dispatch log: who was invited, the
#           denominator of every response rate
# Inputs : FRAME_FILE (secure, holds e-mail addresses), PLZ_LOOKUP_FILE,
#          VERSAND_FILE
# Outputs: none
# Checks : every list of the frame has a group
################################################################################
# Everyone counts as a person: an e-mail address of the lists once, an invitation
# once. There are no token lists (Max, 2026-11-09), so a response cannot be matched
# to its list: by group, the denominator of a rate is the mailing list and the
# numerator the answer to Q2. Every exhibit of a rate by group says so in its note.

# 1. Settings ------------------------------------------------------------------
# The registers the lists were drawn from, with the names of EXPERT_GROUPS
FRAME_TYPE_LABELS <- c(
  "DENA" = "Energy advisers",
  "SHK" = "HVAC installers",
  "Schornsteinfeger" = "Chimney sweeps"
)

# Telekom appears to have blocked part of the mailing: the dispatch log notes "nach
# dieser umfrage tendenziell geblocked von telekom??", and some reminders went out
# "nicht t-online". An invitation that never arrived depresses the response rate.
TONLINE_PATTERN <- "@t-online\\."

# How unanimous the surrounding postcodes have to be before the first three digits
# stand in for a postcode the lookup does not have: as in R/cleaning/03_recode.R,
# so that a respondent and a contact with one postcode land in the same state.
FRAME_PLZ_PREFIX_MIN_PURITY <- 0.9

GROUP_RATE_NOTE <- paste0(
  "By group, the denominator is the mailing list and the numerator the answer to ",
  "Q2: there are no token lists, so a response cannot be matched to its list."
)

# The federal states in official key order, as in the data and on the map
STATE_LABELS <- c(
  "Schleswig-Holstein" = "Schleswig-Holstein",
  "Hamburg" = "Hamburg",
  "Niedersachsen" = "Lower Saxony",
  "Bremen" = "Bremen",
  "Nordrhein-Westfalen" = "North Rhine-Westphalia",
  "Hessen" = "Hesse",
  "Rheinland-Pfalz" = "Rhineland-Palatinate",
  "Baden-Württemberg" = "Baden-Württemberg",
  "Bayern" = "Bavaria",
  "Saarland" = "Saarland",
  "Berlin" = "Berlin",
  "Brandenburg" = "Brandenburg",
  "Mecklenburg-Vorpommern" = "Mecklenburg-Western Pomerania",
  "Sachsen" = "Saxony",
  "Sachsen-Anhalt" = "Saxony-Anhalt",
  "Thüringen" = "Thuringia"
)

# The target group of a wave: Personen and Firmen in the dispatch log, the other
# two only in the synthetic data of tests/simulate_data.R
TARGET_GROUP_LABELS <- c(
  "Personen" = "Persons",
  "Firmen" = "Firms",
  "Energieberater" = "Energy advisers",
  "Schornsteinfeger" = "Chimney sweeps"
)

# 2. The frame -----------------------------------------------------------------

#' The sampling frame: one row per contacted person, the denominator of the rates
#'
#' The e-mail address is the identity of a contact, and it does not leave this
#' function: nothing downstream needs it, so it cannot reach an output.
#'
#' @param path the frame file, one row per entry of the lists: id, Email, type, PLZ
#' @return tibble: frame_id, type, plz, expert_group (factor), is_tonline, bundesland
#'   (factor). Attributes: n_raw (rows of the file), n_duplicate_contacts (rows
#'   collapsed), n_type_conflicts (addresses on more than one list)
load_frame <- function(path = FRAME_FILE) {
  # as_tibble(): the data frame of the file as a tibble
  raw <- as_tibble(readRDS(path))

  # (1) The file was written on a latin1 locale without declaring it: without this
  # every umlaut is invalid UTF-8, and any regular expression over it stops.
  for (column in names(raw)) {
    if (is.character(raw[[column]])) {
      # Encoding<-: declares the encoding; enc2utf8(): converts to UTF-8
      Encoding(raw[[column]]) <- "latin1"
      raw[[column]] <- enc2utf8(raw[[column]])
    }
  }
  # str_squish(): no blanks at the ends, single blanks inside; str_to_lower(): small
  # letters; str_detect(): whether the pattern occurs; coalesce(): the first value
  # that is not NA
  contacts <- raw %>%
    transmute(frame_id = id,
              email = str_to_lower(str_squish(Email)),
              type = type,
              plz = if_else(str_detect(coalesce(PLZ, ""), "^[0-9]{5}$"),
                            PLZ, NA_character_))
  n_raw <- nrow(contacts)
  # Rows without an address cannot be told apart, so they collapse into one person
  # below. Whether they were invited at all is not known: they are counted.
  n_no_address <- sum(coalesce(contacts$email, "") == "")

  # (2) One row per person. Rows that share an e-mail address are one mailbox and
  # count once. An address on two lists stays on the first; these are counted, as
  # the group of the denominator then rests on an arbitrary choice.
  addresses_by_list <- contacts %>%
    distinct(email, type) %>%
    count(email)
  n_type_conflicts <- sum(addresses_by_list$n > 1)
  frame <- contacts %>%
    arrange(frame_id) %>%
    distinct(email, .keep_all = TRUE)

  # (3) The group and t-online
  frame <- frame %>%
    mutate(expert_group = factor(label_of(type, FRAME_TYPE_LABELS,
                                          "list of the frame"),
                                 levels = EXPERT_GROUPS),
           is_tonline = coalesce(str_detect(email, TONLINE_PATTERN), FALSE)) %>%
    select(-email)

  # (4) The federal state, by the rule of R/cleaning/03_recode.R: the postcode, or
  # where the lookup does not have it, its first three digits if they are
  # unanimous. The numerator comes from that rule, and the two sides must be
  # comparable.
  plz_lookup <- read_csv(PLZ_LOOKUP_FILE, show_col_types = FALSE,
                         col_types = cols(plz = col_character(),
                                          bl_code = col_character()))
  state_levels <- plz_lookup %>%
    distinct(bl_code, bundesland) %>%
    arrange(bl_code) %>%
    pull(bundesland)
  # A share of at least FRAME_PLZ_PREFIX_MIN_PURITY leaves one state per prefix.
  # str_sub(): the characters from the first to the third
  plz3_lookup <- plz_lookup %>%
    count(plz3 = str_sub(plz, 1, 3), bundesland) %>%
    group_by(plz3) %>%
    mutate(purity = n / sum(n)) %>%
    ungroup() %>%
    filter(purity >= FRAME_PLZ_PREFIX_MIN_PURITY)
  # match(): the row of the lookup for each postcode, NA if it has none
  exact <- match(frame$plz, plz_lookup$plz)
  prefix <- match(str_sub(frame$plz, 1, 3), plz3_lookup$plz3)
  frame <- frame %>%
    mutate(bundesland = factor(coalesce(plz_lookup$bundesland[exact],
                                        plz3_lookup$bundesland[prefix]),
                               levels = state_levels))

  # attr(): counts attached to the object, for the table of the waves
  attr(frame, "n_raw") <- n_raw
  attr(frame, "n_duplicate_contacts") <- n_raw - nrow(frame)
  attr(frame, "n_type_conflicts") <- n_type_conflicts
  message("Frame: ", nrow(frame), " contacted persons from ", n_raw, " rows (",
          n_type_conflicts, " addresses on more than one list, ", n_no_address,
          " rows without an address), ", sum(frame$is_tonline), " at t-online, ",
          sum(is.na(frame$bundesland)), " without a usable postcode")
  frame
}

# 3. The dispatch log ----------------------------------------------------------

#' The dispatch log per wave, a cross-check on the frame. It counts invitations
#' sent, a different quantity from the people on the lists (a person invited in two
#' waves received two), but a large gap would mean that the frame is not what was
#' mailed.
#'
#' @param path the batch overview, one row per batch of a wave
#' @return tibble, one row per wave: wave, invitations, first_sent, n_partly_sent
#'   (batches marked as only partly sent)
load_versand_waves <- function(path = VERSAND_FILE) {
  read_excel(path, sheet = "Versandübersicht", skip = 7, na = c("", "NA"),
             .name_repair = "unique_quiet") %>%
    filter(!is.na(`Umfrage-ID`)) %>%
    transmute(wave = as.character(`Umfrage-ID`),
              recipients = `Anzahl Empfänger je csv`,
              status = Versandstatus,
              sent = as.Date(Versanddatum)) %>%
    group_by(wave) %>%
    summarise(invitations = sum(recipients, na.rm = TRUE),
              first_sent = min(sent, na.rm = TRUE),
              n_partly_sent = sum(status == "Teilweise versendet", na.rm = TRUE)) %>%
    ungroup()
}
