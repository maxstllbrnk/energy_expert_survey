# =============================================================================
# 02_link.R  -  bring frame and respondents onto the same grain, and divide
# =============================================================================
# The response rate is  answered / contacted, and the two sides come from two
# different files. This script makes them comparable:
#
#   load_respondents()   one row per person who opened the questionnaire
#   stack_gruppen()      a copy of the data per Berufsgruppe, plus one for all
#   ruecklauf_by()       contacted, answered and the rate, per cell
#   abbruch_by()         the break-off rate among those who did answer
#
# Counting people, not rows
# -------------------------
# survey_main has 4,176 rows but only 3,937 distinct (wave, token) pairs: 215
# tokens appear more than once, always inside one wave, i.e. the same invited
# person started the questionnaire more than once. Every duplicate token is
# collapsed to one row here, so a person who started three times counts once -
# using rows as the numerator would overstate the response rate by 6 %.
#
# Of the duplicates the furthest-progressed attempt is kept, so a person who
# broke off once and completed later counts as a completer.
#
# What cannot be deduplicated
# ---------------------------
# Tokens are generated per survey, so the same person invited in two waves has
# two unrelated tokens and is counted twice in the numerator, while the frame
# counts them once (it is deduplicated on the e-mail address). This cannot be
# detected without a token -> e-mail bridge; see TOKEN_BRIDGE_FILE.
# =============================================================================


# --- the numerator -----------------------------------------------------------
load_respondents <- function() {
  dat <- load_analysis_data(completers_only = FALSE)

  dat$main %>%
    # Furthest-progressed attempt first: completed before not completed, and
    # among those the one that reached the highest page. slice_head then keeps
    # exactly that row, so the kept `is_complete` equals any(is_complete) over
    # the person's attempts.
    arrange(desc(is_complete), desc(meta_lastpage)) %>%
    slice_head(n = 1, by = c(wave, meta_token)) %>%
    select(wave, meta_token, is_complete, plz, bundesland, bundesland_source,
           berufsgruppe, target_group)
}


# --- which classification the numerator uses ---------------------------------
# The frame has no token and the survey has no e-mail, so a respondent cannot be
# matched to their frame row. Until a bridge exists the numerator is classified
# from Q2. apply_token_bridge() replaces that with the exact assignment the
# moment a token -> frame mapping appears, and nothing else in the analysis
# changes.
apply_token_bridge <- function(resp, frame) {
  if (!file.exists(TOKEN_BRIDGE_FILE)) {
    attr(resp, "numerator_source") <- "q2"
    return(resp)
  }

  bridge <- read_csv(TOKEN_BRIDGE_FILE, col_types = cols(
    wave = col_character(), token = col_character(), id = col_integer()))

  resp <- resp %>%
    left_join(bridge, by = c("wave", "meta_token" = "token")) %>%
    left_join(frame %>% select(id = frame_id, berufsgruppe_frame = berufsgruppe),
              by = "id") %>%
    mutate(berufsgruppe = coalesce(berufsgruppe_frame, berufsgruppe)) %>%
    select(-berufsgruppe_frame, -id)

  message("Token bridge: ", fmt_n(sum(!is.na(resp$berufsgruppe))), " respondents ",
          "assigned to a sample from the frame")
  attr(resp, "numerator_source") <- "token"
  resp
}


# --- one copy per group ------------------------------------------------------
# Every rate is computed for all contacts together and once per Berufsgruppe.
# Stacking the copies means the same code computes both, with .by = gruppe.
#
# Respondents whose Berufsgruppe is unknown (Q2 unanswered, or "Keine
# Zuordnung") appear in "Alle Berufsgruppen" but in no single group, so the
# three group rates add up to slightly less than the overall rate. That is the
# honest treatment: they were contacted and they answered, but which of the
# three lists they came from is not known.
stack_gruppen <- function(data) {
  bind_rows(
    data %>% mutate(gruppe = ALL_TYPES),
    data %>%
      filter(as.character(berufsgruppe) %in% unname(FRAME_TYPE_LABELS)) %>%
      mutate(gruppe = as.character(berufsgruppe))
  ) %>%
    mutate(gruppe = factor(gruppe, levels = RUECKLAUF_GRUPPEN))
}


# --- the rate ----------------------------------------------------------------
# Contacted, answered and answered/contacted per cell. Cells of the frame with
# no respondent at all still appear, with a rate of 0 - a state nobody answered
# from is a result, not a missing value.
ruecklauf_by <- function(frame, resp, by) {
  kontakte <- frame %>%
    summarise(n_kontakte = n(), .by = all_of(by))

  antworten <- resp %>%
    summarise(n_antworten = n(), .by = all_of(by))

  kontakte %>%
    left_join(antworten, by = by) %>%
    mutate(n_antworten    = coalesce(n_antworten, 0L),
           ruecklaufquote = n_antworten / n_kontakte)
}


# --- the break-off rate ------------------------------------------------------
# Among the people who opened the questionnaire, the share who did not submit
# it. This says nothing about the frame, so it is computed on the respondents
# alone and is the figure that answers "are there regional patterns in
# break-offs?".
abbruch_by <- function(resp, by) {
  resp %>%
    summarise(n_begonnen     = n(),
              n_abgeschlossen = sum(is_complete),
              .by = all_of(by)) %>%
    mutate(n_abbruch     = n_begonnen - n_abgeschlossen,
           abbruchquote  = n_abbruch / n_begonnen)
}


# --- captions ----------------------------------------------------------------
# Every figure states the two sides of the fraction and which frame variant and
# response definition it shows. A figure pasted into a slide has to carry that
# with it.
#
# `gruppiert` says whether the figure splits respondents by Berufsgruppe. Only
# then does the numerator have to be classified, and only then is the note about
# where that classification comes from relevant - on a figure over all contacts
# it would suggest a caveat that does not apply.
rl_caption <- function(n_antworten, n_kontakte, note = NULL, gruppiert = FALSE) {
  first_line <- str_c(c(str_c(fmt_n(n_antworten), " von ", fmt_n(n_kontakte), " Kontakten"), note),
                      collapse = " · ")
  str_c(str_wrap(first_line, 120), "\n",
        "Kontakte: ", rl_variant$label, " · Antworten: ", rl_sample$label,
        if (gruppiert) str_c("\n", str_wrap(unname(NUMERATOR_SOURCE_LABELS[NUMERATOR_SOURCE]), 120)))
}
