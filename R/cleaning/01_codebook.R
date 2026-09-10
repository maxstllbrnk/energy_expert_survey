# =============================================================================
# 01_codebook.R  -  DATA SET 1: the codebook
# =============================================================================
# question text (LimeSurvey)  |  question id  |  analysis variable name
#
# Built from the syntax files alone - no response data needed. Also records,
# for every variable, whether it exists in the soft-launch waves, and logs every
# structural difference between the soft launch and the main waves.
#
# Output: codebook.xlsx  (+ codebook.csv)
# =============================================================================

meta <- read_all_syntax()
ref  <- meta |> filter(wave == REFERENCE_WAVE)
soft <- setdiff(unique(meta$wave), unique(versand$wave[!versand$is_softlaunch]))
main_waves <- setdiff(unique(meta$wave), soft)

# --- one row per variable, reference wave first ------------------------------
codebook <- meta |>
  mutate(is_ref = wave == REFERENCE_WAVE) |>
  arrange(desc(is_ref), col) |>
  distinct(question_id, .keep_all = TRUE) |>
  transmute(
    question_text = question_text,
    question_id   = question_id,
    variable_name = var_name(question_id),
    value_codes   = val_codes,
    value_labels  = val_labels
  )

stopifnot(!any(is.na(codebook$variable_name)))
stopifnot(!any(duplicated(codebook$variable_name)))

# which waves contain each variable
presence <- meta |>
  mutate(soft = wave %in% soft) |>
  summarise(in_main = any(!soft), n_soft = n_distinct(wave[soft]), .by = question_id)

codebook <- codebook |>
  left_join(presence, by = "question_id") |>
  mutate(
    in_main       = if_else(in_main, "yes", "no"),
    in_softlaunch = case_when(n_soft == 2 ~ "both", n_soft == 1 ~ "one only", TRUE ~ "no"),
    block = case_when(
      str_detect(variable_name, "^att_v")   ~ "att",
      str_detect(variable_name, "^vig[0-9]_|^vig_intro$") ~ "vig",
      TRUE ~ str_remove(variable_name, "_.*$")),
    data_set = case_when(
      str_detect(variable_name, "^(vig[0-9]_|att_)") ~ "vignettes_long",
      str_detect(variable_name, "^meta_")            ~ "both (key/meta)",
      TRUE                                           ~ "survey_main")
  ) |>
  select(question_text, question_id, variable_name, block, data_set,
         value_codes, value_labels, in_main, in_softlaunch)

# --- variables the pipeline adds ---------------------------------------------
derived <- tribble(
  ~question_text, ~question_id, ~variable_name, ~block, ~data_set,

  "Unique respondent key, <wave>_<id>. Use this to join the two data sets.",
  "-", "resp_uid", "key", "both (key/meta)",
  "LimeSurvey survey id of the batch this respondent belongs to.",
  "-", "wave", "key", "both (key/meta)",
  "Mailing round from the batch overview (Soft Launch / Finaler Versand).",
  "-", "versandrunde", "key", "both (key/meta)",
  "TRUE for the two soft-launch waves.",
  "-", "is_softlaunch", "key", "both (key/meta)",
  "Target group of the batch (Personen / Firmen).",
  "-", "target_group", "key", "both (key/meta)",

  "TRUE if the respondent submitted the questionnaire (submitdate present).",
  "-", "is_complete", "flag", "survey_main",
  "TRUE if the respondent answered at least one vignette.",
  "-", "reached_vignettes", "flag", "survey_main",
  "Number of vignettes answered (0-6).",
  "-", "n_vignettes_answered", "flag", "survey_main",
  "Fernwaerme branch the respondent was routed into, from Q6a: fw or nofw.",
  "-", "vig_arm", "flag", "both (key/meta)",
  "TRUE if the respondent answered in BOTH branches, which should not happen.",
  "-", "arm_conflict", "flag", "both (key/meta)",

  "Company postcode as a 5-character string, zero-padded (Q0 read as a number loses leading zeros).",
  "Q0", "plz", "firm", "survey_main",
  "TRUE/FALSE: is plz a possible German postcode (01001-99998)?",
  "Q0", "plz_valid", "firm", "survey_main",
  "Federal state of the company, looked up from plz. Never asked in the questionnaire.",
  "Q0", "bundesland", "firm", "survey_main",
  str_c("How bundesland was reached: plz / plz_border (postcode straddles a state ",
        "line) / plz_prefix (matched on the first three digits) / unmatched / no_postcode."),
  "Q0", "bundesland_source", "firm", "survey_main",

  "Q6 with 'Sonstiges' free text mapped back onto a level where unambiguous.",
  "Q6", "firm_radius_recoded", "firm", "survey_main",
  "How firm_radius_recoded was arrived at: original / mapped / other_unmapped / no_answer.",
  "Q6", "firm_radius_recode_source", "firm", "survey_main",
  "Q22 with 'Sonstiges' free text mapped back onto a level where unambiguous.",
  "Q22", "dem_education_recoded", "dem", "survey_main",
  "How dem_education_recoded was arrived at.",
  "Q22", "dem_education_recode_source", "dem", "survey_main",

  "Vignette number as shown to the respondent (1-6).",
  "-", "vig_num", "vig", "vignettes_long",
  "Recommended heating technology, including the 7th option 'Keine Empfehlung'.",
  "-", "vig_rec", "vig", "vignettes_long",
  "Free text given with the 'Sonstiges' option.",
  "-", "vig_rec_other", "vig", "vignettes_long",
  "Response time for this vignette question, in seconds.",
  "-", "vig_time", "vig", "vignettes_long",
  "Group (page) time for this vignette, in seconds.",
  "-", "vig_group_time", "vig", "vignettes_long",
  "Design id (1-128) of the vignette shown, from vignetten_kombinationen.xlsx.",
  "-", "vignette_id", "att", "vignettes_long",
  "Expected investment cost - asked about the LAST vignette only.",
  "-", "vig_cost", "vig", "vignettes_long",
  "Which cost question the value came from: nofw / fw / fw_dhrec.",
  "-", "vig_cost_source", "vig", "vignettes_long",
  "Response time for the investment-cost question.",
  "-", "vig_cost_time", "vig", "vignettes_long"
) |>
  mutate(value_codes = NA_character_, value_labels = NA_character_,
         in_main = "yes", in_softlaunch = "both")

# the nine design attributes, documented with their German source names
att_rows <- tibble(
  slot = seq_along(ATTRIBUTE_NAMES),
  variable_name = str_c("att_", ATTRIBUTE_NAMES),
  german = unname(ATTRIBUTE_SOURCE[ATTRIBUTE_NAMES]),
  role = c("house image shown with the vignette",
           "age of the couple ('...im Hause Schmidt (___)')",
           "household income ('...ein Einkommen von ___')",
           "existing heating system ('Die bestehende ___ ...')",
           "when it must be replaced ('... ___.')",
           "construction year ('im Jahr ___ gebaut')",
           "heat demand in kWh/m2 per year",
           "heat distribution ('wird ueber ___ beheizt')",
           "renovation status ('gebaut und ___')")) |>
  transmute(
    question_text = str_c("Vignette design attribute ", slot, " of ", N_ATTRIBUTES,
                          " (", german, "): ", role),
    question_id = str_c("attribute_<", slot, ",", slot + 9, ",...>"),
    variable_name, block = "att", data_set = "vignettes_long",
    value_codes = NA_character_, value_labels = NA_character_,
    in_main = "yes", in_softlaunch = "both")

# The preference scores are built OUTSIDE the cleaning pipeline, by
# R/add_preferences.R, because standardising them makes them sample-dependent.
# They are documented here anyway - the codebook is the reference for the
# project, not just for what run_cleaning.R happens to write.
pref_rows <- tibble(
  measure = rep(c("tp", "risk"), each = 5),
  suffix  = rep(c("score", "z", "cell", "levels", "complete"), 2),
  concept = rep(c("patience", "risk tolerance"), each = 5),
  staircase = rep(c("Time1..Time5BBBB (14 arms at level 5)",
                    "Risk1..Risk5BBBB (16 arms at level 5)"), each = 5),
  role = rep(c("midpoint of the respondent's cell, in (0, 1); 0 = least",
               "the same, z-standardised - the standardisation SAMPLE is a choice, see the script",
               "the cell itself, 0 .. 2^levels - 1, ordered from least to most",
               "how many of the five choices were answered (0-5)",
               "TRUE if the respondent went as deep as their own branch allows"), 2)) |>
  transmute(
    question_text = str_c("GPS staircase measure of ", concept, " from ", staircase,
                          " - ", role),
    question_id = "-",
    variable_name = str_c(measure, "_", suffix),
    block = measure,
    data_set = "preferences (R/add_preferences.R)",
    value_codes = NA_character_, value_labels = NA_character_,
    in_main = "yes", in_softlaunch = "both")

codebook_out <- bind_rows(codebook, derived, att_rows, pref_rows)

# --- soft-launch differences --------------------------------------------------
compare_wave <- function(w) {
  d <- meta |> filter(wave == w)

  # line the two waves up on question_id, then compare within the tibble
  side_by_side <- full_join(
    ref |> select(question_id, ref_text = question_text,
                  ref_codes = val_codes, ref_labels = val_labels),
    d   |> select(question_id, alt_text = question_text,
                  alt_codes = val_codes, alt_labels = val_labels),
    by = "question_id"
  )

  bind_rows(
    side_by_side |>
      filter(is.na(alt_text) & !is.na(ref_text)) |>
      transmute(question_id, issue = "missing in soft launch",
                detail = "Variable does not exist in this wave; NA after stacking."),

    side_by_side |>
      filter(is.na(ref_text) & !is.na(alt_text)) |>
      transmute(question_id, issue = "extra in soft launch",
                detail = "Variable exists only in this wave; main waves get NA."),

    side_by_side |>
      filter(!is.na(ref_text), !is.na(alt_text),
             !is.na(ref_codes) | !is.na(alt_codes),
             is.na(ref_codes) != is.na(alt_codes) |
               coalesce(ref_codes != alt_codes | ref_labels != alt_labels, FALSE)) |>
      transmute(question_id, issue = "different answer options",
                detail = str_c("main: ", ref_labels, "  ---  ", w, ": ", alt_labels)),

    side_by_side |>
      filter(!is.na(ref_text), !is.na(alt_text),
             str_squish(ref_text) != str_squish(alt_text)) |>
      transmute(question_id, issue = "different question wording",
                detail = str_c("main: ", str_trunc(ref_text, 140), "  ---  ",
                               w, ": ", str_trunc(alt_text, 140))),

    side_by_side |>
      filter(!is.na(ref_text), !is.na(alt_text),
             str_squish(ref_text) == str_squish(alt_text), ref_text != alt_text) |>
      transmute(question_id, issue = "whitespace-only wording difference",
                detail = "Identical once runs of whitespace are collapsed - harmless.")
  ) |>
    mutate(wave = w, variable_name = var_name(question_id), .before = 1)
}

softlaunch_diffs <- map(soft, compare_wave) |> list_rbind() |> arrange(wave, issue, question_id)

# --- notes -------------------------------------------------------------------
readme <- tribble(
  ~item, ~description,
  "Source", "LimeSurvey R syntax exports in SYNTAX_DIR (see config.R)",
  "Main waves", str_c(str_c(sort(main_waves), collapse = ", "),
                      " (structurally identical to one another)"),
  "Soft-launch waves", str_c(str_c(sort(soft), collapse = ", "),
                             " - read from the Versandrunde column of the batch overview"),
  "Reference wave", REFERENCE_WAVE,
  "Variables", str_c(nrow(codebook_out), " (", nrow(codebook), " from the questionnaire, ",
                     nrow(derived) + nrow(att_rows), " added by the pipeline, ",
                     nrow(pref_rows), " added by R/add_preferences.R)"),
  "Naming", "lowercase snake_case, <block>_<concept>[_<item>]; unique and stable across waves",
  "Matching", str_c("Columns are matched to the raw exports BY QUESTION CODE, never by ",
                    "position. This is what lets the soft-launch waves (415 and 416 columns ",
                    "instead of 417) be merged with the main waves."),
  "Caveat: Time* vs timing",
  str_c("Time1..Time5BBBB are the time-PREFERENCE staircase, not response times; they are ",
        "named tp_*. Risk1..Risk5BBBB become risk_*. Response times are all named time_* ",
        "or grouptime_*. Note Time5 has only 14 arms where Risk5 has 16."),
  "Caveat: equation helpers",
  str_c("mix25_rand_*, mix25_score_*, mix25_top* are LimeSurvey equation artifacts. They are ",
        "kept for completeness but are sparsely filled and partly hold internal values."),
  "Caveat: soft-launch Q7",
  "Q7 was a 6-rank question with a 'weiss nicht' option in wave 334335 and a 5-rank question without it in the main waves. cust_rank6 is soft-launch only, and 'weiss nicht' is recoded to NA.",
  "Caveat: soft-launch Q6",
  "'bundesweit' was not offered in either soft-launch wave. The shared options mean the same thing; filter on is_softlaunch before comparing distributions."
)

# The codebook describes the data but contains none of it - it is built purely
# from the syntax files. It therefore goes to REPORT_DIR in Dropbox, where the
# coauthors can read it, rather than travelling with the respondent data.
write_xlsx(list(readme = readme, codebook = codebook_out,
                softlaunch_diffs = softlaunch_diffs),
           file.path(REPORT_DIR, "codebook.xlsx"))
write_csv(codebook_out, file.path(REPORT_DIR, "codebook.csv"), na = "")

message("codebook.xlsx: ", nrow(codebook_out), " variables, ",
        nrow(softlaunch_diffs), " soft-launch differences")
