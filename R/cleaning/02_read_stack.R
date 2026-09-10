# =============================================================================
# 02_read_stack.R  -  read every wave and stack them into one wide table
# =============================================================================
# For each raw export:
#   1. read it as text and rename the columns from their question codes
#   2. rename questions/timings to the analysis names from the codebook
#   3. stack, filling variables a wave does not have with NA
#   4. apply value labels ONCE, on the stacked data, so waves cannot disagree
#
# Produces `survey_wide`: one row per respondent, all waves together.
# =============================================================================

# --- find the raw files ------------------------------------------------------
# Searched recursively, so it does not matter whether the soft-launch exports
# sit beside the main ones or in a sub-folder of their own.
raw_files <- list.files(RAW_DIR, "^survey_[0-9]+_R_data_file\\.csv$",
                        full.names = TRUE, recursive = TRUE)
if (!length(raw_files))
  stop("No raw exports found under '", RAW_DIR, "'. Check RAW_DIR in config.R.", call. = FALSE)

raw_waves <- tibble(path = raw_files,
                    wave = str_match(basename(path), "^survey_([0-9]+)_")[, 2])

unknown <- setdiff(raw_waves$wave, versand$wave)
if (length(unknown))
  stop("These waves are not in the batch overview: ", str_c(unknown, collapse = ", "),
       "\nAdd them to ", VERSAND_FILE, " or they cannot be given a target group.",
       call. = FALSE)

message("Reading ", nrow(raw_waves), " wave(s): ", str_c(raw_waves$wave, collapse = ", "))

# --- read and rename one wave ------------------------------------------------
prepare_wave <- function(path, wave) {
  w    <- read_raw_wave(path)
  d    <- w$data
  kind <- w$kind

  new_names <- names(d)
  is_q    <- kind == "question"
  is_qt   <- kind == "question_time"

  new_names[is_q]  <- var_name(names(d)[is_q])
  new_names[is_qt] <- time_var_name(str_remove(names(d)[is_qt], "Time$"))
  new_names[kind == "interview_time"] <- "time_interview"

  if (any(is.na(new_names)))
    stop("Wave ", wave, ": no analysis name for ",
         str_c(names(d)[is.na(new_names)], collapse = ", "),
         "\nAdd it to NAME_MAP in R/cleaning/00_functions.R.", call. = FALSE)
  if (anyDuplicated(new_names))
    stop("Wave ", wave, ": duplicate analysis names (",
         str_c(unique(new_names[duplicated(new_names)]), collapse = ", "), ").", call. = FALSE)

  names(d) <- new_names
  d |> mutate(wave = wave, .before = 1)
}

survey_wide <- map2(raw_waves$path, raw_waves$wave, prepare_wave) |>
  list_rbind() |>                       # missing columns become NA automatically
  mutate(resp_uid = str_c(wave, "_", meta_id), .before = 1)

if (anyDuplicated(survey_wide$resp_uid))
  stop("resp_uid is not unique - the same export may have been read twice.", call. = FALSE)

# --- attach the batch information --------------------------------------------
survey_wide <- survey_wide |>
  left_join(versand |> select(wave, target_group, versandrunde, is_softlaunch),
            by = "wave")

# --- value labels ------------------------------------------------------------
# Taken from the reference wave where it defines them, otherwise from whichever
# wave does. Applied to the stacked data so every wave ends up with the same
# levels in the same order.
label_lookup <- read_all_syntax() |>
  filter(!is.na(val_codes)) |>
  mutate(is_ref = wave == REFERENCE_WAVE) |>
  arrange(desc(is_ref)) |>
  distinct(question_id, .keep_all = TRUE) |>
  mutate(variable_name = var_name(question_id)) |>
  filter(variable_name %in% names(survey_wide))

# "-oth-" is what LimeSurvey stores when someone picks "Sonstiges". It is not
# one of the answer codes, so it would label to NA and look like a nonresponse.
# For the vignette recommendations it is the pre-registered 7th option, so it
# gets a level of its own; Q6 and Q22 are recovered from their free text in
# 03_recode.R instead.
VIG_OTHER_LABEL <- "Keine Empfehlung"

apply_labels <- function(x, codes, labels, add_other) {
  lv <- split_levels(codes); lb <- split_levels(labels)
  if (!length(lv) || length(lv) != length(lb)) return(x)
  if (add_other) { lv <- c(lv, "-oth-"); lb <- c(lb, VIG_OTHER_LABEL) }
  factor(x, levels = lv, labels = lb)
}

for (i in seq_len(nrow(label_lookup))) {
  v <- label_lookup$variable_name[i]
  survey_wide[[v]] <- apply_labels(
    survey_wide[[v]], label_lookup$val_codes[i], label_lookup$val_labels[i],
    add_other = str_detect(v, "^vig[0-9]_rec_(fw|nofw)$")
  )
}

# --- soft-launch harmonisation ------------------------------------------------
# Q7 (customer ranking) was a 6-rank question with an extra "weiss nicht" option
# in the soft launch and a 5-rank question without it in the main waves. The
# extra option has no counterpart, so it becomes NA; cust_rank6 stays as a
# soft-launch-only column rather than being dropped.
harmonisation_log <- tibble(variable = character(), action = character(),
                            detail = character())

for (v in str_subset(names(survey_wide), "^cust_rank")) {
  hit <- !is.na(survey_wide[[v]]) & as.character(survey_wide[[v]]) == "weiß nicht"
  if (any(hit)) {
    survey_wide[[v]][hit] <- NA
    survey_wide[[v]] <- droplevels(survey_wide[[v]])
    harmonisation_log <- add_row(harmonisation_log, variable = v, action = "recoded to NA",
      detail = str_c(sum(hit), " soft-launch answers of 'weiß nicht' set to NA; ",
                     "the option did not exist in the main waves."))
  }
}
if ("cust_rank6" %in% names(survey_wide))
  harmonisation_log <- add_row(harmonisation_log, variable = "cust_rank6",
    action = "soft-launch only",
    detail = "Rank 6 existed only in wave 334335; NA elsewhere. Q7 is not a like-for-like ranking across waves.")
if ("cpd_other_text" %in% names(survey_wide))
  harmonisation_log <- add_row(harmonisation_log, variable = "cpd_other_text",
    action = "missing by design",
    detail = "Q24a did not exist in either soft-launch wave; NA there.")
harmonisation_log <- add_row(harmonisation_log, variable = "firm_radius",
  action = "flagged, not changed",
  detail = "'bundesweit' was not offered in the soft launch. Shared options mean the same thing; filter on is_softlaunch before comparing distributions.")

# --- numeric columns ----------------------------------------------------------
# Whatever the syntax files declare numeric, minus the postcode: LimeSurvey has
# already dropped its leading zero, and 03_recode.R repairs it from the text.
numeric_vars <- read_all_syntax() |>
  filter(wave == REFERENCE_WAVE, is_numeric, is.na(val_codes)) |>
  mutate(variable_name = var_name(question_id)) |>
  filter(variable_name %in% names(survey_wide), variable_name != "firm_zip") |>
  pull(variable_name)

survey_wide <- survey_wide |>
  mutate(across(all_of(numeric_vars), ~ suppressWarnings(as.numeric(.x)))) |>
  mutate(across(starts_with("time_"),      ~ suppressWarnings(as.numeric(.x)))) |>
  mutate(across(starts_with("grouptime_"), ~ suppressWarnings(as.numeric(.x)))) |>
  mutate(across(c(meta_submitdate, meta_startdate, meta_datestamp),
                ~ suppressWarnings(ymd_hms(.x, quiet = TRUE))))

message("Stacked ", nrow(survey_wide), " respondents x ", ncol(survey_wide), " columns")
