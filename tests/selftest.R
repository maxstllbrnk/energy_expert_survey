################################################################################
# selftest.R — checks on the cleaned data sets, after R/run_cleaning.R
# Inputs : survey_main.rds, vignettes_long.rds, experts.rds, vignettes.rds,
#          reference_values.rds (CLEAN_DATA_DIR); cleaning_report.rds; in a real
#          run also the raw exports
# Outputs: one line per check in the log; exit status 1 if a check fails
# Checks : waves, keys, joins, six rows per person, the arms, the design, the
#          alignment of the soft launch, the staircases, the rules of the heating
#          at home, the shares and the outliers, the N of the cleaning report
################################################################################
# From the project root: Rscript tests/selftest.R, or source() it in RStudio.
#
# These are the checks that fail if the soft-launch waves are merged wrongly, the
# failure of the first pipeline. The strongest is the vignette_id: it only
# resolves if all nine design attributes landed in the right columns, which
# cannot happen by chance if columns are misaligned.
#
# It also runs on synthetic data, without what only the raw stage can have: the
# raw exports, the amounts in the labels of the staircases, the harmonisation of
# Q7 and Q24a.

source(file.path("R", "setup.R"))
source(file.path("R", "cleaning", "00_functions.R"), encoding = "UTF-8")

survey_main <- readRDS(file.path(CLEAN_DATA_DIR, "survey_main.rds"))
vignettes_long <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes_long.rds"))
experts <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds"))
vignettes <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds"))
reference_values <- readRDS(file.path(CLEAN_DATA_DIR, "reference_values.rds"))

checks <- tibble()

#' Adds one check to the list
#'
#' @param checks the list so far
#' @param label  what is checked
#' @param passed TRUE if it holds
#' @param detail what a reader needs to see, for example a count
add_check <- function(checks, label, passed, detail = "") {
  bind_rows(checks, tibble(check = label, passed = isTRUE(passed), detail = detail))
}

# 1. Waves ---------------------------------------------------------------------
checks <- add_check(checks, "12 waves", n_distinct(survey_main$wave) == 12,
                    paste(n_distinct(survey_main$wave), "waves"))
checks <- add_check(checks, "two soft-launch waves",
                    n_distinct(survey_main$wave[survey_main$is_softlaunch]) == 2)
checks <- add_check(checks, "the soft launch follows the Versandrunde",
                    all(survey_main$is_softlaunch ==
                          (survey_main$versandrunde == "Soft Launch")))

# 2. Keys and joins ------------------------------------------------------------
# anyDuplicated(): the position of the first repeated value, 0 if there is none
checks <- add_check(checks, "resp_uid unique in survey_main",
                    !anyDuplicated(survey_main$resp_uid))
checks <- add_check(checks, "resp_uid unique in experts",
                    !anyDuplicated(experts$resp_uid))
checks <- add_check(checks, "resp_uid x vig_num unique in vignettes",
                    !anyDuplicated(vignettes[c("resp_uid", "vig_num")]))
checks <- add_check(checks, "every row of vignettes_long joins to survey_main",
                    all(vignettes_long$resp_uid %in% survey_main$resp_uid))
checks <- add_check(checks, "every questionnaire has rows in vignettes_long",
                    all(survey_main$resp_uid %in% vignettes_long$resp_uid))
checks <- add_check(checks, "every row of vignettes joins to experts",
                    all(vignettes$resp_uid %in% experts$resp_uid))

rows_per_person <- vignettes %>%
  count(resp_uid)
checks <- add_check(checks, "six rows per person in vignettes",
                    all(rows_per_person$n == N_VIGNETTES))
is_without_vignettes <- !(experts$resp_uid %in% vignettes$resp_uid)
# identical(): whether two objects are exactly the same
checks <- add_check(checks, "only an expert in both arms has no vignettes",
                    identical(is_without_vignettes, experts$arm_conflict),
                    paste(sum(is_without_vignettes), "without vignettes"))
checks <- add_check(checks, "no completer has no vignettes",
                    !any(is_without_vignettes & experts$is_complete))

# The furthest attempt is read off the page counter: as text, 9 would beat 10
checks <- add_check(checks, "meta_lastpage is a number",
                    is.numeric(survey_main$meta_lastpage))
# coalesce(): the first value that is not NA
n_completed_persons <- survey_main %>%
  filter(is_complete) %>%
  mutate(person = coalesce(meta_token, resp_uid)) %>%
  distinct(wave, person) %>%
  nrow()
checks <- add_check(checks, "everyone who ever completed counts as a completer",
                    sum(experts$is_complete) == n_completed_persons,
                    paste(n_completed_persons, "persons completed"))

# 3. The arms ------------------------------------------------------------------
# District heating is an option in one arm only: an answer in the other would
# mean that the two branches of the questionnaire were mixed up
is_nofw <- coalesce(vignettes$vig_arm == "nofw", FALSE)
checks <- add_check(checks, "Fernwärme is never recommended in the arm without it",
                    !any(vignettes$vig_rec[is_nofw] == "Fernwärme", na.rm = TRUE))

# 4. The design and the alignment of the soft launch ---------------------------
# The two soft-launch waves have other columns than the main waves (the old
# pipeline's failure), so that each wave group has to resolve on its own
is_soft_row <- vignettes_long$is_softlaunch
checks <- add_check(checks, "every vignette row matches a design, soft launch",
                    all(!is.na(vignettes_long$vignette_id[is_soft_row])))
checks <- add_check(checks, "every vignette row matches a design, main waves",
                    all(!is.na(vignettes_long$vignette_id[!is_soft_row])))
checks <- add_check(checks, "all 128 design combinations appear",
                    n_distinct(vignettes$vignette_id) == 128,
                    paste(n_distinct(vignettes$vignette_id), "combinations"))
is_soft_person <- survey_main$is_softlaunch
for (variable in c("dem_own_heating", "dem_education", "co2_heard",
                   "resp_is_owner")) {
  answers_soft <- unique(as.character(survey_main[[variable]][is_soft_person]))
  answers_main <- unique(as.character(survey_main[[variable]][!is_soft_person]))
  checks <- add_check(checks,
                      paste0("the soft launch has no other answers: ", variable),
                      all(answers_soft %in% answers_main))
}
build_years <- as.numeric(vignettes_long$att_build_year)
checks <- add_check(checks, "design attributes have one range in both wave groups",
                    identical(range(build_years[is_soft_row], na.rm = TRUE),
                              range(build_years[!is_soft_row], na.rm = TRUE)))

# Only the raw stage has these: the exports, and the differences of the soft launch
if (!USE_SYNTHETIC_DATA) {
  checks <- add_check(checks, "cpd_other_text (Q24a) is NA in the soft launch",
                      all(is.na(survey_main$cpd_other_text[is_soft_person])))
  is_other_wave <- survey_main$wave != "334335"
  checks <- add_check(checks, "cust_rank6 exists only in wave 334335",
                      "cust_rank6" %in% names(survey_main) &&
                        all(is.na(survey_main$cust_rank6[is_other_wave])))
  # Rows of the exports by parsing, not lines: free text holds line breaks
  raw_files <- list.files(RAW_DIR, "^survey_[0-9]+_R_data_file\\.csv$",
                          recursive = TRUE, full.names = TRUE)
  n_raw_rows <- 0
  for (raw_file in raw_files) {
    n_raw_rows <- n_raw_rows + nrow(read_raw_wave(raw_file)$data)
  }
  checks <- add_check(checks, "survey_main keeps every row of the raw exports",
                      nrow(survey_main) == n_raw_rows,
                      paste(nrow(survey_main), "cleaned,", n_raw_rows, "raw"))
}

# 5. The staircases ------------------------------------------------------------
# What the preference scores rest on. Option A must stay the first level, as the
# score reads the answer off the level: a reversed export would invert the
# measure with nothing looking wrong.
option_a_patterns <- c(tp = "heute", risk = "^Verlosung")
for (prefix in names(option_a_patterns)) {
  # str_subset(): the values that match a pattern. str_remove(): the text without
  # the pattern. str_sub(): a part of the text. str_to_upper(): the text in
  # capitals.
  columns <- str_subset(names(survey_main), paste0("^", prefix, "_[0-9]"))
  stems <- str_remove(columns, paste0("^", prefix, "_"))
  node_levels <- as.integer(str_sub(stems, 1, 1))
  node_paths <- str_to_upper(str_sub(stems, 2))

  is_two_levels <- rep(FALSE, length(columns))
  is_option_a_first <- rep(FALSE, length(columns))
  # The amount on offer is in the label of option B
  amounts <- rep(NA_real_, length(columns))
  # The answers so far as a binary number, A counting as 1
  path_codes <- rep(0L, length(columns))
  for (k in seq_along(columns)) {
    answer_levels <- levels(survey_main[[columns[k]]])
    is_two_levels[k] <- length(answer_levels) == 2
    # str_detect(): whether the text matches. str_extract(): the part that does.
    # str_split_1(): the letters of one text.
    is_option_a_first[k] <- str_detect(answer_levels[1],
                                       option_a_patterns[[prefix]])
    amounts[k] <- as.numeric(str_extract(answer_levels[2], "[0-9]+"))
    for (letter in str_split_1(node_paths[k], "")) {
      path_codes[k] <- path_codes[k] * 2L + as.integer(letter == "A")
    }
  }
  checks <- add_check(checks, paste0(prefix, ": every node is a two-level factor"),
                      all(is_two_levels))
  checks <- add_check(checks, paste0(prefix, ": option A is the first level"),
                      all(is_option_a_first))

  # The staircase only orders experts if the amount rises with the number of "A"
  # answers so far, at every level. The labels of the synthetic data hold no
  # amounts.
  if (!USE_SYNTHETIC_DATA) {
    is_monotone <- TRUE
    # setdiff(): the values of the first that are not in the second
    for (level in setdiff(unique(node_levels), 1L)) {
      at_level <- node_levels == level
      # cor(): the rank correlation of the two, 1 if the amounts rise throughout
      correlation <- cor(path_codes[at_level], amounts[at_level],
                         method = "spearman")
      is_monotone <- is_monotone && abs(correlation - 1) < 1e-8
    }
    checks <- add_check(checks, paste0(prefix, ": offers rise along the branch"),
                        is_monotone)
  }

  # Nobody may answer two branches of one level: the path would be undefined
  is_one_per_level <- TRUE
  for (level in unique(node_levels)) {
    answers_at_level <- rowSums(!is.na(survey_main[columns[node_levels == level]]))
    is_one_per_level <- is_one_per_level && all(answers_at_level <= 1)
  }
  checks <- add_check(checks, paste0(prefix, ": one answer per level at most"),
                      is_one_per_level)

  score <- experts[[paste0(prefix, "_score")]]
  checks <- add_check(checks, paste0(prefix, ": the score lies in (0, 1)"),
                      all(score > 0 & score < 1, na.rm = TRUE))
}
level_5_paths <- c("aaaa", "aaab", "aaba", "aabb", "abaa", "abab", "abba", "abbb",
                   "baaa", "baab", "baba", "babb", "bbaa", "bbab", "bbba", "bbbb")
checks <- add_check(
  checks, "the time staircase lacks exactly its two known level-5 nodes",
  identical(setdiff(paste0("tp_5", level_5_paths), names(survey_main)),
            c("tp_5abba", "tp_5abbb"))
)
checks <- add_check(checks, "the risk staircase has all 16 level-5 nodes",
                    all(paste0("risk_5", level_5_paths) %in% names(survey_main)))

# 6. The rules of 2026-11-09 hold in the data -----------------------------------
q20_position <- QUESTIONNAIRE$position[QUESTIONNAIRE$item == "time_dem_own_heating"]
has_reached_q20 <- experts$is_complete |
  experts$last_question_position >= q20_position | !is.na(experts$dem_own_heating)
checks <- add_check(
  checks, "own_heating is NA exactly for those who stopped before Q20",
  identical(is.na(experts$own_heating), !has_reached_q20) &&
    identical(is.na(experts$heat_pump_at_home), !has_reached_q20),
  paste(sum(!has_reached_q20), "stopped before Q20")
)
# str_subset(): the names that match
filled_shares <- experts[str_subset(names(experts), "_filled$")]
checks <- add_check(checks, "no filled share outside 0 to 100",
                    all(filled_shares >= 0 & filled_shares <= 100, na.rm = TRUE))
clean_counts <- experts[c("firm_employees_clean", "adv_households_clean",
                          "co2_est_low_clean", "co2_est_high_clean")]
checks <- add_check(checks, "no negative count or carbon price after cleaning",
                    all(clean_counts >= 0, na.rm = TRUE))
checks <- add_check(checks, "no carbon price range with its ends reversed",
                    !any(experts$co2_est_low_clean > experts$co2_est_high_clean,
                         na.rm = TRUE))

# 7. The cleaning report -------------------------------------------------------
# An unknown state name would silently lower the count of the lookups that
# disagree
checks <- add_check(checks, "every state has its NUTS1 code",
                    all(levels(survey_main$bundesland) %in% names(BUNDESLAND_NUTS1)))
# The sample table has to end at the N of the reference values and the data sets
report_file <- file.path(REPORT_DIR, "cleaning_report.rds")
checks <- add_check(checks, "the cleaning report exists", file.exists(report_file))
if (file.exists(report_file)) {
  report <- readRDS(report_file)
  overview <- report$overview
  n_in_overview <- overview$all
  names(n_in_overview) <- overview$step
  checks <- add_check(checks, "the overview ends at the N of the reference values",
                      n_in_overview[["Completed, with an expert group"]] ==
                        reference_values$n_experts,
                      paste(reference_values$n_experts, "experts"))
  checks <- add_check(checks, "the overview counts the persons of experts",
                      n_in_overview[["Furthest attempt of each invitation"]] ==
                        nrow(experts))
  checks <- add_check(checks, "the overview counts the rows of vignettes",
                      n_in_overview[["Six rows per person"]] == nrow(vignettes))

  report_tables <- file.path(tempdir(), "tab_clean")
  dir.create(report_tables, showWarnings = FALSE)
  cleaning_exhibits <- section_cleaning(list(tables = report_tables))
  table_files <- file.path(report_tables, basename(cleaning_exhibits$file))
  checks <- add_check(checks, "section_cleaning() writes its four tables",
                      nrow(cleaning_exhibits) == 4 && all(file.exists(table_files)))
}

# 8. Result --------------------------------------------------------------------
for (k in seq_len(nrow(checks))) {
  cat(sprintf("%-60s %s  %s\n", checks$check[k],
              if (checks$passed[k]) "PASS" else "*** FAIL ***", checks$detail[k]))
}
n_failed <- sum(!checks$passed)
result_text <- paste(n_failed, "CHECK(S) FAILED")
if (n_failed == 0) {
  result_text <- "ALL CHECKS PASSED"
}
cat("\n", result_text, " (", nrow(checks), " checks)\n", sep = "")
if (!interactive()) {
  quit(status = if (n_failed == 0) 0 else 1)
}
