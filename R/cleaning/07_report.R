################################################################################
# 07_report.R — what the cleaning did, one table per topic
# Inputs : the objects of 06_experts_vignettes.R, and of 05_main.R if the raw
#          stage ran; the codebook (CODEBOOK_FILE)
# Outputs: REPORT_DIR/cleaning_report.xlsx, one sheet per topic
#          REPORT_DIR/cleaning_report.rds, the same tables as a named list, read
#          by section_cleaning()
# Checks : none; step 06 checks the data, tests/selftest.R the N of the overview
################################################################################
# A row says what a rule did: how many answers it saw, changed, set to missing.
# Aggregates only (docs/data_contract.md): a count of what a rule did may be
# small, every other statistic rests on at least MIN_N answers, and no minimum
# or maximum of individual values appears. So the limits of the trimmed tails are
# not shown: with few answers they are the most extreme answers themselves.

# 0. Helpers -------------------------------------------------------------------

# The rules apply to everyone; the reference values rest on those who completed
# with a group
report_samples <- list(
  "All persons" = rep(TRUE, nrow(experts)),
  "Completed, with an expert group" = in_reference
)

#' The mean of the answers, NA below the minimum cell
mean_min_cell <- function(values) {
  if (sum(!is.na(values)) < MIN_N) {
    return(NA_real_)
  }
  mean(values, na.rm = TRUE)
}

# 1. Overview: from the rows of survey_main to the experts with a group --------
# The arm of every questionnaire, not only of those that stay
questionnaire_arms <- survey_main %>%
  left_join(expert_arms %>%
              transmute(resp_uid, arm_of_row = as.character(vig_arm)),
            by = "resp_uid", relationship = "one-to-one") %>%
  transmute(resp_uid,
            arm_type = case_when(
              arm_conflict ~ "both",
              is.na(arm_of_row) ~ "unknown",
              TRUE ~ arm_of_row
            ))
is_further_attempt <- !(questionnaire_arms$resp_uid %in% experts$resp_uid)
# match(): the position of each resp_uid in the other table
person_arms <- questionnaire_arms$arm_type[
  match(experts$resp_uid, questionnaire_arms$resp_uid)
]
# coalesce(): the first value that is not NA
vignette_arms <- coalesce(as.character(vignettes$vig_arm), "unknown")
is_recommendation <- !is.na(vignettes$vig_rec)
is_reference_vignette <- vignettes$is_complete & !is.na(vignettes$expert_group)

#' One row of the overview
count_by_arm <- function(unit, step, arm_types, note) {
  tibble(unit = unit, step = step, all = length(arm_types),
         arm_fw = sum(arm_types == "fw"),
         arm_nofw = sum(arm_types == "nofw"),
         arm_unknown = sum(arm_types == "unknown"),
         arm_both = sum(arm_types == "both"),
         note = note)
}

overview <- bind_rows(
  count_by_arm("Questionnaires", "Started", questionnaire_arms$arm_type,
               "Rows of survey_main. arm_both: answered in both arms"),
  count_by_arm("Questionnaires", "Further attempts, left out",
               questionnaire_arms$arm_type[is_further_attempt],
               "Under the token of a person, and not the furthest attempt"),
  count_by_arm("Persons", "Furthest attempt of each invitation", person_arms,
               "Rows of experts: completed first, then the highest page"),
  count_by_arm("Persons", "Answered a vignette",
               person_arms[experts$reached_vignettes], "At least one"),
  count_by_arm("Persons", "Completed", person_arms[experts$is_complete],
               "Submitted the questionnaire"),
  count_by_arm("Persons", "Completed, with an expert group",
               person_arms[in_reference],
               "The experts of the reference values"),
  count_by_arm("Vignettes", "Six rows per person", vignette_arms,
               "Rows of vignettes: no further attempts, no expert in both arms"),
  count_by_arm("Vignettes", "Answered", vignette_arms[is_recommendation],
               "With a recommendation (vig_rec)"),
  count_by_arm("Vignettes", "Answered, completed with a group",
               vignette_arms[is_recommendation & is_reference_vignette],
               "With a recommendation, of those who completed with a group")
)

# How the arm of a questionnaire is known
arms_sheet <- vignettes_long %>%
  mutate(vig_arm = as.character(vig_arm)) %>%
  distinct(resp_uid, vig_arm, arm_source, arm_conflict) %>%
  mutate(is_person = resp_uid %in% experts$resp_uid) %>%
  group_by(vig_arm, arm_source, arm_conflict) %>%
  summarise(questionnaires = n(), persons = sum(is_person)) %>%
  ungroup()

# 2. Waves ---------------------------------------------------------------------
persons_by_wave <- experts %>%
  group_by(wave) %>%
  summarise(persons = n(),
            answered_a_vignette = sum(reached_vignettes),
            completed = sum(is_complete),
            completed_with_group = sum(is_complete & !is.na(expert_group)))
# desc(): from the largest down, here the soft launch first
waves <- survey_main %>%
  group_by(wave, versandrunde, target_group, is_softlaunch) %>%
  summarise(started = n()) %>%
  ungroup() %>%
  left_join(persons_by_wave, by = "wave", relationship = "one-to-one") %>%
  arrange(desc(is_softlaunch), wave)

# 3. Attempts ------------------------------------------------------------------
# Attempts equal on completion and page: how far they went does not decide which
# one stays, and that decides which answers the person has
equal_attempts <- attempts %>%
  count(wave, person, is_complete, meta_lastpage, name = "n_equal") %>%
  filter(n_equal > 1) %>%
  inner_join(experts %>% select(wave, person, is_complete, meta_lastpage),
             by = c("wave", "person", "is_complete", "meta_lastpage"),
             relationship = "one-to-one")
# The arm is between persons, and only the attempt that stays counts: how many
# saw both arms over their attempts?
persons_in_two_arms <- attempts %>%
  left_join(questionnaire_arms, by = "resp_uid", relationship = "one-to-one") %>%
  filter(arm_type %in% c("fw", "nofw")) %>%
  distinct(wave, person, arm_type) %>%
  count(wave, person, name = "n_arms") %>%
  filter(n_arms > 1)
attempts_sheet <- bind_rows(
  attempts_report,
  tribble(
    ~step,                                                       ~n,
    "Questionnaires without a token (each a person of its own)", sum(is.na(survey_main$meta_token)),
    "Persons with one attempt",                                  sum(experts$n_attempts == 1),
    "Persons with two attempts",                                 sum(experts$n_attempts == 2),
    "Persons with three or more attempts",                       sum(experts$n_attempts >= 3),
    "Persons whose furthest attempt is tied with another",       nrow(equal_attempts),
    "  of which completed: the one started first stays",         sum(equal_attempts$is_complete),
    "Persons whose attempts fell in different arms",             nrow(persons_in_two_arms)
  )
)

# 4. Expert groups -------------------------------------------------------------
# Q2 allows several ticks; the rule is in section 2 of 06_experts_vignettes.R
is_q2_answered <- !is.na(experts$firm_cat_hvac)
n_ticks <- rep(0, nrow(experts))
tick_pattern <- rep("", nrow(experts))
for (column in names(FIRM_CATEGORY_LABELS)) {
  is_ticked <- coalesce(experts[[column]] == "Ja", FALSE)
  n_ticks <- n_ticks + is_ticked
  tick_pattern[is_ticked] <- paste0(tick_pattern[is_ticked], " + ",
                                    FIRM_CATEGORY_LABELS[[column]])
}
is_other_text <- !is.na(experts$firm_cat_other)
n_ticks <- n_ticks + is_other_text
tick_pattern[is_other_text] <- paste0(tick_pattern[is_other_text],
                                      " + Other (free text)")
# str_remove(): the text without this pattern, here the first " + "
tick_pattern <- str_remove(tick_pattern, "^ \\+ ")
assigned_group <- coalesce(as.character(experts$expert_group), "No group")

#' One row of the sheet expert_groups
count_group_row <- function(part, category, group, is_in, note = "") {
  tibble(part = part, category = category, assigned_group = group,
         persons = sum(is_in), completed = sum(is_in & experts$is_complete),
         note = note)
}

group_rules <- c(
  "HVAC installers" = "Ticked sanitary, heating, air conditioning",
  "Chimney sweeps" = "Ticked chimney sweep, but not HVAC",
  "Energy advisers" = "Ticked energy advice or engineering, or wrote in other; neither trade"
)
expert_groups <- tibble()
for (group_name in EXPERT_GROUPS) {
  expert_groups <- bind_rows(expert_groups, count_group_row(
    "Expert group", group_name, group_name, assigned_group == group_name,
    group_rules[[group_name]]
  ))
}
expert_groups <- bind_rows(
  expert_groups,
  count_group_row("No expert group (NA)", "Q2 not answered", "No group",
                  experts$expert_group_note == "Q2 not answered",
                  "The answer for HVAC is blank: the person did not submit the page"),
  count_group_row("No expert group (NA)", "Q2 submitted without any category",
                  "No group", experts$expert_group_note == "no category ticked"),
  count_group_row("Categories in Q2", "One category", NA_character_,
                  is_q2_answered & n_ticks == 1),
  count_group_row("Categories in Q2", "Several categories", NA_character_,
                  is_q2_answered & n_ticks > 1,
                  "The priority above decides the group")
)
for (group_name in EXPERT_GROUPS) {
  expert_groups <- bind_rows(expert_groups, count_group_row(
    "Several categories, by the group assigned", group_name, group_name,
    is_q2_answered & n_ticks > 1 & assigned_group == group_name
  ))
}
# Who ticked what, and where the rule puts them. A rare combination could point
# to one firm: those below the minimum cell are pooled within their group.
rare_combination <- paste0("Combinations of fewer than ", MIN_N, " persons")
category_combinations <- tibble(tick_pattern, assigned_group,
                                is_complete = experts$is_complete) %>%
  filter(is_q2_answered) %>%
  group_by(tick_pattern, assigned_group) %>%
  mutate(n_same = n()) %>%
  ungroup() %>%
  mutate(category = case_when(
    n_same < MIN_N ~ rare_combination,
    tick_pattern == "" ~ "None",
    TRUE ~ tick_pattern
  )) %>%
  group_by(category, assigned_group) %>%
  summarise(persons = n(), completed = sum(is_complete)) %>%
  ungroup() %>%
  arrange(desc(persons)) %>%
  transmute(part = "Every combination of categories in Q2", category,
            assigned_group, persons, completed, note = "")
expert_groups <- bind_rows(expert_groups, category_combinations)

# 5. Prices (Q15) --------------------------------------------------------------
prices <- tibble()
for (fuel_name in PRICE_FUELS) {
  reported <- experts[[paste0("price_", fuel_name)]]
  treatment <- experts[[paste0("price_", fuel_name, "_treatment")]]
  price <- experts[[paste0("price_", fuel_name, "_kwh")]]
  is_this_fuel <- reference_values$prices$fuel == fuel_name
  reference <- reference_values$prices[is_this_fuel, ]

  euro_rule <- paste0("(1) Below ", PRICE_EURO_BELOW, ": euro, times 100. ")
  unit_rule <- ""
  if (fuel_name == "oil") {
    euro_rule <- paste0("(1) Up to ", OIL_EURO_UP_TO, ": euro, times 100. ")
    unit_rule <- paste0("(2) From ", OIL_PER_LITRE[1], " to ", OIL_PER_LITRE[2],
                        ": cent per litre, divided by 10. ")
  }
  if (fuel_name == "pellet") {
    unit_rule <- paste0("(3) From ", PELLETS_PER_KG[1], " to ", PELLETS_PER_KG[2],
                        ": cent per kg, divided by 5. ")
  }
  price_rule <- paste0(
    euro_rule, unit_rule,
    "(4) More than ", PRICE_VALID_FACTOR, " times above or below the median of ",
    "those who completed with an expert group: not usable."
  )

  # No mean of the answers as reported: it mixes the units they were typed in,
  # and one typo can dominate it (Max, 2026-11-09)
  for (sample_name in names(report_samples)) {
    in_sample <- report_samples[[sample_name]]
    treated <- treatment[in_sample]
    prices <- bind_rows(prices, tibble(
      fuel = fuel_name, label = FUEL_LABELS[[fuel_name]], sample = sample_name,
      persons = sum(in_sample),
      blank = sum(treated == "blank"),
      answered = sum(treated != "blank"),
      as_reported = sum(treated == "as reported"),
      euro = sum(treated == "euro"),
      per_litre_or_kg = sum(treated == "per litre or kg"),
      euro_and_per_litre_or_kg = sum(treated == "euro and per litre or kg"),
      not_usable = sum(treated == "not usable"),
      usable = sum(!is.na(price[in_sample])),
      median = reference$median,
      lower = reference$lower,
      upper = reference$upper,
      mean_usable = mean_min_cell(price[in_sample]),
      rule = price_rule
    ))
  }
}

# 6. Expected prices (Q17) -----------------------------------------------------
expectations <- tibble()
for (fuel_name in PRICE_FUELS) {
  answer <- as.character(experts[[paste0("pexp_", fuel_name)]])
  rise <- experts[[paste0("pexp_", fuel_name, "_rise")]]
  for (sample_name in names(report_samples)) {
    in_sample <- report_samples[[sample_name]]
    expectation_row <- tibble(
      fuel = fuel_name, label = FUEL_LABELS[[fuel_name]], sample = sample_name,
      persons = sum(in_sample), blank = sum(is.na(answer[in_sample])),
      answered = sum(!is.na(answer[in_sample]))
    )
    for (answer_level in PEXP_ANSWERS) {
      expectation_row[[EXPECTATION_LABELS[[answer_level]]]] <-
        sum(answer[in_sample] == answer_level, na.rm = TRUE)
    }
    expectation_row$expects_rise <- sum(rise[in_sample] == 1, na.rm = TRUE)
    expectation_row$share_rise <- mean_min_cell(rise[in_sample])
    expectation_row$rule <- paste0(
      "pexp_", fuel_name, "_rise is 1 if the answer is \"",
      paste(PEXP_RISE, collapse = "\" or \""),
      "\", 0 for the others, NA if blank."
    )
    expectations <- bind_rows(expectations, expectation_row)
  }
}

# 7. The heating at home (Q20) -------------------------------------------------
# reference_values$own_heating holds the shares of the second sample. Shares of
# the persons who reached Q20; those who stopped before it are NA.
own_heating_sheet <- tibble()
has_reached_q20 <- !is.na(experts$own_heating)
for (answer_level in names(reference_values$own_heating)) {
  is_answer <- coalesce(experts$own_heating == answer_level, FALSE)
  own_heating_sheet <- bind_rows(own_heating_sheet, tibble(
    answer = answer_level, label = OWN_HEATING_LABELS[[answer_level]],
    persons = sum(is_answer), share_reached = mean(is_answer[has_reached_q20]),
    completed_with_group = sum(is_answer & in_reference),
    share_completed_with_group = reference_values$own_heating[[answer_level]],
    heat_pump_at_home = if_else(answer_level == "Wärmepumpe", 1, 0),
    note = if_else(answer_level == "Keine Angabe", "Q20 left blank", "")
  ))
}
own_heating_sheet <- bind_rows(own_heating_sheet, tibble(
  answer = NA_character_, label = "Stopped before Q20",
  persons = sum(!has_reached_q20), share_reached = NA_real_, completed_with_group = 0,
  share_completed_with_group = NA_real_, heat_pump_at_home = NA_real_,
  note = "own_heating and heat_pump_at_home are NA"
))

# 8. The outlier rule ----------------------------------------------------------
outlier_sheet <- tibble()
for (k in seq_len(nrow(outlier_counts))) {
  variable_name <- outlier_counts$variable[k]
  valid_range <- VALID_RANGE[[variable_name]]
  # is.infinite(): no upper bound
  impossible_text <- paste0("outside ", valid_range[1], " to ", valid_range[2])
  if (is.infinite(valid_range[2])) {
    impossible_text <- paste0("below ", valid_range[1])
  }
  is_trimmed <- variable_name %in% TRIMMED_VARIABLES
  trim_rule <- "Not trimmed."
  if (is_trimmed) {
    trim_rule <- paste0("Then below the ", 100 * TRIM_QUANTILES[1], "% or above ",
                        "the ", 100 * TRIM_QUANTILES[2], "% quantile of the ",
                        "remaining answers of everyone who answered: trimmed.")
  }
  reversed_rule <- ""
  if (variable_name %in% c("co2_est_low", "co2_est_high")) {
    reversed_rule <- paste0(" Then a range whose lower end lies above its upper ",
                            "end: both missing.")
  }
  outlier_sheet <- bind_rows(outlier_sheet, tibble(
    variable = variable_name, label = OUTLIER_LABELS[[variable_name]],
    new_column = paste0(variable_name, "_clean"), impossible_if = impossible_text,
    is_trimmed = is_trimmed,
    answered = outlier_counts$answered[k],
    impossible = outlier_counts$impossible[k],
    trimmed = outlier_counts$trimmed[k], reversed = outlier_counts$reversed[k],
    used = outlier_counts$used[k],
    rule = paste0("Answers ", impossible_text, ": impossible.", reversed_rule,
                  " ", trim_rule)
  ))
}

# 9. The shares of the technologies (Q10b, Q11b) -------------------------------
shares_sheet <- tibble()
for (question in names(share_questions)) {
  question_sums <- share_sums[share_sums$question == question, ]
  # The columns of a question follow the order of SHARE_SUFFIXES
  for (k in seq_along(SHARE_SUFFIXES)) {
    variable_name <- share_questions[[question]][k]
    filled <- experts[[paste0(variable_name, "_filled")]]
    # Answers given, those used, and those of persons whose shares are all 0 or
    # include one outside 0 to 100, which are not used
    n_answered <- sum(!is.na(experts[[variable_name]]))
    n_used <- sum(!is.na(experts[[variable_name]]) & !is.na(filled))
    shares_sheet <- bind_rows(shares_sheet, tibble(
      question = question,
      technology = SHARE_TECHNOLOGY_LABELS[[SHARE_SUFFIXES[k]]],
      new_column = paste0(variable_name, "_filled"),
      persons_gave_shares = question_sums$answered,
      persons_outside_0_100 = question_sums$outside_0_100,
      sum_not_100 = question_sums$not_100,
      answered = n_answered,
      used = n_used,
      set_to_missing = n_answered - n_used,
      blank_set_to_0 = sum(!is.na(filled)) - n_used,
      mean_share = mean_min_cell(filled),
      rule = paste0("For a person who gave shares in the question (a total ",
                    "above 0) and none outside 0 to 100: the share, 0 where ",
                    "blank; NA for the others. Used as reported, also if the ",
                    "shares do not sum to 100.")
    ))
  }
}

# 10. Time and risk preferences ------------------------------------------------
preferences_sheet <- tibble()
for (staircase_name in preference_report$staircase) {
  report_row <- preference_report[preference_report$staircase == staircase_name, ]
  score <- experts[[paste0(staircase_name, "_score")]]
  levels_answered <- experts[[paste0(staircase_name, "_levels")]]
  preference_row <- tibble(
    staircase = staircase_name, label = STAIRCASE_LABELS[[staircase_name]],
    persons = nrow(experts), scored = report_row$scored,
    not_scored = sum(is.na(score)),
    whole_branch = sum(experts[[paste0(staircase_name, "_complete")]]),
    broke_off = report_row$broke_off,
    with_stray_answers = report_row$with_stray_answers
  )
  for (level in 1:5) {
    preference_row[[paste0("levels_", level)]] <- sum(levels_answered == level)
  }
  preference_row$mean_score <- mean_min_cell(score)
  preference_row$note <- paste0(
    "The score is the midpoint of the cell in (0, 1). whole_branch: went as ",
    "deep as the branch allows. with_stray_answers: answers on a branch never ",
    "entered, left by going back in the questionnaire; they are not used."
  )
  preferences_sheet <- bind_rows(preferences_sheet, preference_row)
}

# 11. Geography ----------------------------------------------------------------
#' One row of the sheet geography
count_geography_row <- function(item, is_in, note = "") {
  tibble(item = item, persons = sum(is_in),
         completed = sum(is_in & experts$is_complete), note = note)
}
is_postcode_answered <- !is.na(experts$firm_zip)
is_valid_postcode <- !is.na(experts$plz)
is_with_nuts3 <- !is.na(experts$nuts3)
is_with_hdd <- !is.na(experts$hdd)
# BUNDESLAND_NUTS1 (00_functions.R): the first three characters of the regions
# of a state; NA for a person without a state. unname(): the values without
# their names. str_sub(): a part of the text.
nuts1_of_state <- unname(BUNDESLAND_NUTS1[as.character(experts$bundesland)])
is_lookups_disagree <- coalesce(str_sub(experts$nuts3, 1, 3) != nuts1_of_state,
                                FALSE)
bundesland_source_notes <- c(
  plz = "In the lookup, and the postcode lies in one state only",
  plz_border = "In the lookup; on a state line, in the state with most of it",
  plz_prefix = "Not in the lookup; its first 3 digits lie in one state",
  unmatched = "Five digits, but no such postcode exists: left as NA",
  no_postcode = "Q0 blank or unusable"
)
geography <- bind_rows(
  count_geography_row("Postcode answered (Q0)", is_postcode_answered),
  count_geography_row("Valid postcode (plz)", is_valid_postcode),
  count_geography_row("Answered, not a German postcode",
                      is_postcode_answered & !is_valid_postcode,
                      "Fragments typed into Q0: plz is NA")
)
for (source_name in names(bundesland_source_notes)) {
  geography <- bind_rows(geography, count_geography_row(
    paste0("bundesland_source: ", source_name),
    coalesce(experts$bundesland_source == source_name, FALSE),
    bundesland_source_notes[[source_name]]
  ))
}
geography <- bind_rows(
  geography,
  count_geography_row("With a state (bundesland)", !is.na(experts$bundesland)),
  count_geography_row("With a region (nuts3)", is_with_nuts3),
  count_geography_row("Valid postcode, not in Eurostat's list",
                      is_valid_postcode & !is_with_nuts3,
                      "Mostly P.O.-box or large-customer postcodes: no nuts3"),
  count_geography_row(paste0("With degree days (hdd, ", HDD_YEAR, ")"),
                      is_with_hdd,
                      paste0("Those who completed with an expert group: mean ",
                             round(reference_values$hdd$mean), ", quartiles ",
                             round(reference_values$hdd$q25), " and ",
                             round(reference_values$hdd$q75), ", n = ",
                             reference_values$hdd$n)),
  count_geography_row("Without degree days", !is_with_hdd,
                      "Exactly those without nuts3"),
  count_geography_row("Lookups disagree: nuts3 lies outside bundesland",
                      is_lookups_disagree,
                      "nuts3: Eurostat. bundesland: GeoNames and BKG")
)

# 12. The rules of the cleaning, side by side ----------------------------------
# One row per rule: how many answers it saw, how many it changed, and how many
# it set to missing or left out
rules <- bind_rows(
  tibble(rule = "One row per person", new_column = "(rows of experts)",
         answers_seen = nrow(survey_main), changed = 0,
         set_to_missing = sum(is_further_attempt),
         note = "Rows left out: the further attempts of an invitation"),
  tibble(rule = "Expert group (Q2)", new_column = "expert_group",
         answers_seen = nrow(experts),
         changed = sum(is_q2_answered & n_ticks > 1),
         set_to_missing = sum(is.na(experts$expert_group)),
         note = "Changed: several categories, one group. Missing: no group"),
  tibble(rule = "Arm of the vignettes", new_column = "arm",
         answers_seen = nrow(experts), changed = 0,
         set_to_missing = sum(is.na(experts$arm)),
         note = "Missing: arm unknown, or answered in both arms"),
  tibble(rule = "Vignettes of the persons", new_column = "(rows of vignettes)",
         answers_seen = nrow(vignettes_long), changed = 0,
         set_to_missing = nrow(vignettes_long) - nrow(vignettes),
         note = "Rows left out: further attempts, and an expert in both arms"),
  prices %>%
    filter(sample == names(report_samples)[1]) %>%
    transmute(rule = "Price per kWh (Q15)",
              new_column = paste0("price_", fuel, "_kwh"),
              answers_seen = answered,
              changed = euro + per_litre_or_kg + euro_and_per_litre_or_kg,
              set_to_missing = not_usable,
              note = "Changed: read as euro, per litre or kg. Missing: not usable"),
  expectations %>%
    filter(sample == names(report_samples)[1]) %>%
    transmute(rule = "Expected rise (Q17)",
              new_column = paste0("pexp_", fuel, "_rise"),
              answers_seen = answered, changed = 0, set_to_missing = 0,
              note = "Recoded to 0 or 1; a blank stays NA"),
  tibble(rule = "Heating at home (Q20)", new_column = "own_heating",
         answers_seen = sum(!is.na(experts$dem_own_heating)),
         changed = sum(is.na(experts$dem_own_heating) & has_reached_q20),
         set_to_missing = sum(!has_reached_q20),
         note = paste0("Changed: a blank becomes \"Keine Angabe\". Missing: ",
                       "stopped before Q20")),
  tibble(rule = "Degree days", new_column = "hdd",
         answers_seen = nrow(experts), changed = 0,
         set_to_missing = sum(!is_with_hdd),
         note = "Added from Eurostat; NA without nuts3"),
  preferences_sheet %>%
    transmute(rule = "Preference score",
              new_column = paste0(staircase, "_score"),
              answers_seen = scored, changed = 0, set_to_missing = 0,
              note = "From the answers on the expert's own branch"),
  outlier_sheet %>%
    transmute(rule = "Outlier rule", new_column,
              answers_seen = answered, changed = 0,
              set_to_missing = impossible + trimmed + reversed,
              note = paste0("Missing: impossible, in the ", 100 * TRIM_QUANTILES[1],
                            "% tails, or a reversed range")),
  shares_sheet %>%
    group_by(question) %>%
    summarise(answers_seen = sum(answered), changed = sum(blank_set_to_0),
              set_to_missing = sum(set_to_missing)) %>%
    ungroup() %>%
    transmute(rule = paste0("Shares of the technologies, ", question),
              new_column = if_else(question == names(share_questions)[1],
                                   "adv_share_<technology>_filled",
                                   "mix25_<technology>_filled"),
              answers_seen, changed, set_to_missing,
              note = paste0("Answers, not persons. Changed: a blank becomes 0, ",
                            "not among the answers seen. Missing: the answers of ",
                            "a person whose shares are all 0 or include one ",
                            "outside 0 to 100"))
)

# 13. The settings of this run, for section_cleaning() and the reader ----------
settings <- tribble(
  ~setting,             ~value,                                               ~meaning,
  "date",               as.character(Sys.Date()),                             "The day of this run",
  "data",               if_else(USE_SYNTHETIC_DATA, "synthetic", "real"),     "What this run ran on",
  "price_euro_below",   as.character(PRICE_EURO_BELOW),                       "A price below this is read as euro",
  "oil_euro_up_to",     as.character(OIL_EURO_UP_TO),                         "An oil price up to this is read as euro per litre",
  "oil_per_litre",      paste0(OIL_PER_LITRE[1], " to ", OIL_PER_LITRE[2]),   "An oil price in this band is read as cent per litre",
  "pellets_per_kg",     paste0(PELLETS_PER_KG[1], " to ", PELLETS_PER_KG[2]), "A pellet price in this band is read as cent per kg",
  "price_valid_factor", as.character(PRICE_VALID_FACTOR),                     "A price this many times above or below the median is not usable",
  "trim_lower_percent", as.character(100 * TRIM_QUANTILES[1]),                "Answers below this quantile of the valid ones are trimmed, for the variables trimmed",
  "trim_upper_percent", as.character(100 * TRIM_QUANTILES[2]),                "Answers above this quantile of the valid ones are trimmed, for the variables trimmed",
  "survey_year",        as.character(SURVEY_YEAR),                            "Age is this year minus the year of birth",
  "hdd_year",           as.character(HDD_YEAR),                               "The year of the heating degree days",
  "min_n",              as.character(MIN_N),                                  "The fewest answers behind a statistic"
)

# 14. Variables: a data dictionary ---------------------------------------------
# The question text comes from the codebook of the raw stage. A synthetic run
# has no raw stage, so it reads the codebook of the last real run.
codebook_file <- CODEBOOK_FILE
if (USE_SYNTHETIC_DATA) {
  codebook_file <- file.path(DROPBOX_DIR, "output_dropbox", "reports_cleaning",
                             "codebook.csv")
}
codebook <- tibble(variable_name = character(), question_id = character(),
                   question_text = character())
if (file.exists(codebook_file)) {
  codebook <- read_csv(codebook_file, show_col_types = FALSE)
} else {
  message("No codebook at ", codebook_file, ": no question texts in the report")
}

# What step 06 adds, by the definitions of docs/data_contract.md; and LimeSurvey's
# own fields, whose "question text" is only their name
derived_columns <- tribble(
  ~column,                  ~definition,
  "meta_id",                "LimeSurvey's number of the questionnaire within the wave",
  "meta_submitdate",        "When the questionnaire was submitted; NA if it was not",
  "meta_lastpage",          "The last page LimeSurvey counted; pages hidden by routing count too, so it is no question",
  "meta_startlanguage",     "Language of the questionnaire",
  "meta_seed",              "LimeSurvey's random seed of the questionnaire",
  "meta_token",             "Token of the invitation; per survey, so one person invited in two waves has two",
  "meta_startdate",         "When the questionnaire was started",
  "meta_datestamp",         "When the questionnaire was last saved",
  "person",                 "Token of the invitation, or resp_uid without one: identifies a person within a wave",
  "n_attempts",             "Questionnaires started under this invitation",
  "expert_group",           "From Q2, by priority: HVAC installers if ticked, then chimney sweeps, then energy advisers (energy advice, engineering or free text); NA if none can be assigned",
  "expert_group_note",      "Why the group is what it is: assigned, Q2 not answered, no category ticked",
  "arm",                    "Arm of the vignette experiment: fw, with the district-heating option, or nofw; NA if unknown or answered in both",
  "vig_arm",                "In experts: the arm observed from the answers, NA without an answered vignette (use arm). In vignettes: observed or routed from Q6a, see arm_source",
  "arm_source",             "How the arm is known: answered, routed from Q6a, unknown",
  "last_question",          "The last question answered, in English; \"No question answered\" if no page was submitted",
  "last_section",           "The section of the last question answered, in English; NA if no page was submitted",
  "last_question_position", "Position of the last question answered in QUESTIONNAIRE; 0 if no page was submitted",
  "own_heating",            "Answer of Q20, \"Keine Angabe\" if left blank; NA for those who stopped before Q20",
  "heat_pump_at_home",      "1 if a heat pump heats the expert's home, else 0; \"Sonstiges\" and no answer count as no heat pump; NA for those who stopped before Q20",
  "hdd",                    paste0("Heating degree days of the expert's NUTS3 region in ", HDD_YEAR, " (Eurostat); NA without nuts3"),
  "age",                    paste0(SURVEY_YEAR, " minus dem_birthyear_clean"),
  "interview_minutes",      "LimeSurvey's total time in minutes; for a break-off, the time until the break-off. Not trimmed: use medians and quartiles",
  "vig_group_cost_time",    "Time on the page of the cost question of the last vignette, in seconds"
)
for (fuel_name in PRICE_FUELS) {
  fuel_label <- FUEL_LABELS[[fuel_name]]
  derived_columns <- bind_rows(derived_columns, tribble(
    ~column,                                       ~definition,
    paste0("price_", fuel_name, "_kwh"),           paste0(fuel_label, ": reported price in cent per kWh after the unit rules; NA if blank or not usable"),
    paste0("price_", fuel_name, "_treatment"),     paste0(fuel_label, ": what happened to the answer: blank, as reported, euro, per litre or kg, euro and per litre or kg, not usable"),
    paste0("pexp_", fuel_name, "_rise"),           paste0(fuel_label, ": 1 if the expert expects the price to rise, 0 if not, NA if blank")
  ))
}
for (staircase_name in names(STAIRCASE_LABELS)) {
  staircase_label <- STAIRCASE_LABELS[[staircase_name]]
  derived_columns <- bind_rows(derived_columns, tribble(
    ~column,                                ~definition,
    paste0(staircase_name, "_score"),       paste0(staircase_label, ": staircase score, the midpoint of the cell in (0, 1); z-scores depend on the sample, so compute them in the script"),
    paste0(staircase_name, "_cell"),        paste0(staircase_label, ": the cell, 0 to 2^levels - 1, from least to most"),
    paste0(staircase_name, "_levels"),      paste0(staircase_label, ": how many of the five choices were answered on the expert's own branch"),
    paste0(staircase_name, "_complete"),    paste0(staircase_label, ": TRUE if the expert went as deep as the branch allows")
  ))
}
for (k in seq_len(nrow(outlier_sheet))) {
  derived_columns <- bind_rows(derived_columns, tibble(
    column = outlier_sheet$new_column[k],
    definition = paste0(outlier_sheet$label[k], ": the answer after the outlier ",
                        "rule. ", outlier_sheet$rule[k])
  ))
}
for (question in names(share_questions)) {
  derived_columns <- bind_rows(derived_columns, tibble(
    column = paste0(share_questions[[question]], "_filled"),
    definition = paste0(question, ": the share for an expert who gave shares, ",
                        "0 where left blank; NA for the others, and for an ",
                        "expert with a share outside 0 to 100")
  ))
}

variables <- tibble()
data_sets <- list(experts = experts, vignettes = vignettes)
for (data_name in names(data_sets)) {
  for (column in names(data_sets[[data_name]])) {
    values <- data_sets[[data_name]][[column]]
    variables <- bind_rows(variables, tibble(
      data_set = data_name, column = column, type = class(values)[1],
      n_answered = sum(!is.na(values))
    ))
  }
}
# The items of QUESTIONNAIRE are the names of the response times, with one item
# per staircase decision: time_tp_2a and time_tp_2b are both time_tp_2
# (question_times()). str_replace(): the text with this pattern replaced.
# str_starts(): whether the text starts with this.
time_labels <- QUESTIONNAIRE %>%
  transmute(time_item = item, time_label = label)
variables <- variables %>%
  left_join(codebook %>%
              select(column = variable_name, question_id, question_text),
            by = "column", relationship = "many-to-one") %>%
  left_join(derived_columns, by = "column", relationship = "many-to-one") %>%
  mutate(time_item = str_replace(column, "^time_(tp|risk)_([1-5]).*$",
                                 "time_\\1_\\2")) %>%
  left_join(time_labels, by = "time_item", relationship = "many-to-one") %>%
  mutate(description = case_when(
    !is.na(definition) ~ definition,
    !is.na(question_text) ~ question_text,
    !is.na(time_label) ~ paste0("Response time in seconds: ", time_label),
    column == "time_interview" ~ "LimeSurvey's total time, in seconds",
    str_starts(column, "time_") ~ "Response time in seconds, helper question",
    str_starts(column, "grouptime_") ~ "Time in seconds on a group of questions",
    TRUE ~ "Not documented"
  )) %>%
  select(data_set, column, type, n_answered, question_id, description)
n_undocumented <- sum(variables$description == "Not documented")
if (n_undocumented > 0) {
  message(n_undocumented, " columns without a description in the sheet variables")
}

# 15. Write --------------------------------------------------------------------
cleaning_report <- list(
  overview = overview,
  rules = rules,
  settings = settings,
  waves = waves,
  attempts = attempts_sheet,
  arms = arms_sheet,
  expert_groups = expert_groups,
  prices = prices,
  expectations = expectations,
  own_heating = own_heating_sheet,
  outliers = outlier_sheet,
  shares = shares_sheet,
  preferences = preferences_sheet,
  geography = geography
)
# exists(): whether the raw stage made this table in this run. A synthetic run
# has none of them.
if (exists("raw_checks")) {
  cleaning_report$raw_checks <- raw_checks
}
if (exists("harmonisation_log")) {
  cleaning_report$harmonisation <- harmonisation_log
}
if (exists("recode_summary")) {
  cleaning_report$recodes <- recode_summary
}
if (exists("data_quality")) {
  cleaning_report$data_quality <- data_quality
}
cleaning_report$variables <- variables

write_xlsx(cleaning_report, file.path(REPORT_DIR, "cleaning_report.xlsx"))
saveRDS(cleaning_report, file.path(REPORT_DIR, "cleaning_report.rds"))
message("Cleaning report: ", length(cleaning_report), " sheets, ",
        file.path(REPORT_DIR, "cleaning_report.xlsx"))
