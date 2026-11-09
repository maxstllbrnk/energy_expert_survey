################################################################################
# 06_experts_vignettes.R — the cleaned data sets every analysis starts from
# Inputs : survey_main.rds, vignettes_long.rds (CLEAN_DATA_DIR; 05_main.R, or
#          tests/simulate_data.R in a synthetic run); degree days (HDD_FILE)
# Outputs: CLEAN_DATA_DIR/experts.rds    one row per person, key resp_uid
#          CLEAN_DATA_DIR/vignettes.rds  one row per person x vignette, key
#                                        resp_uid x vig_num
#          CLEAN_DATA_DIR/reference_values.rds  means, shares and quartiles, the
#                                        common reference of the estimation
#                                        scripts
#          objects for 07_report.R: attempts, attempts_report, expert_arms,
#          in_reference, outlier_counts, preference_report, share_questions,
#          share_sums
# Checks : unique keys; row counts around every join; known answers of Q17 and
#          Q20; degree days for every region of an expert; the minimum cell of
#          the reference values
################################################################################
# Every data decision of the project is made here, once, for every analysis:
#   1. one row per person (Max, 2026-11-09)
#   2. the expert group (project team, September 2026; NA if none can be
#      assigned, Max, 2026-11-09)
#   3. the arm of the vignette experiment
#   4. the last question answered
#   5. the energy prices per kWh (Max, 2026-10-19 and 2026-10-20)
#   6. the expected price rises (Max, 2026-10-27)
#   7. the heating at home (Max, 2026-11-01)
#   8. the heating degree days (Max, 2026-10-27)
#   9. the time and risk preference scores
#  10. the outlier rule, age and duration (Max, 2026-10-28 and 2026-11-09)
#  11. the shares of the technologies (Max, 2026-10-29)
# What depends on a sample or a model stays in the estimation scripts: centring,
# the dummies for missing answers, the COP, the share of gas in the hybrid
# heating, z-scores.

# 0. Settings ------------------------------------------------------------------

# Reported prices (Q15): what households among the expert's clients paid per kWh
# of each fuel in January and February 2026, in cent. A blank means none of the
# clients uses the fuel, or "do not know". Answers in another unit are converted,
# in this order:
#   (1) below PRICE_EURO_BELOW: euro instead of cent, times 100
#   (2) oil within OIL_PER_LITRE (after (1)): cent per litre, divided by 10 (1 l
#       of heating oil is about 10 kWh, as the question says)
#   (3) pellets within PELLETS_PER_KG (after (1)): cent per kg, divided by 5 (1 kg
#       of pellets is about 5 kWh, as the question says)
#   (4) more than PRICE_VALID_FACTOR times above or below the median answer for
#       the fuel: cannot be read as a price per kWh, counts as not reported
# The bounds are wide on purpose: the survey measures beliefs. Band of (3):
# pellets cost about 400 euro per tonne, 40 cent per kg; it starts at 20, above
# plausible answers per kWh (median about 8 cent). PRICE_VALID_FACTOR: first
# choice, to be confirmed.
PRICE_FUELS <- c("gas", "hpelec", "dh", "oil", "pellet")
PRICE_EURO_BELOW <- 1
OIL_PER_LITRE <- c(40, 140)
PELLETS_PER_KG <- c(20, 70)
PRICE_VALID_FACTOR <- 3

# Expected prices (Q17): the price of each fuel in five years against the start
# of 2026. The answers exactly as in the data (note "10%" against "10 %"). An
# expert expects a rise with either answer "höher". The question asks about the
# price, not the price after inflation.
PEXP_ANSWERS <- c("mehr als 10% niedriger", "bis zu 10 % niedriger",
                  "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher")
PEXP_RISE <- c("bis zu 10 % höher", "mehr als 10% höher")

# Heating at home (Q20)
OWN_HEATING_ANSWERS <- c("Wärmepumpe", "Pelletheizung", "Fernwärme", "Gasheizung",
                         "Ölheizung", "Hybridheizung", "Sonstiges")

# The outlier rule: impossible years go first (VALID_RANGE, the bounds that the
# sheet data_quality of the report flags for them), then those below and above
# TRIM_QUANTILES of what is left, over everyone who answered, so that an answer
# is kept or removed in every table alike.
TRIM_QUANTILES <- c(0.005, 0.995)
VALID_RANGE <- list(
  dem_birthyear = c(1920, 2010),
  dem_training_year = c(1930, 2026)
)
OUTLIER_VARIABLES <- c("dem_birthyear", "dem_training_year", "firm_employees",
                       "adv_households", "co2_est_low", "co2_est_high")

# The technologies of Q10b and Q11b (adv_share_<suffix>, mix25_<suffix>)
SHARE_SUFFIXES <- c("hp", "pellet", "dh", "gas", "oil", "hybrid", "other")

# 1. One row per person --------------------------------------------------------
# survey_main has one row per started questionnaire. A person who starts twice
# has two rows with one token (always inside one wave). The furthest attempt
# stays: completed before not completed, then the highest page; between attempts
# equal on both, the first by resp_uid as text (the sheet attempts counts them).
# So a person who broke off and came back counts as a completer, not also as a
# break-off. A questionnaire without a token is a person of its own. A token is
# per survey, so one person invited in two waves stays two people here: only a
# token list could tell.
survey_main <- readRDS(file.path(CLEAN_DATA_DIR, "survey_main.rds"))
vignettes_long <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes_long.rds"))
n_attempts_all <- nrow(survey_main)

# coalesce(): the first value that is not NA
attempts <- survey_main %>%
  mutate(person = coalesce(meta_token, resp_uid)) %>%
  group_by(wave, person) %>%
  mutate(n_attempts = n()) %>%
  ungroup()
# desc(): from the largest down
experts <- attempts %>%
  arrange(desc(is_complete), desc(meta_lastpage)) %>%
  distinct(wave, person, .keep_all = TRUE) %>%
  arrange(resp_uid)
check_unique(experts, "resp_uid")

attempts_report <- tibble(
  step = c("Started questionnaires (rows of survey_main)",
           "Persons (one row per invitation)",
           "Further attempts left out",
           "Persons who completed",
           "Completed questionnaires",
           "Repeat completions of one invitation",
           "Persons who never completed"),
  n = c(n_attempts_all, nrow(experts), n_attempts_all - nrow(experts),
        sum(experts$is_complete), sum(survey_main$is_complete),
        sum(survey_main$is_complete) - sum(experts$is_complete),
        sum(!experts$is_complete))
)
print(attempts_report)

# 2. The expert group ----------------------------------------------------------
# Q2 allows several ticks, so one expert can fall into several categories. Every
# expert gets exactly one group, by this priority:
#   1. HVAC installers  ticked "Sanitär-, Heizungs-, Klimatechnik"
#   2. Chimney sweeps   ticked "Schornsteinfeger", but not HVAC
#   3. Energy advisers  ticked "Energieberatungsbüro" or "Architektur- /
#      Bauingenieurbüro / sonstiges Ingenieurbüro", or filled in "Sonstige" but
#      neither trade
# NA: Q2 not answered, or the page submitted without any category. The trades
# take priority over energy advice. To change the rule, reorder the case_when().
experts <- experts %>%
  mutate(
    expert_group = case_when(
      is.na(firm_cat_hvac) ~ NA_character_,
      firm_cat_hvac == "Ja" ~ "HVAC installers",
      firm_cat_chimney == "Ja" ~ "Chimney sweeps",
      firm_cat_energyadvice == "Ja" ~ "Energy advisers",
      firm_cat_engineering == "Ja" ~ "Energy advisers",
      !is.na(firm_cat_other) ~ "Energy advisers",
      TRUE ~ NA_character_
    ),
    expert_group_note = case_when(
      !is.na(expert_group) ~ "assigned",
      is.na(firm_cat_hvac) ~ "Q2 not answered",
      TRUE ~ "no category ticked"
    ),
    expert_group = factor(expert_group, levels = EXPERT_GROUPS)
  )
group_assignment <- experts %>%
  count(expert_group, expert_group_note, is_complete, .drop = FALSE)
print(group_assignment)

# The experts over whom the medians, means and quartiles below are taken: the
# completed questionnaires of the three groups in both arms. They are the same
# for both arms, so that every estimation script can share one reference expert.
in_reference <- experts$is_complete & !is.na(experts$expert_group)

# 3. The arm of the vignette experiment ----------------------------------------
# As in vignettes_long: observed from the answers or, for an expert who answered
# no vignette, routed from Q6a. An expert who answered in both arms (arm_conflict,
# one expert, who did not complete) has no arm, and their vignettes are left out
# in section 12, so that an expert and a vignette number identify a row.
expert_arms <- vignettes_long %>%
  filter(!arm_conflict, !is.na(vig_arm)) %>%
  distinct(resp_uid, vig_arm)
check_unique(expert_arms, "resp_uid")
n_experts <- nrow(experts)
experts <- experts %>%
  left_join(expert_arms %>% transmute(resp_uid, arm = as.character(vig_arm)),
            by = "resp_uid", relationship = "one-to-one") %>%
  mutate(arm = factor(arm, levels = c("fw", "nofw")))
stopifnot(nrow(experts) == n_experts)

# 4. The last question answered ------------------------------------------------
# LimeSurvey's page counter cannot be read as a question: pages hidden by routing
# still count, so one page number stands for different questions for different
# experts. A question has a time exactly when the expert submitted its page.
last_questions <- question_times(experts, vignettes_long %>%
                                   filter(resp_uid %in% experts$resp_uid)) %>%
  arrange(resp_uid, desc(position)) %>%
  distinct(resp_uid, .keep_all = TRUE) %>%
  transmute(resp_uid, last_question_position = position,
            last_question = label, last_section = as.character(section))
experts <- experts %>%
  left_join(last_questions, by = "resp_uid", relationship = "one-to-one") %>%
  mutate(last_question_position = coalesce(last_question_position, 0L),
         last_question = coalesce(last_question, "No question answered"))
stopifnot(nrow(experts) == n_experts)

# 5. Energy prices per kWh (Q15) -----------------------------------------------
# price_<fuel>_kwh: cent per kWh, NA if blank or not usable.
# price_<fuel>_treatment: what happened to the answer.
reference_prices <- tibble()
for (fuel in PRICE_FUELS) {
  reported <- experts[[paste0("price_", fuel)]]
  treatment <- if_else(is.na(reported), "blank", "as reported")

  # (1) euro instead of cent
  is_euro <- !is.na(reported) & reported < PRICE_EURO_BELOW
  price <- if_else(is_euro, reported * 100, reported)
  treatment[is_euro] <- "euro"

  # (2) oil per litre, (3) pellets per kg
  is_per_unit <- rep(FALSE, length(price))
  if (fuel == "oil") {
    is_per_unit <- !is.na(price) &
      price >= OIL_PER_LITRE[1] & price <= OIL_PER_LITRE[2]
    price <- if_else(is_per_unit, price / 10, price)
  }
  if (fuel == "pellet") {
    is_per_unit <- !is.na(price) &
      price >= PELLETS_PER_KG[1] & price <= PELLETS_PER_KG[2]
    price <- if_else(is_per_unit, price / 5, price)
  }
  treatment[is_per_unit & !is_euro] <- "per litre or kg"
  treatment[is_per_unit & is_euro] <- "euro and per litre or kg"

  # (4) far from the median answer: not a price per kWh
  price_median <- median(price[in_reference], na.rm = TRUE)
  lower <- price_median / PRICE_VALID_FACTOR
  upper <- price_median * PRICE_VALID_FACTOR
  is_unusable <- !is.na(price) & (price < lower | price > upper)
  price[is_unusable] <- NA
  treatment[is_unusable] <- "not usable"

  experts[[paste0("price_", fuel, "_kwh")]] <- price
  experts[[paste0("price_", fuel, "_treatment")]] <- treatment

  n_usable <- sum(!is.na(price[in_reference]))
  if (n_usable < MIN_N) {
    stop("Only ", n_usable, " usable prices of ", fuel, ": fewer than ", MIN_N,
         " answers behind its mean and quartiles")
  }
  quartiles <- quantile(price[in_reference], c(0.25, 0.75), na.rm = TRUE,
                        names = FALSE)
  reference_prices <- bind_rows(reference_prices, tibble(
    fuel = fuel, n = n_usable, median = price_median, lower = lower,
    upper = upper, mean = mean(price[in_reference], na.rm = TRUE),
    q25 = quartiles[1], q75 = quartiles[2]
  ))
}
print(reference_prices)

# 6. Expected price rises (Q17) ------------------------------------------------
# pexp_<fuel>_rise: 1 if the expert expects the price to rise, 0 if not, NA if
# blank
reference_expectations <- tibble()
for (fuel in PRICE_FUELS) {
  answer <- as.character(experts[[paste0("pexp_", fuel)]])
  # setdiff(): the values of the first that are not in the second
  unknown_answers <- setdiff(unique(answer[!is.na(answer)]), PEXP_ANSWERS)
  if (length(unknown_answers) > 0) {
    stop("pexp_", fuel, " has answers that are not in PEXP_ANSWERS: ",
         paste(unknown_answers, collapse = ", "))
  }
  rise <- if_else(is.na(answer), NA_real_, if_else(answer %in% PEXP_RISE, 1, 0))
  experts[[paste0("pexp_", fuel, "_rise")]] <- rise
  n_answered <- sum(!is.na(rise[in_reference]))
  if (n_answered < MIN_N) {
    stop("Only ", n_answered, " answers of pexp_", fuel, ": fewer than ", MIN_N,
         " answers behind the share expecting a rise")
  }
  reference_expectations <- bind_rows(reference_expectations, tibble(
    fuel = fuel, answered = n_answered,
    share_rise = mean(rise[in_reference], na.rm = TRUE)
  ))
}
print(reference_expectations)

# 7. The heating at home (Q20) -------------------------------------------------
# own_heating: the answer as text, "Keine Angabe" if missing. heat_pump_at_home:
# 1 if a heat pump heats the expert's home, else 0; "Sonstiges" and no answer
# count as no heat pump.
own_heating_answer <- as.character(experts$dem_own_heating)
unknown_answers <- setdiff(unique(own_heating_answer[!is.na(own_heating_answer)]),
                           OWN_HEATING_ANSWERS)
if (length(unknown_answers) > 0) {
  stop("dem_own_heating has answers that are not in OWN_HEATING_ANSWERS: ",
       paste(unknown_answers, collapse = ", "))
}
experts <- experts %>%
  mutate(own_heating = coalesce(own_heating_answer, "Keine Angabe"),
         heat_pump_at_home = if_else(own_heating == "Wärmepumpe", 1, 0))
reference_own_heating <- numeric(0)
for (answer_level in c(OWN_HEATING_ANSWERS, "Keine Angabe")) {
  reference_own_heating[answer_level] <-
    mean(experts$own_heating[in_reference] == answer_level)
}

# 8. Heating degree days -------------------------------------------------------
# Of the NUTS3 region of the expert's postcode in HDD_YEAR, from Eurostat. NA for
# an expert without nuts3 (no valid postcode, or a P.O.-box or large-customer
# postcode). A region of an expert without degree days would be a gap in the
# Eurostat data and stops the script.
hdd_regions <- read.csv(HDD_FILE) %>%
  filter(indic_nrg == "HDD", TIME_PERIOD == HDD_YEAR) %>%
  transmute(nuts3 = geo, hdd = OBS_VALUE)
experts <- experts %>%
  left_join(hdd_regions, by = "nuts3", relationship = "many-to-one")
stopifnot(nrow(experts) == n_experts)
n_region_without_hdd <- sum(!is.na(experts$nuts3) & is.na(experts$hdd))
if (n_region_without_hdd > 0) {
  stop(n_region_without_hdd, " experts have a nuts3 without degree days in ",
       HDD_YEAR)
}
n_with_hdd <- sum(!is.na(experts$hdd[in_reference]))
if (n_with_hdd < MIN_N) {
  stop("Only ", n_with_hdd, " experts with degree days: fewer than ", MIN_N,
       " answers behind their mean and quartiles")
}
hdd_quartiles <- quantile(experts$hdd[in_reference], c(0.25, 0.75), na.rm = TRUE,
                          names = FALSE)
reference_hdd <- list(year = HDD_YEAR, n = n_with_hdd,
                      mean = mean(experts$hdd[in_reference], na.rm = TRUE),
                      q25 = hdd_quartiles[1], q75 = hdd_quartiles[2])

# 9. Time and risk preferences -------------------------------------------------
preferences <- add_preference_scores(experts)
experts <- preferences$data
preference_report <- preferences$report
print(preference_report)

# 10. The outlier rule, age and duration ---------------------------------------
# <variable>_clean is the answer, NA if it fails the rule of section 0
outlier_counts <- tibble()
for (variable in OUTLIER_VARIABLES) {
  values <- experts[[variable]]
  n_answered <- sum(!is.na(values))
  valid_range <- VALID_RANGE[[variable]]
  n_impossible <- 0
  if (!is.null(valid_range)) {
    is_impossible <- !is.na(values) &
      (values < valid_range[1] | values > valid_range[2])
    n_impossible <- sum(is_impossible)
    values[is_impossible] <- NA
  }
  limits <- quantile(values, TRIM_QUANTILES, names = FALSE, na.rm = TRUE)
  values[!is.na(values) & (values < limits[1] | values > limits[2])] <- NA
  experts[[paste0(variable, "_clean")]] <- values
  outlier_counts <- bind_rows(outlier_counts, tibble(
    variable = variable, answered = n_answered, impossible = n_impossible,
    trimmed = n_answered - n_impossible - sum(!is.na(values)),
    used = sum(!is.na(values))
  ))
}
print(outlier_counts)

experts <- experts %>%
  mutate(age = SURVEY_YEAR - dem_birthyear_clean,
         interview_minutes = time_interview / 60)

# 11. The shares of the technologies (Q10b, Q11b) -------------------------------
# <variable>_filled: for an expert who gave shares in the question (a total above
# 0), the share, 0 where the technology was left blank; NA for the others. Used
# as reported, also where they do not sum to 100, which is counted. A share
# outside 0 to 100 stops the script: how should it count?
share_sums <- tibble()
share_questions <- list(
  "Q10b (recommendations)" = paste0("adv_share_", SHARE_SUFFIXES),
  "Q11b (installations)" = paste0("mix25_", SHARE_SUFFIXES)
)
for (question in names(share_questions)) {
  share_variables <- share_questions[[question]]
  n_outside <- sum(experts[share_variables] < 0 | experts[share_variables] > 100,
                   na.rm = TRUE)
  if (n_outside > 0) {
    stop(n_outside, " shares of ", question, " lie outside 0 to 100")
  }
  total <- rowSums(experts[share_variables], na.rm = TRUE)
  gave_shares <- total > 0
  for (variable in share_variables) {
    experts[[paste0(variable, "_filled")]] <- if_else(
      gave_shares, coalesce(as.numeric(experts[[variable]]), 0), NA_real_
    )
  }
  share_sums <- bind_rows(share_sums, tibble(
    question = question, answered = sum(gave_shares),
    not_100 = sum(gave_shares & abs(total - 100) > 1e-6)
  ))
}
print(share_sums)

# 12. The vignettes ------------------------------------------------------------
# The vignettes of the attempts that stayed, without the expert who answered in
# both arms. Every expert keeps six rows, also one who broke off before the
# vignettes (the design is known either way); vig_rec is NA where the vignette
# was not answered.
vignettes <- vignettes_long %>%
  filter(resp_uid %in% experts$resp_uid, !arm_conflict) %>%
  left_join(experts %>% select(resp_uid, expert_group, is_complete),
            by = "resp_uid", relationship = "many-to-one") %>%
  arrange(resp_uid, vig_num)
check_unique(vignettes, c("resp_uid", "vig_num"))

# 13. Write --------------------------------------------------------------------
# The reference values: aggregates of the completed questionnaires of the three
# groups in both arms. The estimation scripts centre at them and take their
# changes from them, so that both arms share one reference expert (Max,
# 2026-11-09).
reference_values <- list(
  prices = reference_prices,
  expectations = reference_expectations,
  own_heating = reference_own_heating,
  hdd = reference_hdd,
  n_experts = sum(in_reference),
  date = Sys.Date()
)

saveRDS(experts, file.path(CLEAN_DATA_DIR, "experts.rds"))
saveRDS(vignettes, file.path(CLEAN_DATA_DIR, "vignettes.rds"))
saveRDS(reference_values, file.path(CLEAN_DATA_DIR, "reference_values.rds"))
message("experts: ", nrow(experts), " rows x ", ncol(experts),
        " columns | vignettes: ", nrow(vignettes), " rows x ", ncol(vignettes),
        " columns")
