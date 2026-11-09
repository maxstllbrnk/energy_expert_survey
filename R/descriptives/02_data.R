################################################################################
# 02_data.R — the sample of the descriptives, and the cleaning of the answers
# Inputs : survey_main and vignettes_long (CLEAN_DATA_DIR) through
#          load_analysis_data(); degree days (HDD_FILE); the saved results of
#          the models in the deck (MODEL_RUNS, MODELS_IN_DECK), for a check
# Outputs: objects for the scripts that follow: completers, experts,
#          vignettes, arm_sample, price_reference, outlier_counts, share_sums
# Checks : experts dropped for lack of a Berufsgruppe; one row per expert and
#          per expert x vignette; one arm per expert; the answers of Q17;
#          degree days for every expert with nuts3; reference prices, price
#          rules and estimation sample against the saved results of the models
################################################################################

# 1. Completed questionnaires of the three Berufsgruppen -----------------------
analysis_data <- load_analysis_data(completers_only = TRUE)
completers <- analysis_data$main

# Completed questionnaires by Berufsgruppe: "Keine Zuordnung" and NA (Q2 not
# answered) are dropped
completers %>%
  count(berufsgruppe) %>%
  print()

experts <- completers %>%
  filter(berufsgruppe %in% BERUFSGRUPPEN) %>%
  mutate(group = factor(label_of(as.character(berufsgruppe), GROUP_LABELS,
                                 "Berufsgruppe"),
                        levels = GROUP_LABELS))
cat("Completed questionnaires:", nrow(completers),
    "| of the three Berufsgruppen:", nrow(experts), "\n")
# anyDuplicated(): the position of the first repeated value, 0 if there is
# none
stopifnot(!anyDuplicated(experts$resp_uid))

vignettes <- analysis_data$vignettes %>%
  filter(resp_uid %in% experts$resp_uid) %>%
  left_join(experts %>% select(resp_uid, group), by = "resp_uid",
            relationship = "many-to-one")
stopifnot(anyDuplicated(vignettes[c("resp_uid", "vig_num")]) == 0)

# The arm of each expert as in vignettes_long: observed from the answers or,
# for an expert who answered no vignette, routed from Q6a. (vig_arm of
# survey_main has the observed arm only.)
expert_arms <- vignettes %>%
  mutate(arm = as.character(vig_arm)) %>%
  distinct(resp_uid, arm)
stopifnot(!anyDuplicated(expert_arms$resp_uid))
experts <- experts %>%
  left_join(expert_arms, by = "resp_uid", relationship = "one-to-one")
cat("Experts by arm:\n")
print(table(experts$arm, useNA = "ifany"))

# The estimation sample of the models in each arm: experts with at least one
# answered vignette, and those vignettes
arm_sample <- vignettes %>%
  filter(!is.na(vig_rec)) %>%
  mutate(arm = as.character(vig_arm)) %>%
  group_by(arm, group) %>%
  summarise(experts = n_distinct(resp_uid), vignettes = n(), .groups = "drop")
print(arm_sample)

# 2. Energy prices (Q15): the rules of the models ------------------------------
# The reference sample of the rules is the sample of the descriptives: the
# completers of the three Berufsgruppen in both arms, as in the models
price_cleaning <- clean_reported_prices(experts,
                                        in_reference = rep(TRUE, nrow(experts)))
experts <- price_cleaning$data
price_reference <- price_cleaning$reference
print(price_reference)

# The models in the deck must have used the same rules and reference prices,
# and the same experts in their arm; otherwise these tables would describe
# other prices and other experts than the regression tables. all.equal():
# equal up to rounding error, a text describing the difference otherwise;
# isTRUE(): TRUE only for TRUE.
results_missing <- character(0)
results_checked <- character(0)
for (r in seq_len(nrow(MODEL_RUNS))) {
  run_arm <- MODEL_RUNS$arm[r]
  # The estimation sample of the arm, as in section 2.2 of
  # R/analysis/01_run_specifications.R
  descriptives_sample <- vignettes %>%
    filter(vig_arm == run_arm, !is.na(vig_rec)) %>%
    mutate(berufsgruppe = as.character(berufsgruppe)) %>%
    distinct(resp_uid, berufsgruppe) %>%
    count(berufsgruppe, name = "experts") %>%
    arrange(berufsgruppe)

  for (model_name in MODELS_IN_DECK) {
    results_file <- file.path(RC_LOGIT_DIR, MODEL_RUNS$folder[r], "results",
                              paste0(model_name, ".rds"))
    if (!file.exists(results_file)) {
      results_missing <- c(results_missing, results_file)
      next
    }
    model_results <- readRDS(results_file)

    model_rules <- model_results$price_settings
    same_rules <- isTRUE(all.equal(model_rules$euro_below, PRICE_EURO_BELOW)) &&
      isTRUE(all.equal(model_rules$oil_per_litre, OIL_PER_LITRE)) &&
      isTRUE(all.equal(model_rules$pellets_per_kg, PELLETS_PER_KG)) &&
      isTRUE(all.equal(model_rules$valid_factor, PRICE_VALID_FACTOR))
    if (!same_rules) {
      stop("The price rules of R/prepare_analysis_data.R differ from those of ",
           "the model saved in ", results_file)
    }
    same_prices <- isTRUE(all.equal(
      as.data.frame(model_results$price_reference),
      as.data.frame(price_reference)
    ))
    if (!same_prices) {
      print(model_results$price_reference)
      stop("The reference prices differ from those of the model (above) ",
           "saved in ", results_file, ": the descriptives and the model were ",
           "run on different data. Rerun the model, or the cleaning")
    }

    model_sample <- model_results$experts_by_group %>%
      mutate(berufsgruppe = as.character(berufsgruppe)) %>%
      arrange(berufsgruppe)
    same_sample <- isTRUE(all.equal(as.data.frame(model_sample),
                                    as.data.frame(descriptives_sample),
                                    check.attributes = FALSE))
    if (!same_sample) {
      print(model_sample)
      print(descriptives_sample)
      stop("The experts of the arm ", run_arm, " differ from the estimation ",
           "sample of the model saved in ", results_file, " (above, first ",
           "the model)")
    }
    results_checked <- c(results_checked, model_name)
  }
}
cat("Reference prices and estimation sample as in the saved results of:",
    paste(results_checked, collapse = ", "), "\n")
if (length(results_missing) > 0) {
  warning("No saved results, so not checked against the descriptives:\n",
          paste(results_missing, collapse = "\n"), call. = FALSE)
}

# 3. Expected price developments (Q17) -----------------------------------------
# Every answer is one of PEXP_ANSWERS, the answers the models know; the shares
# expecting a rise are in 05_policy_brief.R
for (fuel in PRICE_FUELS) {
  answer <- as.character(experts[[paste0("pexp_", fuel)]])
  unknown_answers <- setdiff(unique(answer[!is.na(answer)]), PEXP_ANSWERS)
  if (length(unknown_answers) > 0) {
    stop("pexp_", fuel, " has answers that are not in PEXP_ANSWERS: ",
         paste(unknown_answers, collapse = ", "))
  }
}

# 4. Degree days ---------------------------------------------------------------
# An expert without nuts3 has none; a region of an expert without degree days
# would be a gap in the Eurostat data, which the models do not allow either
experts <- add_heating_degree_days(experts)
is_without_hdd <- !is.na(experts$nuts3) & is.na(experts$hdd)
if (any(is_without_hdd)) {
  stop(sum(is_without_hdd), " experts have a nuts3 without degree days in ",
       HDD_YEAR)
}
cat("Experts without nuts3, so without degree days:",
    sum(is.na(experts$nuts3)), "\n")

# 5. Other continuous answers: the outlier rule, on the pooled sample ----------
# <variable>_clean is the answer, NA if it is outside VALID_RANGE or below or
# above the TRIM_QUANTILES of the remaining answers of all experts
outlier_counts <- tibble()
for (variable in names(OUTLIER_VARIABLES)) {
  values <- experts[[variable]]
  cleaned <- outliers_to_na(values, variable)
  experts[[paste0(variable, "_clean")]] <- cleaned

  valid_range <- VALID_RANGE[[variable]]
  if (is.null(valid_range)) {
    n_impossible <- 0
  } else {
    n_impossible <- sum(values < valid_range[1] | values > valid_range[2],
                        na.rm = TRUE)
  }
  n_answered <- sum(!is.na(values))
  n_used <- sum(!is.na(cleaned))
  outlier_counts <- bind_rows(outlier_counts, tibble(
    variable = variable, answered = n_answered, impossible = n_impossible,
    trimmed = n_answered - n_impossible - n_used, used = n_used
  ))
}
print(outlier_counts)

experts <- experts %>%
  mutate(age = SURVEY_YEAR - dem_birthyear_clean)

# 6. The shares of the technologies (Q10b, Q11b) -------------------------------
# <variable>_filled: for an expert who gave shares in the question (a total
# above 0), the share, 0 where the expert left the technology blank; NA for
# the others. Used as reported, not trimmed (SHARE_QUESTIONS), which assumes
# that every share lies between 0 and 100: checked. share_sums counts the
# experts whose shares do not sum to 100.
share_sums <- tibble()
for (question in names(SHARE_QUESTIONS)) {
  share_variables <- SHARE_QUESTIONS[[question]]
  n_outside <- sum(experts[share_variables] < 0 |
                     experts[share_variables] > 100, na.rm = TRUE)
  if (n_outside > 0) {
    stop(n_outside, " shares of ", question, " lie outside 0 to 100: how ",
         "should they count?")
  }
  total <- rowSums(experts[share_variables], na.rm = TRUE)
  gave_shares <- total > 0
  for (variable in share_variables) {
    # coalesce(): the first value that is not NA, here 0 for a blank share
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
