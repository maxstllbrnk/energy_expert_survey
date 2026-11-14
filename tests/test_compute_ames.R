################################################################################
# test_compute_ames.R — compute_ames() and results.xlsx on small simulated models
# Inputs : simulated decisions, drawn here
# Outputs: in <SECURE_DIR>/output/test_compute_ames/
# Checks : probabilities sum to 1; from equal to to gives 0; a change in `at` moves
#          the reference; a model by group uses the group's constants; a covariate
#          shows up without other changes; L near 0 gives the closed form; extreme
#          draws stay finite; loading; what fit_mixed_logit() refuses; the workbook;
#          effects over observed experts or answers against a brute-force
#          computation, and the same effects as a reference expert's when the
#          population is that expert
################################################################################

source(file.path("R", "setup.R"))
set.seed(20261111)
est_dir <- file.path(OUTPUT_DIR, "test_compute_ames")
# The results of an earlier run go, the models stay: the same seed gives the same data,
# so a second run starts here in seconds. Delete the folder to estimate them again.
unlink(file.path(est_dir, "*", "results.*"))
N_DRAWS_TEST <- 100       # Halton draws per expert in the estimation
N_PREDICTION_TEST <- 500
N_KR_TEST <- 100
N_WORKERS_TEST <- 2

check <- function(label, condition) {
  cat(sprintf("%-68s %s\n", label, if (isTRUE(condition)) "PASS" else "FAIL"))
  if (!isTRUE(condition)) stop("Failed: ", label)
}
stops_with <- function(label, pattern, code) {
  message_text <- tryCatch({
    code
    "no error"
  }, error = function(e) conditionMessage(e))
  check(label, grepl(pattern, message_text, fixed = TRUE))
}

# 1. Simulated decisions -------------------------------------------------------
# 400 experts, six vignettes, five outcomes. Correlated constants; a vignette
# attribute, the group, the expert's age and each technology's price with one
# coefficient.
n_experts <- 400
outcomes <- c("A", "B", "C", "D", "E")
reference <- "C"
experts <- tibble(
  resp_uid = sprintf("e%04d", seq_len(n_experts)),
  expert_group = sample(EXPERT_GROUPS, n_experts, replace = TRUE, prob = c(.5, .3, .2)),
  age = rnorm(n_experts, 50, 10),
  price_a = rnorm(n_experts, 10, 2),
  price_b = rnorm(n_experts, 20, 3)
)
true_L <- matrix(c(1.0, 0, 0, 0,
                   0.4, 0.8, 0, 0,
                   -0.3, 0.2, 0.7, 0,
                   0.1, -0.2, 0.3, 0.6), 4, 4, byrow = TRUE)
constants_of_experts <- t(c(0.5, -0.3, 0.2, -0.8) + true_L %*% matrix(rnorm(4 * n_experts), 4, n_experts))
colnames(constants_of_experts) <- c("A", "B", "D", "E")
true_effects <- tibble(
  alternative = outcomes,
  income_low = c(0.6, -0.3, 0, 0.4, -0.2),
  is_hvac = c(-0.5, 0.4, 0, 0.3, 0.1),
  is_sweep = c(0.3, -0.2, 0, -0.6, 0.2),
  age_c = c(0.4, -0.2, 0, 0.1, -0.3)
)

#' As in an estimation script: from one row per state to one row per state x
#' alternative. No vig_rec.
make_regressors <- function(states) {
  states %>%
    mutate(income_low = if_else(income == "low", 1, 0),
           is_adviser = if_else(expert_group == "Energy advisers", 1, 0),
           is_hvac = if_else(expert_group == "HVAC installers", 1, 0),
           is_sweep = if_else(expert_group == "Chimney sweeps", 1, 0),
           age_c = (age - 50) / 10,
           price_a_c = price_a - 10,
           price_b_c = price_b - 20) %>%
    cross_join(tibble(alternative = factor(outcomes, levels = outcomes))) %>%
    mutate(price = case_when(alternative == "A" ~ price_a_c,
                             alternative == "B" ~ price_b_c,
                             TRUE ~ 0))
}

decisions <- experts %>%
  cross_join(tibble(vig_num = 1:6)) %>%
  mutate(income = sample(c("low", "high"), n(), replace = TRUE)) %>%
  arrange(resp_uid, vig_num) %>%
  mutate(chid = row_number())
choice_long <- make_regressors(decisions) %>%
  left_join(true_effects %>% rename(b_income = income_low, b_hvac = is_hvac,
                                    b_sweep = is_sweep, b_age = age_c),
            by = "alternative", relationship = "many-to-one")
expert_position <- match(choice_long$resp_uid, experts$resp_uid)
constant <- rep(0, nrow(choice_long))
for (technology in colnames(constants_of_experts)) {
  rows <- choice_long$alternative == technology
  constant[rows] <- constants_of_experts[expert_position[rows], technology]
}
choice_long <- choice_long %>%
  mutate(utility = constant + b_income * income_low + b_hvac * is_hvac + b_sweep * is_sweep +
           b_age * age_c - 0.2 * price - log(-log(runif(n())))) %>%
  group_by(chid) %>%
  mutate(chosen = utility == max(utility)) %>%
  ungroup() %>%
  arrange(chid, alternative)
choice_data <- dfidx(choice_long, idx = list(c("chid", "resp_uid"), "alternative"),
                     choice = "chosen", shape = "long")

# 2. The models ----------------------------------------------------------------
time_models <- system.time({
  m_common <- fit_mixed_logit("m_common", chosen ~ price | 1 + is_hvac + is_sweep + income_low,
                              choice_data, reference, est_dir, n_draws = N_DRAWS_TEST)
  m_age <- fit_mixed_logit("m_age", chosen ~ price | 1 + is_hvac + is_sweep + income_low + age_c,
                           choice_data, reference, est_dir, n_draws = N_DRAWS_TEST,
                           start_from = m_common)
  m_group <- fit_mixed_logit("m_group", chosen ~ price | 0 + is_adviser + is_hvac + is_sweep + income_low,
                             choice_data, reference, est_dir, n_draws = N_DRAWS_TEST,
                             group_constants = c("is_adviser", "is_hvac", "is_sweep"),
                             start_from = m_common)
  results_common <- post_estimation(m_common, est_dir, label = "One set", n_draws_kr = N_KR_TEST,
                                    n_workers = N_WORKERS_TEST)
  results_age <- post_estimation(m_age, est_dir, label = "With age", n_draws_kr = N_KR_TEST,
                                 n_workers = N_WORKERS_TEST)
  results_group <- post_estimation(m_group, est_dir, label = "By group", n_draws_kr = N_KR_TEST,
                                   n_workers = N_WORKERS_TEST)
})
cat("Models:", round(time_models[["elapsed"]]), "seconds\n")

at_base <- list(income = "high", age = 50, price_a = 10, price_b = 20)
changes_base <- list(
  income_low = list(to = list(income = "low")),
  price_a_up = list(from = list(price_a = 8), to = list(price_a = 12)),
  same_state = list(from = list(price_a = 12), to = list(price_a = 12)),
  together = list(to = list(income = "low", price_b = 25))
)
call_ames <- function(results, name, at = at_base, changes = changes_base, ...) {
  compute_ames(results, make_regressors, name = name, at = at, changes = changes,
               n_draws = N_PREDICTION_TEST, n_workers = N_WORKERS_TEST, ...)
}

# 3. Probabilities and effects -------------------------------------------------
results_common <- call_ames(results_common, "base")
ames <- results_common$ames$base
check("the table has one row per profile x effect x outcome",
      nrow(ames$table) == 3 * length(changes_base) * length(outcomes))
check("the fields of the contract are there",
      all(c("table", "baseline", "draws", "at", "changes", "profiles", "population", "n_draws",
            "n_draws_kr", "fingerprint", "date") %in% names(ames)) &&
        identical(names(ames$table), c("profile", "effect", "outcome", "p_from", "p_to", "ame",
                                       "se", "z", "p", "ci_low", "ci_high", "ci_low_kr", "ci_high_kr",
                                       "n_units", "n_experts")) &&
        identical(names(ames$baseline), c("profile", "outcome", "probability", "se", "ci_low_kr",
                                          "ci_high_kr", "n_units", "n_experts")))
computed_rows <- ames$table %>%
  filter(effect != "same_state")
check("without a population: one fictitious expert per profile, so no experts to count",
      is.null(ames$population) && all(computed_rows$n_units == 1) &&
        all(is.na(computed_rows$n_experts)))
sum_baseline <- ames$baseline %>%
  group_by(profile) %>%
  summarise(total = sum(probability))
check("probabilities at the estimates sum to 1", all(abs(sum_baseline$total - 1) < 1e-12))
# The states of a blank effect (same_state) have no units, so no probabilities
sum_draws <- apply(ames$draws[["HVAC installers"]], c(1, 3), sum)
computed_states <- !rownames(sum_draws) %in% c("same_state|from", "same_state|to")
check("probabilities at every Krinsky-Robb draw sum to 1",
      all(abs(sum_draws[computed_states, ] - 1) < 1e-12) &&
        all(is.nan(sum_draws[!computed_states, ])))
# A blank effect (same_state, which changes nothing) has no values to sum
sum_effects <- ames$table %>%
  filter(!is.na(ame)) %>%
  group_by(profile, effect) %>%
  summarise(total = sum(ame))
check("the effects of a change sum to 0 across the outcomes", all(abs(sum_effects$total) < 1e-12))
check("the draws are states x outcomes x Krinsky-Robb draws",
      identical(dim(ames$draws[["Energy advisers"]]),
                as.integer(c(1 + 2 * length(changes_base), length(outcomes), N_KR_TEST))) &&
        identical(dimnames(ames$draws[["Energy advisers"]])[[1]][1:3],
                  c("baseline", "income_low|from", "income_low|to")))
same_state <- ames$table %>%
  filter(effect == "same_state")
check("from equal to to changes nothing: the effect is left out, blank (Max, 2026-11-14)",
      all(is.na(same_state$ame)) && all(is.na(same_state$se)) && all(same_state$n_units == 0))
moved <- ames$table %>%
  filter(effect == "income_low")
check("a change moves the probabilities", all(abs(moved$ame) > 1e-4))
check("the effect is the difference of the probabilities",
      all(abs(moved$ame - (moved$p_to - moved$p_from)) < 1e-15))
at_from <- ames$table %>%
  filter(effect == "income_low", profile == "Energy advisers")
baseline_advisers <- ames$baseline %>%
  filter(profile == "Energy advisers")
check("without `from`, an effect starts at `at`",
      all(abs(at_from$p_from - baseline_advisers$probability) < 1e-15))
together <- ames$table %>%
  filter(effect == "together", profile == "Energy advisers")
check("several variables can change together", all(abs(together$p_to - at_from$p_to) > 1e-5))
z_table <- ames$table %>%
  filter(se > 0)
check("z, p and the interval follow from ame and se",
      all(abs(z_table$z - z_table$ame / z_table$se) < 1e-12) &&
        all(abs(z_table$ci_high - (z_table$ame + qnorm(0.975) * z_table$se)) < 1e-12))
gas_draws <- ames$draws[["Energy advisers"]]["income_low|to", "A", ] -
  ames$draws[["Energy advisers"]]["income_low|from", "A", ]
row_a <- at_from %>%
  filter(outcome == "A")
check("se and percentile interval come from the draws",
      abs(row_a$se - sd(gas_draws)) < 1e-15 &&
        abs(row_a$ci_low_kr - quantile(gas_draws, 0.025, names = FALSE)) < 1e-15)

# 4. A change in `at` moves the reference --------------------------------------
results_common <- call_ames(results_common, "low_income", at = modifyList(at_base, list(income = "low")),
                            changes = list(back = list(to = list(income = "high"))))
other <- results_common$ames$low_income
check("the earlier effects stay", all(c("base", "low_income") %in% names(results_common$ames)))
to_state <- ames$table %>%
  filter(effect == "income_low")
new_base <- other$baseline %>%
  left_join(to_state, by = c("profile", "outcome"), relationship = "one-to-one")
check("the reference moves: the new baseline is the old state with low income",
      all(abs(new_base$probability - new_base$p_to) < 1e-12))
check("the new baseline differs from the old one",
      any(abs(other$baseline$probability - ames$baseline$probability) > 1e-3))
check("the effect back is minus the effect there",
      all(abs(other$table$ame + to_state$ame) < 1e-12))

# 5. Profiles ------------------------------------------------------------------
results_common <- call_ames(results_common, "by_list", profiles = list(
  "All experts" = list(expert_group = "Energy advisers"),
  "Old advisers" = list(expert_group = "Energy advisers", age = 65)
))
by_list <- results_common$ames$by_list$baseline
all_experts <- by_list %>%
  filter(profile == "All experts")
check("a profile as a list sets the group", all(abs(all_experts$probability - baseline_advisers$probability) < 1e-15))
check("a list of profiles has its names", identical(unique(by_list$profile), c("All experts", "Old advisers")))
old_advisers <- by_list %>%
  filter(profile == "Old advisers")
check("a profile sets any column, here one the model does not use",
      all(abs(old_advisers$probability - all_experts$probability) < 1e-15))

# 6. Constants by group --------------------------------------------------------
changes_group <- c(changes_base, list(
  to_hvac = list(from = list(expert_group = "Energy advisers"),
                 to = list(expert_group = "HVAC installers"))
))
results_group <- call_ames(results_group, "base", changes = changes_group)
ames_group <- results_group$ames$base

# By hand, for one group: its means and its block of L, from the formula
theta <- results_group$coefficients
technologies <- setdiff(outcomes, reference)
primes <- c(2, 3, 5, 7)
Z <- matrix(NA_real_, N_PREDICTION_TEST, length(technologies))
for (k in seq_along(technologies)) {
  Z[, k] <- qnorm(mlogit:::halton(primes[k], N_PREDICTION_TEST, 100))
}
probabilities_by_hand <- function(group, theta, income_low, price_a_c = 0) {
  random_names <- results_group$random$name[results_group$random$column == group]
  L <- matrix(0, length(random_names), length(random_names))
  for (i in seq_along(random_names)) {
    for (j in seq_len(i)) {
      L[i, j] <- theta[[paste0("chol.", random_names[j], ":", random_names[i])]]
    }
  }
  # in the order of mlogit's constants
  technology_of_constant <- results_group$random$technology[results_group$random$column == group]
  utility <- matrix(0, N_PREDICTION_TEST, length(outcomes), dimnames = list(NULL, outcomes))
  deviations <- Z %*% t(L)
  for (k in seq_along(technology_of_constant)) {
    technology <- technology_of_constant[k]
    utility[, technology] <- theta[[paste0(group, ":", technology)]] +
      theta[[paste0("income_low:", technology)]] * income_low + deviations[, k]
  }
  utility[, "A"] <- utility[, "A"] + theta[["price"]] * price_a_c
  share <- exp(utility) / rowSums(exp(utility))
  colMeans(share)
}
by_hand <- probabilities_by_hand("is_hvac", theta, income_low = 0)
hvac_baseline <- ames_group$baseline %>%
  filter(profile == "HVAC installers")
check("a model by group uses the group's constants: baseline by hand",
      max(abs(by_hand[hvac_baseline$outcome] - hvac_baseline$probability)) < 1e-12)
hvac_income <- ames_group$table %>%
  filter(profile == "HVAC installers", effect == "income_low")
by_hand_low <- probabilities_by_hand("is_hvac", theta, income_low = 1)
check("a model by group: effect by hand",
      max(abs((by_hand_low - by_hand)[hvac_income$outcome] - hvac_income$ame)) < 1e-12)
adviser_baseline <- ames_group$baseline %>%
  filter(profile == "Energy advisers")
check("the groups differ", max(abs(adviser_baseline$probability - hvac_baseline$probability)) > 1e-3)
switch_group <- ames_group$table %>%
  filter(profile == "Chimney sweeps", effect == "to_hvac")
check("a change can move the group, whatever the profile",
      max(abs(switch_group$p_from - adviser_baseline$probability)) < 1e-12 &&
        max(abs(switch_group$p_to - hvac_baseline$probability)) < 1e-12)

# The constants and L of another group change nothing for this group
other_folder <- file.path(est_dir, "m_group_other")
dir.create(other_folder, showWarnings = FALSE)
changed_names <- c(paste0("is_sweep:", technologies),
                   names(theta)[grepl("chol.is_sweep:", names(theta), fixed = TRUE)])
results_other <- results_group
results_other$folder <- other_folder
results_other$coefficients[changed_names] <- results_other$coefficients[changed_names] + 0.5
results_other$theta_draws[, changed_names] <- results_other$theta_draws[, changed_names] + 0.5
results_other$ames <- list()
results_other <- call_ames(results_other, "other", changes = list())
other_group <- results_other$ames$other$baseline
check("another group's constants do not move a group's probabilities",
      max(abs(other_group$probability[other_group$profile == "HVAC installers"] -
                hvac_baseline$probability)) < 1e-12)
check("its own constants do",
      max(abs(other_group$probability[other_group$profile == "Chimney sweeps"] -
                ames_group$baseline$probability[ames_group$baseline$profile == "Chimney sweeps"])) > 1e-3)
check("no changes: only the baseline", nrow(results_other$ames$other$table) == 0 &&
        nrow(other_group) == 3 * length(outcomes))

# 7. A covariate entering through make_regressors() ----------------------------
at_age <- at_base
changes_age <- list(age_up = list(from = list(age = 40), to = list(age = 60)))
results_age <- call_ames(results_age, "age", at = at_age, changes = changes_age)
age_effect <- results_age$ames$age$table
check("a covariate of make_regressors() shows up without other changes", any(abs(age_effect$ame) > 1e-3))
true_direction <- age_effect %>%
  filter(profile == "Energy advisers", outcome == "A")
check("it has the sign of the simulated effect", true_direction$ame > 0)
messages_age <- character(0)
results_common <- withCallingHandlers(
  call_ames(results_common, "age", at = at_age, changes = changes_age),
  message = function(m) {
    messages_age <<- c(messages_age, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
check("in a model without the covariate, its effect is blank, not 0", all(is.na(results_common$ames$age$table$ame)))
check("and it says so", any(grepl("or not answered): age_up", messages_age, fixed = TRUE)))
no_age_at <- at_base
no_age_at$age <- NULL
stops_with("a source variable missing in `at` stops and names it", "age",
           call_ames(results_common, "no_age", at = no_age_at))
stops_with("two values of a variable in `at` stop", "one value per variable",
           call_ames(results_common, "two_values", at = modifyList(at_base, list(age = c(40, 60)))))
without_column <- function(states) {
  make_regressors(states) %>%
    select(-age_c)
}
stops_with("a coefficient without a column stops", "Coefficients without a column",
           compute_ames(results_age, without_column, name = "x", at = at_base,
                        changes = changes_age, n_draws = 50, n_workers = 1))

# 8. L near 0: the closed form of a simple logit --------------------------------
near_folder <- file.path(est_dir, "m_near")
dir.create(near_folder, showWarnings = FALSE)
results_near <- results_common
results_near$folder <- near_folder
results_near$ames <- list()
cholesky_names <- coefficient_parts(results_near)$cholesky$parameter
results_near$coefficients[cholesky_names] <- 1e-10
results_near$theta_draws[, cholesky_names] <- 1e-10
results_near <- call_ames(results_near, "simple", at = list(income = "low", age = 50, price_a = 13, price_b = 17),
                          changes = list(none = list(to = list(price_a = 13))))
near <- results_near$ames$simple
closed_form <- function(theta, is_hvac, is_sweep, income_low, price_a_c, price_b_c) {
  utility <- setNames(rep(0, length(outcomes)), outcomes)
  for (technology in technologies) {
    utility[technology] <- theta[[paste0("(Intercept):", technology)]] +
      theta[[paste0("is_hvac:", technology)]] * is_hvac +
      theta[[paste0("is_sweep:", technology)]] * is_sweep +
      theta[[paste0("income_low:", technology)]] * income_low
  }
  utility["A"] <- utility["A"] + theta[["price"]] * price_a_c
  utility["B"] <- utility["B"] + theta[["price"]] * price_b_c
  exp(utility) / sum(exp(utility))
}
closed_hvac <- closed_form(results_near$coefficients, 1, 0, 1, 3, -3)
near_hvac <- near$baseline %>%
  filter(profile == "HVAC installers")
check("L near 0: the probabilities match the closed form",
      max(abs(closed_hvac[near_hvac$outcome] - near_hvac$probability)) < 1e-8)
closed_draw <- closed_form(results_near$theta_draws[7, ], 1, 0, 1, 3, -3)
check("L near 0: also at a Krinsky-Robb draw",
      max(abs(closed_draw - near$draws[["HVAC installers"]]["baseline", names(closed_draw), 7])) < 1e-8)

# 9. Extreme draws stay finite -------------------------------------------------
# A difference of 800 in utility: exp(800) is Inf without the protection
extreme_folder <- file.path(est_dir, "m_extreme")
dir.create(extreme_folder, showWarnings = FALSE)
results_extreme <- results_common
results_extreme$folder <- extreme_folder
results_extreme$ames <- list()
results_extreme$coefficients["(Intercept):A"] <- 800
results_extreme$theta_draws[, "(Intercept):A"] <- 800
results_extreme <- call_ames(results_extreme, "extreme", changes = list())
extreme_a <- results_extreme$ames$extreme$baseline %>%
  filter(outcome == "A")
check("a utility of 800 gives a probability of about 1, finite", all(abs(extreme_a$probability - 1) < 1e-6))
results_impossible <- results_extreme
results_impossible$theta_draws[, "(Intercept):A"] <- 1e6
results_impossible$theta_draws[, grepl("chol", colnames(results_impossible$theta_draws))] <- 2000
stops_with("what exp() cannot represent stops", "cannot be computed",
           call_ames(results_impossible, "impossible", changes = list(), force = TRUE))

# 10. Loading ------------------------------------------------------------------
marker <- 99
saved <- readRDS(file.path(results_common$folder, "results.rds"))
saved$ames$base$table$ame[1] <- marker
saveRDS(saved, file.path(results_common$folder, "results.rds"))
loaded <- call_ames(results_common, "base")
check("the same call loads", loaded$ames$base$table$ame[1] == marker)
stale <- results_common
stale$ames <- list()
kept <- call_ames(stale, "base")
check("the file keeps the effects, even if the script did not keep the results",
      all(c("base", "low_income", "by_list", "age") %in% names(kept$ames)) &&
        kept$ames$base$table$ame[1] == marker)
recomputed <- call_ames(results_common, "base", force = TRUE)
check("force computes again", recomputed$ames$base$table$ame[1] != marker)
other_changes <- call_ames(recomputed, "base", changes = changes_base[1:2])
check("other changes compute again", n_distinct(other_changes$ames$base$table$effect) == 2)
restored <- call_ames(other_changes, "base")
reloaded_post <- post_estimation(m_common, est_dir, n_draws_kr = N_KR_TEST, n_workers = N_WORKERS_TEST)
check("post_estimation() loads the results with the effects", "base" %in% names(reloaded_post$ames))

# 11. What would otherwise give a wrong result ---------------------------------
stops_with("a change of a variable not in `at` stops", "not in `at`",
           call_ames(results_common, "typo", changes = list(typo = list(to = list(incomme = "low")))))
stops_with("a change without `to` stops", "must set `to`",
           call_ames(results_common, "typo", changes = list(typo = list(from = list(income = "low")))))
stops_with("a misspelled `from` stops", "must set `to`",
           call_ames(results_common, "typo", changes = list(typo = list(fromm = list(income = "low"),
                                                                        to = list(income = "high")))))
stops_with("a name without a sheet stops", "at most 27",
           call_ames(results_common, "a_name_of_more_than_twenty_seven_characters"))
stops_with("make_regressors() that loses rows stops", "one row per state x alternative",
           compute_ames(results_common, function(states) {
             make_regressors(states) %>% filter(alternative != "E")
           }, name = "lost", at = at_base, changes = changes_base, n_draws = 50, n_workers = 1))

# 12. What fit_mixed_logit() refuses before estimating --------------------------
refused_fit <- function(long, formula) {
  refused_data <- dfidx(long, idx = list(c("chid", "resp_uid"), "alternative"),
                        choice = "chosen", shape = "long")
  fit_mixed_logit("m_refused", formula, refused_data, reference, est_dir, n_draws = N_DRAWS_TEST)
}
stops_with("a dummy that is the same for every expert is named", "estimated for blank_na",
           refused_fit(choice_long %>% mutate(blank_na = 0), chosen ~ price + blank_na | 1 + income_low))
stops_with("a term the same for all alternatives is named", "estimated for age_c",
           refused_fit(choice_long, chosen ~ price + age_c | 1 + income_low))
stops_with("linearly dependent terms are named", "estimated for",
           refused_fit(choice_long %>% mutate(income_high = 1 - income_low),
                       chosen ~ price | 1 + income_low + income_high))
never_chosen <- choice_long %>%
  group_by(chid) %>%
  mutate(chose_e = any(chosen & alternative == "E")) %>%
  ungroup() %>%
  mutate(chosen = if_else(chose_e, alternative == "A", chosen))
stops_with("an outcome never chosen is named", "no finite estimate of E",
           refused_fit(never_chosen, chosen ~ price | 1 + income_low))
separated <- choice_long %>%
  group_by(chid) %>%
  mutate(chose_e = any(chosen & alternative == "E")) %>%
  ungroup() %>%
  mutate(chosen = if_else(is_sweep == 1 & chose_e, alternative == "A", chosen))
stops_with("a group that never chooses a technology is named", "no finite estimate of is_sweep:E",
           refused_fit(separated, chosen ~ price | 1 + is_hvac + is_sweep))
never_reference <- choice_long %>%
  group_by(chid) %>%
  mutate(chose_reference = any(chosen & alternative == reference)) %>%
  ungroup() %>%
  mutate(chosen = if_else(is_sweep == 1 & chose_reference, alternative == "A", chosen))
stops_with("a group that never chooses the reference outcome is named", "no finite estimate of is_sweep:A",
           refused_fit(never_reference, chosen ~ price | 1 + is_hvac + is_sweep))
blank_never_chosen <- choice_long %>%
  mutate(blank_na = if_else(alternative == "E" & !chosen & runif(n()) < 0.2, 1, 0))
stops_with("a dummy of one alternative never chosen at 1 is named", "no finite estimate of blank_na",
           refused_fit(blank_never_chosen, chosen ~ price + blank_na | 1 + income_low))
with_missing <- choice_long %>%
  mutate(income_low = if_else(chid == 5, NA_real_, income_low))
stops_with("a missing value in a whole decision is named", "missing values in income_low",
           refused_fit(with_missing, chosen ~ price | 1 + income_low))
one_row_missing <- choice_long %>%
  mutate(price = if_else(chid == 5 & alternative == "A", NA_real_, price))
stops_with("a missing value in one row of a decision is named", "missing values in price",
           refused_fit(one_row_missing, chosen ~ price | 1 + income_low))

# 13. The workbook -------------------------------------------------------------
workbook <- file.path(results_group$folder, "results.xlsx")
sheet_names <- excel_sheets(workbook)
print(sheet_names)
check("the sheets", identical(sheet_names, c(
  "overview", "coefficients_by_technology", "constants_sd", "constants_correlations",
  "ame_base", "effect_definitions", "choice_counts", "fit", "coefficients"
)))
by_technology <- read_excel(workbook, sheet = "coefficients_by_technology")
print(by_technology)
check("one row per regressor, then the term with one coefficient",
      identical(by_technology$term, c("is_adviser", "is_hvac", "is_sweep", "income_low", "price")))
check("estimate and standard error in adjacent columns, one pair per technology",
      identical(names(by_technology), c("term", "A", "A se", "B", "B se", "D", "D se", "E", "E se",
                                        "All technologies", "All technologies se")))
long_coefficients <- read_excel(workbook, sheet = "coefficients")
check("the readable sheet rounds, the long sheet keeps every digit",
      abs(long_coefficients$estimate[long_coefficients$parameter == "income_low:A"] -
            results_group$coefficients[["income_low:A"]]) < 1e-12 &&
        by_technology$A[by_technology$term == "income_low"] ==
        round(results_group$coefficients[["income_low:A"]], 3))
check("the term with one coefficient has its own columns",
      by_technology$`All technologies`[5] == round(results_group$coefficients[["price"]], 3) &&
        is.na(by_technology$A[5]) && is.na(by_technology$`All technologies`[1]))
ame_sheet <- read_excel(workbook, sheet = "ame_base")
print(ame_sheet, n = 7)
first_baseline <- ame_sheet %>%
  filter(profile == ame_sheet$profile[1], effect == "Probability at the baseline")
check("ame_<name>: the probabilities at the baseline first, in %, every digit kept",
      ame_sheet$effect[1] == "Probability at the baseline" &&
        abs(sum(first_baseline$estimate) - 100) < 1e-8)
income_row <- ames_group$table %>%
  filter(profile == "HVAC installers", effect == "income_low", outcome == "A")
income_sheet <- ame_sheet %>%
  filter(profile == "HVAC installers", effect == "income_low", outcome == "A")
check("ame_<name>: an effect in percentage points, with its Krinsky-Robb SE and interval",
      abs(income_sheet$estimate - 100 * income_row$ame) < 1e-10 &&
        abs(income_sheet$se - 100 * income_row$se) < 1e-10 &&
        abs(income_sheet$ci_low - 100 * income_row$ci_low_kr) < 1e-10 &&
        abs(income_sheet$ci_high - 100 * income_row$ci_high_kr) < 1e-10 &&
        income_sheet$excludes_0 ==
        if_else(income_row$ci_low_kr > 0 | income_row$ci_high_kr < 0, "yes", "no"))
check("ame_<name>: a reference expert's set has no column of experts",
      identical(names(ame_sheet), c("profile", "effect", "outcome", "estimate", "se", "ci_low",
                                    "ci_high", "excludes_0", "p_from", "p_to")))
definitions <- read_excel(workbook, sheet = "effect_definitions")
print(definitions, n = 8)
check("the definitions say what each effect changes",
      any(definitions$effect == "price_a_up" & definitions$state == "from" &
            definitions$variable == "price_a" & definitions$value == "8"))
definitions_common <- read_excel(file.path(results_common$folder, "results.xlsx"),
                                 sheet = "effect_definitions")
check("and what a profile as a list sets",
      any(definitions_common$effects == "by_list" & definitions_common$effect == "Old advisers" &
            definitions_common$state == "profile" & definitions_common$variable == "age" &
            definitions_common$value == "65"))

# A count below the minimum cell stays blank, and takes the next smallest with it
results_few <- results_group
results_few$folder <- other_folder
results_few$ames <- list()
results_few$choice_counts[] <- c(500, 200, 100, 50, 5)
write_results_excel(results_few)
counts_few <- read_excel(file.path(other_folder, "results.xlsx"), sheet = "choice_counts")
print(counts_few)
check("a cell below the minimum and the next smallest are blank",
      sum(is.na(counts_few$choices)) == 2 && sum(is.na(counts_few$`share (%)`)) == 2 &&
        !is.na(counts_few$choices[counts_few$outcome == names(results_few$choice_counts)[3]]))

# Outside a git repository there is no commit: the overview has a blank there
results_no_git <- results_few
results_no_git$git_commit <- NA_character_
write_results_excel(results_no_git)
overview_no_git <- read_excel(file.path(other_folder, "results.xlsx"), sheet = "overview")
check("no commit: the workbook is still written", is.na(overview_no_git$value[overview_no_git$item == "Code version"]))

# A workbook older than the results is written again on loading: the run before
# could not write it, for example because it was open in Excel
workbook_common <- file.path(results_common$folder, "results.xlsx")
Sys.setFileTime(workbook_common, Sys.time() - 3600)
time_before <- file.mtime(workbook_common)
reloaded_ames <- call_ames(reloaded_post, "base")
check("a workbook older than the results is written again", file.mtime(workbook_common) > time_before)
time_written <- file.mtime(workbook_common)
Sys.sleep(1.1)
reloaded_ames <- call_ames(reloaded_post, "base")
check("a current workbook is left alone, so that it may stay open in Excel",
      file.mtime(workbook_common) == time_written)

# 14. Effects over observed experts: a brute-force computation ------------------
# Options B to D. Every number is also computed here expert by expert, from the
# formula, with the draws of the function: so blocks, cells, averages and the group
# of each state are checked against code that shares none of them.
make_regressors_na <- function(states) {
  states %>%
    mutate(income_low = if_else(income == "low", 1, 0),
           is_adviser = if_else(expert_group == "Energy advisers", 1, 0),
           is_hvac = if_else(expert_group == "HVAC installers", 1, 0),
           is_sweep = if_else(expert_group == "Chimney sweeps", 1, 0),
           age_c = (age - 50) / 10,
           price_a_c = if_else(is.na(price_a), 0, price_a - 10),
           price_b_c = if_else(is.na(price_b), 0, price_b - 20)) %>%
    cross_join(tibble(alternative = factor(outcomes, levels = outcomes))) %>%
    mutate(price = case_when(alternative == "A" ~ price_a_c,
                             alternative == "B" ~ price_b_c,
                             TRUE ~ 0))
}
call_observed <- function(results, name, at, changes, population, ...) {
  compute_ames(results, make_regressors_na, name = name, at = at, changes = changes,
               population = population, n_draws = N_PREDICTION_TEST,
               n_workers = N_WORKERS_TEST, ...)
}

# The probabilities of one expert of the model with one set of constants, by hand
theta_common <- results_common$coefficients
random_common <- results_common$random
L_common <- matrix(0, nrow(random_common), nrow(random_common))
for (i in seq_len(nrow(random_common))) {
  for (j in seq_len(i)) {
    L_common[i, j] <- theta_common[[paste0("chol.", random_common$name[j], ":", random_common$name[i])]]
  }
}
deviations_common <- Z %*% t(L_common)
probabilities_common <- function(is_hvac, is_sweep, income_low, price_a_c, price_b_c) {
  utility <- matrix(0, N_PREDICTION_TEST, length(outcomes), dimnames = list(NULL, outcomes))
  for (k in seq_len(nrow(random_common))) {
    technology <- random_common$technology[k]
    utility[, technology] <- theta_common[[paste0("(Intercept):", technology)]] +
      theta_common[[paste0("is_hvac:", technology)]] * is_hvac +
      theta_common[[paste0("is_sweep:", technology)]] * is_sweep +
      theta_common[[paste0("income_low:", technology)]] * income_low + deviations_common[, k]
  }
  utility[, "A"] <- utility[, "A"] + theta_common[["price"]] * price_a_c
  utility[, "B"] <- utility[, "B"] + theta_common[["price"]] * price_b_c
  colMeans(exp(utility) / rowSums(exp(utility)))
}
# The average over some experts (rows), for a vignette with income_low, at their own
# prices unless price_a_c or price_b_c is given for them
is_hvac_of <- as.numeric(experts$expert_group == "HVAC installers")
is_sweep_of <- as.numeric(experts$expert_group == "Chimney sweeps")
average_by_hand <- function(rows, income_low, price_a_c, price_b_c) {
  total <- rep(0, length(outcomes))
  for (i in rows) {
    total <- total + probabilities_common(is_hvac_of[i], is_sweep_of[i], income_low,
                                          price_a_c[i], price_b_c[i])
  }
  total / length(rows)
}
rows_of_profile <- list(
  "All experts" = seq_len(n_experts),
  "Energy advisers" = which(experts$expert_group == "Energy advisers"),
  "HVAC installers" = which(experts$expert_group == "HVAC installers"),
  "Chimney sweeps" = which(experts$expert_group == "Chimney sweeps")
)
profiles_observed <- list(
  "All experts" = list(),
  "Energy advisers" = list(expert_group = "Energy advisers"),
  "HVAC installers" = list(expert_group = "HVAC installers"),
  "Chimney sweeps" = list(expert_group = "Chimney sweeps")
)
in_outcome_order <- function(table, column) {
  table[[column]][match(outcomes, table$outcome)]
}

# B: every expert at their own prices and group, the reference vignette set by `at`
changes_observed <- list(
  income_low = list(to = list(income = "low")),
  price_a_up = list(from = list(price_a = 8), to = list(price_a = 12)),
  same_state = list(from = list(price_a = 12), to = list(price_a = 12))
)
at_vignette <- list(income = "high")
population_experts <- experts
results_common <- call_observed(results_common, "observed", at_vignette, changes_observed,
                                population_experts, profiles = profiles_observed)
observed <- results_common$ames$observed
price_a_c_own <- experts$price_a - 10
price_b_c_own <- experts$price_b - 20
gaps_baseline <- numeric(0)
gaps_income <- numeric(0)
gaps_price <- numeric(0)
for (profile_name in names(rows_of_profile)) {
  rows <- rows_of_profile[[profile_name]]
  by_hand_own <- average_by_hand(rows, 0, price_a_c_own, price_b_c_own)
  baseline_rows <- observed$baseline %>%
    filter(profile == profile_name)
  gaps_baseline <- c(gaps_baseline,
                     max(abs(in_outcome_order(baseline_rows, "probability") - by_hand_own)))
  by_hand_low <- average_by_hand(rows, 1, price_a_c_own, price_b_c_own)
  income_rows <- observed$table %>%
    filter(profile == profile_name, effect == "income_low")
  gaps_income <- c(gaps_income,
                   max(abs(in_outcome_order(income_rows, "ame") - (by_hand_low - by_hand_own))))
  by_hand_p8 <- average_by_hand(rows, 0, rep(-2, n_experts), price_b_c_own)
  by_hand_p12 <- average_by_hand(rows, 0, rep(2, n_experts), price_b_c_own)
  price_rows <- observed$table %>%
    filter(profile == profile_name, effect == "price_a_up")
  gaps_price <- c(gaps_price,
                  max(abs(in_outcome_order(price_rows, "ame") - (by_hand_p12 - by_hand_p8))),
                  max(abs(in_outcome_order(price_rows, "p_from") - by_hand_p8)))
}
check("B: the baseline is the average over the experts, by hand", max(gaps_baseline) < 1e-10)
check("B: a change of `at`-level variable, to the experts' own state: by hand", max(gaps_income) < 1e-10)
check("B: a change with `from` and `to`, set for every expert: by hand", max(gaps_price) < 1e-10)
sizes_observed <- observed$baseline %>%
  filter(outcome == "A")
check("B: the profiles count their experts",
      identical(sizes_observed$n_experts, as.integer(lengths(rows_of_profile))) &&
        identical(sizes_observed$n_units, sizes_observed$n_experts))
pooled <- observed$baseline %>%
  filter(profile == "All experts") %>%
  arrange(match(outcome, outcomes))
by_group <- observed$baseline %>%
  filter(profile != "All experts")
weighted <- by_group %>%
  group_by(outcome) %>%
  summarise(probability = sum(probability * n_experts) / sum(n_experts))
check("B: the pooled average is the average of the groups, weighted by their size",
      max(abs(pooled$probability - weighted$probability[match(pooled$outcome, weighted$outcome)])) < 1e-12)
same_state_observed <- observed$table %>%
  filter(effect == "same_state")
check("B: from equal to to changes nothing: blank, counted as 0 experts",
      all(is.na(same_state_observed$ame)) && all(same_state_observed$n_experts == 0))
check("B: the draws are states x outcomes x Krinsky-Robb draws, averages only",
      identical(dim(observed$draws[["HVAC installers"]]),
                as.integer(c(1 + 2 * length(changes_observed), length(outcomes), N_KR_TEST))))
serialized_ames <- rawToChar(serialize(observed, NULL, ascii = TRUE))
check("nothing at the level of experts is saved",
      !grepl(experts$resp_uid[1], serialized_ames, fixed = TRUE) &&
        !grepl(experts$resp_uid[n_experts], serialized_ames, fixed = TRUE) &&
        observed$population$experts == n_experts)

# D: another household, the same experts: the new baseline is the old effect's `to`
results_common <- call_observed(results_common, "observed_low", list(income = "low"),
                                list(back = list(to = list(income = "high"))),
                                population_experts, profiles = profiles_observed)
low_baseline <- results_common$ames$observed_low$baseline
to_low <- observed$table %>%
  filter(effect == "income_low")
joined_low <- low_baseline %>%
  left_join(to_low, by = c("profile", "outcome"), relationship = "one-to-one")
check("D: another `at` moves the household: its baseline is the old effect's `to`",
      max(abs(joined_low$probability - joined_low$p_to)) < 1e-12)

# A population of one expert is a reference expert: the same numbers as without one.
# Without same_state, which is blank in both.
one_expert <- tibble(resp_uid = "one", expert_group = "Energy advisers", income = "high",
                     age = 50, price_a = 10, price_b = 20)
changes_moving <- changes_base[c("income_low", "price_a_up", "together")]
results_common <- call_ames(results_common, "reference_expert", changes = changes_moving,
                            profiles = list("Energy advisers" = list(expert_group = "Energy advisers")))
reference_expert <- results_common$ames$reference_expert
results_common <- call_observed(results_common, "one_expert", list(), changes_moving, one_expert,
                                profiles = list("Energy advisers" = list()), check_sample = FALSE)
same_as_reference <- results_common$ames$one_expert
check("a population of one expert gives the effects of a reference expert",
      max(abs(same_as_reference$draws[["Energy advisers"]] -
                reference_expert$draws[["Energy advisers"]])) < 1e-12 &&
        max(abs(same_as_reference$table$ame - reference_expert$table$ame)) < 1e-12 &&
        all(same_as_reference$table$n_experts == 1))
results_common <- call_observed(results_common, "copies", list(), changes_moving,
                                bind_rows(one_expert, one_expert %>% mutate(resp_uid = "two"),
                                          one_expert %>% mutate(resp_uid = "three")),
                                profiles = list("Energy advisers" = list()), check_sample = FALSE)
copies <- results_common$ames$copies
check("copies of one expert give the same averages, counted as units and experts",
      max(abs(copies$draws[["Energy advisers"]] - same_as_reference$draws[["Energy advisers"]])) < 1e-12 &&
        all(copies$table$n_units == 3) && all(copies$table$n_experts == 3))

# 15. Missing answers: an effect leaves out the experts who did not answer -------
experts_na <- experts
experts_na$price_a[seq(1, n_experts, by = 10)] <- NA
reporters <- !is.na(experts_na$price_a)
price_a_c_na <- if_else(is.na(experts_na$price_a), 0, experts_na$price_a - 10)
# na_effect starts at 12: from the mean, 10, it would move no regressor, as this
# model has no dummy for a missing price, and leave out everyone
changes_na <- list(
  price_a_up = changes_observed$price_a_up,
  na_effect = list(from = list(price_a = 12), to = list(price_a = NA_real_))
)
results_common <- call_observed(results_common, "observed_na", at_vignette, changes_na,
                                experts_na, profiles = profiles_observed)
with_na <- results_common$ames$observed_na
gaps_na <- numeric(0)
counts_ok <- logical(0)
for (profile_name in names(rows_of_profile)) {
  all_rows <- rows_of_profile[[profile_name]]
  reporter_rows <- all_rows[reporters[all_rows]]
  by_hand_p8 <- average_by_hand(reporter_rows, 0, rep(-2, n_experts), price_b_c_own)
  by_hand_p12 <- average_by_hand(reporter_rows, 0, rep(2, n_experts), price_b_c_own)
  price_rows <- with_na$table %>%
    filter(profile == profile_name, effect == "price_a_up")
  gaps_na <- c(gaps_na, max(abs(in_outcome_order(price_rows, "ame") - (by_hand_p12 - by_hand_p8))))
  na_rows <- with_na$table %>%
    filter(profile == profile_name, effect == "na_effect")
  baseline_na <- with_na$baseline %>%
    filter(profile == profile_name)
  gaps_na <- c(gaps_na, max(abs(in_outcome_order(baseline_na, "probability") -
                                  average_by_hand(all_rows, 0, price_a_c_na, price_b_c_own))))
  counts_ok <- c(counts_ok, all(price_rows$n_experts == length(reporter_rows)),
                 all(na_rows$n_experts == length(all_rows)),
                 all(baseline_na$n_experts == length(all_rows)))
}
check("a price change averages over the experts who reported a price, by hand", max(gaps_na) < 1e-10)
check("and the table counts them; an effect that sets the variable to NA keeps everyone",
      all(counts_ok))
results_common <- call_observed(results_common, "observed_all", at_vignette, changes_na,
                                experts_na, profiles = profiles_observed, exclude_na = FALSE)
everyone <- results_common$ames$observed_all$table %>%
  filter(effect == "price_a_up")
check("exclude_na = FALSE: nobody is left out",
      identical(everyone$n_experts[everyone$outcome == "A"],
                as.integer(lengths(rows_of_profile))))

# Without `from` an effect starts at the experts as they are, and with exclude_na
# at those who answered: its start is the average over them, not the baseline. With
# NA in `from` it is the effect of a price of 12 against not answering, for everyone.
changes_partial <- list(
  to_only = list(to = list(price_a = 12)),
  from_na = list(from = list(price_a = NA_real_), to = list(price_a = 12))
)
messages_partial <- character(0)
results_common <- withCallingHandlers(
  call_observed(results_common, "observed_partial", at_vignette, changes_partial,
                experts_na, profiles = profiles_observed),
  message = function(m) {
    messages_partial <<- c(messages_partial, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
partial <- results_common$ames$observed_partial
gaps_partial <- numeric(0)
counts_partial <- logical(0)
for (profile_name in names(rows_of_profile)) {
  all_rows <- rows_of_profile[[profile_name]]
  reporter_rows <- all_rows[reporters[all_rows]]
  to_only_rows <- partial$table %>%
    filter(profile == profile_name, effect == "to_only")
  gaps_partial <- c(
    gaps_partial,
    max(abs(in_outcome_order(to_only_rows, "p_from") -
              average_by_hand(reporter_rows, 0, price_a_c_na, price_b_c_own))),
    max(abs(in_outcome_order(to_only_rows, "p_to") -
              average_by_hand(reporter_rows, 0, rep(2, n_experts), price_b_c_own)))
  )
  from_na_rows <- partial$table %>%
    filter(profile == profile_name, effect == "from_na")
  gaps_partial <- c(
    gaps_partial,
    max(abs(in_outcome_order(from_na_rows, "p_from") -
              average_by_hand(all_rows, 0, rep(0, n_experts), price_b_c_own))),
    max(abs(in_outcome_order(from_na_rows, "p_to") -
              average_by_hand(all_rows, 0, rep(2, n_experts), price_b_c_own)))
  )
  counts_partial <- c(counts_partial, all(to_only_rows$n_experts == length(reporter_rows)),
                      all(from_na_rows$n_experts == length(all_rows)))
}
check("no `from`, some experts left out: the start averages over those who answered, by hand",
      max(gaps_partial) < 1e-10)
check("and NA in `from` keeps everyone; the table counts both", all(counts_partial))
baseline_pooled <- partial$baseline %>%
  filter(profile == "All experts")
to_only_pooled <- partial$table %>%
  filter(profile == "All experts", effect == "to_only")
check("the start of such an effect is not the baseline of everyone",
      max(abs(in_outcome_order(to_only_pooled, "p_from") -
                in_outcome_order(baseline_pooled, "probability"))) > 1e-6)
check("the log says which effects leave experts out, and how many at most",
      any(grepl("experts left out of an effect, at most: to_only (40)", messages_partial, fixed = TRUE)))

# 16. The answers: the effect over the vignettes the experts answered -------------
# C: units are the 6 x 400 answers; at is empty
population_answers <- decisions %>%
  select(resp_uid, vig_num, expert_group, income, age, price_a, price_b)
changes_answers <- list(
  income_low = list(to = list(income = "low")),
  price_a_up = changes_observed$price_a_up
)
results_common <- call_observed(results_common, "answers", list(), changes_answers,
                                population_answers, profiles = list("All experts" = list()))
answers <- results_common$ames$answers
is_hvac_answer <- as.numeric(decisions$expert_group == "HVAC installers")
is_sweep_answer <- as.numeric(decisions$expert_group == "Chimney sweeps")
income_low_answer <- as.numeric(decisions$income == "low")
average_over_answers <- function(rows, income_low, price_a_c) {
  total <- rep(0, length(outcomes))
  for (i in rows) {
    total <- total + probabilities_common(is_hvac_answer[i], is_sweep_answer[i],
                                          income_low[i], price_a_c[i], decisions$price_b[i] - 20)
  }
  total / length(rows)
}
all_answers <- seq_len(nrow(decisions))
own_answers <- average_over_answers(all_answers, income_low_answer, decisions$price_a - 10)
check("C: the baseline is the average over all the answers, by hand",
      max(abs(in_outcome_order(answers$baseline, "probability") - own_answers)) < 1e-10)
# income_low sets `to` only: the answers already at a low income are left out, as
# the change does nothing to them (Max, 2026-11-14)
high_answers <- which(decisions$income == "high")
own_high <- average_over_answers(high_answers, income_low_answer, decisions$price_a - 10)
low_high <- average_over_answers(high_answers, rep(1, nrow(decisions)), decisions$price_a - 10)
income_answers <- answers$table %>%
  filter(effect == "income_low")
check("C: an effect over the answers, without those it does not change, by hand",
      max(abs(in_outcome_order(income_answers, "ame") - (low_high - own_high))) < 1e-10 &&
        all(income_answers$n_units == length(high_answers)))
other_answers <- answers$table %>%
  filter(effect != "income_low")
check("C: the units are the answers, the experts those who gave them",
      all(other_answers$n_units == nrow(decisions)) && all(other_answers$n_experts == n_experts))
observed_shares <- as.numeric(results_common$choice_counts[outcomes]) / results_common$fit$n_choices
# In a mixed logit only the average of the probabilities given each expert's own
# answers matches the shares; the unconditional average is close here, as the
# simulated constants vary little
check("C: over all answers the probabilities are close to the observed shares",
      max(abs(in_outcome_order(answers$baseline, "probability") - observed_shares)) < 0.02)

# 17. A model by group: the group of each expert sets their random part ---------
# The prices of B are fixed by `at`, so that only price A varies and the by-hand
# function of section 6 applies
at_fixed_b <- list(income = "high", price_b = 20)
results_group <- call_observed(results_group, "observed", at_fixed_b, changes_observed[1:2],
                               population_experts, profiles = profiles_observed)
observed_group <- results_group$ames$observed
gaps_group <- numeric(0)
for (profile_name in c("Energy advisers", "HVAC installers", "Chimney sweeps")) {
  rows <- rows_of_profile[[profile_name]]
  group_column <- c("Energy advisers" = "is_adviser", "HVAC installers" = "is_hvac",
                    "Chimney sweeps" = "is_sweep")[[profile_name]]
  total <- rep(0, length(outcomes))
  for (i in rows) {
    total <- total + probabilities_by_hand(group_column, results_group$coefficients,
                                           income_low = 0, price_a_c = price_a_c_own[i])
  }
  baseline_group <- observed_group$baseline %>%
    filter(profile == profile_name)
  gaps_group <- c(gaps_group,
                  max(abs(in_outcome_order(baseline_group, "probability") - total / length(rows))))
}
check("a model by group: each expert's own group's constants, averaged, by hand", max(gaps_group) < 1e-10)
pooled_group <- observed_group$baseline %>%
  filter(profile == "All experts") %>%
  arrange(match(outcome, outcomes))
weighted_group <- observed_group$baseline %>%
  filter(profile != "All experts") %>%
  group_by(outcome) %>%
  summarise(probability = sum(probability * n_experts) / sum(n_experts))
check("a model by group: the pooled average mixes the groups' constants in proportion",
      max(abs(pooled_group$probability -
                weighted_group$probability[match(pooled_group$outcome, weighted_group$outcome)])) < 1e-12)

# 18. Blocks and sessions: the same numbers however the work is divided ----------
# 2,400 answers x 4 states in blocks of fewer units, on one session and on two
results_one_session <- results_common
results_one_session$folder <- other_folder
results_one_session$ames <- list()
one_session <- compute_ames(results_one_session, make_regressors_na, name = "answers",
                            at = list(), changes = changes_answers, population = population_answers,
                            profiles = list("All experts" = list()), n_draws = N_PREDICTION_TEST,
                            n_workers = 1)
check("one session or two, the same numbers",
      max(abs(one_session$ames$answers$draws[["All experts"]] -
                answers$draws[["All experts"]])) < 1e-12)

# 19. What stops, and what is said --------------------------------------------------
stops_with("a population that is not the estimation sample stops", "estimation sample",
           call_observed(results_common, "x", at_vignette, changes_observed,
                         population_experts[1:300, ]))
results_other_population <- call_observed(results_one_session, "other", at_vignette,
                                          list(income_low = changes_observed$income_low),
                                          population_experts[1:300, ], check_sample = FALSE,
                                          profiles = list("All experts" = list()))
check("check_sample = FALSE: another population on purpose",
      all(results_other_population$ames$other$baseline$n_experts == 300))
stops_with("a profile that selects nobody stops", "selects no unit",
           call_observed(results_common, "x", at_vignette, changes_observed, population_experts,
                         profiles = list("Nobody" = list(expert_group = "Teachers"))))
stops_with("a profile that selects on an unknown column stops", "not in the population",
           call_observed(results_common, "x", at_vignette, changes_observed, population_experts,
                         profiles = list("Nobody" = list(speciality = "Teachers"))))
stops_with("a population without resp_uid stops", "resp_uid",
           call_observed(results_common, "x", at_vignette, changes_observed,
                         population_experts %>% select(-resp_uid)))
stops_with("a change of a variable that is nowhere stops", "not in `at`",
           call_observed(results_common, "x", at_vignette,
                         list(typo = list(to = list(incomme = "low"))), population_experts))
stops_with("NA in `to` only stops over a population", "NA on one side only",
           call_observed(results_common, "x", at_vignette,
                         list(bad = list(to = list(price_a = NA_real_))), population_experts))
stops_with("NA in `from` only stops: `to` must set the same variables", "same variables",
           call_observed(results_common, "x", at_vignette,
                         list(bad = list(from = list(price_a = NA_real_),
                                         to = list(income = "low"))), population_experts))
results_few_experts <- call_observed(
  results_one_session, "few", at_vignette, changes_observed[1], population_experts,
  profiles = list("Five experts" = list(resp_uid = experts$resp_uid[1:5]),
                  "Everyone" = list())
)
ame_few <- read_excel(file.path(results_few_experts$folder, "results.xlsx"), sheet = "ame_few")
check("a profile selected on any column; with five experts it shows its effects and its count (Max, 2026-11-12)",
      !any(is.na(results_few_experts$ames$few$table$ame)) && !any(is.na(ame_few$estimate)) &&
        all(ame_few$experts[ame_few$profile == "Five experts"] == 5))
price_none <- experts
price_none$price_a <- NA_real_
messages_empty <- character(0)
results_empty <- withCallingHandlers(
  call_observed(results_one_session, "empty", at_vignette, changes_observed[1:2],
                price_none, profiles = profiles_observed),
  message = function(m) {
    messages_empty <<- c(messages_empty, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
check("an effect that no profile includes is said so",
      any(grepl("or not answered): price_a_up", messages_empty, fixed = TRUE)))
price_never <- experts
price_never$price_a[price_never$expert_group == "Chimney sweeps"] <- NA
results_nobody <- call_observed(results_one_session, "nobody", at_vignette, changes_observed[2],
                                price_never, profiles = profiles_observed)
sweeps_price <- results_nobody$ames$nobody$table %>%
  filter(profile == "Chimney sweeps")
check("an effect that no expert of a profile is included in: blank, counted as 0 experts",
      all(is.na(sweeps_price$ame)) && all(sweeps_price$n_experts == 0) &&
        !any(is.na((results_nobody$ames$nobody$table %>% filter(profile == "HVAC installers"))$ame)))

# 20. Loading and the workbook ---------------------------------------------------
messages_loaded <- character(0)
results_common <- withCallingHandlers(
  call_observed(results_common, "observed", at_vignette, changes_observed, population_experts,
                profiles = profiles_observed),
  message = function(m) {
    messages_loaded <<- c(messages_loaded, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
check("the same call over observed experts loads", any(grepl("nothing changed", messages_loaded)))
experts_changed <- experts
experts_changed$price_a[1] <- experts_changed$price_a[1] + 1
messages_changed <- character(0)
results_changed <- withCallingHandlers(
  call_observed(results_common, "observed", at_vignette, changes_observed, experts_changed,
                profiles = profiles_observed),
  message = function(m) {
    messages_changed <<- c(messages_changed, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
check("another price of one expert computes again", !any(grepl("nothing changed", messages_changed)))
workbook_observed <- file.path(results_changed$folder, "results.xlsx")
ame_observed <- read_excel(workbook_observed, sheet = "ame_observed")
print(ame_observed, n = 8)
check("the sheet over experts counts the experts after each effect",
      identical(names(ame_observed)[1:4], c("profile", "effect", "outcome", "experts")) &&
        ame_observed$experts[1] == n_experts && !"answers" %in% names(ame_observed))
ame_answers <- read_excel(workbook_observed, sheet = "ame_answers")
check("the sheet over answers counts the answers too",
      ame_answers$answers[1] == nrow(decisions) && ame_answers$experts[1] == n_experts)
definitions_observed <- read_excel(workbook_observed, sheet = "effect_definitions")
check("the definitions say that the population was observed",
      any(definitions_observed$effects == "observed" & definitions_observed$effect == "population" &
            definitions_observed$variable == "experts" & definitions_observed$value == as.character(n_experts)))
ame_reference <- read_excel(workbook_observed, sheet = "ame_base")
check("a reference expert's sheet has no column of experts", !"experts" %in% names(ame_reference))
# The main set leads the workbook, also if it was computed last (Max, 2026-11-14)
results_main <- call_observed(results_changed, MAIN_AMES, at_vignette, changes_observed[1],
                              experts_changed, profiles = profiles_observed)
# intersect(): the sheets of the sets, in the order of the workbook
ame_sheets <- intersect(excel_sheets(workbook_observed), paste0("ame_", names(results_main$ames)))
check("the main set (MAIN_AMES) is the first sheet of effects, one sheet per set",
      ame_sheets[1] == paste0("ame_", MAIN_AMES) && length(ame_sheets) == length(results_main$ames))

# 21. What an effect leaves out, and the draws it keeps (Max, 2026-11-14) --------
# The design rule: 128 of the 192 combinations of the levels, and every vignette of
# the design file. expand.grid(): every combination of the levels.
levels_of_attribute <- list()
for (attribute in unique(DESIGN_LEVELS$attribute)) {
  levels_of_attribute[[attribute]] <- DESIGN_LEVELS$level[DESIGN_LEVELS$attribute == attribute]
}
all_combinations <- expand.grid(levels_of_attribute, stringsAsFactors = FALSE)
design_file <- read_excel(VIGNETTE_FILE) %>%
  mutate(across(everything(), as.character)) %>%
  rename(att_current_heating = bestehende_heizung, att_build_year = baujahr,
         att_heat_demand = waermebedarf_kwh_m2)
check("the design rule keeps 128 of the 192 combinations, and every vignette of the design",
      nrow(all_combinations) == 192 && sum(is_in_design(all_combinations)) == 128 &&
        nrow(design_file) == 128 && all(is_in_design(design_file)))

# Each expert with a house (which the model does not use): 1970 at 120 kWh, 1970 at
# 200 kWh, 2000 at 70 kWh. A change to oil heating leaves out the houses built in
# 2000; a change to 2000 alone moves no regressor of this model and leaves out
# everyone. A unit already at the level named: income_low in section 16.
house_of_expert <- rep(1:3, length.out = n_experts)
population_houses <- experts %>%
  mutate(att_build_year = c("1970", "1970", "2000")[house_of_expert],
         att_current_heating = "Gasheizung",
         att_heat_demand = c("120", "200", "70")[house_of_expert])
changes_design <- list(
  income_and_oil = list(from = list(income = "high", att_current_heating = "Gasheizung"),
                        to = list(income = "low", att_current_heating = "Ölheizung")),
  to_2000 = list(to = list(att_build_year = "2000"))
)
results_design <- call_observed(results_one_session, "design", at_vignette, changes_design,
                                population_houses, profiles = list("All experts" = list()))
rows_1970 <- which(house_of_expert != 3)
by_hand_design <- average_by_hand(rows_1970, 1, price_a_c_own, price_b_c_own) -
  average_by_hand(rows_1970, 0, price_a_c_own, price_b_c_own)
oil_rows <- results_design$ames$design$table %>%
  filter(effect == "income_and_oil")
check("a unit that a change takes outside the design is left out: by hand",
      max(abs(in_outcome_order(oil_rows, "ame") - by_hand_design)) < 1e-10 &&
        all(oil_rows$n_experts == length(rows_1970)))
to_2000_rows <- results_design$ames$design$table %>%
  filter(effect == "to_2000")
check("a change of an attribute that the model does not use leaves out every unit",
      all(to_2000_rows$n_experts == 0) && all(is.na(to_2000_rows$ame)))

# At a household built in 2000, a change to oil heating is blank, and said so
at_household <- c(at_base, list(att_build_year = "2000", att_current_heating = "Gasheizung",
                                att_heat_demand = "70"))
messages_household <- character(0)
results_household <- withCallingHandlers(
  call_ames(results_common, "household", at = at_household,
            changes = list(to_oil = list(to = list(att_current_heating = "Ölheizung")),
                           income_low = list(to = list(income = "low")))),
  message = function(m) {
    messages_household <<- c(messages_household, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
household_table <- results_household$ames$household$table
check("a change that leaves the design at a household is blank there, and said so",
      all(is.na(household_table$ame[household_table$effect == "to_oil"])) &&
        !any(is.na(household_table$ame[household_table$effect == "income_low"])) &&
        any(grepl("or not answered): to_oil", messages_household, fixed = TRUE)))

# A covariate that make_regressors() uses for one group only: its change moves no
# regressor of the other groups, which are blank (Claude's proposal, 2026-11-14)
make_regressors_hvac_age <- function(states) {
  make_regressors(states) %>%
    mutate(age_c = if_else(expert_group == "HVAC installers", age_c, 0))
}
results_hvac_age <- compute_ames(results_age, make_regressors_hvac_age, name = "hvac_age",
                                 at = at_age, changes = changes_age,
                                 n_draws = N_PREDICTION_TEST, n_workers = N_WORKERS_TEST)
hvac_age_table <- results_hvac_age$ames$hvac_age$table
hvac_rows <- hvac_age_table$profile == "HVAC installers"
age_hvac_rows <- results_age$ames$age$table %>%
  filter(profile == "HVAC installers", effect == "age_up")
check("a covariate that enters for one group only: blank for the others, as before for that group",
      all(is.na(hvac_age_table$ame[!hvac_rows])) && all(hvac_age_table$n_units[!hvac_rows] == 0) &&
        max(abs(hvac_age_table$ame[hvac_rows] - age_hvac_rows$ame)) < 1e-12)

stops_with("a change whose `from` and `to` set different variables stops", "same variables",
           call_ames(results_common, "x", changes = list(bad = list(from = list(price_a = 8),
                                                                    to = list(price_b = 25)))))

# keep_draws = FALSE saves no draws and the same tables
results_with_draws <- call_observed(results_one_session, "with_draws", at_vignette,
                                    changes_observed[1:2], population_experts,
                                    profiles = profiles_observed)
results_no_draws <- call_observed(results_with_draws, "no_draws", at_vignette,
                                  changes_observed[1:2], population_experts,
                                  profiles = profiles_observed, keep_draws = FALSE)
check("keep_draws = FALSE: no draws saved, the same tables",
      "draws" %in% names(results_no_draws$ames$no_draws) &&
        is.null(results_no_draws$ames$no_draws$draws) &&
        identical(results_no_draws$ames$no_draws$table, results_no_draws$ames$with_draws$table))

# The log prints no count below the minimum cell; the table keeps it, as results.rds
# stays on the secure drive (Max, 2026-11-12)
experts_five_na <- experts
experts_five_na$price_a[1:5] <- NA
messages_five <- character(0)
results_five <- withCallingHandlers(
  call_observed(results_one_session, "five_na", at_vignette, changes_observed[2],
                experts_five_na, profiles = list("All experts" = list())),
  message = function(m) {
    messages_five <<- c(messages_five, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
check("the log does not print a count below the minimum cell, the table does",
      any(grepl("price_a_up (fewer than 10)", messages_five, fixed = TRUE)) &&
        all(results_five$ames$five_na$table$n_experts == n_experts - 5))

# A call where nobody is left out raises no warning (max() of nothing would)
warnings_seen <- character(0)
results_quiet <- withCallingHandlers(
  call_ames(results_common, "quiet", changes = changes_moving, force = TRUE),
  warning = function(w) {
    warnings_seen <<- c(warnings_seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
check("a call where nobody is left out raises no warning", length(warnings_seen) == 0)

cat("compute_ames: all checks passed\n")
