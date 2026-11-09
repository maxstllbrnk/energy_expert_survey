################################################################################
# test_compute_ames.R — compute_ames() and results.xlsx on small simulated models
# Inputs : simulated decisions, drawn here
# Outputs: in <SECURE_DIR>/output/test_compute_ames/
# Checks : probabilities sum to 1; from equal to to gives 0; a change in `at` moves
#          the reference; a model by group uses the group's constants; a covariate
#          shows up without other changes; L near 0 gives the closed form; extreme
#          draws stay finite; loading; what fit_mixed_logit() refuses; the workbook
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
N_WORKERS_TEST <- 4

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
      all(c("table", "baseline", "draws", "at", "changes", "profiles", "n_draws",
            "fingerprint", "date") %in% names(ames)) &&
        identical(names(ames$table), c("profile", "effect", "outcome", "p_from", "p_to", "ame",
                                       "se", "z", "p", "ci_low", "ci_high", "ci_low_kr", "ci_high_kr")) &&
        identical(names(ames$baseline), c("profile", "outcome", "probability", "se", "ci_low_kr",
                                          "ci_high_kr")))
sum_baseline <- ames$baseline %>%
  group_by(profile) %>%
  summarise(total = sum(probability))
check("probabilities at the estimates sum to 1", all(abs(sum_baseline$total - 1) < 1e-12))
sum_draws <- apply(ames$draws[["HVAC installers"]], c(1, 3), sum)
check("probabilities at every Krinsky-Robb draw sum to 1", all(abs(sum_draws - 1) < 1e-12))
sum_effects <- ames$table %>%
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
check("from equal to to gives an effect of exactly 0, with no spread",
      all(same_state$ame == 0) && all(same_state$se == 0))
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
check("in a model without the covariate, its effect is 0", all(results_common$ames$age$table$ame == 0))
check("and it says that the change moves nothing", any(grepl("move nothing in this model: age_up", messages_age, fixed = TRUE)))
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
stops_with("a name without a sheet stops", "at most 22",
           call_ames(results_common, "a_name_of_more_than_twenty_two_characters"))
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
  "pp_base", "effect_definitions", "choice_counts", "fit", "coefficients", "ame_base",
  "baseline_base"
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
pp <- read_excel(workbook, sheet = "pp_base")
print(pp, n = 7)
check("pp: the probabilities first, in percentage points",
      pp$effect[1] == "Probability at the reference state" &&
        abs(sum(pp[1, outcomes]) - 100) < 0.1)
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

cat("compute_ames: all checks passed\n")
