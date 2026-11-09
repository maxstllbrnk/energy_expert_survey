################################################################################
# test_compare_models.R — the protocol of compare_models() where the truth is known
# Inputs : simulated decisions, drawn here, with known parameters
# Outputs: in <SECURE_DIR>/output/test_compare_models/
# Checks : an irrelevant block is rejected and a relevant one accepted; a large
#          model that buys little is caught; levels against logs run and report; the
#          LR on fresh draws is close to the LR if the gain is large; a smaller model
#          above the larger is flagged; an effect only the larger model has counts;
#          a rare dummy is flagged; a cut-off cross-validation resumes; a test that
#          cannot be computed is not compared; two samples stop; the workbook reads
#          well
################################################################################
# 14 minutes on 4 sessions on a busy machine, from nothing; models and folds are
# saved, so a second run takes 4. 2 sessions here, as the machine is shared.
# The AMEs are made by hand here, following docs/results_contract.md.

source(file.path("R", "setup.R"))
est_dir <- file.path(OUTPUT_DIR, "test_compare_models")
dir.create(est_dir, recursive = TRUE, showWarnings = FALSE)
n_experts <- 300
n_draws_test <- 100
n_workers_test <- 2
start_time <- Sys.time()

# 1. Simulated decisions -------------------------------------------------------
# A mixed logit with correlated normal constants. x_relevant matters for every
# technology, the noise_ columns for none, the small_ columns a little for all. The
# price enters in levels. The covariances of the constants are the same in every
# group, except in the second data set.
simulate_decisions <- function(seed, id_prefix, L_by_group) {
  set.seed(seed)
  outcomes <- c("Gasheizung", "Keine Empfehlung", "Wärmepumpe", "Ölheizung")
  technologies <- c("Gasheizung", "Keine Empfehlung", "Ölheizung")
  group_names <- c("adviser", "hvac", "sweep")
  experts <- tibble(
    resp_uid = sprintf("%s%04d", id_prefix, seq_len(n_experts)),
    group = sample(group_names, n_experts, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    is_adviser = as.numeric(group == "adviser"),
    is_hvac = as.numeric(group == "hvac"),
    is_sweep = as.numeric(group == "sweep")
  )
  true_constants <- c(-0.2, -0.8, -1.0)
  constants <- matrix(0, n_experts, 3, dimnames = list(NULL, technologies))
  for (i in seq_len(n_experts)) {
    constants[i, ] <- true_constants +
      as.vector(L_by_group[[experts$group[i]]] %*% rnorm(3))
  }

  decisions <- experts %>%
    cross_join(tibble(vig_num = 1:6)) %>%
    mutate(chid = row_number(),
           expert_number = match(resp_uid, experts$resp_uid),
           att_a = rbinom(n(), 1, 0.5),
           att_b = rbinom(n(), 1, 0.5),
           x_relevant = rnorm(n()),
           noise_1 = rnorm(n()),
           noise_2 = rnorm(n()))
  small_columns <- paste0("small_", 1:8)
  for (column in small_columns) {
    decisions[[column]] <- rnorm(nrow(decisions))
  }

  true_beta <- rbind(
    att_a = c(0.5, 0.2, -0.4),
    att_b = c(-0.6, 0.1, 0.3),
    is_hvac = c(0.3, -0.2, 0.2),
    is_sweep = c(0.5, 0, 0.4),
    x_relevant = c(0.5, -0.3, 0.4)
  )
  colnames(true_beta) <- technologies
  true_small <- matrix(sample(c(-0.045, 0.045), 24, replace = TRUE), 8, 3,
                       dimnames = list(small_columns, technologies))
  true_price <- -0.12

  price <- matrix(10 * exp(rnorm(nrow(decisions) * 4, 0, 0.6)), nrow(decisions), 4,
                  dimnames = list(NULL, outcomes))
  utility <- true_price * price
  for (technology in technologies) {
    utility[, technology] <- utility[, technology] +
      constants[decisions$expert_number, technology] +
      as.vector(as.matrix(decisions[, rownames(true_beta)]) %*% true_beta[, technology]) +
      as.vector(as.matrix(decisions[, small_columns]) %*% true_small[, technology])
  }
  utility <- utility - log(-log(matrix(runif(length(utility)), nrow(utility), 4)))
  chosen_outcome <- outcomes[max.col(utility, ties.method = "first")]

  choice_long <- decisions %>%
    mutate(noise_3 = rnorm(n()), noise_4 = rnorm(n())) %>%
    cross_join(tibble(alternative = factor(outcomes, levels = outcomes))) %>%
    arrange(chid, alternative) %>%
    mutate(price = as.vector(t(price)),
           log_price = log(price),
           chosen = as.character(alternative) == chosen_outcome[chid])
  dfidx(choice_long, idx = list(c("chid", "resp_uid"), "alternative"),
        choice = "chosen", shape = "long")
}

L_common <- rbind(c(0.9, 0, 0), c(0.3, 0.7, 0), c(-0.2, 0.2, 0.6))
L_same <- list(adviser = L_common, hvac = L_common, sweep = L_common)
L_different <- list(adviser = L_common, hvac = 1.8 * L_common,
                    sweep = rbind(c(0.3, 0, 0), c(-0.2, 1.3, 0), c(0.5, -0.5, 0.3)))
choice_data <- simulate_decisions(20261111, "e", L_same)
choice_data_groups <- simulate_decisions(20261112, "f", L_different)

# 2. The models ----------------------------------------------------------------
reference <- "Wärmepumpe"
technologies <- c("Gasheizung", "Keine Empfehlung", "Ölheizung")
groups <- c("is_adviser", "is_hvac", "is_sweep")
fit_and_post <- function(name, formula, data, start_from = NULL,
                         group_constants = NULL) {
  model <- fit_mixed_logit(name, formula, data, reference, est_dir,
                           group_constants = group_constants, start_from = start_from,
                           n_draws = n_draws_test)
  results <- post_estimation(model, est_dir, label = name, n_draws_kr = 50,
                             n_workers = n_workers_test)
  model
}

m_base <- fit_and_post(
  "m_base", chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b, choice_data
)
m_relevant <- fit_and_post(
  "m_relevant", chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant,
  choice_data, start_from = m_base
)
m_irrelevant <- fit_and_post(
  "m_irrelevant",
  chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant + noise_1 + noise_2,
  choice_data, start_from = m_relevant
)
m_other_noise <- fit_and_post(
  "m_other_noise",
  chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant + noise_3 + noise_4,
  choice_data, start_from = m_relevant
)
m_other_terms <- fit_and_post(
  "m_other_terms",
  chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant + small_1 + small_2,
  choice_data, start_from = m_relevant
)
m_many <- fit_and_post(
  "m_many",
  chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant + small_1 + small_2 +
    small_3 + small_4 + small_5 + small_6 + small_7 + small_8,
  choice_data, start_from = m_relevant
)
m_logs <- fit_and_post(
  "m_logs", chosen ~ log_price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant,
  choice_data
)
m_by_group <- fit_and_post(
  "m_by_group",
  chosen ~ price | 0 + is_adviser + is_hvac + is_sweep + att_a + att_b + x_relevant,
  choice_data, start_from = m_relevant, group_constants = groups
)

# The second data set: the covariances truly differ by group
m_common_2 <- fit_and_post(
  "m_common_2", chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant,
  choice_data_groups
)
m_by_group_2 <- fit_and_post(
  "m_by_group_2",
  chosen ~ price | 0 + is_adviser + is_hvac + is_sweep + att_a + att_b + x_relevant,
  choice_data_groups, start_from = m_common_2, group_constants = groups
)

# Another sample: 200 of the experts
experts_part <- sort(unique(as.character(idx(choice_data)$resp_uid)))[1:200]
choice_data_part <- choice_data[as.character(idx(choice_data)$resp_uid) %in% experts_part, ]
m_other_sample <- fit_and_post(
  "m_other_sample", chosen ~ price | 1 + is_hvac + is_sweep + att_a + att_b + x_relevant,
  choice_data_part
)

# A larger model that stopped short of its maximum, below the model it nests, as
# (3pg) and (4pd) did: m_irrelevant at m_relevant's estimates, moved a little. Its
# code says converged.
dir.create(file.path(est_dir, "m_below"), recursive = TRUE, showWarnings = FALSE)
arguments <- as.list(m_irrelevant$call)[-1]
arguments$data <- quote(choice_data)
start_below <- setNames(rep(0, length(coef(m_irrelevant))), names(coef(m_irrelevant)))
start_below[names(coef(m_relevant))] <- coef(m_relevant)
constants_to_move <- paste0("(Intercept):", technologies)
start_below[constants_to_move] <- start_below[constants_to_move] + 0.1
arguments$start <- start_below
arguments$iterlim <- 0
arguments$print.level <- 0
m_below <- do.call("mlogit", arguments)
m_below$est.stat$code <- 1
m_below$name <- "m_below"
m_below$fit_info <- m_irrelevant$fit_info
saveRDS(m_below, file.path(est_dir, "m_below", "model.rds"))
results_below <- post_estimation(m_below, est_dir, label = "Stopped short",
                                 n_draws_kr = 50, n_workers = n_workers_test)

# 3. AMEs by hand, following docs/results_contract.md --------------------------
# m_base and m_relevant: the largest change in an AME is known. m_irrelevant has no
# AMEs at all. One effect of m_relevant's is not in m_base's, as a model has the
# effects of its terms.
make_ames <- function(values, se_values, baseline_values) {
  effects <- c("income_low", "gas_q25_q75", "heat_pump_home")
  changes <- list(
    income_low = list(to = list(att_income = "unter 40.000 € brutto im Jahr")),
    gas_q25_q75 = list(from = list(price_gas_kwh = 10), to = list(price_gas_kwh = 14)),
    heat_pump_home = list(to = list(heat_pump_at_home = 1))
  )
  outcomes <- c("Wärmepumpe", "Gasheizung")
  ame_table <- tibble(
    profile = rep(EXPERT_GROUPS, each = length(effects) * length(outcomes)),
    effect = rep(rep(effects, each = length(outcomes)), times = length(EXPERT_GROUPS)),
    outcome = rep(outcomes, times = length(effects) * length(EXPERT_GROUPS)),
    p_from = 0.4,
    p_to = 0.4 + values,
    ame = values,
    se = se_values
  ) %>%
    mutate(z = ame / se,
           p = 2 * pnorm(-abs(z)),
           ci_low = ame - 1.96 * se,
           ci_high = ame + 1.96 * se,
           ci_low_kr = ci_low,
           ci_high_kr = ci_high)
  baseline <- tibble(
    profile = rep(EXPERT_GROUPS, each = length(outcomes)),
    outcome = rep(outcomes, times = length(EXPERT_GROUPS)),
    probability = baseline_values,
    se = 0.01
  ) %>%
    mutate(ci_low_kr = probability - 1.96 * se,
           ci_high_kr = probability + 1.96 * se)
  list(at_means = list(table = ame_table, baseline = baseline, draws = NULL,
                       at = list(price_gas_kwh = 12), changes = changes,
                       profiles = EXPERT_GROUPS, n_draws = 1000,
                       fingerprint = "by hand", date = Sys.Date()))
}
set_ames <- function(model_name, ames) {
  results_file <- file.path(est_dir, model_name, "results.rds")
  results <- readRDS(results_file)
  results$ames <- ames
  saveRDS(results, results_file)
}
n_ame_rows <- 18
n_baseline_rows <- 6
ame_values <- seq(-0.09, 0.08, length.out = n_ame_rows)
se_values <- rep(0.02, n_ame_rows)
baseline_values <- seq(0.2, 0.45, length.out = n_baseline_rows)
set_ames("m_base", make_ames(ame_values, se_values, baseline_values))
# The largest change: 3 pp, in row 4 (Energy advisers, gas_q25_q75, Gasheizung); the
# median 0.5 pp
ame_changed <- ame_values + 0.005
ame_changed[4] <- ame_values[4] + 0.03
ames_relevant <- make_ames(ame_changed, se_values, baseline_values + 0.002)
ames_relevant$at_means$table$effect[18] <- "only_in_this_model"
ames_relevant$at_means$changes$only_in_this_model <- list(to = list(x_relevant = 1))
set_ames("m_relevant", ames_relevant)

# 4. The full protocol ---------------------------------------------------------
tests <- tribble(
  ~question,                               ~smaller,      ~larger,
  "Does the relevant block matter?",       "m_base",      "m_relevant",
  "Does the irrelevant block matter?",     "m_relevant",  "m_irrelevant",
  "Do many small terms matter?",           "m_relevant",  "m_many",
  "Levels or logs?",                       "m_relevant",  "m_logs",
  "Do covariances differ? (they do not)",  "m_relevant",  "m_by_group"
)
proposal_of <- function(decision, smaller_name, larger_name) {
  decision$proposal[decision$smaller == smaller_name & decision$larger == larger_name]
}
comparison <- compare_models(
  est_dir,
  tests = tests,
  blocks = list(price = "price", relevant = "x_relevant",
                attributes = c("att_a", "att_b")),
  final_model = "m_relevant",
  ame_name = "at_means",
  cv_models = c("m_base", "m_relevant", "m_many", "m_logs"),
  n_fresh_draws = 8,
  n_workers = n_workers_test
)
decision <- comparison$decision
print(comparison$validity %>%
        select(model, code, iterations, scaled_gradient, smallest_eigenvalue,
               condition_number, gap_to_nested, valid),
      width = Inf)
print(comparison$nested_tests %>%
        select(larger, restriction, lr, df, p, p_robust, p_robust_holm, mean_weight,
               critical_value, lr_fresh_mean, lr_fresh_sd),
      width = Inf)

# A truly relevant block is accepted, a truly irrelevant one is not
stopifnot(identical(proposal_of(decision, "m_base", "m_relevant"), "accept larger"))
stopifnot(identical(proposal_of(decision, "m_relevant", "m_irrelevant"), "keep smaller"))
# A large model that buys little is caught: the test says yes, cross-validation no
stopifnot(identical(proposal_of(decision, "m_relevant", "m_many"),
                    "keep smaller: tests and parsimony disagree"))
# Levels against logs: runs and reports; the truth is levels
row_logs <- decision %>%
  filter(larger == "m_logs")
stopifnot(row_logs$comparison == "non-nested",
          !is.na(row_logs$vuong_z_bic),
          !is.na(row_logs$cv_gain_per_expert),
          row_logs$proposal == "prefer m_relevant")
row_vuong <- comparison$non_nested_tests
log_likelihood_logs <- as.numeric(logLik(m_logs))
log_likelihood_levels <- as.numeric(logLik(m_relevant))
stopifnot(nrow(row_vuong) == 1,
          grepl("Shi 2015", row_vuong$note[1], fixed = TRUE),
          abs(row_vuong$vuong_z_bic - sqrt(n_experts) * row_vuong$difference_per_expert /
                row_vuong$sd_per_expert) < 1e-8,
          abs(row_vuong$log_likelihood_difference -
                (log_likelihood_logs - log_likelihood_levels)) < 1e-6)
# Covariances that do not differ: df 2 x 6 = 12, by the group rule
row_groups <- decision %>%
  filter(larger == "m_by_group")
stopifnot(row_groups$comparison == "group covariances",
          row_groups$df == 12,
          row_groups$proposal == "keep smaller")

# The restrictions of the group rule are 0 at the common model written by group, and
# not 0 at the estimates by group
results_common <- readRDS(file.path(est_dir, "m_relevant", "results.rds"))
results_group <- readRDS(file.path(est_dir, "m_by_group", "results.rds"))
relationship <- nesting_of(results_common, results_group)
stopifnot(relationship$type == "group covariances",
          length(relationship$added) == 0,
          relationship$groups_without_shift == "is_adviser")
theta_common <- results_common$coefficients
theta_group <- results_group$coefficients
for (technology in technologies) {
  intercept <- theta_common[[paste0("(Intercept):", technology)]]
  theta_group[[paste0("is_adviser:", technology)]] <- intercept
  theta_group[[paste0("is_hvac:", technology)]] <- intercept +
    theta_common[[paste0("is_hvac:", technology)]]
  theta_group[[paste0("is_sweep:", technology)]] <- intercept +
    theta_common[[paste0("is_sweep:", technology)]]
}
for (cholesky_name in names(theta_common)[startsWith(names(theta_common), "chol.")]) {
  for (group in groups) {
    theta_group[[gsub("(Intercept)", group, cholesky_name, fixed = TRUE)]] <-
      theta_common[[cholesky_name]]
  }
}
stopifnot(max(abs(restrictions_of(theta_group, relationship, results_group$random))) < 1e-12,
          max(abs(restrictions_of(results_group$coefficients, relationship,
                                  results_group$random))) > 0.01)
# A model without the group shifts: the means of the groups are restricted as well. A
# model by group is not a special case of one set of constants.
shift_names <- c(paste0("is_hvac:", technologies), paste0("is_sweep:", technologies))
results_no_shift <- results_common
results_no_shift$coefficients <- theta_common[setdiff(names(theta_common), shift_names)]
relationship_means <- nesting_of(results_no_shift, results_group)
stopifnot(relationship_means$type == "group covariances and means",
          identical(relationship_means$groups_without_shift, groups),
          length(restrictions_of(results_group$coefficients, relationship_means,
                                 results_group$random)) == 12 + 6,
          nesting_of(results_group, results_common)$type == "non-nested")

# The LR on fresh draws: close to the LR if the gain is large, and the noise is
# reported. With no shift, the draws are the estimation's, with one set of constants
# and by group.
row_relevant <- decision %>%
  filter(larger == "m_relevant")
stopifnot(abs(row_relevant$lr_fresh_mean - row_relevant$lr) / row_relevant$lr < 0.1,
          row_relevant$lr_fresh_sd > 0)
fresh_of_models <- comparison$fresh_draws %>%
  filter(row_type == "log-likelihood of the model")
cat("SD of a log-likelihood across fresh sets:", round(fresh_of_models$sd_fresh, 2), "\n")
stopifnot(all(fresh_of_models$sd_fresh > 0),
          all(fresh_of_models$min_fresh < fresh_of_models$max_fresh))
for (model_name in c("m_relevant", "m_by_group")) {
  model <- readRDS(file.path(est_dir, model_name, "model.rds"))
  results <- readRDS(file.path(est_dir, model_name, "results.rds"))
  K_r <- nrow(results$random)
  prepared <- prepare_likelihood(model, z_all = matrix(0, 1, K_r))
  prepared$Z <- NULL
  primes <- HALTON_PRIMES[seq_len(K_r)]
  if (is.list(eval(model$call$halton))) {
    primes <- eval(model$call$halton)$prime
  }
  sequences <- list()
  for (prime in unique(primes)) {
    sequences[[as.character(prime)]] <- mlogit:::halton(
      prime, prepared$n_draws * prepared$n_experts, 100
    )
  }
  no_shift <- matrix(0, 1, length(unique(primes)),
                     dimnames = list(NULL, unique(primes)))
  value <- fresh_log_likelihood(1, results$coefficients, prepared, sequences, primes,
                                no_shift)
  stopifnot(abs(value - results$fit$log_likelihood) < 1e-8)
}

# Cross-validation: every fold ran, the groups are spread over the folds, the folds are
# the same for all models, a gain is the difference of the means, and out of sample is
# worse than in sample
cv_rows <- comparison$cross_validation %>%
  filter(row_type == "model")
print(comparison$cross_validation %>%
        select(row_type, item, held_out_per_expert, minutes, gain_per_expert, paired_se,
               favours, usable),
      n = Inf, width = Inf)
stopifnot(all(cv_rows$folds_converged == 5), all(cv_rows$experts == n_experts),
          grepl("stratified by", comparison$cross_validation$note[1], fixed = TRUE))
for (model_name in cv_rows$item) {
  fit_of_model <- comparison$fit %>%
    filter(model == model_name)
  stopifnot(cv_rows$held_out_per_expert[cv_rows$item == model_name] <
              fit_of_model$log_likelihood / n_experts)
}
saved_base <- readRDS(file.path(est_dir, "m_base", "cross_validation.rds"))$per_expert
saved_many <- readRDS(file.path(est_dir, "m_many", "cross_validation.rds"))$per_expert
stopifnot(identical(arrange(saved_base, expert)$fold, arrange(saved_many, expert)$fold))
group_of_expert <- as.data.frame(choice_data) %>%
  transmute(expert = as.character(idx(choice_data)$resp_uid), is_hvac, is_sweep) %>%
  distinct()
folds_by_group <- saved_base %>%
  left_join(group_of_expert, by = "expert", relationship = "one-to-one") %>%
  count(is_hvac, is_sweep, fold) %>%
  group_by(is_hvac, is_sweep) %>%
  summarise(spread = max(n) - min(n), folds = n())
stopifnot(all(folds_by_group$spread <= 1), all(folds_by_group$folds == 5))
pair_relevant <- comparison$cross_validation %>%
  filter(item == "m_base vs m_relevant")
stopifnot(abs(pair_relevant$gain_per_expert -
                (cv_rows$held_out_per_expert[cv_rows$item == "m_relevant"] -
                   cv_rows$held_out_per_expert[cv_rows$item == "m_base"])) < 1e-10)

# The AMEs: the numbers set by hand, on the rows the two models share
ame_row <- comparison$ame_changes %>%
  filter(smaller == "m_base", larger == "m_relevant", quantity == "AME")
stopifnot(ame_row$rows == n_ame_rows - 1,
          abs(ame_row$largest_change_pp - 3) < 1e-8,
          abs(ame_row$median_change_pp - 0.5) < 1e-8,
          abs(ame_row$largest_change_in_se - 1.5) < 1e-8,
          ame_row$where_largest == "Energy advisers, gas_q25_q75, Gasheizung")
baseline_row <- comparison$ame_changes %>%
  filter(smaller == "m_base", larger == "m_relevant", quantity == "baseline probability")
stopifnot(baseline_row$rows == n_baseline_rows,
          abs(baseline_row$largest_change_pp - 0.2) < 1e-8)
# The effect only m_relevant has counts as moving from 0: 8.5 pp
added_row <- comparison$ame_changes %>%
  filter(smaller == "m_base", larger == "m_relevant",
         quantity == "AME the larger model adds")
stopifnot(added_row$rows == 1, abs(added_row$largest_change_pp - 8.5) < 1e-8,
          added_row$rows_moved == 1)
# m_irrelevant has no AMEs: no row, and the decision says not available
stopifnot(!any(comparison$ame_changes$larger == "m_irrelevant"),
          decision$verdict_ame[decision$larger == "m_irrelevant"] == "not available")

# Absolute fit: predicted shares close to the observed, and the levels are the attributes'
fit_all <- comparison$absolute_fit %>%
  filter(level == "root mean squared difference: all decisions")
print(fit_all %>% select(model, rmse_pp))
stopifnot(all(fit_all$rmse_pp < 2))
attribute_levels <- comparison$absolute_fit %>%
  filter(model == "m_base", !is.na(decisions)) %>%
  pull(level) %>%
  unique()
stopifnot(identical(sort(attribute_levels),
                    sort(c("all decisions", "att_a = 0", "att_a = 1", "att_b = 0",
                           "att_b = 1"))))
one_level <- comparison$absolute_fit %>%
  filter(model == "m_base", level == "att_a = 1")
stopifnot(abs(sum(one_level$observed_percent) - 100) < 1e-8,
          abs(sum(one_level$predicted_percent) - 100) < 1e-6)
# The observed share by hand, in the outcome and level of one row
is_level <- as.numeric(m_base$model$att_a) == 1
is_outcome <- as.character(idx(m_base)$alternative) == "Gasheizung"
share_by_hand <- 100 * sum(as.logical(m_base$model$chosen) & is_level & is_outcome) /
  sum(is_level & is_outcome)
stopifnot(abs(one_level$observed_percent[one_level$outcome == "Gasheizung"] -
                share_by_hand) < 1e-8)

# Blocks: with one coefficient, the Wald statistic is its z squared
block_price <- comparison$block_tests %>%
  filter(block == "price")
row_price <- results_common$coefficient_table$parameter == "price"
z_price <- results_common$coefficient_table$z[row_price]
stopifnot(abs(block_price$statistic - z_price^2) < 1e-8, block_price$df == 1,
          comparison$block_tests$df[comparison$block_tests$block == "relevant"] == 3,
          comparison$block_tests$df[comparison$block_tests$block == "attributes"] == 6)

# 5. The other verdicts of the rule --------------------------------------------
# (a) Covariances that truly differ: accepted
comparison_groups <- compare_models(
  est_dir,
  tests = tibble(question = "Do covariances differ? (they do)",
                 smaller = "m_common_2", larger = "m_by_group_2"),
  cv_models = c("m_common_2", "m_by_group_2"),
  n_fresh_draws = 4,
  n_workers = n_workers_test
)
stopifnot(comparison_groups$decision$proposal == "accept larger",
          comparison_groups$decision$comparison == "group covariances")

# (b) Without cross-validation, BIC takes its place. Two sets of terms that do not
# nest: if one truly matters a little and the other not at all, the first is preferred;
# if neither matters, the data cannot decide.
comparison_bic <- compare_models(
  est_dir,
  tests = bind_rows(
    tests %>%
      filter(larger %in% c("m_relevant", "m_many", "m_logs")),
    tribble(
      ~question,              ~smaller,       ~larger,
      "Which terms?",         "m_irrelevant", "m_other_terms",
      "Which noise?",         "m_irrelevant", "m_other_noise"
    )
  ),
  ame_name = "at_means",
  n_fresh_draws = 2,
  n_workers = 2
)
stopifnot(identical(proposal_of(comparison_bic$decision, "m_base", "m_relevant"),
                    "accept larger"),
          identical(proposal_of(comparison_bic$decision, "m_relevant", "m_many"),
                    "keep smaller: tests and parsimony disagree"),
          identical(proposal_of(comparison_bic$decision, "m_relevant", "m_logs"),
                    "prefer m_relevant"),
          identical(proposal_of(comparison_bic$decision, "m_irrelevant", "m_other_terms"),
                    "prefer m_other_terms"),
          identical(proposal_of(comparison_bic$decision, "m_irrelevant", "m_other_noise"),
                    "data cannot decide"),
          all(comparison_bic$decision$verdict_cross_validation == "not run"),
          grepl("Not run", comparison_bic$cross_validation$note[1], fixed = TRUE))

# (c) A second run loads the folds: the same gain, at once
comparison_cached <- compare_models(
  est_dir,
  tests = tests %>% filter(larger == "m_relevant"),
  cv_models = c("m_base", "m_relevant"),
  n_fresh_draws = 2,
  n_workers = 2
)
stopifnot(identical(comparison_cached$decision$cv_gain_per_expert,
                    decision$cv_gain_per_expert[decision$larger == "m_relevant"]))

# (d) No AME moves by 1 pp: keep the smaller model as the main one
set_ames("m_relevant", make_ames(ame_values + 0.005, se_values, baseline_values))
comparison_ame <- compare_models(
  est_dir,
  tests = tests %>% filter(larger == "m_relevant"),
  ame_name = "at_means",
  n_fresh_draws = 2,
  n_workers = 2
)
stopifnot(comparison_ame$decision$proposal ==
            "keep smaller as main; report larger as robustness")
# An effect only the larger model has, of 2 pp, is enough to accept it, although the
# effects both have move by 0.5 pp only (Max, 2026-11-09)
ames_added <- make_ames(ame_values + 0.005, se_values, baseline_values)
ames_added$at_means$table$effect[18] <- "only_in_this_model"
ames_added$at_means$table$ame[18] <- 0.02
ames_added$at_means$changes$only_in_this_model <- list(to = list(x_relevant = 1))
set_ames("m_relevant", ames_added)
comparison_added <- compare_models(
  est_dir,
  tests = tests %>% filter(larger == "m_relevant"),
  ame_name = "at_means",
  n_fresh_draws = 2,
  n_workers = 2
)
stopifnot(comparison_added$decision$proposal == "accept larger",
          comparison_added$decision$verdict_ame == "moved",
          comparison_added$decision$ame_rows_moved == 0,
          comparison_added$decision$ame_added_rows_moved == 1,
          grepl("largest AME that m_relevant adds 2.00 pp",
                comparison_added$decision$reason, fixed = TRUE))
# The same AMEs at another reference state: they are not compared. An effect that the
# two models define differently: its rows are not compared.
ames_other_state <- make_ames(ame_values + 0.005, se_values, baseline_values)
ames_other_state$at_means$at <- list(price_gas_kwh = 14)
set_ames("m_relevant", ames_other_state)
comparison_state <- compare_models(
  est_dir,
  tests = tests %>% filter(larger == "m_relevant"),
  ame_name = "at_means",
  n_fresh_draws = 2,
  n_workers = 2
)
stopifnot(comparison_state$decision$verdict_ame == "not available",
          comparison_state$decision$proposal == "accept larger",
          grepl("reference state", comparison_state$ame_changes$note[1], fixed = TRUE))
ames_other_change <- make_ames(ame_values + 0.005, se_values, baseline_values)
ames_other_change$at_means$changes$income_low <- list(to = list(att_income = "other"))
set_ames("m_relevant", ames_other_change)
comparison_change <- compare_models(
  est_dir,
  tests = tests %>% filter(larger == "m_relevant"),
  ame_name = "at_means",
  n_fresh_draws = 2,
  n_workers = 2
)
# ...and as it has a name the smaller model has too, it is no added effect either
stopifnot(comparison_change$decision$ame_rows_shared == n_ame_rows - 6,
          !any(comparison_change$ame_changes$quantity == "AME the larger model adds"))
set_ames("m_relevant", ames_relevant)

# (e) With the classical covariance matrix every weight is 1 and the robust test is
# the classical one: a known answer, in a folder of its own
est_dir_classical <- file.path(est_dir, "classical_weights")
for (model_name in c("m_relevant", "m_irrelevant")) {
  folder <- file.path(est_dir_classical, model_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(est_dir, model_name, "model.rds"), folder, overwrite = TRUE)
  results <- readRDS(file.path(est_dir, model_name, "results.rds"))
  results$vcov <- results$inverse_hessian
  saveRDS(results, file.path(folder, "results.rds"))
}
comparison_classical <- compare_models(
  est_dir_classical,
  tests = tests %>% filter(larger == "m_irrelevant"),
  n_fresh_draws = 2,
  n_workers = 2
)
row_classical <- comparison_classical$nested_tests
stopifnot(abs(row_classical$mean_weight - 1) < 1e-8,
          abs(row_classical$p_robust - row_classical$p) < 0.002,
          abs(row_classical$critical_value - qchisq(0.95, row_classical$df)) < 0.05)

# (f) A gain within the noise: an LR that the fresh draws do not repeat. The
# log-likelihood of the larger model on its own draws is raised by hand, as if its
# draws had been lucky.
est_dir_lucky <- file.path(est_dir, "lucky_draws")
for (model_name in c("m_relevant", "m_irrelevant")) {
  folder <- file.path(est_dir_lucky, model_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(est_dir, model_name, "model.rds"), folder, overwrite = TRUE)
  results <- readRDS(file.path(est_dir, model_name, "results.rds"))
  if (model_name == "m_irrelevant") {
    results$fit$log_likelihood <- results$fit$log_likelihood + 15
    results$fit$aic <- results$fit$aic - 30
    results$fit$bic <- results$fit$bic - 30
    results$fit$tic <- results$fit$tic - 30
  }
  saveRDS(results, file.path(folder, "results.rds"))
}
comparison_lucky <- compare_models(
  est_dir_lucky,
  tests = tests %>% filter(larger == "m_irrelevant"),
  n_fresh_draws = 4,
  n_workers = 2
)
stopifnot(comparison_lucky$decision$proposal ==
            "keep smaller: gain does not survive fresh draws")

# (g) A fold that fails: its error is reported, the pair is not usable, and BIC decides.
# The call of the larger model holds a random constant that does not exist; the folds
# of the smaller model are loaded.
est_dir_failed <- file.path(est_dir, "failed_fold")
for (model_name in c("m_base", "m_relevant")) {
  folder <- file.path(est_dir_failed, model_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(est_dir, model_name, "results.rds"), folder, overwrite = TRUE)
  file.copy(file.path(est_dir, "m_base", "cross_validation.rds"),
            file.path(est_dir_failed, "m_base"), overwrite = TRUE)
  model <- readRDS(file.path(est_dir, model_name, "model.rds"))
  if (model_name == "m_relevant") {
    model$call$rpar <- c("no_such_constant" = "n")
  }
  saveRDS(model, file.path(folder, "model.rds"))
}
comparison_failed <- compare_models(
  est_dir_failed,
  tests = tests %>% filter(larger == "m_relevant"),
  cv_models = c("m_base", "m_relevant"),
  n_fresh_draws = 2,
  n_workers = 2
)
failed_row <- comparison_failed$cross_validation %>%
  filter(item == "m_relevant")
# Saved all the same: such an error would come back on every run, after hours of
# estimation
stopifnot(failed_row$folds_converged == 0, failed_row$experts == 0,
          nchar(failed_row$fold_errors) > 0,
          file.exists(file.path(est_dir_failed, "m_relevant", "cross_validation.rds")),
          grepl("not usable", comparison_failed$decision$verdict_cross_validation, fixed = TRUE),
          comparison_failed$decision$proposal == "accept larger",
          grepl("not usable", comparison_failed$decision$reason, fixed = TRUE))

# (g2) A rare dummy: a fold whose experts do not hold it has a column of zeros, which
# makes mlogit's Hessian singular even at iterlim = 0. Every fold runs all the same. A
# folder of its own, as compare_models() reads every model of a folder.
est_dir_rare <- file.path(est_dir, "rare_dummy")
dir.create(file.path(est_dir_rare, "m_base"), recursive = TRUE, showWarnings = FALSE)
file.copy(file.path(est_dir, "m_base", c("model.rds", "results.rds", "cross_validation.rds")),
          file.path(est_dir_rare, "m_base"), overwrite = TRUE)
index_base <- idx(choice_data)
experts_with_oil <- unique(as.character(
  index_base$resp_uid[choice_data$chosen & index_base$alternative == "Ölheizung"]
))
choice_data_rare <- choice_data
choice_data_rare$rare_dummy <- as.numeric(
  as.character(index_base$resp_uid) %in% experts_with_oil[1:3] &
    index_base$alternative == "Ölheizung"
)
m_rare <- fit_mixed_logit(
  "m_rare", chosen ~ price + rare_dummy | 1 + is_hvac + is_sweep + att_a + att_b,
  choice_data_rare, reference, est_dir_rare, start_from = m_base, n_draws = n_draws_test
)
post_estimation(m_rare, est_dir_rare, label = "m_rare", n_draws_kr = 50,
                n_workers = n_workers_test)
comparison_rare <- compare_models(
  est_dir_rare,
  tests = tribble(
    ~question,                ~smaller, ~larger,
    "Does the dummy matter?", "m_base", "m_rare"
  ),
  cv_models = c("m_base", "m_rare"),
  n_fresh_draws = 2,
  n_workers = 2
)
rare_row <- comparison_rare$cross_validation %>%
  filter(item == "m_rare")
# The robust test rests on 3 experts here: the reason says so
stopifnot(rare_row$folds_converged == 5, rare_row$experts == n_experts,
          rare_row$fold_errors == "",
          !grepl("not usable", comparison_rare$decision$verdict_cross_validation,
                 fixed = TRUE),
          comparison_rare$decision$mean_weight < LOW_MEAN_WEIGHT,
          grepl("a restriction rests on few experts", comparison_rare$decision$reason,
                fixed = TRUE))

# (g3) A cross-validation that was cut off resumes with the folds it had: the folds of
# m_base written one by one as a run writes them, without the file of the model.
# Folds 1 to 3 are loaded; fold 4, from other estimates, and fold 5, missing, are
# estimated again, with the same result.
cv_file <- file.path(est_dir, "m_base", "cross_validation.rds")
cv_whole <- readRDS(cv_file)
for (fold_number in 1:4) {
  is_of_fold <- cv_whole$per_expert$fold == fold_number
  fold_row <- cv_whole$folds[cv_whole$folds$fold == fold_number, ]
  fold_fingerprint <- cv_whole$fingerprint
  if (fold_number == 4) {
    fold_fingerprint <- "other estimates"
  }
  saveRDS(list(expert = cv_whole$per_expert$expert[is_of_fold],
               log_likelihood = cv_whole$per_expert$log_likelihood[is_of_fold],
               code = fold_row$code, iterations = fold_row$iterations,
               error = fold_row$error, model_name = "m_base", fold = fold_number,
               fingerprint = fold_fingerprint, minutes = fold_row$minutes),
          file.path(est_dir, "m_base",
                    paste0("cross_validation_fold_", fold_number, ".rds")))
}
unlink(cv_file)
resume_messages <- character(0)
comparison_resumed <- withCallingHandlers(
  compare_models(
    est_dir,
    tests = tests %>% filter(larger == "m_relevant"),
    cv_models = c("m_base", "m_relevant"),
    n_fresh_draws = 2,
    n_workers = 2
  ),
  # withCallingHandlers(): collects the messages, and lets them through
  message = function(m) {
    resume_messages <<- c(resume_messages, conditionMessage(m))
  }
)
per_expert_resumed <- readRDS(cv_file)$per_expert
stopifnot(file.exists(cv_file),
          any(grepl("m_base: 3 of 5 folds loaded", resume_messages, fixed = TRUE)),
          identical(per_expert_resumed$expert, cv_whole$per_expert$expert),
          identical(per_expert_resumed$fold, cv_whole$per_expert$fold),
          max(abs(per_expert_resumed$log_likelihood -
                    cv_whole$per_expert$log_likelihood)) < 1e-8,
          length(list.files(file.path(est_dir, "m_base"),
                            pattern = "^cross_validation_fold_")) == 0,
          abs(comparison_resumed$decision$cv_gain_per_expert -
                decision$cv_gain_per_expert[decision$larger == "m_relevant"]) < 1e-8)

# (g4) Restrictions that are not independent at the estimates, here the first random
# constant with a standard deviation of exactly 0 in every group (in one group only,
# the other groups' parameters still span the restrictions): that test is not
# compared, and the call goes on. A folder of its own.
est_dir_degenerate <- file.path(est_dir, "degenerate")
for (model_name in c("m_relevant", "m_by_group")) {
  folder <- file.path(est_dir_degenerate, model_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  model <- readRDS(file.path(est_dir, model_name, "model.rds"))
  results <- readRDS(file.path(est_dir, model_name, "results.rds"))
  if (model_name == "m_by_group") {
    for (group_column in unique(results$random$column)) {
      first_constant <- results$random$name[results$random$column == group_column][1]
      diagonal <- paste0("chol.", first_constant, ":", first_constant)
      stopifnot(diagonal %in% names(model$coefficients))
      model$coefficients[[diagonal]] <- 0
      results$coefficients[[diagonal]] <- 0
    }
  }
  saveRDS(model, file.path(folder, "model.rds"))
  saveRDS(results, file.path(folder, "results.rds"))
}
comparison_degenerate <- compare_models(
  est_dir_degenerate,
  tests = tests %>% filter(larger == "m_by_group"),
  n_fresh_draws = 2,
  n_workers = 2
)
stopifnot(comparison_degenerate$decision$proposal == "not compared",
          is.na(comparison_degenerate$decision$p_robust),
          !is.na(comparison_degenerate$decision$p),
          grepl("not independent", comparison_degenerate$decision$reason,
                fixed = TRUE))

# (h) A smaller model above the larger: flagged in validity, and not compared, also if
# no test names the smaller one. The Holm family keeps its size.
comparison_below <- compare_models(
  est_dir,
  tests = tribble(
    ~question,                 ~smaller,     ~larger,
    "A failed estimation",     "m_base",     "m_below",
    "Does the block matter?",  "m_base",     "m_many"
  ),
  n_fresh_draws = 2,
  n_workers = 2
)
validity_below <- comparison_below$validity %>%
  filter(model == "m_below")
print(validity_below %>% select(model, log_likelihood, nests, gap_to_nested, valid, why),
      width = Inf)
stopifnot(!validity_below$valid,
          grepl("below m_relevant", validity_below$why, fixed = TRUE),
          grepl("m_relevant", validity_below$nests, fixed = TRUE),
          validity_below$gap_to_nested < 0,
          all(comparison_below$validity$valid[comparison_below$validity$model != "m_below"]))
decision_below <- comparison_below$decision
stopifnot(identical(proposal_of(decision_below, "m_base", "m_below"), "not compared"),
          is.na(decision_below$p_robust_holm[decision_below$larger == "m_below"]))
holm_kept <- comparison_below$nested_tests %>%
  filter(larger == "m_many")
stopifnot(abs(holm_kept$p_robust_holm - min(1, 2 * holm_kept$p_robust)) < 1e-12)
# The models of the second data set have the same names but other values
stopifnot(length(columns_that_differ(m_base, m_relevant)) == 0,
          length(columns_that_differ(m_common_2, m_relevant)) > 0)

# (i) Models on different samples stop, as do models the wrong way round
message_of_stop <- function(tests_here, cv_here = NULL) {
  tryCatch({
    compare_models(est_dir, tests = tests_here, cv_models = cv_here, n_fresh_draws = 2,
                   n_workers = 2)
    "no stop"
  }, error = function(e) conditionMessage(e))
}
message_samples <- message_of_stop(
  tibble(question = "Another sample", smaller = "m_other_sample", larger = "m_irrelevant")
)
cat("Different samples:", message_samples, "\n")
stopifnot(grepl("different samples", message_samples, fixed = TRUE))
message_folds <- message_of_stop(
  tests %>% filter(larger == "m_relevant"), cv_here = c("m_base", "m_other_sample")
)
cat("Folds on different samples:", message_folds, "\n")
stopifnot(grepl("different samples", message_folds, fixed = TRUE))
message_swapped <- message_of_stop(
  tibble(question = "Swapped", smaller = "m_irrelevant", larger = "m_relevant")
)
cat("Swapped:", message_swapped, "\n")
stopifnot(grepl("Swap", message_swapped, fixed = TRUE))

# 6. The workbook reads well ---------------------------------------------------
# The last run that wrote it: (h). Every sheet read back, as a reader opens it.
workbook <- file.path(est_dir, "model_comparison", "model_comparison.xlsx")
sheet_names <- excel_sheets(workbook)
stopifnot(identical(sheet_names, c("decision", "validity", "fit", "nested_tests",
                                   "non_nested_tests", "fresh_draws", "cross_validation",
                                   "absolute_fit", "ame_changes", "block_tests")))
for (sheet_name in sheet_names) {
  sheet <- read_excel(workbook, sheet = sheet_name)
  cat("Sheet", sheet_name, ":", nrow(sheet), "rows,", ncol(sheet), "columns\n")
  stopifnot(nrow(sheet) >= 1)
}
decision_read <- read_excel(workbook, sheet = "decision")
stopifnot(all(is.na(decision_read$final_decision)),
          identical(decision_read$proposal, comparison_below$decision$proposal),
          all(nchar(decision_read$reason) > 20),
          abs(decision_read$lr[2] - comparison_below$decision$lr[2]) < 0.01)
saved_sheets <- readRDS(file.path(est_dir, "model_comparison", "model_comparison.rds"))
stopifnot(identical(names(saved_sheets), sheet_names))
# A filled final decision is kept in a copy before the next run overwrites it
sheets_filled <- comparison_below
sheets_filled$decision$final_decision[1] <- "Estimate it again"
write_xlsx(sheets_filled, workbook)
comparison_again <- compare_models(
  est_dir,
  tests = tests %>% filter(larger == "m_relevant"),
  n_fresh_draws = 2,
  n_workers = 2
)
copy_kept <- list.files(file.path(est_dir, "model_comparison"),
                        pattern = "^model_comparison_.*xlsx$", full.names = TRUE)
stopifnot(length(copy_kept) == 1,
          read_excel(copy_kept, sheet = "decision")$final_decision[1] == "Estimate it again",
          all(is.na(read_excel(workbook, sheet = "decision")$final_decision)))
invisible(file.remove(copy_kept))

minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat("compare_models(): all checks passed in", round(minutes, 1), "minutes\n")
