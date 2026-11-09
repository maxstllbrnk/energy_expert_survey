################################################################################
# test_simulated_likelihood.R — the fast likelihood against mlogit's, with one set
#                               of constants and with constants by group
# Inputs : simulated decisions, drawn here
# Outputs: none; prints the differences and stops if one is too large
# Checks : log-likelihood, per expert and in total, and the scores per expert
################################################################################

source(file.path("R", "setup.R"))
set.seed(20261110)

# 1. Simulated decisions: 300 experts, 6 vignettes, 5 outcomes -----------------
n_experts <- 300
outcomes <- c("A", "B", "C", "D", "E")
technologies <- c("A", "B", "D", "E")
reference <- "C"
experts <- tibble(
  resp_uid = sprintf("e%04d", seq_len(n_experts)),
  group = sample(c("g1", "g2", "g3"), n_experts, replace = TRUE),
  is_g1 = as.numeric(group == "g1"),
  is_g2 = as.numeric(group == "g2"),
  is_g3 = as.numeric(group == "g3")
)
choice_long <- experts %>%
  cross_join(tibble(vig_num = 1:6)) %>%
  mutate(x1 = rbinom(n(), 1, 0.5), x2 = rnorm(n()), chid = row_number()) %>%
  cross_join(tibble(alternative = factor(outcomes, levels = outcomes))) %>%
  mutate(price = rnorm(n()), utility = 0.5 * x1 * (alternative == "A") - 0.3 * price +
           rnorm(n_experts, 0, 1)[match(resp_uid, experts$resp_uid)] * (alternative == "B") -
           log(-log(runif(n())))) %>%
  group_by(chid) %>%
  mutate(chosen = utility == max(utility)) %>%
  ungroup() %>%
  arrange(chid, alternative)
choice_data <- dfidx(choice_long, idx = list(c("chid", "resp_uid"), "alternative"),
                     choice = "chosen", shape = "long")

compare_with_mlogit <- function(model, theta, label) {
  prepared <- prepare_likelihood(model)
  evaluated <- simulated_likelihood(theta, prepared)
  expert_of_decision <- prepared$expert_idx
  difference_total <- abs(evaluated$log_likelihood - as.numeric(logLik(model)))
  first_decision <- !duplicated(expert_of_decision)
  difference_expert <- max(abs(log(as.numeric(fitted(model)))[first_decision] -
                                 evaluated$log_likelihood_expert))
  # mlogit's gradient has one row per decision and the opposite sign
  scores_mlogit <- rowsum(-model$gradient[, names(theta), drop = FALSE],
                          expert_of_decision, reorder = FALSE)
  difference_scores <- max(abs(scores_mlogit - evaluated$scores))
  cat(label, ": log-likelihood", signif(difference_total, 3), "| per expert",
      signif(difference_expert, 3), "| scores", signif(difference_scores, 3), "\n")
  stopifnot(difference_total < 1e-8, difference_expert < 1e-8, difference_scores < 1e-8)
}

# 2. One set of constants for all experts --------------------------------------
formula_common <- chosen ~ price | 1 + x1 + x2
rpar_common <- setNames(rep("n", 4), paste0("(Intercept):", technologies))
names_common <- names(coef(mlogit(formula_common, data = choice_data, reflevel = reference,
                                  rpar = rpar_common, panel = TRUE, R = 10, halton = NA,
                                  correlation = TRUE, iterlim = 0, print.level = 0)))
theta_common <- setNames(rnorm(length(names_common), 0, 0.3), names_common)
model_common <- mlogit(formula_common, data = choice_data, reflevel = reference,
                       rpar = rpar_common, panel = TRUE, R = 50, halton = NA,
                       correlation = TRUE, start = theta_common, iterlim = 0, print.level = 0)
compare_with_mlogit(model_common, theta_common, "One set of constants")

# 3. Constants by group, with the elements of L between groups fixed at 0 ------
# Called through do.call(), as mlogit() looks up constPar in the global environment
formula_group <- chosen ~ price | 0 + is_g1 + is_g2 + is_g3 + x1 + x2
groups <- c("is_g1", "is_g2", "is_g3")
random_names <- character(0)
for (group in groups) {
  random_names <- c(random_names, paste0(group, ":", technologies))
}
rpar_group <- setNames(rep("n", length(random_names)), random_names)
names_group <- names(coef(mlogit(formula_group, data = choice_data, reflevel = reference,
                                 rpar = rpar_group, panel = TRUE, R = 10, halton = NA,
                                 correlation = TRUE, iterlim = 0, print.level = 0)))
fixed_names <- character(0)
for (i in seq_along(random_names)) {
  for (j in seq_len(i - 1)) {
    if (str_replace(random_names[i], ":.*$", "") != str_replace(random_names[j], ":.*$", "")) {
      fixed_names <- c(fixed_names, paste0("chol.", random_names[j], ":", random_names[i]))
    }
  }
}
stopifnot(all(fixed_names %in% names_group))
start_group <- setNames(rnorm(length(names_group), 0, 0.3), names_group)
start_group[fixed_names] <- 0
fixed_parameters <- setNames(rep(0, length(fixed_names)), fixed_names)
model_group <- do.call("mlogit", list(
  formula = formula_group, data = quote(choice_data), reflevel = reference,
  rpar = rpar_group, panel = TRUE, R = 50,
  halton = list(prime = rep(c(2, 3, 5, 7), times = 3)),
  correlation = TRUE, start = start_group, iterlim = 0, print.level = 0,
  constPar = fixed_parameters
))
theta_group <- start_group[names(coef(model_group))]
compare_with_mlogit(model_group, theta_group, "Constants by group")

cat("All differences below 1e-8\n")
