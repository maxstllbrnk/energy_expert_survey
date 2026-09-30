################################################################################
# 03_rc_logit_with_ame.R — which heating technology do experts
#                                  recommend? (Mixed) multinomial logit models
################################################################################

source(file.path("R", "prepare_analysis_data.R"))   # load_analysis_data()

# 1. Settings ------------------------------------------------------------------

# Draws model estimation
n_draws_model <- 1000

# Draws of the coefficient vector used to average the predicted probabilities
# over the distribution of the random coefficients (marginal effects)
n_draws_ame <- 1000

# Draws for AME Krinsky-Robb Standard Errors 
n_draws_kr <- 1000

# Estimate for the experts who have "Fernwärme" in their choice set or not
SELECTED_FW_ARM = "fw"

# The seven possible recommendations; the reference outcome comes first
REFERENCE_OUTCOME <- "Keine Empfehlung"

# 05_run_all_specifications.R runs this script for several arms and reference
# outcomes: it passes them in RUN_SETTINGS, which then replaces the two
# values above. Run on its own, the script uses the values above.
if (exists("RUN_SETTINGS", inherits = FALSE)) {
  SELECTED_FW_ARM <- RUN_SETTINGS$arm
  REFERENCE_OUTCOME <- RUN_SETTINGS$reference
}
cat("Arm:", SELECTED_FW_ARM, "| reference outcome:", REFERENCE_OUTCOME, "\n")

if (SELECTED_FW_ARM == "fw"){
  RECOMMENDATIONS <- unique(
    c(REFERENCE_OUTCOME,
      "Wärmepumpe", "Pelletheizung", "Hybridheizung",
      "Gasheizung", "Ölheizung", "Fernwärme", "Keine Empfehlung"
    ))
} else {
  RECOMMENDATIONS <- unique(
    c(REFERENCE_OUTCOME,
      "Wärmepumpe", "Pelletheizung", "Hybridheizung",
      "Gasheizung", "Ölheizung", "Keine Empfehlung"
    ))
}

# Levels of the vignette attributes exactly as in the data; the first level is
# the reference category
ATTRIBUTE_LEVELS <- list(
  att_couple_age = c("Mitte 40", "Mitte 60"),
  att_income = c("65.000 € brutto im Jahr", "unter 40.000 € brutto im Jahr"),
  att_current_heating = c("Gasheizung", "Ölheizung"),
  att_replacement_timing = c("muss in den nächsten Jahren ersetzt werden",
                             "ist defekt und muss kurzfristig ersetzt werden"),
  att_build_year = c("1970", "2000"),
  att_heat_demand = c("120", "70", "200"),
  att_heat_distribution = c("Heizkörper", "eine Fußbodenheizung")
)

# Regressors. choice_formula is mlogit's notation: before `|` come attributes
# of the technologies (none: 0); after it come attributes of the vignette,
# which get one coefficient per technology, plus a constant per technology (1).
# design_formula lists the same attributes; it builds the design matrix for
# the marginal effects. A check in section 4 ties the two together.
choice_formula <- vig_rec ~ 0 | 1 + att_couple_age + att_income +
  att_current_heating + att_replacement_timing + att_build_year +
  att_heat_demand + att_heat_distribution

design_formula <- ~ att_couple_age + att_income + att_current_heating +
  att_replacement_timing + att_build_year + att_heat_demand +
  att_heat_distribution

# 2. Data ----------------------------------------------------------------------
analysis_data <- load_analysis_data(completers_only = TRUE)
vignettes_all <- analysis_data$vignettes

# District heating is an option only in the fw arm (between-subject), so the
# models are estimated on that arm, where all seven outcomes are available
decisions <- vignettes_all %>%
  filter(vig_arm == SELECTED_FW_ARM)
if (nrow(decisions) == 0) stop("No vignettes for the arm ", SELECTED_FW_ARM)

decisions <- decisions %>%
  select(resp_uid, vig_num, vig_rec, att_couple_age, att_income,
         att_current_heating, att_replacement_timing, att_build_year,
         att_heat_demand, att_heat_distribution) %>%
  mutate(
    vig_rec = factor(vig_rec, levels = RECOMMENDATIONS),
    att_couple_age = factor(att_couple_age,
                            levels = ATTRIBUTE_LEVELS$att_couple_age),
    att_income = factor(att_income,
                        levels = ATTRIBUTE_LEVELS$att_income),
    att_current_heating = factor(att_current_heating,
                                 levels = ATTRIBUTE_LEVELS$att_current_heating),
    att_replacement_timing = factor(att_replacement_timing,
                                    levels = ATTRIBUTE_LEVELS$att_replacement_timing),
    att_build_year = factor(att_build_year,
                            levels = ATTRIBUTE_LEVELS$att_build_year),
    att_heat_demand = factor(att_heat_demand,
                             levels = ATTRIBUTE_LEVELS$att_heat_demand),
    att_heat_distribution = factor(att_heat_distribution,
                                   levels = ATTRIBUTE_LEVELS$att_heat_distribution)
  ) %>%
  arrange(resp_uid, vig_num) %>%
  mutate(chid = row_number())   # one choice situation per expert x vignette

n_choices <- nrow(decisions)
n_experts <- n_distinct(decisions$resp_uid)
choice_counts <- table(decisions$vig_rec)
print(choice_counts)

# dfidx() reshapes the data to one row per choice situation x outcome, the
# format mlogit() needs; idx nests each choice situation (chid) within its
# expert (resp_uid), which panel = TRUE uses
choice_data <- dfidx(
  decisions,
  shape = "wide",
  choice = "vig_rec",
  idx = list(c("chid", "resp_uid")),
  idnames = c(NA, "alternatives")
)

# 3. Check vignette distribution -----------------------------------------------
ggplot(vignettes_all) +
  geom_histogram(bins = 128, aes(vignette_id)) 

ggplot(vignettes_all %>% filter(vig_arm == "fw")) +
  geom_histogram(bins = 128, aes(vignette_id))

ggplot(vignettes_all %>% filter(vig_arm != "fw")) +
  geom_histogram(bins = 128, aes(vignette_id))

vignette_weights <- vignettes_all %>%
  group_by(vignette_id) %>%
  summarise(
    count_all = n()
  ) %>%
  mutate(weight_all = count_all / sum(count_all)) %>%
  left_join(
    vignettes_all %>%
      filter(vig_arm == "fw") %>% 
      group_by(vignette_id) %>%
      summarise(
        count_fw = n()
      ) %>%
      mutate(weight_fw = count_fw / sum(count_fw)), by = c("vignette_id")
  ) %>%
  left_join(
    vignettes_all %>%
      filter(vig_arm != "fw") %>% 
      group_by(vignette_id) %>%
      summarise(
        count_no_fw = n()
      ) %>%
      mutate(weight_no_fw = count_no_fw / sum(count_no_fw)), by = c("vignette_id")
  )

summary(vignette_weights)

# 4. Estimate models -----------------------------------------------------------
# (1) Multinomial logit
m_mnl <- mlogit(choice_formula, data = choice_data, reflevel = REFERENCE_OUTCOME)

# The regressors of the design matrix times the six technologies must be
# exactly the coefficients of the model (setequal(): same elements, in any order)
terms <- colnames(model.matrix(design_formula, decisions))
expected_coefs <- outer(terms, RECOMMENDATIONS[RECOMMENDATIONS != REFERENCE_OUTCOME], paste, sep = ":")

rpar_names <- names(coef(m_mnl))[grep("Intercept",names(coef(m_mnl)))]

# (2) Mixed logit: every coefficient normally distributed across experts,
# independently of the others; each expert keeps one draw for all six vignettes.
# R = 40 pseudo-random draws per expert is mlogit's default; the stored
# correlated model was estimated with it.
random_parameters <- rep("n", length(rpar_names))
names(random_parameters) <- rpar_names

m_mixed <- mlogit(choice_formula, data = choice_data,
                  reflevel = REFERENCE_OUTCOME, rpar = random_parameters,
                  panel = TRUE, halton = NA, R = n_draws_model)

# (3) Mixed logit with correlated random coefficients: full covariance matrix
m_mixed_corr <- mlogit(choice_formula, data = choice_data,
                       reflevel = REFERENCE_OUTCOME, rpar = random_parameters,
                       panel = TRUE, halton = NA, correlation = TRUE,
                       R = n_draws_model)

models <- list(mnl = m_mnl, mixed = m_mixed, mixed_corr = m_mixed_corr)

# 5. Covariance matrix of the parameters clustered at the expert level ---------

#' Cluster-robust (sandwich) covariance matrix of the estimates of an mlogit
#' model, clustered by expert:
#'
#'   V = G / (G - 1) * H^{-1} B H^{-1}
#'
#' H     Hessian of -log L at the estimates ("bread").
#'       MNL: mlogit's analytic Hessian; vcov(model) is its inverse.
#'       Mixed logit: mlogit provides none, so H is computed by central
#'       differences of the gradient. The gradient is re-evaluated by
#'       re-running the model's call with iterlim = 0, which reuses the
#'       estimation draws (checked below).
#' B     sum over experts of the outer product of the expert-level scores
#'       ("meat"). model$gradient has one row per choice situation; the rows
#'       of an expert add up to her score.
#' G/(G-1) small-sample factor with G experts (as in Stata and sandwich::vcovCL).
#'
#' @param model  fitted mlogit model (MNL or mixed logit)
#' @param cluster name of the cluster variable in idx(model)
#' @param h      step size for the numerical derivatives (mixed logit only)
#' @return covariance matrix, rows and columns named as coef(model)
vcov_cluster_robust <- function(model, cluster = "resp_uid", h = 1e-4) {
  theta_hat <- coef(model)
  K <- length(theta_hat)
  
  # Meat: one score vector per expert
  index <- idx(model)
  cluster_id <- index[[cluster]][!duplicated(index$chid)]   # one per choice situation
  
  expert_gradients <- rowsum(model$gradient, cluster_id)               # G x K
  meat <- t(expert_gradients) %*% expert_gradients                     # sum_g s_g s_g'
  G <- nrow(expert_gradients)
  
  # Bread: inverse Hessian of -log L
  if (is.null(model$rpar)) {
    bread <- unclass(vcov(model))                            # MNL: analytic
  } else {
    # Gradient of -log L at any parameter vector theta: re-estimate the model
    # starting at theta with zero iterations, so that mlogit only evaluates
    # the log-likelihood and its gradient there. model$gradient holds one row
    # per choice situation; -log L is a sum over them, so its gradient is the
    # sum of the rows.
    total_gradient <- function(theta, model) {
      model_at_theta <- update(model, start = theta, iterlim = 0, print.level = 0)
      colSums(model_at_theta$gradient)
    }
    # Re-evaluating at the estimates must reproduce the stored gradient,
    # otherwise data, settings or draws differ from the estimation
    if (max(abs(total_gradient(theta_hat, model) - colSums(model$gradient))) > 1e-6) {
      stop("Re-evaluating the model does not reproduce its gradient: ",
           "check that the objects in the model's call are unchanged")
    }
    H <- matrix(NA_real_, K, K)
    for (k in seq_len(K)) {
      step <- replace(numeric(K), k, h)
      H[, k] <- (total_gradient(theta_hat + step, model) -
                   total_gradient(theta_hat - step, model)) / (2 * h)
      if (k %% 10 == 0) message("Hessian: column ", k, " of ", K)
    }
    H <- (H + t(H)) / 2
    if (any(eigen(H, symmetric = TRUE, only.values = TRUE)$values <= 0)) {
      stop("The numerical Hessian is not positive definite")
    }
    bread <- solve(H)
  }
  
  V <- G / (G - 1) * bread %*% meat %*% bread
  dimnames(V) <- list(names(theta_hat), names(theta_hat))
  V
}

# Compute cluster-robust (sandwich) covariance matrix for each model
V_mnl <- vcov_cluster_robust(models$mnl, cluster = "resp_uid", h = 1e-04)

V_mixed <- vcov_cluster_robust(models$mixed, cluster = "resp_uid", h = 1e-04)

V_mixed_corr <- vcov_cluster_robust(models$mixed_corr, cluster = "resp_uid", h = 1e-04)

# 6. Average marginal effect at the reference vignette -------------------------
# Question: by how much does the probability that an expert recommends
# technology j change if one attribute of the reference vignette (every
# attribute at its first level) is set to another level?
#
#   AME_j = P_j(reference vignette with one attribute changed)
#           - P_j(reference vignette)
#
# P_j(x) is the probability for a randomly drawn expert: the logit probability
# averaged over the distribution of the random ASCs across experts,
# alpha = mu + L z with z ~ N(0, I) and L being the Choleski factor 
# of Omega such that Omega = LL'. The average is approximated with
# R = n_draws_ame draws z_1, ..., z_R:
#
#   P_j(x) = 1/R * sum_r exp(u_j(x, r)) / sum_k exp(u_k(x, r))
#   u_j(x, r) = x' beta_j + (L z_r)_j     for the six technologies
#   u_0(x, r) = 0                         for the reference outcome
#
# beta_j holds the coefficients of technology j; for the mixed logits its
# constant is the mean ASC mu_j. For the MNL, L = 0, so every draw gives the
# same probability: the MNL probability.


# 6.1 The vignettes: the reference vignette and its eight variations ------------
# Reference vignette: every attribute at its first level. tibble(.rows = 1)
# is an empty table with one row; the loop adds one column per attribute.
reference_vignette <- tibble(.rows = 1)
for (attribute in names(ATTRIBUTE_LEVELS)) {
  reference_vignette[[attribute]] <- factor(ATTRIBUTE_LEVELS[[attribute]][1],
                                            levels = ATTRIBUTE_LEVELS[[attribute]])
}
reference_vignette$label <- "Reference vignette"

# Variations: the reference vignette with exactly one attribute changed, one
# row per non-reference level ([-1]: all levels but the first)
ame_vignettes <- reference_vignette
for (attribute in names(ATTRIBUTE_LEVELS)) {
  for (level in ATTRIBUTE_LEVELS[[attribute]][-1]) {
    variation <- reference_vignette
    variation[[attribute]] <- factor(level, levels = ATTRIBUTE_LEVELS[[attribute]])
    variation$label <- paste0(attribute, ": ", level)
    ame_vignettes <- bind_rows(ame_vignettes, variation)
  }
}
print(ame_vignettes)

# Design matrix: one row per vignette, one column per regressor (constant and
# the dummies). Row 1 is the reference vignette (a 1 in the constant only);
# every other row has one dummy switched on.
X_ame <- model.matrix(design_formula, ame_vignettes)
rownames(X_ame) <- ame_vignettes$label

# 6.2 Standard normal draws z_r --------------------------------------------------
# One row per draw, one column per random ASC. The same draws are used for all
# vignettes and all models, so that differences between vignettes do not mix
# in different simulation noise. The seed is set right here, so the draws do
# not depend on what was run before.
set.seed(20261001)
standard_draws <- matrix(rnorm(n_draws_ame * length(rpar_names)),
                         nrow = n_draws_ame, ncol = length(rpar_names))

# 6.3 Probabilities and AMEs for one coefficient vector --------------------------

#' Probabilities of the seven recommendations for each vignette in X, averaged
#' over the random ASCs, and the AMEs (each variation minus the reference
#' vignette). Section 7 evaluates it again at many
#' draws of the coefficient vector to get standard errors.
#'
#' @param theta          coefficient vector, named as coef() of an mlogit model
#' @param X              design matrix; its first row is the reference vignette
#' @param standard_draws standard normal draws, one row per draw, one column
#'                       per random ASC
#' @param rpar_names     names of the ASCs in the order mlogit uses in coef()
#' @return list with
#'   probabilities  matrix, one row per vignette, one column per outcome
#'   ame            matrix, one row per variation, one column per outcome
#'   L              the Cholesky factor used (for the checks below)
ame_at_reference <- function(theta, X, standard_draws, rpar_names) {
  technologies <- RECOMMENDATIONS[RECOMMENDATIONS != REFERENCE_OUTCOME]
  regressors <- colnames(X)
  
  # (a) Coefficients beta: one row per regressor, one column per technology.
  #     mlogit names each coefficient "<regressor>:<technology>".
  beta <- matrix(NA_real_, nrow = length(regressors), ncol = length(technologies),
                 dimnames = list(regressors, technologies))
  for (technology in technologies) {
    beta[, technology] <- theta[paste0(regressors, ":", technology)]
  }
  if (anyNA(beta)) stop("Coefficients missing in theta")
  
  # (b) Cholesky factor L of the covariance matrix of the ASCs, so that the
  #     covariance matrix is L %*% t(L). Rows and columns follow mlogit's
  #     order of the ASCs (rpar_names); the lower-triangular form refers to
  #     this order.
  K <- length(rpar_names)
  L <- matrix(0, nrow = K, ncol = K, dimnames = list(rpar_names, rpar_names))
  if (any(startsWith(names(theta), "chol."))) {
    # Correlated model: mlogit names L[i, j] (i >= j) "chol.<ASC j>:<ASC i>"
    for (i in 1:K) {
      for (j in 1:i) {
        L[i, j] <- theta[paste0("chol.", rpar_names[j], ":", rpar_names[i])]
      }
    }
  } else if (any(startsWith(names(theta), "sd."))) {
    # Uncorrelated model: L is diagonal with the standard deviations
    for (i in 1:K) {
      L[i, i] <- theta[paste0("sd.", rpar_names[i])]
    }
  }
  # MNL: no random ASCs, L stays 0
  if (anyNA(L)) stop("Cholesky or standard deviation parameters missing in theta")
  
  # (c) Deviation of the ASCs from their means for each draw, L z_r. Row r of
  #     standard_draws is z_r', so row r of standard_draws %*% t(L) is (L z_r)'.
  #     The columns are then put in the order of the technologies in beta.
  asc_deviations <- standard_draws %*% t(L)
  colnames(asc_deviations) <- rpar_names
  asc_deviations <- asc_deviations[, paste0("(Intercept):", technologies), drop = FALSE]
  
  # (d) Probabilities of each vignette, averaged over the draws
  n_draws <- nrow(standard_draws)
  probabilities <- matrix(NA_real_, nrow = nrow(X), ncol = length(RECOMMENDATIONS),
                          dimnames = list(rownames(X), RECOMMENDATIONS))
  for (v in 1:nrow(X)) {
    # utility of each technology at the mean ASCs, x' beta: one number per technology
    mean_utility <- as.vector(X[v, ] %*% beta)
    # utility for each draw: copy x' beta into every row and add that draw's
    # ASC deviations (n_draws rows x 6 technologies)
    utility <- matrix(mean_utility, nrow = n_draws, ncol = length(technologies),
                      byrow = TRUE) + asc_deviations
    # the reference outcome has utility 0: first column (n_draws x 7)
    utility <- cbind(0, utility)
    # logit probabilities for each draw; each row sums to one. Subtracting each
    # row's largest utility before exp() avoids overflow; it cancels in the ratio.
    exp_utility <- exp(utility - apply(utility, 1, max))
    prob_draws <- exp_utility / rowSums(exp_utility)
    # average over the draws, i.e. over experts
    probabilities[v, ] <- colMeans(prob_draws)
  }
  
  # (e) AMEs: probabilities of each variation minus those of the reference
  #     vignette. reference_probabilities repeats row 1 once per variation.
  reference_probabilities <- matrix(probabilities[1, ], nrow = nrow(X) - 1,
                                    ncol = ncol(probabilities), byrow = TRUE)
  ame <- probabilities[-1, , drop = FALSE] - reference_probabilities
  
  list(probabilities = probabilities, ame = ame, L = L)
}

# 6.4 AMEs of the three models ----------------------------------------------------
ame_estimates <- list()
for (model_name in names(models)) {
  ame_estimates[[model_name]] <- ame_at_reference(coef(models[[model_name]]),
                                                  X_ame, standard_draws, rpar_names)
  cat("\nAMEs at the reference vignette:", model_name, "\n")
  print(round(ame_estimates[[model_name]]$ame, 3))
}

# 6.5 Checks -------------------------------------------------------------------
# (a) The Cholesky factor reproduces mlogit's covariance matrix of the ASCs
#     (correlated model) and its standard deviations (uncorrelated model)
L_corr <- ame_estimates$mixed_corr$L
Omega_mlogit <- unclass(cov.mlogit(models$mixed_corr))[rpar_names, rpar_names]
stopifnot(max(abs(L_corr %*% t(L_corr) - Omega_mlogit)) < 1e-8)

L_uncorr <- ame_estimates$mixed$L
stopifnot(max(abs(abs(diag(L_uncorr)) - stdev(models$mixed)[rpar_names])) < 1e-8)

# (b) MNL: at the observed vignettes, the function reproduces mlogit's
#     probabilities exactly (only $probabilities is used here)
X_observed <- model.matrix(design_formula, decisions)
p_own <- ame_at_reference(coef(models$mnl), X_observed, standard_draws,
                          rpar_names)$probabilities
p_mlogit <- fitted(models$mnl, type = "probabilities")[, RECOMMENDATIONS]
stopifnot(max(abs(p_own - p_mlogit)) < 1e-8)

# (c) Mixed logits: mlogit's fitted probabilities also average over draws of
#     the ASCs, but with its own draws (R per expert), so they agree with ours
#     only up to simulation noise: the difference should be small
for (model_name in c("mixed", "mixed_corr")) {
  p_own <- ame_at_reference(coef(models[[model_name]]), X_observed, standard_draws,
                            rpar_names)$probabilities
  p_mlogit <- fitted(models[[model_name]], type = "probabilities")[, RECOMMENDATIONS]
  cat(model_name, ": mean absolute difference to mlogit's probabilities:",
      round(mean(abs(p_own - p_mlogit)), 4), "\n")
}

# (d) Probabilities sum to one, so the AMEs sum to zero across the outcomes
for (model_name in names(ame_estimates)) {
  stopifnot(all(abs(rowSums(ame_estimates[[model_name]]$ame)) < 1e-12))
}

# (e) Simulation noise: other draws should change the AMEs by much less than
#     their standard errors (section 7); otherwise increase n_draws_ame
set.seed(20261002)
standard_draws_check <- matrix(rnorm(n_draws_ame * length(rpar_names)),
                               nrow = n_draws_ame, ncol = length(rpar_names))
ame_check <- ame_at_reference(coef(models$mixed_corr), X_ame, standard_draws_check,
                              rpar_names)$ame
cat("Largest change of an AME with other draws (mixed_corr):",
    round(max(abs(ame_check - ame_estimates$mixed_corr$ame)), 4), "\n")

# 6.6 Results as a table ---------------------------------------------------------
# One row per model x variation x outcome: the probability at the reference
# vignette and the AME
ame_table <- tibble()
for (model_name in names(ame_estimates)) {
  reference_probabilities <- ame_estimates[[model_name]]$probabilities["Reference vignette", ]
  ame_long <- as_tibble(ame_estimates[[model_name]]$ame, rownames = "vignette") %>%
    pivot_longer(-vignette, names_to = "outcome", values_to = "ame") %>%
    mutate(model = model_name,
           p_reference = reference_probabilities[outcome])
  ame_table <- bind_rows(ame_table, ame_long)
}
ame_table <- ame_table %>%
  mutate(outcome = factor(outcome, levels = RECOMMENDATIONS)) %>%
  select(model, vignette, outcome, p_reference, ame)


# 7. Standard errors, confidence intervals and p-values ------------------------
# All inference in this section rests on one approximation: in large samples
# the estimates are normally distributed around the true parameters, with the
# cluster-robust covariance matrix V of section 5,
#
#   theta_hat ~ N(theta, V).
#
# Estimate / se is therefore compared with the standard normal distribution.
# It is a z statistic, not a t statistic: maximum likelihood delivers normality
# only asymptotically, and no t distribution is involved.
#
#   z = estimate / se,   p = 2 * P(Z > |z|),   95% CI = estimate +- 1.96 * se

covariance_matrices <- list(mnl = V_mnl, mixed = V_mixed, mixed_corr = V_mixed_corr)
z_critical <- qnorm(0.975)   # 1.96

# 7.1 Coefficients ---------------------------------------------------------------
# Table rows per model:
#   <term>:<technology>   fixed coefficients and, in the mixed logits, the mean
#                         ASCs mu_j: estimate and square root of the diagonal of V
#   sd.<ASC>              standard deviations of the ASCs across experts
#   cor.<ASC>:<ASC>       correlations of the ASCs (correlated model only)
#
# Standard deviations. Uncorrelated model: mlogit estimates s_j in
# alpha_j = mu_j + s_j z_j; only |s_j| is identified, so the table shows |s_j|
# with the standard error of s_j (the sign flip does not change it).
# Correlated model: mlogit estimates the Cholesky factor L. A single entry of L
# has no substantive meaning, so the table shows what L implies,
#   Omega = L L',   sd_j = sqrt(Omega_jj),   cor_jk = Omega_jk / (sd_j sd_k),
# with delta-method standard errors: se = sqrt(diag(J V J')), where J is the
# derivative of (sd, cor) with respect to theta, computed by numDeriv::jacobian().
#
# Tests and intervals. z = estimate / se and p = 2 * P(Z > |z|) for all rows but
# the standard deviations: their test of sd = 0 is not valid in the usual form,
# because sd = 0 lies on the boundary of the parameter space (z and p are NA;
# use a likelihood-ratio test against the MNL instead). The 95% intervals are
# estimate +- 1.96 se, cut at 0 for standard deviations and at -1 and 1 for
# correlations.
coefficient_table <- tibble()
for (model_name in names(models)) {
  theta_hat <- coef(models[[model_name]])
  V <- covariance_matrices[[model_name]][names(theta_hat), names(theta_hat)]
  se <- sqrt(diag(V))
  
  # (a) Fixed coefficients and mean ASCs: every parameter but sd. and chol.
  is_mean <- !startsWith(names(theta_hat), "sd.") & !startsWith(names(theta_hat), "chol.")
  coefficients_model <- tibble(
    model = model_name,
    parameter = names(theta_hat)[is_mean],
    estimate = as.numeric(theta_hat[is_mean]),
    se = as.numeric(se[is_mean])
  )
  
  # (b) Uncorrelated model: |s_j| with the standard error of s_j
  if (any(startsWith(names(theta_hat), "sd."))) {
    sd_names <- paste0("sd.", rpar_names)
    coefficients_model <- bind_rows(coefficients_model, tibble(
      model = model_name,
      parameter = sd_names,
      estimate = abs(as.numeric(theta_hat[sd_names])),
      se = as.numeric(se[sd_names])
    ))
  }
  
  # (c) Correlated model: standard deviations and correlations implied by L
  if (any(startsWith(names(theta_hat), "chol."))) {
    # From a coefficient vector to (sd_1, ..., sd_K, cor_21, cor_31, cor_32, ...)
    omega_parameters <- function(theta) {
      # Cholesky factor as in section 6.3 (b): L[i, j] is "chol.<ASC j>:<ASC i>"
      K <- length(rpar_names)
      L <- matrix(0, nrow = K, ncol = K)
      for (i in 1:K) {
        for (j in 1:i) {
          L[i, j] <- theta[paste0("chol.", rpar_names[j], ":", rpar_names[i])]
        }
      }
      Omega <- L %*% t(L)
      sds <- sqrt(diag(Omega))
      correlations <- Omega / outer(sds, sds)   # element [i, j]: Omega_ij / (sd_i sd_j)
      # each correlation once: the elements below the diagonal
      result <- setNames(sds, paste0("sd.", rpar_names))
      for (i in 2:K) {
        for (j in 1:(i - 1)) {
          result[paste0("cor.", rpar_names[j], ":", rpar_names[i])] <- correlations[i, j]
        }
      }
      result
    }
    omega_estimate <- omega_parameters(theta_hat)
    J <- numDeriv::jacobian(omega_parameters, theta_hat)   # one row per sd or cor
    omega_se <- sqrt(diag(J %*% V %*% t(J)))
    
    # Check: the standard deviations agree with mlogit's covariance matrix
    stopifnot(max(abs(omega_estimate[paste0("sd.", rpar_names)] -
                        sqrt(diag(unclass(cov.mlogit(models[[model_name]])))[rpar_names]))) < 1e-8)
    
    coefficients_model <- bind_rows(coefficients_model, tibble(
      model = model_name,
      parameter = names(omega_estimate),
      estimate = as.numeric(omega_estimate),
      se = as.numeric(omega_se)
    ))
  }
  
  # (d) Tests and intervals
  coefficients_model <- coefficients_model %>%
    mutate(is_sd = startsWith(parameter, "sd."),
           is_cor = startsWith(parameter, "cor."),
           z = if_else(is_sd, NA_real_, estimate / se),
           p = 2 * pnorm(-abs(z)),
           ci_low = estimate - z_critical * se,
           ci_high = estimate + z_critical * se,
           ci_low = case_when(is_sd ~ pmax(0, ci_low),
                              is_cor ~ pmax(-1, ci_low),
                              TRUE ~ ci_low),
           ci_high = if_else(is_cor, pmin(1, ci_high), ci_high)) %>%
    select(-is_sd, -is_cor)
  coefficient_table <- bind_rows(coefficient_table, coefficients_model)
}

# 7.2 AMEs: Krinsky-Robb standard errors ---------------------------------------
# The AMEs are nonlinear functions of theta, g(theta) = ame_at_reference(theta)$ame.
# Krinsky-Robb passes the uncertainty in theta_hat through this function:
#   1. draw theta_1, ..., theta_S from N(theta_hat, V), the same distribution
#      that underlies the coefficient standard errors in 7.1
#   2. compute the AMEs at every draw, with the same standard_draws z_r as the
#      point estimates, so that the spread reflects only the uncertainty in
#      theta and not new simulation noise
#   3. the standard deviation of the S AMEs is the standard error; their 2.5%
#      and 97.5% quantiles are a percentile confidence interval
# The point estimate stays the AME at theta_hat from section 6, not the mean of
# the draws. Runtime: n_draws_kr evaluations of ame_at_reference() per model.

set.seed(20261003)
kr_se <- list()        # standard errors per model, kept for the checks below
kr_table <- tibble()
for (model_name in names(models)) {
  theta_hat <- coef(models[[model_name]])
  V <- covariance_matrices[[model_name]][names(theta_hat), names(theta_hat)]
  
  # (a) Draws of the coefficient vector: one row per draw, columns named as theta_hat
  theta_draws <- MASS::mvrnorm(n_draws_kr, mu = theta_hat, Sigma = V)
  
  # (b) AMEs and probabilities at the reference vignette for every draw.
  #     ame_draws[, , s] is the AME matrix (variations x outcomes) of draw s.
  ame_draws <- array(NA_real_,
                     dim = c(nrow(X_ame) - 1, length(RECOMMENDATIONS), n_draws_kr),
                     dimnames = list(rownames(X_ame)[-1], RECOMMENDATIONS, NULL))
  p_reference_draws <- matrix(NA_real_, nrow = n_draws_kr, ncol = length(RECOMMENDATIONS),
                              dimnames = list(NULL, RECOMMENDATIONS))
  for (s in 1:n_draws_kr) {
    result_s <- ame_at_reference(theta_draws[s, ], X_ame, standard_draws, rpar_names)
    ame_draws[, , s] <- result_s$ame
    p_reference_draws[s, ] <- result_s$probabilities["Reference vignette", ]
    if (s %% 100 == 0) message(model_name, ": draw ", s, " of ", n_draws_kr)
  }
  
  # (c) Summaries over the draws. apply(ame_draws, c(1, 2), f) applies f to the
  #     n_draws_kr values of each variation x outcome cell.
  ame_se <- apply(ame_draws, c(1, 2), sd)
  ame_low <- apply(ame_draws, c(1, 2), quantile, probs = 0.025)
  ame_high <- apply(ame_draws, c(1, 2), quantile, probs = 0.975)
  p_reference_se <- apply(p_reference_draws, 2, sd)
  kr_se[[model_name]] <- ame_se
  
  # (d) Long format, one row per variation x outcome, as in ame_table
  kr_model <- as_tibble(ame_se, rownames = "vignette") %>%
    pivot_longer(-vignette, names_to = "outcome", values_to = "se") %>%
    left_join(as_tibble(ame_low, rownames = "vignette") %>%
                pivot_longer(-vignette, names_to = "outcome", values_to = "ci_low_kr"),
              by = c("vignette", "outcome")) %>%
    left_join(as_tibble(ame_high, rownames = "vignette") %>%
                pivot_longer(-vignette, names_to = "outcome", values_to = "ci_high_kr"),
              by = c("vignette", "outcome")) %>%
    mutate(model = model_name,
           se_p_reference = p_reference_se[outcome])
  kr_table <- bind_rows(kr_table, kr_model)
}

# (e) Add standard errors, z, p and confidence intervals to ame_table.
#     ci_low, ci_high: estimate +- 1.96 se, consistent with z and p.
#     ci_low_kr, ci_high_kr: percentile interval of the draws. If the two
#     intervals differ markedly, the distribution of the AME is skewed and the
#     percentile interval is the better one to report.
ame_table <- ame_table %>%
  left_join(kr_table %>% mutate(outcome = factor(outcome, levels = RECOMMENDATIONS)),
            by = c("model", "vignette", "outcome")) %>%
  mutate(z = ame / se,
         p = 2 * pnorm(-abs(z)),
         ci_low = ame - z_critical * se,
         ci_high = ame + z_critical * se) %>%
  select(model, vignette, outcome, p_reference, se_p_reference,
         ame, se, z, p, ci_low, ci_high, ci_low_kr, ci_high_kr)

# 7.3 Check: delta-method standard errors ---------------------------------------
# The delta method linearises the AMEs around theta_hat,
#   Var(AME) = G V G',   G = derivative of the AMEs with respect to theta at theta_hat,
# with one row of G per AME; numDeriv::jacobian() computes G numerically. If the
# AMEs are close to linear in theta over the plausible range of theta, the
# delta-method and Krinsky-Robb standard errors agree up to the simulation
# noise of the Krinsky-Robb draws (a few percent with n_draws_kr = 1000).
for (model_name in names(models)) {
  theta_hat <- coef(models[[model_name]])
  V <- covariance_matrices[[model_name]][names(theta_hat), names(theta_hat)]
  # jacobian() needs a function of theta that returns a vector: here the AME
  # matrix stacked column by column (all variations of outcome 1, then outcome 2, ...)
  G <- numDeriv::jacobian(function(theta) {
    as.vector(ame_at_reference(theta, X_ame, standard_draws, rpar_names)$ame)
  }, theta_hat)
  # back to a matrix with the same layout as the AME matrix
  se_delta <- matrix(sqrt(diag(G %*% V %*% t(G))),
                     nrow = nrow(X_ame) - 1, dimnames = dimnames(kr_se[[model_name]]))
  ratio <- se_delta / kr_se[[model_name]]
  cat(model_name, ": delta-method / Krinsky-Robb standard errors between",
      round(min(ratio), 2), "and", round(max(ratio), 2), "\n")
}

# 8. Output: tables, figures, Excel sheets, models ------------------------------
# All outputs are in German (policy brief). Everything goes into one subfolder
# of FIRST_POLICY_BRIEF_DIR, one per arm, so that running the script for the
# other arm does not overwrite these files:
#   tabellen/     one LaTeX table per model; the LaTeX document needs
#                 \usepackage[T1]{fontenc}, \usepackage[ngerman]{babel},
#                 \usepackage{booktabs} and \usepackage{threeparttable}
#   abbildungen/  one SVG per outcome: AMEs with 95% confidence intervals
#                 (ggsave() writes SVG files with the svglite package)
#   excel/        coefficient table, AME table and fit statistics
#   modelle/      the three fitted models and their covariance matrices
# The subfolder's name contains the arm and the reference outcome.
# Numbers use the German decimal comma (0,123) and thousands point (2.400).

# 8.1 Labels and folders --------------------------------------------------------
# German labels. The names are the values as they appear in the data and in
# the coefficient names; the labels in TERM_LABELS and
# TECHNOLOGY_COLUMN_HEADERS may contain LaTeX code.
MODEL_LABELS <- c(
  mnl = "Multinomiales Logit",
  mixed = "Mixed Logit",
  mixed_corr = "Mixed Logit mit korrelierten Konstanten"
)

# Outcomes in figure titles and Excel (here the same as in the data)
TECHNOLOGY_LABELS <- c(
  "Keine Empfehlung" = "Keine Empfehlung",
  "Wärmepumpe" = "Wärmepumpe",
  "Pelletheizung" = "Pelletheizung",
  "Hybridheizung" = "Hybridheizung",
  "Gasheizung" = "Gasheizung",
  "Ölheizung" = "Ölheizung",
  "Fernwärme" = "Fernwärme"
)

# Table column headers, hyphenated over two lines (\\ is a line break in
# LaTeX), so that six columns fit the text width
TECHNOLOGY_COLUMN_HEADERS <- c(
  "Keine Empfehlung" = "Keine\\\\Empfehlung",
  "Wärmepumpe" = "Wärme-\\\\pumpe",
  "Pelletheizung" = "Pellet-\\\\heizung",
  "Hybridheizung" = "Hybrid-\\\\heizung",
  "Gasheizung" = "Gas-\\\\heizung",
  "Ölheizung" = "Öl-\\\\heizung",
  "Fernwärme" = "Fern-\\\\wärme"
)

# File names of the figures (without umlauts)
OUTCOME_FILE_NAMES <- c(
  "Keine Empfehlung" = "keine_empfehlung",
  "Wärmepumpe" = "waermepumpe",
  "Pelletheizung" = "pelletheizung",
  "Hybridheizung" = "hybridheizung",
  "Gasheizung" = "gasheizung",
  "Ölheizung" = "oelheizung",
  "Fernwärme" = "fernwaerme"
)

# Output folder: one per arm and reference outcome,
# e.g. rc_logit_fw_ref_keine_empfehlung
output_dir <- file.path(FIRST_POLICY_BRIEF_DIR,
                        paste0("rc_logit_", SELECTED_FW_ARM, "_ref_",
                               OUTCOME_FILE_NAMES[[REFERENCE_OUTCOME]]))
output_dirs <- list(
  tables = file.path(output_dir, "tabellen"),
  figures = file.path(output_dir, "abbildungen"),
  excel = file.path(output_dir, "excel"),
  models = file.path(output_dir, "modelle")
)
for (folder in output_dirs) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# Table rows: the regressors, named as in colnames(model.matrix())
TERM_LABELS <- c(
  "(Intercept)" = "Konstante",
  "att_couple_ageMitte 60" = "Paar Mitte 60",
  "att_incomeunter 40.000 € brutto im Jahr" = "Einkommen unter 40.000 Euro/Jahr",
  "att_current_heatingÖlheizung" = "Aktuelle Heizung: Öl",
  "att_replacement_timingist defekt und muss kurzfristig ersetzt werden" =
    "Heizung defekt, Ersatz kurzfristig",
  "att_build_year2000" = "Baujahr 2000",
  "att_heat_demand70" = "Wärmebedarf 70 kWh/m$^2$",
  "att_heat_demand200" = "Wärmebedarf 200 kWh/m$^2$",
  "att_heat_distributioneine Fußbodenheizung" = "Fußbodenheizung"
)

# Figure rows and Excel: the variations of the reference vignette, named as in
# section 6.1 (\u00b2 is the superscript 2, written as a code so it survives
# any file encoding)
VARIATION_LABELS <- c(
  "att_couple_age: Mitte 60" = "Paar Mitte 60 (statt Mitte 40)",
  "att_income: unter 40.000 € brutto im Jahr" = "Einkommen unter 40.000 € (statt 65.000 €)",
  "att_current_heating: Ölheizung" = "Aktuelle Heizung: Öl (statt Gas)",
  "att_replacement_timing: ist defekt und muss kurzfristig ersetzt werden" =
    "Heizung defekt (statt Ersatz in den nächsten Jahren)",
  "att_build_year: 2000" = "Baujahr 2000 (statt 1970)",
  "att_heat_demand: 70" = "Wärmebedarf 70 kWh/m\u00b2 (statt 120)",
  "att_heat_demand: 200" = "Wärmebedarf 200 kWh/m\u00b2 (statt 120)",
  "att_heat_distribution: eine Fußbodenheizung" = "Fußbodenheizung (statt Heizkörper)"
)

# Every value that appears in the results needs a label
stopifnot(
  all(RECOMMENDATIONS %in% names(TECHNOLOGY_LABELS)),
  all(RECOMMENDATIONS %in% names(OUTCOME_FILE_NAMES)),
  all(terms %in% names(TERM_LABELS)),
  all(ame_table$vignette %in% names(VARIATION_LABELS))
)

# The six technologies: the table columns
technologies <- RECOMMENDATIONS[RECOMMENDATIONS != REFERENCE_OUTCOME]
stopifnot(all(technologies %in% names(TECHNOLOGY_COLUMN_HEADERS)))

# 8.2 Fit statistics -------------------------------------------------------------
# McFadden R^2 and the likelihood-ratio (LR) test as mlogit's summary() reports
# them. Both compare the model with a model that has constants only, i.e. that
# predicts the observed share of each recommendation for every vignette. Its
# log-likelihood follows from the choice counts n_j alone:
#   logL_0 = sum_j n_j log(n_j / N)
#   R^2 = 1 - logL / logL_0,   LR = 2 (logL - logL_0),   df = parameters - 6.
# These are computed here from the formulas, because mlogit stores logL_0
# differently across versions (1.1: attribute "null" of model$logLik; 2.0:
# element "null" of the vector model$logLik). A check compares the result with
# the R^2 of summary().
# For the mixed logits a second LR test compares them with the MNL: do the
# constants vary across experts? A variance of zero lies on the boundary of the
# parameter space, so the chi-squared p-value of this test is conservative
# (too large).
chosen_counts <- choice_counts[choice_counts > 0]   # outcomes never chosen add 0
log_likelihood_constants_only <- sum(chosen_counts * log(chosen_counts / n_choices))

fit_statistics <- tibble()
for (model_name in names(models)) {
  # (not called 'model': inside tibble() that name would refer to the column)
  fitted_model <- models[[model_name]]
  log_likelihood <- as.numeric(logLik(fitted_model))
  n_parameters <- length(coef(fitted_model))
  is_mixed <- !is.null(fitted_model$rpar)
  
  # Comparison with the constants-only model
  mcfadden_r2 <- 1 - log_likelihood / log_likelihood_constants_only
  lr_constants_only <- 2 * (log_likelihood - log_likelihood_constants_only)
  lr_constants_only_df <- n_parameters - (length(RECOMMENDATIONS) - 1)
  lr_constants_only_p <- pchisq(lr_constants_only, df = lr_constants_only_df,
                                lower.tail = FALSE)
  if (abs(mcfadden_r2 - as.numeric(summary(fitted_model)$mfR2)) > 1e-8) {
    stop("McFadden R^2 differs from mlogit's summary() for ", model_name)
  }
  
  # Comparison with the MNL (mixed logits only)
  if (is_mixed) {
    lr_mnl <- 2 * (log_likelihood - as.numeric(logLik(models$mnl)))
    lr_mnl_df <- n_parameters - length(coef(models$mnl))
    lr_mnl_p <- pchisq(lr_mnl, df = lr_mnl_df, lower.tail = FALSE)
  } else {
    lr_mnl <- NA_real_
    lr_mnl_df <- NA_real_
    lr_mnl_p <- NA_real_
  }
  
  fit_statistics <- bind_rows(fit_statistics, tibble(
    model = model_name,
    n_choices = n_choices,
    n_experts = n_experts,
    n_parameters = n_parameters,
    n_draws_per_expert = if (is_mixed) n_draws_model else NA_real_,
    log_likelihood = log_likelihood,
    log_likelihood_constants_only = log_likelihood_constants_only,
    mcfadden_r2 = mcfadden_r2,
    lr_constants_only = lr_constants_only,
    lr_constants_only_df = lr_constants_only_df,
    lr_constants_only_p = lr_constants_only_p,
    lr_mnl = lr_mnl,
    lr_mnl_df = lr_mnl_df,
    lr_mnl_p = lr_mnl_p
  ))
}
# tibble() silently returns zero rows if one of its values has length 0, so
# check that every model has its row
stopifnot(nrow(fit_statistics) == length(models))
print(fit_statistics)

# 8.3 LaTeX tables, one per model ------------------------------------------------
# Layout: one column per technology. Panel A: one coefficient per regressor and
# technology, with its standard error in parentheses below. Mixed logits: panel
# B with the standard deviations of the constants across experts; correlated
# model: panel C with their correlations (lower triangle). Panel D: fit.

#' Numbers as LaTeX text in German format: fixed number of digits, decimal
#' comma, thousands point, a proper minus sign ($-$), "--" for a missing value
latex_number <- function(x, digits) {
  text <- formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  text <- sub("^-", "$-$", text)
  text[is.na(x)] <- "--"
  text
}

#' Number inside a LaTeX formula ($...$): the decimal comma is written {,},
#' because LaTeX puts a space after a plain comma in formulas
math_number <- function(x, digits) {
  sub(",", "{,}", formatC(x, format = "f", digits = digits, decimal.mark = ","),
      fixed = TRUE)
}

#' p-value as LaTeX text
latex_p <- function(p) {
  if (p < 0.001) "$p<0{,}001$" else paste0("$p=", math_number(p, 3), "$")
}

# Formatted cells: estimate with stars, standard error in parentheses. Rows
# without a p-value (standard deviations, see section 7.1) get no stars.
coefficient_text <- coefficient_table %>%
  mutate(
    stars = case_when(
      is.na(p) ~ "",
      p < 0.01 ~ "$^{***}$",
      p < 0.05 ~ "$^{**}$",
      p < 0.10 ~ "$^{*}$",
      TRUE ~ ""
    ),
    estimate_text = paste0(latex_number(estimate, 3), stars),
    se_text = paste0("(", latex_number(se, 3), ")")
  )

# Text of the notes that is the same for all models
sample_note <- if (SELECTED_FW_ARM == "fw") {
  "Stichprobe: Befragte, die Fernwärme empfehlen konnten."
} else {
  "Stichprobe: Befragte, die Fernwärme nicht empfehlen konnten."
}
reference_note <- paste(
  "Referenzvignette: Paar Mitte 40, Bruttoeinkommen 65.000 Euro im Jahr,",
  "Gasheizung, die in den nächsten Jahren ersetzt werden muss, Baujahr 1970,",
  "Wärmebedarf 120 kWh/m$^2$, Heizkörper."
)

# Column headers: a one-column tabular inside each header cell, so that the
# line break in TECHNOLOGY_COLUMN_HEADERS works
column_headers <- paste0("\\begin{tabular}[b]{@{}c@{}}",
                         TECHNOLOGY_COLUMN_HEADERS[technologies],
                         "\\end{tabular}")

n_columns <- length(technologies) + 1                # row labels + technologies
span <- paste0("\\multicolumn{", length(technologies), "}{c}{")   # fit rows

for (model_name in names(models)) {
  model_rows <- coefficient_text %>% filter(model == model_name)
  # Look-up tables: parameter name -> formatted estimate / standard error
  estimate_of <- setNames(model_rows$estimate_text, model_rows$parameter)
  se_of <- setNames(model_rows$se_text, model_rows$parameter)
  fit <- fit_statistics %>% filter(model == model_name)
  is_mixed <- any(startsWith(model_rows$parameter, "sd."))
  is_correlated <- any(startsWith(model_rows$parameter, "cor."))
  
  # (a) Head of the table
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{Empfohlene Heiztechnologie: ", MODEL_LABELS[[model_name]],
           " (Referenzkategorie: ", REFERENCE_OUTCOME, ")}"),
    paste0("\\label{tab:rcl_", SELECTED_FW_ARM, "_ref_",
           OUTCOME_FILE_NAMES[[REFERENCE_OUTCOME]], "_", model_name, "}"),
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{4pt}",
    paste0("\\begin{tabular}{l", strrep("c", length(technologies)), "}"),
    "\\toprule",
    paste0(" & ", paste(column_headers, collapse = " & "), " \\\\"),
    "\\midrule",
    paste0("\\multicolumn{", n_columns, "}{l}{\\textit{A. Koeffizienten}} \\\\")
  )
  
  # (b) Panel A: two rows per regressor, the estimates and the standard errors
  for (term in terms) {
    parameters <- paste0(term, ":", technologies)
    if (anyNA(estimate_of[parameters])) stop("Coefficients missing for ", term)
    label <- TERM_LABELS[[term]]
    if (term == "(Intercept)" && is_mixed) label <- "Konstante (Mittelwert)"
    lines <- c(lines,
               paste0(label, " & ", paste(estimate_of[parameters], collapse = " & "), " \\\\"),
               paste0(" & ", paste(se_of[parameters], collapse = " & "), " \\\\"))
  }
  
  # (c) Panel B: standard deviations of the constants across experts
  if (is_mixed) {
    parameters <- paste0("sd.(Intercept):", technologies)
    lines <- c(lines,
               "\\midrule",
               paste0("\\multicolumn{", n_columns,
                      "}{l}{\\textit{B. Standardabweichung der Konstanten zwischen den Befragten}} \\\\"),
               paste0("Konstante & ", paste(estimate_of[parameters], collapse = " & "), " \\\\"),
               paste0(" & ", paste(se_of[parameters], collapse = " & "), " \\\\"))
  }
  
  # (d) Panel C: correlations of the constants, lower triangle: the row of
  #     technology i has the correlations with the technologies j left of it.
  #     Section 7.1 names each correlation once, with the two constants in
  #     mlogit's order, so both orders of the pair are tried.
  if (is_correlated) {
    lines <- c(lines,
               "\\midrule",
               paste0("\\multicolumn{", n_columns,
                      "}{l}{\\textit{C. Korrelation der Konstanten zwischen den Befragten}} \\\\"))
    for (i in 2:length(technologies)) {
      estimate_cells <- rep("", length(technologies))
      se_cells <- rep("", length(technologies))
      for (j in 1:(i - 1)) {
        name_ji <- paste0("cor.(Intercept):", technologies[j], ":(Intercept):", technologies[i])
        name_ij <- paste0("cor.(Intercept):", technologies[i], ":(Intercept):", technologies[j])
        parameter <- if (name_ji %in% names(estimate_of)) name_ji else name_ij
        if (!parameter %in% names(estimate_of)) stop("Correlation missing: ", parameter)
        estimate_cells[j] <- estimate_of[[parameter]]
        se_cells[j] <- se_of[[parameter]]
      }
      lines <- c(lines,
                 paste0(TECHNOLOGY_LABELS[[technologies[i]]], " & ",
                        paste(estimate_cells, collapse = " & "), " \\\\"),
                 paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"))
    }
  }
  
  # (e) Panel D: fit statistics, each value centred across the technology columns
  lines <- c(lines,
             "\\midrule",
             paste0("\\multicolumn{", n_columns, "}{l}{\\textit{D. Modellgüte}} \\\\"),
             paste0("Entscheidungen & ", span, latex_number(fit$n_choices, 0), "} \\\\"),
             paste0("Befragte & ", span, latex_number(fit$n_experts, 0), "} \\\\"),
             paste0("Parameter & ", span, fit$n_parameters, "} \\\\"),
             paste0("Log-Likelihood & ", span, latex_number(fit$log_likelihood, 1), "} \\\\"),
             paste0("McFadden-$R^2$ & ", span, latex_number(fit$mcfadden_r2, 3), "} \\\\"),
             paste0("LR-Test gegen Nullmodell & ", span,
                    "$\\chi^2(", fit$lr_constants_only_df, ")=",
                    math_number(fit$lr_constants_only, 1), "$, ",
                    latex_p(fit$lr_constants_only_p), "} \\\\"))
  if (is_mixed) {
    lines <- c(lines,
               paste0("LR-Test gegen multinomiales Logit & ", span,
                      "$\\chi^2(", fit$lr_mnl_df, ")=", math_number(fit$lr_mnl, 1), "$, ",
                      latex_p(fit$lr_mnl_p), "} \\\\"),
               paste0("Halton-Ziehungen pro Person & ", span, fit$n_draws_per_expert, "} \\\\"))
  }
  
  # (f) Notes
  random_constants_note <- if (!is_mixed) {
    NULL
  } else if (!is_correlated) {
    paste("Die Konstanten sind zwischen den Befragten unabhängig normalverteilt;",
          "Panel A zeigt ihre Mittelwerte, Panel B ihre Standardabweichungen.")
  } else {
    paste("Die Konstanten sind zwischen den Befragten gemeinsam normalverteilt;",
          "Panel A zeigt ihre Mittelwerte, Panel B und C ihre Standardabweichungen",
          "und Korrelationen mit Standardfehlern nach der Delta-Methode.")
  }
  notes <- c(
    paste(if (is_mixed) "Mixed-Logit-Modell" else "Multinomiales Logit-Modell",
          "der Heiztechnologie, die Befragte für den Haushalt einer Vignette",
          paste0("empfehlen. Referenzkategorie ist \\textit{", REFERENCE_OUTCOME, "}:"),
          "Jeder Koeffizient gibt die Wirkung auf die logarithmierte Chance",
          "(Log-Odds) der Empfehlung in seiner Spalte relativ zur",
          "Referenzkategorie an."),
    random_constants_note,
    reference_note,
    paste("Standardfehler in Klammern, geclustert nach Befragten",
          "(Sandwich-Schätzer). $^{*}$ $p<0{,}10$, $^{**}$ $p<0{,}05$,",
          "$^{***}$ $p<0{,}01$ (zweiseitige $z$-Tests)."),
    if (is_mixed) {
      paste("Standardabweichungen tragen keine Sterne, da eine Standardabweichung",
            "von null auf dem Rand des Parameterraums liegt.")
    },
    paste("McFadden-$R^2$ und LR-Test beziehen sich auf das Nullmodell, ein Modell",
          "nur mit Konstanten, das für jede Vignette die beobachteten Anteile der",
          "Empfehlungen vorhersagt."),
    if (is_mixed) {
      paste("Der LR-Test gegen das multinomiale Logit prüft, ob die Konstanten",
            "zwischen den Befragten variieren; sein $p$-Wert ist konservativ, da",
            "eine Varianz von null auf dem Rand des Parameterraums liegt.")
    },
    sample_note
  )
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\begin{tablenotes}[flushleft]",
             "\\footnotesize",
             paste0("\\item \\textit{Anmerkungen:} ", paste(notes, collapse = " ")),
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}")
  
  # (g) Write the file (UTF-8, so the umlauts are read the same on every system)
  table_file <- file(file.path(output_dirs$tables, paste0("tab_rcl_", model_name, ".tex")),
                     open = "w", encoding = "UTF-8")
  writeLines(lines, table_file)
  close(table_file)
}

# 8.4 Figures: AMEs at the reference vignette, one per outcome --------------------
# Each figure shows, for one outcome, the eight variations of the reference
# vignette: the AME (point) and the 95% Krinsky-Robb percentile interval (line)
# of each model, in percentage points. For the intervals estimate +- 1.96 se
# instead, use ci_low and ci_high.
figure_data <- ame_table %>%
  mutate(
    variation = factor(VARIATION_LABELS[vignette], levels = rev(VARIATION_LABELS)),
    model = factor(MODEL_LABELS[model], levels = MODEL_LABELS),
    ame = 100 * ame,
    ci_low = 100 * ci_low_kr,
    ci_high = 100 * ci_high_kr,
    p_reference = 100 * p_reference
  )

for (outcome_name in RECOMMENDATIONS) {
  plot_data <- figure_data %>% filter(outcome == outcome_name)
  
  # Subtitle: probability of this outcome at the reference vignette, per model,
  # e.g. "40,2 % (Multinomiales Logit)"
  baseline <- plot_data %>% distinct(model, p_reference) %>% arrange(model)
  baseline_text <- paste0(formatC(baseline$p_reference, format = "f", digits = 1,
                                  decimal.mark = ","),
                          " % (", baseline$model, ")")
  subtitle <- paste0("Wahrscheinlichkeit bei der Referenzvignette:\n",
                     paste(baseline_text, collapse = ", "))
  
  # coord_flip() puts the variations on the vertical axis; position_dodge()
  # places the three models next to each other within each variation, in the
  # order of `group`: reversed, so that the first model is at the top
  plot <- ggplot(plot_data, aes(x = variation, y = ame, ymin = ci_low, ymax = ci_high,
                                colour = model, shape = model,
                                group = factor(model, levels = rev(MODEL_LABELS)))) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(position = position_dodge(width = 0.6), size = 0.4) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
    scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
    scale_shape_manual(values = c(16, 17, 15)) +
    labs(title = TECHNOLOGY_LABELS[[outcome_name]],
         subtitle = subtitle,
         x = NULL,
         y = "Veränderung der Wahrscheinlichkeit in Prozentpunkten (95-%-Konfidenzintervall)",
         colour = NULL, shape = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.subtitle = element_text(size = 9),
          axis.title.x = element_text(size = 10),
          plot.title.position = "plot")
  
  file_name <- paste0("abb_ame_", OUTCOME_FILE_NAMES[[outcome_name]], ".svg")
  ggsave(file.path(output_dirs$figures, file_name), plot, width = 8, height = 5)
}

# 8.5 Excel: coefficient table, AME table and fit statistics ---------------------
# One workbook with one sheet per table, with German column names and labels.
# The numbers are unrounded; Excel shows them with the decimal comma of its
# language setting.
coefficients_excel <- coefficient_table %>%
  transmute(Modell = unname(MODEL_LABELS[model]),
            Parameter = parameter,
            `Schätzwert` = estimate,
            Standardfehler = se,
            z = z,
            p = p,
            `KI unten (95 %)` = ci_low,
            `KI oben (95 %)` = ci_high)

ames_excel <- ame_table %>%
  transmute(Modell = unname(MODEL_LABELS[model]),
            Variation = unname(VARIATION_LABELS[vignette]),
            Empfehlung = unname(TECHNOLOGY_LABELS[as.character(outcome)]),
            `Wahrscheinlichkeit Referenzvignette` = p_reference,
            `Standardfehler Wahrscheinlichkeit` = se_p_reference,
            `Marginaler Effekt` = ame,
            Standardfehler = se,
            z = z,
            p = p,
            `KI unten (95 %)` = ci_low,
            `KI oben (95 %)` = ci_high,
            `KI unten (95 %, Krinsky-Robb)` = ci_low_kr,
            `KI oben (95 %, Krinsky-Robb)` = ci_high_kr)

fit_excel <- fit_statistics %>%
  transmute(Modell = unname(MODEL_LABELS[model]),
            Entscheidungen = n_choices,
            Befragte = n_experts,
            Parameter = n_parameters,
            `Halton-Ziehungen pro Person` = n_draws_per_expert,
            `Log-Likelihood` = log_likelihood,
            `Log-Likelihood Nullmodell` = log_likelihood_constants_only,
            `McFadden-R2` = mcfadden_r2,
            `LR-Test Nullmodell: Chi2` = lr_constants_only,
            `LR-Test Nullmodell: Freiheitsgrade` = lr_constants_only_df,
            `LR-Test Nullmodell: p` = lr_constants_only_p,
            `LR-Test MNL: Chi2` = lr_mnl,
            `LR-Test MNL: Freiheitsgrade` = lr_mnl_df,
            `LR-Test MNL: p` = lr_mnl_p)

writexl::write_xlsx(
  list(Koeffizienten = coefficients_excel,
       `Marginale Effekte` = ames_excel,
       `Modellgüte` = fit_excel),
  path = file.path(output_dirs$excel, "rc_logit_ergebnisse.xlsx")
)

# 8.6 Models and covariance matrices ------------------------------------------------
# readRDS() restores a model; the covariance matrices are stored as well, since
# the numerical Hessian of the mixed logits takes long to compute
for (model_name in names(models)) {
  saveRDS(models[[model_name]],
          file.path(output_dirs$models, paste0("m_", model_name, ".rds")))
}
saveRDS(covariance_matrices, file.path(output_dirs$models, "covariance_matrices.rds"))

cat("Output written to", output_dir, "\n")