################################################################################
# 01_run_specifications.R — which heating technology do experts recommend?
#                           Mixed logit specifications, to study how
#                           recommendations differ across experts
# Inputs : survey_main and vignettes_long (CLEAN_DATA_DIR), through
#          load_analysis_data()
# Outputs: RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/<specification>.rds
#            aggregate results: coefficients, covariance matrices, AMEs, fit
#          RC_LOGIT_MODEL_DIR/rc_logit_<arm>_ref_<outcome>/<specification>.rds
#            the fitted models, which contain the data (secure drive)
# Checks : experts and decisions dropped by each restriction; no missing
#          values in the estimation variables; the specifications; per
#          specification: a design matrix of full rank, coefficient names,
#          convergence, estimation sample, and the checks of section 6.5 of
#          the policy brief script
################################################################################
# Every specification in SPECIFICATIONS (section 3) is estimated on the same
# sample: a multinomial logit with the same regressors, the mixed logit, their
# cluster-robust covariance matrices, the AMEs at the reference vignette with
# Krinsky-Robb standard errors, and the fit statistics. 02_output.R turns the
# saved results into tables, figures and an Excel workbook. The functions are
# in 00_functions_estimation.R.
#
#   add or change a specification  section 3; to report a new one, give it a
#                                  number in SPECIFICATION_NUMBERS of
#                                  02_output.R
#   add a variable                 add it to `experts` or `decisions`
#                                  (section 2), then to the regressors of a
#                                  specification
#   rerun some specifications      list them in SPECIFICATIONS_TO_RUN
#                                  (section 3)
#
# Runtime: two steps take most of it. The estimation and numerical Hessian of
# the mixed logits grow with the number of parameters, in particular of the
# random coefficients (the specifications with one set of constants per
# Berufsgruppe have three times as many); the Krinsky-Robb standard errors
# grow with the number of profiles. See the README, section "Heterogeneity of
# the recommendations".

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

# load_analysis_data(), BERUFSGRUPPEN
source(file.path("R", "prepare_analysis_data.R"))
source(file.path("R", "analysis", "00_functions_estimation.R"))

# 1. Settings ------------------------------------------------------------------

# Halton draws per expert in the estimation of the mixed logits
n_draws_model <- 1000

# Draws of the random coefficients used to average the predicted probabilities
# over experts (marginal effects). The simulation error of one probability is
# about 1.1 percentage points with 1,000 draws and about 0.1 with 100,000.
n_draws_ame <- 100000

# Draws for check (c) in section 5.4, which evaluates every observed vignette
# and needs no more precision than this
n_draws_check <- 100

# Draws of the coefficient vector for the Krinsky-Robb standard errors
n_draws_kr <- 1000

# Estimate for the experts who have "Fernwärme" in their choice set ("fw") or
# not ("nofw")
SELECTED_FW_ARM <- "fw"

# The outcome whose utility is normalised to 0
REFERENCE_OUTCOME <- "Wärmepumpe"

# A runner script can pass the arm and the reference outcome in RUN_SETTINGS,
# as R/first_policy_brief/03_run_all_specifications.R does
if (exists("RUN_SETTINGS", inherits = FALSE)) {
  SELECTED_FW_ARM <- RUN_SETTINGS$arm
  REFERENCE_OUTCOME <- RUN_SETTINGS$reference
}
cat("Arm:", SELECTED_FW_ARM, "| reference outcome:", REFERENCE_OUTCOME, "\n")

# The possible recommendations, seven in the fw arm and six in the nofw arm
# (no Fernwärme); the reference outcome comes first
if (SELECTED_FW_ARM == "fw") {
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
# the reference category and, all together, the reference vignette of the AMEs
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

# 2. Data ----------------------------------------------------------------------
analysis_data <- load_analysis_data(completers_only = TRUE)
experts_all <- analysis_data$main
vignettes_all <- analysis_data$vignettes

# 2.1 Expert characteristics ---------------------------------------------------
# One row per expert. Add expert-level variables here, e.g. from survey_main or
# from regional data joined by nuts3; the join below brings them to the
# decisions, where a specification can use them as regressors.
experts <- experts_all %>%
  select(resp_uid)

n_vignettes_all <- nrow(vignettes_all)
decisions <- vignettes_all %>%
  left_join(experts, by = "resp_uid", relationship = "many-to-one")
stopifnot(nrow(decisions) == n_vignettes_all)

# 2.2 Sample -------------------------------------------------------------------
# (a) District heating is an option only in the fw arm (between-subject), so
#     each arm is estimated on its own: the one set in SELECTED_FW_ARM
decisions <- decisions %>%
  filter(vig_arm == SELECTED_FW_ARM)
if (nrow(decisions) == 0) stop("No vignettes for the arm ", SELECTED_FW_ARM)
cat("Arm", SELECTED_FW_ARM, ": experts", n_distinct(decisions$resp_uid),
    "| decisions", nrow(decisions), "\n")

# (b) The specifications with Berufsgruppe need one for every expert, so the
#     filter keeps the three Berufsgruppen: it drops "Keine Zuordnung" and the
#     experts who did not answer Q2 (berufsgruppe NA; %in% gives FALSE for NA).
#     It applies to every specification, so that all are estimated on the
#     same sample and their log-likelihoods can be compared. Dropped, by
#     reason:
dropped_berufsgruppe <- decisions %>%
  filter(!(berufsgruppe %in% BERUFSGRUPPEN)) %>%
  group_by(berufsgruppe) %>%
  summarise(experts = n_distinct(resp_uid), decisions = n())
print(dropped_berufsgruppe)

decisions <- decisions %>%
  filter(berufsgruppe %in% BERUFSGRUPPEN)
cat("With a Berufsgruppe: experts", n_distinct(decisions$resp_uid),
    "| decisions", nrow(decisions), "\n")

# (c) A vignette without a recommendation (not answered, vig_rec NA) carries
#     no choice. mlogit() drops such choice situations without notice, as it
#     did in the policy brief; here they are dropped visibly. Their experts
#     stay in the sample with their other vignettes.
cat("Vignettes without a recommendation:", sum(is.na(decisions$vig_rec)),
    "of", nrow(decisions), "\n")
decisions <- decisions %>%
  filter(!is.na(vig_rec))

# 2.3 Variables of the specifications ------------------------------------------
# berufsgruppe as a regressor: SHK-Handwerk, the first level of BERUFSGRUPPEN,
# is the reference category; "Keine Zuordnung" is no longer a level, as no
# expert in the sample has it. The interactions with berufsgruppe use the same
# reference.
# is_*: one dummy per Berufsgruppe, for one set of constants per Berufsgruppe
# (estimate_specification() checks that they sum to one in every row).
decisions <- decisions %>%
  mutate(
    berufsgruppe = factor(berufsgruppe, levels = BERUFSGRUPPEN),
    is_shk = if_else(berufsgruppe == "SHK-Handwerk", 1, 0),
    is_schornsteinfeger = if_else(berufsgruppe == "Schornsteinfeger", 1, 0),
    is_energieberater = if_else(berufsgruppe == "Energieberater", 1, 0)
  )

# 3. Specifications ------------------------------------------------------------
# One element per specification; its name is the name of its result files.
# Each states its model in full:
#   label        name in tables and figures
#   constants    "1": one constant per technology for all experts, the term
#                "(Intercept)"; or 0/1 group dummies that sum to one for every
#                expert: one constant per technology and group, each group's
#                a term named as its dummy
#   regressors   terms with one coefficient per technology: a variable (a
#                factor gets one per level but the first, its reference) or
#                an interaction "a:b" (one per pair of levels but the first;
#                list a and b as well, or for the Berufsgruppe its group
#                constants)
#   random       the terms whose coefficients vary across experts, each with
#                its distribution: "n" normal, "ln" log-normal, "cn" censored
#                normal, "u" uniform, "t" triangular. Every other coefficient
#                is fixed.
#   correlation  "none": the random coefficients are independent; "full":
#                one covariance matrix for all of them; or a list of blocks of
#                random terms: the coefficients of a block are correlated
#                (across terms and technologies), those of different blocks
#                are not. Uniform and triangular coefficients only with "none".
#   draws        "halton": mlogit's Halton draws; "halton_by_group": the
#                primes start again for each group's constants
#                (halton_primes() in 00_functions_estimation.R)
# For example, random = c("(Intercept)" = "n", att_income = "n") lets the
# effect of the low income vary across experts as well; with correlation =
# list("(Intercept)", "att_income"), the constants are correlated with each
# other and so are the income effects, but the constants are not correlated
# with the income effects. check_specification() stops on a specification
# whose elements do not fit together.

# The building blocks of the specifications below.
# Vignette attributes, each a factor with the levels of ATTRIBUTE_LEVELS
ATTRIBUTES <- c(
  "att_couple_age",
  "att_income",
  "att_current_heating",
  "att_replacement_timing",
  "att_build_year",
  "att_heat_demand",
  "att_heat_distribution"
)

# Interactions of the Berufsgruppe with the attributes: by how much the effect
# of an attribute differs between a Berufsgruppe and the SHK-Handwerk (the
# reference). Each gives one coefficient per technology for each Berufsgruppe
# but the reference and each level of the attribute but the reference.
BERUFSGRUPPE_X_ATTRIBUTES <- c(
  "berufsgruppe:att_couple_age",
  "berufsgruppe:att_income",
  "berufsgruppe:att_current_heating",
  "berufsgruppe:att_replacement_timing",
  "berufsgruppe:att_build_year",
  "berufsgruppe:att_heat_demand",
  "berufsgruppe:att_heat_distribution"
)

# One set of constants per Berufsgruppe: the dummies of section 2.3
BERUFSGRUPPE_CONSTANTS <- c("is_shk", "is_schornsteinfeger", "is_energieberater")

SPECIFICATIONS <- list(
  # (1) random constants, independent of each other
  asc_uncorrelated = list(
    label = "Random constants, uncorrelated",
    constants = "1",
    regressors = ATTRIBUTES,
    random = c("(Intercept)" = "n"),
    correlation = "none",
    draws = "halton"
  ),
  # (2) random constants with a full covariance matrix
  asc_correlated = list(
    label = "Random constants, correlated",
    constants = "1",
    regressors = ATTRIBUTES,
    random = c("(Intercept)" = "n"),
    correlation = "full",
    draws = "halton"
  ),
  # (3) as (1), with the Berufsgruppe shifting the means of the constants
  asc_uncorrelated_berufsgruppe = list(
    label = "Random constants, uncorrelated, with occupational group",
    constants = "1",
    regressors = c("berufsgruppe", ATTRIBUTES),
    random = c("(Intercept)" = "n"),
    correlation = "none",
    draws = "halton"
  ),
  # (4) as (2), with the Berufsgruppe shifting the means of the constants
  asc_correlated_berufsgruppe = list(
    label = "Random constants, correlated, with occupational group",
    constants = "1",
    regressors = c("berufsgruppe", ATTRIBUTES),
    random = c("(Intercept)" = "n"),
    correlation = "full",
    draws = "halton"
  ),
  # (5) one set of independent random constants per Berufsgruppe
  asc_by_group_uncorrelated = list(
    label = "Random constants by occupational group, uncorrelated",
    constants = BERUFSGRUPPE_CONSTANTS,
    regressors = ATTRIBUTES,
    random = c(is_shk = "n", is_schornsteinfeger = "n", is_energieberater = "n"),
    correlation = "none",
    draws = "halton_by_group"
  ),
  # (6) one set of random constants per Berufsgruppe, correlated within the
  #     Berufsgruppe: one correlation block per Berufsgruppe. Correlations
  #     across Berufsgruppen, e.g. cor(SHK: Fernwärme, Schornsteinfeger:
  #     Gasheizung), never enter the likelihood: they are set to 0 as a
  #     normalisation, not estimated (cholesky_fixed_at_zero())
  asc_by_group_correlated = list(
    label = "Random constants by occupational group, correlated",
    constants = BERUFSGRUPPE_CONSTANTS,
    regressors = ATTRIBUTES,
    random = c(is_shk = "n", is_schornsteinfeger = "n", is_energieberater = "n"),
    correlation = list("is_shk", "is_schornsteinfeger", "is_energieberater"),
    draws = "halton_by_group"
  ),
  # (3i) to (6i): (3) to (6), with the effects of the attributes differing by
  #      Berufsgruppe. In (5i) and (6i) the Berufsgruppen share no parameter,
  #      so each is a separate model per Berufsgruppe, estimated together.
  asc_uncorrelated_berufsgruppe_interactions = list(
    label = "Random constants, uncorrelated, with occupational group and interactions",
    constants = "1",
    regressors = c("berufsgruppe", ATTRIBUTES, BERUFSGRUPPE_X_ATTRIBUTES),
    random = c("(Intercept)" = "n"),
    correlation = "none",
    draws = "halton"
  ),
  asc_correlated_berufsgruppe_interactions = list(
    label = "Random constants, correlated, with occupational group and interactions",
    constants = "1",
    regressors = c("berufsgruppe", ATTRIBUTES, BERUFSGRUPPE_X_ATTRIBUTES),
    random = c("(Intercept)" = "n"),
    correlation = "full",
    draws = "halton"
  ),
  asc_by_group_uncorrelated_interactions = list(
    label = "Random constants by occupational group, uncorrelated, with interactions",
    constants = BERUFSGRUPPE_CONSTANTS,
    regressors = c(ATTRIBUTES, BERUFSGRUPPE_X_ATTRIBUTES),
    random = c(is_shk = "n", is_schornsteinfeger = "n", is_energieberater = "n"),
    correlation = "none",
    draws = "halton_by_group"
  ),
  asc_by_group_correlated_interactions = list(
    label = "Random constants by occupational group, correlated, with interactions",
    constants = BERUFSGRUPPE_CONSTANTS,
    regressors = c(ATTRIBUTES, BERUFSGRUPPE_X_ATTRIBUTES),
    random = c(is_shk = "n", is_schornsteinfeger = "n", is_energieberater = "n"),
    correlation = list("is_shk", "is_schornsteinfeger", "is_energieberater"),
    draws = "halton_by_group"
  )
)

# The specifications this run estimates; the others keep their saved results
SPECIFICATIONS_TO_RUN <- "asc_by_group_correlated" # names(SPECIFICATIONS)

# Expert types at which the AMEs are evaluated, one row each. A specification
# that uses one of these columns gets AMEs for every profile; the others get a
# single profile, "All experts". A new expert characteristic in a
# specification needs a column here, e.g. its sample mean.
AME_PROFILES <- tibble(
  profile = BERUFSGRUPPEN,
  berufsgruppe = factor(BERUFSGRUPPEN, levels = BERUFSGRUPPEN),
  is_shk = c(1, 0, 0),
  is_schornsteinfeger = c(0, 1, 0),
  is_energieberater = c(0, 0, 1)
)

# Every attribute of a specification needs its levels
stopifnot(all(ATTRIBUTES %in% names(ATTRIBUTE_LEVELS)))
for (spec_name in names(SPECIFICATIONS)) {
  check_specification(SPECIFICATIONS[[spec_name]], spec_name)
}
if (!all(SPECIFICATIONS_TO_RUN %in% names(SPECIFICATIONS))) {
  stop("Not in SPECIFICATIONS: ",
       paste(setdiff(SPECIFICATIONS_TO_RUN, names(SPECIFICATIONS)), collapse = ", "))
}

# 4. Estimation data in mlogit's format ----------------------------------------
# Built from the variables of all specifications, so that every specification
# has the same sample even when only some of them run. all.vars() lists the
# variables of a formula, e.g. berufsgruppe and att_income for the term
# berufsgruppe:att_income.
model_variables <- character(0)
for (spec_name in names(SPECIFICATIONS)) {
  specification <- SPECIFICATIONS[[spec_name]]
  spec_terms <- specification$regressors
  if (!identical(specification$constants, "1")) {
    spec_terms <- c(specification$constants, spec_terms)
  }
  spec_formula <- as.formula(paste("~", paste(c("1", spec_terms),
                                               collapse = " + ")))
  model_variables <- c(model_variables, all.vars(spec_formula))
}
prepared <- prepare_choice_data(decisions, RECOMMENDATIONS, ATTRIBUTE_LEVELS,
                                model_variables)
decisions <- prepared$decisions
choice_data <- prepared$choice_data

n_choices <- nrow(decisions)
n_experts <- n_distinct(decisions$resp_uid)
choice_counts <- table(decisions$vig_rec)
cat("Estimation sample: experts", n_experts, "| choice situations", n_choices, "\n")
print(choice_counts)

# Experts per Berufsgruppe: each group's constants, and in (3i) to (6i) its
# interactions, are estimated from its experts alone
experts_by_group <- decisions %>%
  distinct(resp_uid, berufsgruppe) %>%
  count(berufsgruppe, name = "experts")
print(experts_by_group)

# 5. Estimate and save, one specification at a time ----------------------------
# Each step is one function of 00_functions_estimation.R. The models are saved
# right after the estimation, so that a check that fails later does not
# discard them; the aggregate results are saved at the end. A specification
# that stops with an error is recorded, and the loop continues with the next.
run_dir <- run_folder(SELECTED_FW_ARM, REFERENCE_OUTCOME)
results_dir <- file.path(RC_LOGIT_DIR, run_dir, "results")
model_dir <- file.path(RC_LOGIT_MODEL_DIR, run_dir)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

run_status <- tibble(specification = SPECIFICATIONS_TO_RUN,
                     status = NA_character_,
                     minutes = NA_real_)

for (s in seq_along(SPECIFICATIONS_TO_RUN)) {
  spec_name <- SPECIFICATIONS_TO_RUN[s]
  specification <- SPECIFICATIONS[[spec_name]]
  message("\n==== Specification ", s, " of ", length(SPECIFICATIONS_TO_RUN), ": ",
          spec_name, " ====")
  start_time <- Sys.time()   # Sys.time(): the current date and time

  # Results of an earlier run of this specification are removed first, so that
  # 02_output.R cannot read them as current if this run stops with an error
  results_file <- file.path(results_dir, paste0(spec_name, ".rds"))
  model_file <- file.path(model_dir, paste0(spec_name, ".rds"))
  if (file.exists(results_file)) file.remove(results_file)
  if (file.exists(model_file)) file.remove(model_file)

  # tryCatch() runs the block; if a step stops with an error, it returns the
  # error message (conditionMessage()) instead, and the loop continues
  run_status$status[s] <- tryCatch({
    # 5.1 Multinomial and mixed logit ------------------------------------------
    estimates <- estimate_specification(specification, choice_data, decisions,
                                        REFERENCE_OUTCOME, n_draws_model)
    models <- list(mnl = estimates$mnl, mixed = estimates$mixed)
    saveRDS(models, model_file)
    for (model_name in names(models)) {
      check_estimation(models[[model_name]], model_name, n_choices)
    }
    random <- estimates$random_coefficients
    rpar_names <- random$name
    # Elements of L fixed at 0 (constPar) belong to the mixed logit only; the
    # MNL has no L
    fixed_by_model <- list(mnl = NULL, mixed = estimates$fixed_parameters)

    # What was estimated: the formula as mlogit received it (each interaction
    # as its 0/1 products; showEnv = FALSE leaves out where R stores it); the
    # random coefficients by term, with their distribution, correlation block
    # and Halton primes; and the number of parameters
    print(estimates$formula, showEnv = FALSE)
    print(random %>%
            group_by(term, distribution, block) %>%
            summarise(coefficients = n(),
                      primes = paste(prime, collapse = ", "),
                      .groups = "drop"))
    cat("Parameters: MNL", length(coef(models$mnl)), "| mixed logit",
        length(coef(models$mixed)), "| fixed at 0:",
        length(estimates$fixed_parameters), "\n")

    # 5.2 Cluster-robust covariance matrices -----------------------------------
    # The inverse Hessians (the bread of each sandwich) are kept as well: the
    # robust LR tests of 02_output.R need them
    covariance_matrices <- list()
    inverse_hessians <- list()
    for (model_name in names(models)) {
      V <- vcov_cluster_robust(models[[model_name]], choice_data,
                               cluster = "resp_uid", h = 1e-4)
      inverse_hessians[[model_name]] <- attr(V, "inverse_hessian")
      attr(V, "inverse_hessian") <- NULL
      covariance_matrices[[model_name]] <- V
    }

    # 5.3 Coefficients, standard deviations and correlations -------------------
    coefficients <- tibble()
    for (model_name in names(models)) {
      coefficients <- bind_rows(coefficients, coefficient_table(
        models[[model_name]], covariance_matrices[[model_name]], model_name,
        random, fixed_by_model[[model_name]]
      ))
    }

    # 5.4 AMEs at the reference vignette ---------------------------------------
    # Steps (1) to (5); the checks keep their letters from section 6.5 of the
    # policy brief script.
    # (1) One design matrix per expert profile: the reference vignette and its
    #     variations
    X_by_profile <- ame_design_matrices(estimates$formula_terms,
                                        estimates$column_names,
                                        ATTRIBUTE_LEVELS, AME_PROFILES)

    # (2) Standard normal draws z_r, one row per draw, one column per random
    #     coefficient. The same draws for all vignettes, profiles and both
    #     models. The seed is set right here, so the draws do not depend on
    #     what ran before. For the MNL, L = 0 and every draw gives the same
    #     probability, so one draw is enough (the policy brief used all draws,
    #     with the same result).
    set.seed(20261001)
    standard_draws <- matrix(rnorm(n_draws_ame * length(rpar_names)),
                             nrow = n_draws_ame, ncol = length(rpar_names))
    draws_by_model <- list(mnl = standard_draws[1, , drop = FALSE],
                           mixed = standard_draws)

    # (3) Checks (a), (b), (c) and (e) of the policy brief.
    #     Check (a): the Cholesky factor reproduces mlogit's covariance matrix
    #     of the random coefficients (correlated) or their standard deviations
    #     (uncorrelated). Generalisation: the standard deviations are the
    #     parameters s of mlogit's random coefficients ($rpar); for other
    #     distributions than the normal, mlogit's stdev() is the standard
    #     deviation of the coefficient itself, not s.
    L <- cholesky_factor(coef(models$mixed), rpar_names, fixed_by_model$mixed)
    if (identical(specification$correlation, "none")) {
      sigma_mlogit <- numeric(0)
      for (name in rpar_names) {
        sigma_mlogit[name] <- models$mixed$rpar[[name]]$sigma
      }
      stopifnot(max(abs(abs(diag(L)) - abs(sigma_mlogit))) < 1e-8)
    } else {
      Omega_mlogit <- unclass(cov.mlogit(models$mixed))[rpar_names, rpar_names]
      stopifnot(max(abs(L %*% t(L) - Omega_mlogit)) < 1e-8)
    }

    #     Not in the policy brief: the smallest eigenvalue of the covariance
    #     matrix of each correlation block, printed. Close to 0, the matrix is
    #     nearly singular (a standard deviation near 0, or correlations near
    #     -1 or 1), as can happen in a small group; its standard deviations,
    #     correlations and LR tests are then unreliable.
    if (!identical(specification$correlation, "none")) {
      Omega <- L %*% t(L)
      for (b in unique(random$block)) {
        in_block <- random$name[random$block == b]
        eigenvalues <- eigen(Omega[in_block, in_block], symmetric = TRUE,
                             only.values = TRUE)$values
        cat("Smallest eigenvalue of the covariance matrix of block", b, "(",
            paste(unique(random$term[random$block == b]), collapse = ", "), "):",
            signif(min(eigenvalues), 3), "\n")
      }
    }

    #     Not in the policy brief: for random coefficients that are not
    #     normal, the mean of their simulated draws against mlogit's formula
    #     for their mean (mean() of its $rpar), in standard errors of the
    #     simulated mean. Mostly below 3; a much larger gap means that the
    #     draws do not follow the estimated distribution.
    is_not_normal <- random$distribution != "n"
    if (any(is_not_normal)) {
      result_draws <- ame_at_reference(coef(models$mixed), X_by_profile[[1]],
                                       standard_draws, random, RECOMMENDATIONS,
                                       fixed_by_model$mixed)
      for (k in which(is_not_normal)) {
        draws_k <- coef(models$mixed)[[rpar_names[k]]] +
          result_draws$deviations[, k]
        gap <- abs(mean(draws_k) - mean(models$mixed$rpar[[rpar_names[k]]])) /
          (sd(draws_k) / sqrt(length(draws_k)))
        cat(rpar_names[k], "(", random$distribution[k], "):",
            "gap of the simulated mean", round(gap, 2), "standard errors\n")
      }
    }

    #     Check (b): MNL: at the observed vignettes, ame_at_reference()
    #     reproduces mlogit's probabilities exactly (only $probabilities is used)
    X_observed <- design_matrix(estimates$formula_terms, decisions)
    p_own <- ame_at_reference(coef(models$mnl), X_observed, draws_by_model$mnl,
                              random, RECOMMENDATIONS)$probabilities
    p_mlogit <- fitted(models$mnl, type = "probabilities")[, RECOMMENDATIONS]
    stopifnot(max(abs(p_own - p_mlogit)) < 1e-8)

    #     Check (c): mixed logit: mlogit's fitted probabilities also average
    #     over draws of the random coefficients, but with its own draws (R per
    #     expert), so they agree with ours only up to simulation noise. It
    #     evaluates every observed vignette, so it uses the first
    #     n_draws_check draws.
    n_draws_observed <- min(n_draws_check, n_draws_ame)
    standard_draws_observed <- standard_draws[seq_len(n_draws_observed), ,
                                              drop = FALSE]
    p_own <- ame_at_reference(coef(models$mixed), X_observed,
                              standard_draws_observed, random, RECOMMENDATIONS,
                              fixed_by_model$mixed)$probabilities
    p_mlogit <- fitted(models$mixed, type = "probabilities")[, RECOMMENDATIONS]
    cat("Mean absolute difference to mlogit's probabilities:",
        round(mean(abs(p_own - p_mlogit)), 4), "\n")

    #     Check (e): simulation noise. Other draws should change the AMEs and
    #     the probabilities at the reference vignette by much less than their
    #     standard errors, i.e. by well under 0.5 percentage points; otherwise
    #     increase n_draws_ame. Printed, not stopped.
    set.seed(20261002)
    standard_draws_check <- matrix(rnorm(n_draws_ame * length(rpar_names)),
                                   nrow = n_draws_ame, ncol = length(rpar_names))
    for (profile_name in names(X_by_profile)) {
      X_profile <- X_by_profile[[profile_name]]
      result_1 <- ame_at_reference(coef(models$mixed), X_profile, standard_draws,
                                   random, RECOMMENDATIONS, fixed_by_model$mixed)
      result_2 <- ame_at_reference(coef(models$mixed), X_profile,
                                   standard_draws_check, random, RECOMMENDATIONS,
                                   fixed_by_model$mixed)
      change_ame <- max(abs(result_2$ame - result_1$ame))
      change_reference <- max(abs(result_2$probabilities[1, ] -
                                    result_1$probabilities[1, ]))
      cat(profile_name, ": largest change with other draws, in percentage",
          "points: AME", round(100 * change_ame, 2),
          "| probability at the reference vignette",
          round(100 * change_reference, 2), "\n")
    }
    rm(standard_draws_check)

    # (4) AMEs with Krinsky-Robb standard errors (ame_table()), and check (d):
    #     probabilities sum to one, so the AMEs sum to zero across the outcomes.
    #     Generalisation: the seed is set before each model, not once before
    #     the loop as in the policy brief, so that a model's draws do not
    #     depend on the models before it.
    ames <- tibble()
    for (model_name in names(models)) {
      set.seed(20261003)
      ames_model <- ame_table(model_name, coef(models[[model_name]]),
                              covariance_matrices[[model_name]], X_by_profile,
                              draws_by_model[[model_name]], random,
                              RECOMMENDATIONS, fixed_by_model[[model_name]],
                              n_draws_kr)
      ame_sums <- ames_model %>%
        group_by(profile, vignette) %>%
        summarise(sum_ame = sum(ame), .groups = "drop")
      stopifnot(all(abs(ame_sums$sum_ame) < 1e-12))
      ames <- bind_rows(ames, ames_model)
    }

    # (5) Check: delta-method standard errors (section 7.3 of the policy
    #     brief). If the AMEs are close to linear in theta, they agree with the
    #     Krinsky-Robb standard errors up to the simulation noise of the
    #     Krinsky-Robb draws.
    for (model_name in names(models)) {
      for (profile_name in names(X_by_profile)) {
        se_delta <- ame_delta_method_se(coef(models[[model_name]]),
                                        covariance_matrices[[model_name]],
                                        X_by_profile[[profile_name]],
                                        draws_by_model[[model_name]], random,
                                        RECOMMENDATIONS,
                                        fixed_by_model[[model_name]])
        ratio <- ames %>%
          filter(model == model_name, profile == profile_name) %>%
          left_join(se_delta, by = c("vignette", "outcome")) %>%
          mutate(ratio = se_delta / se) %>%
          pull(ratio)
        cat(model_name, ",", profile_name,
            ": delta-method / Krinsky-Robb standard errors between",
            round(min(ratio, na.rm = TRUE), 2), "and",
            round(max(ratio, na.rm = TRUE), 2), "\n")
      }
    }

    # 5.5 Fit ------------------------------------------------------------------
    fit <- fit_statistics(models, choice_counts, n_experts, n_draws_model)
    print(fit %>%
            select(model, n_parameters, log_likelihood, mcfadden_r2, lr_mnl,
                   lr_mnl_p, aic, bic))

    # 5.6 Save the aggregate results -------------------------------------------
    # Everything 02_output.R needs, and nothing at the level of experts or
    # vignettes: the models themselves stay in model_dir
    results <- list(
      name = spec_name,
      specification = specification,
      arm = SELECTED_FW_ARM,
      reference_outcome = REFERENCE_OUTCOME,
      outcomes = RECOMMENDATIONS,
      attribute_levels = ATTRIBUTE_LEVELS,
      technologies = estimates$technologies,
      formula_terms = estimates$formula_terms,
      columns = estimates$columns,
      column_names = estimates$column_names,
      random_coefficients = random,
      fixed_parameters = estimates$fixed_parameters,
      coefficients = list(mnl = coef(models$mnl), mixed = coef(models$mixed)),
      covariance_matrices = covariance_matrices,
      inverse_hessians = inverse_hessians,
      coefficient_table = coefficients,
      ame_table = ames,
      fit_statistics = fit,
      choice_counts = choice_counts,
      draws = list(n_draws_model = n_draws_model, n_draws_ame = n_draws_ame,
                   n_draws_kr = n_draws_kr),
      date = Sys.Date()
    )
    saveRDS(results, results_file)
    "done"
  }, error = function(e) {
    paste("error:", conditionMessage(e))
  })

  # difftime(): the time between two points in time, here in minutes
  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  run_status$minutes[s] <- round(minutes, 1)
  message("Specification ", spec_name, ": ", run_status$status[s],
          " (", run_status$minutes[s], " minutes)")
}

# 6. Summary of the runs -------------------------------------------------------
print(run_status)
cat("Results written to", results_dir, "\n")
