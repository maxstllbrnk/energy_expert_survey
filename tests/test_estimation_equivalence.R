################################################################################
# test_estimation_equivalence.R — the new functions against the old ones (git tag
#                                 pre-restructure), pasted below: same numbers
# Inputs : simulated decisions, drawn here; for sections 7 and 8 the synthetic
#          cleaned data and the results of R/estimation/district_heating.R (run it
#          first)
# Outputs: in <SECURE_DIR>/output/test_estimation_equivalence/
# Checks : vcov_cluster_robust() gives the same vcov as post_estimation();
#          ame_krinsky_robb() and probabilities_krinsky_robb() the same AMEs and
#          standard errors as compute_ames(), with the same draws;
#          start_from_common() and start_from_nested() the same starting values;
#          add_regressors(), add_alternative_prices() and long_terms() the same
#          regressors as make_regressors() of both scripts; term_changes() of old
#          01_run_term_effects.R the same effects as the changes of the script
################################################################################

source(file.path("R", "setup.R"))
set.seed(20261112)
est_dir <- file.path(OUTPUT_DIR, "test_estimation_equivalence")
# The results of an earlier run go, the models stay: the same seed gives the same data,
# so a second run starts here in seconds. Delete the folder to estimate them again.
unlink(file.path(est_dir, "*", "results.*"))
N_DRAWS_TEST <- 100       # Halton draws per expert in the estimation
N_PREDICTION_TEST <- 1000
N_KR_TEST <- 200
N_WORKERS_TEST <- 2
TOLERANCE <- 1e-10

check <- function(label, value, limit) {
  cat(sprintf("%-68s %10.2e  %s\n", label, value, if (value < limit) "PASS" else "FAIL"))
  if (!(value < limit)) stop("Failed: ", label)
}

# 1. The old functions, as they were -------------------------------------------
#' Design matrix: one row per row of `data`, one column per name in
#' `columns`. "(Intercept)" is a column of ones; every other column is the
#' variable of that name in `data`.
#'
#' @param data    one row per decision or vignette
#' @param columns the constants and regressors of a model
#' @return matrix
design_matrix <- function(data, columns) {
  X <- matrix(NA_real_, nrow = nrow(data), ncol = length(columns),
              dimnames = list(NULL, columns))
  for (column in columns) {
    if (column == "(Intercept)") {
      X[, column] <- 1
    } else if (column %in% names(data)) {
      X[, column] <- data[[column]]
    } else {
      stop("Not in the data: ", column)
    }
  }
  X
}

#' The regressors of the models, from the vignette attributes (att_*) and the
#' Berufsgruppe
#'   <dummy>                       one per row of attribute_dummies
#'   is_shk, is_schornsteinfeger,  one per Berufsgruppe
#'   is_energieberater
#'   is_<group>_x_<dummy>          a Berufsgruppe's dummy times an attribute
#'                                 dummy, for all three; the models use those
#'                                 of the groups besides the reference group
#'                                 (GROUP_X_ATTRIBUTES of 01)
#'   <dummy>_x_<dummy>             two attribute dummies multiplied, the
#'                                 interactions of the attributes
#'                                 (ATTRIBUTE_INTERACTIONS of 01)
#'
#' @param data              one row per decision or vignette, with the att_*
#'                          columns and berufsgruppe
#' @param attribute_dummies ATTRIBUTE_DUMMIES of 01: dummy, attribute, level
#' @return data with the regressors
add_regressors <- function(data, attribute_dummies) {
  for (k in seq_len(nrow(attribute_dummies))) {
    is_level <- data[[attribute_dummies$attribute[k]]] ==
      attribute_dummies$level[k]
    data[[attribute_dummies$dummy[k]]] <- if_else(is_level, 1, 0)
  }
  # Interactions of the attributes. No vignette has a house built in 2000 with
  # a heat demand of 200 kWh/m², so demand_200 x built_2000 would be 0 in every
  # vignette, without a coefficient.
  data$demand_70_x_built_2000 <- data$demand_70 * data$built_2000
  data$demand_70_x_floor_heating <- data$demand_70 * data$floor_heating
  data$demand_200_x_floor_heating <- data$demand_200 * data$floor_heating
  data$floor_heating_x_built_2000 <- data$floor_heating * data$built_2000
  data <- data %>%
    mutate(
      is_shk = if_else(berufsgruppe == "SHK-Handwerk", 1, 0),
      is_schornsteinfeger = if_else(berufsgruppe == "Schornsteinfeger", 1, 0),
      is_energieberater = if_else(berufsgruppe == "Energieberater", 1, 0)
    )
  for (group in c("is_shk", "is_schornsteinfeger", "is_energieberater")) {
    for (dummy in attribute_dummies$dummy) {
      data[[paste0(group, "_x_", dummy)]] <- data[[group]] * data[[dummy]]
    }
  }
  data
}

#' The prices and expected rises of the heat pump and the hybrid heating, from
#' those of the fuels
#'
#' The heat pump's price per kWh of heat is the electricity price divided by
#' the COP of the vignette's heat distribution. The hybrid's is the weighted
#' average of the gas price and the heat pump's; it counts as not reported if
#' the gas or the electricity price is not reported. An expert who reported
#' the mean prices has 0 in every vignette. In logs the COP cancels,
#' log(p / COP) - log(mean / COP) = log(p) - log(mean). The hybrid's log price
#' is the weighted average of those of gas and the heat pump: the log of a
#' weighted geometric mean, close to the log of the weighted average if gas
#' and the heat pump's heat cost about the same per kWh. The hybrid's expected
#' rise is the weighted average of those of gas and electricity, blank if
#' either is blank (Max, 2026-10-27).
#'
#' @param data             one row per decision or vignette, with
#'                         floor_heating (add_regressors()) and the expert's
#'                         price_<fuel>_c, _lc, _na and pexp_<fuel>_c, _na
#' @param cop              COP of 01: c(radiators = , floor_heating = )
#' @param hybrid_gas_share HYBRID_GAS_SHARE of 01
#' @return data with price_hpelec_heat_c, price_hybrid_na, price_hybrid_c,
#'   price_hpelec_heat_lc, price_hybrid_lc, pexp_hybrid_na, pexp_hybrid_c
add_alternative_prices <- function(data, cop, hybrid_gas_share) {
  data %>%
    mutate(
      cop = if_else(floor_heating == 1, cop[["floor_heating"]], cop[["radiators"]]),
      price_hpelec_heat_c = price_hpelec_c / cop,
      price_hybrid_na = if_else(price_gas_na == 1 | price_hpelec_na == 1, 1, 0),
      price_hybrid_c = if_else(price_hybrid_na == 1, 0,
                               hybrid_gas_share * price_gas_c +
                                 (1 - hybrid_gas_share) * price_hpelec_heat_c),
      price_hpelec_heat_lc = price_hpelec_lc,
      price_hybrid_lc = if_else(price_hybrid_na == 1, 0,
                                hybrid_gas_share * price_gas_lc +
                                  (1 - hybrid_gas_share) * price_hpelec_heat_lc),
      pexp_hybrid_na = if_else(pexp_gas_na == 1 | pexp_hpelec_na == 1, 1, 0),
      pexp_hybrid_c = if_else(pexp_hybrid_na == 1, 0,
                              hybrid_gas_share * pexp_gas_c +
                                (1 - hybrid_gas_share) * pexp_hpelec_c)
    )
}

#' The terms that differ between the outcomes of a decision, with one
#' coefficient for all technologies (the `prices` of a model), in data with
#' one row per decision x outcome
#'
#' In the rows of each technology: its price per kWh (`price`), its log price
#' (`log_price`), its dummy for a price not reported (<name>_price_na), its
#' expected rise (`expects_rise`) and its dummy for a blank in Q17
#' (<name>_rise_na); 0 in the rows of the other outcomes. The price times the
#' heat demand of the vignette (price_x_demand_70, price_x_demand_200): with
#' both in a model, the price coefficient is the effect at 120 kWh/m², and
#' theirs are the differences at 70 and 200. own_heating_match: in the row of
#' the technology that heats the expert's home 1, in the others 0, minus the
#' share of experts with that heating at home, so that 0 is an expert with
#' average characteristics. An expert with "Sonstiges" or no answer has no
#' match; "Keine Empfehlung" is no heating and has 0. A heat pump at home is a
#' regressor of the decisions, with one coefficient per technology
#' (heat_pump_at_home, 01_run_specifications.R, section 2.1), not a term here.
#'
#' @param choice_long           one row per decision x outcome: alternative,
#'                              the columns named in the two tables below,
#'                              demand_70, demand_200 and own_heating
#' @param price_variables       PRICE_VARIABLES of 01: technology, name, and
#'                              the columns price, log_price, not_reported
#' @param expectation_variables EXPECTATION_VARIABLES of 01: technology, name,
#'                              and the columns rise, not_answered
#' @param own_heating_share     share of each heating at home (01)
#' @param own_heating_answers   OWN_HEATING_ANSWERS of 01
#' @param outcomes              all outcomes
#' @return choice_long with the terms
long_terms <- function(choice_long, price_variables, expectation_variables,
                       own_heating_share, own_heating_answers, outcomes) {
  choice_long$price <- 0
  choice_long$log_price <- 0
  for (k in seq_len(nrow(price_variables))) {
    is_technology <- choice_long$alternative == price_variables$technology[k]
    price_k <- choice_long[[price_variables$price[k]]]
    log_price_k <- choice_long[[price_variables$log_price[k]]]
    not_reported_k <- choice_long[[price_variables$not_reported[k]]]
    choice_long$price[is_technology] <- price_k[is_technology]
    choice_long$log_price[is_technology] <- log_price_k[is_technology]
    dummy_name <- paste0(price_variables$name[k], "_price_na")
    choice_long[[dummy_name]] <- if_else(is_technology, not_reported_k, 0)
  }
  choice_long <- choice_long %>%
    mutate(price_x_demand_70 = price * demand_70,
           price_x_demand_200 = price * demand_200)

  choice_long$expects_rise <- 0
  for (k in seq_len(nrow(expectation_variables))) {
    is_technology <- choice_long$alternative == expectation_variables$technology[k]
    rise_k <- choice_long[[expectation_variables$rise[k]]]
    not_answered_k <- choice_long[[expectation_variables$not_answered[k]]]
    choice_long$expects_rise[is_technology] <- rise_k[is_technology]
    dummy_name <- paste0(expectation_variables$name[k], "_rise_na")
    choice_long[[dummy_name]] <- if_else(is_technology, not_answered_k, 0)
  }

  choice_long$own_heating_match <- 0
  for (technology in intersect(own_heating_answers, outcomes)) {
    is_technology <- choice_long$alternative == technology
    is_own <- if_else(choice_long$own_heating == technology, 1, 0)
    choice_long$own_heating_match[is_technology] <-
      is_own[is_technology] - own_heating_share[[technology]]
  }
  choice_long
}

#' Design matrices of some vignettes, one per Berufsgruppe (profile): the
#' vignettes for an expert of that group. If the model has no regressor that
#' depends on the Berufsgruppe, all are the same, and one profile, "All
#' experts", is enough.
#'
#' @param vignettes         one row per vignette: vignette (its name), the
#'                          att_* columns and every other regressor of the
#'                          model that add_regressors() does not make
#' @param groups            the Berufsgruppen (BERUFSGRUPPEN)
#' @param columns           the constants and regressors of the model
#' @param attribute_dummies as in add_regressors()
#' @return named list of matrices, one row per vignette
design_by_profile <- function(vignettes, groups, columns, attribute_dummies) {
  X_by_profile <- list()
  for (profile_name in groups) {
    profile_vignettes <- vignettes %>%
      mutate(berufsgruppe = profile_name) %>%
      add_regressors(attribute_dummies)
    X <- design_matrix(profile_vignettes, columns)
    rownames(X) <- vignettes$vignette
    X_by_profile[[profile_name]] <- X
  }
  is_same_for_all <- TRUE
  for (profile_name in groups) {
    if (!identical(X_by_profile[[profile_name]], X_by_profile[[1]])) {
      is_same_for_all <- FALSE
    }
  }
  if (is_same_for_all) {
    X_by_profile <- list("All experts" = X_by_profile[[1]])
  }
  X_by_profile
}

#' Starting values of a model with constants by Berufsgruppe, from the same
#' model with one set of constants for all experts and the Berufsgruppe
#' shifting their means (e.g. model 6pe from model 4pe)
#'
#' Each group gets the common model's constants plus its shift (0 for the
#' reference group, which has no shift), and the common model's Cholesky
#' factor as its block of L. The elements of L between two groups are 0, and
#' every other parameter (attributes, interactions, prices) is the common
#' model's. At these values the two models predict the same for every expert,
#' so the estimation starts at the common model's log-likelihood and can only
#' improve on it. mlogit's default starts every element of L at 0.1, also
#' those off the diagonal: in every block, constants with small variances and
#' correlations close to one.
#'
#' @param parameter_names all parameters of the model, in mlogit's order
#' @param theta_common    coef() of the common model
#' @param groups          the group dummies, e.g. "is_shk"
#' @param technologies    the outcomes other than the reference outcome
#' @param fixed_names     the elements of L fixed at 0 (constPar)
#' @return named vector, in the order of parameter_names
start_from_common <- function(parameter_names, theta_common, groups,
                              technologies, fixed_names) {
  start <- rep(NA_real_, length(parameter_names))
  names(start) <- parameter_names
  # startsWith(): TRUE for the names that begin with "chol.", the elements of L
  cholesky_common <- names(theta_common)[startsWith(names(theta_common), "chol.")]

  # The common model must have the Berufsgruppe among its regressors: a shift
  # for every group but one, the reference group
  groups_with_shift <- character(0)
  for (group in groups) {
    if (paste0(group, ":", technologies[1]) %in% names(theta_common)) {
      groups_with_shift <- c(groups_with_shift, group)
    }
  }
  if (length(groups_with_shift) != length(groups) - 1) {
    stop("The common model must have the group dummies of all groups but ",
         "the reference group among its regressors")
  }

  for (group in groups) {
    # (a) The group's constants. In the common model, "<group>:<technology>"
    #     is the group's shift of the constant; the reference group has none.
    for (technology in technologies) {
      constant_name <- paste0(group, ":", technology)
      shift <- 0
      if (constant_name %in% names(theta_common)) {
        shift <- theta_common[[constant_name]]
      }
      start[[constant_name]] <- theta_common[[paste0("(Intercept):", technology)]] +
        shift
    }
    # (b) The group's block of L: "chol.(Intercept):Gasheizung:(Intercept):
    #     Ölheizung" becomes "chol.is_shk:Gasheizung:is_shk:Ölheizung"
    for (name in cholesky_common) {
      group_name <- gsub("(Intercept)", group, name, fixed = TRUE)
      if (!(group_name %in% parameter_names)) {
        stop("Not a parameter of the model: ", group_name)
      }
      start[[group_name]] <- theta_common[[name]]
    }
  }

  # (c) The elements of L between two groups
  start[fixed_names] <- 0

  # (d) Every other parameter, as in the common model
  for (name in parameter_names[is.na(start)]) {
    if (!(name %in% names(theta_common))) {
      stop("No starting value for ", name, ": the common model does not ",
           "have this parameter")
    }
    start[[name]] <- theta_common[[name]]
  }
  if (length(start) != length(parameter_names)) {
    stop("Starting values for parameters the model does not have")
  }
  start
}

#' Starting values of a model from a model that it nests, with the same
#' constants (e.g. model 4pd from model 4p): every parameter of the nested
#' model at its estimate, every parameter the model adds at 0
#'
#' At these values the two models are the same for every expert (same draws),
#' so the estimation starts at the nested model's log-likelihood and can only
#' improve on it. Without them, (4pd) and (3pg) ended below the model they
#' nest, although mlogit reported convergence (runs of 22 and 28 October 2026).
#'
#' @param parameter_names all parameters of the model, in mlogit's order
#' @param theta_nested    coef() of the nested model
#' @return named vector, in the order of parameter_names
start_from_nested <- function(parameter_names, theta_nested) {
  not_in_model <- setdiff(names(theta_nested), parameter_names)
  if (length(not_in_model) > 0) {
    stop("The model does not nest the model it starts from, which also has: ",
         paste(not_in_model, collapse = ", "))
  }
  start <- rep(0, length(parameter_names))
  names(start) <- parameter_names
  start[names(theta_nested)] <- theta_nested
  start
}

#' Cluster-robust (sandwich) covariance matrix of the estimates, clustered by
#' expert (section 5 of the policy brief script):
#'
#'   V = G / (G - 1) * H^{-1} B H^{-1}
#'
#' H     Hessian of -log L at the estimates ("bread"). mlogit provides none
#'       for the mixed logit, so H is computed by central differences of the
#'       gradient. The gradient is re-evaluated by re-running the model's call
#'       with iterlim = 0, which reuses the estimation draws (checked below).
#' B     sum over experts of the outer product of the expert-level scores
#'       ("meat"). model$gradient has one row per choice situation; the rows
#'       of an expert add up to their score.
#' G/(G-1) small-sample factor with G experts (as in Stata and sandwich::vcovCL).
#'
#' Parameters fixed by constPar are not estimated: they are not in coef(), but
#' mlogit() needs a starting value for them.
#'
#' @param model       fitted mlogit model
#' @param choice_data the data the model was estimated on
#' @param cluster     name of the cluster variable in idx(model)
#' @param h           step size of the numerical derivatives
#' @return list: vcov (rows and columns named as coef(model)) and
#'   inverse_hessian (the bread H^{-1})
vcov_cluster_robust <- function(model, choice_data, cluster = "resp_uid",
                                h = 1e-4) {
  theta_hat <- coef(model)                         # without the fixed parameters
  K <- length(theta_hat)

  # Meat: one score vector per expert
  index <- idx(model)
  cluster_id <- index[[cluster]][!duplicated(index$chid)]  # one per choice situation
  # rowsum(): the sum of the rows of each expert, one row per expert
  expert_gradients <- rowsum(model$gradient[, names(theta_hat), drop = FALSE],
                             cluster_id)                    # G x K
  meat <- t(expert_gradients) %*% expert_gradients          # sum_g s_g s_g'
  G <- nrow(expert_gradients)

  # Gradient of -log L at any parameter vector theta: re-estimate the model
  # starting at theta with zero iterations, so that mlogit only evaluates the
  # log-likelihood and its gradient there. -log L is a sum over choice
  # situations, so its gradient is the sum of the rows of model$gradient.
  total_gradient <- function(theta) {
    # mlogit needs a starting value for every parameter, also for those fixed
    # by constPar: model$coefficients has all of them, in mlogit's order
    start <- setNames(as.numeric(model$coefficients), names(model$coefficients))
    start[names(theta)] <- theta
    # update() evaluates the model's call here. The only name in the call is
    # choice_data, which R finds as the argument of vcov_cluster_robust()
    model_at_theta <- update(model, start = start, iterlim = 0, print.level = 0)
    colSums(model_at_theta$gradient[, names(theta), drop = FALSE])
  }

  # Re-evaluating at the estimates must reproduce the stored gradient,
  # otherwise data, settings or draws differ from the estimation
  gradient_at_estimates <- colSums(model$gradient[, names(theta_hat), drop = FALSE])
  if (max(abs(total_gradient(theta_hat) - gradient_at_estimates)) > 1e-6) {
    stop("Re-evaluating the model does not reproduce its gradient: ",
         "check that choice_data is the data the model was estimated on")
  }

  # Bread: inverse of the numerical Hessian
  H <- matrix(NA_real_, K, K)
  for (k in seq_len(K)) {
    # replace(): a vector of K zeros with h in position k
    step <- replace(numeric(K), k, h)
    H[, k] <- (total_gradient(theta_hat + step) -
                 total_gradient(theta_hat - step)) / (2 * h)
    if (k %% 10 == 0) message("Hessian: column ", k, " of ", K)
  }
  H <- (H + t(H)) / 2
  if (any(eigen(H, symmetric = TRUE, only.values = TRUE)$values <= 0)) {
    stop("The numerical Hessian is not positive definite")
  }
  bread <- solve(H)

  V <- G / (G - 1) * bread %*% meat %*% bread
  dimnames(V) <- list(names(theta_hat), names(theta_hat))
  dimnames(bread) <- dimnames(V)
  list(vcov = V, inverse_hessian = bread)
}

#' Cholesky factor L of the covariance matrix of the random constants, so
#' that the covariance matrix is L %*% t(L). Rows and columns in mlogit's
#' order of the random coefficients. mlogit names L[i, j] (i >= j)
#' "chol.<coefficient j>:<coefficient i>"; the elements fixed at 0 by constPar
#' are not in theta.
#'
#' @param theta               parameter vector, named as coef() of the model
#' @param random_coefficients from estimate_mixed_logit()
#' @param fixed_parameters    the elements of L fixed at 0 (NULL if none)
#' @return K x K matrix
cholesky_factor <- function(theta, random_coefficients, fixed_parameters = NULL) {
  rpar_names <- random_coefficients$name
  K <- length(rpar_names)
  L <- matrix(0, nrow = K, ncol = K, dimnames = list(rpar_names, rpar_names))
  for (i in seq_len(K)) {
    for (j in seq_len(i)) {
      name <- paste0("chol.", rpar_names[j], ":", rpar_names[i])
      if (name %in% names(theta)) {
        L[i, j] <- theta[[name]]
      } else if (!(name %in% names(fixed_parameters))) {
        stop("Element of the Cholesky factor missing in theta: ", name)
      }
    }
  }
  L
}

#' Probabilities of the recommendations at each vignette (row) of X, for a
#' randomly drawn expert: the logit probabilities averaged over the
#' distribution of the random constants (section 6.3 of the policy brief
#' script)
#'
#'   P_j(x) = 1/R * sum_r exp(u_j(x, r)) / sum_k exp(u_k(x, r))
#'   u_j(x, r) = x' beta_j + g' p_j + (L z_r)_j,   u_0(x, r) = g' p_0
#'
#' (L z_r)_j is draw r of the deviation of the expert's constant of technology
#' j from its mean. With constants by group, X holds the profile's group
#' dummies, so only the deviations of that group's constants enter. g' p_j are
#' the terms with one coefficient for all technologies (the `prices` of a
#' model), also in the reference outcome's utility. Without generic_values
#' they are 0: the expert reported the mean of each, e.g. the mean price of
#' every fuel, as at the vignettes of the AMEs.
#'
#' @param theta               parameter vector, named as coef() of the model
#' @param X                   design matrix from design_matrix(), one row per
#'                            vignette, with the model's constants and
#'                            regressors
#' @param standard_draws      standard normal draws, one row per draw, one
#'                            column per random constant
#' @param random_coefficients from estimate_mixed_logit()
#' @param outcomes            all outcomes, the reference outcome first
#' @param fixed_parameters    the elements of L fixed at 0 (NULL if none)
#' @param generic_values      NULL, or a named list with one matrix per term
#'                            of the `prices` (rows as X, columns the
#'                            outcomes): its value in each utility
#' @return matrix, one row per vignette, one column per outcome
predict_probabilities <- function(theta, X, standard_draws, random_coefficients,
                                  outcomes, fixed_parameters = NULL,
                                  generic_values = NULL) {
  technologies <- outcomes[-1]
  n_draws <- nrow(standard_draws)

  # (a) Coefficients: one row per column of X, one column per technology.
  #     mlogit names them "<column>:<technology>"; for a random constant,
  #     theta holds its mean.
  beta <- matrix(NA_real_, nrow = ncol(X), ncol = length(technologies),
                 dimnames = list(colnames(X), technologies))
  for (technology in technologies) {
    beta[, technology] <- theta[paste0(colnames(X), ":", technology)]
  }
  if (anyNA(beta)) stop("Coefficients of the columns of X missing in theta")

  # (b) What the random constants add to each technology's utility, one row
  #     per draw. Draw r of the deviation of constant k is row k of L times
  #     z_r; it enters times its column of X (1 for "(Intercept)", the group
  #     dummy for a group's constant), which is the same in every vignette.
  L <- cholesky_factor(theta, random_coefficients, fixed_parameters)
  random_utility <- matrix(0, nrow = n_draws, ncol = length(technologies),
                           dimnames = list(NULL, technologies))
  for (k in seq_len(nrow(random_coefficients))) {
    column_values <- X[, random_coefficients$column[k]]
    if (any(column_values != column_values[1])) {
      stop("The column ", random_coefficients$column[k], " differs between ",
           "the vignettes of one profile")
    }
    if (column_values[1] != 0) {
      technology <- random_coefficients$technology[k]
      deviation <- as.vector(standard_draws %*% L[k, ])
      random_utility[, technology] <- random_utility[, technology] +
        deviation * column_values[1]
    }
  }

  # (c) The terms with one coefficient for all technologies: their part of
  #     each utility, one row per vignette, one column per outcome
  generic_utility <- matrix(0, nrow = nrow(X), ncol = length(outcomes),
                            dimnames = list(rownames(X), outcomes))
  for (term in names(generic_values)) {
    if (!(term %in% names(theta))) stop("Coefficient missing in theta: ", term)
    if (!identical(dim(generic_values[[term]]), dim(generic_utility))) {
      stop("generic_values of ", term, " needs one row per vignette and one ",
           "column per outcome")
    }
    generic_utility <- generic_utility + theta[[term]] * generic_values[[term]]
  }

  # (d) Probabilities of each vignette v, averaged over the draws r. The
  #     utility of technology j is a_vj + c_rj, its mean part a_vj = x_v'
  #     beta_j + g' p_vj and its random part c_rj = (L z_r)_j; the reference
  #     outcome's is a_v0. Every utility is reduced by the largest mean part
  #     of its vignette, A_v, and the largest random part of its draw, B_r
  #     (at least 0), which cancels in the ratio and keeps every exp() at or
  #     below 1, also at an extreme Krinsky-Robb draw. With
  #     w_vj = exp(a_vj - A_v), e_rj = exp(c_rj - B_r), e_r0 = exp(-B_r),
  #       P_vj = w_vj * 1/R * sum_r e_rj / D_rv,
  #       P_v0 = w_v0 * 1/R * sum_r e_r0 / D_rv,
  #       D_rv = w_v0 e_r0 + sum_k w_vk e_rk,
  #     so every vignette's probabilities come from matrix products, and
  #     exp() runs once per draw instead of once per draw and vignette.
  mean_utility <- X %*% beta + generic_utility[, technologies, drop = FALSE]
  reference_utility <- generic_utility[, outcomes[1]]
  largest_mean <- reference_utility
  largest_random <- rep(0, n_draws)
  for (k in seq_along(technologies)) {
    largest_mean <- pmax(largest_mean, mean_utility[, k])
    largest_random <- pmax(largest_random, random_utility[, k])
  }
  # vignettes x technologies: each row minus its vignette's largest mean part;
  # draws x technologies: each row minus its draw's largest random part
  weights <- exp(mean_utility - largest_mean)
  weights_reference <- exp(reference_utility - largest_mean)
  exp_random <- exp(random_utility - largest_random)
  exp_reference <- exp(-largest_random)
  # draws x vignettes: exp_random %*% t(weights) sums w_vk e_rk over k; the
  # product of the column e_r0 and the row w_v0 adds w_v0 e_r0
  denominators <- exp_random %*% t(weights) +
    matrix(exp_reference, ncol = 1) %*% matrix(weights_reference, nrow = 1)
  inverse <- 1 / denominators
  probabilities <- matrix(NA_real_, nrow = nrow(X), ncol = length(outcomes),
                          dimnames = list(rownames(X), outcomes))
  probabilities[, outcomes[1]] <- weights_reference *
    as.vector(t(inverse) %*% exp_reference) / n_draws
  # t(exp_random) %*% inverse: technologies x vignettes, the sums over the
  # draws of e_rj / D_rv
  probabilities[, technologies] <- t(t(exp_random) %*% inverse) / n_draws *
    weights
  # A denominator of 0 (every term below what exp() can represent) would give
  # Inf or NaN; checked at every evaluation, the Krinsky-Robb draws included
  if (any(!is.finite(probabilities))) {
    stop("A probability cannot be computed: the utilities of a vignette ",
         "differ by more than exp() can represent (at a Krinsky-Robb draw, a ",
         "coefficient can be extreme)")
  }
  probabilities
}

#' Probabilities at the vignettes of X, at theta_hat and at every
#' Krinsky-Robb draw of theta, with the same standard draws throughout, so
#' that the spread over the draws reflects only the uncertainty in theta
#'
#' @param theta_hat   coef() of the model
#' @param theta_draws draws of theta, one row per draw
#' @param X, standard_draws, random_coefficients, outcomes, fixed_parameters,
#'   generic_values as in predict_probabilities()
#' @param label       name of the profile in the progress messages
#' @return list: estimate (vignettes x outcomes, at theta_hat), draws
#'   (vignettes x outcomes x draws)
probabilities_krinsky_robb <- function(theta_hat, theta_draws, X, standard_draws,
                                       random_coefficients, outcomes,
                                       fixed_parameters = NULL,
                                       generic_values = NULL, label = "") {
  n_draws_kr <- nrow(theta_draws)
  estimate <- predict_probabilities(theta_hat, X, standard_draws,
                                    random_coefficients, outcomes,
                                    fixed_parameters, generic_values)
  # The probabilities of each vignette sum to one, so the AMEs of each
  # variation sum to zero across the outcomes
  stopifnot(all(abs(rowSums(estimate) - 1) < 1e-12))
  draws <- array(NA_real_, dim = c(nrow(X), length(outcomes), n_draws_kr),
                 dimnames = list(rownames(X), outcomes, NULL))
  for (s in seq_len(n_draws_kr)) {
    draws[, , s] <- predict_probabilities(theta_draws[s, ], X, standard_draws,
                                          random_coefficients, outcomes,
                                          fixed_parameters, generic_values)
    if (s %% 100 == 0) {
      message(label, ": Krinsky-Robb draw ", s, " of ", n_draws_kr)
    }
  }
  list(estimate = estimate, draws = draws)
}

#' AMEs at the reference vignette, with Krinsky-Robb standard errors
#' (sections 6 and 7.2 of the policy brief script)
#'
#'   AME_j = P_j(reference vignette with one attribute changed)
#'           - P_j(reference vignette)
#'
#' Krinsky-Robb passes the uncertainty in theta_hat through the AMEs: compute
#' them at every draw theta_s of N(theta_hat, V), with the same standard
#' draws as the point estimate, so that their spread reflects only the
#' uncertainty in theta. The standard deviation over the draws is the
#' standard error; their 2.5% and 97.5% quantiles are a percentile interval.
#' The point estimate stays the AME at theta_hat.
#'
#' ci_low, ci_high: estimate +- 1.96 se, consistent with z and p.
#' ci_low_kr, ci_high_kr: the percentile interval. If the two differ markedly,
#' the distribution of the AME is skewed and the percentile interval is the
#' better one to report.
#'
#' @param theta_hat    coef() of the model
#' @param theta_draws  draws of theta, one row per draw
#' @param X_by_profile design matrices of the AME vignettes, one per profile;
#'                     the first row is the reference vignette
#' @param standard_draws, random_coefficients, outcomes, fixed_parameters as in
#'   predict_probabilities()
#' @return list:
#'   table              one row per profile x variation x outcome: profile,
#'                      vignette, outcome, p_reference (probability at the
#'                      reference vignette), se_p_reference, ame, se, z, p,
#'                      ci_low, ci_high, ci_low_kr, ci_high_kr
#'   probability_draws  per profile, the probabilities at every draw:
#'                      vignettes x outcomes x draws
ame_krinsky_robb <- function(theta_hat, theta_draws, X_by_profile,
                             standard_draws, random_coefficients, outcomes,
                             fixed_parameters = NULL) {
  table <- tibble()
  probability_draws <- list()
  for (profile_name in names(X_by_profile)) {
    X <- X_by_profile[[profile_name]]

    # (a) Probabilities at theta_hat and at every draw
    probabilities <- probabilities_krinsky_robb(theta_hat, theta_draws, X,
                                                standard_draws,
                                                random_coefficients, outcomes,
                                                fixed_parameters,
                                                label = profile_name)
    estimate <- probabilities$estimate
    draws <- probabilities$draws
    probability_draws[[profile_name]] <- draws

    # (b) AMEs: each variation (rows 2, 3, ...) minus the reference vignette
    #     (row 1), at theta_hat and at every draw
    for (v in 2:nrow(X)) {
      for (outcome in outcomes) {
        ame_draws <- draws[v, outcome, ] - draws[1, outcome, ]
        table <- bind_rows(table, tibble(
          profile = profile_name,
          vignette = rownames(X)[v],
          outcome = outcome,
          p_reference = estimate[1, outcome],
          se_p_reference = sd(draws[1, outcome, ]),
          ame = estimate[v, outcome] - estimate[1, outcome],
          se = sd(ame_draws),
          ci_low_kr = as.numeric(quantile(ame_draws, 0.025)),
          ci_high_kr = as.numeric(quantile(ame_draws, 0.975))
        ))
      }
    }
  }

  # (c) z, p and the intervals estimate +- 1.96 se
  z_critical <- qnorm(0.975)
  table <- table %>%
    mutate(z = ame / se,
           p = 2 * pnorm(-abs(z)),
           ci_low = ame - z_critical * se,
           ci_high = ame + z_critical * se) %>%
    select(profile, vignette, outcome, p_reference, se_p_reference, ame, se,
           z, p, ci_low, ci_high, ci_low_kr, ci_high_kr)
  list(table = table, probability_draws = probability_draws)
}

#' The reference state: the reference vignette and an expert with average
#' characteristics, as at the AMEs of 01_run_specifications.R. One row with
#' the attributes (att_*), the degree days (hdd_c), the heat pump at home
#' (heat_pump_at_home) and the expert's prices, expected rises and heating at
#' home, coded as in 01: centred, 0 at the mean; "not reported" and "blank" 0.
#' own_match_<technology> is the value of own_heating_match in the rows of
#' that technology (0: average).
#'
#' @param reference_levels the attributes of the reference vignette
#' @param fuels            the fuels of the prices and expected rises
#' @param technologies     the technologies of the heating at home
#' @return tibble, one row
reference_state <- function(reference_levels, fuels, technologies) {
  state <- as_tibble(reference_levels)
  state$hdd_c <- 0
  state$heat_pump_at_home <- 0
  for (fuel in fuels) {
    state[[paste0("price_", fuel, "_c")]] <- 0
    state[[paste0("price_", fuel, "_lc")]] <- 0
    state[[paste0("price_", fuel, "_na")]] <- 0
    state[[paste0("pexp_", fuel, "_c")]] <- 0
    state[[paste0("pexp_", fuel, "_na")]] <- 0
  }
  for (technology in technologies) {
    state[[paste0("own_match_", technology)]] <- 0
  }
  state
}

#' The neutral base state (TERM_EFFECTS_BASE "neutral"): the reference state,
#' but in a model with the expected rises or the heating at home, an expert
#' who expects no price to rise and has no heat pump at home (with the match
#' of every heating at home: whose home none of the technologies heats), so
#' that a change of these terms switches on one answer (a technology of
#' OWN_HEATING_BEFORE starts from the heating it replaces). Coded as in 01: 0
#' minus the share, as an answer "no". The prices and degree days stay at
#' their means. In a model without these terms, the reference state itself.
#'
#' @param reference        reference_state()
#' @param prices           the model's terms with one coefficient for all
#'                         technologies (specification$prices)
#' @param regressors       the model's regressors (specification$regressors)
#' @param expert_reference of data_reference.rds: the shares of the answers
#' @param fuels            the fuels of the expected rises
#' @param technologies     the technologies of the heating at home
#' @return tibble, one row
neutral_state <- function(reference, prices, regressors, expert_reference,
                          fuels, technologies) {
  state <- reference
  if ("expects_rise" %in% prices) {
    for (fuel in fuels) {
      is_fuel <- expert_reference$pexp$fuel == fuel
      state[[paste0("pexp_", fuel, "_c")]] <-
        0 - expert_reference$pexp$share_rise[is_fuel]
    }
  }
  if ("own_heating_match" %in% prices) {
    for (technology in technologies) {
      state[[paste0("own_match_", technology)]] <-
        0 - expert_reference$own_heating_share[[technology]]
    }
  }
  if ("heat_pump_at_home" %in% regressors) {
    state$heat_pump_at_home <-
      0 - expert_reference$own_heating_share[["Wärmepumpe"]]
  }
  state
}

#' One change: what differs from the base state before and after it
#' (named lists of columns of the state; an empty list is the base state
#' itself), and what it is, for the labels of 02_output.R. condition: of an
#' interaction, the attribute dummy where the attribute changes; of the
#' heating at home, the heating it replaces (OWN_HEATING_BEFORE), NA if none
new_change <- function(id, type, before, after, fuel = NA_character_,
                       technology = NA_character_, attribute = NA_character_,
                       condition = NA_character_, demand = NA_character_,
                       value_before = NA_real_, value_after = NA_real_) {
  list(id = id, type = type, before = before, after = after, fuel = fuel,
       technology = technology, attribute = attribute, condition = condition,
       demand = demand, value_before = value_before, value_after = value_after)
}

#' The changes that apply to a model: those of the terms it has
#'
#' @param specification     the model's specification (its saved results)
#' @param attribute_dummies ATTRIBUTE_DUMMIES of 01
#' @param outcomes          all outcomes
#' @param data_reference    data_reference.rds of 01: the means, shares and
#'                          quartiles, and which technology uses which fuel
#' @param own_heating_before OWN_HEATING_BEFORE: the heating at home that a
#'                          technology's heating at home replaces
#' @return list of changes (new_change())
term_changes <- function(specification, attribute_dummies, outcomes,
                         data_reference, own_heating_before) {
  prices <- specification$prices
  regressors <- specification$regressors
  price_reference <- data_reference$price_reference
  price_quartiles <- data_reference$price_quartiles
  expert_reference <- data_reference$expert_reference
  # The fuels of a technology of this arm: in the arm without district
  # heating, its fuel moves no utility
  price_fuels <- intersect(price_reference$fuel,
                           data_reference$price_variables$name)
  expectation_fuels <- intersect(expert_reference$pexp$fuel,
                                 data_reference$expectation_variables$name)
  changes <- list()
  # The dummies of the heat demand other than the reference demand, and the
  # price times each, named as in long_terms()
  demand_dummies <- attribute_dummies %>%
    filter(attribute == "att_heat_demand")
  demand_terms <- paste0("price_x_", demand_dummies$dummy)

  # (a) The price of each fuel, from its 25th to its 75th percentile, in
  #     levels and logs (the model uses one of them), relative to the mean
  if (any(c("price", "log_price") %in% prices)) {
    for (fuel in price_fuels) {
      mean_price <- price_reference$mean[price_reference$fuel == fuel]
      q25 <- price_quartiles$q25[price_quartiles$fuel == fuel]
      q75 <- price_quartiles$q75[price_quartiles$fuel == fuel]
      before <- list()
      before[[paste0("price_", fuel, "_c")]] <- q25 - mean_price
      before[[paste0("price_", fuel, "_lc")]] <- log(q25) - log(mean_price)
      after <- list()
      after[[paste0("price_", fuel, "_c")]] <- q75 - mean_price
      after[[paste0("price_", fuel, "_lc")]] <- log(q75) - log(mean_price)
      changes[[length(changes) + 1]] <- new_change(
        paste0("price_", fuel), "price", before, after, fuel = fuel,
        value_before = q25, value_after = q75)
      # at each other heat demand, in a model with the price times the heat
      # demand: the same change in a house with that demand
      if (all(demand_terms %in% prices)) {
        for (k in seq_len(nrow(demand_dummies))) {
          before_k <- before
          after_k <- after
          before_k$att_heat_demand <- demand_dummies$level[k]
          after_k$att_heat_demand <- demand_dummies$level[k]
          changes[[length(changes) + 1]] <- new_change(
            paste0("price_", fuel, "_", demand_dummies$dummy[k]), "price",
            before_k, after_k, fuel = fuel, demand = demand_dummies$dummy[k],
            value_before = q25, value_after = q75)
        }
      }
    }
  }

  # (b) Price not reported: from reported (at the mean) to not reported, for
  #     the fuels whose dummy the model has
  for (fuel in price_fuels) {
    if (paste0(fuel, "_price_na") %in% prices) {
      after <- list()
      after[[paste0("price_", fuel, "_na")]] <- 1
      changes[[length(changes) + 1]] <- new_change(
        paste0("price_na_", fuel), "price_na", list(), after, fuel = fuel)
    }
  }

  # (c) Expected rise: from no (0 minus the share) to yes (1 minus the share),
  #     for one fuel; the other fuels stay as in the base state (their share,
  #     or no in the neutral base state)
  if ("expects_rise" %in% prices) {
    for (fuel in expectation_fuels) {
      is_fuel <- expert_reference$pexp$fuel == fuel
      share <- expert_reference$pexp$share_rise[is_fuel]
      before <- list()
      before[[paste0("pexp_", fuel, "_c")]] <- 0 - share
      after <- list()
      after[[paste0("pexp_", fuel, "_c")]] <- 1 - share
      changes[[length(changes) + 1]] <- new_change(
        paste0("expects_rise_", fuel), "expects_rise", before, after,
        fuel = fuel)
    }
  }

  # (d) No expectation: from the base state to blank, for the fuels whose
  #     dummy the model has. A blank is coded as in 01: the dummy 1 and the
  #     expected rise 0.
  for (fuel in expectation_fuels) {
    if (paste0(fuel, "_rise_na") %in% prices) {
      after <- list()
      after[[paste0("pexp_", fuel, "_c")]] <- 0
      after[[paste0("pexp_", fuel, "_na")]] <- 1
      changes[[length(changes) + 1]] <- new_change(
        paste0("rise_na_", fuel), "rise_na", list(), after, fuel = fuel)
    }
  }

  # (e) Heating at home: from not this technology (0 minus the share) to this
  #     technology (1 minus the share); the other technologies stay as in the
  #     base state (their share, or not at home in the neutral base state).
  #     A technology of own_heating_before replaces another heating: before
  #     the change that one is at home (1 minus its share), after it not (0
  #     minus its share), e.g. a heat pump instead of a gas heating
  if ("own_heating_match" %in% prices) {
    own_technologies <- intersect(data_reference$own_heating_answers, outcomes)
    # a name that is not a heating at home of the model would be skipped
    unknown <- setdiff(names(own_heating_before), own_technologies)
    if (length(unknown) > 0) {
      stop("OWN_HEATING_BEFORE: ", paste(unknown, collapse = ", "),
           " is not a heating at home of the model")
    }
    for (technology in own_technologies) {
      share <- expert_reference$own_heating_share[[technology]]
      before <- list()
      before[[paste0("own_match_", technology)]] <- 0 - share
      after <- list()
      after[[paste0("own_match_", technology)]] <- 1 - share
      id <- paste0("own_heating_", technology)
      replaced <- NA_character_
      if (technology %in% names(own_heating_before)) {
        replaced <- own_heating_before[[technology]]
        if (!(replaced %in% own_technologies) || replaced == technology) {
          stop("OWN_HEATING_BEFORE: ", replaced, " is not another heating at ",
               "home of the model")
        }
        replaced_share <- expert_reference$own_heating_share[[replaced]]
        before[[paste0("own_match_", replaced)]] <- 1 - replaced_share
        after[[paste0("own_match_", replaced)]] <- 0 - replaced_share
        id <- paste0(id, "_instead_of_", replaced)
      }
      changes[[length(changes) + 1]] <- new_change(
        id, "own_heating", before, after, technology = technology,
        condition = replaced)
    }
  }

  # (e) A heat pump at home: from no (0 minus the share) to yes (1 minus the
  #     share), in a model with heat_pump_at_home among the regressors ((4pe),
  #     (6pe)); with one coefficient per technology, it moves every utility.
  #     The other heatings at home are not in the model.
  if ("heat_pump_at_home" %in% regressors) {
    share <- expert_reference$own_heating_share[["Wärmepumpe"]]
    changes[[length(changes) + 1]] <- new_change(
      "heat_pump_at_home", "own_heating",
      list(heat_pump_at_home = 0 - share), list(heat_pump_at_home = 1 - share),
      technology = "Wärmepumpe")
  }

  # (f) Heating degree days: from the 25th to the 75th percentile, centred
  #     and in units as in 01
  if ("hdd_c" %in% regressors) {
    q25 <- data_reference$hdd_quartiles[["q25"]]
    q75 <- data_reference$hdd_quartiles[["q75"]]
    changes[[length(changes) + 1]] <- new_change(
      "hdd", "hdd",
      list(hdd_c = (q25 - expert_reference$hdd_mean) / expert_reference$hdd_unit),
      list(hdd_c = (q75 - expert_reference$hdd_mean) / expert_reference$hdd_unit),
      value_before = q25, value_after = q75)
  }

  # (g) Interactions of the attributes: for each pair of dummies whose product
  #     <dummy>_x_<dummy> is a regressor (the exact names of add_regressors()),
  #     the AME of each at the other's level
  for (i in seq_len(nrow(attribute_dummies))) {
    for (j in seq_len(nrow(attribute_dummies))) {
      interaction <- paste0(attribute_dummies$dummy[i], "_x_",
                            attribute_dummies$dummy[j])
      if (interaction %in% regressors) {
        pairs <- list(c(i, j), c(j, i))
        for (pair in pairs) {
          changed <- attribute_dummies[pair[1], ]
          condition <- attribute_dummies[pair[2], ]
          before <- list()
          before[[condition$attribute]] <- condition$level
          after <- before
          after[[changed$attribute]] <- changed$level
          changes[[length(changes) + 1]] <- new_change(
            paste0(changed$dummy, "_at_", condition$dummy), "interaction",
            before, after, attribute = changed$dummy,
            condition = condition$dummy)
        }
      }
    }
  }
  changes
}

#' Rows of states: the base state with the values of a change set
apply_change <- function(state, values) {
  for (column in names(values)) {
    if (!(column %in% names(state))) stop("Not a column of the state: ", column)
    state[[column]] <- values[[column]]
  }
  state
}


# 2. One small simulated model, with one set of constants and by group ---------
n_experts <- 300
outcomes_data <- c("A", "B", "C", "D", "E")
reference <- "C"
technologies <- c("A", "B", "D", "E")
groups <- c("g1", "g2", "g3")
experts_simulated <- tibble(
  resp_uid = sprintf("e%04d", seq_len(n_experts)),
  expert_group = sample(groups, n_experts, replace = TRUE),
  price_a = rnorm(n_experts, 10, 2),
  price_b = rnorm(n_experts, 20, 3)
)
make_regressors_simulated <- function(states) {
  states %>%
    mutate(is_g1 = if_else(expert_group == "g1", 1, 0),
           is_g2 = if_else(expert_group == "g2", 1, 0),
           is_g3 = if_else(expert_group == "g3", 1, 0),
           price_a_c = price_a - 10,
           price_b_c = price_b - 20) %>%
    cross_join(tibble(alternative = factor(outcomes_data, levels = outcomes_data))) %>%
    mutate(price = case_when(alternative == "A" ~ price_a_c,
                             alternative == "B" ~ price_b_c,
                             TRUE ~ 0))
}
decisions_simulated <- experts_simulated %>%
  cross_join(tibble(vig_num = 1:6)) %>%
  mutate(x1 = rbinom(n(), 1, 0.5)) %>%
  arrange(resp_uid, vig_num) %>%
  mutate(chid = row_number())
true_L <- matrix(rnorm(16, 0, 0.3), 4, 4)
true_L[upper.tri(true_L)] <- 0
diag(true_L) <- abs(diag(true_L)) + 0.7
constants_of_experts <- t(c(0.5, -0.3, 0.2, -0.8) + true_L %*% matrix(rnorm(4 * n_experts), 4, n_experts))
colnames(constants_of_experts) <- technologies
utilities <- make_regressors_simulated(decisions_simulated) %>%
  mutate(constant = 0)
for (technology in technologies) {
  rows <- utilities$alternative == technology
  utilities$constant[rows] <- constants_of_experts[match(utilities$resp_uid[rows], experts_simulated$resp_uid), technology]
}
choice_long_simulated <- utilities %>%
  mutate(shift = case_when(alternative == "A" ~ 0.5 * x1 + 0.3 * is_g2,
                           alternative == "D" ~ -0.4 * x1 + 0.4 * is_g3,
                           TRUE ~ 0),
         utility = constant + shift - 0.2 * price - log(-log(runif(n())))) %>%
  group_by(chid) %>%
  mutate(chosen = utility == max(utility)) %>%
  ungroup() %>%
  arrange(chid, alternative)
choice_data <- dfidx(choice_long_simulated, idx = list(c("chid", "resp_uid"), "alternative"),
                     choice = "chosen", shape = "long")
# The model's call names its data `data`: update() in vcov_cluster_robust() finds
# them here, whatever the function's argument is called
data <- choice_data

m_small <- fit_mixed_logit("m_small", chosen ~ price | 1 + is_g2 + is_g3, choice_data, reference,
                           est_dir, n_draws = N_DRAWS_TEST)
m_common <- fit_mixed_logit("m_common", chosen ~ price | 1 + is_g2 + is_g3 + x1, choice_data,
                            reference, est_dir, n_draws = N_DRAWS_TEST, start_from = m_small)
m_group <- fit_mixed_logit("m_group", chosen ~ price | 0 + is_g1 + is_g2 + is_g3 + x1, choice_data,
                           reference, est_dir, n_draws = N_DRAWS_TEST,
                           group_constants = c("is_g1", "is_g2", "is_g3"), start_from = m_common)

# 3. vcov_cluster_robust() against post_estimation() ---------------------------
for (model in list(m_common, m_group)) {
  time_old <- system.time(old_robust <- vcov_cluster_robust(model, choice_data))[["elapsed"]]
  time_new <- system.time(results <- post_estimation(model, est_dir, n_draws_kr = N_KR_TEST,
                                                     n_workers = N_WORKERS_TEST))[["elapsed"]]
  cat("\n", model$name, ": seconds, old", round(time_old, 1), "| new", round(time_new, 1), "\n")
  new_se <- sqrt(diag(results$vcov))
  old_se <- sqrt(diag(old_robust$vcov))[names(new_se)]
  check(paste(model$name, "largest relative difference in the standard errors"),
        max(abs(new_se / old_se - 1)), 1e-3)
  check(paste(model$name, "largest relative difference in the bread"),
        max(abs(diag(results$inverse_hessian) / diag(old_robust$inverse_hessian)[names(new_se)] - 1)), 1e-3)
  check(paste(model$name, "largest difference in the coefficient table's estimates"),
        max(abs(results$coefficient_table$estimate - coef(model))), TOLERANCE)
}
results_common <- post_estimation(m_common, est_dir, n_draws_kr = N_KR_TEST, n_workers = N_WORKERS_TEST)
results_group <- post_estimation(m_group, est_dir, n_draws_kr = N_KR_TEST, n_workers = N_WORKERS_TEST)

# The Cholesky factor of compute_ames() reproduces mlogit's covariance matrix, so the
# order and names of the random constants are right. unclass(): as a plain matrix.
parts <- coefficient_parts(results_common)
L <- matrix(0, 4, 4)
L[cbind(parts$cholesky$i, parts$cholesky$j)] <- results_common$coefficients[parts$cholesky$parameter]
omega_mlogit <- unclass(cov.mlogit(m_common))[results_common$random$name, results_common$random$name]
check("L L' against cov.mlogit()", max(abs(L %*% t(L) - omega_mlogit)), 1e-8)

# 4. ame_krinsky_robb() against compute_ames() ----------------------------------
#' The same Halton draws as compute_ames(), the primes starting again per group
halton_draws <- function(random, n_draws) {
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
  Z <- matrix(NA_real_, n_draws, nrow(random))
  for (constant in unique(random$column)) {
    of_constant <- which(random$column == constant)
    for (position in seq_along(of_constant)) {
      Z[, of_constant[position]] <- qnorm(mlogit:::halton(primes[position], n_draws, 100))
    }
  }
  Z
}
vignettes_old <- tibble(vignette = c("reference", "x1_on"), x1 = c(0, 1))
at_simulated <- list(x1 = 0, price_a = 10, price_b = 20)
changes_simulated <- list(x1_on = list(to = list(x1 = 1)))
for (results in list(results_common, results_group)) {
  columns <- unique(coefficient_parts(results)$terms$column)
  columns <- setdiff(columns, "price")
  X_by_profile <- list()
  for (group in groups) {
    profile_vignettes <- vignettes_old %>%
      mutate(is_g1 = if_else(group == "g1", 1, 0),
             is_g2 = if_else(group == "g2", 1, 0),
             is_g3 = if_else(group == "g3", 1, 0))
    X <- design_matrix(profile_vignettes, columns)
    rownames(X) <- vignettes_old$vignette
    X_by_profile[[group]] <- X
  }
  standard_draws <- halton_draws(results$random, N_PREDICTION_TEST)
  old_ames <- ame_krinsky_robb(results$coefficients, results$theta_draws, X_by_profile, standard_draws,
                               results$random, results$outcomes, results$fixed_parameters)
  time_new <- system.time(results <- compute_ames(
    results, make_regressors_simulated, name = "x1", at = at_simulated, changes = changes_simulated,
    profiles = groups, n_draws = N_PREDICTION_TEST, n_workers = N_WORKERS_TEST, force = TRUE
  ))[["elapsed"]]
  new_table <- results$ames$x1$table
  new_baseline <- results$ames$x1$baseline
  joined <- old_ames$table %>%
    mutate(effect = vignette) %>%
    left_join(new_table, by = c("profile", "effect", "outcome"), suffix = c("_old", "_new"),
              relationship = "one-to-one")
  stopifnot(nrow(joined) == nrow(new_table), !anyNA(joined$ame_new))
  cat("\n", results$name, ": effects in", round(time_new, 1), "seconds\n")
  check(paste(results$name, "AMEs"), max(abs(joined$ame_old - joined$ame_new)), TOLERANCE)
  check(paste(results$name, "standard errors"), max(abs(joined$se_old - joined$se_new)), TOLERANCE)
  check(paste(results$name, "percentile intervals"),
        max(abs(c(joined$ci_low_kr_old - joined$ci_low_kr_new, joined$ci_high_kr_old - joined$ci_high_kr_new))),
        TOLERANCE)
  check(paste(results$name, "intervals, z and p"),
        max(abs(c(joined$ci_low_old - joined$ci_low_new, joined$z_old - joined$z_new, joined$p_old - joined$p_new))),
        1e-8)
  check(paste(results$name, "probability at the reference"), max(abs(joined$p_reference - joined$p_from)), TOLERANCE)
  baseline_joined <- joined %>%
    left_join(new_baseline, by = c("profile", "outcome"), suffix = c("", "_baseline"), relationship = "many-to-one")
  check(paste(results$name, "standard error of the probability"),
        max(abs(baseline_joined$se_p_reference - baseline_joined$se)), TOLERANCE)
  largest_draw_difference <- 0
  for (group in groups) {
    largest_draw_difference <- max(
      largest_draw_difference,
      abs(old_ames$probability_draws[[group]]["reference", , ] -
            results$ames$x1$draws[[group]]["baseline", dimnames(old_ames$probability_draws[[group]])[[2]], ]),
      abs(old_ames$probability_draws[[group]]["x1_on", , ] -
            results$ames$x1$draws[[group]]["x1_on|to", dimnames(old_ames$probability_draws[[group]])[[2]], ])
    )
  }
  check(paste(results$name, "probabilities at every draw"), largest_draw_difference, TOLERANCE)
}

# 5. With terms of one coefficient: probabilities_krinsky_robb() ----------------
at_price <- list(x1 = 1, price_a = 13, price_b = 17)
changes_price <- list(price_a_q = list(from = list(price_a = 8), to = list(price_a = 12)))
results <- compute_ames(results_common, make_regressors_simulated, name = "price", at = at_price,
                        changes = changes_price, profiles = groups,
                        n_draws = N_PREDICTION_TEST, n_workers = N_WORKERS_TEST)
price_states <- c("baseline", "price_a_q|from", "price_a_q|to")
price_values <- matrix(0, 3, length(results$outcomes), dimnames = list(price_states, results$outcomes))
price_values[, "A"] <- c(3, -2, 2)
price_values[, "B"] <- c(-3, -3, -3)
X <- cbind("(Intercept)" = c(1, 1, 1), is_g2 = c(1, 1, 1), is_g3 = c(0, 0, 0), x1 = c(1, 1, 1))
rownames(X) <- price_states
old_probabilities <- probabilities_krinsky_robb(
  results$coefficients, results$theta_draws, X, halton_draws(results$random, N_PREDICTION_TEST),
  results$random, results$outcomes, NULL, list(price = price_values)
)
new_g2 <- results$ames$price$draws[["g2"]]
check("a term with one coefficient: probabilities at every draw",
      max(abs(old_probabilities$draws[price_states, results$outcomes, ] - new_g2[price_states, results$outcomes, ])),
      TOLERANCE)
new_table_g2 <- results$ames$price$table %>%
  filter(profile == "g2")
check("a term with one coefficient: AMEs",
      max(abs((old_probabilities$estimate["price_a_q|to", ] - old_probabilities$estimate["price_a_q|from", ])[new_table_g2$outcome] -
                new_table_g2$ame)), TOLERANCE)

# 6. start_from_nested() and start_from_common() --------------------------------
# All parameters, also those fixed by constPar: $coefficients. The log-likelihood at
# the old starting values, from the fast likelihood, against the model it starts from
# and against what the new fit_mixed_logit() recorded.
start_nested <- start_from_nested(names(m_common$coefficients), coef(m_small))
log_likelihood_nested <- simulated_likelihood(start_nested[names(coef(m_common))],
                                              prepare_likelihood(m_common))$log_likelihood
check("start_from_nested(): against the model it starts from",
      abs(log_likelihood_nested - as.numeric(logLik(m_small))), 1e-6)
check("start_from_nested(): against fit_mixed_logit()",
      abs(log_likelihood_nested - m_common$fit_info$log_likelihood_start), 1e-6)
start_common <- start_from_common(names(m_group$coefficients), coef(m_common),
                                  c("is_g1", "is_g2", "is_g3"), results_group$outcomes[-1],
                                  names(results_group$fixed_parameters))
log_likelihood_common <- simulated_likelihood(start_common[names(coef(m_group))],
                                              prepare_likelihood(m_group))$log_likelihood
check("start_from_common(): against the model it starts from",
      abs(log_likelihood_common - as.numeric(logLik(m_common))), 1e-6)
check("start_from_common(): against fit_mixed_logit()",
      abs(log_likelihood_common - m_group$fit_info$log_likelihood_start), 1e-6)

# 7. make_regressors() of both scripts against the old variables ----------------
# On the synthetic cleaned data. From each script only its settings, its lists and
# make_regressors(): the rest would estimate.
script_objects <- function(script) {
  wanted <- c("alternatives", "reference", "reference_values", "COP", "HYBRID_GAS_SHARE",
              "prices", "expectations", "price_mean", "price_q25", "price_q75", "rise_share",
              "heat_pump_share", "hdd_mean", "make_regressors", "at_neutral", "attribute_changes",
              "price_changes", "expectation_changes", "expert_changes")
  objects <- new.env(parent = globalenv())
  for (expression in as.list(parse(script, encoding = "UTF-8", keep.source = FALSE))) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
        is.name(expression[[2]]) && as.character(expression[[2]]) %in% wanted) {
      eval(expression, objects)
    }
  }
  objects
}

# The settings of the old 01_run_specifications.R, sections 1 and 2.5
ATTRIBUTE_DUMMIES <- tribble(
  ~dummy,               ~attribute,               ~level,
  "age_60",             "att_couple_age",         "Mitte 60",
  "income_low",         "att_income",             "unter 40.000 € brutto im Jahr",
  "heating_oil",        "att_current_heating",    "Ölheizung",
  "replacement_urgent", "att_replacement_timing", "ist defekt und muss kurzfristig ersetzt werden",
  "built_2000",         "att_build_year",         "2000",
  "demand_70",          "att_heat_demand",        "70",
  "demand_200",         "att_heat_demand",        "200",
  "floor_heating",      "att_heat_distribution",  "eine Fußbodenheizung"
)
REFERENCE_LEVELS <- list(
  att_couple_age = "Mitte 40",
  att_income = "65.000 € brutto im Jahr",
  att_current_heating = "Gasheizung",
  att_replacement_timing = "muss in den nächsten Jahren ersetzt werden",
  att_build_year = "1970",
  att_heat_demand = "120",
  att_heat_distribution = "Heizkörper"
)
OLD_PRICE_VARIABLES <- tribble(
  ~technology,     ~name,    ~price,                ~log_price,             ~not_reported,
  "Gasheizung",    "gas",    "price_gas_c",         "price_gas_lc",         "price_gas_na",
  "Ölheizung",     "oil",    "price_oil_c",         "price_oil_lc",         "price_oil_na",
  "Pelletheizung", "pellet", "price_pellet_c",      "price_pellet_lc",      "price_pellet_na",
  "Wärmepumpe",    "hpelec", "price_hpelec_heat_c", "price_hpelec_heat_lc", "price_hpelec_na",
  "Hybridheizung", "hybrid", "price_hybrid_c",      "price_hybrid_lc",      "price_hybrid_na",
  "Fernwärme",     "dh",     "price_dh_c",          "price_dh_lc",          "price_dh_na"
)
OLD_EXPECTATION_VARIABLES <- tribble(
  ~technology,     ~name,    ~rise,           ~not_answered,
  "Gasheizung",    "gas",    "pexp_gas_c",    "pexp_gas_na",
  "Ölheizung",     "oil",    "pexp_oil_c",    "pexp_oil_na",
  "Pelletheizung", "pellet", "pexp_pellet_c", "pexp_pellet_na",
  "Wärmepumpe",    "hpelec", "pexp_hpelec_c", "pexp_hpelec_na",
  "Hybridheizung", "hybrid", "pexp_hybrid_c", "pexp_hybrid_na",
  "Fernwärme",     "dh",     "pexp_dh_c",     "pexp_dh_na"
)
OLD_GROUPS <- c("HVAC installers" = "SHK-Handwerk", "Chimney sweeps" = "Schornsteinfeger",
                "Energy advisers" = "Energieberater")
OWN_HEATING_ANSWERS <- c("Wärmepumpe", "Pelletheizung", "Fernwärme", "Gasheizung", "Ölheizung",
                         "Hybridheizung", "Sonstiges")
experts_clean <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds"))
vignettes_clean <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds"))

for (arm_scripts in list(list(arm = "fw", script = "district_heating"),
                         list(arm = "nofw", script = "no_district_heating"))) {
  objects <- script_objects(file.path("R", "estimation", paste0(arm_scripts$script, ".R")))
  reference_values <- objects$reference_values
  outcomes_arm <- objects$alternatives
  price_variables <- OLD_PRICE_VARIABLES %>%
    filter(technology %in% outcomes_arm)
  expectation_variables <- OLD_EXPECTATION_VARIABLES %>%
    filter(technology %in% outcomes_arm)

  # Section 2.1 of the old script, from the same reference values
  experts_old <- experts_clean %>%
    filter(is_complete, arm == arm_scripts$arm, !is.na(expert_group)) %>%
    mutate(berufsgruppe = OLD_GROUPS[as.character(expert_group)])
  for (k in seq_len(nrow(reference_values$prices))) {
    fuel <- reference_values$prices$fuel[k]
    price <- experts_old[[paste0("price_", fuel, "_kwh")]]
    experts_old[[paste0("price_", fuel, "_na")]] <- if_else(is.na(price), 1, 0)
    experts_old[[paste0("price_", fuel, "_c")]] <- if_else(is.na(price), 0, price - reference_values$prices$mean[k])
    experts_old[[paste0("price_", fuel, "_lc")]] <- if_else(is.na(price), 0, log(price) - log(reference_values$prices$mean[k]))
    rise <- experts_old[[paste0("pexp_", fuel, "_rise")]]
    experts_old[[paste0("pexp_", fuel, "_na")]] <- if_else(is.na(rise), 1, 0)
    experts_old[[paste0("pexp_", fuel, "_c")]] <- if_else(is.na(rise), 0, rise - reference_values$expectations$share_rise[k])
  }
  experts_old <- experts_old %>%
    mutate(heat_pump_at_home_old = heat_pump_at_home - reference_values$own_heating[["Wärmepumpe"]],
           hdd_c_old = if_else(is.na(hdd), 0, (hdd - reference_values$hdd$mean) / 1000))
  decisions_old <- vignettes_clean %>%
    filter(resp_uid %in% experts_old$resp_uid, !is.na(vig_rec)) %>%
    select(resp_uid, vig_num, vig_rec, starts_with("att_")) %>%
    left_join(experts_old, by = "resp_uid", relationship = "many-to-one") %>%
    arrange(resp_uid, vig_num) %>%
    mutate(chid = row_number())

  decisions_wide <- decisions_old %>%
    add_regressors(ATTRIBUTE_DUMMIES) %>%
    add_alternative_prices(objects$COP, objects$HYBRID_GAS_SHARE)
  long_old <- decisions_wide %>%
    cross_join(tibble(alternative = factor(sort(outcomes_arm), levels = sort(outcomes_arm)))) %>%
    long_terms(price_variables, expectation_variables, reference_values$own_heating,
               OWN_HEATING_ANSWERS, outcomes_arm) %>%
    arrange(chid, alternative)
  long_new <- objects$make_regressors(decisions_old) %>%
    arrange(chid, alternative)

  check(paste(arm_scripts$arm, "the same rows"),
        as.numeric(!identical(paste(long_old$chid, long_old$alternative),
                              paste(long_new$chid, long_new$alternative))), 0.5)
  check(paste(arm_scripts$arm, "alternatives are alphabetical on this computer"),
        as.numeric(!identical(sort(outcomes_arm), objects$alternatives)), 0.5)
  same_names <- c(ATTRIBUTE_DUMMIES$dummy, "price", paste0(price_variables$name, "_price_na"),
                  "expects_rise", paste0(expectation_variables$name, "_rise_na"))
  largest <- 0
  for (column in same_names) {
    largest <- max(largest, abs(long_old[[column]] - long_new[[column]]))
    if (anyNA(long_new[[column]])) stop("Missing values in ", column)
  }
  check(paste(arm_scripts$arm, "attributes, prices, expected rises and their dummies"), largest, 1e-12)
  check(paste(arm_scripts$arm, "groups"),
        max(abs(long_old$is_shk - long_new$is_hvac), abs(long_old$is_schornsteinfeger - long_new$is_sweep),
            abs(long_old$is_energieberater - long_new$is_adviser)), 1e-12)
  check(paste(arm_scripts$arm, "degree days and a heat pump at home"),
        max(abs(long_old$hdd_c_old - long_new$hdd_c),
            abs(long_old$heat_pump_at_home_old - long_new$heat_pump_at_home_c)), 1e-12)
}

# 8. term_changes() of old 01_run_term_effects.R against the script's changes ----
# The results of (4pe) and (6pe) of R/estimation/district_heating.R, read as the old
# code would have them: same parameters under the old names. The old code then computes
# the probabilities of its neutral state and of its changes, at the estimates and at the
# Krinsky-Robb draws of the model.
objects <- script_objects(file.path("R", "estimation", "district_heating.R"))
reference_values <- objects$reference_values
outcomes_script <- c(objects$reference, setdiff(objects$alternatives, objects$reference))
old_names <- function(names_new) {
  names_new <- gsub("is_hvac", "is_shk", names_new, fixed = TRUE)
  names_new <- gsub("is_sweep", "is_schornsteinfeger", names_new, fixed = TRUE)
  names_new <- gsub("is_adviser", "is_energieberater", names_new, fixed = TRUE)
  gsub("heat_pump_at_home_c", "heat_pump_at_home", names_new, fixed = TRUE)
}
price_reference <- reference_values$prices
price_quartiles <- reference_values$prices %>%
  select(fuel, q25, q75)
expert_reference <- list(
  pexp = reference_values$expectations,
  own_heating_share = reference_values$own_heating,
  hdd_year = reference_values$hdd$year,
  hdd_unit = 1000,
  hdd_mean = reference_values$hdd$mean
)
data_reference <- list(
  price_reference = price_reference,
  price_quartiles = price_quartiles,
  expert_reference = expert_reference,
  hdd_quartiles = c(q25 = reference_values$hdd$q25, q75 = reference_values$hdd$q75),
  price_settings = list(cop = objects$COP, hybrid_gas_share = objects$HYBRID_GAS_SHARE),
  price_variables = OLD_PRICE_VARIABLES,
  expectation_variables = OLD_EXPECTATION_VARIABLES,
  own_heating_answers = OWN_HEATING_ANSWERS
)
OLD_PRICES <- c("price", paste0(OLD_PRICE_VARIABLES$name, "_price_na"))
OLD_TERMS <- c("expects_rise", paste0(OLD_EXPECTATION_VARIABLES$name, "_rise_na"))
for (model_name in c("m4pe", "m6pe")) {
  results_file <- file.path(OUTPUT_DIR, "district_heating", model_name, "results.rds")
  if (!file.exists(results_file)) {
    stop("Run R/estimation/district_heating.R first: no ", results_file)
  }
  results <- readRDS(results_file)
  new_ames <- results$ames$neutral
  outcomes <- results$outcomes
  stopifnot(identical(outcomes, outcomes_script))

  random_old <- results$random %>%
    mutate(name = old_names(name), column = old_names(column))
  theta_old <- setNames(as.numeric(results$coefficients), old_names(names(results$coefficients)))
  draws_old <- results$theta_draws
  colnames(draws_old) <- old_names(colnames(draws_old))
  fixed_old <- results$fixed_parameters
  if (!is.null(fixed_old)) {
    names(fixed_old) <- old_names(names(fixed_old))
  }
  constants <- "(Intercept)"
  shifts <- c("is_shk", "is_schornsteinfeger")
  if (model_name == "m6pe") {
    constants <- c("is_shk", "is_schornsteinfeger", "is_energieberater")
    shifts <- character(0)
  }
  specification <- list(constants = constants, prices = c(OLD_PRICES, OLD_TERMS),
                        regressors = c(shifts, ATTRIBUTE_DUMMIES$dummy, "hdd_c", "heat_pump_at_home"))
  changes_old <- term_changes(specification, ATTRIBUTE_DUMMIES, outcomes, data_reference,
                              c("Wärmepumpe" = "Gasheizung"))
  technologies_own <- intersect(OWN_HEATING_ANSWERS, outcomes)
  reference_old <- reference_state(REFERENCE_LEVELS, price_reference$fuel, technologies_own)
  base_old <- neutral_state(reference_old, specification$prices, specification$regressors,
                            expert_reference, price_reference$fuel, technologies_own)
  states <- base_old %>%
    mutate(vignette = "base")
  for (k in seq_len(nrow(ATTRIBUTE_DUMMIES))) {
    variation <- base_old
    variation[[ATTRIBUTE_DUMMIES$attribute[k]]] <- ATTRIBUTE_DUMMIES$level[k]
    states <- bind_rows(states, variation %>%
                          mutate(vignette = paste0("base|", ATTRIBUTE_DUMMIES$dummy[k])))
  }
  for (change in changes_old) {
    if (length(change$before) > 0) {
      states <- bind_rows(states, apply_change(base_old, change$before) %>%
                            mutate(vignette = paste0(change$id, "|before")))
    }
    states <- bind_rows(states, apply_change(base_old, change$after) %>%
                          mutate(vignette = paste0(change$id, "|after")))
  }
  groups_old <- c("Energieberater", "SHK-Handwerk", "Schornsteinfeger")
  X_by_profile <- design_by_profile(states, groups_old, c(constants, specification$regressors),
                                    ATTRIBUTE_DUMMIES)
  states_long <- states %>%
    mutate(berufsgruppe = groups_old[1], own_heating = "Keine Angabe") %>%
    add_regressors(ATTRIBUTE_DUMMIES) %>%
    add_alternative_prices(objects$COP, objects$HYBRID_GAS_SHARE) %>%
    cross_join(tibble(alternative = outcomes)) %>%
    long_terms(OLD_PRICE_VARIABLES, OLD_EXPECTATION_VARIABLES, reference_values$own_heating,
               OWN_HEATING_ANSWERS, outcomes)
  generic_values <- list()
  for (term in specification$prices) {
    values <- matrix(NA_real_, nrow(states), length(outcomes), dimnames = list(states$vignette, outcomes))
    for (outcome_name in outcomes) {
      values[, outcome_name] <- states_long[[term]][states_long$alternative == outcome_name]
    }
    generic_values[[term]] <- values
  }
  standard_draws <- halton_draws(results$random, new_ames$n_draws)
  labels_new <- c("Energieberater" = "Energy advisers", "SHK-Handwerk" = "HVAC installers",
                  "Schornsteinfeger" = "Chimney sweeps")
  largest_baseline <- 0
  largest_effect <- 0
  largest_se <- 0
  n_compared <- 0
  for (profile_name in names(X_by_profile)) {
    old_probabilities <- probabilities_krinsky_robb(
      theta_old, draws_old, X_by_profile[[profile_name]], standard_draws, random_old, outcomes, fixed_old,
      generic_values
    )
    new_table <- new_ames$table %>%
      filter(profile == labels_new[[profile_name]])
    new_baseline <- new_ames$baseline %>%
      filter(profile == labels_new[[profile_name]])
    largest_baseline <- max(largest_baseline,
                            abs(old_probabilities$estimate["base", new_baseline$outcome] - new_baseline$probability))
    for (k in seq_len(nrow(new_table))) {
      effect <- new_table$effect[k]
      outcome_name <- new_table$outcome[k]
      if (effect %in% ATTRIBUTE_DUMMIES$dummy) {
        row_from <- "base"
        row_to <- paste0("base|", effect)
      } else {
        row_to <- paste0(effect, "|after")
        row_from <- paste0(effect, "|before")
        if (!(row_from %in% states$vignette)) {
          row_from <- "base"
        }
      }
      ame_old <- old_probabilities$estimate[row_to, outcome_name] - old_probabilities$estimate[row_from, outcome_name]
      se_old <- sd(old_probabilities$draws[row_to, outcome_name, ] - old_probabilities$draws[row_from, outcome_name, ])
      largest_effect <- max(largest_effect, abs(ame_old - new_table$ame[k]))
      largest_se <- max(largest_se, abs(se_old - new_table$se[k]))
      n_compared <- n_compared + 1
    }
  }
  old_ids <- character(0)
  for (change in changes_old) {
    old_ids <- c(old_ids, change$id)
  }
  check(paste(model_name, "every old change is among the new effects"),
        length(setdiff(c(old_ids, ATTRIBUTE_DUMMIES$dummy), new_ames$table$effect)), 0.5)
  check(paste(model_name, "every new effect is an old change"),
        length(setdiff(new_ames$table$effect, c(old_ids, ATTRIBUTE_DUMMIES$dummy))), 0.5)
  cat(model_name, ":", n_compared, "effects compared\n")
  check(paste(model_name, "probabilities at the neutral state"), largest_baseline, TOLERANCE)
  check(paste(model_name, "effects"), largest_effect, TOLERANCE)
  check(paste(model_name, "standard errors"), largest_se, TOLERANCE)

  # The script's other reference state, at the average expert, is the reference state
  # of the old 01_run_specifications.R: the AMEs of its section 4.4
  if (model_name == "m4pe") {
    vignettes_at_mean <- as_tibble(REFERENCE_LEVELS) %>%
      mutate(vignette = "reference")
    for (k in seq_len(nrow(ATTRIBUTE_DUMMIES))) {
      variation <- as_tibble(REFERENCE_LEVELS)
      variation[[ATTRIBUTE_DUMMIES$attribute[k]]] <- ATTRIBUTE_DUMMIES$level[k]
      vignettes_at_mean <- bind_rows(vignettes_at_mean, variation %>%
                                       mutate(vignette = ATTRIBUTE_DUMMIES$dummy[k]))
    }
    vignettes_at_mean <- vignettes_at_mean %>%
      mutate(hdd_c = 0, heat_pump_at_home = 0)
    X_average <- design_by_profile(vignettes_at_mean, groups_old, c(constants, specification$regressors),
                                   ATTRIBUTE_DUMMIES)
    old_average <- ame_krinsky_robb(theta_old, draws_old, X_average, standard_draws, random_old,
                                    outcomes, fixed_old)
    joined_average <- old_average$table %>%
      mutate(profile = labels_new[profile], effect = vignette) %>%
      inner_join(results$ames$average$table, by = c("profile", "effect", "outcome"),
                 suffix = c("_old", "_new"), relationship = "one-to-one")
    stopifnot(nrow(joined_average) == nrow(old_average$table),
              nrow(joined_average) == nrow(results$ames$average$table))
    check("m4pe at the average expert: AMEs against ame_krinsky_robb()",
          max(abs(joined_average$ame_old - joined_average$ame_new)), TOLERANCE)
    check("m4pe at the average expert: standard errors",
          max(abs(joined_average$se_old - joined_average$se_new)), TOLERANCE)
  }
}

cat("\nEquivalence with the old functions: all checks passed\n")
