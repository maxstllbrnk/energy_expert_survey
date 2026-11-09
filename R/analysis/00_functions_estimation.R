################################################################################
# 00_functions_estimation.R — functions for the mixed logit models of the
#                             recommendations: estimation, covariance matrix
#                             clustered by expert, probabilities and AMEs with
#                             Krinsky-Robb standard errors
# Inputs : none; sourced by 01_run_specifications.R
# Outputs: none
# Checks : inside the functions: starting values for every parameter;
#          convergence; the re-evaluated gradient; a positive definite Hessian
################################################################################
# The model (Train 2009, chapters 6 and 10). Expert n, technology j, vignette t:
#
#   U_njt = a_nj + x_nt' b_j + g' p_njt + e_njt
#   U_n0t =                    g' p_n0t + e_n0t   (reference outcome)
#
# a_nj is the expert's constant of technology j. The constants vary across
# experts: a_n = a + L z_n with z_n ~ N(0, I), so they are normal with means a
# and covariance matrix Omega = L L', correlated. x_nt holds the regressors
# (vignette attributes, Berufsgruppe, interactions), each with one fixed
# coefficient per technology. p_njt holds the price terms of a model with
# prices, e.g. the technology's own price per kWh, with fixed coefficients g
# that are the same for all technologies. e_njt is iid type-I extreme value.
# The probability of an expert's choices is a product of logits integrated
# over a_n; mlogit() simulates the integral with Halton draws per expert.
#
# A model has either one set of constants for all experts ("(Intercept)") or
# one set per Berufsgruppe (the group dummies is_shk, is_schornsteinfeger,
# is_energieberater). With one set per Berufsgruppe, each has its own means
# and covariance matrix. Every expert has the constants of one group only, so
# the covariances between two groups' constants never enter the likelihood:
# they are fixed at 0 (constPar), a normalisation rather than a restriction.

# 1. Helpers -------------------------------------------------------------------

#' Text as part of a file or folder name: lower case, umlauts written out,
#' every other character but letters and digits replaced by "_", e.g.
#' "Wärmepumpe" -> "waermepumpe" (as in the folder names of the policy brief)
file_name <- function(text) {
  text <- tolower(text)
  text <- gsub("ä", "ae", text, fixed = TRUE)
  text <- gsub("ö", "oe", text, fixed = TRUE)
  text <- gsub("ü", "ue", text, fixed = TRUE)
  text <- gsub("ß", "ss", text, fixed = TRUE)
  # [^a-z0-9]+: one or more characters that are neither letters nor digits
  text <- gsub("[^a-z0-9]+", "_", text)
  # no "_" at the start or the end
  gsub("^_|_$", "", text)
}

#' Folder of one run, one per arm and reference outcome as in the policy
#' brief, e.g. rc_logit_fw_ref_waermepumpe
run_folder <- function(arm, reference_outcome) {
  paste0("rc_logit_", arm, "_ref_", file_name(reference_outcome))
}

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

# 2. The variables of the models -----------------------------------------------
# Used by 01_run_specifications.R for the decisions and by
# 01_run_term_effects.R for the vignettes of its changes, so that both build
# every variable the same way.

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

# 3. Estimation ----------------------------------------------------------------

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

#' The model with its fitted values, gradient and log-likelihood at its
#' coefficients
#'
#' When mlogit() stops with convergence code 2 (successive log-likelihoods
#' within tolerance), it returns the coefficients of the last iterate but the
#' gradient and log-likelihood of the trial point one small step further. The
#' meat of the sandwich (vcov_cluster_robust()) would then sit at the wrong
#' point, and its gradient check would fail. Re-evaluating the model at its
#' coefficients with zero iterations makes all of them belong together again;
#' the convergence information of the estimation is kept.
#'
#' @param model       fitted mlogit model
#' @param choice_data the data it was estimated on (the model's call names it)
#' @return the model, re-evaluated if its convergence code is 2
reevaluate_at_coefficients <- function(model, choice_data) {
  if (model$est.stat$code != 2) {
    model
  } else {
    # starting values for every parameter, also those fixed by constPar
    # (setNames(): the values, with these names)
    start <- setNames(as.numeric(model$coefficients), names(model$coefficients))
    model_at_coefficients <- update(model, start = start, iterlim = 0,
                                    print.level = 0)
    model_at_coefficients$est.stat <- model$est.stat
    model_at_coefficients
  }
}

#' Estimate one model: a mixed logit whose constants are random, normal and
#' correlated
#'
#' mlogit() is called with do.call("mlogit", arguments), which stores the
#' values of the arguments, not their names, in the model's call. mlogit()
#' looks up some arguments, constPar among them, in the global environment
#' instead of where it is called, so inside this function they would
#' otherwise not be found. Only the data enter by name (quote(choice_data)),
#' so that printing a model does not print the data; vcov_cluster_robust()
#' re-evaluates the model with the data passed to it.
#'
#' Halton draws: mlogit gives the k-th random coefficient the k-th prime (2,
#' 3, 5, ...). With constants by group, the primes start again at 2 for each
#' group: an expert's utility contains only their own group's constants, and
#' the high primes the last group would otherwise get cover the unit cube
#' unevenly (Train 2009, section 9.3.3). Each group's constants then use the
#' same draws as the constants of the common model, which the starting values
#' rely on.
#'
#' @param specification list, see section 3 of 01_run_specifications.R
#' @param choice_data   dfidx data, one row per decision x outcome
#' @param outcomes      all outcomes, the reference outcome first
#' @param n_draws       Halton draws per expert
#' @param start_model   the model to take the starting values from: for
#'                      constants by group, the common model
#'                      (start_from_common()); otherwise a model with the same
#'                      constants that this one nests (start_from_nested());
#'                      NULL: mlogit's default
#' @return list: model, formula, random_coefficients (name, column and
#'   technology of each random constant, in mlogit's order), fixed_parameters
#'   (the elements of L fixed at 0; NULL if none), log_likelihood_start (at
#'   the starting values; NA without start_model)
estimate_mixed_logit <- function(specification, choice_data, outcomes, n_draws,
                                 start_model = NULL) {
  technologies <- outcomes[-1]
  constants <- specification$constants
  has_groups <- !identical(constants, "(Intercept)")

  # (a) Formula: the prices before `|`, each with one coefficient; the
  #     constants and regressors after it, each with one coefficient per
  #     technology. "0 + is_shk + ..." gives one constant per technology and
  #     group.
  price_part <- "0"
  if (length(specification$prices) > 0) {
    price_part <- paste(specification$prices, collapse = " + ")
  }
  if (has_groups) {
    constant_part <- c("0", constants)
  } else {
    constant_part <- "1"
  }
  formula <- as.formula(paste(
    "chosen ~", price_part, "|",
    paste(c(constant_part, specification$regressors), collapse = " + ")
  ))

  # (b) The random coefficients: one constant per technology (and group),
  #     named as mlogit names them, e.g. "is_shk:Gasheizung"
  random <- tibble()
  for (constant in constants) {
    random <- bind_rows(random, tibble(
      name = paste0(constant, ":", technologies),
      column = constant,
      technology = technologies
    ))
  }
  random_parameters <- rep("n", nrow(random))
  names(random_parameters) <- random$name

  arguments <- list(
    formula = formula,
    data = quote(choice_data),
    reflevel = outcomes[1],
    rpar = random_parameters,
    panel = TRUE,
    R = n_draws,
    halton = NA,
    correlation = TRUE
  )

  # (c) mlogit's names of all parameters, in its order: a run with few draws
  #     and no iterations. The random coefficients in this order are the rows
  #     and columns of L.
  names_arguments <- arguments
  names_arguments$R <- 10
  names_arguments$iterlim <- 0
  names_arguments$print.level <- 0
  parameter_names <- names(coef(do.call("mlogit", names_arguments)))
  # match(): the position of each name among parameter_names, NA if not there
  random <- random %>%
    mutate(position = match(name, parameter_names)) %>%
    arrange(position)
  if (anyNA(random$position)) {
    stop("Random constants that are not among mlogit's parameters")
  }
  random <- random %>%
    select(-position)
  # rpar in this order as well. mlogit's likelihood orders the random
  # coefficients as their coefficients, whatever the order of rpar, but
  # cov.mlogit() and stdev() label them in the order of rpar (make.rpar() in
  # mlogit 2.0.0), so another order would mislabel them.
  random_parameters <- rep("n", nrow(random))
  names(random_parameters) <- random$name
  arguments$rpar <- random_parameters

  # (d) Constants by group: the elements of L between two groups fixed at 0
  #     (mlogit names L[i, j], i >= j, "chol.<coefficient j>:<coefficient
  #     i>") and the Halton primes by group
  fixed_parameters <- NULL
  if (has_groups) {
    fixed_names <- character(0)
    for (i in seq_len(nrow(random))) {
      for (j in seq_len(i - 1)) {
        if (random$column[i] != random$column[j]) {
          fixed_names <- c(fixed_names,
                           paste0("chol.", random$name[j], ":", random$name[i]))
        }
      }
    }
    if (!all(fixed_names %in% parameter_names)) {
      stop("The elements of L fixed at 0 are not mlogit's")
    }
    fixed_parameters <- rep(0, length(fixed_names))
    names(fixed_parameters) <- fixed_names
    arguments$constPar <- fixed_parameters

    primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
    halton_primes <- rep(NA_real_, nrow(random))
    for (constant in constants) {
      is_group <- random$column == constant
      halton_primes[is_group] <- primes[seq_len(sum(is_group))]
    }
    arguments$halton <- list(prime = halton_primes)
  }

  # (e) Starting values from start_model: with constants by group, from the
  #     common model; otherwise from a model that this one nests
  log_likelihood_start <- NA_real_
  if (!is.null(start_model)) {
    if (has_groups) {
      arguments$start <- start_from_common(parameter_names, coef(start_model),
                                           constants, technologies,
                                           names(fixed_parameters))
    } else {
      arguments$start <- start_from_nested(parameter_names, coef(start_model))
    }
    # Check: at the starting values, the model has the log-likelihood of
    # start_model (same draws for every expert). If not, start_model was
    # estimated on other data or with other draws, e.g. in an earlier run
    # with other settings.
    start_arguments <- arguments
    start_arguments$iterlim <- 0
    start_arguments$print.level <- 0
    m_start <- do.call("mlogit", start_arguments)
    log_likelihood_start <- as.numeric(logLik(m_start))
    log_likelihood_from <- as.numeric(logLik(start_model))
    cat("Log-likelihood at the starting values:",
        round(log_likelihood_start, 3), "| model it starts from:",
        round(log_likelihood_from, 3), "\n")
    if (abs(log_likelihood_start - log_likelihood_from) > 1e-6) {
      stop("At the starting values, the log-likelihood differs from that of ",
           "the model it starts from: estimate that model again in this run")
    }
  }

  # (f) Estimate
  model <- do.call("mlogit", arguments)

  # Code 3: the line search found no higher log-likelihood along the
  # direction from mlogit's approximation of the inverse Hessian (BFGS), which
  # can be poor near the maximum. A restart from the last iterate, with the
  # same draws, builds it anew. At most three restarts; check_convergence()
  # stops if the code is still not 1 or 2.
  for (restart in 1:3) {
    if (model$est.stat$code != 3) break
    message("Code 3 after ", model$est.stat$nb.iter, " iterations, ",
            "log-likelihood ", round(as.numeric(logLik(model)), 2),
            "; restart ", restart, " from the last iterate")
    # starting values for every parameter, also those fixed by constPar
    start <- setNames(as.numeric(model$coefficients), names(model$coefficients))
    model <- update(model, start = start)
  }
  model <- reevaluate_at_coefficients(model, choice_data)

  list(model = model, formula = formula, random_coefficients = random,
       fixed_parameters = fixed_parameters,
       log_likelihood_start = log_likelihood_start)
}

#' Stop if the model has not converged or has not used every choice situation
#'
#' mlogit's convergence codes: 1 gradient close to zero, 2 successive values
#' of the log-likelihood within tolerance, 3 the last step could not find a
#' higher value, 4 iteration limit exceeded. The largest element of the
#' gradient is printed as well; at a maximum it is close to zero. A code of 1
#' or 2 does not prove a maximum: compare the log-likelihood with that of a
#' model this one nests (02_output.R marks a negative LR statistic).
#'
#' @param model     fitted mlogit model
#' @param n_choices number of choice situations in the estimation data
#' @return list, saved with the results: code, iterations, max_gradient
#'   (largest absolute element of the gradient)
check_convergence <- function(model, n_choices) {
  code <- model$est.stat$code
  gradient <- colSums(model$gradient[, names(coef(model)), drop = FALSE])
  cat("Convergence code", code, "| iterations", model$est.stat$nb.iter,
      "| largest gradient element", signif(max(abs(gradient)), 3),
      "| log-likelihood", round(as.numeric(logLik(model)), 3), "\n")
  if (!(code %in% c(1, 2))) {
    stop("The model has not converged (code ", code, ")")
  }
  if (length(fitted(model)) != n_choices) {
    stop("The model does not use all ", n_choices, " choice situations")
  }
  list(code = code, iterations = model$est.stat$nb.iter,
       max_gradient = max(abs(gradient)))
}

# 4. Covariance matrix clustered by expert -------------------------------------

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

# 5. Probabilities, AMEs and the random constants ------------------------------

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

#' Standard deviations and correlations of the random constants implied by a
#' parameter vector:
#'
#'   Omega = L L',   sd_j = sqrt(Omega_jj),   cor_jk = Omega_jk / (sd_j sd_k)
#'
#' Each correlation once, and only within a set of constants: with constants
#' by group, those between two groups are 0 by construction, not estimated.
#'
#' @return named vector: "sd.<constant>" for each random constant, then
#'   "cor.<constant j>:<constant k>" for each pair
random_constant_values <- function(theta, random_coefficients,
                                   fixed_parameters = NULL) {
  rpar_names <- random_coefficients$name
  L <- cholesky_factor(theta, random_coefficients, fixed_parameters)
  Omega <- L %*% t(L)
  sds <- sqrt(diag(Omega))
  values <- setNames(sds, paste0("sd.", rpar_names))
  for (i in seq_along(rpar_names)) {
    for (j in seq_len(i - 1)) {
      if (random_coefficients$column[i] == random_coefficients$column[j]) {
        name <- paste0("cor.", rpar_names[j], ":", rpar_names[i])
        values[name] <- Omega[i, j] / (sds[i] * sds[j])
      }
    }
  }
  values
}

#' Standard deviations and correlations of the random constants with
#' Krinsky-Robb standard errors and percentile intervals, from the same draws
#' of theta as the AMEs
#'
#' @param theta_hat, theta_draws as in ame_krinsky_robb()
#' @return tibble: parameter, type ("sd" or "cor"), estimate, se, ci_low_kr,
#'   ci_high_kr
random_constants_table <- function(theta_hat, theta_draws, random_coefficients,
                                   fixed_parameters = NULL) {
  estimate <- random_constant_values(theta_hat, random_coefficients,
                                     fixed_parameters)
  draws <- matrix(NA_real_, nrow = nrow(theta_draws), ncol = length(estimate),
                  dimnames = list(NULL, names(estimate)))
  for (s in seq_len(nrow(theta_draws))) {
    draws[s, ] <- random_constant_values(theta_draws[s, ], random_coefficients,
                                         fixed_parameters)
  }
  tibble(
    parameter = names(estimate),
    type = if_else(startsWith(names(estimate), "sd."), "sd", "cor"),
    estimate = as.numeric(estimate),
    # apply(draws, 2, f): f applied to each column, i.e. over the draws
    se = apply(draws, 2, sd),
    ci_low_kr = apply(draws, 2, quantile, probs = 0.025),
    ci_high_kr = apply(draws, 2, quantile, probs = 0.975)
  )
}

# 6. Fit -----------------------------------------------------------------------

#' Fit statistics of one model (section 8.2 of the policy brief script)
#'
#' McFadden R^2 compares the model with one that has constants only, i.e.
#' predicts the observed share of each recommendation for every vignette:
#'   logL_0 = sum_j n_j log(n_j / N),   R^2 = 1 - logL / logL_0.
#' AIC = -2 logL + 2 parameters, BIC = -2 logL + log(experts) parameters, to
#' compare models that are not nested. BIC counts experts, not choice
#' situations, because the panel likelihood is a product over experts.
#'
#' @param model         fitted mlogit model
#' @param choice_counts table of the recommendations in the estimation sample
#' @param n_experts     number of experts in the estimation sample
#' @param n_draws       Halton draws per expert
#' @return tibble, one row
fit_statistics <- function(model, choice_counts, n_experts, n_draws) {
  n_choices <- sum(choice_counts)
  log_likelihood <- as.numeric(logLik(model))
  log_likelihood_constants_only <- sum(choice_counts *
                                         log(choice_counts / n_choices))
  n_parameters <- length(coef(model))
  tibble(
    n_choices = n_choices,
    n_experts = n_experts,
    n_parameters = n_parameters,
    n_draws_per_expert = n_draws,
    log_likelihood = log_likelihood,
    log_likelihood_constants_only = log_likelihood_constants_only,
    mcfadden_r2 = 1 - log_likelihood / log_likelihood_constants_only,
    aic = -2 * log_likelihood + 2 * n_parameters,
    bic = -2 * log_likelihood + log(n_experts) * n_parameters
  )
}
