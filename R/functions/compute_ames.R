################################################################################
# compute_ames.R — the effects of the attributes and of any covariate on the
#                  probabilities of the recommendations, at a reference state
#                  set per call, with Krinsky-Robb standard errors
# Inputs : the results of a model (post_estimation()) and the make_regressors()
#          of the estimation script
# Outputs: results$ames[[name]]; results.rds and results.xlsx of the model
# Checks : one value per variable in `at`; a change sets known variables; every
#          coefficient has a column; one row per state x alternative; the
#          probabilities are finite at every draw
################################################################################
# The model is that of simulated_likelihood.R. For an expert drawn at random,
# the probability of outcome j in state v (a vignette and an expert's
# characteristics, in the source variables of make_regressors()) is, over R
# Halton draws z_r:
#
#   P_vj = 1/R sum_r exp(u_vjr) / sum_k exp(u_vkr)
#   u_vjr = x_vj' beta + d_vj (L z_r)_j
#
# x_vj holds the regressors of outcome j in state v; d_vj is the column of j's
# random constant: 1 for "(Intercept)", the group dummy for a constant by group.
# An effect is P(to) - P(from). Krinsky-Robb (Krinsky and Robb 1986) passes the
# uncertainty in theta through it: the effect at every draw of N(theta_hat, V),
# with the same z_r throughout, so that the spread reflects only the uncertainty
# in theta. The standard deviation over the draws is the standard error, their
# 2.5% and 97.5% quantiles a percentile interval. If the interval ame +- 1.96 se
# and the percentile interval differ markedly, the effect is skewed and the
# percentile interval is the better one to report. The point estimate stays the
# effect at theta_hat.

#' The change in the probability of every recommendation between two states, in
#' every profile, with Krinsky-Robb standard errors. Loads the saved effects if
#' nothing changed.
#'
#' @param results         the results of a model, from post_estimation()
#' @param make_regressors the function of the estimation script, the same that
#'                        built the data, so that no variable is coded twice:
#'                        from one row per state with the source variables to
#'                        one row per state x alternative, with the column
#'                        alternative. It keeps the columns it is given and
#'                        does not use vig_rec.
#' @param name            the name of this set of effects, at most 22 characters:
#'                        the sheets ame_<name>, baseline_<name>, pp_<name>. A
#'                        name no longer used stays in the results until
#'                        post_estimation(force = TRUE) starts them anew.
#' @param at              the reference state in the source variables of
#'                        make_regressors(): a named list with one value each
#' @param changes         named list, one element per effect. Each sets `to` and
#'                        optionally `from` (default: `at`), named lists of
#'                        source variables, each on top of `at`. Several
#'                        variables may change together. The effect is P(to) -
#'                        P(from), for every outcome.
#' @param profiles        a character vector sets expert_group; a named list of
#'                        lists sets any other column, e.g. list("All experts" =
#'                        list(expert_group = "Energy advisers"))
#' @param n_draws         Halton draws over the random constants
#' @param n_workers       R sessions for the Krinsky-Robb loop
#' @param force           TRUE: compute even if nothing changed
#' @return results with results$ames[[name]] (docs/results_contract.md); saves
#'   results.rds and rewrites results.xlsx
compute_ames <- function(results, make_regressors, name, at, changes,
                         profiles = EXPERT_GROUPS,
                         n_draws = N_DRAWS_PREDICTION, n_workers = N_WORKERS,
                         force = FALSE) {
  start_time <- Sys.time()
  if (nchar(name) > 22) {
    stop("The name ", name, " makes a sheet name of more than 31 characters, ",
         "Excel's limit: use at most 22")
  }
  # The effects of earlier calls come from the file, in case the script did not
  # keep what an earlier call returned: saving would otherwise lose them
  results_file <- file.path(results$folder, "results.rds")
  if (file.exists(results_file)) {
    saved_results <- readRDS(results_file)
    if (identical(saved_results$fingerprint, results$fingerprint)) {
      results$ames <- saved_results$ames
    }
  }
  outcomes <- results$outcomes
  n_outcomes <- length(outcomes)
  random <- results$random
  parts <- coefficient_parts(results)

  # (a) The states of one profile: the baseline and both ends of every change.
  # A misspelled from, to or variable would silently give an effect of 0, and a
  # second value in `at` would be ignored. lengths(): the length of each element.
  # anyDuplicated(): the position of the first repeated name, 0 if none.
  if (any(lengths(at) != 1)) {
    stop("`at` must hold one value per variable")
  }
  profile_settings <- profiles
  if (is.character(profiles)) {
    profile_settings <- list()
    for (group in profiles) {
      profile_settings[[group]] <- list(expert_group = group)
    }
  }
  if (length(changes) > 0 &&
      (is.null(names(changes)) || any(names(changes) == "") ||
       anyDuplicated(names(changes)) > 0)) {
    stop("changes must be a list with a different name for each effect")
  }
  known_columns <- names(at)
  for (profile_name in names(profile_settings)) {
    known_columns <- c(known_columns, names(profile_settings[[profile_name]]))
  }
  state_settings <- list(baseline = list())
  for (effect in names(changes)) {
    change <- changes[[effect]]
    not_allowed <- setdiff(names(change), c("from", "to"))
    if (is.null(change[["to"]]) || length(not_allowed) > 0) {
      stop("The change ", effect, " must set `to`, optionally `from`, and ",
           "nothing else")
    }
    unknown_columns <- setdiff(c(names(change[["from"]]), names(change[["to"]])),
                               known_columns)
    if (length(unknown_columns) > 0) {
      stop("The change ", effect, " sets variables that are not in `at` or the ",
           "profiles: ", paste(unknown_columns, collapse = ", "))
    }
    state_settings[[paste0(effect, "|from")]] <- as.list(change[["from"]])
    state_settings[[paste0(effect, "|to")]] <- change[["to"]]
  }

  # (b) One row per profile x state: `at`, then the profile's columns, then the
  # change's, so that a change can also move the group. The states go through
  # make_regressors(), then into one design matrix over all profiles.
  # row_number(): the position of each row.
  state_rows <- list()
  for (profile_name in names(profile_settings)) {
    for (state_name in names(state_settings)) {
      # as_tibble(): the list as a table with one row
      state <- as_tibble(at)
      # A column in both, as the group in a change, takes the last value
      settings <- c(profile_settings[[profile_name]],
                    state_settings[[state_name]])
      for (k in seq_along(settings)) {
        state[[names(settings)[k]]] <- settings[[k]]
      }
      state$ame_profile <- profile_name
      state$ame_state <- state_name
      state_rows[[length(state_rows) + 1]] <- state
    }
  }
  states <- bind_rows(state_rows) %>%
    mutate(ame_row = row_number())
  n_states <- nrow(states)
  # match(): the position of each alternative among the outcomes
  regressors <- make_regressors(states) %>%
    mutate(outcome_position = match(as.character(alternative), outcomes)) %>%
    arrange(ame_row, outcome_position)
  if (!identical(regressors$ame_row, rep(states$ame_row, each = n_outcomes)) ||
      !identical(regressors$outcome_position,
                 rep(seq_len(n_outcomes), n_states))) {
    stop("make_regressors() must return one row per state x alternative, with ",
         "the outcomes of the model: ", paste(outcomes, collapse = ", "))
  }

  # (c) X_long: rows state x outcome (sorted by state, then outcome), one column
  # per coefficient that is not an element of L. The name of a coefficient says
  # how its column enters: <column>:<technology> in the rows of that technology,
  # <column> in the rows of all outcomes.
  terms <- parts$terms
  no_column <- setdiff(terms$column, c(names(regressors), "(Intercept)"))
  if (length(no_column) > 0) {
    stop("Coefficients without a column in make_regressors(): ",
         paste(no_column, collapse = ", "),
         ". A factor in a formula? Make its dummies in make_regressors().")
  }
  X_long <- matrix(0, nrow(regressors), nrow(terms),
                   dimnames = list(NULL, terms$parameter))
  for (k in seq_len(nrow(terms))) {
    values <- 1
    if (terms$column[k] != "(Intercept)") {
      values <- regressors[[terms$column[k]]]
    }
    if (is.na(terms$technology[k])) {
      X_long[, k] <- values
    } else {
      X_long[, k] <- values * (regressors$alternative == terms$technology[k])
    }
  }
  if (anyNA(X_long)) {
    stop("Missing values in the regressors of the states: ",
         paste(colnames(X_long)[colSums(is.na(X_long)) > 0], collapse = ", "))
  }

  # (d) The column of each random constant in each state. States with the same
  # columns share their random part: with one set of constants all of them, with
  # constants by group those of one group. apply(m, 1, f): f applied to each row.
  random_value <- matrix(0, n_states, nrow(random))
  for (k in seq_len(nrow(random))) {
    position <- match(random$technology[k], outcomes)
    rows <- seq(position, by = n_outcomes, length.out = n_states)
    random_value[, k] <- X_long[rows, random$name[k]]
  }
  columns_of_state <- apply(random_value, 1, paste, collapse = " ")
  state_groups <- list()
  for (rows in split(seq_len(n_states), columns_of_state)) {
    state_groups[[length(state_groups) + 1]] <- list(
      states = rows,
      values = random_value[rows[1], ]
    )
  }

  # (e) Nothing changed? hash(): a short code that changes whenever its object
  # does. The design matrix, not the function, enters it: make_regressors() may
  # change, in a variable that another model uses, without changing what this
  # model predicts.
  fingerprint <- rlang::hash(list(results$fingerprint, at, changes, profiles,
                                  n_draws, X_long, random_value))
  if (!force && identical(results$ames[[name]]$fingerprint, fingerprint)) {
    message("Model ", results$name, ": effects ", name,
            " loaded, nothing changed")
    refresh_workbook(results)
    return(results)
  }

  # (f) The prediction draws: one row per draw, one column per random constant.
  # The primes start again at 2 for each group, as in fit_mixed_logit().
  # mlogit:::halton(): a Halton sequence, here without its first 100 elements.
  # qnorm(): from uniform to standard normal.
  Z <- matrix(NA_real_, n_draws, nrow(random))
  for (constant in unique(random$column)) {
    of_constant <- which(random$column == constant)
    for (position in seq_along(of_constant)) {
      uniform_draws <- mlogit:::halton(HALTON_PRIMES[position], n_draws, 100)
      Z[, of_constant[position]] <- qnorm(uniform_draws)
    }
  }

  # (g) The probabilities at the estimates, and at every Krinsky-Robb draw on
  # n_workers R sessions, in one chunk of draws per session. --vanilla: each
  # session is ready at once, and state_probabilities() needs only base R.
  # on.exit(): runs when this function ends, also with an error.
  prediction <- list(
    X_long = X_long,
    Z = Z,
    state_groups = state_groups,
    n_states = n_states,
    n_outcomes = n_outcomes,
    constant_outcome = match(random$technology, outcomes),
    beta_names = terms$parameter,
    cholesky_names = parts$cholesky$parameter,
    cholesky_i = parts$cholesky$i,
    cholesky_j = parts$cholesky$j
  )
  # t(): the vector as a matrix of one row
  estimates <- matrix(state_probabilities(t(results$coefficients), prediction),
                      n_states, n_outcomes)

  n_draws_kr <- nrow(results$theta_draws)
  chunk_of_draw <- ceiling(seq_len(n_draws_kr) * n_workers / n_draws_kr)
  draws_of_chunk <- split(seq_len(n_draws_kr), chunk_of_draw)
  chunks <- list()
  for (w in seq_along(draws_of_chunk)) {
    chunks[[w]] <- results$theta_draws[draws_of_chunk[[w]], , drop = FALSE]
  }
  cluster <- parallel::makeCluster(n_workers, rscript_args = "--vanilla")
  on.exit(parallel::stopCluster(cluster))
  # parLapply(): lapply on the sessions
  chunk_probabilities <- parallel::parLapply(cluster, chunks,
                                             state_probabilities,
                                             prediction = prediction)
  all_draws <- array(NA_real_, dim = c(n_states, n_outcomes, n_draws_kr))
  for (w in seq_along(draws_of_chunk)) {
    all_draws[, , draws_of_chunk[[w]]] <- chunk_probabilities[[w]]
  }

  # (h) The tables: the effects and the probabilities at `at`, per profile. The
  # first element of table_rows: without changes, the table keeps its columns.
  z_critical <- qnorm(0.975)
  table_rows <- list(tibble(
    profile = character(0), effect = character(0), outcome = character(0),
    p_from = numeric(0), p_to = numeric(0), ame = numeric(0), se = numeric(0),
    z = numeric(0), p = numeric(0), ci_low = numeric(0), ci_high = numeric(0),
    ci_low_kr = numeric(0), ci_high_kr = numeric(0)
  ))
  baseline_rows <- list()
  draws <- list()
  for (profile_name in names(profile_settings)) {
    rows <- which(states$ame_profile == profile_name)
    estimate <- estimates[rows, , drop = FALSE]
    dimnames(estimate) <- list(states$ame_state[rows], outcomes)
    profile_draws <- all_draws[rows, , , drop = FALSE]
    dimnames(profile_draws) <- list(states$ame_state[rows], outcomes, NULL)
    draws[[profile_name]] <- profile_draws
    for (outcome in outcomes) {
      baseline_draws <- profile_draws["baseline", outcome, ]
      baseline_rows[[length(baseline_rows) + 1]] <- tibble(
        profile = profile_name,
        outcome = outcome,
        probability = estimate["baseline", outcome],
        se = sd(baseline_draws),
        ci_low_kr = quantile(baseline_draws, 0.025, names = FALSE),
        ci_high_kr = quantile(baseline_draws, 0.975, names = FALSE)
      )
    }
    for (effect in names(changes)) {
      state_from <- paste0(effect, "|from")
      state_to <- paste0(effect, "|to")
      for (outcome in outcomes) {
        effect_draws <- profile_draws[state_to, outcome, ] -
          profile_draws[state_from, outcome, ]
        table_rows[[length(table_rows) + 1]] <- tibble(
          profile = profile_name,
          effect = effect,
          outcome = outcome,
          p_from = estimate[state_from, outcome],
          p_to = estimate[state_to, outcome],
          ame = p_to - p_from,
          se = sd(effect_draws),
          z = ame / se,
          p = 2 * pnorm(-abs(z)),
          ci_low = ame - z_critical * se,
          ci_high = ame + z_critical * se,
          ci_low_kr = quantile(effect_draws, 0.025, names = FALSE),
          ci_high_kr = quantile(effect_draws, 0.975, names = FALSE)
        )
      }
    }
  }
  effects_table <- bind_rows(table_rows)

  # A change of a variable the model does not use gives exactly 0, which would
  # read as a finding about that variable
  moving_effects <- unique(effects_table$effect[effects_table$se > 0])
  not_moving <- setdiff(names(changes), moving_effects)
  if (length(not_moving) > 0) {
    message("Model ", results$name, ": effects ", name, ": these changes move ",
            "nothing in this model: ", paste(not_moving, collapse = ", "))
  }

  # (i) Save
  results$ames[[name]] <- list(
    table = effects_table,
    baseline = bind_rows(baseline_rows),
    draws = draws,
    at = at,
    changes = changes,
    profiles = profiles,
    n_draws = n_draws,
    fingerprint = fingerprint,
    date = Sys.Date()
  )
  saveRDS(results, results_file)
  write_results_excel(results)
  seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  message("Model ", results$name, ": effects ", name, ", ", n_states,
          " states, ", n_draws_kr, " Krinsky-Robb draws on ", n_workers,
          " R sessions, ", round(seconds), " seconds")
  results
}

#' The probabilities of the recommendations in every state, at each row of
#' theta_rows. Only base R: it runs on --vanilla sessions.
#'
#' The utility of outcome j in state v at draw r is a_vj + c_rj, with the mean
#' part a_vj = x_vj' beta and the random part c_rj = d_j (L z_r)_j, the same in
#' all states of a group of states; the reference outcome's random part is 0.
#' Every utility is reduced by the largest mean part of its state, A_v, and the
#' largest random part of its draw, B_r (at least 0), which cancels in the ratio
#' and keeps every exp() at or below 1, also at an extreme Krinsky-Robb draw.
#' With w_vj = exp(a_vj - A_v) and e_rj = exp(c_rj - B_r),
#'   P_vj = w_vj * 1/R * sum_r e_rj / D_rv,   D_rv = sum_k w_vk e_rk,
#' so every state's probabilities come from matrix products, and exp() runs once
#' per draw instead of once per draw and state.
#'
#' @param theta_rows matrix, one row per parameter vector, columns named as
#'                   coef()
#' @param prediction list made in compute_ames(), section (g)
#' @return array: states x outcomes x rows of theta_rows
state_probabilities <- function(theta_rows, prediction) {
  Z <- prediction$Z
  n_states <- prediction$n_states
  n_outcomes <- prediction$n_outcomes
  n_draws <- nrow(Z)
  L <- matrix(0, ncol(Z), ncol(Z))
  cholesky_positions <- cbind(prediction$cholesky_i, prediction$cholesky_j)
  # array(): a matrix with a third dimension
  probabilities <- array(NA_real_,
                         dim = c(n_states, n_outcomes, nrow(theta_rows)))
  for (s in seq_len(nrow(theta_rows))) {
    theta <- theta_rows[s, ]

    # (a) The mean parts: outcomes x states, X_long being sorted by state, then
    # outcome. Each column minus its state's largest. pmax(): the larger value,
    # element by element.
    mean_utility <- matrix(prediction$X_long %*% theta[prediction$beta_names],
                           n_outcomes, n_states)
    largest_mean <- mean_utility[1, ]
    for (j in seq_len(n_outcomes)) {
      largest_mean <- pmax(largest_mean, mean_utility[j, ])
    }
    weights <- exp(mean_utility - rep(largest_mean, each = n_outcomes))

    # (b) The deviations of the constants: draws x constants, row r is (L z_r)'
    L[cholesky_positions] <- theta[prediction$cholesky_names]
    deviations <- Z %*% t(L)

    # (c) The states that share a random part, usually all of them
    for (state_group in prediction$state_groups) {
      values <- state_group$values
      random_utility <- matrix(0, n_draws, n_outcomes)
      for (k in which(values != 0)) {
        j <- prediction$constant_outcome[k]
        random_utility[, j] <- random_utility[, j] + values[k] * deviations[, k]
      }
      largest_random <- rep(0, n_draws)
      for (j in seq_len(n_outcomes)) {
        largest_random <- pmax(largest_random, random_utility[, j])
      }
      exp_random <- exp(random_utility - largest_random)
      # denominators: draws x states, D_rv. crossprod(a, b) is t(a) %*% b, here
      # outcomes x states: the sums over the draws of e_rj / D_rv.
      weights_group <- weights[, state_group$states, drop = FALSE]
      denominators <- exp_random %*% weights_group
      sums_over_draws <- crossprod(exp_random, 1 / denominators)
      probabilities[state_group$states, , s] <-
        t(weights_group * sums_over_draws / n_draws)
    }
  }
  # A denominator of 0 (every term below what exp() can represent) would give Inf
  # or NaN; checked at every evaluation, the Krinsky-Robb draws included
  if (any(!is.finite(probabilities))) {
    stop("A probability cannot be computed: the utilities of a state differ by ",
         "more than exp() can represent (at a Krinsky-Robb draw, a coefficient ",
         "can be extreme)")
  }
  probabilities
}

#' How the coefficients of a model enter the utility, read from their names
#'
#' @param results the results of a model
#' @return list:
#'   terms     one row per coefficient that is not an element of L: parameter,
#'             column, technology (NA: one coefficient for all technologies)
#'   cholesky  one row per element of L among the coefficients, those fixed at 0
#'             being none: parameter, i, j, its row and column in L. mlogit names
#'             L[i, j], i >= j, "chol.<coefficient j>:<coefficient i>".
coefficient_parts <- function(results) {
  parameters <- names(results$coefficients)
  random_names <- results$random$name
  technologies <- setdiff(results$outcomes, results$reference)
  cholesky <- tibble()
  for (i in seq_along(random_names)) {
    for (j in seq_len(i)) {
      parameter <- paste0("chol.", random_names[j], ":", random_names[i])
      if (parameter %in% parameters) {
        cholesky <- bind_rows(cholesky,
                              tibble(parameter = parameter, i = i, j = j))
      }
    }
  }
  terms <- tibble()
  for (parameter in setdiff(parameters, cholesky$parameter)) {
    # The part after the last ":". The formula holds plain variable names, so
    # only a technology can stand there. str_replace(): replaces what the pattern
    # matches. substr(): the characters from one position to another.
    last_part <- str_replace(parameter, "^.*:", "")
    if (last_part %in% technologies) {
      column <- substr(parameter, 1, nchar(parameter) - nchar(last_part) - 1)
      terms <- bind_rows(terms, tibble(parameter = parameter, column = column,
                                       technology = last_part))
    } else {
      terms <- bind_rows(terms, tibble(parameter = parameter, column = parameter,
                                       technology = NA_character_))
    }
  }
  list(terms = terms, cholesky = cholesky)
}
