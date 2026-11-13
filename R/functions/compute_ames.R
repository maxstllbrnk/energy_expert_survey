################################################################################
# compute_ames.R — the effects of the attributes and of any covariate on the
#                  probabilities of the recommendations: for a reference expert
#                  or averaged over the observed experts, with Krinsky-Robb
#                  standard errors
# Inputs : the results of a model (post_estimation()), the make_regressors() of
#          the estimation script, and for effects over observed experts their
#          source variables (`population`)
# Outputs: results$ames[[name]]; results.rds and results.xlsx of the model
# Checks : one value per variable in `at`; a change sets known variables; the
#          population is the model's estimation sample; every coefficient has a
#          column; one row per state x alternative; the probabilities are finite
#          at every draw
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
#
# A unit is what the probabilities are computed for. Its state is its row in
# the source variables, with the change on top. The probability of a profile in
# a state is the average of P_vj over the profile's units, and an effect is
# P(to) - P(from) of these averages. The same function gives four kinds of
# effect, according to what the units are (docs/results_contract.md):
#   A. population = NULL: one fictitious expert per profile, described fully by
#      `at` and the profile's columns. The effect at a reference expert.
#   B. population = the experts, at = the reference vignette: every expert keeps
#      their own characteristics, and the change is made to each of them. The
#      average marginal effect over the observed experts, as if all of them had
#      seen the reference vignette.
#   C. population = the answers (one row per expert x vignette), at = list():
#      every expert keeps their characteristics and every vignette that they
#      answered. The effect over the vignettes of the design.
#   D. as B with another `at`: the effect for another household.
# Krinsky-Robb (Krinsky and Robb 1986) passes the uncertainty in theta through
# it: the effect at every draw of N(theta_hat, V), with the same z_r throughout,
# so that the spread reflects only the uncertainty in theta, given the units. The
# standard deviation over the draws is the standard error, their 2.5% and 97.5%
# quantiles a percentile interval. If the interval ame +- 1.96 se and the
# percentile interval differ markedly, the effect is skewed and the percentile
# interval is the better one to report. The point estimate stays the effect at
# theta_hat.
#
# With observed units the work grows with their number: the states of a unit
# are its own state and, for every change, one or two more. The rows of the
# design matrix go in blocks of units, the average over a profile's units is
# taken inside the Krinsky-Robb loop, and only these averages are kept, so that
# nothing at the level of experts reaches results.rds.

#' The change in the probability of every recommendation between two states, for
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
#' @param at              source variables, a named list with one value each. Without
#'                        a population the reference state, all of it. With a
#'                        population only what is set for every unit, e.g. the
#'                        reference vignette; the rest is the unit's own.
#' @param changes         named list, one element per effect. Each sets `to` and
#'                        optionally `from`, named lists of source variables, each
#'                        on top of the unit. Without `from`, an effect starts at the
#'                        unit as it is (`at` without a population). Several
#'                        variables may change together. The effect is P(to) -
#'                        P(from), for every outcome. With a population and
#'                        exclude_na, an effect leaves out the units whose own value
#'                        of a variable that it sets is missing, unless it sets
#'                        that variable to NA in `from` or `to`; n_experts of the
#'                        table says how many it averages over.
#' @param profiles        a character vector sets expert_group (without a
#'                        population) or selects it (with one); a named list of
#'                        lists sets (without a population) or selects (with one,
#'                        the units whose value is among the given ones) any other
#'                        column, e.g. list("All experts" = list(), "Advisers with
#'                        a heat pump" = list(expert_group = "Energy advisers",
#'                        heat_pump_at_home = 1))
#' @param population      NULL, or a table with one row per unit: resp_uid and the
#'                        source variables of make_regressors() that `at` does not
#'                        set. Keep only these columns, as every column is copied
#'                        for every state.
#' @param exclude_na      FALSE: no unit is left out of an effect (see `changes`)
#' @param check_sample    TRUE: stop if the experts of the population are not those
#'                        of the model's estimation sample. FALSE for another
#'                        population on purpose.
#' @param n_draws         Halton draws over the random constants
#' @param n_workers       R sessions for the Krinsky-Robb loop
#' @param force           TRUE: compute even if nothing changed
#' @return results with results$ames[[name]] (docs/results_contract.md); saves
#'   results.rds and rewrites results.xlsx
compute_ames <- function(results, make_regressors, name, at, changes,
                         profiles = EXPERT_GROUPS, population = NULL,
                         exclude_na = TRUE, check_sample = TRUE,
                         n_draws = N_DRAWS_PREDICTION,
                         n_workers = N_WORKERS, force = FALSE) {
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
  terms <- parts$terms

  # (a) The units and the profiles they belong to. A misspelled variable would
  # silently give an effect of 0, and a second value in `at` would be ignored.
  # lengths(): the length of each element. anyDuplicated(): the position of the
  # first repeated name, 0 if none.
  if (any(lengths(at) != 1)) {
    stop("`at` must hold one value per variable")
  }
  if (length(changes) > 0 &&
      (is.null(names(changes)) || any(names(changes) == "") ||
       anyDuplicated(names(changes)) > 0)) {
    stop("changes must be a list with a different name for each effect")
  }
  profile_settings <- profiles
  if (is.character(profiles)) {
    profile_settings <- list()
    for (group in profiles) {
      profile_settings[[group]] <- list(expert_group = group)
    }
  }
  n_profiles <- length(profile_settings)

  # is_member: units x profiles. Without a population each profile is one
  # fictitious expert: `at`, then the profile's columns. With one, the units are
  # the rows of the population with `at` set, and a profile selects among them.
  # as_tibble(): the list as a table with one row. %in%: is among.
  if (is.null(population)) {
    unit_rows <- list()
    is_member <- matrix(FALSE, n_profiles, n_profiles)
    for (p in seq_len(n_profiles)) {
      unit <- as_tibble(at)
      settings <- profile_settings[[p]]
      for (k in seq_along(settings)) {
        unit[[names(settings)[k]]] <- settings[[k]]
      }
      unit_rows[[p]] <- unit
      is_member[p, p] <- TRUE
    }
    unit_data <- bind_rows(unit_rows)
  } else {
    unit_data <- as_tibble(population)
    if (nrow(unit_data) == 0 || !"resp_uid" %in% names(unit_data)) {
      stop("`population` needs rows and the column resp_uid")
    }
    if (check_sample &&
        !identical(rlang::hash(sort(unique(as.character(unit_data$resp_uid)))),
                   results$sample_hash)) {
      stop("The experts of the population are not the estimation sample of model ",
           results$name, ": use the experts of the estimation script, or ",
           "check_sample = FALSE for another population on purpose")
    }
    for (variable in names(at)) {
      unit_data[[variable]] <- at[[variable]]
    }
    is_member <- matrix(FALSE, nrow(unit_data), n_profiles)
    for (p in seq_len(n_profiles)) {
      selected <- rep(TRUE, nrow(unit_data))
      settings <- profile_settings[[p]]
      for (variable in names(settings)) {
        if (!variable %in% names(unit_data)) {
          stop("The profile ", names(profile_settings)[p], " selects on ",
               variable, ", which is not in the population or `at`")
        }
        selected <- selected & unit_data[[variable]] %in% settings[[variable]]
      }
      if (!any(selected)) {
        stop("The profile ", names(profile_settings)[p], " selects no unit of the population")
      }
      is_member[, p] <- selected
    }
    # a unit in no profile costs nothing
    in_a_profile <- rowSums(is_member) > 0
    unit_data <- unit_data[in_a_profile, ]
    is_member <- is_member[in_a_profile, , drop = FALSE]
  }
  n_units <- nrow(unit_data)

  # (b) The effects: what they set, and the units they average over
  # setNames(): the values, with these names
  effects <- names(changes)
  has_from <- setNames(logical(length(effects)), effects)
  included <- list()
  for (effect in effects) {
    change <- changes[[effect]]
    not_allowed <- setdiff(names(change), c("from", "to"))
    if (is.null(change[["to"]]) || length(not_allowed) > 0) {
      stop("The change ", effect, " must set `to`, optionally `from`, and ",
           "nothing else")
    }
    unknown_columns <- setdiff(c(names(change[["from"]]), names(change[["to"]])),
                               names(unit_data))
    if (length(unknown_columns) > 0) {
      stop("The change ", effect, " sets variables that are not in `at`, the ",
           "population or the profiles: ", paste(unknown_columns, collapse = ", "))
    }
    has_from[[effect]] <- length(change[["from"]]) > 0
    # NA on one side only would start (or end) every expert at their own answer,
    # and the experts who did not answer would add an effect of 0 to the average
    if (!is.null(population)) {
      for (variable in union(names(change[["from"]]), names(change[["to"]]))) {
        na_in_from <- anyNA(change[["from"]][[variable]])
        na_in_to <- anyNA(change[["to"]][[variable]])
        if ((na_in_from && is.null(change[["to"]][[variable]])) ||
            (na_in_to && is.null(change[["from"]][[variable]]))) {
          stop("The change ", effect, " sets ", variable, " to NA on one side only: ",
               "over a population give the other side a value too (`from` or `to`), ",
               "so that the effect is the same for every expert")
        }
      }
    }
    keep <- rep(TRUE, n_units)
    if (!is.null(population) && exclude_na) {
      for (variable in union(names(change[["from"]]), names(change[["to"]]))) {
        values_set <- c(change[["from"]][[variable]], change[["to"]][[variable]])
        if (!anyNA(values_set)) {
          keep <- keep & !is.na(unit_data[[variable]])
        }
      }
    }
    included[[effect]] <- keep
  }

  # Cells: profile x state, in the order of the draws saved. The states are the
  # baseline (the units as they are) and both ends of every effect; an effect
  # without `from` starts at the baseline of the units it averages over.
  state_labels <- "baseline"
  for (effect in effects) {
    state_labels <- c(state_labels, paste0(effect, "|from"), paste0(effect, "|to"))
  }
  n_labels <- length(state_labels)
  n_cells <- n_profiles * n_labels

  # The rows of the design matrix: a unit's own state, then one or two states
  # per effect that includes it. Size before building, as a long list of effects
  # over many units can fill the memory.
  n_states <- n_units
  for (effect in effects) {
    n_states <- n_states + sum(included[[effect]]) * (1 + has_from[[effect]])
  }
  design_gb <- n_states * n_outcomes * nrow(terms) * 8 / 1e9
  if (!is.null(population)) {
    message("Model ", results$name, ": effects ", name, ": ", n_units, " units, ",
            n_states, " states, design matrix ", round(design_gb, 2), " GB")
  }
  if (design_gb > 8) {
    stop("The design matrix would take ", round(design_gb, 1), " GB: use fewer ",
         "changes or profiles, or a population of fewer units")
  }

  # (c) The design matrix in blocks of units, so that the matrix of draws x
  # states stays small. Per block: X (rows state x outcome, one column per
  # coefficient that is not an element of L; a coefficient named
  # <column>:<technology> enters in the rows of that technology, <column> in the
  # rows of all outcomes), the random part of every state, and which rows go
  # into which cell.
  rows_per_unit <- 1 + sum(1 + has_from)
  states_per_block <- max(50, floor(2e6 / n_draws))
  units_per_block <- max(1, floor(states_per_block / rows_per_unit))
  # split(): the positions, in groups. paste(): joins the values of a row.
  # apply(m, 1, f): f applied to each row.
  unit_blocks <- split(seq_len(n_units), ceiling(seq_len(n_units) / units_per_block))
  blocks <- list()
  block_hashes <- character(0)
  member_unit <- integer(0)
  member_cell <- integer(0)
  for (b in seq_along(unit_blocks)) {
    block_units <- unit_blocks[[b]]
    row_unit <- block_units
    row_state <- rep("own", length(block_units))
    for (effect in effects) {
      in_effect <- block_units[included[[effect]][block_units]]
      if (has_from[[effect]]) {
        row_unit <- c(row_unit, in_effect)
        row_state <- c(row_state, rep(paste0(effect, "|from"), length(in_effect)))
      }
      row_unit <- c(row_unit, in_effect)
      row_state <- c(row_state, rep(paste0(effect, "|to"), length(in_effect)))
    }
    states <- unit_data[row_unit, ]
    for (effect in effects) {
      for (side in c("from", "to")) {
        settings <- changes[[effect]][[side]]
        rows <- which(row_state == paste0(effect, "|", side))
        for (k in seq_along(settings)) {
          states[[names(settings)[k]]][rows] <- settings[[k]]
        }
      }
    }
    states$ame_row <- seq_len(nrow(states))
    n_block_states <- nrow(states)
    # match(): the position of each alternative among the outcomes
    regressors <- make_regressors(states) %>%
      mutate(outcome_position = match(as.character(alternative), outcomes)) %>%
      arrange(ame_row, outcome_position)
    if (!identical(regressors$ame_row, rep(states$ame_row, each = n_outcomes)) ||
        !identical(regressors$outcome_position,
                   rep(seq_len(n_outcomes), n_block_states))) {
      stop("make_regressors() must return one row per state x alternative, with ",
           "the outcomes of the model: ", paste(outcomes, collapse = ", "))
    }
    no_column <- setdiff(terms$column, c(names(regressors), "(Intercept)"))
    if (length(no_column) > 0) {
      stop("Coefficients without a column in make_regressors(): ",
           paste(no_column, collapse = ", "),
           ". A factor in a formula? Make its dummies in make_regressors().")
    }
    X_block <- matrix(0, nrow(regressors), nrow(terms),
                      dimnames = list(NULL, terms$parameter))
    for (k in seq_len(nrow(terms))) {
      values <- 1
      if (terms$column[k] != "(Intercept)") {
        values <- regressors[[terms$column[k]]]
      }
      if (is.na(terms$technology[k])) {
        X_block[, k] <- values
      } else {
        X_block[, k] <- values * (regressors$alternative == terms$technology[k])
      }
    }
    if (anyNA(X_block)) {
      stop("Missing values in the regressors of the states: ",
           paste(colnames(X_block)[colSums(is.na(X_block)) > 0], collapse = ", "))
    }

    # The column of each random constant in each state. States with the same
    # columns share their random part: with one set of constants all of them,
    # with constants by group those of one group.
    random_value <- matrix(0, n_block_states, nrow(random))
    for (k in seq_len(nrow(random))) {
      position <- match(random$technology[k], outcomes)
      rows <- seq(position, by = n_outcomes, length.out = n_block_states)
      random_value[, k] <- X_block[rows, random$name[k]]
    }
    columns_of_state <- apply(random_value, 1, paste, collapse = " ")
    state_groups <- list()
    for (rows in split(seq_len(n_block_states), columns_of_state)) {
      state_groups[[length(state_groups) + 1]] <- list(
        states = rows,
        values = random_value[rows[1], ]
      )
    }

    # Which rows go into which cell: an own state into the baseline of each
    # profile its unit belongs to, and into the start of every effect without
    # `from` that includes the unit; any other state into its own cell.
    pair_row <- seq_len(n_block_states)
    pair_label <- if_else(row_state == "own", "baseline", row_state)
    for (effect in effects[!has_from]) {
      own_in_effect <- which(row_state == "own" & included[[effect]][row_unit])
      pair_row <- c(pair_row, own_in_effect)
      pair_label <- c(pair_label, rep(paste0(effect, "|from"), length(own_in_effect)))
    }
    pair_label_position <- match(pair_label, state_labels)
    member_rows <- integer(0)
    member_cells <- integer(0)
    for (p in seq_len(n_profiles)) {
      keep <- is_member[row_unit[pair_row], p]
      member_rows <- c(member_rows, pair_row[keep])
      member_cells <- c(member_cells, (p - 1L) * n_labels + pair_label_position[keep])
    }
    member_unit <- c(member_unit, row_unit[member_rows])
    member_cell <- c(member_cell, member_cells)

    blocks[[b]] <- list(X = X_block, state_groups = state_groups,
                        member_rows = member_rows, member_cells = member_cells)
    block_hashes[b] <- rlang::hash(blocks[[b]])
  }
  # The units and the experts that each cell averages over. tabulate(): how often
  # each cell number occurs.
  cell_units <- tabulate(member_cell, nbins = n_cells)
  cell_experts <- rep(NA_integer_, n_cells)
  if (!is.null(population)) {
    experts_of_cell <- split(as.character(unit_data$resp_uid[member_unit]),
                             factor(member_cell, levels = seq_len(n_cells)))
    for (cell in seq_len(n_cells)) {
      cell_experts[cell] <- length(unique(experts_of_cell[[cell]]))
    }
  }

  # (d) Nothing changed? hash(): a short code that changes whenever its object
  # does. The design matrix, not the function, enters it: make_regressors() may
  # change, in a variable that another model uses, without changing what this
  # model predicts. The experts behind each cell and the setting that leaves
  # experts out enter too, as they are saved with the effects.
  fingerprint <- rlang::hash(list(results$fingerprint, at, changes, profiles,
                                  exclude_na, n_draws, HALTON_PRIMES,
                                  cell_experts, block_hashes))
  if (!force && identical(results$ames[[name]]$fingerprint, fingerprint)) {
    message("Model ", results$name, ": effects ", name,
            " loaded, nothing changed")
    refresh_workbook(results)
    return(results)
  }

  # (e) The prediction draws: one row per draw, one column per random constant.
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

  # (f) The sums of the probabilities over each cell's units, at the estimates and
  # at every Krinsky-Robb draw. The work goes to n_workers R sessions, each with
  # its share of the blocks and of the draws: the blocks in as many groups as
  # there are sessions or blocks, and the draws in as many chunks as fit, so that
  # no session gets two tasks. --vanilla: each session is ready at once, and
  # cell_probabilities() needs only base R. on.exit(): runs when this function
  # ends, also with an error.
  prediction <- list(
    Z = Z,
    n_cells = n_cells,
    n_outcomes = n_outcomes,
    constant_outcome = match(random$technology, outcomes),
    beta_names = terms$parameter,
    cholesky_names = parts$cholesky$parameter,
    cholesky_i = parts$cholesky$i,
    cholesky_j = parts$cholesky$j
  )
  # t(): the vector as a matrix of one row
  sums_at_estimates <- cell_probabilities(list(
    prediction = prediction, blocks = blocks,
    theta_rows = t(results$coefficients)
  ))

  theta_draws <- results$theta_draws
  n_draws_kr <- nrow(theta_draws)
  n_blocks <- length(blocks)
  n_block_groups <- min(n_workers, n_blocks)
  n_chunks <- floor(n_workers / n_block_groups)
  block_group_of_block <- ceiling(seq_len(n_blocks) * n_block_groups / n_blocks)
  chunk_of_draw <- ceiling(seq_len(n_draws_kr) * n_chunks / n_draws_kr)
  draws_of_chunk <- split(seq_len(n_draws_kr), chunk_of_draw)
  tasks <- list()
  for (g in seq_len(n_block_groups)) {
    for (chunk in seq_along(draws_of_chunk)) {
      tasks[[length(tasks) + 1]] <- list(
        prediction = prediction,
        blocks = blocks[block_group_of_block == g],
        theta_rows = theta_draws[draws_of_chunk[[chunk]], , drop = FALSE],
        chunk = chunk
      )
    }
  }
  cluster <- parallel::makeCluster(n_workers, rscript_args = "--vanilla")
  on.exit(parallel::stopCluster(cluster))
  # parLapply(): lapply on the sessions
  task_sums <- parallel::parLapply(cluster, tasks, cell_probabilities)
  sums_at_draws <- array(0, dim = c(n_cells, n_outcomes, n_draws_kr))
  for (task_index in seq_along(tasks)) {
    draws_of_task <- draws_of_chunk[[tasks[[task_index]]$chunk]]
    sums_at_draws[, , draws_of_task] <- sums_at_draws[, , draws_of_task, drop = FALSE] +
      task_sums[[task_index]]
  }
  # The averages: each cell's sum over its units, divided by their number (0 for
  # an effect that no unit of the profile is included in)
  estimates <- matrix(sums_at_estimates, n_cells, n_outcomes) / cell_units
  all_draws <- sums_at_draws / cell_units

  # (g) The tables: the effects and the probabilities at the baseline, per
  # profile. The first element of table_rows: without changes, the table keeps
  # its columns.
  z_critical <- qnorm(0.975)
  table_rows <- list(tibble(
    profile = character(0), effect = character(0), outcome = character(0),
    p_from = numeric(0), p_to = numeric(0), ame = numeric(0), se = numeric(0),
    z = numeric(0), p = numeric(0), ci_low = numeric(0), ci_high = numeric(0),
    ci_low_kr = numeric(0), ci_high_kr = numeric(0),
    n_units = integer(0), n_experts = integer(0)
  ))
  baseline_rows <- list()
  draws <- list()
  for (p in seq_len(n_profiles)) {
    profile_name <- names(profile_settings)[p]
    cells <- (p - 1) * n_labels + seq_len(n_labels)
    estimate <- estimates[cells, , drop = FALSE]
    dimnames(estimate) <- list(state_labels, outcomes)
    profile_draws <- all_draws[cells, , , drop = FALSE]
    dimnames(profile_draws) <- list(state_labels, outcomes, NULL)
    draws[[profile_name]] <- profile_draws
    units_of_state <- setNames(as.integer(cell_units[cells]), state_labels)
    experts_of_state <- setNames(cell_experts[cells], state_labels)
    for (outcome in outcomes) {
      baseline_draws <- profile_draws["baseline", outcome, ]
      baseline_rows[[length(baseline_rows) + 1]] <- tibble(
        profile = profile_name,
        outcome = outcome,
        probability = estimate["baseline", outcome],
        se = sd(baseline_draws),
        ci_low_kr = quantile(baseline_draws, 0.025, names = FALSE),
        ci_high_kr = quantile(baseline_draws, 0.975, names = FALSE),
        n_units = units_of_state[["baseline"]],
        n_experts = experts_of_state[["baseline"]]
      )
    }
    for (effect in effects) {
      state_from <- paste0(effect, "|from")
      state_to <- paste0(effect, "|to")
      if (units_of_state[[state_to]] == 0) {
        table_rows[[length(table_rows) + 1]] <- tibble(
          profile = profile_name, effect = effect, outcome = outcomes,
          p_from = NA_real_, p_to = NA_real_, ame = NA_real_, se = NA_real_,
          z = NA_real_, p = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
          ci_low_kr = NA_real_, ci_high_kr = NA_real_,
          n_units = 0L, n_experts = 0L
        )
        next
      }
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
          ci_high_kr = quantile(effect_draws, 0.975, names = FALSE),
          n_units = units_of_state[[state_to]],
          n_experts = experts_of_state[[state_to]]
        )
      }
    }
  }
  effects_table <- bind_rows(table_rows)
  baseline_table <- bind_rows(baseline_rows)

  # A change of a variable the model does not use gives exactly 0, which would
  # read as a finding about that variable
  moving_effects <- unique(effects_table$effect[which(effects_table$se > 0)])
  empty_effects <- setdiff(effects, effects_table$effect[which(effects_table$n_units > 0)])
  not_moving <- setdiff(effects, c(moving_effects, empty_effects))
  if (length(not_moving) > 0) {
    message("Model ", results$name, ": effects ", name, ": these changes move ",
            "nothing in this model: ", paste(not_moving, collapse = ", "),
            " (a variable it does not use, or a level the units already have)")
  }
  if (length(empty_effects) > 0) {
    message("Model ", results$name, ": effects ", name, ": no unit of any profile ",
            "is included in: ", paste(empty_effects, collapse = ", "))
  }
  # Which effects leave experts out of the average, and how many at most
  # (exclude_na); n_experts of the table has the number for every profile
  left_out <- effects_table %>%
    distinct(profile, effect, n_experts) %>%
    left_join(distinct(baseline_table, profile, n_experts_all = n_experts),
              by = "profile", relationship = "many-to-one") %>%
    mutate(n_left_out = n_experts_all - n_experts) %>%
    filter(n_left_out > 0) %>%
    group_by(effect) %>%
    summarise(most = max(n_left_out))
  if (nrow(left_out) > 0) {
    message("Model ", results$name, ": effects ", name, ": experts left out of an ",
            "effect, at most: ",
            paste0(left_out$effect, " (", left_out$most, ")", collapse = ", "))
  }
  # Over all the answers of the sample and with nothing set, the average
  # probabilities are the shares of the recommendations, which a model with a
  # constant for every outcome reproduces up to the simulation error
  for (p in seq_len(n_profiles)) {
    baseline_cell <- (p - 1) * n_labels + 1
    if (length(at) == 0 && cell_units[baseline_cell] == results$fit$n_choices) {
      observed <- as.numeric(results$choice_counts[outcomes]) / results$fit$n_choices
      gap <- max(abs(estimates[baseline_cell, ] - observed))
      if (gap > 0.02) {
        message("Model ", results$name, ": effects ", name, ": over all ",
                results$fit$n_choices, " answers the average probabilities differ ",
                "from the observed shares by up to ", round(100 * gap, 1),
                " percentage points (profile ", names(profile_settings)[p], ")")
      }
    }
  }

  # (h) Save. Averages only: nothing at the level of experts or units
  population_summary <- NULL
  if (!is.null(population)) {
    population_summary <- list(
      experts = length(unique(unit_data$resp_uid)),
      units = n_units,
      exclude_na = exclude_na
    )
  }
  results$ames[[name]] <- list(
    table = effects_table,
    baseline = baseline_table,
    draws = draws,
    at = at,
    changes = changes,
    profiles = profiles,
    population = population_summary,
    n_draws = n_draws,
    n_draws_kr = n_draws_kr,
    fingerprint = fingerprint,
    date = Sys.Date()
  )
  saveRDS(results, results_file)
  write_results_excel(results)
  seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  message("Model ", results$name, ": effects ", name, ", ", n_units, " units, ",
          n_states, " states, ", n_draws_kr, " Krinsky-Robb draws on ", n_workers,
          " R sessions, ", round(seconds), " seconds")
  results
}

#' The sums of the probabilities of the recommendations over the units of each
#' cell, at each row of theta_rows. Only base R: it runs on --vanilla sessions.
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
#' @param task list made in compute_ames(): prediction (the draws and the
#'             coefficients' names), blocks (X, state_groups, member_rows,
#'             member_cells: the rows of a state each cell sums) and theta_rows
#'             (matrix, one row per parameter vector, columns named as coef())
#' @return array: cells x outcomes x rows of theta_rows, the sums over the rows of
#'         each cell
cell_probabilities <- function(task) {
  prediction <- task$prediction
  theta_rows <- task$theta_rows
  Z <- prediction$Z
  n_outcomes <- prediction$n_outcomes
  n_draws <- nrow(Z)
  L <- matrix(0, ncol(Z), ncol(Z))
  cholesky_positions <- cbind(prediction$cholesky_i, prediction$cholesky_j)
  # array(): a matrix with a third dimension
  cell_sums <- array(0, dim = c(prediction$n_cells, n_outcomes, nrow(theta_rows)))
  for (s in seq_len(nrow(theta_rows))) {
    theta <- theta_rows[s, ]

    # (a) The deviations of the constants: draws x constants, row r is (L z_r)'
    L[cholesky_positions] <- theta[prediction$cholesky_names]
    deviations <- Z %*% t(L)

    for (block in task$blocks) {
      n_states <- nrow(block$X) / n_outcomes

      # (b) The mean parts: outcomes x states, X being sorted by state, then
      # outcome. Each column minus its state's largest. pmax(): the larger value,
      # element by element.
      mean_utility <- matrix(block$X %*% theta[prediction$beta_names],
                             n_outcomes, n_states)
      largest_mean <- mean_utility[1, ]
      for (j in seq_len(n_outcomes)) {
        largest_mean <- pmax(largest_mean, mean_utility[j, ])
      }
      weights <- exp(mean_utility - rep(largest_mean, each = n_outcomes))

      # (c) The states that share a random part, usually all of them
      probabilities <- matrix(NA_real_, n_states, n_outcomes)
      for (state_group in block$state_groups) {
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
        probabilities[state_group$states, ] <-
          t(weights_group * sums_over_draws / n_draws)
      }
      # A denominator of 0 (every term below what exp() can represent) would give
      # Inf or NaN; checked at every evaluation, the Krinsky-Robb draws included
      if (any(!is.finite(probabilities))) {
        stop("A probability cannot be computed: the utilities of a state differ by ",
             "more than exp() can represent (at a Krinsky-Robb draw, a coefficient ",
             "can be extreme)")
      }

      # (d) The sums over the rows of each cell. rowsum(): the sum of the rows
      # that share a group, sorted by group.
      sums <- rowsum(probabilities[block$member_rows, , drop = FALSE],
                     block$member_cells)
      cells <- as.integer(rownames(sums))
      previous <- matrix(cell_sums[cells, , s], length(cells), n_outcomes)
      cell_sums[cells, , s] <- previous + sums
    }
  }
  cell_sums
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
