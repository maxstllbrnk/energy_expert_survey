################################################################################
# fit_mixed_logit.R — estimates a mixed logit whose constants are random, normal
#                     and correlated, from the formula written in the estimation
#                     script
# Inputs : the formula and the data in mlogit's format
# Outputs: <est_dir>/<name>/model.rds, the fitted model. It contains the data, so
#          it stays on the secure drive.
# Checks : before the estimation: no missing value; every outcome chosen; a term
#          with two values takes both among the choices; the terms are linearly
#          independent. With starting values from another model: the
#          log-likelihood there equals that model's. The model has converged.
################################################################################
# A model has either one set of constants for all experts (a 1 in the formula,
# the default) or one set per group (a 0 and the group dummies in the formula,
# named in group_constants). With one set per group, each group has its own means
# and covariance matrix. Every expert has the constants of one group only, so the
# covariances between two groups' constants never enter the likelihood: they are
# fixed at 0, a normalisation rather than a restriction.

#' Estimate one model, or load it if nothing changed since it was estimated
#'
#' Keeps what running mlogit() directly would get wrong:
#'   rpar in mlogit's order   cov.mlogit() and stdev() label the constants in the
#'                            order of rpar, while the likelihood uses the order
#'                            of the coefficients
#'   starting values          from a model this one nests, or from the same model
#'                            with one set of constants. Without them, (3pg) and
#'                            (4pd) ended below the model they nest although
#'                            mlogit reported convergence (22 and 28 October 2026).
#'   Halton primes by group   the primes start again at 2 for each group: an
#'                            expert's utility contains only the constants of
#'                            their group, and high primes cover the unit cube
#'                            unevenly (Train 2009, section 9.3.3)
#'   restart after code 3     the line search found no higher value along mlogit's
#'                            approximation of the Hessian, which can be poor near
#'                            the maximum. A restart from the last iterate builds
#'                            it anew, at most three times.
#'   re-evaluation at code 2  mlogit then returns the coefficients of the last
#'                            iterate but the gradient and log-likelihood of a
#'                            trial point one small step further
#'
#' @param name            name of the model, also of its folder, e.g. "m4pe"
#' @param formula         mlogit's formula, e.g.
#'                        chosen ~ price | 1 + is_hvac + is_sweep + age_60. Plain
#'                        variable names only: make transformations visible in
#'                        make_regressors() of the estimation script.
#' @param data            data in mlogit's format (dfidx()), with idx chid,
#'                        resp_uid and alternative
#' @param reference       the outcome whose utility is normalised to 0
#' @param est_dir         the folder of the estimation script
#' @param group_constants NULL for one set of constants; otherwise the group
#'                        dummies whose constants are random, e.g.
#'                        c("is_adviser", "is_hvac", "is_sweep"), which the
#'                        formula lists after a 0
#' @param start_from      NULL: mlogit's default starting values. Otherwise a
#'                        fitted model, on the same data with the same draws: a
#'                        model this one nests (added parameters start at 0), or,
#'                        with group_constants, the same model with one set of
#'                        constants and the group among its regressors
#' @param n_draws         Halton draws per expert
#' @param force           TRUE: estimate even if nothing changed
#' @return the fitted model, with $name and $fit_info
fit_mixed_logit <- function(name, formula, data, reference, est_dir,
                            group_constants = NULL, start_from = NULL,
                            n_draws = N_DRAWS_MODEL, force = FALSE) {
  model_dir <- file.path(est_dir, name)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  model_file <- file.path(model_dir, "model.rds")

  # (a) Nothing changed? hash(): a short code that changes whenever its object
  # does. Only the columns of this formula, so that a new variable in
  # make_regressors() for another model does not estimate this one again.
  # all.vars(): the variables of a formula. all_of(): the columns with these names.
  start_coefficients <- NULL
  if (!is.null(start_from)) {
    start_coefficients <- coef(start_from)
  }
  columns_of_formula <- as.data.frame(data) %>%
    select(all_of(all.vars(formula)))
  fingerprint <- rlang::hash(list(
    paste(deparse(formula), collapse = " "), rlang::hash(columns_of_formula),
    rlang::hash(as.data.frame(idx(data))),
    reference, group_constants, n_draws, start_coefficients
  ))
  if (!force && file.exists(model_file)) {
    saved_model <- readRDS(model_file)
    if (identical(saved_model$fit_info$fingerprint, fingerprint)) {
      message("Model ", name, ": nothing changed, loaded from ", model_file)
      return(saved_model)
    }
  }
  message("\n==== Model ", name, " ====")
  start_time <- Sys.time()   # Sys.time(): the current date and time

  # (b) The random coefficients: one constant per technology (and group), named
  # as mlogit names them, e.g. "is_hvac:Gasheizung". droplevels(): the
  # alternatives of this sample only.
  alternatives <- levels(droplevels(idx(data)$alternative))
  technologies <- setdiff(alternatives, reference)
  has_groups <- !is.null(group_constants)
  constants <- "(Intercept)"
  if (has_groups) {
    constants <- group_constants
  }
  random <- tibble()
  for (constant in constants) {
    random <- bind_rows(random, tibble(
      name = paste0(constant, ":", technologies),
      column = constant,
      technology = technologies
    ))
  }
  arguments <- list(
    formula = formula,
    # the data enter by name, so that printing a model does not print them
    data = quote(data),
    reflevel = reference,
    rpar = setNames(rep("n", nrow(random)), random$name),
    panel = TRUE,
    R = n_draws,
    halton = NA,
    correlation = TRUE
  )

  # (c) What the data cannot estimate. mlogit() would drop a decision with a
  # missing value without notice, stop with a singular matrix that names no term,
  # or go on with a coefficient that runs away; a smaller sample makes all of
  # them likelier. So first, no missing value in the variables of the formula.
  has_missing <- colSums(is.na(columns_of_formula)) > 0
  if (any(has_missing)) {
    stop("Model ", name, ": missing values in ",
         paste(names(columns_of_formula)[has_missing], collapse = ", "),
         ". Restrict the sample visibly in the script")
  }
  # Every outcome is chosen. A term with two values in one alternative (a dummy,
  # centred or not) takes both among the decisions for that alternative, and, if
  # it has a coefficient per technology, among those for the reference outcome:
  # there, all its coefficients would run away together. estimate = FALSE:
  # mlogit() returns its data and does not estimate. do.call() stores the values
  # of the arguments in the call: mlogit() looks up some of them, constPar among
  # them, in the global environment instead of where it is called, so inside this
  # function it would not find them otherwise.
  frame_arguments <- arguments
  frame_arguments$estimate <- FALSE
  frame <- do.call("mlogit", frame_arguments)
  X <- model.matrix(frame)
  index <- idx(frame)
  is_chosen <- as.logical(frame[[1]])
  chose_reference <- index$chid %in%
    index$chid[is_chosen & index$alternative == reference]
  not_finite <- setdiff(alternatives,
                        as.character(index$alternative)[is_chosen])
  for (parameter in colnames(X)) {
    varies_in <- character(0)
    for (alternative in alternatives) {
      if (n_distinct(X[index$alternative == alternative, parameter]) > 1) {
        varies_in <- c(varies_in, alternative)
      }
    }
    if (length(varies_in) == 1) {
      rows <- index$alternative == varies_in
      if (n_distinct(X[rows, parameter]) == 2) {
        n_values_chosen <- n_distinct(X[rows & is_chosen, parameter])
        n_values_reference <- 2
        if (endsWith(parameter, paste0(":", varies_in))) {
          n_values_reference <- n_distinct(X[rows & chose_reference, parameter])
        }
        if (n_values_chosen < 2 || n_values_reference < 2) {
          not_finite <- c(not_finite, parameter)
        }
      }
    }
  }
  if (length(not_finite) > 0) {
    stop("Model ", name, ": no finite estimate of ",
         paste(not_finite, collapse = ", "),
         ". Never chosen in this sample, or chosen only at one value of the ",
         "term: change the sample or the formula")
  }
  # The terms are linearly independent. Only differences within a decision enter
  # the likelihood, so a term the same for all alternatives counts as none.
  # rowsum(): the sum of the rows of each decision. qr(): puts the columns it
  # cannot use last.
  decision <- as.character(index$chid)
  means_of_decisions <- rowsum(X, decision) / length(alternatives)
  decomposition <- qr(X - means_of_decisions[decision, , drop = FALSE])
  if (decomposition$rank < ncol(X)) {
    dropped <- colnames(X)[decomposition$pivot[(decomposition$rank + 1):ncol(X)]]
    stop("Model ", name, ": no coefficient can be estimated for ",
         paste(dropped, collapse = ", "), ": the term is the same for every ",
         "expert of the sample, or depends on other terms. Delete it from the ",
         "formula")
  }

  # (d) mlogit's names of all parameters, in its order: a run with few draws and
  # no iterations. The random coefficients in this order are the rows and columns
  # of L.
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
    stop("Random constants that are not among mlogit's parameters: is the ",
         "formula's constant part consistent with group_constants?")
  }
  random <- random %>%
    select(-position)
  arguments$rpar <- setNames(rep("n", nrow(random)), random$name)

  # (e) Constants by group: the elements of L between two groups fixed at 0
  # (mlogit names L[i, j], i >= j, "chol.<coefficient j>:<coefficient i>"), and
  # the primes
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
    fixed_parameters <- setNames(rep(0, length(fixed_names)), fixed_names)
    arguments$constPar <- fixed_parameters
    halton_primes <- rep(NA_real_, nrow(random))
    for (constant in constants) {
      is_group <- random$column == constant
      halton_primes[is_group] <- HALTON_PRIMES[seq_len(sum(is_group))]
    }
    arguments$halton <- list(prime = halton_primes)
  }

  # (f) Starting values
  log_likelihood_start <- NA_real_
  start_from_name <- NA_character_
  if (!is.null(start_from)) {
    start_from_name <- start_from$name
    theta_from <- coef(start_from)
    start <- setNames(rep(0, length(parameter_names)), parameter_names)
    from_is_common <- any(startsWith(names(theta_from), "(Intercept):"))
    if (has_groups && from_is_common) {
      # Each group gets the common model's constants plus its shift (0 for the
      # reference group), and the common model's L as its block of L. At these
      # values the two models predict the same for every expert.
      cholesky_from <- names(theta_from)[startsWith(names(theta_from), "chol.")]
      constants_of_groups <- character(0)
      for (group in group_constants) {
        for (technology in technologies) {
          constant_name <- paste0(group, ":", technology)
          constants_of_groups <- c(constants_of_groups, constant_name)
          shift <- 0
          if (constant_name %in% names(theta_from)) {
            shift <- theta_from[[constant_name]]
          }
          common_constant <- theta_from[[paste0("(Intercept):", technology)]]
          start[[constant_name]] <- common_constant + shift
        }
        for (cholesky_name in cholesky_from) {
          group_name <- gsub("(Intercept)", group, cholesky_name, fixed = TRUE)
          start[[group_name]] <- theta_from[[cholesky_name]]
        }
      }
      shared_parameters <- setdiff(intersect(parameter_names, names(theta_from)),
                                   constants_of_groups)
      for (parameter in shared_parameters) {
        start[[parameter]] <- theta_from[[parameter]]
      }
    } else {
      # A model this one nests: every parameter there at its estimate, the added
      # ones at 0
      not_in_model <- setdiff(names(theta_from), parameter_names)
      if (length(not_in_model) > 0) {
        stop("Model ", name, " does not nest ", start_from_name,
             ", which also has: ", paste(not_in_model, collapse = ", "))
      }
      start[names(theta_from)] <- theta_from
    }
    arguments$start <- start
    # At the starting values, the model has the log-likelihood of start_from
    # (same draws for every expert). If not, start_from was estimated on other
    # data or with other draws.
    start_arguments <- arguments
    start_arguments$iterlim <- 0
    start_arguments$print.level <- 0
    log_likelihood_start <- as.numeric(logLik(do.call("mlogit", start_arguments)))
    cat("Log-likelihood at the starting values:", round(log_likelihood_start, 3),
        "| ", start_from_name, ":", round(as.numeric(logLik(start_from)), 3),
        "\n")
    if (abs(log_likelihood_start - as.numeric(logLik(start_from))) > 1e-6) {
      stop("At the starting values, the log-likelihood of ", name,
           " differs from that of ", start_from_name,
           ": estimate that model again on this data")
    }
  }

  # (g) Estimate
  model <- do.call("mlogit", arguments)
  for (restart in 1:3) {
    if (model$est.stat$code != 3) break
    message("Code 3 after ", model$est.stat$nb.iter,
            " iterations, log-likelihood ",
            round(as.numeric(logLik(model)), 2), "; restart ", restart)
    # starting values for every parameter, also those fixed by constPar
    start <- setNames(as.numeric(model$coefficients),
                      names(model$coefficients))
    model <- update(model, start = start)
  }
  if (!(model$est.stat$code %in% c(1, 2))) {
    stop("Model ", name, " has not converged (code ", model$est.stat$code, ")")
  }
  if (model$est.stat$code == 2) {
    # update() evaluates the model's call here, where its data are
    start <- setNames(as.numeric(model$coefficients),
                      names(model$coefficients))
    estimation_status <- model$est.stat
    model <- update(model, start = start, iterlim = 0, print.level = 0)
    model$est.stat <- estimation_status
  }

  # (h) Save
  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  model$name <- name
  model$fit_info <- list(
    fingerprint = fingerprint,
    reference = reference,
    outcomes = c(reference, technologies),
    random = random,
    fixed_parameters = fixed_parameters,
    start_from = start_from_name,
    log_likelihood_start = log_likelihood_start,
    n_draws = n_draws,
    minutes = round(minutes, 1)
  )
  saveRDS(model, model_file)
  cat("Model", name, ": log-likelihood", round(as.numeric(logLik(model)), 3),
      "| code", model$est.stat$code, "| iterations", model$est.stat$nb.iter,
      "| minutes", round(minutes, 1), "\n")
  model
}
