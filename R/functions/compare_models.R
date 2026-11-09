################################################################################
# compare_models.R — the fixed protocol for accepting or rejecting models, and
#                    one decision sheet that makes clear why a model was chosen
# Inputs : <est_dir>/<model>/model.rds and results.rds of every model in the
#          folder (fit_mixed_logit(), post_estimation())
# Outputs: <est_dir>/model_comparison/model_comparison.rds (a named list of the
#          sheets) and model_comparison.xlsx, aggregates only;
#          <est_dir>/<model>/cross_validation.rds, the held-out log-likelihood
#          per expert, so that a new run need not estimate again: not an
#          aggregate, so it stays next to model.rds. While the folds run, each
#          writes cross_validation_fold_<k>.rds, so that a run that is cut off
#          resumes with the folds it had.
# Checks : the models of a test share sample, reference and draws; the df equal
#          the difference in parameters; results.rds belongs to model.rds
################################################################################
# Why a protocol (seminar tables, 2 November 2026). (4pe) against (6pe): LR 69.2,
# df 42, robust p below 0.001, while AIC and BIC favour (4pe). (4p) against (6p):
# p 0.099, robust p 0.034. (4p) against (4pl): 0.7 log points, while fresh Halton
# draws alone move a log-likelihood with SD 2.0. So one measure does not decide,
# and the rule below is fixed before results are read. The proposal is the
# rule's, the final call the researchers'.
#
# The rule for a larger model, in this order:
#   a model of the test is not valid        not compared
#   the robust test, after Holm, says no    keep smaller
#   the gain does not survive fresh draws   keep smaller: gain does not survive
#                                           fresh draws
#   cross-validation does not favour it by  keep smaller: tests and parsimony
#   more than one paired SE (BIC if it was  disagree
#   not run or is not usable)
#   no AME moves by ame_threshold or more   keep smaller as main; report larger
#                                           as robustness
#   otherwise                               accept larger
# The AMEs are those both models compute, and those only the larger model has,
# which count as moving from 0, their value in the smaller model, if their 95%
# Krinsky-Robb interval excludes 0 (Max, 2026-11-09): a larger model passes if its
# new terms or its other effects move by ame_threshold.
# For models that do not nest each other: prefer a model if Vuong's test and
# cross-validation (or BIC, as above) agree on it, otherwise data cannot decide.
# A model that did not converge, or whose Hessian is not positive definite, never
# reaches this function: fit_mixed_logit() and post_estimation() stop on it. A
# nested test whose restrictions are not independent at the estimates, as when one
# random constant has a standard deviation near 0 in several groups, is not
# compared.

# The limits of the protocol, set before any result is read.
# A Newton step would still add half of g' H^{-1} g to the log-likelihood: at 0.1,
# 0.05 log points, far below the noise of the simulated likelihood.
MAX_SCALED_GRADIENT <- 0.1
# Central differences with h = 1e-4 give the Hessian to about 8 digits, so at this
# condition number no reliable digit is left in its weakest direction
MAX_CONDITION_NUMBER <- 1e8
# As in fit_mixed_logit(): below this, two log-likelihoods are the same number
LOG_LIKELIHOOD_TOLERANCE <- 1e-6
# Draws for a robust p-value: its standard error is at most 0.0005
ROBUST_DRAWS <- 1e6
# A mean weight of the robust test below this is flagged in the reason: a
# clustered variance that rests on few experts, as for a rare dummy, pushes the
# weights below 1, and the robust test then rejects too easily (0.06 with a dummy
# held by 3 of 300 experts, tests/test_compare_models.R)
LOW_MEAN_WEIGHT <- 0.5
CV_FOLDS <- 5
# The columns that tell an expert's group, if no model has constants by group
CV_GROUP_COLUMNS <- c("is_adviser", "is_hvac", "is_sweep")

#' Is model a a special case of model b? Read from the names of the parameters.
#'
#' Added parameters: a's parameters are among b's, and the restriction is that the
#' others are 0. Group covariances: b has constants by group, a one set of
#' constants with group shifts. Each group's constants in b are a's constant plus
#' the shift, written differently, so the means need no restriction; the restriction
#' is that the covariance matrices of the constants are equal, and that the
#' parameters a does not have are 0. If a lacks the shifts of further groups, their
#' means must be equal as well.
#'
#' @param results_a,results_b the results of the two models
#' @return list: type, one of "added parameters", "group covariances", "group
#'   covariances and means", "non-nested"; added, the names of the parameters that
#'   are 0 under a; groups_without_shift, the groups with a's constants
nesting_of <- function(results_a, results_b) {
  names_a <- names(results_a$coefficients)
  names_b <- names(results_b$coefficients)
  not_nested <- list(type = "non-nested", added = character(0),
                     groups_without_shift = character(0))
  # setdiff(a, b): a without b. identical(): exactly the same object.
  if (all(names_a %in% names_b) && length(names_b) > length(names_a)) {
    return(list(type = "added parameters", added = setdiff(names_b, names_a),
                groups_without_shift = character(0)))
  }
  groups_b <- unique(results_b$random$column)
  if (length(groups_b) < 2 ||
      !identical(unique(results_a$random$column), "(Intercept)")) {
    return(not_nested)
  }
  # What is neither a constant nor an element of L must be in b too.
  # startsWith(): does the text begin with this?
  cholesky_a <- names_a[startsWith(names_a, "chol.")]
  cholesky_b <- names_b[startsWith(names_b, "chol.")]
  others_a <- setdiff(names_a, c(results_a$random$name, cholesky_a,
                                 results_b$random$name))
  others_b <- setdiff(names_b, c(results_b$random$name, cholesky_b))
  if (!all(others_a %in% others_b)) {
    return(not_nested)
  }
  groups_without_shift <- character(0)
  for (group in groups_b) {
    if (!any(paste0(group, ":", results_b$outcomes[-1]) %in% names_a)) {
      groups_without_shift <- c(groups_without_shift, group)
    }
  }
  type <- "group covariances and means"
  if (length(groups_without_shift) == 1) {
    type <- "group covariances"
  }
  list(type = type, added = setdiff(others_b, others_a),
       groups_without_shift = groups_without_shift)
}

#' The restrictions of a nested test as a function of the larger model's
#' parameters, all 0 if the smaller model holds. The robust test needs their
#' derivative.
#'
#' The covariance matrices of the constants, not their Cholesky factors, because a
#' covariance matrix has several Cholesky factors (the signs of their columns).
#' Each group's covariance matrix minus the first group's, every variance and
#' covariance once.
#'
#' @param theta        coefficients of the larger model
#' @param relationship nesting_of()
#' @param random       results$random of the larger model
#' @return numeric vector, one element per restriction
restrictions_of <- function(theta, relationship, random) {
  restrictions <- theta[relationship$added]
  if (relationship$type %in% c("group covariances", "group covariances and means")) {
    K_r <- nrow(random)
    L <- matrix(0, K_r, K_r, dimnames = list(random$name, random$name))
    for (i in seq_len(K_r)) {
      for (j in seq_len(i)) {
        cholesky_name <- paste0("chol.", random$name[j], ":", random$name[i])
        if (cholesky_name %in% names(theta)) {
          L[i, j] <- theta[[cholesky_name]]
        }
      }
    }
    Omega <- L %*% t(L)
    groups <- unique(random$column)
    technologies <- unique(random$technology)
    first <- paste0(groups[1], ":", technologies)
    for (group in groups[-1]) {
      members <- paste0(group, ":", technologies)
      difference <- Omega[members, members] - Omega[first, first]
      # lower.tri(diag = TRUE): the lower triangle with the diagonal
      restrictions <- c(restrictions,
                        difference[lower.tri(difference, diag = TRUE)])
    }
    # The groups that a has no shift for have the same constants
    base_group <- relationship$groups_without_shift[1]
    for (group in relationship$groups_without_shift[-1]) {
      restrictions <- c(restrictions, theta[paste0(group, ":", technologies)] -
                          theta[paste0(base_group, ":", technologies)])
    }
  }
  as.numeric(restrictions)
}

#' What two models share by name but not by value: the columns, and the experts of
#' the rows. The same name must mean the same variable, or a nesting read from the
#' names is a coincidence.
#'
#' @param model_a,model_b two fitted models
#' @return the names that differ, character(0) if none
columns_that_differ <- function(model_a, model_b) {
  different <- character(0)
  if (!identical(as.character(idx(model_a)$resp_uid),
                 as.character(idx(model_b)$resp_uid))) {
    different <- "the experts"
  }
  # intersect(): the elements in both
  shared_columns <- setdiff(intersect(names(model_a$model), names(model_b$model)),
                            c("idx", "probabilities", "linpred"))
  for (column in shared_columns) {
    if (!identical(as.numeric(model_a$model[[column]]),
                   as.numeric(model_b$model[[column]]))) {
      different <- c(different, column)
    }
  }
  different
}

#' The log-likelihood at the estimates on one set of fresh draws, on an R session
#' of its own. Builds the matrices of step (d) of prepare_likelihood() from the
#' shifted Halton sequences. Constants with the same prime, those of different
#' groups, share one matrix: they see the same draws, and the memory holds it once.
#'
#' @param set              which set: a row of shifts
#' @param theta            coefficients of the model
#' @param prepared         prepare_likelihood(), without its Z
#' @param halton_sequences named by prime: the sequence, length experts x draws
#' @param primes           the prime of each random constant
#' @param shifts           sets x primes, the columns named by prime
#' @return the log-likelihood
fresh_log_likelihood <- function(set, theta, prepared, halton_sequences, primes,
                                 shifts) {
  z_of_prime <- list()
  for (prime in as.character(unique(primes))) {
    z <- qnorm((halton_sequences[[prime]] + shifts[set, prime]) %% 1)
    z_of_prime[[prime]] <- t(matrix(z, nrow = prepared$n_draws,
                                    ncol = prepared$n_experts))
  }
  prepared$Z <- z_of_prime[as.character(primes)]
  simulated_likelihood(theta, prepared, want_scores = FALSE)$log_likelihood
}

#' One fold of the cross-validation, on an R session of its own: estimate on the
#' other folds from the full-sample estimates, then evaluate the held-out experts.
#' Restarts after code 3 as fit_mixed_logit() does.
#'
#' @param job list: model_name, fold, fingerprint; arguments of mlogit();
#'   training_data, held_out_data
#' @return list: model_name, fold, fingerprint, expert, log_likelihood (one per
#'   held-out expert), code, iterations, minutes, error; also saved to
#'   job$fold_file, so that a run that is cut off resumes with the folds it had
cross_validate_fold <- function(job) {
  start_time <- Sys.time()
  # A term without variation in these data has no estimate, and makes mlogit's
  # Hessian singular even at iterlim = 0: a rare dummy, among the few experts of a
  # fold. It stays at its value in `start`, which does not matter where its column is
  # 0. estimate = FALSE: mlogit() returns its data and does not estimate. colSums():
  # the sum per column, here the number of rows that are not 0. Inside this function,
  # as the sessions do not know the others of this file.
  fix_terms_without_variation <- function(arguments) {
    frame_arguments <- arguments
    frame_arguments$estimate <- FALSE
    X <- model.matrix(do.call("mlogit", frame_arguments))
    without_variation <- colnames(X)[colSums(X != 0) == 0]
    if (length(without_variation) > 0) {
      arguments$constPar <- c(arguments$constPar, arguments$start[without_variation])
    }
    arguments
  }

  # tryCatch(): an error comes back as text, so that one fold cannot lose the work
  # of the others. do.call(): as in fit_mixed_logit(), which explains why.
  # setNames(): the vector with these names. duplicated(): seen earlier in the vector.
  fold_result <- tryCatch({
    training_data <- job$training_data
    held_out_data <- job$held_out_data
    arguments <- job$arguments
    arguments$data <- quote(training_data)
    arguments <- fix_terms_without_variation(arguments)
    fit <- do.call("mlogit", arguments)
    for (restart in 1:3) {
      if (fit$est.stat$code != 3) break
      arguments$start <- setNames(as.numeric(fit$coefficients),
                                  names(fit$coefficients))
      fit <- do.call("mlogit", arguments)
    }
    # iterlim = 0: the log-likelihood at these parameters, on the draws mlogit gives
    # the held-out experts. fitted(): the expert's probability, once per decision.
    arguments <- job$arguments
    arguments$data <- quote(held_out_data)
    arguments$start <- setNames(as.numeric(fit$coefficients),
                                names(fit$coefficients))
    arguments$iterlim <- 0
    arguments <- fix_terms_without_variation(arguments)
    held_out <- do.call("mlogit", arguments)
    index <- idx(held_out)
    expert_of_decision <- as.character(index$resp_uid)[!duplicated(index$chid)]
    is_first_decision <- !duplicated(expert_of_decision)
    list(expert = expert_of_decision[is_first_decision],
         log_likelihood = log(as.numeric(fitted(held_out)))[is_first_decision],
         code = fit$est.stat$code, iterations = fit$est.stat$nb.iter,
         error = NA_character_)
  }, error = function(e) {
    list(expert = character(0), log_likelihood = numeric(0), code = NA_real_,
         iterations = NA_real_, error = conditionMessage(e))
  })
  fold_result$model_name <- job$model_name
  fold_result$fold <- job$fold
  fold_result$fingerprint <- job$fingerprint
  fold_result$minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  saveRDS(fold_result, job$fold_file)
  fold_result
}

#' A sheet with its note: in a last column, in the first row only, so that the text
#' runs on to the right in Excel. A sheet without rows becomes the note alone.
add_note <- function(sheet, note) {
  if (nrow(sheet) == 0) {
    return(tibble(note = note))
  }
  sheet$note <- c(note, rep(NA_character_, nrow(sheet) - 1))
  sheet
}

#' A p-value in the text of a reason
p_text <- function(p_value) {
  if (is.na(p_value)) {
    return("not available")
  }
  if (p_value < 0.001) {
    return("below 0.001")
  }
  sprintf("%.3f", p_value)
}

#' Run the protocol on the models of one estimation script: the rule at the top of
#' this file, the arguments and sheets in docs/results_contract.md
#'
#' Reads the models from est_dir, estimates nothing but the folds of the
#' cross-validation, and saves the sheets. Stops if the models of a test differ in
#' sample, reference outcome or draws, or if the restrictions of a nested test
#' differ in number from the difference in parameters.
#'
#' @param est_dir       the folder of the estimation script
#' @param tests         tibble: question, smaller, larger (the names of the models).
#'                      For models that do not nest each other, either order
#' @param blocks        NULL, or a named list of terms: Wald tests in final_model. A
#'                      term is a coefficient's name, or a column with one
#'                      coefficient per technology
#' @param final_model   NULL, or the name of the model of the block tests
#' @param ame_name      NULL, or the name of a set of AMEs in results$ames
#' @param cv_models     NULL, or the names of the models to cross-validate
#' @param ame_threshold an AME moving by less than this (0.01: 1 pp) is immaterial
#' @param alpha         the level of the tests
#' @param n_fresh_draws sets of fresh Halton draws
#' @param n_workers     R sessions for the fresh draws and the folds
#' @return the sheets, a named list of tibbles
compare_models <- function(est_dir, tests, blocks = NULL, final_model = NULL,
                           ame_name = NULL, cv_models = NULL, ame_threshold = 0.01,
                           alpha = 0.05, n_fresh_draws = 12,
                           n_workers = N_WORKERS) {
  start_time <- Sys.time()

  # 0. The models, and whether each test is possible ---------------------------
  # (a) Every model of the folder, also those no test names: step 1 compares each
  # model with every model it nests. A model's draws: their number and the prime of
  # each technology. prepare_likelihood() with a placeholder for the draws: step 2
  # brings fresh draws, and until then only the data are needed.
  # list.dirs(): the subfolders.
  check_unique(tests, c("smaller", "larger"))
  model_names <- unique(c(tests$smaller, tests$larger, final_model, cv_models))
  folders <- list.dirs(est_dir, full.names = FALSE, recursive = FALSE)
  has_model <- file.exists(file.path(est_dir, folders, "model.rds")) &
    file.exists(file.path(est_dir, folders, "results.rds"))
  missing_models <- setdiff(model_names, folders[has_model])
  if (length(missing_models) > 0) {
    stop("No model.rds or results.rds in ", est_dir, " for: ",
         paste(missing_models, collapse = ", "))
  }
  models <- list()
  results_list <- list()
  prepared_list <- list()
  primes_list <- list()
  draws_list <- list()
  for (model_name in folders[has_model]) {
    model <- readRDS(file.path(est_dir, model_name, "model.rds"))
    results <- readRDS(file.path(est_dir, model_name, "results.rds"))
    if (!identical(coef(model), results$coefficients)) {
      if (model_name %in% model_names) {
        stop("Model ", model_name, ": results.rds does not belong to model.rds. ",
             "Run post_estimation() again")
      }
      next
    }
    K_r <- nrow(results$random)
    primes <- HALTON_PRIMES[seq_len(K_r)]
    halton <- eval(model$call$halton)
    if (is.list(halton)) {
      primes <- halton$prime
    }
    draws_of_technologies <- results$random %>%
      mutate(prime = primes) %>%
      distinct(technology, prime) %>%
      arrange(technology)
    models[[model_name]] <- model
    results_list[[model_name]] <- results
    primes_list[[model_name]] <- primes
    draws_list[[model_name]] <- paste(
      results$fit$n_draws_per_expert, "draws, primes",
      paste(draws_of_technologies$technology, draws_of_technologies$prime,
            collapse = ", ")
    )
    if (model_name %in% model_names) {
      prepared <- prepare_likelihood(model, z_all = matrix(0, 1, K_r))
      prepared$Z <- NULL
      prepared_list[[model_name]] <- prepared
    }
  }

  # (b) Each test: same sample, reference and draws, then its kind. For a nested
  # test the matrix R, the derivative of its restrictions: numDeriv::jacobian()
  # passes the arguments after theta on to restrictions_of(). Its rows must be df
  # linearly independent ones (qr()$rank: the number of independent rows).
  tests <- tests %>%
    select(question, smaller, larger) %>%
    mutate(comparison = NA_character_, added = NA_character_, degenerate = FALSE)
  restriction_matrices <- list()
  for (i in seq_len(nrow(tests))) {
    smaller_name <- tests$smaller[i]
    larger_name <- tests$larger[i]
    test_name <- paste0(smaller_name, " against ", larger_name)
    results_smaller <- results_list[[smaller_name]]
    results_larger <- results_list[[larger_name]]
    prepared_smaller <- prepared_list[[smaller_name]]
    prepared_larger <- prepared_list[[larger_name]]
    if (!identical(results_smaller$reference, results_larger$reference) ||
        !identical(results_smaller$outcomes, results_larger$outcomes)) {
      stop("Test ", test_name, ": different reference outcomes or alternatives")
    }
    if (!identical(results_smaller$sample_hash, results_larger$sample_hash) ||
        !identical(prepared_smaller$chosen, prepared_larger$chosen)) {
      stop("Test ", test_name, ": different samples (experts or decisions)")
    }
    # The order of the experts sets who gets which draws
    if (!identical(draws_list[[smaller_name]], draws_list[[larger_name]]) ||
        !identical(prepared_smaller$experts, prepared_larger$experts)) {
      stop("Test ", test_name, ": different draws. ", smaller_name, ": ",
           draws_list[[smaller_name]], "; ", larger_name, ": ",
           draws_list[[larger_name]])
    }
    relationship <- nesting_of(results_smaller, results_larger)
    tests$comparison[i] <- relationship$type
    if (relationship$type == "non-nested") {
      if (nesting_of(results_larger, results_smaller)$type != "non-nested") {
        stop("Test ", test_name, ": the first model nests the second. Swap them")
      }
      next
    }
    different <- columns_that_differ(models[[smaller_name]], models[[larger_name]])
    if (length(different) > 0) {
      stop("Test ", test_name, ": the models differ in ",
           paste(different, collapse = ", "))
    }
    tests$added[i] <- paste(relationship$added, collapse = ", ")
    R <- numDeriv::jacobian(restrictions_of, results_larger$coefficients,
                            relationship = relationship,
                            random = results_larger$random)
    df <- length(results_larger$coefficients) - length(results_smaller$coefficients)
    if (nrow(R) != df) {
      stop("Test ", test_name, ": the restrictions give ", nrow(R), " rows, but ",
           "the models differ by ", df, " parameters")
    }
    # Rows that are not independent at the estimates, as when one random constant
    # has a standard deviation near 0 in several groups: the robust test cannot be
    # computed, and the decision sheet says why
    tests$degenerate[i] <- qr(R)$rank < df
    if (tests$degenerate[i]) {
      message("Test ", test_name, ": the restrictions give ", qr(R)$rank,
              " independent rows of ", df, " at the estimates; not compared")
    }
    restriction_matrices[[test_name]] <- R
  }
  is_nested <- tests$comparison != "non-nested"
  # One set of folds needs one sample
  for (model_name in cv_models) {
    if (!identical(results_list[[model_name]]$sample_hash,
                   results_list[[cv_models[1]]]$sample_hash)) {
      stop("cv_models: ", model_name, " and ", cv_models[1],
           " have different samples")
    }
  }
  # The coefficients of each block, now rather than after the cross-validation: a
  # dummy that is the same for every expert has no coefficient
  block_parameters <- list()
  if (!is.null(final_model)) {
    theta <- results_list[[final_model]]$coefficients
    technologies <- results_list[[final_model]]$outcomes[-1]
    for (block_name in names(blocks)) {
      for (term in blocks[[block_name]]) {
        found <- intersect(c(term, paste0(term, ":", technologies)), names(theta))
        if (length(found) == 0) {
          stop("Block ", block_name, ": ", term, " has no coefficient in ",
               final_model)
        }
        block_parameters[[block_name]] <- c(block_parameters[[block_name]], found)
      }
    }
  }

  # 1. Validity ----------------------------------------------------------------
  # A code of 1 or 2 does not prove a maximum: (3pg) and (4pd) ended below the model
  # they nest (22 and 28 October 2026). So each model is compared with every model
  # of the folder that it nests.
  validity <- tibble()
  for (model_name in model_names) {
    results <- results_list[[model_name]]
    diagnostics <- results$diagnostics
    nested_names <- character(0)
    above_this_model <- character(0)
    gap_to_nested <- NA_real_
    for (other_name in setdiff(names(results_list), model_name)) {
      other <- results_list[[other_name]]
      is_nested_here <- identical(other$sample_hash, results$sample_hash) &&
        identical(other$reference, results$reference) &&
        identical(draws_list[[other_name]], draws_list[[model_name]]) &&
        nesting_of(other, results)$type != "non-nested" &&
        length(columns_that_differ(models[[other_name]], models[[model_name]])) == 0
      if (!is_nested_here) next
      nested_names <- c(nested_names, other_name)
      gap <- results$fit$log_likelihood - other$fit$log_likelihood
      if (is.na(gap_to_nested) || gap < gap_to_nested) {
        gap_to_nested <- gap
      }
      if (gap < -LOG_LIKELIHOOD_TOLERANCE) {
        above_this_model <- c(above_this_model, other_name)
      }
    }
    why <- character(0)
    if (diagnostics$scaled_gradient > MAX_SCALED_GRADIENT) {
      why <- c(why, paste0("scaled gradient ",
                           signif(diagnostics$scaled_gradient, 3),
                           ": not at a maximum"))
    }
    if (diagnostics$condition_number > MAX_CONDITION_NUMBER) {
      why <- c(why, paste0("condition number ",
                           signif(diagnostics$condition_number, 3),
                           ": the Hessian is close to singular"))
    }
    if (length(above_this_model) > 0) {
      why <- c(why, paste0("log-likelihood below ",
                           paste(above_this_model, collapse = ", "),
                           ", which it nests: it stopped short of its maximum"))
    }
    validity <- bind_rows(validity, tibble(
      model = model_name,
      label = results$label,
      n_parameters = results$fit$n_parameters,
      code = diagnostics$code,
      iterations = diagnostics$iterations,
      scaled_gradient = diagnostics$scaled_gradient,
      smallest_eigenvalue = diagnostics$smallest_eigenvalue,
      condition_number = diagnostics$condition_number,
      start_from = diagnostics$start_from,
      log_likelihood = results$fit$log_likelihood,
      nests = paste(nested_names, collapse = ", "),
      gap_to_nested = gap_to_nested,
      valid = length(why) == 0,
      why = paste(why, collapse = "; ")
    ))
  }
  # match(): the position of each name among those of the second vector
  tests$smaller_valid <- validity$valid[match(tests$smaller, validity$model)]
  tests$larger_valid <- validity$valid[match(tests$larger, validity$model)]
  tests$both_valid <- tests$smaller_valid & tests$larger_valid

  # 2. Noise: fresh draws ------------------------------------------------------
  # Randomly shifted Halton draws, one shift per prime: (u + shift) mod 1 is again
  # evenly spread (Train 2009, section 9.3.4). The same shifts for every model, so
  # that differences between models do not reflect different draws. The parameters
  # stay at the estimates, which were tuned on the estimation's draws: the more
  # random parameters, the more they fit those draws. On synthetic data with equal
  # covariances (703 experts, 6 constants, 100 draws, 2026-11-09), the model by group
  # had an LR of 61.2 on the estimation's draws and of 39.9 on fresh ones.
  message("Fresh draws: ", length(model_names), " models, ", n_fresh_draws, " sets")
  all_primes <- sort(unique(unlist(primes_list[model_names])))
  set.seed(20261101)
  shifts <- matrix(runif(n_fresh_draws * length(all_primes)), n_fresh_draws,
                   length(all_primes), dimnames = list(NULL, all_primes))
  fresh_values <- matrix(NA_real_, n_fresh_draws, length(model_names),
                         dimnames = list(NULL, model_names))
  # on.exit(): runs when this function ends, also if a later step stops
  cluster <- parallel::makeCluster(n_workers, rscript_args = "--vanilla")
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterExport(cluster, "simulated_likelihood")
  for (model_name in model_names) {
    prepared <- prepared_list[[model_name]]
    primes <- primes_list[[model_name]]
    halton_sequences <- list()
    for (prime in unique(primes)) {
      # halton(): the sequence of a prime; mlogit drops its first 100 elements
      halton_sequences[[as.character(prime)]] <- mlogit:::halton(
        prime, prepared$n_draws * prepared$n_experts, 100
      )
    }
    # parLapply(): lapply on the sessions; they get the arguments after the function
    set_values <- parallel::parLapply(
      cluster, seq_len(n_fresh_draws), fresh_log_likelihood,
      theta = results_list[[model_name]]$coefficients, prepared = prepared,
      halton_sequences = halton_sequences, primes = primes, shifts = shifts
    )
    fresh_values[, model_name] <- unlist(set_values)
  }
  # Minimum and maximum over the sets of draws, not over experts
  fresh_draws <- tibble()
  for (model_name in model_names) {
    fresh_draws <- bind_rows(fresh_draws, tibble(
      row_type = "log-likelihood of the model",
      item = model_name,
      question = NA_character_,
      draws_per_set = prepared_list[[model_name]]$n_draws,
      at_estimation_draws = results_list[[model_name]]$fit$log_likelihood,
      mean_fresh = mean(fresh_values[, model_name]),
      sd_fresh = sd(fresh_values[, model_name]),
      min_fresh = min(fresh_values[, model_name]),
      max_fresh = max(fresh_values[, model_name])
    ))
  }
  for (i in seq_len(nrow(tests))) {
    smaller_name <- tests$smaller[i]
    larger_name <- tests$larger[i]
    fresh_lr <- 2 * (fresh_values[, larger_name] - fresh_values[, smaller_name])
    fresh_draws <- bind_rows(fresh_draws, tibble(
      row_type = "LR of the test, 2 x (larger - smaller)",
      item = paste0(smaller_name, " vs ", larger_name),
      question = tests$question[i],
      draws_per_set = prepared_list[[larger_name]]$n_draws,
      at_estimation_draws = 2 * (results_list[[larger_name]]$fit$log_likelihood -
                                   results_list[[smaller_name]]$fit$log_likelihood),
      mean_fresh = mean(fresh_lr),
      sd_fresh = sd(fresh_lr),
      min_fresh = min(fresh_lr),
      max_fresh = max(fresh_lr)
    ))
  }

  # 3. Nested tests ------------------------------------------------------------
  # Port of the old lr_tests(). LR = 2 (logL_larger - logL_smaller). Classical p:
  # chi-squared with df degrees of freedom, which assumes that the larger model is
  # correctly specified; the standard errors clustered by expert do not.
  # Robust p (Kent 1982; White 1982, section 6): whether or not the model is
  # correctly specified, under the restrictions LR is approximately distributed as
  # sum_i lambda_i Z_i^2, with Z_i independent standard normal and lambda_i the
  # eigenvalues of (R H^{-1} R')^{-1} (R V R'): H^{-1} the inverse Hessian of the
  # larger model, V its clustered covariance matrix, R the derivative of the
  # restrictions. With a correctly specified model every lambda_i is about 1; a mean
  # weight above 1 means that the classical test rejects too often. In a small group
  # the clustered variance is biased downward, which pushes the weights below 1, so
  # that the robust test then rejects too often (old lr_tests(): 0.7 at about 60
  # experts).
  nested_tests <- tibble(
    question = character(0), smaller = character(0), larger = character(0),
    restriction = character(0), added = character(0), lr = numeric(0),
    df = numeric(0), p = numeric(0), p_robust = numeric(0),
    mean_weight = numeric(0), critical_value = numeric(0),
    lr_fresh_mean = numeric(0), lr_fresh_sd = numeric(0),
    boundary_note = character(0)
  )
  for (i in which(is_nested)) {
    smaller_name <- tests$smaller[i]
    larger_name <- tests$larger[i]
    results_larger <- results_list[[larger_name]]
    lr <- 2 * (results_larger$fit$log_likelihood -
                 results_list[[smaller_name]]$fit$log_likelihood)
    R <- restriction_matrices[[paste0(smaller_name, " against ", larger_name)]]
    df <- nrow(R)
    # The eigenvalues of (R H^{-1} R')^{-1} (R V R') are those of the symmetric
    # matrix C'^{-1} (R V R') C^{-1}, with C the Cholesky factor of R H^{-1} R'
    # (chol(): C'C = R H^{-1} R'), so that eigen() returns real values.
    # Restrictions that are not independent (step 0) have no robust test, and nor
    # have those that are nearly so: a standard deviation near 0 but not 0 can leave
    # R H^{-1} R' without a Cholesky factor. tryCatch(): NULL instead of a stop.
    weights <- NA_real_
    critical_value <- NA_real_
    parameters <- names(results_larger$coefficients)
    inverse_hessian <- results_larger$inverse_hessian[parameters, parameters]
    cholesky <- NULL
    if (!tests$degenerate[i]) {
      cholesky <- tryCatch(chol(R %*% inverse_hessian %*% t(R)),
                           error = function(error_message) NULL)
      tests$degenerate[i] <- is.null(cholesky)
    }
    if (!tests$degenerate[i]) {
      V <- results_larger$vcov[parameters, parameters]
      C_inverse <- solve(cholesky)
      weights_matrix <- t(C_inverse) %*% (R %*% V %*% t(R)) %*% C_inverse
      weights <- eigen(weights_matrix, symmetric = TRUE, only.values = TRUE)$values
      # The seed before each test, so that its p-value does not depend on the
      # others. rchisq(df = 1): draws of Z^2.
      set.seed(20261004)
      weighted_sum <- numeric(ROBUST_DRAWS)
      for (weight in weights) {
        weighted_sum <- weighted_sum + weight * rchisq(ROBUST_DRAWS, df = 1)
      }
      # quantile(): the value that the share 1 - alpha of the draws lies below
      critical_value <- quantile(weighted_sum, 1 - alpha, names = FALSE)
    }
    # A negative LR is no test: the larger model has not reached its maximum
    p_classical <- NA_real_
    p_robust <- NA_real_
    if (lr >= 0) {
      p_classical <- pchisq(lr, df = df, lower.tail = FALSE)
      if (!tests$degenerate[i]) {
        p_robust <- mean(weighted_sum >= lr)
      }
    }
    fresh_lr <- 2 * (fresh_values[, larger_name] - fresh_values[, smaller_name])
    # Added elements of L put a variance on the boundary of its space. Every model
    # of fit_mixed_logit() has all random constants, so this does not occur in the
    # first version. grepl(): is this text in the string?
    boundary_note <- NA_character_
    if (grepl("chol.", tests$added[i], fixed = TRUE)) {
      boundary_note <- paste0(
        "The larger model adds random constants, so a variance sits on the boundary ",
        "of its space under the smaller one: LR is then a mixture of chi-squared ",
        "distributions (Self and Liang 1987), and these p-values do not apply"
      )
    }
    nested_tests <- bind_rows(nested_tests, tibble(
      question = tests$question[i],
      smaller = smaller_name,
      larger = larger_name,
      restriction = tests$comparison[i],
      added = tests$added[i],
      lr = lr,
      df = df,
      p = p_classical,
      p_robust = p_robust,
      mean_weight = mean(weights),
      critical_value = critical_value,
      lr_fresh_mean = mean(fresh_lr),
      lr_fresh_sd = sd(fresh_lr),
      boundary_note = boundary_note
    ))
  }
  # p.adjust(): Holm (1979). Over all nested tests declared: the family does not
  # shrink when a model turns out invalid; a test without a valid pair of models
  # counts as one that did not reject.
  p_for_holm <- nested_tests$p_robust
  p_for_holm[!tests$both_valid[is_nested]] <- NA_real_
  nested_tests <- nested_tests %>%
    mutate(p_robust_holm = p.adjust(p_for_holm, method = "holm",
                                    n = nrow(nested_tests))) %>%
    select(question, smaller, larger, restriction, lr, df, p, p_robust,
           p_robust_holm, mean_weight, critical_value, lr_fresh_mean, lr_fresh_sd,
           added, boundary_note)

  # 4. Non-nested tests --------------------------------------------------------
  # Vuong (1989): if the two models fit equally well, the z below is standard
  # normal. It compares the experts' log-likelihoods, which are independent across
  # experts: d_n = log P_n(larger) - log P_n(smaller). The BIC correction takes
  # (K_larger - K_smaller) / 2 * log(G) off the sum.
  non_nested_tests <- tibble(
    question = character(0), smaller = character(0), larger = character(0),
    log_likelihood_difference = numeric(0), difference_per_expert = numeric(0),
    sd_per_expert = numeric(0), vuong_z_bic = numeric(0), vuong_p = numeric(0),
    vuong_favours = character(0), difference_fresh_mean = numeric(0),
    difference_fresh_sd = numeric(0)
  )
  for (i in which(!is_nested)) {
    smaller_name <- tests$smaller[i]
    larger_name <- tests$larger[i]
    log_expert <- list()
    for (model_name in c(smaller_name, larger_name)) {
      # fitted(): the expert's probability, once per decision
      values <- log(as.numeric(fitted(models[[model_name]])))
      is_first <- !duplicated(prepared_list[[model_name]]$expert_idx)
      log_expert[[model_name]] <- values[is_first]
    }
    experts_smaller <- prepared_list[[smaller_name]]$experts
    experts_larger <- prepared_list[[larger_name]]$experts
    difference <- log_expert[[larger_name]][match(experts_smaller, experts_larger)] -
      log_expert[[smaller_name]]
    G <- length(difference)
    K_difference <- results_list[[larger_name]]$fit$n_parameters -
      results_list[[smaller_name]]$fit$n_parameters
    omega <- sqrt(mean(difference^2) - mean(difference)^2)
    vuong_z <- (sum(difference) - K_difference / 2 * log(G)) / (sqrt(G) * omega)
    vuong_p <- 2 * pnorm(-abs(vuong_z))
    vuong_favours <- "neither"
    if (!is.na(vuong_p) && vuong_p < alpha) {
      vuong_favours <- if_else(vuong_z > 0, larger_name, smaller_name)
    }
    fresh_difference <- fresh_values[, larger_name] - fresh_values[, smaller_name]
    non_nested_tests <- bind_rows(non_nested_tests, tibble(
      question = tests$question[i],
      smaller = smaller_name,
      larger = larger_name,
      log_likelihood_difference = sum(difference),
      difference_per_expert = mean(difference),
      sd_per_expert = omega,
      vuong_z_bic = vuong_z,
      vuong_p = vuong_p,
      vuong_favours = vuong_favours,
      difference_fresh_mean = mean(fresh_difference),
      difference_fresh_sd = sd(fresh_difference)
    ))
  }

  # 5. Parsimony ---------------------------------------------------------------
  # From results$fit. Test rows: larger minus smaller, so that a negative difference
  # in AIC, BIC or TIC favours the larger model.
  fit_sheet <- tibble()
  for (model_name in model_names) {
    fit_sheet <- bind_rows(fit_sheet, results_list[[model_name]]$fit %>%
                             mutate(row_type = "model", model = model_name))
  }
  for (i in seq_len(nrow(tests))) {
    fit_smaller <- results_list[[tests$smaller[i]]]$fit
    fit_larger <- results_list[[tests$larger[i]]]$fit
    fit_sheet <- bind_rows(fit_sheet, tibble(
      row_type = "test: larger minus smaller",
      smaller = tests$smaller[i],
      larger = tests$larger[i],
      question = tests$question[i],
      n_parameters = fit_larger$n_parameters - fit_smaller$n_parameters,
      log_likelihood = fit_larger$log_likelihood - fit_smaller$log_likelihood,
      mcfadden_r2 = fit_larger$mcfadden_r2 - fit_smaller$mcfadden_r2,
      aic = fit_larger$aic - fit_smaller$aic,
      bic = fit_larger$bic - fit_smaller$bic,
      trace_hinv_b = fit_larger$trace_hinv_b - fit_smaller$trace_hinv_b,
      tic = fit_larger$tic - fit_smaller$tic
    ))
  }
  fit_sheet <- fit_sheet %>%
    select(row_type, model, smaller, larger, question, n_parameters, log_likelihood,
           mcfadden_r2, aic, bic, trace_hinv_b, tic)

  # 6. Out of sample -----------------------------------------------------------
  cv_of_models <- tibble()
  cv_pairs <- tibble(
    smaller = character(0), larger = character(0), question = character(0),
    experts = numeric(0), gain_per_expert = numeric(0), paired_se = numeric(0),
    gain_in_se = numeric(0), favours = character(0), usable = logical(0)
  )
  cv_note <- "Not run: no cv_models given. The decision uses BIC instead."
  if (length(cv_models) > 0) {
    cv_start_time <- Sys.time()
    # (a) The folds, the same for all models: by expert, as an expert's decisions
    # are not independent, and within each group, so that a small group is in every
    # fold. The group: the columns of constants by group, or the group dummies, from
    # the model of this sample that holds the most of them. So the folds do not
    # depend on which models this call names. union(): the elements of both.
    group_candidates <- CV_GROUP_COLUMNS
    for (results in results_list) {
      group_candidates <- union(group_candidates, results$random$column)
    }
    group_columns <- character(0)
    group_model <- models[[cv_models[1]]]
    for (model_name in names(models)) {
      columns_found <- intersect(group_candidates, names(models[[model_name]]$model))
      if (identical(results_list[[model_name]]$sample_hash,
                    results_list[[cv_models[1]]]$sample_hash) &&
          length(columns_found) > length(group_columns)) {
        group_columns <- columns_found
        group_model <- models[[model_name]]
      }
    }
    stratum <- rep("all", nrow(group_model$model))
    for (column in group_columns) {
      stratum <- paste(stratum, as.numeric(group_model$model[[column]]))
    }
    expert_folds <- tibble(expert = as.character(idx(group_model)$resp_uid),
                           stratum = stratum) %>%
      distinct() %>%
      arrange(expert) %>%
      mutate(fold = NA_integer_)
    check_unique(expert_folds, "expert")
    set.seed(20261105)
    for (stratum_name in unique(expert_folds$stratum)) {
      rows <- which(expert_folds$stratum == stratum_name)
      # rep_len(): 1, ..., 5, 1, ... up to this length; sample(): in random order
      expert_folds$fold[rows] <- sample(rep_len(seq_len(CV_FOLDS), length(rows)))
    }

    # (b) The jobs. Starting values for every parameter, also those fixed by
    # constPar. The call of a model evaluated again at code 2 holds iterlim = 0,
    # which goes. A model whose folds ran on the same estimates and folds is loaded,
    # and so is each fold of a run that was cut off.
    # hash(): a short code that changes whenever its object does.
    jobs <- list()
    cv_saved <- list()
    fold_results <- list()
    for (model_name in cv_models) {
      model <- models[[model_name]]
      fingerprint <- rlang::hash(list(results_list[[model_name]]$coefficients,
                                      expert_folds$expert, expert_folds$fold,
                                      prepared_list[[model_name]]$n_draws))
      saved_file <- file.path(est_dir, model_name, "cross_validation.rds")
      if (file.exists(saved_file)) {
        saved <- readRDS(saved_file)
        if (identical(saved$fingerprint, fingerprint)) {
          message("Cross-validation of ", model_name, ": nothing changed, loaded")
          if (any(!is.na(saved$folds$error))) {
            message("  It holds a failed fold: delete ", saved_file,
                    " to run its folds again")
          }
          cv_saved[[model_name]] <- saved
          next
        }
      }
      arguments <- as.list(model$call)[-1]
      arguments$iterlim <- NULL
      arguments$print.level <- 0
      arguments$start <- setNames(as.numeric(model$coefficients),
                                  names(model$coefficients))
      expert_of_row <- as.character(idx(model)$resp_uid)
      n_resumed <- 0
      for (fold_number in seq_len(CV_FOLDS)) {
        fold_file <- file.path(est_dir, model_name,
                               paste0("cross_validation_fold_", fold_number, ".rds"))
        if (file.exists(fold_file)) {
          saved_fold <- readRDS(fold_file)
          if (identical(saved_fold$fingerprint, fingerprint)) {
            fold_results[[length(fold_results) + 1]] <- saved_fold
            n_resumed <- n_resumed + 1
            next
          }
        }
        experts_held_out <- expert_folds$expert[expert_folds$fold == fold_number]
        is_held_out <- expert_of_row %in% experts_held_out
        jobs[[length(jobs) + 1]] <- list(
          model_name = model_name, fold = fold_number, fingerprint = fingerprint,
          arguments = arguments,
          training_data = model$model[!is_held_out, ],
          held_out_data = model$model[is_held_out, ],
          fold_file = fold_file
        )
      }
      if (n_resumed > 0) {
        message("Cross-validation of ", model_name, ": ", n_resumed, " of ",
                CV_FOLDS, " folds loaded from a run that was cut off")
      }
    }

    # (c) The folds on the sessions, which need mlogit here. clusterCall(): the same
    # call on every session; clusterEvalQ(): the same expression. parLapplyLB(): a
    # new job for whichever session is free, as folds of different models take
    # different times.
    if (length(jobs) > 0) {
      message("Cross-validation: ", length(jobs), " folds on ", n_workers,
              " R sessions")
      parallel::clusterCall(cluster, ".libPaths", .libPaths())
      parallel::clusterEvalQ(cluster,
                             suppressPackageStartupMessages(library(mlogit)))
      fold_results <- c(fold_results,
                        parallel::parLapplyLB(cluster, jobs, cross_validate_fold,
                                              chunk.size = 1))
    }

    # (d) By model: the held-out experts, and how each fold went, in the order of
    # the folds also where some were loaded. Saved also with a failed fold: such an
    # error lies in the data, it would come back on every run after hours of
    # estimation, and the sheet reports it. The files of the single folds go then.
    for (model_name in setdiff(cv_models, names(cv_saved))) {
      per_expert <- tibble()
      folds_of_model <- tibble()
      for (fold_result in fold_results) {
        if (fold_result$model_name != model_name) next
        fingerprint <- fold_result$fingerprint
        per_expert <- bind_rows(per_expert, tibble(
          expert = fold_result$expert,
          fold = fold_result$fold,
          log_likelihood = fold_result$log_likelihood
        ))
        folds_of_model <- bind_rows(folds_of_model, tibble(
          fold = fold_result$fold,
          code = fold_result$code,
          iterations = fold_result$iterations,
          minutes = fold_result$minutes,
          error = fold_result$error
        ))
      }
      cv_saved[[model_name]] <- list(fingerprint = fingerprint,
                                     per_expert = arrange(per_expert, fold),
                                     folds = arrange(folds_of_model, fold))
      saveRDS(cv_saved[[model_name]],
              file.path(est_dir, model_name, "cross_validation.rds"))
      # unlink(): deletes files
      unlink(file.path(est_dir, model_name,
                       paste0("cross_validation_fold_", seq_len(CV_FOLDS), ".rds")))
    }

    # (e) Per model, and per pair: the mean gain per expert, with its standard error
    # over experts, as the two models are compared on the same experts. In a pair
    # that is a test, the test's order.
    for (model_name in cv_models) {
      per_expert <- cv_saved[[model_name]]$per_expert
      folds_of_model <- cv_saved[[model_name]]$folds
      cv_of_models <- bind_rows(cv_of_models, tibble(
        row_type = "model",
        item = model_name,
        question = NA_character_,
        experts = nrow(per_expert),
        folds_converged = sum(folds_of_model$code %in% c(1, 2)),
        held_out_log_likelihood = sum(per_expert$log_likelihood),
        held_out_per_expert = mean(per_expert$log_likelihood),
        minutes = sum(folds_of_model$minutes),
        fold_errors = paste(folds_of_model$error[!is.na(folds_of_model$error)],
                            collapse = "; ")
      ))
    }
    for (i in seq_along(cv_models)) {
      for (j in seq_len(i - 1)) {
        smaller_name <- cv_models[j]
        larger_name <- cv_models[i]
        if (any(tests$smaller == larger_name & tests$larger == smaller_name)) {
          smaller_name <- cv_models[i]
          larger_name <- cv_models[j]
        }
        test_row <- which(tests$smaller == smaller_name & tests$larger == larger_name)
        question_text <- NA_character_
        if (length(test_row) == 1) {
          question_text <- tests$question[test_row]
        }
        paired <- inner_join(cv_saved[[smaller_name]]$per_expert,
                             cv_saved[[larger_name]]$per_expert,
                             by = c("expert", "fold"),
                             suffix = c("_smaller", "_larger"),
                             relationship = "one-to-one") %>%
          mutate(gain = log_likelihood_larger - log_likelihood_smaller)
        gain <- mean(paired$gain)
        paired_se <- sd(paired$gain) / sqrt(nrow(paired))
        favours <- "neither"
        if (nrow(paired) > 1 && gain > paired_se) {
          favours <- larger_name
        }
        if (nrow(paired) > 1 && gain < -paired_se) {
          favours <- smaller_name
        }
        cv_pairs <- bind_rows(cv_pairs, tibble(
          smaller = smaller_name,
          larger = larger_name,
          question = question_text,
          experts = nrow(paired),
          gain_per_expert = gain,
          paired_se = paired_se,
          gain_in_se = gain / paired_se,
          favours = favours,
          usable = all(cv_saved[[smaller_name]]$folds$code %in% c(1, 2)) &&
            all(cv_saved[[larger_name]]$folds$code %in% c(1, 2))
        ))
      }
    }
    cv_minutes <- as.numeric(difftime(Sys.time(), cv_start_time, units = "mins"))
    strata_text <- "not stratified: no group column found"
    if (length(group_columns) > 0) {
      strata_text <- paste0("stratified by ", paste(group_columns, collapse = ", "))
    }
    cv_note <- sprintf(paste0(
      "%d folds by expert, %s, the same folds for all models. Estimated on the other ",
      "folds with mlogit from the full-sample estimates, then the log-likelihood of ",
      "the held-out experts with iterlim = 0, on the draws mlogit gives them. Gain: ",
      "larger minus smaller, mean per expert, with its SE over experts; the folds ",
      "share experts in training, so this SE is rather too small. A model is favoured ",
      "if its gain exceeds one SE. A term without variation in a fold stays at its ",
      "full-sample estimate. A pair is not usable if a fold failed or did not ",
      "converge: the decision then uses BIC. This ",
      "run took %.1f minutes (the column minutes: estimation time of the folds, added ",
      "up, also from an earlier run if loaded)."
    ), CV_FOLDS, strata_text, cv_minutes)
  }
  cross_validation_sheet <- bind_rows(
    cv_of_models,
    cv_pairs %>%
      transmute(row_type = "pair: larger minus smaller",
                item = paste0(smaller, " vs ", larger),
                question = question,
                experts = experts,
                gain_per_expert = gain_per_expert,
                paired_se = paired_se,
                gain_in_se = gain_in_se,
                favours = favours,
                usable = usable)
  )

  # 7. Absolute fit ------------------------------------------------------------
  # model$probabilities: the predicted probabilities over the draws, one row per
  # decision, one column per outcome (the column of that name in model$model has
  # its rows in another order: mlogit writes the reference outcome first). Attribute
  # dummies: 0/1 columns, the same for all alternatives of a decision, that vary
  # across an expert's vignettes. n_distinct(a, b): the distinct pairs.
  fit_summary <- tibble()
  fit_shares <- tibble()
  for (model_name in model_names) {
    model <- models[[model_name]]
    probabilities <- model$probabilities
    outcomes <- colnames(probabilities)
    index <- idx(model)
    chid <- index$chid
    expert_of_row <- as.character(index$resp_uid)
    is_first_row <- !duplicated(chid)
    is_chosen <- as.logical(model$model[[1]])
    # One row per decision, as in probabilities
    chosen_row <- which(is_chosen)[match(chid[is_first_row], chid[is_chosen])]
    chosen_outcome <- as.character(index$alternative)[chosen_row]
    observed <- matrix(0, nrow(probabilities), length(outcomes),
                       dimnames = list(NULL, outcomes))
    observed[cbind(seq_len(nrow(observed)), match(chosen_outcome, outcomes))] <- 1

    attribute_columns <- character(0)
    candidate_columns <- setdiff(names(model$model), c(names(model$model)[1], "idx",
                                                       "probabilities", "linpred"))
    for (column in candidate_columns) {
      values <- as.numeric(model$model[[column]])
      if (!all(values %in% c(0, 1))) next
      is_same_in_decision <- n_distinct(chid, values) == n_distinct(chid)
      varies_within_expert <- n_distinct(expert_of_row, values) >
        n_distinct(expert_of_row)
      if (is_same_in_decision && varies_within_expert) {
        attribute_columns <- c(attribute_columns, column)
      }
    }

    level_names <- "all decisions"
    level_rows <- list(rep(TRUE, nrow(probabilities)))
    for (column in attribute_columns) {
      values <- as.numeric(model$model[[column]])[is_first_row]
      for (level_value in c(0, 1)) {
        level_names <- c(level_names, paste0(column, " = ", level_value))
        level_rows[[length(level_rows) + 1]] <- values == level_value
      }
    }
    shares_of_model <- tibble()
    for (k in seq_along(level_names)) {
      rows <- level_rows[[k]]
      n_rows <- sum(rows)
      # The minimum cell: a share rests on at least MIN_N decisions.
      # colMeans(): the mean of each column.
      observed_share <- rep(NA_real_, length(outcomes))
      predicted_share <- rep(NA_real_, length(outcomes))
      if (n_rows >= MIN_N) {
        observed_share <- colMeans(observed[rows, , drop = FALSE])
        predicted_share <- colMeans(probabilities[rows, , drop = FALSE])
      }
      shares_of_model <- bind_rows(shares_of_model, tibble(
        model = model_name,
        level = level_names[k],
        outcome = outcomes,
        decisions = n_rows,
        observed_percent = 100 * as.numeric(observed_share),
        predicted_percent = 100 * as.numeric(predicted_share),
        difference_pp = predicted_percent - observed_percent
      ))
    }
    overall <- shares_of_model %>%
      filter(level == "all decisions")
    by_attribute <- shares_of_model %>%
      filter(level != "all decisions")
    fit_summary <- bind_rows(fit_summary, tibble(
      model = model_name,
      level = c("root mean squared difference: all decisions",
                "root mean squared difference: by attribute level"),
      outcome = "all outcomes",
      rmse_pp = c(sqrt(mean(overall$difference_pp^2, na.rm = TRUE)),
                  sqrt(mean(by_attribute$difference_pp^2, na.rm = TRUE)))
    ))
    fit_shares <- bind_rows(fit_shares, shares_of_model)
  }
  absolute_fit <- bind_rows(fit_summary, fit_shares)
  # A test compares its two models on the cells both hold: a further dummy adds cells,
  # so two models' root mean squared differences over all their cells do not compare
  rmse_tests <- tibble()
  for (i in seq_len(nrow(tests))) {
    shared_cells <- inner_join(
      fit_shares %>%
        filter(model == tests$smaller[i], level != "all decisions"),
      fit_shares %>%
        filter(model == tests$larger[i], level != "all decisions"),
      by = c("level", "outcome"), suffix = c("_smaller", "_larger"),
      relationship = "one-to-one"
    )
    rmse_tests <- bind_rows(rmse_tests, tibble(
      smaller = tests$smaller[i],
      larger = tests$larger[i],
      rmse_smaller_pp = sqrt(mean(shared_cells$difference_pp_smaller^2,
                                  na.rm = TRUE)),
      rmse_larger_pp = sqrt(mean(shared_cells$difference_pp_larger^2,
                                 na.rm = TRUE))
    ))
  }

  # 8. Substance ---------------------------------------------------------------
  # The AMEs and baseline probabilities in the two models, on the rows they share,
  # relative to the SE in the smaller model, the one kept if the change is small.
  # And the AMEs only the larger model has, as a change from 0, their value in the
  # smaller model, relative to their own SE. Such an AME moves only if its 95%
  # Krinsky-Robb interval also excludes 0, as otherwise it cannot be told apart from
  # the smaller model's 0 (Max, 2026-11-09). Only at one reference state: otherwise
  # a change says nothing about the models.
  ame_changes <- tibble(
    question = character(0), smaller = character(0), larger = character(0),
    quantity = character(0), rows = numeric(0),
    largest_change_pp = numeric(0), median_change_pp = numeric(0),
    largest_change_in_se = numeric(0), median_change_in_se = numeric(0),
    rows_moved = numeric(0), where_largest = character(0)
  )
  if (!is.null(ame_name)) {
    for (i in seq_len(nrow(tests))) {
      ames_smaller <- results_list[[tests$smaller[i]]]$ames[[ame_name]]
      ames_larger <- results_list[[tests$larger[i]]]$ames[[ame_name]]
      if (is.null(ames_smaller) || is.null(ames_larger) ||
          !identical(ames_smaller$at, ames_larger$at)) {
        next
      }
      # A model has the effects of its terms, so the sets of effects can differ. An
      # effect that both have counts only if both define it alike; an effect of the
      # same name that the two define differently does not count at all.
      same_effects <- character(0)
      for (effect_name in intersect(names(ames_smaller$changes),
                                    names(ames_larger$changes))) {
        if (identical(ames_smaller$changes[[effect_name]],
                      ames_larger$changes[[effect_name]])) {
          same_effects <- c(same_effects, effect_name)
        }
      }
      added_effects <- setdiff(names(ames_larger$changes),
                               names(ames_smaller$changes))
      shared_ames <- inner_join(
        ames_smaller$table %>%
          filter(effect %in% same_effects) %>%
          transmute(where = paste(profile, effect, outcome, sep = ", "),
                    value_smaller = ame, se_smaller = se),
        ames_larger$table %>%
          transmute(where = paste(profile, effect, outcome, sep = ", "),
                    value_larger = ame),
        by = "where", relationship = "one-to-one"
      )
      shared_baselines <- inner_join(
        ames_smaller$baseline %>%
          transmute(where = paste(profile, outcome, sep = ", "),
                    value_smaller = probability, se_smaller = se),
        ames_larger$baseline %>%
          transmute(where = paste(profile, outcome, sep = ", "),
                    value_larger = probability),
        by = "where", relationship = "one-to-one"
      )
      rows_of_quantity <- list(
        "AME" = shared_ames %>%
          transmute(where, change = abs(value_larger - value_smaller),
                    se = se_smaller, excludes_0 = TRUE),
        "baseline probability" = shared_baselines %>%
          transmute(where, change = abs(value_larger - value_smaller),
                    se = se_smaller, excludes_0 = TRUE),
        "AME the larger model adds" = ames_larger$table %>%
          filter(effect %in% added_effects) %>%
          transmute(where = paste(profile, effect, outcome, sep = ", "),
                    change = abs(ame), se = se,
                    excludes_0 = ci_low_kr > 0 | ci_high_kr < 0)
      )
      for (quantity_name in names(rows_of_quantity)) {
        compared <- rows_of_quantity[[quantity_name]] %>%
          mutate(change_in_se = if_else(se > 0, change / se, NA_real_))
        if (nrow(compared) == 0) next
        ame_changes <- bind_rows(ame_changes, tibble(
          question = tests$question[i],
          smaller = tests$smaller[i],
          larger = tests$larger[i],
          quantity = quantity_name,
          rows = nrow(compared),
          largest_change_pp = 100 * max(compared$change, na.rm = TRUE),
          median_change_pp = 100 * median(compared$change, na.rm = TRUE),
          largest_change_in_se = max(compared$change_in_se, na.rm = TRUE),
          median_change_in_se = median(compared$change_in_se, na.rm = TRUE),
          rows_moved = sum(compared$change >= ame_threshold & compared$excludes_0,
                           na.rm = TRUE),
          # which.max(): the position of the largest value
          where_largest = compared$where[which.max(compared$change)]
        ))
      }
    }
  }

  # 9. The blocks of the final model -------------------------------------------
  # Port of the old wald_test(), for exclusion restrictions: W = theta' V^{-1} theta
  # over the block's coefficients, with V clustered by expert, chi-squared with as
  # many degrees of freedom as coefficients.
  block_tests <- tibble()
  for (block_name in names(block_parameters)) {
    results <- results_list[[final_model]]
    parameters <- block_parameters[[block_name]]
    theta_block <- results$coefficients[parameters]
    V_block <- results$vcov[parameters, parameters, drop = FALSE]
    statistic <- as.numeric(t(theta_block) %*% solve(V_block) %*% theta_block)
    block_tests <- bind_rows(block_tests, tibble(
      model = final_model,
      model_valid = validity$valid[validity$model == final_model],
      block = block_name,
      statistic = statistic,
      df = length(parameters),
      p = pchisq(statistic, df = length(parameters), lower.tail = FALSE),
      parameters = paste(parameters, collapse = ", ")
    ))
  }

  # 10. The decision -----------------------------------------------------------
  # (a) All key numbers, by test
  by_test <- c("smaller", "larger")
  decision <- tests %>%
    select(question, smaller, larger, comparison, both_valid, smaller_valid,
           larger_valid, degenerate) %>%
    left_join(nested_tests %>%
                select(smaller, larger, lr, df, p, p_robust, p_robust_holm,
                       mean_weight, critical_value, lr_fresh_mean, lr_fresh_sd),
              by = by_test, relationship = "one-to-one") %>%
    left_join(non_nested_tests %>%
                select(smaller, larger, vuong_z_bic, vuong_p, vuong_favours,
                       difference_fresh_mean, difference_fresh_sd),
              by = by_test, relationship = "one-to-one") %>%
    left_join(fit_sheet %>%
                filter(row_type == "test: larger minus smaller") %>%
                select(smaller, larger, delta_k = n_parameters,
                       delta_log_likelihood = log_likelihood, delta_aic = aic,
                       delta_bic = bic, delta_tic = tic),
              by = by_test, relationship = "one-to-one") %>%
    left_join(cv_pairs %>%
                select(smaller, larger, cv_gain_per_expert = gain_per_expert,
                       cv_paired_se = paired_se, cv_favours = favours,
                       cv_usable = usable),
              by = by_test, relationship = "one-to-one") %>%
    left_join(ame_changes %>%
                filter(quantity == "AME") %>%
                select(smaller, larger, ame_rows_shared = rows,
                       ame_rows_moved = rows_moved,
                       ame_largest_change_pp = largest_change_pp,
                       ame_median_change_pp = median_change_pp,
                       ame_largest_change_in_se = largest_change_in_se,
                       ame_where_largest = where_largest),
              by = by_test, relationship = "one-to-one") %>%
    left_join(ame_changes %>%
                filter(quantity == "AME the larger model adds") %>%
                select(smaller, larger, ame_rows_added = rows,
                       ame_added_rows_moved = rows_moved,
                       ame_added_largest_pp = largest_change_pp,
                       ame_added_where_largest = where_largest),
              by = by_test, relationship = "one-to-one") %>%
    left_join(ame_changes %>%
                filter(quantity == "baseline probability") %>%
                select(smaller, larger,
                       baseline_largest_change_pp = largest_change_pp),
              by = by_test, relationship = "one-to-one") %>%
    left_join(rmse_tests, by = by_test, relationship = "one-to-one")

  # (b) A verdict per criterion. A criterion that favours a model names it.
  decision <- decision %>%
    mutate(
      verdict_validity = case_when(
        both_valid ~ "both valid",
        !smaller_valid & !larger_valid ~ paste0(smaller, " and ", larger,
                                                " not valid"),
        !smaller_valid ~ paste0(smaller, " not valid"),
        TRUE ~ paste0(larger, " not valid")
      ),
      test_rejects = !is.na(p_robust_holm) & p_robust_holm < alpha,
      verdict_test = case_when(
        comparison == "non-nested" ~ "no nested test",
        is.na(p_robust_holm) ~ "not compared",
        test_rejects ~ "rejects the smaller model",
        TRUE ~ "does not reject"
      ),
      survives_fresh = !is.na(lr_fresh_mean) & lr_fresh_mean > critical_value,
      verdict_fresh_draws = case_when(
        comparison == "non-nested" ~ "no nested test",
        is.na(p_robust_holm) ~ "not compared",
        survives_fresh ~ "survives",
        TRUE ~ "does not survive"
      ),
      verdict_vuong = if_else(comparison == "non-nested", vuong_favours,
                              "no non-nested test"),
      verdict_aic = if_else(delta_aic < 0, larger, smaller),
      verdict_bic = if_else(delta_bic < 0, larger, smaller),
      verdict_tic = if_else(delta_tic < 0, larger, smaller),
      cv_usable = !is.na(cv_usable) & cv_usable,
      verdict_cross_validation = case_when(
        is.na(cv_favours) ~ "not run",
        !cv_usable ~ "not usable: a fold failed or did not converge",
        TRUE ~ cv_favours
      ),
      # What the rule's parsimony criterion favours: cross-validation, or BIC without
      parsimony_favours = if_else(cv_usable, cv_favours, verdict_bic),
      verdict_absolute_fit = case_when(
        is.na(rmse_smaller_pp) | is.na(rmse_larger_pp) ~ "not available",
        rmse_larger_pp < rmse_smaller_pp ~ larger,
        TRUE ~ smaller
      ),
      # The rows that move: of the effects both models share, and of those only the
      # larger one has
      ame_available = !is.na(ame_rows_moved) | !is.na(ame_added_rows_moved),
      ame_moved = coalesce(ame_rows_moved, 0) + coalesce(ame_added_rows_moved, 0),
      verdict_ame = case_when(
        !ame_available ~ "not available",
        ame_moved > 0 ~ "moved",
        TRUE ~ "did not move"
      ),
      alpha = alpha,
      ame_threshold = ame_threshold,
      proposal = NA_character_,
      final_decision = NA_character_,
      reason = NA_character_
    )

  # (c) The rule of the top of this file, and its reason in words
  for (i in seq_len(nrow(decision))) {
    row <- decision[i, ]
    parsimony_name <- "BIC"
    parsimony_text <- sprintf("BIC of %s minus %s %.1f (cross-validation: %s)",
                              row$larger, row$smaller, row$delta_bic,
                              row$verdict_cross_validation)
    if (row$cv_usable) {
      parsimony_name <- "cross-validation"
      parsimony_text <- sprintf(
        "held-out log-likelihood of %s minus %s %.3f per expert (paired SE %.3f)",
        row$larger, row$smaller, row$cv_gain_per_expert, row$cv_paired_se
      )
    }
    if (!row$both_valid) {
      proposal <- "not compared"
      reasons <- character(0)
      for (model_name in c(row$smaller, row$larger)) {
        why <- validity$why[validity$model == model_name]
        if (why != "") {
          reasons <- c(reasons, paste0(model_name, ": ", why))
        }
      }
      reason <- paste0("Not valid, so no part in a decision. ",
                       paste(reasons, collapse = ". "), ".")
    } else if (row$degenerate) {
      proposal <- "not compared"
      reason <- sprintf(paste0(
        "The robust test cannot be computed: its restrictions are not independent ",
        "at the estimates of %s, as when one random constant has a standard ",
        "deviation near 0 in several groups. LR %.1f, df %.0f, classical p %s."
      ), row$larger, row$lr, row$df, p_text(row$p))
    } else if (row$comparison == "non-nested") {
      vuong_text <- sprintf("Vuong's z with BIC correction %.2f (p %s) favours %s",
                            row$vuong_z_bic, p_text(row$vuong_p), row$vuong_favours)
      if (row$vuong_favours != "neither" &&
          row$vuong_favours == row$parsimony_favours) {
        proposal <- paste0("prefer ", row$vuong_favours)
        reason <- sprintf("%s, and %s agrees: %s.", vuong_text, parsimony_name,
                          parsimony_text)
      } else {
        proposal <- "data cannot decide"
        reason <- sprintf("%s; %s favours %s: %s.", vuong_text, parsimony_name,
                          row$parsimony_favours, parsimony_text)
      }
    } else {
      test_text <- sprintf(
        "LR %.1f, df %.0f, p %s, robust p %s, after Holm %s, mean weight %.2f",
        row$lr, row$df, p_text(row$p), p_text(row$p_robust),
        p_text(row$p_robust_holm), row$mean_weight
      )
      fresh_text <- sprintf(
        "mean LR on fresh draws %.1f (SD %.1f), the robust test's critical value %.1f",
        row$lr_fresh_mean, row$lr_fresh_sd, row$critical_value
      )
      ame_text <- "AMEs not compared"
      if (row$ame_available) {
        ame_parts <- character(0)
        if (!is.na(row$ame_rows_shared)) {
          ame_parts <- c(ame_parts, sprintf(
            "largest change in a shared AME %.2f pp, at %s, of %.0f rows",
            row$ame_largest_change_pp, row$ame_where_largest, row$ame_rows_shared
          ))
        }
        if (!is.na(row$ame_rows_added)) {
          ame_parts <- c(ame_parts, sprintf(
            "largest AME that %s adds %.2f pp, at %s, of %.0f rows", row$larger,
            row$ame_added_largest_pp, row$ame_added_where_largest, row$ame_rows_added
          ))
        }
        ame_text <- paste(ame_parts, collapse = "; ")
      }
      if (!row$test_rejects) {
        proposal <- "keep smaller"
        reason <- sprintf("The robust test does not reject the smaller model (%s).",
                          test_text)
      } else if (!row$survives_fresh) {
        proposal <- "keep smaller: gain does not survive fresh draws"
        reason <- sprintf(paste0("The robust test rejects (%s), but the gain is ",
                                 "within the noise of the draws: %s."),
                          test_text, fresh_text)
      } else if (row$parsimony_favours != row$larger) {
        proposal <- "keep smaller: tests and parsimony disagree"
        reason <- sprintf(paste0("The robust test rejects (%s) and the gain survives ",
                                 "(%s), but the larger model does not pay for its ",
                                 "parameters: %s."),
                          test_text, fresh_text, parsimony_text)
      } else if (row$ame_available && row$ame_moved == 0) {
        proposal <- "keep smaller as main; report larger as robustness"
        reason <- sprintf(paste0("All favour the larger model (%s; %s; %s), but no ",
                                 "AME moves by %.1f pp or more: %s."),
                          test_text, fresh_text, parsimony_text,
                          100 * ame_threshold, ame_text)
      } else {
        proposal <- "accept larger"
        reason <- sprintf("All favour the larger model: %s; %s; %s; %s.",
                          test_text, fresh_text, parsimony_text, ame_text)
      }
      # BIC counts every parameter at log(G): say so if it alone would have said no
      if (row$cv_usable && row$verdict_bic != row$larger) {
        reason <- paste0(reason, sprintf(" BIC rises by %.1f.", row$delta_bic))
      }
      if (!is.na(row$mean_weight) && row$mean_weight < LOW_MEAN_WEIGHT) {
        reason <- paste0(reason, sprintf(paste0(
          " Mean weight %.2f: a restriction rests on few experts, so the robust ",
          "p-value is too small; compare the classical one."
        ), row$mean_weight))
      }
    }
    decision$proposal[i] <- proposal
    decision$reason[i] <- reason
  }
  # What a reader wants first comes first
  decision <- decision %>%
    select(question, smaller, larger, proposal, final_decision, reason, comparison,
           verdict_validity, verdict_test, verdict_fresh_draws,
           verdict_cross_validation, verdict_bic, verdict_aic, verdict_tic,
           verdict_vuong, verdict_absolute_fit, verdict_ame, lr, df, p, p_robust,
           p_robust_holm, mean_weight, critical_value, lr_fresh_mean, lr_fresh_sd,
           vuong_z_bic, vuong_p, difference_fresh_mean, difference_fresh_sd, delta_k,
           delta_log_likelihood, delta_aic, delta_bic, delta_tic, cv_gain_per_expert,
           cv_paired_se, rmse_smaller_pp, rmse_larger_pp, ame_rows_shared,
           ame_rows_moved, ame_largest_change_pp, ame_median_change_pp,
           ame_largest_change_in_se, ame_rows_added, ame_added_rows_moved,
           ame_added_largest_pp, baseline_largest_change_pp, alpha, ame_threshold)

  # 11. Save and print ---------------------------------------------------------
  ame_note <- "Not run: no ame_name given."
  if (!is.null(ame_name)) {
    ame_note <- paste0(
      "AME set ", ame_name, ". Absolute change between the two models in ",
      "percentage points, on the rows they share and with effects defined alike ",
      "(changes); in_se: relative to the SE in the smaller model. AME the larger ",
      "model adds: an effect the smaller model does not have, whose value there is ",
      "0, so its change is the AME itself; in_se: relative to its own SE. ",
      "rows_moved: rows that change by ", 100 * ame_threshold, " pp or more, and ",
      "for an AME the larger model adds, whose 95% Krinsky-Robb interval excludes ",
      "0; the rule counts the rows of both kinds of AME, not the baselines. A test ",
      "without ",
      "a row: a model does not have this set, or has it at another reference state ",
      "(at)."
    )
  }
  sheets <- list(
    decision = decision,
    validity = add_note(validity, sprintf(paste0(
      "Valid: code 1 or 2, scaled gradient (g' H^-1 g) at most %g, Hessian positive ",
      "definite, condition number at most %g (it depends on the units of the ",
      "regressors), and a log-likelihood not below any model of this folder that it ",
      "nests (gap_to_nested: to the highest of them, in log points). An invalid ",
      "model takes no part in a decision."
    ), MAX_SCALED_GRADIENT, MAX_CONDITION_NUMBER)),
    fit = add_note(fit_sheet, paste0(
      "BIC counts experts. TIC replaces the number of parameters by tr(H^-1 B). ",
      "Test rows: larger minus smaller, so a negative difference in AIC, BIC or TIC ",
      "favours the larger model."
    )),
    nested_tests = add_note(nested_tests, sprintf(paste0(
      "LR on the estimation's draws. Classical p: chi-squared with df. Robust p: a ",
      "weighted sum of chi-squared(1) (Kent 1982; White 1982), simulated with %d ",
      "draws, so 0 means below %g. Holm over all %d nested tests declared; a test ",
      "with an invalid model, or with restrictions that are not independent at the ",
      "estimates (no robust p), counts as not rejected. critical_value: the %g ",
      "quantile of the robust test's distribution, not adjusted for Holm; a gain ",
      "survives fresh draws if the mean LR on them is above it. The parameters stay ",
      "at the estimates on the fresh draws, so the mean LR tends to lie below the LR."
    ), ROBUST_DRAWS, 1 / ROBUST_DRAWS, nrow(nested_tests), 1 - alpha)),
    non_nested_tests = add_note(non_nested_tests, paste0(
      "Vuong (1989) with the BIC correction, from the log-likelihood of each expert: ",
      "a positive z favours the model in the column larger. With many parameters it ",
      "can over-reject (Shi 2015), so it never decides alone; its normal limit is for ",
      "models that do not overlap. If cross-validation is not available, BIC adds no ",
      "information of its own: z and the difference in BIC have the same sign by ",
      "construction."
    )),
    fresh_draws = add_note(fresh_draws, sprintf(paste0(
      "%d sets of Halton draws, each shifted at random: (u + runif(1)) %%%% 1 per ",
      "prime, the same shifts for every model, one shift per prime, so that constants ",
      "by group and common constants see the same draws. At the estimated parameters."
    ), n_fresh_draws)),
    cross_validation = add_note(cross_validation_sheet, cv_note),
    absolute_fit = add_note(absolute_fit, paste0(
      "Predicted (model$probabilities) against observed shares of each ",
      "recommendation, in percent; shares rest on at least ", MIN_N, " decisions. A ",
      "model that contains a dummy fits its cells closely by construction, so a small ",
      "difference shows little. A model's root mean squared difference covers the ",
      "dummies it holds; the decision sheet compares two models on the cells they ",
      "share."
    )),
    ame_changes = add_note(ame_changes, ame_note),
    block_tests = add_note(block_tests, paste0(
      "Wald test in the final model, with the covariance matrix clustered by expert, ",
      "chi-squared with df. No row: no final model or no blocks given."
    ))
  )
  output_dir <- file.path(est_dir, "model_comparison")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(sheets, file.path(output_dir, "model_comparison.rds"))

  # A filled final_decision is the researchers' work, which this run would overwrite
  xlsx_file <- file.path(output_dir, "model_comparison.xlsx")
  if (file.exists(xlsx_file)) {
    previous <- read_excel(xlsx_file, sheet = "decision", col_types = "text")
    if (any(!is.na(previous$final_decision))) {
      copy_file <- file.path(output_dir, paste0("model_comparison_",
                                                format(Sys.time(), "%Y%m%d_%H%M%S"),
                                                ".xlsx"))
      file.copy(xlsx_file, copy_file)
      message("The earlier file held final decisions: kept as ", copy_file)
    }
  }
  # Six significant digits to read; the .rds keeps every digit
  sheets_to_read <- sheets
  for (sheet_name in names(sheets_to_read)) {
    for (column in names(sheets_to_read[[sheet_name]])) {
      if (is.numeric(sheets_to_read[[sheet_name]][[column]])) {
        sheets_to_read[[sheet_name]][[column]] <- signif(
          sheets_to_read[[sheet_name]][[column]], 6
        )
      }
    }
  }
  # The decision sheet is read first: the question and the models stay in view, and
  # the reason wraps in a wide column. xl_sheet(): a table with the layout of its
  # sheet; xl_col_spec(): the width and format of columns.
  wrapped_text <- xl_format(xl_align(wrap = TRUE, vertical = "top"))
  sheets_to_read$decision <- xl_sheet(
    sheets_to_read$decision,
    cols = list(xl_col_spec(c("question", "proposal"), width = 40,
                            format = wrapped_text),
                xl_col_spec("final_decision", width = 30, format = wrapped_text),
                xl_col_spec("reason", width = 110, format = wrapped_text)),
    freeze = list(row = 1, col = 3)
  )
  write_xlsx(sheets_to_read, xlsx_file)

  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  message("Model comparison written to ", output_dir, " (", round(minutes, 1),
          " minutes)")
  print(decision %>%
          select(smaller, larger, proposal, verdict_test, verdict_fresh_draws,
                 verdict_cross_validation, verdict_bic, verdict_vuong, verdict_ame),
        n = Inf, width = Inf)
  for (i in seq_len(nrow(decision))) {
    cat("\n", decision$smaller[i], " vs ", decision$larger[i], " (",
        decision$question[i], "): ", decision$proposal[i], "\n  ",
        decision$reason[i], "\n", sep = "")
  }
  sheets
}
