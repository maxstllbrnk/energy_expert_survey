################################################################################
# post_estimation.R — everything that follows an estimation: standard errors
#                     clustered by expert, Krinsky-Robb draws, standard
#                     deviations and correlations of the constants, fit and
#                     diagnostics
# Inputs : a fitted model (fit_mixed_logit())
# Outputs: <est_dir>/<name>/results.rds, aggregates only
#          (docs/results_contract.md); <est_dir>/<name>/results.xlsx, to read
# Checks : the fast likelihood reproduces mlogit's at the estimates; the Hessian
#          is positive definite
################################################################################

#' Standard errors clustered by expert, the constants and fit of one model. Loads
#' the saved results if the coefficients have not changed.
#'
#' The cluster-robust (sandwich) covariance matrix, clustered by expert:
#'   V = G / (G - 1) * H^{-1} B H^{-1}
#' H     Hessian of -log L at the estimates ("bread"). mlogit provides none for
#'       the mixed logit, so H comes from central differences of the gradient,
#'       column by column on n_workers R sessions.
#' B     sum over experts of the outer product of the expert's score ("meat").
#' G     the number of experts; G / (G - 1) as in Stata and sandwich::vcovCL.
#'
#' Krinsky-Robb draws from N(theta_hat, V) give the standard errors and
#' percentile intervals of everything that is a nonlinear function of the
#' parameters: the standard deviations and correlations of the constants here,
#' the AMEs in compute_ames(). The percentile intervals stay within the bounds (a
#' standard deviation above 0, a correlation between -1 and 1) and show skewness,
#' which matters most in the smaller groups.
#'
#' @param model      fitted model, from fit_mixed_logit()
#' @param est_dir    the folder of the estimation script
#' @param label      what the model is, plain text, for tables and the Excel
#'                   file; a new label needs force = TRUE
#' @param n_draws_kr Krinsky-Robb draws
#' @param h          step size of the numerical derivatives
#' @param n_workers  R sessions for the Hessian
#' @param force      TRUE: compute even if the coefficients have not changed
#' @return the results, a list (docs/results_contract.md)
post_estimation <- function(model, est_dir, label = "", n_draws_kr = N_DRAWS_KR,
                            h = 1e-4, n_workers = N_WORKERS, force = FALSE) {
  name <- model$name
  model_dir <- file.path(est_dir, name)
  results_file <- file.path(model_dir, "results.rds")
  theta_hat <- coef(model)
  fingerprint <- rlang::hash(list(theta_hat, n_draws_kr, h))
  if (!force && file.exists(results_file)) {
    saved_results <- readRDS(results_file)
    if (identical(saved_results$fingerprint, fingerprint)) {
      message("Model ", name, ": nothing changed, results loaded from ",
              results_file)
      # where they are now, in case the folder has moved since
      saved_results$folder <- model_dir
      refresh_workbook(saved_results)
      return(saved_results)
    }
  }
  start_time <- Sys.time()
  K <- length(theta_hat)

  # (a) The fast likelihood reproduces mlogit's, so it uses the estimation's data
  # and draws
  prepared <- prepare_likelihood(model)
  at_estimates <- simulated_likelihood(theta_hat, prepared)
  difference <- abs(at_estimates$log_likelihood - as.numeric(logLik(model)))
  if (difference > 1e-6) {
    stop("Model ", name, ": the fast likelihood differs from mlogit's by ",
         difference,
         ". Estimate models with fit_mixed_logit(), which re-evaluates at code 2")
  }

  # (b) Meat: one score per expert. Bread: the inverse of the numerical Hessian,
  # one column per parameter, on n_workers R sessions. makeCluster(): starts
  # them, with --vanilla so that each is ready at once; the likelihood needs only
  # base R. parLapply(): lapply on the sessions. on.exit(): runs when this
  # function ends, also with an error.
  message("Model ", name, ": Hessian, ", 2 * K, " evaluations on ", n_workers,
          " R sessions")
  meat <- crossprod(at_estimates$scores)
  G <- nrow(at_estimates$scores)
  hessian_column <- function(k) {
    step <- replace(numeric(length(theta_hat)), k, h)
    names(step) <- names(theta_hat)
    up <- colSums(simulated_likelihood(theta_hat + step, prepared)$scores)
    down <- colSums(simulated_likelihood(theta_hat - step, prepared)$scores)
    -(up - down) / (2 * h)
  }
  cluster <- parallel::makeCluster(n_workers, rscript_args = "--vanilla")
  on.exit(parallel::stopCluster(cluster))
  parallel::clusterExport(cluster, "simulated_likelihood")
  hessian_columns <- parallel::parLapply(cluster, seq_len(K), hessian_column)
  H <- matrix(unlist(hessian_columns), K, K,
              dimnames = list(names(theta_hat), names(theta_hat)))
  H <- (H + t(H)) / 2
  eigenvalues <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  if (any(eigenvalues <= 0)) {
    stop("Model ", name, ": the numerical Hessian is not positive definite")
  }
  bread <- solve(H)
  V <- G / (G - 1) * bread %*% meat %*% bread
  dimnames(V) <- dimnames(H)

  z_critical <- qnorm(0.975)
  coefficient_table <- tibble(
    parameter = names(theta_hat),
    estimate = as.numeric(theta_hat),
    se = sqrt(diag(V))
  ) %>%
    mutate(z = estimate / se,
           p = 2 * pnorm(-abs(z)),
           ci_low = estimate - z_critical * se,
           ci_high = estimate + z_critical * se)

  # (c) Krinsky-Robb draws. The seed is set right here, so that the draws do not
  # depend on what ran before (project profile).
  set.seed(20261003)
  theta_draws <- MASS::mvrnorm(n_draws_kr, mu = theta_hat, Sigma = V)

  # (d) Standard deviations and correlations of the constants: Omega = L L',
  # sd_j = sqrt(Omega_jj), cor_jk = Omega_jk / (sd_j sd_k). Each correlation once
  # and only within a set of constants: with constants by group, those between
  # two groups are 0 by construction.
  random <- model$fit_info$random
  K_r <- nrow(random)
  pairs <- tibble()
  for (i in seq_len(K_r)) {
    for (j in seq_len(i - 1)) {
      if (random$column[i] == random$column[j]) {
        pairs <- bind_rows(pairs, tibble(i = i, j = j))
      }
    }
  }
  constants_of <- function(theta) {
    L <- matrix(0, K_r, K_r)
    L[cbind(prepared$cholesky_i, prepared$cholesky_j)] <-
      theta[prepared$cholesky_names]
    Omega <- L %*% t(L)
    sds <- sqrt(diag(Omega))
    correlations <- Omega[cbind(pairs$i, pairs$j)] /
      (sds[pairs$i] * sds[pairs$j])
    c(sds, correlations)
  }
  constant_names <- c(
    paste0("sd.", random$name),
    paste0("cor.", random$name[pairs$j], ":", random$name[pairs$i])
  )
  estimate_constants <- constants_of(theta_hat)
  draws_constants <- matrix(NA_real_, n_draws_kr, length(estimate_constants))
  for (s in seq_len(n_draws_kr)) {
    draws_constants[s, ] <- constants_of(theta_draws[s, ])
  }
  constant_table <- tibble(
    parameter = constant_names,
    type = if_else(startsWith(constant_names, "sd."), "sd", "cor"),
    estimate = estimate_constants,
    # apply(draws, 2, f): f applied to each column, i.e. over the draws
    se = apply(draws_constants, 2, sd),
    ci_low_kr = apply(draws_constants, 2, quantile, probs = 0.025),
    ci_high_kr = apply(draws_constants, 2, quantile, probs = 0.975)
  )

  # (e) Fit. McFadden's R^2 compares with a model that has constants only, which
  # predicts the observed share of each recommendation. AIC and BIC to compare
  # models that are not nested; BIC counts experts, as the panel likelihood is a
  # product over experts. TIC replaces the number of parameters by tr(H^{-1} B),
  # which equals it in a correctly specified model (Takeuchi 1976).
  outcomes <- model$fit_info$outcomes
  chosen_outcome <-
    as.character(idx(model)$alternative)[as.logical(model$model[[1]])]
  choice_counts <- table(factor(chosen_outcome, levels = outcomes))
  n_choices <- sum(choice_counts)
  log_likelihood <- at_estimates$log_likelihood
  counts_above_0 <- choice_counts[choice_counts > 0]
  log_likelihood_constants_only <- sum(counts_above_0 *
                                         log(counts_above_0 / n_choices))
  trace_hinv_b <- sum(diag(bread %*% meat))
  fit <- tibble(
    n_choices = n_choices,
    n_experts = G,
    n_parameters = K,
    n_draws_per_expert = model$fit_info$n_draws,
    log_likelihood = log_likelihood,
    log_likelihood_constants_only = log_likelihood_constants_only,
    mcfadden_r2 = 1 - log_likelihood / log_likelihood_constants_only,
    aic = -2 * log_likelihood + 2 * K,
    bic = -2 * log_likelihood + log(G) * K,
    trace_hinv_b = trace_hinv_b,
    tic = -2 * log_likelihood + 2 * trace_hinv_b
  )

  # (f) Diagnostics. A code of 1 or 2 does not prove a maximum: compare the
  # log-likelihood with that of a model this one nests (compare_models()). The
  # scaled gradient g' H^{-1} g is close to 0 at a maximum, whatever the units of
  # the parameters.
  gradient <- colSums(at_estimates$scores)
  diagnostics <- list(
    code = model$est.stat$code,
    iterations = model$est.stat$nb.iter,
    max_gradient = max(abs(gradient)),
    scaled_gradient = as.numeric(t(gradient) %*% bread %*% gradient),
    smallest_eigenvalue = min(eigenvalues),
    condition_number = max(eigenvalues) / min(eigenvalues),
    start_from = model$fit_info$start_from,
    log_likelihood_start = model$fit_info$log_likelihood_start,
    minutes_estimation = model$fit_info$minutes
  )

  # (g) Save. Nothing at the level of experts or vignettes: the model stays in
  # model.rds. The formula is saved as text, because a formula object can carry
  # the environment it was made in. The hash of the experts lets compare_models()
  # check that two models share one sample without storing who they are. Outside
  # a git repository, system() returns nothing, without an error.
  git_commit <- suppressWarnings(tryCatch(
    system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE),
    error = function(e) character(0)
  ))
  if (length(git_commit) == 0) {
    git_commit <- NA_character_
  }
  results <- list(
    name = name,
    label = label,
    folder = model_dir,
    formula = paste(deparse(model$formula), collapse = " "),
    reference = model$fit_info$reference,
    outcomes = outcomes,
    random = random,
    fixed_parameters = model$fit_info$fixed_parameters,
    coefficients = theta_hat,
    vcov = V,
    inverse_hessian = bread,
    meat = meat,
    coefficient_table = coefficient_table,
    constant_table = constant_table,
    theta_draws = theta_draws,
    fit = fit,
    diagnostics = diagnostics,
    choice_counts = choice_counts,
    sample_hash = rlang::hash(sort(prepared$experts)),
    ames = list(),
    n_draws_kr = n_draws_kr,
    fingerprint = fingerprint,
    git_commit = git_commit,
    date = Sys.Date()
  )
  saveRDS(results, results_file)
  write_results_excel(results)
  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  message("Model ", name, ": results written to ", model_dir, " (",
          round(minutes, 1), " minutes)")
  results
}

#' The results to read: one Excel workbook per model. The sheets to read come
#' first, rounded; the long sheets follow with grey tabs and every digit, for
#' reuse. Called by post_estimation() and compute_ames(); the file is rewritten
#' each time.
#'   overview                    the model, its fit and how it converged
#'   coefficients_by_technology  one row per regressor, one column per technology
#'   constants_sd, constants_correlations    of the random constants
#'   pp_<name>                   one row per effect, one column per outcome, in
#'                               percentage points; one per call of compute_ames()
#'   effect_definitions          the reference state and the changes of each call
#'   choice_counts               the recommendations in the sample
#'   fit, coefficients, ame_<name>, baseline_<name>    the long sheets
#'
#' @param results the results, a list
#' @return nothing; writes <results$folder>/results.xlsx
write_results_excel <- function(results) {
  fit <- results$fit
  diagnostics <- results$diagnostics
  coefficient_table <- results$coefficient_table
  random <- results$random

  # The outcomes in the order and with the names of the tables (labels.R); an
  # outcome without a label keeps its name. intersect(), setdiff(): the elements
  # in both, in the first only. setNames(): the values, with these names.
  outcomes <- c(intersect(names(TECHNOLOGY_LABELS), results$outcomes),
                setdiff(results$outcomes, names(TECHNOLOGY_LABELS)))
  outcome_labels <- setNames(outcomes, outcomes)
  has_label <- outcomes %in% names(TECHNOLOGY_LABELS)
  outcome_labels[has_label] <- TECHNOLOGY_LABELS[outcomes[has_label]]

  # (a) The overview: fit, then how the estimation ended, then what is where.
  # str_squish(): one space between words, as deparse() breaks a long formula.
  # coalesce(): the first value that is not missing.
  sheet_guide <- paste0(
    "coefficients_by_technology: utility relative to ",
    outcome_labels[[results$reference]], ", estimate and standard error ",
    "clustered by expert. pp_<name>: the probabilities at the reference state ",
    "and the effects, in percentage points, with Krinsky-Robb standard errors. ",
    "Grey tabs: every digit."
  )
  overview <- tibble(
    item = c("Model", "Label", "Formula", "Reference outcome", "Experts",
             "Choices", "Parameters", "Log-likelihood", "McFadden R2", "AIC",
             "BIC", "TIC", "Halton draws per expert", "Krinsky-Robb draws",
             "Starting values from", "Log-likelihood at the start",
             "Convergence code", "Iterations", "Largest gradient element",
             "Scaled gradient", "Smallest eigenvalue of the Hessian",
             "Condition number of the Hessian", "Minutes of estimation", "Date",
             "Code version", "Sheets"),
    value = as.character(c(
      results$name, results$label, str_squish(results$formula),
      results$reference, fit$n_experts, fit$n_choices, fit$n_parameters,
      round(fit$log_likelihood, 3), round(fit$mcfadden_r2, 4),
      round(fit$aic, 1), round(fit$bic, 1), round(fit$tic, 1),
      fit$n_draws_per_expert, results$n_draws_kr,
      coalesce(diagnostics$start_from, "mlogit's defaults"),
      round(diagnostics$log_likelihood_start, 3), diagnostics$code,
      diagnostics$iterations, signif(diagnostics$max_gradient, 3),
      signif(diagnostics$scaled_gradient, 3),
      signif(diagnostics$smallest_eigenvalue, 3),
      signif(diagnostics$condition_number, 3), diagnostics$minutes_estimation,
      as.character(results$date), results$git_commit, sheet_guide
    ))
  )
  # xl_col_spec(): the layout of a column; xl_align(wrap = TRUE): long text wraps
  overview_value <- xl_col_spec("value", width = 100,
                                format = xl_align(wrap = TRUE))

  # (b) One row per regressor, one column per technology, the estimate and its
  # standard error next to each other; then the terms with one coefficient for
  # all technologies, in columns of their own.
  terms <- coefficient_parts(results)$terms
  of_technology <- terms %>%
    filter(!is.na(technology))
  by_technology <- tibble(term = unique(of_technology$column))
  for (technology_name in setdiff(outcomes, results$reference)) {
    positions <- match(paste0(by_technology$term, ":", technology_name),
                       coefficient_table$parameter)
    label <- outcome_labels[[technology_name]]
    by_technology[[label]] <- coefficient_table$estimate[positions]
    by_technology[[paste(label, "se")]] <- coefficient_table$se[positions]
  }
  of_all <- coefficient_table %>%
    filter(parameter %in% terms$parameter[is.na(terms$technology)]) %>%
    transmute(term = parameter, "All technologies" = estimate,
              "All technologies se" = se)
  if (nrow(of_all) > 0) {
    by_technology <- bind_rows(by_technology, of_all)
  }

  # (c) The constants, with the technologies in words
  group_of_constant <- if_else(random$column == "(Intercept)", "All experts",
                               random$column)
  constants_sd <- results$constant_table %>%
    filter(type == "sd") %>%
    transmute(constants = group_of_constant,
              technology = outcome_labels[random$technology],
              estimate, se, ci_low_kr, ci_high_kr)
  pair_rows <- list()
  for (i in seq_len(nrow(random))) {
    for (j in seq_len(i - 1)) {
      if (random$column[i] == random$column[j]) {
        pair_rows[[length(pair_rows) + 1]] <- tibble(
          parameter = paste0("cor.", random$name[j], ":", random$name[i]),
          constants = group_of_constant[i],
          technology_1 = outcome_labels[[random$technology[j]]],
          technology_2 = outcome_labels[[random$technology[i]]]
        )
      }
    }
  }
  constants_correlations <- bind_rows(pair_rows) %>%
    left_join(results$constant_table, by = "parameter",
              relationship = "one-to-one") %>%
    select(constants, technology_1, technology_2, estimate, se, ci_low_kr,
           ci_high_kr)

  sheets <- list(
    overview = xl_sheet(overview, cols = overview_value, auto_colwidth = TRUE),
    coefficients_by_technology = readable_sheet(by_technology, 3),
    constants_sd = readable_sheet(constants_sd, 3, n_frozen = 2),
    constants_correlations = readable_sheet(constants_correlations, 3,
                                            n_frozen = 3)
  )

  # (d) Per call of compute_ames(): the probabilities at the reference state and
  # the effects side by side, in percentage points, and what each effect changes
  definition_rows <- list()
  for (ame_name in names(results$ames)) {
    ames <- results$ames[[ame_name]]
    per_effect <- bind_rows(
      ames$baseline %>%
        transmute(profile, effect = "Probability at the reference state",
                  outcome, value = probability, se),
      ames$table %>%
        select(profile, effect, outcome, value = ame, se)
    )
    effect_names <- unique(per_effect$effect)
    profile_names <- unique(per_effect$profile)
    pp <- tibble(profile = rep(profile_names, each = length(effect_names)),
                 effect = rep(effect_names, times = length(profile_names)))
    for (outcome_name in outcomes) {
      one_outcome <- per_effect %>%
        filter(outcome == outcome_name)
      positions <- match(paste(pp$profile, pp$effect),
                         paste(one_outcome$profile, one_outcome$effect))
      label <- outcome_labels[[outcome_name]]
      pp[[label]] <- 100 * one_outcome$value[positions]
      pp[[paste(label, "se")]] <- 100 * one_outcome$se[positions]
    }
    sheets[[paste0("pp_", ame_name)]] <- readable_sheet(pp, 2, n_frozen = 2)

    settings <- list()
    settings[["reference state|at"]] <- ames$at
    if (is.list(ames$profiles)) {
      for (profile_name in names(ames$profiles)) {
        settings[[paste0(profile_name, "|profile")]] <-
          ames$profiles[[profile_name]]
      }
    }
    for (effect in names(ames$changes)) {
      settings[[paste0(effect, "|from")]] <- ames$changes[[effect]][["from"]]
      settings[[paste0(effect, "|to")]] <- ames$changes[[effect]][["to"]]
    }
    for (setting in names(settings)) {
      for (variable in names(settings[[setting]])) {
        value <- settings[[setting]][[variable]]
        if (is.numeric(value)) {
          value <- round(value, 3)
        }
        definition_rows[[length(definition_rows) + 1]] <- tibble(
          effects = ame_name,
          effect = str_replace(setting, "\\|[a-z]+$", ""),
          state = str_replace(setting, "^.*\\|", ""),
          variable = variable,
          value = coalesce(as.character(value), "NA: not reported or blank")
        )
      }
    }
  }
  if (length(definition_rows) > 0) {
    sheets$effect_definitions <- xl_sheet(bind_rows(definition_rows),
                                          freeze = list(row = 1, col = 2),
                                          auto_colwidth = TRUE)
  }

  # A cell below the minimum (docs/data_contract.md) stays blank. A single one
  # takes the next smallest with it, as the total of the overview would give it
  # away.
  n_choices <- as.integer(results$choice_counts[outcomes])
  is_blank <- n_choices < MIN_N
  if (sum(is_blank) == 1) {
    is_blank[order(n_choices)[2]] <- TRUE
  }
  choice_counts <- tibble(
    outcome = outcome_labels[outcomes],
    choices = if_else(is_blank, NA_integer_, n_choices),
    "share (%)" = if_else(is_blank, NA_real_, 100 * n_choices / sum(n_choices))
  )
  sheets$choice_counts <- readable_sheet(choice_counts, 1)

  # (e) The long sheets
  sheets$fit <- long_sheet(fit)
  sheets$coefficients <- long_sheet(coefficient_table)
  for (ame_name in names(results$ames)) {
    ames <- results$ames[[ame_name]]
    sheets[[paste0("ame_", ame_name)]] <- long_sheet(ames$table)
    sheets[[paste0("baseline_", ame_name)]] <- long_sheet(ames$baseline)
  }
  write_xlsx(sheets, file.path(results$folder, "results.xlsx"))
}

#' A table as a sheet to read: its decimal numbers rounded and shown with the same
#' decimals, each column as wide as its contents, the header and the first columns
#' always in view. xl_sheet(): a table with the layout of its sheet.
#'
#' @param table    a table
#' @param digits   decimals of its numbers
#' @param n_frozen columns that stay in view when scrolling to the right
#' @return an xl_sheet, for write_xlsx()
readable_sheet <- function(table, digits, n_frozen = 1) {
  decimal_columns <- character(0)
  for (column in names(table)) {
    if (is.double(table[[column]])) {
      table[[column]] <- round(table[[column]], digits)
      decimal_columns <- c(decimal_columns, column)
    }
  }
  # xl_num_format(): how Excel shows a number; 3 digits: "0.000". strrep():
  # repeats a text.
  number_format <- xl_num_format(paste0("0.", strrep("0", digits)))
  xl_sheet(table, cols = xl_col_spec(decimal_columns, format = number_format),
           freeze = list(row = 1, col = n_frozen), auto_colwidth = TRUE)
}

#' A table as a sheet for reuse: every digit, filters on, a grey tab
#'
#' @param table a table
#' @return an xl_sheet, for write_xlsx()
long_sheet <- function(table) {
  xl_sheet(table, freeze = list(row = 1, col = 0), auto_colwidth = TRUE,
           autofilter = TRUE, tab_color = "gray")
}

#' Writes the workbook again if it is older than the results: a run could not
#' write it, most often because it was open in Excel, and the next run would load
#' the results and leave the workbook without them.
#'
#' @param results the results, a list
#' @return nothing
refresh_workbook <- function(results) {
  workbook <- file.path(results$folder, "results.xlsx")
  results_file <- file.path(results$folder, "results.rds")
  # file.mtime(): when a file was last written
  if (!file.exists(workbook) || file.mtime(workbook) < file.mtime(results_file)) {
    write_results_excel(results)
  }
}
