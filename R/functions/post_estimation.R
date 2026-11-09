################################################################################
# post_estimation.R — everything that follows an estimation: standard errors
#                     clustered by expert, Krinsky-Robb draws, standard deviations
#                     and correlations of the constants, fit and diagnostics
# Inputs : a fitted model (fit_mixed_logit())
# Outputs: <est_dir>/<name>/results.rds, aggregates only (docs/results_contract.md);
#          <est_dir>/<name>/results.xlsx, the same to read
# Checks : the fast likelihood reproduces mlogit's at the estimates; the Hessian is
#          positive definite
################################################################################

#' Standard errors clustered by expert, the constants and fit of one model. Loads
#' the saved results if the coefficients have not changed.
#'
#' The cluster-robust (sandwich) covariance matrix, clustered by expert:
#'   V = G / (G - 1) * H^{-1} B H^{-1}
#' H     Hessian of -log L at the estimates ("bread"). mlogit provides none for the
#'       mixed logit, so H comes from central differences of the gradient, column by
#'       column on n_workers R sessions.
#' B     sum over experts of the outer product of the expert's score ("meat").
#' G     the number of experts; G / (G - 1) as in Stata and sandwich::vcovCL.
#'
#' Krinsky-Robb draws from N(theta_hat, V) give the standard errors and percentile
#' intervals of everything that is a nonlinear function of the parameters: the
#' standard deviations and correlations of the constants here, the AMEs in
#' compute_ames(). The percentile intervals stay within the bounds (a standard
#' deviation above 0, a correlation between -1 and 1) and show skewness, which
#' matters most in the smaller groups.
#'
#' @param model      fitted model, from fit_mixed_logit()
#' @param est_dir    the folder of the estimation script
#' @param label      what the model is, plain text, for tables and the Excel file
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
      message("Model ", name, ": nothing changed, results loaded from ", results_file)
      return(saved_results)
    }
  }
  start_time <- Sys.time()
  K <- length(theta_hat)

  # (a) The fast likelihood reproduces mlogit's, so it uses the estimation's data and
  # draws. fitted(): the expert's simulated probability, repeated per decision.
  prepared <- prepare_likelihood(model)
  at_estimates <- simulated_likelihood(theta_hat, prepared)
  difference <- abs(at_estimates$log_likelihood - as.numeric(logLik(model)))
  if (difference > 1e-6) {
    stop("Model ", name, ": the fast likelihood differs from mlogit's by ", difference,
         ". Estimate models with fit_mixed_logit(), which re-evaluates at code 2")
  }

  # (b) Meat: one score per expert. Bread: the inverse of the numerical Hessian, one
  # column per parameter, on n_workers R sessions. makeCluster(): starts them, with
  # --vanilla so that each is ready at once; the likelihood needs only base R.
  # parLapply(): lapply on the sessions.
  message("Model ", name, ": Hessian, ", 2 * K, " evaluations on ", n_workers, " R sessions")
  meat <- crossprod(at_estimates$scores)
  G <- nrow(at_estimates$scores)
  hessian_column <- function(k) {
    step <- replace(numeric(length(theta_hat)), k, h)
    names(step) <- names(theta_hat)
    gradient_up <- colSums(simulated_likelihood(theta_hat + step, prepared)$scores)
    gradient_down <- colSums(simulated_likelihood(theta_hat - step, prepared)$scores)
    -(gradient_up - gradient_down) / (2 * h)
  }
  cluster <- parallel::makeCluster(n_workers, rscript_args = "--vanilla")
  parallel::clusterExport(cluster, "simulated_likelihood")
  hessian_columns <- parallel::parLapply(cluster, seq_len(K), hessian_column)
  parallel::stopCluster(cluster)
  H <- matrix(unlist(hessian_columns), K, K, dimnames = list(names(theta_hat), names(theta_hat)))
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
  # sd_j = sqrt(Omega_jj), cor_jk = Omega_jk / (sd_j sd_k). Each correlation once and
  # only within a set of constants: with constants by group, those between two groups
  # are 0 by construction.
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
    L[cbind(prepared$cholesky_i, prepared$cholesky_j)] <- theta[prepared$cholesky_names]
    Omega <- L %*% t(L)
    sds <- sqrt(diag(Omega))
    correlations <- Omega[cbind(pairs$i, pairs$j)] / (sds[pairs$i] * sds[pairs$j])
    c(sds, correlations)
  }
  constant_names <- c(paste0("sd.", random$name),
                      paste0("cor.", random$name[pairs$j], ":", random$name[pairs$i]))
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
  # predicts the observed share of each recommendation. AIC and BIC to compare models
  # that are not nested; BIC counts experts, as the panel likelihood is a product over
  # experts. TIC replaces the number of parameters by tr(H^{-1} B), which equals it in
  # a correctly specified model (Takeuchi 1976).
  outcomes <- model$fit_info$outcomes
  chosen_outcome <- as.character(idx(model)$alternative)[as.logical(model$model[[1]])]
  choice_counts <- table(factor(chosen_outcome, levels = outcomes))
  n_choices <- sum(choice_counts)
  log_likelihood <- at_estimates$log_likelihood
  log_likelihood_constants_only <- sum(choice_counts[choice_counts > 0] *
                                         log(choice_counts[choice_counts > 0] / n_choices))
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
  # log-likelihood with that of a model this one nests (compare_models()). The scaled
  # gradient g' H^{-1} g is close to 0 at a maximum, whatever the units of the
  # parameters.
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
  # model.rds. The formula is saved as text, because a formula object can carry the
  # environment it was made in. The hash of the experts lets compare_models() check
  # that two models share one sample without storing who they are.
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
    git_commit = suppressWarnings(tryCatch(
      system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE),
      error = function(e) NA_character_
    )),
    date = Sys.Date()
  )
  saveRDS(results, results_file)
  write_results_excel(results)
  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  message("Model ", name, ": results written to ", model_dir, " (", round(minutes, 1),
          " minutes)")
  results
}

#' The results to read: one Excel workbook per model, one sheet per kind of result,
#' and two sheets per call of compute_ames(). Called by post_estimation() and
#' compute_ames(); the file is rewritten each time.
#'
#' @param results the results, a list
#' @return nothing; writes <results$folder>/results.xlsx
write_results_excel <- function(results) {
  overview <- tibble(
    item = c("Model", "Label", "Formula", "Reference outcome", "Experts", "Choices",
             "Parameters", "Halton draws per expert", "Krinsky-Robb draws", "Estimated from",
             "Log-likelihood at the start", "Convergence code", "Iterations",
             "Largest gradient element", "Scaled gradient", "Smallest eigenvalue of the Hessian",
             "Condition number of the Hessian", "Date", "Code version"),
    value = as.character(c(
      results$name, results$label, results$formula, results$reference,
      results$fit$n_experts, results$fit$n_choices, results$fit$n_parameters,
      results$fit$n_draws_per_expert, results$n_draws_kr, results$diagnostics$start_from,
      results$diagnostics$log_likelihood_start, results$diagnostics$code,
      results$diagnostics$iterations, results$diagnostics$max_gradient,
      results$diagnostics$scaled_gradient, results$diagnostics$smallest_eigenvalue,
      results$diagnostics$condition_number, as.character(results$date), results$git_commit
    ))
  )
  sheets <- list(
    overview = overview,
    fit = results$fit,
    coefficients = results$coefficient_table,
    constants_sd = results$constant_table %>%
      filter(type == "sd") %>%
      select(-type),
    constants_correlations = results$constant_table %>%
      filter(type == "cor") %>%
      select(-type),
    choice_counts = tibble(outcome = names(results$choice_counts),
                           choices = as.numeric(results$choice_counts))
  )
  for (ame_name in names(results$ames)) {
    sheets[[paste0("ame_", ame_name)]] <- results$ames[[ame_name]]$table
    sheets[[paste0("baseline_", ame_name)]] <- results$ames[[ame_name]]$baseline
  }
  write_xlsx(sheets, file.path(results$folder, "results.xlsx"))
}
