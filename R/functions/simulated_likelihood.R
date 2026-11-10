################################################################################
# simulated_likelihood.R — mlogit's simulated log-likelihood and its scores, fast
# Inputs : a fitted mixed logit (mlogit), whose constants are the only random
#          coefficients
# Outputs: none
# Checks : tests/test_simulated_likelihood.R compares it with mlogit;
#          post_estimation() does so for every model
################################################################################
# The model (Train 2009, chapters 6 and 10). Expert n, technology j, vignette t:
#
#   U_njt = x_njt' beta + (L z_n)_j + e_njt
#
# x_njt' beta is the mean utility, the same for all draws. a_n = L z_n are the
# expert's deviations from the mean constants, with z_n ~ N(0, I), so that the
# constants have the covariance matrix L L'. The probability of expert n's choices,
# simulated over R Halton draws z_nr, the same draws as in the estimation:
#
#   P_n = 1/R sum_r prod_t p_nrt,   p_nrt = exp(V_nrjt) / sum_k exp(V_nrkt)
#
# Each row of the score of an expert is d log P_n / d theta. With the weight of draw
# r, w_nr = prod_t p_nrt / sum_s prod_t p_nst:
#   for a mean:    sum_t (x_n,chosen,t - sum_j Pbar_ntj x_njt), Pbar_ntj = sum_r w_nr p_nrjt
#   for L[k, m]:   sum_r w_nr z_nrm sum_t (1(y_nt = k) - p_nrkt)
#
# Why not mlogit itself: one evaluation there re-reads the data, rebuilds the draws
# and loops over experts: 30.7 seconds against 9 at 1,500 experts, 1,000 draws and 113
# parameters (benchmark of 2026-11-09), and a Hessian takes 226 evaluations. Here
# every step is a matrix operation over experts and draws, and it returns what
# mlogit does not: the score and the log-likelihood per expert. It reproduces
# mlogit's log-likelihood exactly and its scores to 3e-14.

#' Everything the likelihood needs from a fitted model, once
#'
#' @param model   fitted mlogit model, with panel = TRUE and random constants
#' @param cluster name of the expert in idx(model)
#' @param z_all   optional other draws: one row per expert x draw, one column per
#'                random coefficient, the draws of one expert in consecutive rows
#'                (as mlogit:::make.random.nb() returns them); NULL: the draws of
#'                the estimation
#' @return list, the argument `prepared` of simulated_likelihood()
prepare_likelihood <- function(model, cluster = "resp_uid", z_all = NULL) {
  index <- idx(model)
  decision <- match(index$chid, unique(index$chid))

  # The reference outcome comes first in mlogit's order of the coefficients
  alternatives <- levels(droplevels(index$alternative))
  reference <- setdiff(alternatives, names(model$coefficients))
  coefficient_names <- names(coef(model))
  outcomes_in_data <- as.character(index$alternative)

  # (a) One row per decision x alternative, in the order decision, alternative.
  # model.matrix(): one column per coefficient that is not an element of L
  X_long <- model.matrix(model)
  order_rows <- order(decision, match(outcomes_in_data, alternatives))
  X_long <- X_long[order_rows, , drop = FALSE]
  n_decisions <- max(decision)
  n_alternatives <- length(alternatives)
  if (nrow(X_long) != n_decisions * n_alternatives) {
    stop("Every decision must have the same alternatives")
  }
  is_chosen <- as.logical(model$model[[1]])[order_rows]
  # the position of the chosen alternative, per decision
  chosen <- ((which(is_chosen) - 1L) %% n_alternatives) + 1L

  # (b) The experts, in the order of their first decision: mlogit's order of the
  # draws. One expert per decision.
  expert_of_row <- as.character(index[[cluster]])[order_rows]
  expert_of_decision <- expert_of_row[seq(1, by = n_alternatives, length.out = n_decisions)]
  experts <- unique(expert_of_decision)
  expert_idx <- match(expert_of_decision, experts)

  # (c) The random coefficients, in mlogit's order: that of the columns of X
  beta_names <- colnames(X_long)
  random_names <- beta_names[beta_names %in% names(model$rpar)]
  K_r <- length(random_names)
  n_draws <- eval(model$call$R)

  # Each random constant: its alternative, and its value for each expert, 1 for
  # "(Intercept)" and the group dummy for a constant by group. The value is the
  # same in all decisions of an expert.
  random_alternative <- integer(K_r)
  random_value <- matrix(0, length(experts), K_r)
  for (k in seq_len(K_r)) {
    technology <- str_replace(random_names[k], "^.*:", "")
    random_alternative[k] <- match(technology, alternatives)
    rows <- seq(random_alternative[k], by = n_alternatives, length.out = n_decisions)
    values <- X_long[rows, random_names[k]]
    random_value[expert_idx, k] <- values
  }

  # (d) The draws: for each random coefficient, one matrix experts x draws
  if (is.null(z_all)) {
    z_all <- mlogit:::make.random.nb(n_draws * length(experts), K_r, eval(model$call$halton))
  }
  Z <- vector("list", K_r)
  for (k in seq_len(K_r)) {
    Z[[k]] <- t(matrix(z_all[, k], nrow = n_draws, ncol = length(experts)))
  }

  # (e) The elements of L in the coefficients: mlogit names L[i, j], i >= j,
  # "chol.<coefficient j>:<coefficient i>". Those fixed at 0 are not among them.
  cholesky_names <- character(0)
  cholesky_i <- integer(0)
  cholesky_j <- integer(0)
  for (i in seq_len(K_r)) {
    for (j in seq_len(i)) {
      name <- paste0("chol.", random_names[j], ":", random_names[i])
      if (name %in% coefficient_names) {
        cholesky_names <- c(cholesky_names, name)
        cholesky_i <- c(cholesky_i, i)
        cholesky_j <- c(cholesky_j, j)
      }
    }
  }
  if (!setequal(c(beta_names, cholesky_names), coefficient_names)) {
    stop("The coefficients are not the columns of the design matrix and the elements of L")
  }

  list(
    X_long = X_long,
    x_chosen = X_long[is_chosen, , drop = FALSE],
    chosen = chosen,
    expert_idx = expert_idx,
    experts = experts,
    n_decisions = n_decisions,
    n_alternatives = n_alternatives,
    n_experts = length(experts),
    n_draws = n_draws,
    K_r = K_r,
    Z = Z,
    random_alternative = random_alternative,
    random_value = random_value,
    beta_names = beta_names,
    cholesky_names = cholesky_names,
    cholesky_i = cholesky_i,
    cholesky_j = cholesky_j
  )
}

#' The simulated log-likelihood at theta, and optionally the scores per expert
#'
#' @param theta       named vector, as coef(model)
#' @param prepared    prepare_likelihood()
#' @param want_scores FALSE: only the log-likelihood, about half the time
#' @return list: log_likelihood; log_likelihood_expert (one per expert, in the order
#'   prepared$experts); scores (experts x coefficients, the derivatives of the
#'   log-likelihood; mlogit's model$gradient is minus its rows)
simulated_likelihood <- function(theta, prepared, want_scores = TRUE) {
  n_decisions <- prepared$n_decisions
  n_alternatives <- prepared$n_alternatives
  n_experts <- prepared$n_experts
  n_draws <- prepared$n_draws
  K_r <- prepared$K_r
  expert_idx <- prepared$expert_idx
  chosen <- prepared$chosen

  # Mean utilities: decisions x alternatives, stored by alternative
  mean_long <- as.vector(prepared$X_long %*% theta[prepared$beta_names])
  L <- matrix(0, K_r, K_r)
  L[cbind(prepared$cholesky_i, prepared$cholesky_j)] <- theta[prepared$cholesky_names]

  # (a) exp(utility) of each alternative: decisions x draws. exp(mean + deviation) =
  # exp(mean) * exp(deviation), and the deviation is per expert, so there are far
  # fewer exp() to take.
  exp_utility <- vector("list", n_alternatives)
  denominator <- matrix(0, n_decisions, n_draws)
  for (j in seq_len(n_alternatives)) {
    exp_mean <- exp(mean_long[seq(j, by = n_alternatives, length.out = n_decisions)])
    constants_of_j <- which(prepared$random_alternative == j)
    if (length(constants_of_j) == 0) {
      exp_utility[[j]] <- matrix(exp_mean, n_decisions, n_draws)
    } else {
      deviation <- matrix(0, n_experts, n_draws)
      for (k in constants_of_j) {
        for (m in seq_len(k)) {
          if (L[k, m] != 0) {
            deviation <- deviation + (L[k, m] * prepared$random_value[, k]) * prepared$Z[[m]]
          }
        }
      }
      exp_utility[[j]] <- exp_mean * exp(deviation)[expert_idx, , drop = FALSE]
    }
    denominator <- denominator + exp_utility[[j]]
  }

  # (b) log P_n^r: experts x draws. rowsum(): the sum of the rows of each expert.
  log_probability <- matrix(0, n_decisions, n_draws)
  for (j in seq_len(n_alternatives)) {
    rows <- which(chosen == j)
    log_probability[rows, ] <- log(exp_utility[[j]][rows, , drop = FALSE] /
                                     denominator[rows, , drop = FALSE])
  }
  expert_probability <- exp(rowsum(log_probability, expert_idx, reorder = FALSE))
  rm(log_probability)
  sum_over_draws <- rowSums(expert_probability)
  log_likelihood_expert <- log(sum_over_draws / n_draws)
  if (!want_scores) {
    return(list(log_likelihood = sum(log_likelihood_expert),
                log_likelihood_expert = as.vector(log_likelihood_expert)))
  }

  # (c) The scores. w: the weight of each draw, experts x draws, summing to 1 for
  # each expert.
  w <- expert_probability / sum_over_draws
  w_of_decision <- w[expert_idx, , drop = FALSE]
  p_bar <- matrix(0, n_decisions, n_alternatives)
  residual_by_expert <- vector("list", K_r)
  for (j in seq_len(n_alternatives)) {
    p_j <- exp_utility[[j]] / denominator
    exp_utility[[j]] <- NA
    p_bar[, j] <- rowSums(p_j * w_of_decision)
    constants_of_j <- which(prepared$random_alternative == j)
    if (length(constants_of_j) > 0) {
      # experts x draws: sum_t (1(y_t = j) - p_tj^r)
      residual_j <- rowsum((chosen == j) - p_j, expert_idx, reorder = FALSE)
      for (k in constants_of_j) {
        residual_by_expert[[k]] <- residual_j * prepared$random_value[, k]
      }
    }
  }
  scores_cholesky <- matrix(0, n_experts, length(prepared$cholesky_names))
  for (m in seq_along(prepared$cholesky_names)) {
    i <- prepared$cholesky_i[m]
    j <- prepared$cholesky_j[m]
    scores_cholesky[, m] <- rowSums(residual_by_expert[[i]] * prepared$Z[[j]] * w)
  }
  decision_of_row <- rep(seq_len(n_decisions), each = n_alternatives)
  x_expected <- rowsum(prepared$X_long * as.vector(t(p_bar)), decision_of_row,
                       reorder = FALSE)
  scores_beta <- rowsum(prepared$x_chosen - x_expected, expert_idx, reorder = FALSE)

  scores <- cbind(scores_beta, scores_cholesky)
  colnames(scores) <- c(prepared$beta_names, prepared$cholesky_names)
  list(log_likelihood = sum(log_likelihood_expert),
       log_likelihood_expert = as.vector(log_likelihood_expert),
       scores = scores[, names(theta), drop = FALSE])
}
