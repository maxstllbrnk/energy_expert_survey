################################################################################
# 00_functions_output.R — tables, figures and tests from the saved results of
#                         the mixed logit models of the recommendations
# Inputs : none; sourced by 02_output.R
# Outputs: none
# Checks : inside the functions: every parameter a table needs is in the
#          results; a model with Berufsgruppe shifts or interactions has one
#          reference group; the restrictions of an LR test account for its
#          degrees of freedom; every value has a label
################################################################################
# Every table and figure shows a quantity the same way, whatever the model:
#
#   mean constant       of a Berufsgruppe (or of all experts, if the model has
#                       no Berufsgruppe): the mean utility of a technology
#                       relative to the reference outcome, at the reference
#                       vignette. With Berufsgruppe shifts (model 4) it is the
#                       constant plus the group's shift; with constants by
#                       Berufsgruppe (model 6) the group's constant.
#   attribute effect    of a Berufsgruppe (models with interactions) or of all
#                       experts: the attribute's coefficient plus the group's
#                       interaction
#   standard deviation  of a constant across the experts of a covariance
#   and correlation     block: all experts (one covariance matrix) or one
#                       Berufsgruppe (a covariance matrix per Berufsgruppe)
#
# A row thus always shows a group's own value, never its difference to the
# reference group. Whether the Berufsgruppen differ is a question for the LR
# tests, and for the probabilities and AMEs for the Krinsky-Robb draws
# (group_differences()).
#
# Standard errors: a sum of coefficients is linear in them, so its standard
# error comes from the cluster-robust covariance matrix; everything nonlinear
# (standard deviations, correlations, probabilities, AMEs) has Krinsky-Robb
# standard errors and percentile intervals, saved by 01_run_specifications.R.

# 1. Formatting ----------------------------------------------------------------

#' Numbers as LaTeX text: fixed decimals, a thousands comma, the minus sign as
#' $-$, an empty cell for NA. Rounded first, so that -0.001 reads 0.00 and not
#' -0.00 (round() keeps the sign of a negative zero).
latex_number <- function(x, digits) {
  x_rounded <- round(x, digits)
  # which(): the positions where the condition is TRUE, leaving out NA
  x_rounded[which(x_rounded == 0)] <- 0
  text <- formatC(x_rounded, format = "f", digits = digits, big.mark = ",")
  text <- sub("^-", "$-$", text)
  text[is.na(x)] <- ""
  text
}

#' p-value as LaTeX text: three decimals, "$<$0.001" below 0.001, "--" if it
#' could not be computed
latex_p <- function(p) {
  text <- latex_number(p, 3)
  text[!is.na(p) & p < 0.001] <- "$<$0.001"
  text[is.na(p)] <- "--"
  text
}

#' Significance stars at 10, 5 and 1 percent; none without a p-value
star_text <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.10 ~ "$^{*}$",
    TRUE ~ ""
  )
}

#' Plain text as LaTeX: the characters that LaTeX reserves are escaped, and
#' the superscript two, the multiplication sign and the euro sign become
#' LaTeX, so that the same labels serve the tables, the figures and Excel
latex_text <- function(text) {
  text <- gsub("&", "\\&", text, fixed = TRUE)
  text <- gsub("%", "\\%", text, fixed = TRUE)
  text <- gsub("_", "\\_", text, fixed = TRUE)
  text <- gsub("#", "\\#", text, fixed = TRUE)
  text <- gsub("$", "\\$", text, fixed = TRUE)
  text <- gsub("²", "$^2$", text, fixed = TRUE)
  text <- gsub("×", "$\\times$", text, fixed = TRUE)
  gsub("€", "\\texteuro{}", text, fixed = TRUE)
}

#' The label of each value, from a named vector of labels; stops if a value
#' has none, so that no raw name reaches a table or figure
#'
#' @param values character vector, e.g. the outcomes of a table
#' @param labels named character vector: value -> label
#' @param what   what the values are, for the error message
label_of <- function(values, labels, what) {
  missing_values <- setdiff(unique(values), names(labels))
  if (length(missing_values) > 0) {
    stop("No label for the ", what, ": ", paste(missing_values, collapse = ", "),
         " (add it in section 1 of 02_output.R)")
  }
  unname(labels[values])
}

#' Write the lines of a .tex file in UTF-8, so the special characters are read
#' the same on every system
write_tex <- function(lines, file) {
  connection <- file(file, open = "w", encoding = "UTF-8")
  writeLines(lines, connection)
  close(connection)
}

# 2. Quantities from the saved results -----------------------------------------

#' Estimate and standard error of a sum of coefficients, e.g. a constant plus
#' a Berufsgruppe's shift. The sum is linear in the coefficients, so its
#' variance is w' V w, with w the 0/1 weights and V the cluster-robust
#' covariance matrix of the coefficients.
#'
#' @param results    saved results of one model
#' @param parameters names of the coefficients to add up
#' @return tibble, one row: estimate, se, z, p
sum_of_coefficients <- function(results, parameters) {
  theta <- results$coefficients
  missing_parameters <- setdiff(parameters, names(theta))
  if (length(missing_parameters) > 0) {
    stop("Not a parameter of ", results$name, ": ",
         paste(missing_parameters, collapse = ", "))
  }
  weights <- rep(0, length(theta))
  names(weights) <- names(theta)
  weights[parameters] <- 1
  V <- results$vcov[names(theta), names(theta)]
  estimate <- sum(weights * theta)
  se <- sqrt(as.numeric(t(weights) %*% V %*% weights))
  tibble(estimate = estimate, se = se, z = estimate / se,
         p = 2 * pnorm(-abs(estimate / se)))
}

#' How a model treats the Berufsgruppen, read from its specification
#'
#' @param specification results$specification
#' @param group_dummies named vector: group dummy -> Berufsgruppe, e.g.
#'                      is_shk = "SHK-Handwerk"
#' @return list:
#'   group_constants  TRUE: one set of constants, and one covariance matrix,
#'                    per Berufsgruppe
#'   group_shifts     the group dummies that shift the mean constants
#'   interactions     TRUE: the attribute effects differ by Berufsgruppe
#'   prices           TRUE: the model has the energy prices
#'   reference_dummy  the group dummy without shift and interactions, NA if
#'                    the model has neither
model_structure <- function(specification, group_dummies) {
  regressors <- specification$regressors
  group_constants <- !identical(specification$constants, "(Intercept)")
  group_shifts <- intersect(names(group_dummies), regressors)
  # the group dummies with interactions, e.g. is_shk for is_shk_x_age_60
  dummies_with_interactions <- character(0)
  for (dummy in names(group_dummies)) {
    if (any(startsWith(regressors, paste0(dummy, "_x_")))) {
      dummies_with_interactions <- c(dummies_with_interactions, dummy)
    }
  }

  # The groups with a shift or with interactions are all but one: the
  # reference group, whose constants and attribute coefficients are the
  # model's own
  reference_dummy <- NA_character_
  for (dummies in list(group_shifts, dummies_with_interactions)) {
    if (length(dummies) == 0) next
    if (length(dummies) != length(group_dummies) - 1) {
      stop("A model needs the shifts or interactions of all Berufsgruppen ",
           "but one: ", paste(dummies, collapse = ", "))
    }
    reference <- setdiff(names(group_dummies), dummies)
    if (!is.na(reference_dummy) && reference != reference_dummy) {
      stop("The shifts and the interactions have different reference groups")
    }
    reference_dummy <- reference
  }

  list(group_constants = group_constants,
       group_shifts = group_shifts,
       interactions = length(dummies_with_interactions) > 0,
       prices = length(specification$prices) > 0,
       reference_dummy = reference_dummy)
}

#' Mean constants by Berufsgruppe, or for all experts if the model has no
#' Berufsgruppe (see the top of this file)
#'
#' @return tibble: group (Berufsgruppe or "All experts"), technology,
#'   estimate, se, z, p
mean_constants <- function(results, group_dummies) {
  setup <- model_structure(results$specification, group_dummies)
  technologies <- results$outcomes[-1]
  has_groups <- setup$group_constants || length(setup$group_shifts) > 0
  table <- tibble()
  if (!has_groups) {
    for (technology in technologies) {
      table <- bind_rows(table, tibble(
        group = "All experts",
        technology = technology,
        sum_of_coefficients(results, paste0("(Intercept):", technology))
      ))
    }
    return(table)
  }
  for (dummy in names(group_dummies)) {
    for (technology in technologies) {
      if (setup$group_constants) {
        parameters <- paste0(dummy, ":", technology)
      } else {
        parameters <- paste0("(Intercept):", technology)
        if (dummy %in% setup$group_shifts) {
          parameters <- c(parameters, paste0(dummy, ":", technology))
        }
      }
      table <- bind_rows(table, tibble(
        group = group_dummies[[dummy]],
        technology = technology,
        sum_of_coefficients(results, parameters)
      ))
    }
  }
  table
}

#' Effects of the vignette attributes by Berufsgruppe (models with
#' interactions), or for all experts (see the top of this file)
#'
#' @return tibble: group (Berufsgruppe or "All experts"), attribute (the
#'   dummy), technology, estimate, se, z, p
attribute_effects <- function(results, group_dummies) {
  setup <- model_structure(results$specification, group_dummies)
  technologies <- results$outcomes[-1]
  attributes <- results$attribute_dummies$dummy
  if (setup$interactions) {
    groups <- names(group_dummies)
  } else {
    groups <- "All experts"
  }
  table <- tibble()
  for (dummy in groups) {
    for (attribute in attributes) {
      for (technology in technologies) {
        parameters <- paste0(attribute, ":", technology)
        if (setup$interactions && dummy != setup$reference_dummy) {
          parameters <- c(parameters,
                          paste0(dummy, "_x_", attribute, ":", technology))
        }
        group <- "All experts"
        if (setup$interactions) group <- group_dummies[[dummy]]
        table <- bind_rows(table, tibble(
          group = group,
          attribute = attribute,
          technology = technology,
          sum_of_coefficients(results, parameters)
        ))
      }
    }
  }
  table
}

#' The covariance block of each random constant: "All experts" for the
#' constants of all experts, the Berufsgruppe for a group's constants
block_of <- function(columns, group_dummies) {
  block <- rep("All experts", length(columns))
  is_group <- columns %in% names(group_dummies)
  block[is_group] <- group_dummies[columns[is_group]]
  if (!all(is_group | columns == "(Intercept)")) {
    stop("Random constants of an unknown column: ",
         paste(unique(columns[!(is_group | columns == "(Intercept)")]), collapse = ", "))
  }
  block
}

#' Standard deviations of the constants across experts, by covariance block,
#' with Krinsky-Robb standard errors and percentile intervals
#'
#' @return tibble: group (the block), technology, estimate, se, ci_low_kr,
#'   ci_high_kr
constant_sds <- function(results, group_dummies) {
  random <- results$random_coefficients
  sds <- results$random_constants
  blocks <- block_of(random$column, group_dummies)
  table <- tibble()
  for (k in seq_len(nrow(random))) {
    is_row <- sds$parameter == paste0("sd.", random$name[k])
    if (sum(is_row) != 1) stop("No standard deviation of ", random$name[k])
    table <- bind_rows(table, tibble(
      group = blocks[k],
      technology = random$technology[k],
      estimate = sds$estimate[is_row],
      se = sds$se[is_row],
      ci_low_kr = sds$ci_low_kr[is_row],
      ci_high_kr = sds$ci_high_kr[is_row]
    ))
  }
  table
}

#' Correlations of the constants across experts, by covariance block: every
#' pair of technologies in both orders, and 1 on the diagonal, as the heat
#' maps show them. Each correlation is saved once, named "cor.<constant
#' j>:<constant i>" in mlogit's order of the constants, so both orders of the
#' names are tried.
#'
#' @return tibble: group (the block), technology_row, technology_column,
#'   estimate, se, ci_low_kr, ci_high_kr (NA on the diagonal)
correlation_table <- function(results, group_dummies) {
  random <- results$random_coefficients
  correlations <- results$random_constants
  technologies <- results$outcomes[-1]
  table <- tibble()
  for (column_name in unique(random$column)) {
    block <- block_of(column_name, group_dummies)
    for (technology_row in technologies) {
      for (technology_column in technologies) {
        if (technology_row == technology_column) {
          table <- bind_rows(table, tibble(
            group = block, technology_row = technology_row,
            technology_column = technology_column, estimate = 1,
            se = NA_real_, ci_low_kr = NA_real_, ci_high_kr = NA_real_
          ))
          next
        }
        name_row <- paste0(column_name, ":", technology_row)
        name_column <- paste0(column_name, ":", technology_column)
        is_row <- correlations$parameter %in%
          c(paste0("cor.", name_row, ":", name_column),
            paste0("cor.", name_column, ":", name_row))
        if (sum(is_row) != 1) {
          stop("No correlation of ", name_row, " and ", name_column)
        }
        table <- bind_rows(table, tibble(
          group = block, technology_row = technology_row,
          technology_column = technology_column,
          estimate = correlations$estimate[is_row],
          se = correlations$se[is_row],
          ci_low_kr = correlations$ci_low_kr[is_row],
          ci_high_kr = correlations$ci_high_kr[is_row]
        ))
      }
    }
  }
  table
}

#' Probabilities of the recommendations at the reference vignette, by profile
#' (Berufsgruppe or "All experts"), with Krinsky-Robb standard errors and
#' percentile intervals from the saved probabilities at every draw
#'
#' @return tibble: group (the profile), outcome, estimate, se, ci_low_kr,
#'   ci_high_kr
baseline_probabilities <- function(results) {
  ame <- results$ame_table
  table <- tibble()
  for (profile_name in names(results$probability_draws)) {
    draws <- results$probability_draws[[profile_name]]
    for (outcome_name in results$outcomes) {
      # every row of the AME table of a profile and outcome has the same
      # probability at the reference vignette
      is_row <- ame$profile == profile_name & ame$outcome == outcome_name
      if (sum(is_row) == 0) stop("No AMEs for ", profile_name, ", ", outcome_name)
      outcome_draws <- draws["reference", outcome_name, ]
      table <- bind_rows(table, tibble(
        group = profile_name,
        outcome = outcome_name,
        estimate = ame$p_reference[is_row][1],
        se = sd(outcome_draws),
        ci_low_kr = as.numeric(quantile(outcome_draws, 0.025)),
        ci_high_kr = as.numeric(quantile(outcome_draws, 0.975))
      ))
    }
  }
  table
}

#' Differences between the Berufsgruppen in the probabilities at the
#' reference vignette and in the AMEs, each group against the reference group
#' and the other two against each other, with Krinsky-Robb standard errors
#' and percentile intervals
#'
#' The saved probabilities of all profiles come from the same Krinsky-Robb
#' draws of the parameters, so the difference at each draw carries the
#' correlation between the two groups' estimates. Where the groups share the
#' attribute coefficients (model 4), their AMEs are almost perfectly
#' correlated (0.95 on simulated data): a small difference that comes only
#' from the groups' different baseline probabilities can then be significant
#' although the two groups' intervals overlap almost completely. Point
#' estimates are the differences of the point estimates. `significant`: the
#' 95% percentile interval excludes 0; p is from z = difference / se, as in
#' the AME table.
#'
#' @param reference_group the Berufsgruppe the others are compared with first
#' @return tibble: quantity ("probability" or "ame"), group, compared_with,
#'   vignette ("reference" for the probabilities), outcome, difference, se,
#'   z, p, ci_low_kr, ci_high_kr, significant; no rows for a model with one
#'   profile
group_differences <- function(results, reference_group) {
  draws <- results$probability_draws
  ame <- results$ame_table
  profiles <- names(draws)
  table <- tibble()
  if (length(profiles) < 2) return(table)
  if (!(reference_group %in% profiles)) {
    stop("No AMEs for the reference group ", reference_group, " in ", results$name)
  }

  # The pairs: each group against the reference group, then the others
  # against each other
  others <- setdiff(profiles, reference_group)
  pairs <- tibble(group = others, compared_with = reference_group)
  for (i in seq_along(others)) {
    for (j in seq_len(i - 1)) {
      pairs <- bind_rows(pairs, tibble(group = others[i],
                                       compared_with = others[j]))
    }
  }

  vignettes <- dimnames(draws[[1]])[[1]]
  for (k in seq_len(nrow(pairs))) {
    group_draws <- draws[[pairs$group[k]]]
    other_draws <- draws[[pairs$compared_with[k]]]
    for (vignette_name in vignettes) {
      for (outcome_name in results$outcomes) {
        is_group <- ame$profile == pairs$group[k] & ame$outcome == outcome_name
        is_other <- ame$profile == pairs$compared_with[k] & ame$outcome == outcome_name
        if (vignette_name == "reference") {
          quantity <- "probability"
          difference_draws <- group_draws["reference", outcome_name, ] -
            other_draws["reference", outcome_name, ]
          difference <- ame$p_reference[is_group][1] - ame$p_reference[is_other][1]
        } else {
          quantity <- "ame"
          difference_draws <-
            (group_draws[vignette_name, outcome_name, ] -
               group_draws["reference", outcome_name, ]) -
            (other_draws[vignette_name, outcome_name, ] -
               other_draws["reference", outcome_name, ])
          is_group <- is_group & ame$vignette == vignette_name
          is_other <- is_other & ame$vignette == vignette_name
          if (sum(is_group) != 1 || sum(is_other) != 1) {
            stop("AME missing: ", vignette_name, ", ", outcome_name)
          }
          difference <- ame$ame[is_group] - ame$ame[is_other]
        }
        se <- sd(difference_draws)
        ci_low_kr <- as.numeric(quantile(difference_draws, 0.025))
        ci_high_kr <- as.numeric(quantile(difference_draws, 0.975))
        table <- bind_rows(table, tibble(
          quantity = quantity,
          group = pairs$group[k],
          compared_with = pairs$compared_with[k],
          vignette = vignette_name,
          outcome = outcome_name,
          difference = difference,
          se = se,
          z = difference / se,
          p = 2 * pnorm(-abs(difference / se)),
          ci_low_kr = ci_low_kr,
          ci_high_kr = ci_high_kr,
          significant = ci_low_kr > 0 | ci_high_kr < 0
        ))
      }
    }
  }
  table
}

# 3. Likelihood-ratio tests ----------------------------------------------------

#' Restrictions of a likelihood-ratio test, as functions of the coefficients
#' theta of the larger (unrestricted) model: every element is 0 when theta
#' satisfies the smaller (restricted) model. The robust test needs their
#' derivative with respect to theta.
#'
#' Types:
#'   regressors         the coefficients of the regressors the smaller model
#'                      does not have (the Berufsgruppe shifts, the
#'                      interactions), one per technology
#'   prices             the coefficients of the price terms the smaller model
#'                      does not have
#'   group_covariances  (constants by Berufsgruppe) each group's covariance
#'                      matrix of the constants minus the first group's,
#'                      every variance and covariance once. The covariance
#'                      matrices, not their Cholesky factors, because a
#'                      covariance matrix has several Cholesky factors (the
#'                      signs of their columns). The means need no
#'                      restriction: model 6's group constants are model 4's
#'                      constant plus shift, written differently.
#'
#' @param theta        coefficients of the larger model, coef()
#' @param type         one of the three types
#' @param unrestricted saved results of the larger model
#' @param restricted   saved results of the smaller model
#' @return numeric vector, one element per restriction
lr_restrictions <- function(theta, type, unrestricted, restricted) {
  technologies <- unrestricted$outcomes[-1]
  if (type == "regressors") {
    dropped <- setdiff(unrestricted$specification$regressors,
                       restricted$specification$regressors)
    if (length(dropped) == 0) stop("Restriction regressors: none to drop")
    # outer(): every regressor with every technology, "<regressor>:<technology>"
    restrictions <- theta[as.vector(outer(dropped, technologies, paste, sep = ":"))]
  } else if (type == "prices") {
    dropped <- setdiff(unrestricted$specification$prices,
                       restricted$specification$prices)
    if (length(dropped) == 0) stop("Restriction prices: none to drop")
    restrictions <- theta[dropped]
  } else if (type == "group_covariances") {
    random <- unrestricted$random_coefficients
    columns <- unique(random$column)
    if (length(columns) < 2) {
      stop("Restriction group_covariances needs constants by Berufsgruppe")
    }
    L <- cholesky_factor(theta, random, unrestricted$fixed_parameters)
    Omega <- L %*% t(L)
    first <- paste0(columns[1], ":", technologies)
    restrictions <- numeric(0)
    for (column_name in columns[-1]) {
      members <- paste0(column_name, ":", technologies)
      difference <- Omega[members, members] - Omega[first, first]
      # lower.tri(diag = TRUE): the lower triangle with the diagonal, each
      # variance and covariance once
      restrictions <- c(restrictions,
                        difference[lower.tri(difference, diag = TRUE)])
    }
  } else {
    stop("Unknown restriction type: ", type)
  }
  if (anyNA(restrictions)) stop("Restriction ", type, ": coefficients missing")
  as.numeric(restrictions)
}

#' Likelihood-ratio tests between nested models, classical and robust
#'
#' LR = 2 (logL_larger - logL_smaller), df = the number of parameters the
#' larger model has more. Valid only if the smaller model is a special case of
#' the larger one, on the same sample and with the same draws per expert. The
#' sample and the number of draws are checked here. The Halton primes are the
#' same by construction: the constants of every model get the primes 2, 3, 5,
#' ... in the order of the technologies, by Berufsgruppe if they are by
#' Berufsgruppe (estimate_mixed_logit()).
#'
#' Classical p-value: chi-squared with df degrees of freedom; it assumes the
#' larger model is correctly specified, which the cluster-robust standard
#' errors do not.
#'
#' Robust p-value (Kent 1982; White 1982, section 6): whether or not the model
#' is correctly specified, under the restrictions LR is approximately
#' distributed as sum_i lambda_i Z_i^2, with Z_i independent standard normal
#' and lambda_i the eigenvalues of (R H^{-1} R')^{-1} (R V R'): H^{-1} the
#' inverse Hessian of the larger model, V its cluster-robust covariance
#' matrix, R the derivative of the restrictions at the estimates. With a
#' correctly specified model every lambda_i is about 1 and the two tests
#' agree; mean_weight above 1 means the classical test rejects too often. The
#' cluster-robust variance of parameters that only a small Berufsgruppe
#' identifies is biased downward, which pushes the weights below 1: in a small
#' Berufsgruppe the robust test then rejects too often. (On simulated data from
#' a correctly specified model, a group of about 60 experts had a mean weight
#' of 0.7 in the test of its correlations; old lr_tests(), commit 93010be.)
#'
#' The robust p-value is simulated with n_draws draws (with 1,000,000, its
#' standard error is at most 0.0005). A negative LR means the larger model
#' has not reached its maximum; its p-values are then NA.
#'
#' @param results_list named list of saved results
#' @param tests        tibble: restricted, unrestricted (model names),
#'                     restrictions (types of lr_restrictions(), several
#'                     joined by " + ")
#' @param n_draws      draws for the robust p-value
#' @param seed         set before the draws of each test, so that a test's
#'                     p-value does not depend on the tests before it
#' @return tibble, one row per test: restricted, unrestricted, restrictions,
#'   lr, df, p, p_robust, mean_weight
lr_tests <- function(results_list, tests, n_draws = 1e6, seed = 20261004) {
  # the columns from the start, so that the result has them even without a test
  table <- tibble(restricted = character(0), unrestricted = character(0),
                  restrictions = character(0), lr = numeric(0), df = numeric(0),
                  p = numeric(0), p_robust = numeric(0), mean_weight = numeric(0))
  for (i in seq_len(nrow(tests))) {
    restricted <- results_list[[tests$restricted[i]]]
    unrestricted <- results_list[[tests$unrestricted[i]]]
    if (is.null(restricted) || is.null(unrestricted)) {
      message("LR test skipped, results missing: ", tests$restricted[i],
              " against ", tests$unrestricted[i])
      next
    }

    # (a) The statistic: both models on the same sample with the same draws
    if (restricted$fit$n_choices != unrestricted$fit$n_choices ||
        restricted$fit$n_experts != unrestricted$fit$n_experts ||
        restricted$draws$n_draws_model != unrestricted$draws$n_draws_model) {
      stop("LR test ", tests$restricted[i], " against ", tests$unrestricted[i],
           ": different samples or draws")
    }
    lr <- 2 * (unrestricted$fit$log_likelihood - restricted$fit$log_likelihood)
    df <- unrestricted$fit$n_parameters - restricted$fit$n_parameters

    # (b) R: one row per restriction, one column per coefficient of the larger
    #     model. numDeriv::jacobian(): the numerical derivatives of the
    #     restrictions with respect to theta; it passes the arguments after
    #     theta on to lr_restrictions(). The rows must be df linearly
    #     independent ones (qr()$rank: the number of linearly independent rows).
    theta <- unrestricted$coefficients
    # strsplit(): the types, e.g. "regressors + prices" -> both
    types <- strsplit(tests$restrictions[i], " + ", fixed = TRUE)[[1]]
    R <- NULL
    for (type in types) {
      R <- rbind(R, numDeriv::jacobian(lr_restrictions, theta, type = type,
                                       unrestricted = unrestricted,
                                       restricted = restricted))
    }
    if (nrow(R) != df || qr(R)$rank != df) {
      stop("LR test ", tests$restricted[i], " against ", tests$unrestricted[i],
           ": the restrictions give ", qr(R)$rank, " distinct rows in ",
           nrow(R), ", but the models differ by ", df, " parameters")
    }

    # (c) The weights: the eigenvalues of (R H^{-1} R')^{-1} (R V R'), which
    #     are those of the symmetric matrix C'^{-1} (R V R') C^{-1}, with C
    #     the Cholesky factor of R H^{-1} R' (chol(): C'C = R H^{-1} R'), so
    #     that eigen() returns real values
    inverse_hessian <- unrestricted$inverse_hessian[names(theta), names(theta)]
    V <- unrestricted$vcov[names(theta), names(theta)]
    C_inverse <- solve(chol(R %*% inverse_hessian %*% t(R)))
    weights_matrix <- t(C_inverse) %*% (R %*% V %*% t(R)) %*% C_inverse
    weights <- eigen(weights_matrix, symmetric = TRUE, only.values = TRUE)$values

    # (d) p-values. Robust: the share of draws of sum_i lambda_i Z_i^2 at
    #     least as large as LR (rchisq(df = 1): draws of Z^2)
    if (lr < 0) {
      message("Negative LR statistic: ", tests$unrestricted[i],
              " has a lower log-likelihood than ", tests$restricted[i])
      p_classical <- NA_real_
      p_robust <- NA_real_
    } else {
      p_classical <- pchisq(lr, df = df, lower.tail = FALSE)
      set.seed(seed)
      weighted_sum <- numeric(n_draws)
      for (weight in weights) {
        weighted_sum <- weighted_sum + weight * rchisq(n_draws, df = 1)
      }
      p_robust <- mean(weighted_sum >= lr)
    }

    table <- bind_rows(table, tibble(
      restricted = tests$restricted[i],
      unrestricted = tests$unrestricted[i],
      restrictions = tests$restrictions[i],
      lr = lr,
      df = df,
      p = p_classical,
      p_robust = p_robust,
      mean_weight = mean(weights)
    ))
  }
  table
}

# 4. LaTeX tables --------------------------------------------------------------

#' A complete table float: caption, label, a booktabs tabular and the notes,
#' in a threeparttable (profile: the code writes the float). The preamble
#' needs booktabs and threeparttable.
#'
#' @param column_spec the tabular's columns, e.g. "lcccccc"
#' @param header      lines of the header, without the rules
#' @param body        lines of the body
#' @param caption     caption, LaTeX
#' @param label       label, e.g. "tab:rcl_fit"
#' @param notes       notes, LaTeX, one paragraph
#' @param font_size   e.g. "\\scriptsize", for the table and the notes
#' @return character vector, the lines of the .tex file
table_float <- function(column_spec, header, body, caption, label, notes,
                        font_size) {
  c("\\begin{table}[htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    font_size,
    "\\setlength{\\tabcolsep}{4pt}",
    # rows a little closer than LaTeX's default, so that a table of eight
    # attributes with standard errors fits a 16:9 frame
    "\\renewcommand{\\arraystretch}{0.86}",
    paste0("\\begin{tabular}{", column_spec, "}"),
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    font_size,
    # not justified: in a narrow table, justified notes have wide gaps
    "\\raggedright",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}")
}

#' A header cell over several lines: a one-column tabular inside the cell, so
#' that \\ breaks the line, e.g. "Heat\\\\pump"
header_cell <- function(text) {
  paste0("\\begin{tabular}[b]{@{}c@{}}", text, "\\end{tabular}")
}

#' A row with one cell across the table, e.g. the title of a panel
panel_row <- function(text, n_columns) {
  paste0("\\multicolumn{", n_columns, "}{l}{\\textit{", text, "}} \\\\")
}

#' Two rows of a table: the estimates, with stars if p is given, and below
#' them the standard errors in parentheses. NA gives an empty cell.
#'
#' @param label    row label, LaTeX
#' @param estimate,se,p one value per column
#' @param digits   decimals
estimate_rows <- function(label, estimate, se, p = NULL, digits = 2) {
  estimate_text <- latex_number(estimate, digits)
  if (!is.null(p)) estimate_text <- paste0(estimate_text, star_text(p))
  se_text <- paste0("(", latex_number(se, digits), ")")
  se_text[is.na(se)] <- ""
  c(paste0(label, " & ", paste(estimate_text, collapse = " & "), " \\\\"),
    paste0(" & ", paste(se_text, collapse = " & "), " \\\\"))
}

#' One row of a table for one coefficient that holds for all columns, e.g.
#' the price coefficient in a table with a column per technology: the
#' estimate, with stars, and the standard error in parentheses beside it,
#' centred across the columns (one row instead of two, so that the table
#' still fits a frame)
#'
#' @param label     row label, LaTeX
#' @param estimate,se,p one value each
#' @param n_columns the columns the cell spans
#' @param digits    decimals
spanning_row <- function(label, estimate, se, p, n_columns, digits = 2) {
  paste0(label, " & \\multicolumn{", n_columns, "}{c}{",
         latex_number(estimate, digits), star_text(p), " (",
         latex_number(se, digits), ")} \\\\")
}

#' Lines of a .tex file that defines numbers for the text of the slides, e.g.
#' \newcommand{\rclExperts}{340}
numbers_tex <- function(values) {
  paste0("\\newcommand{\\", names(values), "}{", values, "}")
}

# 5. Figures -------------------------------------------------------------------

#' The look of every figure of the mixed logits: theme_minimal(), the legend
#' at the bottom, the title and subtitle aligned with the plot's left edge
#' (profile: these figures keep their titles), the caption below
theme_rcl <- function(base_size) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(size = base_size - 1, colour = "grey25"),
      plot.caption = element_text(size = base_size - 2, colour = "grey35",
                                  hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = base_size - 1)
    )
}

#' Estimates with their 95% intervals, one row of the vertical axis per item
#' (an attribute, an outcome) and one point per series (a Berufsgruppe, a
#' model), optionally in panels
#'
#' @param plot_data one row per item x series (x panel): item (factor; its
#'                  first level at the bottom), series (factor; its first
#'                  level at the top of each item), estimate, ci_low,
#'                  ci_high, and panel (factor) if in panels
#' @param colours,shapes named by the levels of series; shapes with a fill
#'                  (21 to 25), filled in the series' colour
#' @param title,subtitle,caption texts of the figure
#' @param x_title   title of the axis of the estimates
#' @param base_size font size in pt
#' @param legend_rows rows of the legend
#' @return ggplot
plot_estimates <- function(plot_data, colours, shapes, title, subtitle, caption,
                           x_title, base_size, legend_rows = 1) {
  series_levels <- levels(plot_data$series)
  missing_series <- setdiff(series_levels, names(colours))
  if (length(missing_series) > 0) {
    stop("No colour for: ", paste(missing_series, collapse = ", "))
  }

  # coord_flip() puts the items on the vertical axis; position_dodge() places
  # the series next to each other within an item, in the order of `group`:
  # reversed, so that the first series is at the top
  plot <- ggplot(plot_data,
                 aes(x = item, y = estimate, ymin = ci_low, ymax = ci_high,
                     colour = series, shape = series, fill = series,
                     group = factor(series, levels = rev(series_levels)))) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    # key_glyph = "point": the legend shows the symbols without the lines
    geom_pointrange(position = position_dodge(width = 0.7), size = 0.25,
                    linewidth = 0.4, stroke = 0.6, key_glyph = "point") +
    coord_flip() +
    scale_colour_manual(values = colours[series_levels]) +
    scale_shape_manual(values = shapes[series_levels]) +
    scale_fill_manual(values = colours[series_levels]) +
    # One legend for colour, symbol and fill, its symbols a little larger
    guides(colour = guide_legend(nrow = legend_rows,
                                 override.aes = list(size = 1.8)),
           shape = guide_legend(nrow = legend_rows),
           fill = guide_legend(nrow = legend_rows)) +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = NULL, y = x_title) +
    theme_rcl(base_size) +
    theme(legend.key.height = unit(base_size, "pt"),
          legend.key.spacing.y = unit(1, "pt"))
  if ("panel" %in% names(plot_data)) {
    plot <- plot + facet_wrap(~panel, nrow = 1)
  }
  plot
}

#' The rows of a table of all models (e.g. the probabilities or the AMEs) for
#' the panels of a figure across models, one panel per Berufsgruppe: each
#' model's rows of that Berufsgruppe, or, for a model without Berufsgruppe,
#' its rows for all experts in every panel
#'
#' @param table  one row per model x group x ...: columns model and group
#' @param models the models of the figure
#' @param groups the Berufsgruppen, one panel each
#' @return the rows, with the column panel (the Berufsgruppe of the panel)
rows_by_panel <- function(table, models, groups) {
  panel_rows <- tibble()
  for (model_name in models) {
    model_rows <- table %>%
      filter(model == model_name)
    for (group_name in groups) {
      if ("All experts" %in% model_rows$group) {
        rows <- model_rows %>%
          filter(group == "All experts")
      } else {
        rows <- model_rows %>%
          filter(group == group_name)
      }
      if (nrow(rows) == 0) stop("No rows of ", group_name, " in ", model_name)
      panel_rows <- bind_rows(panel_rows, rows %>%
                                mutate(panel = group_name))
    }
  }
  panel_rows
}

#' Heat map of the correlations of the constants of one covariance block,
#' with 1 on the diagonal. A correlation is bold if its 95% Krinsky-Robb
#' percentile interval excludes 0.
#'
#' @param heatmap_data one row per pair of technologies, both orders and the
#'                     diagonal: row, column (factors of the labels; the
#'                     first level at the top and on the left), estimate,
#'                     is_diagonal, is_significant (NA on the diagonal)
#' @param title,subtitle,caption texts of the figure
#' @param base_size    font size in pt
#' @param text_size    size of the numbers in the tiles, in mm (geom_text())
#' @return ggplot
plot_correlations <- function(heatmap_data, title, subtitle, caption, base_size,
                              text_size) {
  # From red (-1) over light grey (0) to blue (1), the colours of the heat
  # maps of the policy brief; the diagonal in grey, outside this scale. White
  # numbers on the darkest tiles.
  heatmap_data <- heatmap_data %>%
    mutate(
      fill_value = if_else(is_diagonal, NA_real_, estimate),
      label = if_else(is_diagonal, "1",
                      formatC(estimate, format = "f", digits = 2)),
      text_colour = if_else(!is_diagonal & abs(estimate) > 0.7, "white", "#2F3B4A"),
      fontface = if_else(!is_diagonal & is_significant, "bold", "plain")
    )
  ggplot(heatmap_data, aes(x = column, y = row, fill = fill_value)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = label, colour = text_colour, fontface = fontface),
              size = text_size) +
    scale_fill_gradient2(low = "#E15759", mid = "#F2F2F2", high = "#4E79A7",
                         midpoint = 0, limits = c(-1, 1), na.value = "grey85") +
    scale_colour_identity() +
    scale_x_discrete(position = "top") +
    coord_fixed() +
    labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    theme_rcl(base_size) +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.text = element_text(colour = "#2F3B4A"),
          # the column labels at an angle, so that neighbours do not touch
          axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0))
}

#' Save a figure as a vector PDF for pdfLaTeX, at a fixed size so that the
#' text has the same size in every figure. cairo_pdf (R's PDF device through
#' the Cairo library) embeds the fonts and prints a hyphen as a hyphen.
#'
#' @param size c(width, height) in inches
save_figure_pdf <- function(plot, file, size) {
  ggsave(file, plot, width = size[["width"]], height = size[["height"]],
         device = cairo_pdf)
}
