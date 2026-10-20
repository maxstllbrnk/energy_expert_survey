################################################################################
# 00_functions_estimation.R — functions for the mixed (random coefficient)
#                             logit models of the recommendations: data,
#                             specifications, estimation, covariance matrix,
#                             marginal effects, likelihood-ratio tests
# Inputs : none; sourced by 01_run_specifications.R and 02_output.R
# Outputs: none
# Checks : inside the functions: the specifications; missing values and keys
#          of the estimation data; a design matrix of full rank; coefficient
#          names against mlogit; convergence; the re-evaluated gradient; a
#          positive definite Hessian; the standard deviations against mlogit
################################################################################
# The functions generalise sections 4 to 8 of
# R/first_policy_brief/03_rc_logit_with_ame.R, so that they work for every
# specification in 01_run_specifications.R: other regressors and
# interactions, one set of constants per group (Berufsgruppe), other random
# coefficients than the constants, other distributions than the normal, and
# covariance matrices with blocks fixed at zero. A function with a counterpart
# there says so in its header and follows it step by step; what differs is
# marked "Generalisation:".
#
# The model (Train 2009, chapters 6 and 10). Expert n, technology j, vignette t:
#
#   U_njt = x_nt' beta_nj + e_njt,   U_n0t = e_n0t (reference outcome)
#
# x_nt holds a 1 for the constant and the regressors of the vignette and the
# expert; e_njt is iid type-I extreme value. A coefficient is either fixed,
# the same for every expert, or random: it varies across experts and is the
# same in all of an expert's vignettes. The random coefficients of expert n
# are normal,
#
#   beta_n = b + L z_n,   z_n ~ N(0, I),   Omega = L L',
#
# or a transformation of b + L z_n for the other distributions
# (coefficient_draws()). In the policy brief, the constants are the random
# coefficients, all normal. The probability of an expert's choices is a
# product of logits, integrated over the random coefficients (the panel
# likelihood); mlogit() simulates the integral with R draws per expert
# (maximum simulated likelihood).
#
# The specifications are written in section 3 of 01_run_specifications.R,
# which describes their elements. From a specification and the data, the
# functions build what mlogit and the later steps need:
#   formula_terms        the terms of mlogit's formula (formula_terms())
#   columns              the columns of the design matrix, each with one
#                        coefficient per technology (design_columns())
#   random_coefficients  the random coefficients with their distribution,
#                        correlation block and Halton prime
#                        (random_coefficients(), halton_primes())

# 1. Specifications ------------------------------------------------------------

#' Text as part of a file or folder name: lower case, umlauts written out,
#' every other character but letters and digits replaced by "_", e.g.
#' "Wärmepumpe" -> "waermepumpe" (as in the folder names of the policy brief),
#' "Heat pump" -> "heat_pump"
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

#' Check that a specification has its six elements and that they fit together
#' (section 3 of 01_run_specifications.R describes the elements)
#'
#' @param specification list
#' @param name          its name in SPECIFICATIONS, for the error messages
check_specification <- function(specification, name) {
  missing_elements <- setdiff(c("label", "constants", "regressors", "random",
                                "correlation", "draws"),
                              names(specification))
  if (length(missing_elements) > 0) {
    stop("Specification ", name, " has no ",
         paste(missing_elements, collapse = ", "))
  }
  # identical(): TRUE if both are exactly the same, here the single value "1"
  has_groups <- !identical(specification$constants, "1")

  # (a) Regressors: each one term, as in a formula. "a * b" would be three
  #     terms (a, b and a:b), so it is written as three regressors. terms()
  #     splits a formula into its terms; "term.labels" holds their names.
  for (regressor in specification$regressors) {
    regressor_terms <- attr(terms(as.formula(paste("~", regressor))), "term.labels")
    if (length(regressor_terms) != 1) {
      stop("Specification ", name, ": ", regressor, " is not a single term; ",
           "write a * b as the three regressors a, b and a:b")
    }
  }
  # The terms of the specification, the names that `random` and
  # `correlation` use: "(Intercept)" or the group dummies, then the regressors
  if (has_groups) {
    terms <- c(specification$constants, specification$regressors)
  } else {
    terms <- c("(Intercept)", specification$regressors)
  }
  if (anyDuplicated(terms) > 0) {
    stop("Specification ", name, ": a term appears twice")
  }

  # (b) Random coefficients: terms of the specification, each with a
  #     distribution that mlogit knows
  random <- specification$random
  if (length(random) == 0 || is.null(names(random))) {
    stop("Specification ", name, ": random must name at least one term, ",
         "e.g. c(\"(Intercept)\" = \"n\")")
  }
  unknown_terms <- setdiff(names(random), terms)
  if (length(unknown_terms) > 0) {
    stop("Specification ", name, ": random names terms that are not in the ",
         "specification: ", paste(unknown_terms, collapse = ", "))
  }
  if (!all(random %in% c("n", "ln", "cn", "u", "t"))) {
    stop("Specification ", name, ": the distributions must be n, ln, cn, u or t")
  }

  # (c) Correlation: "none", "full", or blocks that together hold every
  #     random term exactly once
  correlation <- specification$correlation
  if (is.list(correlation)) {
    # unlist(): the terms of all blocks in one vector
    terms_in_blocks <- unlist(correlation)
    if (anyDuplicated(terms_in_blocks) > 0 ||
        !setequal(terms_in_blocks, names(random))) {
      stop("Specification ", name, ": the correlation blocks must hold every ",
           "random term exactly once")
    }
  } else if (!(identical(correlation, "none") || identical(correlation, "full"))) {
    stop("Specification ", name, ": correlation must be \"none\", \"full\" ",
         "or a list of blocks")
  }
  # mlogit transforms correlated random coefficients only if they are
  # log-normal or censored normal; correlated uniform or triangular ones would
  # be estimated as normal (make.beta() in mlogit 2.0.0)
  if (!identical(correlation, "none") && any(random %in% c("u", "t"))) {
    stop("Specification ", name, ": uniform (u) and triangular (t) ",
         "coefficients only with correlation \"none\"")
  }

  # (d) Group constants. Every expert belongs to one group, so the covariance
  #     between two groups' constants never enters the likelihood: it is not
  #     identified, and no correlation may link two groups' constants
  if (has_groups) {
    random_groups <- intersect(specification$constants, names(random))
    if (identical(correlation, "full") && length(random_groups) > 1) {
      stop("Specification ", name, ": with random group constants, use one ",
           "correlation block per group instead of \"full\"")
    }
    if (is.list(correlation)) {
      for (block in correlation) {
        if (length(intersect(block, specification$constants)) > 1) {
          stop("Specification ", name, ": a correlation block holds the ",
               "constants of two groups")
        }
      }
    }
  }

  # (e) Draws. With halton_by_group, the groups' constants share their draws
  #     (halton_primes()). That is right only if no expert's utility depends
  #     on the draws of another group: a group's constants may be correlated
  #     only with each other.
  if (!(identical(specification$draws, "halton") ||
        identical(specification$draws, "halton_by_group"))) {
    stop("Specification ", name, ": draws must be halton or halton_by_group")
  }
  if (specification$draws == "halton_by_group") {
    random_groups <- intersect(specification$constants, names(random))
    if (!has_groups || length(random_groups) == 0) {
      stop("Specification ", name, ": halton_by_group needs random group constants")
    }
    if (identical(correlation, "full")) {
      stop("Specification ", name, ": halton_by_group needs correlation ",
           "\"none\" or one block per group")
    }
    if (is.list(correlation)) {
      for (block in correlation) {
        if (any(block %in% specification$constants) && length(block) > 1) {
          stop("Specification ", name, ": with halton_by_group, each group's ",
               "constants must be a correlation block of their own")
        }
      }
    }
  }
  invisible(TRUE)
}

#' The columns of an interaction term, as products for mlogit's formula
#'
#' R codes the interaction a:b of two factors with one column per pair of
#' levels but the first (the reference), 1 where both hold. For
#' berufsgruppe:att_income: one column for chimney sweeps on vignettes with
#' the low income, one for energy advisers. A numeric variable enters as
#' itself. This is R's coding when the specification also has a and b (for
#' the Berufsgruppe: its main effect or its group constants); the
#' coefficients are then the differences in the effect of b between a level
#' of a and its reference.
#'
#' mlogit 2.0 cannot take a:b itself: it cuts each coefficient name at its
#' ":" and keeps two parts, so the coefficients of the interaction of two
#' factors would share their names. Each column is therefore written as I()
#' of a product of comparisons, e.g.
#'   I((berufsgruppe == "Schornsteinfeger") *
#'     (att_income == "unter 40.000 € brutto im Jahr"))
#' which mlogit() and model.matrix() evaluate in any data with these
#' variables, also in the vignettes of the AMEs.
#'
#' @param term an interaction of variables in `data`, e.g.
#'             "berufsgruppe:att_income"
#' @param data the decisions, with every factor at its levels
#' @return tibble, one row per column: term, formula_term (the I()
#'   expression) and r_name (the column's name in R's own coding of a:b,
#'   for the labels of the output)
interaction_columns <- function(term, data) {
  # strsplit(): the parts of the term between its colons, its variables
  variables <- strsplit(term, ":", fixed = TRUE)[[1]]
  expressions <- NULL
  r_names <- NULL
  for (variable in variables) {
    if (!(variable %in% names(data))) {
      stop("Interaction ", term, ": ", variable, " is not a variable in the data")
    }
    # The columns of this variable: one comparison per level but the first,
    # or the variable itself
    if (is.factor(data[[variable]])) {
      variable_levels <- levels(data[[variable]])[-1]
      if (any(grepl("\"", variable_levels, fixed = TRUE))) {
        stop("Interaction ", term, ": a level of ", variable, " contains a quote")
      }
      variable_expressions <- paste0("(", variable, " == \"", variable_levels,
                                     "\")")
      variable_names <- paste0(variable, variable_levels)
    } else if (is.numeric(data[[variable]])) {
      variable_expressions <- variable
      variable_names <- variable
    } else {
      stop("Interaction ", term, ": make ", variable, " a factor or a number ",
           "in the data section of 01_run_specifications.R")
    }
    # Multiply with the columns of the variables before: outer() pairs every
    # column so far with every column of this variable, and as.vector() lists
    # the pairs with the earlier variables varying fastest, as R orders them
    if (is.null(expressions)) {
      expressions <- variable_expressions
      r_names <- variable_names
    } else {
      expressions <- as.vector(outer(expressions, variable_expressions, paste,
                                     sep = " * "))
      r_names <- as.vector(outer(r_names, variable_names, paste, sep = ":"))
    }
  }
  tibble(term = term, formula_term = paste0("I(", expressions, ")"),
         r_name = r_names)
}

#' The terms of mlogit's formula after `|`, which are also those of the
#' design matrix
#'
#' First the constants: "1" (one per technology), or "0" and the group
#' dummies (one per technology and group). Then the regressors, each
#' interaction as its columns (interaction_columns()).
#'
#' @param specification list, see section 3 of 01_run_specifications.R
#' @param data          the decisions, with every factor at its levels
#' @return tibble, one row per term of the formula: term (as in the
#'   specification; NA for the "0"), formula_term (as in the formula) and
#'   r_name (for a column of an interaction, its name in R's coding; NA
#'   otherwise)
formula_terms <- function(specification, data) {
  if (identical(specification$constants, "1")) {
    result <- tibble(term = "(Intercept)", formula_term = "1", r_name = NA_character_)
  } else {
    result <- tibble(term = c(NA, specification$constants),
                     formula_term = c("0", specification$constants),
                     r_name = NA_character_)
  }
  for (regressor in specification$regressors) {
    # "order" of a term: 1 for a variable, 2 for an interaction of two
    regressor_order <- attr(terms(as.formula(paste("~", regressor))), "order")
    if (regressor_order > 1) {
      result <- bind_rows(result, interaction_columns(regressor, data))
    } else {
      result <- bind_rows(result, tibble(term = regressor, formula_term = regressor,
                                         r_name = NA_character_))
    }
  }
  result
}

#' Design matrix of a specification, as model.matrix(design_formula, ...) in
#' the policy brief script: one row per row of `data`, one column per
#' coefficient of each technology, named as in mlogit's coefficients
#' "<column>:<technology>". With group constants ("0" in formula_terms), the
#' matrix is built with a constant and that column is dropped: "~ 0 + ..."
#' would code the first factor with all its levels, unlike mlogit
#' (estimate_specification() checks the names against mlogit's).
#'
#' The attribute "assign" of the matrix gives, for each column, the term
#' behind it: its position among the rows of formula_terms other than "0"
#' and "1", or 0 for the constant (design_columns() uses it).
#'
#' @param formula_terms from formula_terms()
#' @param data          one row per vignette, with every variable of the terms
#' @return matrix
design_matrix <- function(formula_terms, data) {
  is_constant <- formula_terms$formula_term %in% c("0", "1")
  design_terms <- c("1", formula_terms$formula_term[!is_constant])
  design_formula <- as.formula(paste("~", paste(design_terms, collapse = " + ")))
  # keep.order = TRUE keeps the terms in the order of formula_terms, so that
  # "assign" refers to that order
  X <- model.matrix(terms(design_formula, keep.order = TRUE), data)
  # model.matrix() drops rows with a missing value without notice
  if (nrow(X) != nrow(data)) {
    stop("Missing values in the variables of the design matrix")
  }
  if ("0" %in% formula_terms$formula_term) {
    assign <- attr(X, "assign")
    is_kept <- colnames(X) != "(Intercept)"
    X <- X[, is_kept, drop = FALSE]
    attr(X, "assign") <- assign[is_kept]
  }
  X
}

#' The term of the specification behind each column of the design matrix
#'
#' @param formula_terms from formula_terms()
#' @param X             design matrix from design_matrix() with these terms
#' @return tibble, one row per column of X, in its order:
#'   column  its name, the first part of mlogit's coefficient names
#'   term    the term of the specification it belongs to
#'   r_name  its name in R's own coding, for the labels of the output: for a
#'           column of an interaction e.g. "berufsgruppeSchornsteinfeger:
#'           att_incomeunter 40.000 € brutto im Jahr", otherwise the column
design_columns <- function(formula_terms, X) {
  terms_in_matrix <- formula_terms %>%
    filter(!(formula_term %in% c("0", "1")))
  assign <- attr(X, "assign")
  if (is.null(assign) || max(assign) > nrow(terms_in_matrix)) {
    stop("The design matrix was not built from these formula terms")
  }
  columns <- tibble(column = colnames(X), term = NA_character_,
                    r_name = NA_character_)
  for (k in seq_len(ncol(X))) {
    if (assign[k] == 0) {
      columns$term[k] <- "(Intercept)"
      columns$r_name[k] <- "(Intercept)"
    } else {
      term_row <- assign[k]
      columns$term[k] <- terms_in_matrix$term[term_row]
      if (is.na(terms_in_matrix$r_name[term_row])) {
        columns$r_name[k] <- colnames(X)[k]
      } else {
        columns$r_name[k] <- terms_in_matrix$r_name[term_row]
      }
    }
  }
  columns
}

# 2. Data ----------------------------------------------------------------------

#' Decisions in the format mlogit() needs (section 2 of the policy brief
#' script)
#'
#' Keeps the variables of the specifications, makes the outcome and the
#' vignette attributes factors with the given levels (the first level is the
#' reference category), numbers the choice situations (chid) and reshapes the
#' data with dfidx() to one row per choice situation and outcome. idx nests
#' each choice situation (chid) within its expert (resp_uid), which panel =
#' TRUE uses.
#'
#' Every variable must be non-missing: mlogit() would drop choice situations
#' with a missing value without notice, and the specifications would no longer
#' share one sample. The function stops instead; restrict the sample visibly
#' in the data section of 01_run_specifications.R.
#'
#' @param decisions        one row per expert x vignette: resp_uid, vig_num,
#'                         vig_rec and the variables
#' @param outcomes         the possible recommendations, reference outcome first
#' @param attribute_levels levels of the vignette attributes; the first level
#'                         is the reference category
#' @param variables        every variable that a specification uses
#' @return list with
#'   decisions    one row per choice situation, in the order of chid, with the
#'                factors set (for design matrices and checks)
#'   choice_data  the same in dfidx format (for mlogit())
prepare_choice_data <- function(decisions, outcomes, attribute_levels, variables) {
  variables <- unique(variables)
  columns <- c("resp_uid", "vig_num", "vig_rec", variables)
  missing_columns <- setdiff(columns, names(decisions))
  if (length(missing_columns) > 0) {
    stop("Not in the data: ", paste(missing_columns, collapse = ", "))
  }
  decisions <- decisions[columns]

  # (a) Factors. A value that is not among the levels becomes NA, so (b) also
  #     catches spelling differences between the data and the levels.
  decisions$vig_rec <- factor(decisions$vig_rec, levels = outcomes)
  for (attribute in intersect(names(attribute_levels), variables)) {
    decisions[[attribute]] <- factor(decisions[[attribute]],
                                     levels = attribute_levels[[attribute]])
  }

  # (b) Missing values: the outcome (a vignette not answered, or an answer
  #     that is not in `outcomes`) and every variable
  n_missing <- c(vig_rec = sum(is.na(decisions$vig_rec)))
  for (variable in variables) {
    n_missing[variable] <- sum(is.na(decisions[[variable]]))
  }
  if (any(n_missing > 0)) {
    print(n_missing[n_missing > 0])
    stop("Missing values in the estimation variables (counts above). ",
         "Restrict the sample visibly in the data section of ",
         "01_run_specifications.R")
  }

  # (c) Every outcome must be chosen at least once; otherwise its constant is
  #     not identified
  choice_counts <- table(decisions$vig_rec)
  if (any(choice_counts == 0)) {
    stop("Never chosen in this sample: ",
         paste(names(choice_counts)[choice_counts == 0], collapse = ", "))
  }

  # (d) One choice situation per expert x vignette. anyDuplicated(): the
  #     position of the first row that repeats an earlier one, 0 if none.
  if (anyDuplicated(decisions[c("resp_uid", "vig_num")]) > 0) {
    stop("More than one row per expert and vignette")
  }
  decisions <- decisions %>%
    arrange(resp_uid, vig_num) %>%
    mutate(chid = row_number())

  # (e) One row per choice situation x outcome
  choice_data <- dfidx(
    decisions,
    shape = "wide",
    choice = "vig_rec",
    idx = list(c("chid", "resp_uid")),
    idnames = c(NA, "alternatives")
  )

  list(decisions = decisions, choice_data = choice_data)
}

# 3. Estimation ----------------------------------------------------------------

#' The random coefficients of a specification, in mlogit's order
#'
#' Every coefficient of a random term is random: one per column of the term
#' and technology. mlogit orders them as their coefficients in coef(), and
#' cholesky_factor(), halton_primes() and the draws follow that order.
#'
#' @param columns           from design_columns()
#' @param coefficient_names names(coef()) of the multinomial logit
#' @param technologies      the outcomes other than the reference outcome
#' @return tibble, one row per random coefficient, in the order of coef():
#'   name          mlogit's name of the coefficient, "<column>:<technology>"
#'   column, technology, term
#'   distribution  "n", "ln", "cn", "u" or "t", from specification$random
#'   block         number of its correlation block: NA with "none", 1 for all
#'                 with "full", the position of its block in the list otherwise
#'   group         for a group constant, its group dummy; NA otherwise
random_coefficients <- function(specification, columns, coefficient_names,
                                technologies) {
  random <- specification$random

  # (a) The coefficients of the random terms: one per column and technology
  random_columns <- columns %>%
    filter(term %in% names(random))
  result <- tibble()
  for (k in seq_len(nrow(random_columns))) {
    result <- bind_rows(result, tibble(
      name = paste0(random_columns$column[k], ":", technologies),
      column = random_columns$column[k],
      technology = technologies,
      term = random_columns$term[k]
    ))
  }

  # (b) In the order of coef(). match(): the position of each name among
  #     coefficient_names, NA if it is not there.
  result <- result %>%
    mutate(position = match(name, coefficient_names)) %>%
    arrange(position)
  if (anyNA(result$position)) {
    stop("Random coefficients that are not among mlogit's coefficients")
  }
  result <- result %>%
    select(-position)

  # (c) Distribution, correlation block and group
  result$distribution <- unname(random[result$term])
  result$block <- NA_real_
  if (identical(specification$correlation, "full")) {
    result$block <- 1
  } else if (is.list(specification$correlation)) {
    for (b in seq_along(specification$correlation)) {
      result$block[result$term %in% specification$correlation[[b]]] <- b
    }
  }
  result$group <- NA_character_
  if (!identical(specification$constants, "1")) {
    is_group_constant <- result$term %in% specification$constants
    result$group[is_group_constant] <- result$term[is_group_constant]
  }
  result
}

#' Elements of the Cholesky factor L that link two correlation blocks (as in
#' the draft 05_rc_logit_explore_heterogeneity.R, where the blocks are the
#' Berufsgruppen)
#'
#' Fixing at 0 every element L[i, j] whose coefficients i and j are in
#' different blocks makes the covariance matrix L L' block diagonal, whatever
#' the order of the coefficients: one full covariance matrix per block, zero
#' covariances across blocks. Between the blocks of two groups' constants, the
#' zeros are a normalisation: every expert has the constants of one group
#' only, so their covariances never enter the likelihood and are not
#' identified (like the constant of the reference outcome). Between other
#' blocks, they are a restriction. mlogit names element L[i, j] (i >= j)
#' "chol.<random coefficient j>:<random coefficient i>", in the order of
#' coef().
#'
#' @param random_coefficients from random_coefficients()
#' @return named vector of zeros, the constPar argument of mlogit(); empty if
#'   all random coefficients are in one block
cholesky_fixed_at_zero <- function(random_coefficients) {
  rpar_names <- random_coefficients$name
  block <- random_coefficients$block
  fixed_names <- character(0)
  for (i in seq_along(rpar_names)) {
    for (j in seq_len(i)) {
      if (block[i] != block[j]) {
        fixed_names <- c(fixed_names,
                         paste0("chol.", rpar_names[j], ":", rpar_names[i]))
      }
    }
  }
  fixed_parameters <- rep(0, length(fixed_names))
  names(fixed_parameters) <- fixed_names
  fixed_parameters
}

#' The Halton prime of each random coefficient, and the halton argument of
#' mlogit()
#'
#' mlogit draws each random coefficient from its own Halton sequence: the
#' k-th random coefficient in the order of coef() from the k-th prime of its
#' list (2, 3, 5, 7, ...; make.random.nb() in mlogit 2.0.0, whose list skips
#' 67), each expert from the next R points of every sequence. This order holds
#' when the random coefficients are all correlated or all uncorrelated, as
#' here; a partly correlated model would list the uncorrelated ones first.
#'
#' "halton": mlogit's default, halton = NA, as in the policy brief.
#'
#' "halton_by_group": the primes start again at 2 for each group's constants;
#' the other random coefficients, which every expert has, take the primes
#' after them. With three groups of six constants, mlogit's default gives the
#' last group the primes 41 to 61, whose Halton sequences cover the unit cube
#' unevenly unless very many points are used (Train 2009, section 9.3.3). An
#' expert's utility contains only the constants of their own group, so each
#' expert needs only those draws, and the groups can share the first primes.
#' On simulated data with 400 draws per expert, this cut the error of the
#' simulated log-likelihood of an expert in the last group by about three
#' quarters. check_specification() allows it only where no expert's utility
#' depends on the draws of another group.
#'
#' mlogit() also accepts an element `drop` in the list, but R 4.3 and later
#' then stop with "'length = 2' in coercion to 'logical(1)'"; without it,
#' mlogit drops the first 100 elements of each sequence, as with NA.
#'
#' @param draws               "halton" or "halton_by_group"
#' @param random_coefficients from random_coefficients()
#' @return list: primes (one per random coefficient) and halton (the argument
#'   of mlogit())
halton_primes <- function(draws, random_coefficients) {
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59,
              61, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131,
              137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197,
              199)
  K <- nrow(random_coefficients)
  if (draws == "halton") {
    if (K > length(primes)) {
      stop("More than ", length(primes), " random coefficients")
    }
    list(primes = primes[seq_len(K)], halton = NA)
  } else {
    group <- random_coefficients$group
    prime_of <- numeric(K)
    n_largest_group <- 0
    for (g in unique(group[!is.na(group)])) {
      is_in_group <- !is.na(group) & group == g
      prime_of[is_in_group] <- primes[seq_len(sum(is_in_group))]
      n_largest_group <- max(n_largest_group, sum(is_in_group))
    }
    is_common <- is.na(group)
    prime_of[is_common] <- primes[n_largest_group + seq_len(sum(is_common))]
    if (anyNA(prime_of)) {
      stop("More random coefficients than the ", length(primes), " primes")
    }
    list(primes = prime_of, halton = list(prime = prime_of))
  }
}

#' The model with its fitted values, gradient and log-likelihood at its
#' coefficients
#'
#' When mlogit() stops with convergence code 2 (successive log-likelihoods
#' within tolerance), it returns the coefficients of the last iterate but the
#' fitted values, gradient and log-likelihood of the trial point one small
#' step further. Checks that compare them (check (b) in
#' 01_run_specifications.R, the gradient check in vcov_cluster_robust()) then
#' fail although nothing is wrong, and the meat of the sandwich sits at the
#' wrong point. Re-evaluating the model at its coefficients with zero
#' iterations makes all of them belong to the coefficients again; the
#' convergence information of the estimation is kept. With the other codes,
#' coefficients and fitted values already belong together.
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

#' Estimate one specification: a multinomial logit with the same regressors
#' (every coefficient fixed) and the mixed logit with its random coefficients
#' (section 4 of the policy brief script)
#'
#' Both are estimated with do.call("mlogit", arguments), which calls mlogit()
#' with the arguments in the list and stores their values, not their names, in
#' the model's call (model$call). This matters because mlogit() looks up some
#' arguments, constPar among them, in the global environment instead of where
#' it is called, so inside this function they would not be found. Only the
#' data enter by name (quote(choice_data)), so that printing a model does not
#' print the data; vcov_cluster_robust() re-evaluates a model with the data
#' passed to it.
#'
#' @param specification     list, see section 3 of 01_run_specifications.R
#' @param choice_data       dfidx data from prepare_choice_data()
#' @param decisions         the decisions from prepare_choice_data()
#' @param reference_outcome the outcome whose utility is normalised to 0
#' @param n_draws_model     Halton draws per expert
#' @return list with the two models (mnl, mixed) and what the later steps
#'   need: formula (mlogit's), formula_terms, columns (from design_columns()),
#'   column_names (of the design matrix), technologies, random_coefficients
#'   (with the Halton prime of each), fixed_parameters (the constPar values;
#'   NULL if there are none)
estimate_specification <- function(specification, choice_data, decisions,
                                   reference_outcome, n_draws_model) {
  technologies <- setdiff(levels(decisions$vig_rec), reference_outcome)

  # (a) Group constants: the dummies must sum to one in every row and be the
  #     same in all of an expert's vignettes, so that every expert has exactly
  #     one set of constants
  if (!identical(specification$constants, "1")) {
    group_sum <- rowSums(as.matrix(decisions[specification$constants]))
    if (any(group_sum != 1)) {
      stop("The group dummies do not sum to one in every row")
    }
    expert_groups <- distinct(decisions[c("resp_uid", specification$constants)])
    if (nrow(expert_groups) != n_distinct(decisions$resp_uid)) {
      stop("The group dummies differ between the vignettes of an expert")
    }
  }

  # (b) Formula and design matrix. mlogit's formula, as choice_formula in the
  #     policy brief script: before `|` come attributes of the technologies
  #     (none: 0); after it the terms of formula_terms(), which get one
  #     coefficient per technology. Each column of the design matrix gets one
  #     coefficient per technology, so it must vary and must not be a linear
  #     combination of the other columns; otherwise its coefficients are not
  #     identified. qr()$rank: the number of linearly independent columns.
  terms_table <- formula_terms(specification, decisions)
  formula <- as.formula(paste("vig_rec ~ 0 |",
                              paste(terms_table$formula_term, collapse = " + ")))
  X <- design_matrix(terms_table, decisions)
  columns <- design_columns(terms_table, X)
  is_zero_column <- colSums(X != 0) == 0
  if (any(is_zero_column)) {
    stop("Columns of the design matrix that are 0 in every decision: ",
         paste(colnames(X)[is_zero_column], collapse = ", "))
  }
  if (qr(X)$rank < ncol(X)) {
    stop("The columns of the design matrix are linearly dependent")
  }

  # (c) Multinomial logit
  m_mnl <- do.call("mlogit", list(formula = formula,
                                  data = quote(choice_data),
                                  reflevel = reference_outcome))
  m_mnl <- reevaluate_at_coefficients(m_mnl, choice_data)

  # The columns of the design matrix times the technologies must be exactly
  # the coefficients of the model (setequal(): same elements, in any order)
  expected_coefs <- outer(colnames(X), technologies, paste, sep = ":")
  if (!setequal(expected_coefs, names(coef(m_mnl)))) {
    stop("The design matrix does not match mlogit's coefficients")
  }

  # (d) Mixed logit: the coefficients of the random terms vary across experts
  #     with their distribution (rpar); the other coefficients are fixed. Each
  #     expert keeps one draw for all six vignettes (panel = TRUE),
  #     n_draws_model per expert. With correlation, mlogit estimates the
  #     Cholesky factor L of their covariance matrix, otherwise one standard
  #     deviation per random coefficient.
  random <- random_coefficients(specification, columns, names(coef(m_mnl)),
                                technologies)
  # mlogit starts a log-normal coefficient at the log of its MNL estimate
  # (mlogit.start()), so that estimate must be positive; otherwise mlogit
  # stops with "missing value where TRUE/FALSE needed"
  log_normal <- random$name[random$distribution == "ln"]
  not_positive <- log_normal[coef(m_mnl)[log_normal] <= 0]
  if (length(not_positive) > 0) {
    stop("A log-normal coefficient needs a positive MNL estimate, its ",
         "starting value in mlogit; use another distribution for: ",
         paste(not_positive, collapse = ", "))
  }
  halton <- halton_primes(specification$draws, random)
  random$prime <- halton$primes
  random_parameters <- random$distribution
  names(random_parameters) <- random$name

  mixed_arguments <- list(
    formula = formula,
    data = quote(choice_data),
    reflevel = reference_outcome,
    rpar = random_parameters,
    panel = TRUE,
    R = n_draws_model,
    halton = halton$halton,
    correlation = !identical(specification$correlation, "none")
  )

  # (e) Correlation blocks: the elements of L that link two blocks are fixed
  #     at 0 (constPar). Check first that these are mlogit's names: a run
  #     with few draws and no iterations returns all elements of L. It uses
  #     mlogit's default draws: with halton_by_group the groups share their
  #     draws, so without the restriction the elements that link two groups
  #     multiply the same draws as those within a group, and mlogit's first
  #     step fails on a singular matrix. The names do not depend on the draws.
  fixed_parameters <- NULL
  if (is.list(specification$correlation)) {
    fixed_parameters <- cholesky_fixed_at_zero(random)
  }
  if (length(fixed_parameters) > 0) {
    names_arguments <- mixed_arguments
    names_arguments$R <- 10
    names_arguments$halton <- NA
    names_arguments$iterlim <- 0
    names_arguments$print.level <- 0
    m_names <- do.call("mlogit", names_arguments)
    # startsWith(): TRUE for the names that begin with "chol."
    cholesky_names <- names(coef(m_names))[startsWith(names(coef(m_names)),
                                                      "chol.")]
    # elements of L within the blocks: K (K + 1) / 2 for a block of K
    block_sizes <- as.numeric(table(random$block))
    n_within <- sum(block_sizes * (block_sizes + 1) / 2)
    if (!all(names(fixed_parameters) %in% cholesky_names) ||
        length(fixed_parameters) != length(cholesky_names) - n_within) {
      stop("The Cholesky elements fixed at 0 are not mlogit's")
    }
    mixed_arguments$constPar <- fixed_parameters
  } else {
    fixed_parameters <- NULL
  }

  # (f) Estimate. The elements fixed at 0 must not be among the estimated
  #     coefficients, coef(): only the elements within blocks are estimated.
  m_mixed <- do.call("mlogit", mixed_arguments)
  
  # Code 3: the line search found no higher log-likelihood along the direction
  # from mlogit's approximation of the inverse Hessian (BFGS), which can be
  # poor near the maximum. A restart from the last iterate, with the same
  # draws, builds it anew. At most three restarts; check_estimation() stops
  # if the code is still not 1 or 2.
  for (restart in 1:3) {
    if (m_mixed$est.stat$code != 3) break
    message("Mixed logit: code 3 after ", m_mixed$est.stat$nb.iter,
            " iterations, log-likelihood ", round(as.numeric(logLik(m_mixed)), 2),
            "; restart ", restart, " from the last iterate")
    # starting values for every parameter, also those fixed by constPar
    start <- setNames(as.numeric(m_mixed$coefficients),
                      names(m_mixed$coefficients))
    m_mixed <- update(m_mixed, start = start)
  }
  
  m_mixed <- reevaluate_at_coefficients(m_mixed, choice_data)
  if (any(names(fixed_parameters) %in% names(coef(m_mixed)))) {
    stop("Cholesky elements fixed at 0 were estimated")
  }

  list(mnl = m_mnl, mixed = m_mixed, formula = formula,
       formula_terms = terms_table, columns = columns,
       column_names = colnames(X), technologies = technologies,
       random_coefficients = random, fixed_parameters = fixed_parameters)
}

#' Stop if a model has not converged or has not used every choice situation
#' (the checks at the end of section 4 of the draft
#' 05_rc_logit_explore_heterogeneity.R)
#'
#' mlogit's convergence codes: 1 gradient close to zero, 2 successive values
#' of the log-likelihood within tolerance, 3 the last step could not find a
#' higher value, 4 iteration limit exceeded. The largest element of the
#' gradient is printed as well; at a maximum it is close to zero.
#'
#' @param model      fitted mlogit model
#' @param model_name name for the messages
#' @param n_choices  number of choice situations in the estimation data
check_estimation <- function(model, model_name, n_choices) {
  code <- model$est.stat$code
  gradient <- colSums(model$gradient[, names(coef(model)), drop = FALSE])
  cat(model_name, ": convergence code", code,
      "| iterations", model$est.stat$nb.iter,
      "| largest gradient element", signif(max(abs(gradient)), 3), "\n")
  if (!(code %in% c(1, 2))) {
    stop("Model ", model_name, " has not converged (code ", code, ")")
  }
  if (length(fitted(model)) != n_choices) {
    stop("Model ", model_name, " does not use all ", n_choices,
         " choice situations")
  }
}

# 4. Covariance matrix of the parameters clustered at the expert level ---------

#' Cluster-robust (sandwich) covariance matrix of the estimates of an mlogit
#' model, clustered by expert (section 5 of the policy brief script):
#'
#'   V = G / (G - 1) * H^{-1} B H^{-1}
#'
#' H     Hessian of -log L at the estimates ("bread").
#'       MNL: mlogit's analytic Hessian; vcov(model) is its inverse.
#'       Mixed logit: mlogit provides none, so H is computed by central
#'       differences of the gradient. The gradient is re-evaluated by
#'       re-running the model's call with iterlim = 0, which reuses the
#'       estimation draws (checked below).
#' B     sum over experts of the outer product of the expert-level scores
#'       ("meat"). model$gradient has one row per choice situation; the rows
#'       of an expert add up to her score.
#' G/(G-1) small-sample factor with G experts (as in Stata and sandwich::vcovCL).
#'
#' Generalisation: parameters fixed by constPar are not estimated. They are
#' not in coef(), but model$gradient has a column for them and mlogit() needs a
#' starting value for them, so the function selects and adds them explicitly.
#' And the data are an argument: the model's call refers to them by the name
#' choice_data (see estimate_specification()). The bread H^{-1} is returned as
#' well, as the attribute "inverse_hessian" of V: the robust LR test
#' (lr_tests()) needs it.
#'
#' @param model       fitted mlogit model (MNL or mixed logit)
#' @param choice_data the data the model was estimated on
#' @param cluster     name of the cluster variable in idx(model)
#' @param h           step size for the numerical derivatives (mixed logit only)
#' @return covariance matrix, rows and columns named as coef(model), with the
#'   inverse Hessian in attr(V, "inverse_hessian")
vcov_cluster_robust <- function(model, choice_data, cluster = "resp_uid", h = 1e-4) {
  theta_hat <- coef(model)                       # without the fixed parameters
  K <- length(theta_hat)

  # Meat: one score vector per expert
  index <- idx(model)
  cluster_id <- index[[cluster]][!duplicated(index$chid)]   # one per choice situation

  expert_gradients <- rowsum(model$gradient[, names(theta_hat), drop = FALSE],
                             cluster_id)                    # G x K
  meat <- t(expert_gradients) %*% expert_gradients          # sum_g s_g s_g'
  G <- nrow(expert_gradients)

  # Bread: inverse Hessian of -log L
  if (is.null(model$rpar)) {
    bread <- unclass(vcov(model))                            # MNL: analytic
  } else {
    # Gradient of -log L at any parameter vector theta: re-estimate the model
    # starting at theta with zero iterations, so that mlogit only evaluates
    # the log-likelihood and its gradient there. model$gradient holds one row
    # per choice situation; -log L is a sum over them, so its gradient is the
    # sum of the rows.
    total_gradient <- function(theta, model) {
      # mlogit needs a starting value for every parameter, also for those
      # fixed by constPar: model$coefficients has all of them, in mlogit's
      # order (as.numeric() drops the attributes mlogit adds)
      start <- setNames(as.numeric(model$coefficients), names(model$coefficients))
      start[names(theta)] <- theta
      # update() evaluates the model's call here. The only name in the call
      # is choice_data, which R finds as the argument of vcov_cluster_robust()
      model_at_theta <- update(model, start = start, iterlim = 0, print.level = 0)
      colSums(model_at_theta$gradient[, names(theta), drop = FALSE])
    }
    # Re-evaluating at the estimates must reproduce the stored gradient,
    # otherwise data, settings or draws differ from the estimation
    gradient_at_estimates <- colSums(model$gradient[, names(theta_hat), drop = FALSE])
    if (max(abs(total_gradient(theta_hat, model) - gradient_at_estimates)) > 1e-6) {
      stop("Re-evaluating the model does not reproduce its gradient: ",
           "check that choice_data is the data the model was estimated on")
    }
    H <- matrix(NA_real_, K, K)
    for (k in seq_len(K)) {
      step <- replace(numeric(K), k, h)
      H[, k] <- (total_gradient(theta_hat + step, model) -
                   total_gradient(theta_hat - step, model)) / (2 * h)
      if (k %% 10 == 0) message("Hessian: column ", k, " of ", K)
    }
    H <- (H + t(H)) / 2
    if (any(eigen(H, symmetric = TRUE, only.values = TRUE)$values <= 0)) {
      stop("The numerical Hessian is not positive definite")
    }
    bread <- solve(H)
  }

  V <- G / (G - 1) * bread %*% meat %*% bread
  dimnames(V) <- list(names(theta_hat), names(theta_hat))
  inverse_hessian <- matrix(bread, nrow = K, ncol = K, dimnames = dimnames(V))
  attr(V, "inverse_hessian") <- inverse_hessian
  V
}

# 5. Average marginal effects at the reference vignette ------------------------
# Question: by how much does the probability that an expert recommends
# technology j change if one attribute of the reference vignette (every
# attribute at its first level) is set to another level?
#
#   AME_j = P_j(reference vignette with one attribute changed)
#           - P_j(reference vignette)
#
# P_j(x) is the probability for a randomly drawn expert of a profile (e.g. a
# Berufsgruppe): the logit probability averaged over the distribution of the
# random coefficients, beta_n = b + L z with z ~ N(0, I) (transformed for the
# other distributions), approximated with R = n_draws_ame draws z_1, ..., z_R.

#' Cholesky factor L of the covariance matrix of the random coefficients, so
#' that the covariance matrix is L %*% t(L) (step (b) of ame_at_reference() in
#' the policy brief script)
#'
#' Rows and columns follow mlogit's order of the random coefficients
#' (rpar_names); the lower-triangular form refers to this order.
#' Generalisation: elements fixed at 0 by constPar are not in coef(), so they
#' are added from fixed_parameters, but only to a theta that has the other
#' elements of L: the MNL of the same specification has none, and its L is 0.
#' For other distributions than the normal, L belongs to the normal draws
#' b + L z that coefficient_draws() transforms.
#'
#' @param theta            coefficient vector, named as coef() of an mlogit model
#' @param rpar_names       names of the random coefficients in mlogit's order
#' @param fixed_parameters elements of L fixed by constPar (NULL if none)
#' @return K x K matrix; all zero for a model without random coefficients
cholesky_factor <- function(theta, rpar_names, fixed_parameters = NULL) {
  if (any(startsWith(names(theta), "chol."))) {
    theta <- c(theta, fixed_parameters)
  }
  K <- length(rpar_names)
  L <- matrix(0, nrow = K, ncol = K, dimnames = list(rpar_names, rpar_names))
  if (any(startsWith(names(theta), "chol."))) {
    # Correlated model: mlogit names L[i, j] (i >= j) "chol.<ASC j>:<ASC i>"
    for (i in 1:K) {
      for (j in 1:i) {
        L[i, j] <- theta[paste0("chol.", rpar_names[j], ":", rpar_names[i])]
      }
    }
  } else if (any(startsWith(names(theta), "sd."))) {
    # Uncorrelated model: L is diagonal with the standard deviations
    for (i in 1:K) {
      L[i, i] <- theta[paste0("sd.", rpar_names[i])]
    }
  }
  # MNL: no random coefficients, L stays 0
  if (anyNA(L)) stop("Cholesky or standard deviation parameters missing in theta")
  L
}

#' Draws of one random coefficient from its distribution, computed as mlogit
#' computes them (make.beta() in mlogit 2.0.0), so that the marginal effects
#' rest on the distribution that was estimated
#'
#'   "n"   normal            b + eta
#'   "ln"  log-normal        exp(b + eta)
#'   "cn"  censored normal   max(0, b + eta)
#'   "u"   uniform           b - s + 2 s Phi(z), on [b - s, b + s]
#'   "t"   triangular        b + s w(Phi(z)), on [b - s, b + s], with
#'                           w(p) = sqrt(2 p) - 1 below p = 0.5 and
#'                           1 - sqrt(2 (1 - p)) above
#'
#' b is the coefficient's first parameter (named as the coefficient), s its
#' sd. parameter, eta = (L z)_k its normal deviation and z its standard normal
#' draw; Phi() is pnorm(), the standard normal distribution function. mlogit
#' transforms correlated coefficients only if they are log-normal or censored
#' normal, so check_specification() allows uniform and triangular ones only
#' without correlation, where eta = s z.
#'
#' @param distribution "n", "ln", "cn", "u" or "t"
#' @param b, s         the coefficient's two parameters
#' @param eta, z       vectors, one element per draw
#' @return vector of draws of the coefficient
coefficient_draws <- function(distribution, b, s, eta, z) {
  if (distribution == "n") {
    b + eta
  } else if (distribution == "ln") {
    exp(b + eta)
  } else if (distribution == "cn") {
    pmax(b + eta, 0)
  } else if (distribution == "u") {
    b - s + 2 * s * pnorm(z)
  } else if (distribution == "t") {
    p <- pnorm(z)
    b + s * ((p < 0.5) * (sqrt(2 * p) - 1) + (p > 0.5) * (1 - sqrt(2 * (1 - p))))
  } else {
    stop("Unknown distribution: ", distribution)
  }
}

#' Probabilities of the recommendations for each vignette in X, averaged over
#' the random coefficients, and the AMEs (each variation minus the reference
#' vignette). As ame_at_reference() in section 6.3 of the policy brief script.
#'
#'   P_j(x) = 1/R * sum_r exp(u_j(x, r)) / sum_k exp(u_k(x, r))
#'   u_j(x, r) = x' beta_j + sum_k x_k d_kj(r)   for the other outcomes
#'   u_0(x, r) = 0                               for the reference outcome
#'
#' d_kj(r) is the deviation of draw r of the random coefficient of regressor k
#' for technology j from its parameter in beta (0 if that coefficient is
#' fixed); for a normal coefficient (L z_r)_kj, as in the policy brief.
#' Generalisation: in the policy brief every random coefficient is a constant,
#' "(Intercept):<j>", whose regressor is 1 in every vignette, so its draw adds
#' to the utility of technology j as it is. Here a random coefficient
#' "<regressor>:<j>" adds its draw times the value of its regressor. With
#' group constants that value is the expert's group dummy, so the draws of the
#' other groups' constants are multiplied by 0. And a random coefficient may
#' follow another distribution than the normal (coefficient_draws()).
#'
#' @param theta               coefficient vector, named as coef() of an mlogit
#'                            model
#' @param X                   design matrix; its first row is the reference
#'                            vignette
#' @param standard_draws      standard normal draws, one row per draw, one
#'                            column per random coefficient
#' @param random_coefficients from random_coefficients(): names (in mlogit's
#'                            order) and distributions
#' @param outcomes            all outcomes, the reference outcome first
#' @param fixed_parameters    elements of L fixed by constPar (NULL if none)
#' @return list with
#'   probabilities  matrix, one row per vignette, one column per outcome
#'   ame            matrix, one row per variation, one column per outcome
#'   L              the Cholesky factor used (for the checks)
#'   deviations     d(r): one row per draw, one column per random coefficient
#'                  (for the checks)
ame_at_reference <- function(theta, X, standard_draws, random_coefficients, outcomes,
                             fixed_parameters = NULL) {
  technologies <- outcomes[-1]
  regressors <- colnames(X)
  rpar_names <- random_coefficients$name

  # (a) Coefficients beta: one row per regressor, one column per technology.
  #     mlogit names each coefficient "<regressor>:<technology>". For a
  #     random coefficient, theta holds its first parameter b (for a normal
  #     one, its mean).
  beta <- matrix(NA_real_, nrow = length(regressors), ncol = length(technologies),
                 dimnames = list(regressors, technologies))
  for (technology in technologies) {
    beta[, technology] <- theta[paste0(regressors, ":", technology)]
  }
  if (anyNA(beta)) stop("Coefficients missing in theta")

  # (b) Cholesky factor L of the covariance matrix of the random coefficients
  L <- cholesky_factor(theta, rpar_names, fixed_parameters)

  # (c) Deviation of the random coefficients from their means for each draw,
  #     L z_r. Row r of standard_draws is z_r', so row r of
  #     standard_draws %*% t(L) is (L z_r)'.
  coefficient_deviations <- standard_draws %*% t(L)
  colnames(coefficient_deviations) <- rpar_names

  #     Generalisation: a random coefficient that is not normal is a
  #     transformation of b + (L z_r) (coefficient_draws()); its deviation is
  #     the transformed draw minus b, the value it has in beta. Only the
  #     mixed logit has random coefficients: the MNL of the same
  #     specification has no sd. or chol. parameters, every coefficient is
  #     fixed at its estimate, and its deviations stay 0.
  is_mixed <- any(startsWith(names(theta), "sd.") | startsWith(names(theta), "chol."))
  if (is_mixed) {
    for (k in seq_along(rpar_names)) {
      distribution <- random_coefficients$distribution[k]
      if (distribution != "n") {
        if (distribution %in% c("u", "t") && any(L[k, -k] != 0)) {
          stop("A uniform or triangular coefficient is correlated: ", rpar_names[k])
        }
        b <- theta[[rpar_names[k]]]
        draws_k <- coefficient_draws(distribution, b, L[k, k],
                                     coefficient_deviations[, k], standard_draws[, k])
        coefficient_deviations[, k] <- draws_k - b
      }
    }
  }

  #     Generalisation: the regressor and the technology of each random
  #     coefficient, found by trying every pair of regressor and technology
  random_regressor <- character(0)
  random_technology <- character(0)
  for (regressor in regressors) {
    for (technology in technologies) {
      name <- paste0(regressor, ":", technology)
      if (name %in% rpar_names) {
        random_regressor[name] <- regressor
        random_technology[name] <- technology
      }
    }
  }
  if (!setequal(names(random_regressor), rpar_names)) {
    stop("Random coefficients that are not in the design matrix")
  }
  random_regressor <- random_regressor[rpar_names]
  random_technology <- random_technology[rpar_names]

  # (d) Probabilities of each vignette, averaged over the draws
  n_draws <- nrow(standard_draws)
  probabilities <- matrix(NA_real_, nrow = nrow(X), ncol = length(outcomes),
                          dimnames = list(rownames(X), outcomes))
  for (v in 1:nrow(X)) {
    # utility of each technology at the mean coefficients, x' beta: one number
    # per technology
    mean_utility <- as.vector(X[v, ] %*% beta)
    # Generalisation: loading[i, j] is what one unit of the deviation of
    # random coefficient i adds to the utility of technology j in this
    # vignette: the value of its regressor if it belongs to technology j,
    # otherwise 0. For the constants of the policy brief, loading has a single
    # 1 in every row and column, so that deviations %*% loading only puts the
    # columns in the order of the technologies.
    loading <- matrix(0, nrow = length(rpar_names), ncol = length(technologies),
                      dimnames = list(rpar_names, technologies))
    for (i in seq_along(rpar_names)) {
      loading[i, random_technology[[i]]] <- X[v, random_regressor[[i]]]
    }
    # utility for each draw: copy x' beta into every row and add that draw's
    # deviations (n_draws rows x one column per technology)
    utility <- matrix(mean_utility, nrow = n_draws, ncol = length(technologies),
                      byrow = TRUE) + coefficient_deviations %*% loading
    # the reference outcome has utility 0: first column (n_draws rows x one
    # column per outcome)
    utility <- cbind(0, utility)
    # logit probabilities for each draw; each row sums to one. Subtracting each
    # row's largest utility before exp() avoids overflow; it cancels in the ratio.
    # Each row's largest utility: the elementwise maximum over the columns
    # (same result as apply(utility, 1, max), but much faster)
    row_max <- utility[, 1]
    for (k in 2:ncol(utility)) {
      row_max <- pmax(row_max, utility[, k])
    }
    exp_utility <- exp(utility - row_max)
    prob_draws <- exp_utility / rowSums(exp_utility)
    # average over the draws, i.e. over experts
    probabilities[v, ] <- colMeans(prob_draws)
  }

  # (e) AMEs: probabilities of each variation minus those of the reference
  #     vignette. reference_probabilities repeats row 1 once per variation.
  reference_probabilities <- matrix(probabilities[1, ], nrow = nrow(X) - 1,
                                    ncol = ncol(probabilities), byrow = TRUE)
  ame <- probabilities[-1, , drop = FALSE] - reference_probabilities

  list(probabilities = probabilities, ame = ame, L = L,
       deviations = coefficient_deviations)
}

#' The reference vignette and its variations (section 6.1 of the policy brief
#' script). Reference vignette: every attribute at its first level.
#' Variations: the reference vignette with exactly one attribute changed, one
#' row per non-reference level ([-1]: all levels but the first).
#'
#' @param attribute_levels levels of the attributes to vary
#' @return tibble, one row per vignette: the attributes as factors and a label
ame_vignettes <- function(attribute_levels) {
  # tibble(.rows = 1) is an empty table with one row; the loop adds one column
  # per attribute
  reference_vignette <- tibble(.rows = 1)
  for (attribute in names(attribute_levels)) {
    reference_vignette[[attribute]] <- factor(attribute_levels[[attribute]][1],
                                              levels = attribute_levels[[attribute]])
  }
  reference_vignette$label <- "Reference vignette"

  vignettes <- reference_vignette
  for (attribute in names(attribute_levels)) {
    for (level in attribute_levels[[attribute]][-1]) {
      variation <- reference_vignette
      variation[[attribute]] <- factor(level, levels = attribute_levels[[attribute]])
      variation$label <- paste0(attribute, ": ", level)
      vignettes <- bind_rows(vignettes, variation)
    }
  }
  vignettes
}

#' Design matrices for the AMEs of one specification, one per expert profile
#'
#' The vignettes of ame_vignettes(), for the attributes the specification
#' uses, evaluated for an expert with the characteristics in one row of
#' `profiles` (e.g. a Berufsgruppe). An interaction of an attribute with an
#' expert characteristic is evaluated at the profile's value. A specification
#' without expert characteristics gets a single profile, "All experts".
#'
#' @param formula_terms    from estimate_specification(): the terms with the
#'                         levels of the estimation data
#' @param column_names     the columns of the estimation's design matrix; the
#'                         design matrices must have exactly these
#' @param attribute_levels levels of all vignette attributes
#' @param profiles         tibble, one row per profile: its name in `profile`
#'                         and one column per expert characteristic, with the
#'                         same factor levels as in the data
#' @return named list of design matrices (names: the profiles); rows named by
#'   the vignette labels, the reference vignette first
ame_design_matrices <- function(formula_terms, column_names, attribute_levels,
                                profiles) {
  # The variables of the terms: all.vars() lists the variables of a formula,
  # also those inside the I() expressions of the interactions
  is_constant <- formula_terms$formula_term %in% c("0", "1")
  model_terms <- formula_terms$formula_term[!is_constant]
  variables <- all.vars(as.formula(paste("~", paste(c("1", model_terms),
                                                     collapse = " + "))))
  attributes <- intersect(names(attribute_levels), variables)
  vignettes <- ame_vignettes(attribute_levels[attributes])

  expert_variables <- setdiff(variables, attributes)
  if (length(expert_variables) == 0) {
    profiles <- tibble(profile = "All experts")
  }
  missing_columns <- setdiff(expert_variables, names(profiles))
  if (length(missing_columns) > 0) {
    stop("AME_PROFILES has no column ", paste(missing_columns, collapse = ", "),
         ": add the values at which the marginal effects are evaluated")
  }

  X_by_profile <- list()
  for (p in seq_len(nrow(profiles))) {
    profile_vignettes <- vignettes
    for (variable in expert_variables) {
      # the profile's value, the same in every vignette
      profile_vignettes[[variable]] <- profiles[[variable]][p]
    }
    X <- design_matrix(formula_terms, profile_vignettes)
    if (!identical(colnames(X), column_names)) {
      stop("The design matrix of profile ", profiles$profile[p], " has other ",
           "columns than the estimation's: check the factor levels in ",
           "AME_PROFILES")
    }
    rownames(X) <- vignettes$label
    X_by_profile[[profiles$profile[p]]] <- X
  }
  X_by_profile
}

# 6. Standard errors, confidence intervals and p-values ------------------------
# As in section 7 of the policy brief script, all inference rests on the
# large-sample approximation theta_hat ~ N(theta, V), with the cluster-robust
# covariance matrix V of section 4: z = estimate / se, p = 2 * P(Z > |z|),
# 95% CI = estimate +- 1.96 * se.

#' The pairs of random coefficients whose correlation is estimated: those in
#' the same correlation block, each pair once, j before i in mlogit's order
#'
#' @param random_coefficients from random_coefficients()
#' @return tibble: parameter ("cor.<j>:<i>"), coefficient (j), coefficient_2 (i)
correlation_pairs <- function(random_coefficients) {
  rpar_names <- random_coefficients$name
  block <- random_coefficients$block
  pairs <- tibble(parameter = character(0), coefficient = character(0),
                  coefficient_2 = character(0))
  for (i in seq_along(rpar_names)) {
    for (j in seq_len(i - 1)) {
      if (!is.na(block[i]) && block[i] == block[j]) {
        pairs <- bind_rows(pairs, tibble(
          parameter = paste0("cor.", rpar_names[j], ":", rpar_names[i]),
          coefficient = rpar_names[j],
          coefficient_2 = rpar_names[i]
        ))
      }
    }
  }
  pairs
}

#' Standard deviations and correlations of the random coefficients implied by
#' a coefficient vector (omega_parameters() in section 7.1 of the policy brief
#' script)
#'
#'   Omega = L L',   sd_j = sqrt(Omega_jj),   cor_jk = Omega_jk / (sd_j sd_k)
#'
#' Each correlation once. Generalisation: only the correlations within a
#' correlation block (correlation_pairs()); those across blocks are 0 by
#' construction, not estimated. For a log-normal or censored normal
#' coefficient, these are the standard deviations and correlations of the
#' normal b + L z before the transformation.
#'
#' @param random_coefficients from random_coefficients()
#' @return named vector: "sd.<rpar>" for each random coefficient, then
#'   "cor.<rpar j>:<rpar i>" for each pair of correlation_pairs()
omega_parameters <- function(theta, random_coefficients, fixed_parameters = NULL) {
  rpar_names <- random_coefficients$name
  L <- cholesky_factor(theta, rpar_names, fixed_parameters)
  Omega <- L %*% t(L)
  sds <- sqrt(diag(Omega))
  correlations <- Omega / outer(sds, sds)   # element [i, j]: Omega_ij / (sd_i sd_j)
  result <- setNames(sds, paste0("sd.", rpar_names))
  pairs <- correlation_pairs(random_coefficients)
  for (k in seq_len(nrow(pairs))) {
    result[pairs$parameter[k]] <- correlations[pairs$coefficient_2[k],
                                               pairs$coefficient[k]]
  }
  result
}

#' Coefficient table of one model (section 7.1 of the policy brief script)
#'
#' Rows:
#'   <column>:<technology>  fixed coefficients and the first parameter b of
#'                          the random coefficients (for a normal one, its
#'                          mean): estimate and square root of the diagonal
#'                          of V
#'   sd.<rpar>              standard deviations of the random coefficients
#'   cor.<rpar>:<rpar>      their correlations (correlated models only)
#'
#' Standard deviations. Uncorrelated model: mlogit estimates s in
#' beta = b + s z; only |s| is identified, so the table shows |s| with the
#' standard error of s (the sign flip does not change it). Correlated model:
#' mlogit estimates the Cholesky factor L. A single entry of L has no
#' substantive meaning, so the table shows what L implies (see
#' omega_parameters()), with delta-method standard errors: se = sqrt(diag(J V
#' J')), where J is the derivative of (sd, cor) with respect to theta, computed
#' by numDeriv::jacobian(). Generalisation: for the other distributions, sd. is
#' s, the parameter of coefficient_draws() (for a uniform or triangular
#' coefficient the half-width of its range).
#'
#' Tests and intervals. z = estimate / se and p = 2 * P(Z > |z|) for all rows
#' but the standard deviations: their test of sd = 0 is not valid in the usual
#' form, because sd = 0 lies on the boundary of the parameter space (z and p are
#' NA). The 95% intervals are estimate +- 1.96 se, cut at 0 for standard
#' deviations and at -1 and 1 for correlations.
#'
#' Generalisation: the columns type, coefficient and coefficient_2 say what
#' each row is, for the labels of the output.
#'
#' @param model      fitted mlogit model
#' @param V          its covariance matrix from vcov_cluster_robust()
#' @param model_name value of the column `model`
#' @param random_coefficients, fixed_parameters from estimate_specification()
#' @return tibble: model, parameter, type ("coefficient", "sd" or "cor"),
#'   coefficient (the coefficient of the row; for a correlation the first),
#'   coefficient_2 (the second of a correlation), estimate, se, z, p, ci_low,
#'   ci_high
coefficient_table <- function(model, V, model_name, random_coefficients,
                              fixed_parameters = NULL) {
  z_critical <- qnorm(0.975)   # 1.96
  theta_hat <- coef(model)
  V <- V[names(theta_hat), names(theta_hat)]
  se <- sqrt(diag(V))
  rpar_names <- random_coefficients$name

  # (a) Fixed coefficients and first parameters of the random coefficients:
  #     every parameter but sd. and chol.
  is_mean <- !startsWith(names(theta_hat), "sd.") &
    !startsWith(names(theta_hat), "chol.")
  coefficients_model <- tibble(
    model = model_name,
    parameter = names(theta_hat)[is_mean],
    type = "coefficient",
    coefficient = names(theta_hat)[is_mean],
    coefficient_2 = NA_character_,
    estimate = as.numeric(theta_hat[is_mean]),
    se = as.numeric(se[is_mean])
  )

  # (b) Uncorrelated model: |s| with the standard error of s
  if (any(startsWith(names(theta_hat), "sd."))) {
    sd_names <- paste0("sd.", rpar_names)
    coefficients_model <- bind_rows(coefficients_model, tibble(
      model = model_name,
      parameter = sd_names,
      type = "sd",
      coefficient = rpar_names,
      coefficient_2 = NA_character_,
      estimate = abs(as.numeric(theta_hat[sd_names])),
      se = as.numeric(se[sd_names])
    ))
  }

  # (c) Correlated model: standard deviations and correlations implied by L.
  #     jacobian() passes the arguments after theta_hat on to omega_parameters().
  if (any(startsWith(names(theta_hat), "chol."))) {
    omega_estimate <- omega_parameters(theta_hat, random_coefficients,
                                       fixed_parameters)
    # one row per sd or cor
    J <- numDeriv::jacobian(omega_parameters, theta_hat,
                            random_coefficients = random_coefficients,
                            fixed_parameters = fixed_parameters)
    omega_se <- sqrt(diag(J %*% V %*% t(J)))

    # Check: the standard deviations agree with mlogit's covariance matrix
    sd_mlogit <- sqrt(diag(unclass(cov.mlogit(model))))[rpar_names]
    stopifnot(max(abs(omega_estimate[paste0("sd.", rpar_names)] - sd_mlogit)) < 1e-8)

    # What each row is: the standard deviations, then the pairs of the
    # correlations, in the order of omega_parameters()
    omega_rows <- bind_rows(
      tibble(parameter = paste0("sd.", rpar_names), type = "sd",
             coefficient = rpar_names, coefficient_2 = NA_character_),
      correlation_pairs(random_coefficients) %>%
        mutate(type = "cor")
    )
    stopifnot(identical(omega_rows$parameter, names(omega_estimate)))

    coefficients_model <- bind_rows(coefficients_model, omega_rows %>%
                                      mutate(model = model_name,
                                             estimate = as.numeric(omega_estimate),
                                             se = as.numeric(omega_se)))
  }

  # (d) Tests and intervals
  coefficients_model %>%
    mutate(is_sd = type == "sd",
           is_cor = type == "cor",
           z = if_else(is_sd, NA_real_, estimate / se),
           p = 2 * pnorm(-abs(z)),
           ci_low = estimate - z_critical * se,
           ci_high = estimate + z_critical * se,
           ci_low = case_when(is_sd ~ pmax(0, ci_low),
                              is_cor ~ pmax(-1, ci_low),
                              TRUE ~ ci_low),
           ci_high = if_else(is_cor, pmin(1, ci_high), ci_high)) %>%
    select(model, parameter, type, coefficient, coefficient_2, estimate, se, z, p,
           ci_low, ci_high)
}

#' AMEs of one model at the reference vignette of each profile, with
#' Krinsky-Robb standard errors (sections 6.4, 6.6 and 7.2 of the policy brief
#' script)
#'
#' The AMEs are nonlinear functions of theta, g(theta) =
#' ame_at_reference(theta)$ame. Krinsky-Robb passes the uncertainty in
#' theta_hat through this function:
#'   1. draw theta_1, ..., theta_S from N(theta_hat, V), the same distribution
#'      that underlies the coefficient standard errors
#'   2. compute the AMEs at every draw, with the same standard_draws z_r as the
#'      point estimates, so that the spread reflects only the uncertainty in
#'      theta and not new simulation noise
#'   3. the standard deviation of the S AMEs is the standard error; their 2.5%
#'      and 97.5% quantiles are a percentile confidence interval
#' The point estimate stays the AME at theta_hat, not the mean of the draws.
#' Generalisation: one set of theta draws for all profiles. Set the seed before
#' calling the function.
#'
#' ci_low, ci_high: estimate +- 1.96 se, consistent with z and p.
#' ci_low_kr, ci_high_kr: percentile interval of the draws. If the two
#' intervals differ markedly, the distribution of the AME is skewed and the
#' percentile interval is the better one to report.
#'
#' @param model_name   value of the column `model`
#' @param theta_hat    coef() of the model
#' @param V            its covariance matrix
#' @param X_by_profile design matrices from ame_design_matrices()
#' @param n_draws_kr   number of draws of theta
#' @param standard_draws, random_coefficients, outcomes, fixed_parameters as in
#'   ame_at_reference()
#' @return tibble, one row per profile x variation x outcome: model, profile,
#'   vignette, outcome, p_reference, se_p_reference, ame, se, z, p, ci_low,
#'   ci_high, ci_low_kr, ci_high_kr
ame_table <- function(model_name, theta_hat, V, X_by_profile, standard_draws,
                      random_coefficients, outcomes, fixed_parameters, n_draws_kr) {
  z_critical <- qnorm(0.975)   # 1.96

  # (a) Draws of the coefficient vector: one row per draw, columns named as
  #     theta_hat
  theta_draws <- MASS::mvrnorm(n_draws_kr, mu = theta_hat, Sigma = V)

  result <- tibble()
  for (profile in names(X_by_profile)) {
    X <- X_by_profile[[profile]]

    # (b) Point estimates at theta_hat
    estimate <- ame_at_reference(theta_hat, X, standard_draws, random_coefficients,
                                 outcomes, fixed_parameters)

    # (c) AMEs and probabilities at the reference vignette for every draw.
    #     ame_draws[, , s] is the AME matrix (variations x outcomes) of draw s.
    ame_draws <- array(NA_real_,
                       dim = c(nrow(X) - 1, length(outcomes), n_draws_kr),
                       dimnames = list(rownames(X)[-1], outcomes, NULL))
    p_reference_draws <- matrix(NA_real_, nrow = n_draws_kr, ncol = length(outcomes),
                                dimnames = list(NULL, outcomes))
    for (s in 1:n_draws_kr) {
      result_s <- ame_at_reference(theta_draws[s, ], X, standard_draws,
                                   random_coefficients, outcomes, fixed_parameters)
      ame_draws[, , s] <- result_s$ame
      p_reference_draws[s, ] <- result_s$probabilities[1, ]
      if (s %% 100 == 0) {
        message(model_name, ", ", profile, ": draw ", s, " of ", n_draws_kr)
      }
    }

    # (d) Summaries over the draws. apply(ame_draws, c(1, 2), f) applies f to
    #     the n_draws_kr values of each variation x outcome cell.
    ame_se <- apply(ame_draws, c(1, 2), sd)
    ame_low <- apply(ame_draws, c(1, 2), quantile, probs = 0.025)
    ame_high <- apply(ame_draws, c(1, 2), quantile, probs = 0.975)
    p_reference_se <- apply(p_reference_draws, 2, sd)
    reference_probabilities <- estimate$probabilities[1, ]

    # (e) Long format, one row per variation x outcome
    profile_table <- as_tibble(estimate$ame, rownames = "vignette") %>%
      pivot_longer(-vignette, names_to = "outcome", values_to = "ame") %>%
      left_join(as_tibble(ame_se, rownames = "vignette") %>%
                  pivot_longer(-vignette, names_to = "outcome", values_to = "se"),
                by = c("vignette", "outcome")) %>%
      left_join(as_tibble(ame_low, rownames = "vignette") %>%
                  pivot_longer(-vignette, names_to = "outcome",
                               values_to = "ci_low_kr"),
                by = c("vignette", "outcome")) %>%
      left_join(as_tibble(ame_high, rownames = "vignette") %>%
                  pivot_longer(-vignette, names_to = "outcome",
                               values_to = "ci_high_kr"),
                by = c("vignette", "outcome")) %>%
      mutate(model = model_name,
             profile = profile,
             p_reference = reference_probabilities[outcome],
             se_p_reference = p_reference_se[outcome])
    result <- bind_rows(result, profile_table)
  }

  # (f) z, p and the intervals estimate +- 1.96 se
  result %>%
    mutate(z = ame / se,
           p = 2 * pnorm(-abs(z)),
           ci_low = ame - z_critical * se,
           ci_high = ame + z_critical * se) %>%
    select(model, profile, vignette, outcome, p_reference, se_p_reference,
           ame, se, z, p, ci_low, ci_high, ci_low_kr, ci_high_kr)
}

#' Delta-method standard errors of the AMEs at one profile, a check on the
#' Krinsky-Robb standard errors (section 7.3 of the policy brief script)
#'
#' The delta method linearises the AMEs around theta_hat,
#'   Var(AME) = G V G',   G = derivative of the AMEs with respect to theta,
#' with one row of G per AME; numDeriv::jacobian() computes G numerically. If
#' the AMEs are close to linear in theta over the plausible range of theta,
#' the delta-method and Krinsky-Robb standard errors agree up to the
#' simulation noise of the Krinsky-Robb draws.
#'
#' @return tibble, one row per variation x outcome: vignette, outcome, se_delta
ame_delta_method_se <- function(theta_hat, V, X, standard_draws,
                                random_coefficients, outcomes,
                                fixed_parameters) {
  # jacobian() needs a function of theta that returns a vector: here the AME
  # matrix stacked column by column (all variations of outcome 1, then
  # outcome 2, ...)
  G <- numDeriv::jacobian(function(theta) {
    as.vector(ame_at_reference(theta, X, standard_draws, random_coefficients,
                               outcomes, fixed_parameters)$ame)
  }, theta_hat)
  # back to a matrix with the same layout as the AME matrix
  se_delta <- matrix(sqrt(diag(G %*% V %*% t(G))), nrow = nrow(X) - 1,
                     dimnames = list(rownames(X)[-1], outcomes))
  as_tibble(se_delta, rownames = "vignette") %>%
    pivot_longer(-vignette, names_to = "outcome", values_to = "se_delta")
}

# 7. Fit statistics and likelihood-ratio tests ---------------------------------

#' Fit statistics of the models of one specification (section 8.2 of the
#' policy brief script)
#'
#' McFadden R^2 and the likelihood-ratio (LR) test compare a model with a model
#' that has constants only, i.e. that predicts the observed share of each
#' recommendation for every vignette. Its log-likelihood follows from the
#' choice counts n_j alone:
#'   logL_0 = sum_j n_j log(n_j / N)
#'   R^2 = 1 - logL / logL_0,   LR = 2 (logL - logL_0),
#'   df = parameters - (outcomes - 1).
#' For the mixed logit a second LR test compares it with the MNL: do the
#' random coefficients vary across experts? A variance of zero lies on the
#' boundary of the parameter space, so the chi-squared p-value of this test is
#' conservative (too large).
#'
#' Addition to the policy brief: AIC = -2 logL + 2 parameters and BIC =
#' -2 logL + log(experts) parameters, to compare specifications that are not
#' nested. BIC counts experts, not choice situations, because the panel
#' likelihood is a product over experts.
#'
#' Generalisation: the policy brief checks R^2 against mlogit's summary(),
#' which reports it only for models with a common constant. Here logL_0 is
#' checked against mlogit's own, logLik(model, type = "null").
#'
#' @param models        list of the fitted models (mnl and mixed)
#' @param choice_counts table of the recommendations in the estimation sample
#' @param n_experts     number of experts in the estimation sample
#' @param n_draws_model Halton draws per expert of the mixed logit
#' @return tibble, one row per model
fit_statistics <- function(models, choice_counts, n_experts, n_draws_model) {
  n_choices <- sum(choice_counts)
  chosen_counts <- choice_counts[choice_counts > 0]   # outcomes never chosen add 0
  log_likelihood_constants_only <- sum(chosen_counts * log(chosen_counts / n_choices))

  fit <- tibble()
  for (model_name in names(models)) {
    # (not called 'model': inside tibble() that name would refer to the column)
    fitted_model <- models[[model_name]]
    log_likelihood <- as.numeric(logLik(fitted_model))
    n_parameters <- length(coef(fitted_model))
    is_mixed <- !is.null(fitted_model$rpar)

    # Comparison with the constants-only model
    if (abs(log_likelihood_constants_only -
            as.numeric(logLik(fitted_model, type = "null"))) > 1e-8) {
      stop("Log-likelihood of the constants-only model differs from mlogit's for ",
           model_name)
    }
    mcfadden_r2 <- 1 - log_likelihood / log_likelihood_constants_only
    lr_constants_only <- 2 * (log_likelihood - log_likelihood_constants_only)
    lr_constants_only_df <- n_parameters - (length(choice_counts) - 1)
    lr_constants_only_p <- pchisq(lr_constants_only, df = lr_constants_only_df,
                                  lower.tail = FALSE)

    # Comparison with the MNL (mixed logit only)
    if (is_mixed) {
      lr_mnl <- 2 * (log_likelihood - as.numeric(logLik(models$mnl)))
      lr_mnl_df <- n_parameters - length(coef(models$mnl))
      lr_mnl_p <- pchisq(lr_mnl, df = lr_mnl_df, lower.tail = FALSE)
    } else {
      lr_mnl <- NA_real_
      lr_mnl_df <- NA_real_
      lr_mnl_p <- NA_real_
    }

    fit <- bind_rows(fit, tibble(
      model = model_name,
      n_choices = n_choices,
      n_experts = n_experts,
      n_parameters = n_parameters,
      n_draws_per_expert = if (is_mixed) n_draws_model else NA_real_,
      log_likelihood = log_likelihood,
      log_likelihood_constants_only = log_likelihood_constants_only,
      mcfadden_r2 = mcfadden_r2,
      lr_constants_only = lr_constants_only,
      lr_constants_only_df = lr_constants_only_df,
      lr_constants_only_p = lr_constants_only_p,
      lr_mnl = lr_mnl,
      lr_mnl_df = lr_mnl_df,
      lr_mnl_p = lr_mnl_p,
      aic = -2 * log_likelihood + 2 * n_parameters,
      bic = -2 * log_likelihood + log(n_experts) * n_parameters
    ))
  }
  # tibble() silently returns zero rows if one of its values has length 0, so
  # check that every model has its row
  stopifnot(nrow(fit) == length(models))
  fit
}

#' Restrictions of a likelihood-ratio test between two nested specifications,
#' as functions of the coefficients theta of the unrestricted mixed logit:
#' every element is 0 when theta satisfies the restricted specification. The
#' robust LR test (lr_tests()) needs their derivative with respect to theta.
#'
#' The types (a test can combine several with " + "):
#'   regressors         the coefficients of the columns of the design matrix
#'                      that the restricted specification does not have (e.g.
#'                      the Berufsgruppe, or its interactions)
#'   correlations       the elements of the Cholesky factor L off its diagonal:
#'                      all 0 exactly when the random coefficients are
#'                      uncorrelated
#'   group_covariances  (group constants) each group's covariance matrix of
#'                      the constants minus the first group's: the variances,
#'                      and with correlated constants also the covariances
#'
#' @param theta        coefficients of the unrestricted mixed logit, coef()
#' @param type         one of the three types
#' @param unrestricted saved results of the unrestricted specification
#' @param restricted   saved results of the restricted specification
#' @return numeric vector, one element per restriction
lr_restrictions <- function(theta, type, unrestricted, restricted) {
  technologies <- unrestricted$technologies
  random <- unrestricted$random_coefficients

  if (type == "regressors") {
    dropped_columns <- setdiff(unrestricted$column_names,
                               restricted$column_names)
    if (length(dropped_columns) == 0) {
      stop("Restriction regressors: no regressor to drop")
    }
    parameters <- as.vector(outer(dropped_columns, technologies, paste,
                                  sep = ":"))
    restrictions <- theta[parameters]

  } else if (type == "correlations") {
    # mlogit names L[i, j] (i > j, off the diagonal) "chol.<j>:<i>"; those
    # fixed at 0 by constPar are not in theta and are no restriction
    rpar_names <- random$name
    off_diagonal <- character(0)
    for (i in seq_along(rpar_names)) {
      for (j in seq_len(i - 1)) {
        off_diagonal <- c(off_diagonal,
                          paste0("chol.", rpar_names[j], ":", rpar_names[i]))
      }
    }
    parameters <- off_diagonal[off_diagonal %in% names(theta)]
    if (length(parameters) == 0) {
      stop("Restriction correlations: the unrestricted specification has no ",
           "correlations")
    }
    restrictions <- theta[parameters]

  } else if (type == "group_covariances") {
    groups <- unique(random$group[!is.na(random$group)])
    if (length(groups) < 2) {
      stop("Restriction group_covariances needs random group constants in the ",
           "unrestricted specification")
    }
    L <- cholesky_factor(theta, random$name, unrestricted$fixed_parameters)
    Omega <- L %*% t(L)
    first_group <- paste0(groups[1], ":", technologies)
    restrictions <- numeric(0)
    for (group in groups[-1]) {
      members <- paste0(group, ":", technologies)
      difference <- Omega[members, members] - Omega[first_group, first_group]
      if (identical(unrestricted$specification$correlation, "none")) {
        restrictions <- c(restrictions, diag(difference))
      } else {
        # each variance and covariance once: the lower triangle with the
        # diagonal
        restrictions <- c(restrictions,
                          difference[lower.tri(difference, diag = TRUE)])
      }
    }

  } else {
    stop("Unknown restriction type: ", type)
  }

  if (anyNA(restrictions)) {
    stop("Restriction ", type, ": coefficients missing in theta")
  }
  as.numeric(restrictions)
}

#' The draws an expert gets in a specification, as text. Two simulated
#' log-likelihoods can be compared only if every expert gets the same draws.
#' Each random coefficient has its Halton prime (halton_primes()), and an
#' expert uses the random coefficients of their own group's constants and
#' those that every expert has. The text lists their primes if they are the
#' same for every group, as with one set of constants or with
#' halton_by_group; with group constants and draws = "halton", they depend on
#' the group.
expert_draws <- function(results) {
  random <- results$random_coefficients
  groups <- unique(random$group[!is.na(random$group)])
  if (length(groups) == 0) {
    primes_by_group <- paste(random$prime, collapse = ", ")
  } else {
    primes_by_group <- character(0)
    for (group in groups) {
      is_used <- is.na(random$group) | random$group == group
      primes_by_group[group] <- paste(random$prime[is_used], collapse = ", ")
    }
  }
  if (length(unique(primes_by_group)) == 1) {
    primes <- paste("the Halton primes", primes_by_group[1], "for every expert")
  } else {
    primes <- "Halton primes that depend on the group"
  }
  paste0(results$draws$n_draws_model, " draws per expert, ", primes)
}

#' Likelihood-ratio tests between nested specifications (mixed logits),
#' classical and robust
#'
#' LR = 2 (logL_unrestricted - logL_restricted), with df = the number of
#' parameters the unrestricted model has more. The test is valid only if the
#' restricted specification is a special case of the unrestricted one, on the
#' same sample and with the same draws per expert (expert_draws()): both are
#' checked here, and the restrictions must account for exactly df parameters.
#'
#' Classical p-value: LR against the chi-squared distribution with df degrees
#' of freedom. That holds only if the unrestricted model is correctly
#' specified, which the cluster-robust standard errors do not assume.
#'
#' Robust p-value (Kent 1982; White 1982, section 6): under the restrictions,
#' whether or not the model is correctly specified, LR is approximately
#' distributed as
#'
#'   sum_i lambda_i Z_i^2,   Z_1, ..., Z_df independent standard normal,
#'
#' where lambda_1, ..., lambda_df are the eigenvalues of
#'
#'   (R H^{-1} R')^{-1} (R V R').
#'
#' H^{-1} is the inverse Hessian of the unrestricted model (the bread of its
#' sandwich), V its cluster-robust covariance matrix (with G/(G-1), as in the
#' tables), and R the derivative of the restrictions (lr_restrictions()) with
#' respect to its coefficients, at the estimates. If the model is correctly
#' specified, V = H^{-1} in large samples, every lambda_i is 1, and the two
#' tests agree. With a single restriction, lambda is the squared ratio of the
#' robust to the classical standard error. mean_weight, the mean of the
#' lambda_i, is a rough guide to the direction: above 1, the classical test
#' tends to reject too often.
#'
#' Small samples: parameters that only one group identifies, such as a small
#' Berufsgruppe's (co)variances or its interactions, have a cluster-robust
#' variance computed from that group's experts alone, which is biased downward
#' when they are few. The weights then fall below 1 although the model is
#' correctly specified, and the robust test rejects too often. (On simulated
#' data from a correctly specified model, the cluster-robust variances of the
#' covariance parameters of a group of about 60 experts were about 0.6 times
#' the Hessian-based ones, and the mean weight of the test of their
#' correlations was 0.7.)
#'
#' The robust p-value is simulated with n_draws draws; with 1,000,000 its
#' standard error is at most 0.0005. A p-value of 0 means below 1 / n_draws.
#'
#' Both models use the same draws per expert, so the unrestricted model's
#' simulated log-likelihood cannot be smaller at its maximum: a negative LR
#' means the unrestricted model has not reached its maximum. Its p-values are
#' then NA.
#'
#' @param results_list named list of saved results
#' @param pairs        tibble: restricted, unrestricted (names of
#'                     specifications), restrictions (types of
#'                     lr_restrictions(), several joined by " + ")
#' @param n_draws      draws for the robust p-value
#' @param seed         set before the draws of each test, so that a test's
#'                     p-value does not depend on the tests before it
#' @return tibble, one row per pair: restricted, unrestricted, restrictions,
#'   lr, df, p, p_robust, mean_weight
lr_tests <- function(results_list, pairs, n_draws = 1e6, seed = 20261004) {
  # the columns from the start, so that the result has them even without a test
  result <- tibble(restricted = character(0), unrestricted = character(0),
                   restrictions = character(0), lr = numeric(0), df = numeric(0),
                   p = numeric(0), p_robust = numeric(0), mean_weight = numeric(0))
  for (i in seq_len(nrow(pairs))) {
    restricted_name <- pairs$restricted[i]
    unrestricted_name <- pairs$unrestricted[i]
    if (!(restricted_name %in% names(results_list)) ||
        !(unrestricted_name %in% names(results_list))) {
      message("LR test skipped, results missing: ", restricted_name,
              " against ", unrestricted_name)
      next
    }
    restricted <- results_list[[restricted_name]]
    unrestricted <- results_list[[unrestricted_name]]

    # (a) The LR statistic: both models on the same sample, with the same
    #     draws per expert
    fit_restricted <- restricted$fit_statistics %>%
      filter(model == "mixed")
    fit_unrestricted <- unrestricted$fit_statistics %>%
      filter(model == "mixed")
    if (fit_restricted$n_choices != fit_unrestricted$n_choices ||
        fit_restricted$n_experts != fit_unrestricted$n_experts) {
      stop("Different samples: ", restricted_name, " and ", unrestricted_name)
    }
    if (expert_draws(restricted) != expert_draws(unrestricted)) {
      stop("Different draws: ", restricted_name, " (", expert_draws(restricted),
           ") and ", unrestricted_name, " (", expert_draws(unrestricted), ")")
    }
    lr <- 2 * (fit_unrestricted$log_likelihood - fit_restricted$log_likelihood)
    df <- fit_unrestricted$n_parameters - fit_restricted$n_parameters

    # (b) R: one row per restriction, one column per coefficient of the
    #     unrestricted model. jacobian() passes the arguments after theta on
    #     to lr_restrictions(). The restrictions must be df distinct ones:
    #     qr()$rank counts the linearly independent rows.
    theta <- unrestricted$coefficients$mixed
    # strsplit(): the types, e.g. "regressors + correlations" -> both
    types <- strsplit(pairs$restrictions[i], " + ", fixed = TRUE)[[1]]
    R <- NULL
    for (type in types) {
      R <- rbind(R, numDeriv::jacobian(lr_restrictions, theta, type = type,
                                       unrestricted = unrestricted,
                                       restricted = restricted))
    }
    if (nrow(R) != df || qr(R)$rank != df) {
      stop("LR test ", restricted_name, " against ", unrestricted_name, ": the ",
           "restrictions (", pairs$restrictions[i], ") give ", qr(R)$rank,
           " distinct restrictions in ", nrow(R), " rows, but the two ",
           "specifications differ by ", df, " parameters")
    }

    # (c) The weights lambda: eigenvalues of (R H^{-1} R')^{-1} (R V R'). They
    #     equal those of the symmetric matrix C'^{-1} (R V R') C^{-1}, where
    #     C is the Cholesky factor of R H^{-1} R' (chol(): C'C = R H^{-1} R'),
    #     for which eigen() returns real values.
    inverse_hessian <- unrestricted$inverse_hessians$mixed
    if (is.null(inverse_hessian)) {
      stop("No inverse Hessian in the results of ", unrestricted_name,
           ": rerun 01_run_specifications.R")
    }
    inverse_hessian <- inverse_hessian[names(theta), names(theta)]
    V <- unrestricted$covariance_matrices$mixed[names(theta), names(theta)]
    C <- chol(R %*% inverse_hessian %*% t(R))
    C_inverse <- solve(C)
    weights_matrix <- t(C_inverse) %*% (R %*% V %*% t(R)) %*% C_inverse
    weights <- eigen(weights_matrix, symmetric = TRUE, only.values = TRUE)$values

    # (d) p-values: classical from the chi-squared distribution; robust as the
    #     share of n_draws draws of sum_i lambda_i Z_i^2 (rchisq(): draws of
    #     Z^2) that are at least as large as LR
    if (lr < 0) {
      message("Negative LR statistic: ", unrestricted_name,
              " has a lower log-likelihood than ", restricted_name)
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

    result <- bind_rows(result, tibble(
      restricted = restricted_name,
      unrestricted = unrestricted_name,
      restrictions = pairs$restrictions[i],
      lr = lr,
      df = df,
      p = p_classical,
      p_robust = p_robust,
      mean_weight = mean(weights)
    ))
  }
  result
}
