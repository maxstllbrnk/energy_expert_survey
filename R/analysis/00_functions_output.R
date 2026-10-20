################################################################################
# 00_functions_output.R — tables, figures and Excel sheets of the mixed logit
#                         specifications
# Inputs : none; sourced by 02_output.R
# Outputs: none
# Checks : every value in a table has a label; every parameter a table needs
#          is in the results
################################################################################
# The functions generalise section 8 of
# R/first_policy_brief/03_rc_logit_with_ame.R and the heat map of
# R/first_policy_brief/04_heatmap_rc_logit.R to the specifications of
# 01_run_specifications.R. They take the saved results of one or several
# specifications (the .rds files in results/) and a list of labels, which
# 02_output.R defines:
#   models              model -> label ("mnl", "mixed")
#   technologies        outcome -> label
#   technology_headers  outcome -> table column header (LaTeX, \\ breaks)
#   terms               column of the design matrix of a variable -> label;
#                       a column of an interaction is labelled by its parts
#                       (column_labels())
#   variations          variation of the reference vignette -> figure row
#   profiles            profile of the AMEs -> label
#   variables           expert characteristic -> label (comparison table)
#   distributions       distribution of a random coefficient ("n", "ln",
#                       ...) -> name
#   group_name          what a group of constants is, e.g. "occupational group"
# The labels are plain text; latex_text() turns them into LaTeX for the
# tables. All outputs are in English: numbers with a decimal point (0.123)
# and a thousands comma (2,400).

# 1. Numbers, text and labels --------------------------------------------------

#' Numbers as LaTeX text: fixed number of digits, thousands comma, a proper
#' minus sign ($-$), "--" for a missing value (as in section 8.3 of the policy
#' brief script)
latex_number <- function(x, digits) {
  text <- formatC(x, format = "f", digits = digits, big.mark = ",")
  text <- sub("^-", "$-$", text)
  text[is.na(x)] <- "--"
  text
}

#' Number inside a LaTeX formula ($...$), without a thousands separator: a
#' comma in a formula would be followed by a space
math_number <- function(x, digits) {
  formatC(x, format = "f", digits = digits)
}

#' p-value as LaTeX text
latex_p <- function(p) {
  if (p < 0.001) "$p<0.001$" else paste0("$p=", math_number(p, 3), "$")
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

#' Write the lines of a .tex file in UTF-8, so the special characters are read
#' the same on every system
write_tex <- function(lines, file) {
  table_file <- file(file, open = "w", encoding = "UTF-8")
  writeLines(lines, table_file)
  close(table_file)
}

#' Formatted cells of a coefficient table: estimate with stars, standard error
#' in parentheses. Rows without a p-value (standard deviations) get no stars.
#' @param digits decimals: 3 in the paper tables, 2 on the slides
#' @return named lists estimate_of and se_of: parameter name -> cell text
coefficient_cells <- function(coefficients, digits = 3) {
  coefficient_text <- coefficients %>%
    mutate(
      stars = case_when(
        is.na(p) ~ "",
        p < 0.01 ~ "$^{***}$",
        p < 0.05 ~ "$^{**}$",
        p < 0.10 ~ "$^{*}$",
        TRUE ~ ""
      ),
      estimate_text = paste0(latex_number(estimate, digits), stars),
      se_text = paste0("(", latex_number(se, digits), ")")
    )
  # setNames(): the cell texts, named by their parameters
  list(estimate_of = setNames(coefficient_text$estimate_text,
                              coefficient_text$parameter),
       se_of = setNames(coefficient_text$se_text, coefficient_text$parameter))
}

#' Labels of the columns of a design matrix (plain text): labels$terms for the
#' columns of a variable; for a column of an interaction, the labels of its
#' parts joined by " × ", e.g. "Chimney sweep × Income below €40,000". The
#' parts are those of the column's name in R's coding (r_name of
#' design_columns()).
#'
#' @param columns the columns of a specification (results$columns)
#' @return named character vector: column -> label
column_labels <- function(columns, labels) {
  result <- character(0)
  missing_labels <- character(0)
  for (k in seq_len(nrow(columns))) {
    # one part for a variable's column, one per variable for an interaction
    parts <- strsplit(columns$r_name[k], ":", fixed = TRUE)[[1]]
    missing_labels <- c(missing_labels, setdiff(parts, names(labels$terms)))
    result[columns$column[k]] <- paste(labels$terms[parts], collapse = " × ")
  }
  if (length(missing_labels) > 0) {
    stop("No label for: ", paste(unique(missing_labels), collapse = ", "),
         " (add it to TERM_LABELS in 02_output.R)")
  }
  result
}

#' Label of a regressor of a specification that is not a vignette attribute,
#' for the comparison table: labels$variables for a variable; for an
#' interaction, the labels of its variables joined by " × ", with "attributes"
#' for every attribute, e.g. "Occupational group × attributes" for all
#' interactions of the Berufsgruppe with an attribute
#'
#' @param regressor  a regressor as written in the specification
#' @param attributes the vignette attributes
regressor_label <- function(regressor, labels, attributes) {
  variables <- strsplit(regressor, ":", fixed = TRUE)[[1]]
  parts <- character(0)
  for (variable in variables) {
    if (variable %in% attributes) {
      parts <- c(parts, "attributes")
    } else if (variable %in% names(labels$variables)) {
      parts <- c(parts, labels$variables[[variable]])
    } else {
      stop("No label for: ", variable, " (add it to VARIABLE_LABELS in 02_output.R)")
    }
  }
  paste(unique(parts), collapse = " × ")
}

#' Distributions of the random coefficients, and how panels A and B of the
#' coefficient table read for each, as text for the notes
#'
#' @param distributions the distributions that occur ("n", "ln", ...)
#' @return character vector of sentences; empty if all are normal
distribution_note <- function(distributions, labels) {
  notes <- character(0)
  if (any(distributions %in% c("ln", "cn"))) {
    notes <- c(notes, paste(
      "For a log-normal or censored normal coefficient, panels A and B show",
      "the mean and the standard deviation of the normal before it is",
      "exponentiated or censored at zero."))
  }
  if (any(distributions %in% c("u", "t"))) {
    notes <- c(notes, paste(
      "For a uniform or triangular coefficient, panel A shows the mean and",
      "panel B the half-width of its range."))
  }
  notes
}

#' One cell of a table with one item per line: a one-column tabular inside
#' the cell, so that \\ breaks the line
stacked_cell <- function(items) {
  if (length(items) == 0) {
    "--"
  } else {
    paste0("\\begin{tabular}[t]{@{}c@{}}", paste(items, collapse = "\\\\"),
           "\\end{tabular}")
  }
}

# 2. LaTeX tables --------------------------------------------------------------

# How the rows of a random coefficient read in the tables, by its
# distribution: its first parameter (panel A) and its spread (panel B)
MEAN_SUFFIX <- c(n = " (mean)", ln = " (mean of the log)",
                 cn = " (mean before censoring)", u = " (mean)", t = " (mean)")
SPREAD_SUFFIX <- c(n = "", ln = " (sd of the log)",
                   cn = " (sd before censoring)", u = " (half-width)",
                   t = " (half-width)")

#' Rows of the correlations of one correlation block, lower triangle: the row
#' of technology i has its correlations with the technologies j left of it,
#' the standard errors in parentheses below. omega_parameters() names each
#' correlation once, with the two coefficients in mlogit's order, so both
#' orders of the pair are tried.
#'
#' @param estimate_of, se_of from coefficient_cells()
#' @param column       the column of the design matrix whose coefficients
#'                     form the block (one column per block)
#' @param technologies the outcomes but the reference, in their order
#' @return character vector, two lines per technology but the first
correlation_rows <- function(estimate_of, se_of, column, technologies, labels) {
  lines <- character(0)
  for (i in 2:length(technologies)) {
    estimate_cells <- rep("", length(technologies))
    se_cells <- rep("", length(technologies))
    for (j in 1:(i - 1)) {
      name_ji <- paste0("cor.", column, ":", technologies[j], ":", column, ":",
                        technologies[i])
      name_ij <- paste0("cor.", column, ":", technologies[i], ":", column, ":",
                        technologies[j])
      parameter <- if (name_ji %in% names(estimate_of)) name_ji else name_ij
      if (!parameter %in% names(estimate_of)) {
        stop("Correlation missing: ", parameter)
      }
      estimate_cells[j] <- estimate_of[[parameter]]
      se_cells[j] <- se_of[[parameter]]
    }
    lines <- c(lines,
               paste0(latex_text(labels$technologies[[technologies[i]]]), " & ",
                      paste(estimate_cells, collapse = " & "), " \\\\"),
               paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"))
  }
  lines
}

#' LaTeX table of the mixed logit of one specification (section 8.3 of the
#' policy brief script)
#'
#' Layout: one column per technology. Panel A: one coefficient per regressor
#' and technology, with its standard error in parentheses below; for a random
#' coefficient, its mean. Panel B: the standard deviations of the random
#' coefficients across experts, one row per regressor. Panel C (correlated
#' models): their correlations, lower triangle, one block per correlation
#' block. The last panel (D, or C without correlations): fit.
#' Generalisation: with group constants, panel A has one row of mean
#' constants per group, and panel B one row per group; random coefficients of
#' other regressors get their rows in panels B and C as well.
#' The LaTeX document needs \usepackage[T1]{fontenc}, booktabs and
#' threeparttable.
#'
#' @param results           saved results of one specification
#' @param labels            list of labels, see the top of this file
#' @param reference_note    text of the notes describing the reference vignette
#' @param sample_note       text of the notes describing the sample
#' @param extra_note        further text for the notes of this table, or NULL
#' @param show_correlations FALSE leaves panel C out, e.g. when it would not
#'                          fit on a page; the notes then refer to the heat
#'                          maps and the Excel workbook. Panel C is left out as
#'                          well when a correlation block holds the
#'                          coefficients of several regressors.
#' @param hidden_terms      terms (as in the specification) whose rows panel A
#'                          leaves out, e.g. the interactions; the notes then
#'                          refer to the Excel workbook
#' @return character vector, the lines of the .tex file
coefficient_table_tex <- function(results, labels, reference_note, sample_note,
                                  extra_note = NULL, show_correlations = TRUE,
                                  hidden_terms = NULL) {
  # the outcomes but the reference, in their order
  technologies <- results$technologies
  coefficients <- results$coefficient_table %>%
    filter(model == "mixed")
  fit <- results$fit_statistics %>%
    filter(model == "mixed")
  random <- results$random_coefficients
  specification <- results$specification
  is_correlated <- !identical(specification$correlation, "none")
  has_groups <- !identical(specification$constants, "1")
  random_columns <- unique(random$column)
  blocks <- unique(random$block)

  # Every value that appears in the table needs a label
  column_label <- column_labels(results$columns, labels)
  missing_labels <- c(setdiff(technologies, names(labels$technology_headers)),
                      setdiff(technologies, names(labels$technologies)),
                      setdiff(results$reference_outcome,
                              names(labels$technologies)))
  if (length(missing_labels) > 0) {
    stop("No label for: ", paste(missing_labels, collapse = ", "),
         " (add it to the labels in 02_output.R)")
  }
  reference_label <- latex_text(labels$technologies[[results$reference_outcome]])

  # How the rows of a random coefficient read in panels A and B, by its
  # distribution (MEAN_SUFFIX, SPREAD_SUFFIX at the top of this section)
  mean_suffix <- MEAN_SUFFIX
  spread_suffix <- SPREAD_SUFFIX
  # column -> distribution
  distribution_of <- setNames(random$distribution, random$column)

  # Panel C needs one regressor per correlation block: the block is then a
  # triangle of technologies
  columns_per_block <- numeric(0)
  for (b in blocks) {
    is_in_block <- !is.na(random$block) & random$block == b
    columns_per_block <- c(columns_per_block,
                           length(unique(random$column[is_in_block])))
  }
  has_panel_c <- is_correlated && show_correlations &&
    all(columns_per_block == 1)

  cells <- coefficient_cells(coefficients)
  estimate_of <- cells$estimate_of
  se_of <- cells$se_of

  # Column headers: a one-column tabular inside each header cell, so that the
  # line break in the header labels works
  column_headers <- paste0("\\begin{tabular}[b]{@{}c@{}}",
                           labels$technology_headers[technologies],
                           "\\end{tabular}")
  n_columns <- length(technologies) + 1                # row labels + technologies
  span <- paste0("\\multicolumn{", length(technologies), "}{c}{")   # fit rows

  # (a) Head of the table
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{Recommended heating technology: ",
           latex_text(specification$label), " (reference category: ",
           reference_label, ")}"),
    paste0("\\label{tab:rcl_", results$arm, "_ref_",
           file_name(results$reference_outcome), "_", results$name, "}"),
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{4pt}",
    paste0("\\begin{tabular}{l", strrep("c", length(technologies)), "}"),
    "\\toprule",
    paste0(" & ", paste(column_headers, collapse = " & "), " \\\\"),
    "\\midrule",
    paste0("\\multicolumn{", n_columns, "}{l}{\\textit{A. Coefficients}} \\\\")
  )

  # (b) Panel A: two rows per column of the design matrix, the estimates and
  #     the standard errors. A random coefficient's row shows its mean.
  is_shown <- !(results$columns$term %in% hidden_terms)
  for (column in results$columns$column[is_shown]) {
    parameters <- paste0(column, ":", technologies)
    if (anyNA(estimate_of[parameters])) stop("Coefficients missing for ", column)
    label <- latex_text(column_label[[column]])
    if (column %in% random_columns) {
      label <- paste0(label, latex_text(mean_suffix[[distribution_of[[column]]]]))
    }
    lines <- c(lines,
               paste0(label, " & ", paste(estimate_of[parameters], collapse = " & "),
                      " \\\\"),
               paste0(" & ", paste(se_of[parameters], collapse = " & "), " \\\\"))
  }

  # (c) Panel B: standard deviations of the random coefficients across
  #     experts, one row per column of the design matrix
  lines <- c(lines,
             "\\midrule",
             paste0("\\multicolumn{", n_columns,
                    "}{l}{\\textit{B. Standard deviations across experts}} \\\\"))
  for (column in random_columns) {
    parameters <- paste0("sd.", column, ":", technologies)
    if (anyNA(estimate_of[parameters])) {
      stop("Standard deviations missing for ", column)
    }
    label <- paste0(latex_text(column_label[[column]]),
                    latex_text(spread_suffix[[distribution_of[[column]]]]))
    lines <- c(lines,
               paste0(label, " & ", paste(estimate_of[parameters], collapse = " & "),
                      " \\\\"),
               paste0(" & ", paste(se_of[parameters], collapse = " & "), " \\\\"))
  }

  # (d) Panel C: correlations, lower triangle (correlation_rows()). One block
  #     per correlation block; correlations across blocks are zero by
  #     construction.
  if (has_panel_c) {
    lines <- c(lines,
               "\\midrule",
               paste0("\\multicolumn{", n_columns,
                      "}{l}{\\textit{C. Correlations across experts}} \\\\"))
    for (b in blocks) {
      column <- unique(random$column[random$block == b])
      if (length(blocks) > 1) {
        lines <- c(lines, paste0("\\multicolumn{", n_columns, "}{l}{",
                                 latex_text(column_label[[column]]), "} \\\\"))
      }
      lines <- c(lines, correlation_rows(estimate_of, se_of, column,
                                         technologies, labels))
    }
  }

  # (e) The last panel, D (C without correlations): fit statistics, each value
  #     centred across the technology columns
  fit_panel <- if (has_panel_c) "D" else "C"
  lines <- c(lines,
             "\\midrule",
             paste0("\\multicolumn{", n_columns, "}{l}{\\textit{", fit_panel,
                    ". Fit}} \\\\"),
             paste0("Decisions & ", span, latex_number(fit$n_choices, 0), "} \\\\"),
             paste0("Experts & ", span, latex_number(fit$n_experts, 0), "} \\\\"),
             paste0("Parameters & ", span, fit$n_parameters, "} \\\\"),
             paste0("Log-likelihood & ", span,
                    latex_number(fit$log_likelihood, 1), "} \\\\"),
             paste0("McFadden $R^2$ & ", span,
                    latex_number(fit$mcfadden_r2, 3), "} \\\\"),
             paste0("LR test against constants only & ", span,
                    "$\\chi^2(", fit$lr_constants_only_df, ")=",
                    math_number(fit$lr_constants_only, 1), "$, ",
                    latex_p(fit$lr_constants_only_p), "} \\\\"),
             paste0("LR test against multinomial logit & ", span,
                    "$\\chi^2(", fit$lr_mnl_df, ")=",
                    math_number(fit$lr_mnl, 1), "$, ",
                    latex_p(fit$lr_mnl_p), "} \\\\"),
             paste0("Halton draws per expert & ", span,
                    latex_number(fit$n_draws_per_expert, 0), "} \\\\"))

  # (f) Notes. What is random, with which distribution: the constants, the
  #     constants of each group, or a regressor's label (the labels of its
  #     columns)
  random_items <- character(0)
  for (term in unique(random$term)) {
    term_columns <- unique(random$column[random$term == term])
    distribution <- random$distribution[random$term == term][1]
    if (term == "(Intercept)") {
      item <- "the constants"
    } else if (has_groups && term %in% specification$constants) {
      item <- paste("the constants of each", labels$group_name)
    } else {
      item <- paste(column_label[term_columns], collapse = ", ")
    }
    random_items <- c(random_items,
                      paste0(item, " (", labels$distributions[[distribution]],
                             ")"))
  }
  # the constants of the groups once
  random_items <- unique(random_items)
  random_note <- paste0("Random coefficients, which vary across experts: ",
                        paste(random_items, collapse = "; "), ".")
  if (has_groups) {
    random_note <- paste0(random_note, " Each ", labels$group_name,
                          " has its own means (panel A) and standard ",
                          "deviations (panel B) of the constants.")
  }
  # Correlation: which coefficients are correlated, and where the table shows
  # their standard deviations and correlations. One block per group's
  # constants reads "within each group"; lengths(): the number of terms of
  # each block.
  is_block_per_group <- has_groups && is.list(specification$correlation) &&
    all(lengths(specification$correlation) == 1) &&
    all(unlist(specification$correlation) %in% specification$constants)
  if (!is_correlated) {
    correlation_note <- "The random coefficients are independent of each other."
  } else {
    if (identical(specification$correlation, "full")) {
      structure_text <- "The random coefficients are correlated"
    } else if (is_block_per_group) {
      # every expert belongs to one group, so these correlations never enter
      # the likelihood (cholesky_fixed_at_zero())
      structure_text <- paste0(
        "The constants are correlated within each ", labels$group_name,
        "; correlations across groups are not identified, as each expert ",
        "belongs to one group, and are set to zero")
    } else {
      block_texts <- character(0)
      for (b in blocks) {
        block_columns <- unique(random$column[random$block == b])
        block_texts <- c(block_texts,
                         paste(column_label[block_columns], collapse = "; "))
      }
      structure_text <- paste0(
        "The random coefficients are correlated within each of the blocks [",
        paste(block_texts, collapse = "], ["), "]; correlations across blocks ",
        "are set to zero")
    }
    if (has_panel_c) {
      panels_text <- paste("; panels B and C show their standard deviations and",
                           "correlations, with delta-method standard errors.")
    } else {
      panels_text <- paste("; panel B shows their standard deviations, with",
                           "delta-method standard errors.")
    }
    correlation_note <- paste0(structure_text, panels_text)
  }
  correlations_note <- NULL
  if (is_correlated && !has_panel_c) {
    correlations_note <- paste("The correlations are not in this table because",
                               "of their number; they are in the heat maps and",
                               "the Excel workbook.")
  }
  hidden_note <- NULL
  if (any(results$columns$term %in% hidden_terms)) {
    hidden_labels <- character(0)
    for (term in intersect(hidden_terms, results$columns$term)) {
      hidden_labels <- c(hidden_labels,
                         regressor_label(term, labels,
                                         names(results$attribute_levels)))
    }
    hidden_note <- paste0("Not in this table because of their number: the ",
                          "coefficients of ",
                          paste(unique(hidden_labels), collapse = ", "),
                          ". They are in the Excel workbook.")
  }
  draws_note <- NULL
  if (specification$draws == "halton_by_group") {
    draws_note <- paste0("The Halton sequences start with the same primes for ",
                         "each ", labels$group_name, ".")
  }
  # The sample first, then the model and its parameters
  notes <- c(
    sample_note,
    paste("Mixed logit of the heating technology that experts recommend for the",
          "household of a vignette. Reference category:",
          paste0("\\textit{", reference_label, "};"),
          "each coefficient is the effect on the log odds of the recommendation in",
          "its column relative to the reference category."),
    latex_text(random_note),
    distribution_note(random$distribution, labels),
    latex_text(correlation_note),
    correlations_note,
    latex_text(hidden_note),
    extra_note,
    reference_note,
    paste("Standard errors in parentheses, clustered by expert (sandwich",
          "estimator). $^{*}$ $p<0.10$, $^{**}$ $p<0.05$, $^{***}$ $p<0.01$",
          "(two-sided $z$ tests)."),
    paste("Standard deviations carry no stars: a standard deviation of zero lies",
          "on the boundary of the parameter space."),
    paste("McFadden $R^2$ and the first LR test compare the model with a model",
          "with constants only, which predicts the observed shares of the",
          "recommendations for every vignette."),
    paste("The LR test against the multinomial logit tests whether the random",
          "coefficients vary across experts; its $p$-value is conservative,",
          "because a variance of zero lies on the boundary of the parameter",
          "space."),
    draws_note
  )
  c(lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste0("\\item \\textit{Notes:} ", paste(notes, collapse = " ")),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}")
}

#' LaTeX table comparing the mixed logits of several specifications: one
#' column per specification, headed by its number. Panel A: what each
#' specification contains and its fit. Panel B: likelihood-ratio tests between
#' nested specifications.
#'
#' @param results_list named list of saved results, in the order of the columns
#' @param lr           tibble from lr_tests(): restricted, unrestricted, lr, df, p,
#'                     p_robust
#' @param attributes   the vignette attributes; the other regressors are listed
#'                     in the row "Further regressors"
#' @param numbers      number of each specification, e.g. "(5)", named by the
#'                     specifications; all of them, also those of the tests
#'                     with a specification that is not a column
#' @param table_name   part of the LaTeX label, e.g. "comparison"
#' @return character vector, the lines of the .tex file
comparison_table_tex <- function(results_list, lr, labels, attributes,
                                 sample_note, numbers, table_name) {
  n_specifications <- length(results_list)
  column_numbers <- numbers[names(results_list)]
  n_columns <- n_specifications + 1
  first <- results_list[[1]]

  # One cell per specification and row
  spec_labels <- character(0)
  random_cells <- character(0)
  regressors_cells <- character(0)
  correlation_cells <- character(0)
  has_any_groups <- FALSE
  fit_rows <- tibble()
  for (spec_name in names(results_list)) {
    results <- results_list[[spec_name]]
    specification <- results$specification
    random <- results$random_coefficients
    spec_labels[spec_name] <- specification$label
    has_groups <- !identical(specification$constants, "1")
    has_any_groups <- has_any_groups || has_groups
    column_label <- column_labels(results$columns, labels)

    # Random coefficients: one line per regressor, and its distribution on
    # the next line if it is not normal (\\ breaks the line in the cell)
    random_items <- character(0)
    for (term in unique(random$term)) {
      distribution <- random$distribution[random$term == term][1]
      if (term == "(Intercept)") {
        item <- "Constants"
      } else if (has_groups && term %in% specification$constants) {
        item <- "Group constants"
      } else {
        item <- paste(column_label[unique(random$column[random$term == term])],
                      collapse = ", ")
      }
      item <- latex_text(item)
      if (distribution != "n") {
        item <- paste0(item, "\\\\(", labels$distributions[[distribution]], ")")
      }
      random_items <- c(random_items, item)
    }
    random_cells[spec_name] <- stacked_cell(unique(random_items))

    # Further regressors: those that are not vignette attributes. An
    # interaction takes two lines, broken after its "×", so that four or
    # five columns fit the page.
    other_regressors <- setdiff(specification$regressors, attributes)
    regressor_items <- character(0)
    for (regressor in other_regressors) {
      regressor_items <- c(regressor_items,
                           regressor_label(regressor, labels, attributes))
    }
    regressor_items <- gsub(" $\\times$ ", " $\\times$\\\\",
                            latex_text(unique(regressor_items)), fixed = TRUE)
    regressors_cells[spec_name] <- stacked_cell(regressor_items)

    # Correlation: none, all, within each group (one block per group's
    # constants), or in blocks
    correlation <- specification$correlation
    if (identical(correlation, "none")) {
      correlation_cells[spec_name] <- "None"
    } else if (identical(correlation, "full")) {
      correlation_cells[spec_name] <- "All"
    } else if (has_groups && all(lengths(correlation) == 1) &&
               all(unlist(correlation) %in% specification$constants)) {
      correlation_cells[spec_name] <- "Within groups"
    } else {
      correlation_cells[spec_name] <- "In blocks"
    }

    fit_rows <- bind_rows(fit_rows, results$fit_statistics %>%
                            filter(model == "mixed") %>%
                            mutate(specification = spec_name))
  }

  # One row of panel A: its label, then one cell per specification
  table_row <- function(label, cells) {
    paste0(label, " & ", paste(cells, collapse = " & "), " \\\\")
  }
  reference_label <- latex_text(labels$technologies[[first$reference_outcome]])
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{Recommended heating technology: comparison of the ",
           "specifications (reference category: ", reference_label, ")}"),
    paste0("\\label{tab:rcl_", first$arm, "_ref_",
           file_name(first$reference_outcome), "_", table_name, "}"),
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{4pt}",
    paste0("\\begin{tabular}{l", strrep("c", n_specifications), "}"),
    "\\toprule",
    table_row("", column_numbers),
    "\\midrule",
    paste0("\\multicolumn{", n_columns,
           "}{l}{\\textit{A. Specification and fit}} \\\\"),
    table_row("Random coefficients", random_cells),
    table_row("Further regressors", regressors_cells),
    table_row("Correlation", correlation_cells),
    table_row("Decisions", latex_number(fit_rows$n_choices, 0)),
    table_row("Experts", latex_number(fit_rows$n_experts, 0)),
    table_row("Parameters", fit_rows$n_parameters),
    table_row("Log-likelihood", latex_number(fit_rows$log_likelihood, 1)),
    table_row("McFadden $R^2$", latex_number(fit_rows$mcfadden_r2, 3)),
    table_row("AIC", latex_number(fit_rows$aic, 1)),
    table_row("BIC", latex_number(fit_rows$bic, 1))
  )

  # Panel B: one row per test, its text across all specification columns: the
  # statistic, the classical and the robust p-value (lr_tests()). A negative
  # statistic gets a dagger and no p-values.
  if (nrow(lr) > 0) {
    lines <- c(lines,
               "\\midrule",
               paste0("\\multicolumn{", n_columns, "}{l}{\\textit{B. LR tests ",
                      "between nested specifications}} \\\\"))
    for (i in seq_len(nrow(lr))) {
      statistic <- paste0("$\\chi^2(", lr$df[i], ")=", math_number(lr$lr[i], 1),
                          "$")
      if (lr$lr[i] < 0) {
        p_values <- "$^{\\dagger}$; --"
      } else {
        p_values <- paste0("; ", latex_p(lr$p[i]), ", robust ",
                           latex_p(lr$p_robust[i]))
      }
      lines <- c(lines, paste0(
        numbers[[lr$restricted[i]]], " vs ", numbers[[lr$unrestricted[i]]],
        " & \\multicolumn{", n_specifications, "}{l}{", statistic, p_values,
        "} \\\\"
      ))
    }
  }

  lr_notes <- NULL
  if (nrow(lr) > 0) {
    lr_notes <- paste(
      "An LR test compares a specification with a more general one that",
      "contains it as a special case. Its first $p$-value comes from the",
      "$\\chi^2$ distribution, which holds only if the model is correctly",
      "specified; the robust one from the distribution under possible",
      "misspecification, a weighted sum of $\\chi^2_1$ variables with weights",
      "from the Hessian and the covariance matrix clustered by expert, as for",
      "the standard errors (Kent 1982; White 1982)."
    )
  }
  if (nrow(lr) > 0 && any(lr$lr < 0)) {
    lr_notes <- c(lr_notes, paste(
      "$^{\\dagger}$ Negative statistic: the more general specification has the",
      "lower log-likelihood, so it has not reached its maximum; the test is not",
      "informative."
    ))
  }
  groups_note <- NULL
  if (has_any_groups) {
    groups_note <- paste0("Group constants: one set of constants per ",
                          labels$group_name, ", each with its own means and ",
                          "(co)variances.")
  }

  # The sample first, then the specifications and the statistics
  notes <- c(
    sample_note,
    paste("Mixed logits of the heating technology that experts recommend for the",
          "household of a vignette, all with the attributes of the vignette as",
          "regressors."),
    paste0("Specifications: ",
           paste0(column_numbers, " ", latex_text(spec_labels), collapse = "; "),
           "."),
    groups_note,
    paste("AIC $= -2 \\log L + 2k$, BIC $= -2 \\log L + k \\log N$, with $k$",
          "parameters and $N$ experts."),
    lr_notes
  )
  c(lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste0("\\item \\textit{Notes:} ", paste(notes, collapse = " ")),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}")
}

# 3. Figures -------------------------------------------------------------------

#' Figure of the AMEs at the reference vignette for one outcome (section 8.4
#' of the policy brief script): the variations of the reference vignette on
#' the vertical axis; for each group (a profile or a specification), the AME
#' (point) and its 95% interval (line), in percentage points.
#'
#' @param plot_data one row per variation x group: variation (factor; its
#'                  first level at the bottom), group (factor; its first level
#'                  at the top of each variation), ame, ci_low, ci_high
#' @param title     the outcome; NULL for none (on the slides the frame title
#'                  says it)
#' @param subtitle  text below the title
#' @param base_size font size in pt: 11 for the figures of the paper size
#'                  (8 x 5 in), 9 for the slide versions; the subtitle and the
#'                  axis title are scaled with it
#' @return ggplot
plot_ame <- function(plot_data, title, subtitle, base_size = 11) {
  # Okabe-Ito colours, the first three as in the policy brief
  colours <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9")
  shapes <- c(16, 17, 15, 18, 3, 4)
  n_groups <- length(levels(plot_data$group))
  if (n_groups > length(colours)) {
    stop("At most ", length(colours), " groups per figure")
  }

  # coord_flip() puts the variations on the vertical axis; position_dodge()
  # places the groups next to each other within each variation, in the order
  # of `group`: reversed, so that the first group is at the top
  ggplot(plot_data, aes(x = variation, y = ame, ymin = ci_low, ymax = ci_high,
                        colour = group, shape = group,
                        group = factor(group, levels = rev(levels(group))))) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(position = position_dodge(width = 0.6), size = 0.4) +
    coord_flip() +
    scale_colour_manual(values = colours[seq_len(n_groups)]) +
    scale_shape_manual(values = shapes[seq_len(n_groups)]) +
    labs(title = title,
         subtitle = subtitle,
         x = NULL,
         y = "Change in probability in percentage points (95% confidence interval)",
         colour = NULL, shape = NULL) +
    theme_minimal(base_size = base_size) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.subtitle = element_text(size = 9 * base_size / 11),
          axis.title.x = element_text(size = 10 * base_size / 11),
          plot.title.position = "plot")
}

#' Subtitle of an AME figure: a first line, then the probability of the
#' outcome at the reference vignette for each group, e.g. "40.2% (Energy
#' advisers)"
#'
#' @param plot_data    as for plot_ame(), with p_reference in percent
#' @param first_line   text of the first line; NULL for none (the slides)
#' @param group_labels short names of the groups for the subtitle, named by
#'                     the groups (e.g. "(1)" for a specification); NULL: the
#'                     groups themselves
baseline_subtitle <- function(plot_data, first_line, group_labels = NULL) {
  baseline <- plot_data %>%
    distinct(group, p_reference) %>%
    arrange(group)
  group_text <- as.character(baseline$group)
  if (!is.null(group_labels)) group_text <- group_labels[group_text]
  baseline_text <- paste0(formatC(baseline$p_reference, format = "f", digits = 1),
                          "% (", group_text, ")")
  baseline_line <- paste0("Probability at the reference vignette: ",
                          paste(baseline_text, collapse = ", "))
  if (is.null(first_line)) {
    baseline_line
  } else {
    paste0(first_line, "\n", baseline_line)
  }
}

#' Standard deviations and correlations of the random coefficients of one
#' correlation block as a matrix (step (c) of
#' R/first_policy_brief/04_heatmap_rc_logit.R): standard deviations on the
#' diagonal, correlations off it
#'
#' @param coefficients the coefficient table of the mixed logit
#' @param rpar_names   the random coefficients, in the order of rows and columns
#' @return K x K matrix
sd_cor_matrix <- function(coefficients, rpar_names) {
  estimate_of <- setNames(coefficients$estimate, coefficients$parameter)
  K <- length(rpar_names)
  result <- matrix(NA_real_, nrow = K, ncol = K,
                   dimnames = list(rpar_names, rpar_names))
  for (i in 1:K) {
    for (j in 1:K) {
      if (i == j) {
        parameter <- paste0("sd.", rpar_names[i])
      } else {
        # each correlation is stored once, with the two coefficients in
        # mlogit's order, so both orders of the pair are tried
        name_ij <- paste0("cor.", rpar_names[i], ":", rpar_names[j])
        name_ji <- paste0("cor.", rpar_names[j], ":", rpar_names[i])
        parameter <- if (name_ij %in% names(estimate_of)) name_ij else name_ji
      }
      if (!parameter %in% names(estimate_of)) stop("Parameter missing: ", parameter)
      result[i, j] <- estimate_of[[parameter]]
    }
  }
  # anyNA(): TRUE if any cell is NA
  is_diagonal_cell <- diag(K) == 1
  stopifnot(
    !anyNA(result),
    all(result[is_diagonal_cell] > 0),
    all(abs(result[!is_diagonal_cell]) <= 1)
  )
  result
}

#' Heat map of the standard deviations (diagonal) and correlations (off the
#' diagonal) of the random coefficients of one correlation block, as in
#' R/first_policy_brief/04_heatmap_rc_logit.R
#'
#' @param sd_cor     matrix from sd_cor_matrix()
#' @param row_labels random coefficient -> label, for the rows and columns
#' @param title, subtitle, caption texts of the figure; NULL for none
#' @param base_size  font size in pt: 11 for the figures of the paper size
#'                   (8 x 8 in), 9 for the slide versions
#' @param text_size  size of the numbers in the tiles (in mm, as in
#'                   geom_text()); NULL: 3.8, or 2.6 with more than 8 rows
#' @return ggplot
plot_correlation_heatmap <- function(sd_cor, row_labels, title, subtitle, caption,
                                     base_size = 11, text_size = NULL) {
  # Correlations from red (-1) over light grey (0) to blue (1); the diagonal
  # (standard deviations) is grey, outside this scale. White text on dark
  # tiles, from a correlation of about 0.88 on (see the heat map script).
  color_negative <- "#E15759"
  color_zero <- "#F2F2F2"
  color_positive <- "#4E79A7"
  color_diagonal <- "grey85"
  color_text <- "#2F3B4A"
  white_text_above <- 0.88
  # smaller numbers when there are many tiles
  if (is.null(text_size)) {
    text_size <- if (nrow(sd_cor) <= 8) 3.8 else 2.6
  }

  # Long format, one row per cell. Off the diagonal, `correlation` sets the
  # colour; on it, correlation is NA, which the fill scale draws in
  # color_diagonal. Factor levels: the first coefficient in the top row and
  # the left column. formatC() writes the numbers with two decimals.
  rpar_names <- rownames(sd_cor)
  labels_in_order <- unname(row_labels[rpar_names])
  heatmap_data <- as_tibble(sd_cor, rownames = "row") %>%
    pivot_longer(-row, names_to = "column", values_to = "value") %>%
    mutate(
      is_diagonal = row == column,
      correlation = if_else(is_diagonal, NA_real_, value),
      label = formatC(value, format = "f", digits = 2),
      text_white = !is_diagonal & value > white_text_above,
      fontface = if_else(is_diagonal, "bold", "plain"),
      row = factor(unname(row_labels[row]), levels = rev(labels_in_order)),
      column = factor(unname(row_labels[column]), levels = labels_in_order)
    )

  ggplot(heatmap_data, aes(x = column, y = row, fill = correlation)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = label, colour = text_white, fontface = fontface),
              size = text_size) +
    scale_fill_gradient2(low = color_negative, mid = color_zero,
                         high = color_positive, midpoint = 0, limits = c(-1, 1),
                         na.value = color_diagonal) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = color_text),
                        guide = "none") +
    scale_x_discrete(position = "top") +
    coord_fixed() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = NULL,
         y = NULL) +
    theme_minimal(base_size = base_size) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
          axis.text = element_text(colour = color_text),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 9 * base_size / 11),
          plot.caption = element_text(size = 9 * base_size / 11, hjust = 0),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.position = "none")
}

# 4. Excel ---------------------------------------------------------------------

#' The sheets of the Excel workbook (section 8.5 of the policy brief script):
#' the specifications, and the coefficients, AMEs, fit and LR tests of all
#' specifications, with labels. The numbers are unrounded.
#'
#' @param results_list named list of saved results
#' @param lr           tibble from lr_tests()
#' @param numbers      number of each specification, e.g. "(5)", named by the
#'                     specifications
#' @return named list of tibbles, one per sheet
results_excel_sheets <- function(results_list, lr, labels, numbers) {
  # The specification of each sheet row: its number and label
  spec_labels <- character(0)
  for (spec_name in names(results_list)) {
    spec_labels[spec_name] <- paste(numbers[[spec_name]],
                                    results_list[[spec_name]]$specification$label)
  }

  specifications <- tibble()
  coefficients <- tibble()
  ames <- tibble()
  fit <- tibble()
  for (spec_name in names(results_list)) {
    results <- results_list[[spec_name]]
    specification <- results$specification

    # (a) The specification as written in 01_run_specifications.R, a record
    #     in R's names; the column names say so
    if (is.list(specification$correlation)) {
      correlation_text <- character(0)
      for (block in specification$correlation) {
        correlation_text <- c(correlation_text,
                              paste0("(", paste(block, collapse = ", "), ")"))
      }
      correlation_text <- paste("blocks", paste(correlation_text, collapse = " "))
    } else {
      correlation_text <- specification$correlation
    }
    specifications <- bind_rows(specifications, tibble(
      Specification = spec_labels[[spec_name]],
      `Draws per expert` = expert_draws(results),
      Estimated = as.character(results$date),
      `Name (as in 01_run_specifications.R)` = spec_name,
      `Constants (as in 01_run_specifications.R)` =
        paste(specification$constants, collapse = ", "),
      `Regressors (as in 01_run_specifications.R)` =
        paste(specification$regressors, collapse = ", "),
      `Random (as in 01_run_specifications.R)` =
        paste0(names(specification$random), " = ", specification$random,
               collapse = ", "),
      `Correlation (as in 01_run_specifications.R)` = correlation_text,
      `Draws (as in 01_run_specifications.R)` = specification$draws
    ))

    # (b) Coefficients, with the labels of each coefficient's column and
    #     technology: a lookup from mlogit's names "<column>:<technology>"
    column_label <- column_labels(results$columns, labels)
    lookup <- tibble()
    for (k in seq_len(nrow(results$columns))) {
      column <- results$columns$column[k]
      lookup <- bind_rows(lookup, tibble(
        coefficient = paste0(column, ":", results$technologies),
        term_label = column_label[[column]],
        technology_label = unname(labels$technologies[results$technologies])
      ))
    }
    lookup_2 <- lookup %>%
      rename(coefficient_2 = coefficient, term_label_2 = term_label,
             technology_label_2 = technology_label)
    #     What each row is: for a random coefficient of the mixed logit, the
    #     parameters depend on its distribution (coefficient_draws())
    distributions <- results$random_coefficients %>%
      select(coefficient = name, distribution)
    coefficients_spec <- results$coefficient_table %>%
      left_join(lookup, by = "coefficient") %>%
      left_join(lookup_2, by = "coefficient_2") %>%
      left_join(distributions, by = "coefficient") %>%
      mutate(
        specification = spec_labels[[spec_name]],
        is_random = model == "mixed" & !is.na(distribution),
        parameter_label = case_when(
          type == "cor" ~ "Correlation",
          type == "coefficient" & !is_random ~ "Coefficient",
          type == "coefficient" & distribution == "ln" ~ "Mean of the log",
          type == "coefficient" & distribution == "cn" ~ "Mean before censoring",
          type == "coefficient" ~ "Mean",
          distribution == "ln" ~ "Standard deviation of the log",
          distribution == "cn" ~ "Standard deviation before censoring",
          distribution %in% c("u", "t") ~ "Half-width",
          TRUE ~ "Standard deviation"
        )
      )
    if (anyNA(coefficients_spec$term_label)) {
      stop("Coefficients without a label in ", spec_name)
    }
    coefficients <- bind_rows(coefficients, coefficients_spec)

    ames <- bind_rows(ames, results$ame_table %>%
                        mutate(specification = spec_labels[[spec_name]]))
    fit <- bind_rows(fit, results$fit_statistics %>%
                       mutate(specification = spec_labels[[spec_name]]))
  }

  # Labels of the variations, outcomes and profiles; a missing label would
  # leave an empty cell, so check first
  missing_labels <- c(setdiff(ames$vignette, names(labels$variations)),
                      setdiff(ames$outcome, names(labels$technologies)),
                      setdiff(ames$profile, names(labels$profiles)))
  if (length(missing_labels) > 0) {
    stop("No label for: ", paste(unique(missing_labels), collapse = ", "))
  }

  # The restrictions of the LR tests in words: the types of lr_restrictions(),
  # several joined by " + "
  restriction_labels <- c(
    regressors = "the coefficients of the added regressors are zero",
    correlations = "the correlations are zero",
    group_covariances = "the (co)variances of the constants are equal across groups"
  )
  restriction_text <- character(nrow(lr))
  for (i in seq_len(nrow(lr))) {
    types <- strsplit(lr$restrictions[i], " + ", fixed = TRUE)[[1]]
    restriction_text[i] <- paste(restriction_labels[types], collapse = "; ")
  }

  list(
    Specifications = specifications,
    Coefficients = coefficients %>%
      transmute(Specification = specification,
                Model = unname(labels$models[model]),
                Parameter = parameter_label,
                Regressor = term_label,
                Technology = technology_label,
                `Regressor 2` = term_label_2,
                `Technology 2` = technology_label_2,
                Estimate = estimate,
                `Standard error` = se,
                z = z,
                p = p,
                `CI low (95%)` = ci_low,
                `CI high (95%)` = ci_high,
                `Parameter in mlogit` = parameter),
    `Marginal effects` = ames %>%
      transmute(Specification = specification,
                Model = unname(labels$models[model]),
                Profile = unname(labels$profiles[profile]),
                Variation = unname(labels$variations[vignette]),
                Recommendation = unname(labels$technologies[outcome]),
                `Probability at the reference vignette` = p_reference,
                `Standard error of the probability` = se_p_reference,
                `Marginal effect` = ame,
                `Standard error` = se,
                z = z,
                p = p,
                `CI low (95%)` = ci_low,
                `CI high (95%)` = ci_high,
                `CI low (95%, Krinsky-Robb)` = ci_low_kr,
                `CI high (95%, Krinsky-Robb)` = ci_high_kr),
    Fit = fit %>%
      transmute(Specification = specification,
                Model = unname(labels$models[model]),
                Decisions = n_choices,
                Experts = n_experts,
                Parameters = n_parameters,
                `Halton draws per expert` = n_draws_per_expert,
                `Log-likelihood` = log_likelihood,
                `Log-likelihood, constants only` = log_likelihood_constants_only,
                `McFadden R2` = mcfadden_r2,
                `LR test against constants only: chi2` = lr_constants_only,
                `LR test against constants only: df` = lr_constants_only_df,
                `LR test against constants only: p` = lr_constants_only_p,
                `LR test against MNL: chi2` = lr_mnl,
                `LR test against MNL: df` = lr_mnl_df,
                `LR test against MNL: p` = lr_mnl_p,
                AIC = aic,
                BIC = bic),
    `LR tests` = lr %>%
      transmute(`Restricted specification` = unname(spec_labels[restricted]),
                `More general specification` = unname(spec_labels[unrestricted]),
                Restrictions = restriction_text,
                Chi2 = lr,
                `Degrees of freedom` = df,
                p = p,
                `p (robust)` = p_robust,
                `Mean weight` = mean_weight)
  )
}

# 5. Slides --------------------------------------------------------------------
# Versions of the tables for the 16:9 slides of the economics-latex layout,
# which a paper table is too long for. Each specification gets several short
# tables, each a bare tabular: the slide frame around it (slide_frame()) has
# the title and the notes. Estimates are rounded to two decimals, as on
# slides; the paper tables and Excel have the precise values.

#' A slide table: one label column and n_value_columns centred columns
#'
#' @param header the line of the column headers
#' @param body   the lines of the rows
#' @return character vector, the lines of the tabular
slide_tabular <- function(header, body, n_value_columns) {
  c("\\setlength{\\tabcolsep}{4pt}",
    paste0("\\begin{tabular}{l", strrep("c", n_value_columns), "}"),
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}")
}

#' Rows of a slide table of coefficients: for each row, the estimates of its
#' parameters in the technology columns, and their standard errors in
#' parentheses below
#'
#' @param cells        from coefficient_cells()
#' @param row_labels   LaTeX labels of the rows
#' @param prefixes     for each row, its parameters without ":<technology>",
#'                     e.g. "att_build_year2000" or "sd.(Intercept)"
#' @param technologies the technology columns, in their order
#' @return character vector, two lines per row
slide_coefficient_rows <- function(cells, row_labels, prefixes, technologies) {
  lines <- character(0)
  for (k in seq_along(prefixes)) {
    parameters <- paste0(prefixes[k], ":", technologies)
    if (anyNA(cells$estimate_of[parameters])) {
      stop("Parameters missing for ", prefixes[k])
    }
    lines <- c(lines,
               paste0(row_labels[k], " & ",
                      paste(cells$estimate_of[parameters], collapse = " & "),
                      " \\\\"),
               paste0(" & ", paste(cells$se_of[parameters], collapse = " & "),
                      " \\\\"))
  }
  lines
}

#' LaTeX labels of the rows of the coefficients of some columns of the design
#' matrix: the column's label, and for a random coefficient what its row shows
#' (MEAN_SUFFIX)
#'
#' @param columns_shown  the columns, in the order of the rows
#' @param column_label   from column_labels()
#' @param random         the random coefficients (results$random_coefficients)
coefficient_row_labels <- function(columns_shown, column_label, random) {
  row_labels <- character(0)
  for (column in columns_shown) {
    label <- column_label[[column]]
    if (column %in% random$column) {
      distribution <- random$distribution[random$column == column][1]
      label <- paste0(label, MEAN_SUFFIX[[distribution]])
    }
    row_labels <- c(row_labels, latex_text(label))
  }
  row_labels
}

#' The slide tables of the mixed logit of one specification: the paper table
#' (coefficient_table_tex()) split into tables that fit a slide
#'
#'   constants      the constants and the other regressors that are neither
#'                  attributes nor interactions (the occupational group), then
#'                  the standard deviations of the random coefficients
#'   attributes     the effects of the vignette attributes
#'   interactions_  one table per level of the first variable of the
#'                  interactions, e.g. chimney sweeps x each attribute
#'   correlations   the correlations, one table per correlation block, if each
#'                  block holds the coefficients of one column (otherwise they
#'                  are in the heat maps and Excel only)
#'
#' @param results saved results of one specification
#' @param labels  list of labels, see the top of this file; labels$groups names
#'                the groups of the group constants
#' @return list of tables, each a list: name (part of the file name), title
#'   (plain text), note (LaTeX, for the frame) and lines (the tabular)
slide_coefficient_tables <- function(results, labels) {
  technologies <- results$technologies
  coefficients <- results$coefficient_table %>%
    filter(model == "mixed")
  cells <- coefficient_cells(coefficients, digits = 2)
  random <- results$random_coefficients
  columns <- results$columns
  column_label <- column_labels(columns, labels)
  random_columns <- unique(random$column)
  n_columns <- length(technologies) + 1

  column_headers <- paste0("\\begin{tabular}[b]{@{}c@{}}",
                           labels$technology_headers[technologies],
                           "\\end{tabular}")
  header <- paste0(" & ", paste(column_headers, collapse = " & "), " \\\\")
  stars_note <- paste("Standard errors clustered by expert in parentheses;",
                      "$^{*}$ $p<0.10$, $^{**}$ $p<0.05$, $^{***}$ $p<0.01$.")

  is_attribute <- columns$term %in% names(results$attribute_levels)
  is_interaction <- grepl(":", columns$term, fixed = TRUE)
  tables <- list()

  # (a) Constants and the other regressors, then the standard deviations
  constant_columns <- columns$column[!is_attribute & !is_interaction]
  sd_labels <- character(0)
  for (column in random_columns) {
    distribution <- random$distribution[random$column == column][1]
    sd_labels <- c(sd_labels, latex_text(paste0(column_label[[column]],
                                                SPREAD_SUFFIX[[distribution]])))
  }
  constants_body <- c(
    paste0("\\multicolumn{", n_columns, "}{l}{\\textit{Coefficients}} \\\\"),
    slide_coefficient_rows(cells,
                           coefficient_row_labels(constant_columns, column_label,
                                                  random),
                           constant_columns, technologies),
    "\\midrule",
    paste0("\\multicolumn{", n_columns,
           "}{l}{\\textit{Standard deviations across experts}} \\\\"),
    slide_coefficient_rows(cells, sd_labels, paste0("sd.", random_columns),
                           technologies)
  )
  tables[["constants"]] <- list(
    name = "constants",
    title = "Constants and their standard deviations across experts",
    note = paste(stars_note, "Standard deviations carry no stars."),
    lines = slide_tabular(header, constants_body, length(technologies))
  )

  # (b) Attributes
  attribute_columns <- columns$column[is_attribute]
  attributes_body <- slide_coefficient_rows(
    cells, coefficient_row_labels(attribute_columns, column_label, random),
    attribute_columns, technologies
  )
  tables[["attributes"]] <- list(
    name = "attributes",
    title = "Effects of the vignette attributes",
    note = stars_note,
    lines = slide_tabular(header, attributes_body, length(technologies))
  )

  # (c) Interactions, one table per level of their first variable. The row
  #     label is the label of the other parts of the column's name in R's
  #     coding, e.g. "Income below €40,000" for "berufsgruppeSchornsteinfeger:
  #     att_incomeunter 40.000 € brutto im Jahr".
  #     The note names the variable whose reference category the difference
  #     is from (labels$variables, e.g. "occupational group").
  interaction_columns <- columns$column[is_interaction]
  interaction_r_names <- columns$r_name[is_interaction]
  interaction_terms <- columns$term[is_interaction]
  first_parts <- character(0)
  other_labels <- character(0)
  first_variables <- character(0)
  for (k in seq_along(interaction_r_names)) {
    parts <- strsplit(interaction_r_names[k], ":", fixed = TRUE)[[1]]
    first_parts <- c(first_parts, parts[1])
    other_labels <- c(other_labels,
                      paste(labels$terms[parts[-1]], collapse = " × "))
    first_variables <- c(first_variables,
                         strsplit(interaction_terms[k], ":", fixed = TRUE)[[1]][1])
  }
  for (first_part in unique(first_parts)) {
    is_part <- first_parts == first_part
    first_label <- labels$terms[[first_part]]
    first_variable <- first_variables[is_part][1]
    if (first_variable %in% names(labels$variables)) {
      variable_label <- tolower(labels$variables[[first_variable]])
    } else {
      variable_label <- first_variable
    }
    interactions_body <- slide_coefficient_rows(
      cells, latex_text(other_labels[is_part]), interaction_columns[is_part],
      technologies
    )
    table_name <- paste0("interactions_", file_name(first_label))
    tables[[table_name]] <- list(
      name = table_name,
      title = paste0("Interactions: ", first_label, " × attribute"),
      note = paste0("Difference in the effect of the attribute from the ",
                    "reference category of the ", latex_text(variable_label),
                    ". ", stars_note),
      lines = slide_tabular(header, interactions_body, length(technologies))
    )
  }

  # (d) Correlations, one table per correlation block of one column
  if (!identical(results$specification$correlation, "none")) {
    blocks <- unique(random$block)
    for (b in blocks) {
      block_columns <- unique(random$column[random$block == b])
      if (length(block_columns) > 1) next
      block_term <- unique(random$term[random$block == b])
      is_constant <- block_term == "(Intercept)" ||
        block_term %in% results$specification$constants
      if (is_constant) {
        title <- "Correlations of the constants across experts"
      } else {
        title <- paste0("Correlations of the coefficients of ",
                        column_label[[block_columns]], " across experts")
      }
      table_name <- "correlations"
      if (length(blocks) > 1) {
        if (block_columns %in% names(labels$groups)) {
          block_label <- labels$groups[[block_columns]]
        } else {
          block_label <- column_label[[block_columns]]
        }
        title <- paste0(title, ": ", block_label)
        table_name <- paste0("correlations_", file_name(block_label))
      }
      correlations_body <- correlation_rows(cells$estimate_of, cells$se_of,
                                            block_columns, technologies, labels)
      tables[[table_name]] <- list(
        name = table_name,
        title = title,
        note = paste("Correlations implied by the estimated Cholesky factor,",
                     "with delta-method standard errors in parentheses;",
                     "$^{*}$ $p<0.10$, $^{**}$ $p<0.05$, $^{***}$ $p<0.01$."),
        lines = slide_tabular(header, correlations_body, length(technologies))
      )
    }
  }
  tables
}

#' Slide table of the probabilities of the recommendations at the reference
#' vignette, one row per profile of the AMEs, with their Krinsky-Robb standard
#' errors in parentheses, in percent
#'
#' @return a list as the elements of slide_coefficient_tables()
slide_baseline_table <- function(results, labels) {
  outcomes <- results$outcomes
  probabilities <- results$ame_table %>%
    filter(model == "mixed") %>%
    distinct(profile, outcome, p_reference, se_p_reference)
  profiles <- unique(probabilities$profile)
  if (nrow(probabilities) != length(profiles) * length(outcomes)) {
    stop("More than one probability at the reference vignette per profile ",
         "and outcome in ", results$name)
  }
  column_headers <- paste0("\\begin{tabular}[b]{@{}c@{}}",
                           labels$technology_headers[outcomes],
                           "\\end{tabular}")
  header <- paste0(" & ", paste(column_headers, collapse = " & "), " \\\\")
  body <- character(0)
  for (profile_name in profiles) {
    profile_rows <- probabilities %>%
      filter(profile == profile_name)
    # the outcomes in the order of the columns
    profile_rows <- profile_rows[match(outcomes, profile_rows$outcome), ]
    body <- c(body,
              paste0(latex_text(labels$profiles[[profile_name]]), " & ",
                     paste(latex_number(100 * profile_rows$p_reference, 1),
                           collapse = " & "), " \\\\"),
              paste0(" & ", paste0("(", latex_number(100 * profile_rows$se_p_reference,
                                                     1), ")", collapse = " & "),
                     " \\\\"))
  }
  list(
    name = "baseline",
    title = "Probabilities at the reference vignette (%)",
    note = "Krinsky-Robb standard errors in parentheses, in percentage points.",
    lines = slide_tabular(header, body, length(outcomes))
  )
}

#' Slide table of the fit of the mixed logits, one row per specification of
#' `numbers`, in its order; a specification without results gets a row that
#' says so. The lowest AIC and BIC are in bold.
#'
#' @param results_list named list of saved results
#' @param numbers      number of each specification, e.g. "(5)", named by the
#'                     specifications
#' @return character vector, the lines of the tabular
slide_fit_table <- function(results_list, numbers) {
  fit <- tibble()
  for (spec_name in names(results_list)) {
    fit <- bind_rows(fit, results_list[[spec_name]]$fit_statistics %>%
                       filter(model == "mixed") %>%
                       mutate(specification = spec_name))
  }
  header <- paste("Specification & Parameters & Log-likelihood &",
                  "McFadden $R^2$ & AIC & BIC \\\\")
  body <- character(0)
  for (spec_name in names(numbers)) {
    if (!(spec_name %in% fit$specification)) {
      body <- c(body, paste0(numbers[[spec_name]],
                             " & \\multicolumn{5}{l}{\\textit{no results}} \\\\"))
      next
    }
    fit_spec <- fit %>%
      filter(specification == spec_name)
    aic_cell <- latex_number(fit_spec$aic, 1)
    if (fit_spec$aic == min(fit$aic)) aic_cell <- paste0("\\textbf{", aic_cell, "}")
    bic_cell <- latex_number(fit_spec$bic, 1)
    if (fit_spec$bic == min(fit$bic)) bic_cell <- paste0("\\textbf{", bic_cell, "}")
    body <- c(body, paste0(numbers[[spec_name]], " & ", fit_spec$n_parameters,
                           " & ", latex_number(fit_spec$log_likelihood, 1),
                           " & ", latex_number(fit_spec$mcfadden_r2, 3),
                           " & ", aic_cell, " & ", bic_cell, " \\\\"))
  }
  slide_tabular(header, body, 5)
}

#' p-value in a table cell: "<0.001" below 0.001, three decimals otherwise,
#' "--" if missing
p_cell <- function(p) {
  if (is.na(p)) {
    "--"
  } else if (p < 0.001) {
    "$<$0.001"
  } else {
    math_number(p, 3)
  }
}

#' Slide table of the likelihood-ratio tests (lr_tests()), one row per pair
#' of `pairs`, in its order; a test that did not run, because one of the two
#' specifications has no results, gets a row that says so
#'
#' @param pairs        the pairs of the tests (LR_TEST_PAIRS of 02_output.R)
#' @param lr           tibble from lr_tests()
#' @param results_list named list of saved results
#' @param numbers      number of each specification, named by the
#'                     specifications
#' @return character vector, the lines of the tabular
slide_lr_table <- function(pairs, lr, results_list, labels, numbers) {
  restriction_labels <- c(
    regressors = "Added regressors",
    correlations = "Correlations",
    group_covariances = "Group (co)variances"
  )
  header <- paste("Test & Restriction & $\\chi^2$ & df & $p$ & Robust $p$ &",
                  "Mean weight \\\\")
  body <- character(0)
  for (i in seq_len(nrow(pairs))) {
    restricted_name <- pairs$restricted[i]
    unrestricted_name <- pairs$unrestricted[i]
    has_results <- restricted_name %in% names(results_list) &&
      unrestricted_name %in% names(results_list)
    # What the test restricts; for added regressors, which ones
    types <- strsplit(pairs$restrictions[i], " + ", fixed = TRUE)[[1]]
    restriction_items <- character(0)
    for (type in types) {
      item <- restriction_labels[[type]]
      if (type == "regressors" && has_results) {
        added <- setdiff(results_list[[unrestricted_name]]$specification$regressors,
                         results_list[[restricted_name]]$specification$regressors)
        added_labels <- character(0)
        for (regressor in added) {
          added_labels <- c(added_labels, regressor_label(
            regressor, labels, names(results_list[[unrestricted_name]]$attribute_levels)
          ))
        }
        item <- paste(unique(added_labels), collapse = ", ")
      }
      restriction_items <- c(restriction_items, item)
    }
    test_cells <- paste0(numbers[[restricted_name]], " vs ",
                         numbers[[unrestricted_name]], " & ",
                         latex_text(paste(restriction_items, collapse = "; ")))
    test <- lr %>%
      filter(restricted == restricted_name, unrestricted == unrestricted_name)
    if (nrow(test) == 0) {
      body <- c(body, paste0(test_cells,
                             " & \\multicolumn{5}{l}{\\textit{not run: no results}} \\\\"))
    } else if (test$lr < 0) {
      body <- c(body, paste0(test_cells, " & ", math_number(test$lr, 1),
                             "$^{\\dagger}$ & ", test$df,
                             " & -- & -- & -- \\\\"))
    } else {
      body <- c(body, paste0(test_cells, " & ", math_number(test$lr, 1), " & ",
                             test$df, " & ", p_cell(test$p), " & ",
                             p_cell(test$p_robust), " & ",
                             latex_number(test$mean_weight, 2), " \\\\"))
    }
  }
  c("\\setlength{\\tabcolsep}{4pt}",
    "\\begin{tabular}{llrrrrr}",
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}")
}

#' Slide version of an AME figure (plot_ame()): the variations with their
#' short labels, no title, the probabilities at the reference vignette as
#' subtitle, saved at slide size with cairo_pdf (R's PDF device through the
#' Cairo library), which embeds the fonts and prints a hyphen as a hyphen
#'
#' @param plot_data        as for plot_ame(), with the column vignette
#' @param file_path        where to save the PDF
#' @param variation_labels vignette -> short label, in the order of the rows
#'                         from the top
#' @param size             c(width, height) in inches
#' @param base_size        font size in pt
#' @param group_labels     as in baseline_subtitle()
save_slide_ame_figure <- function(plot_data, file_path, variation_labels, size,
                                  base_size, group_labels = NULL) {
  slide_plot_data <- plot_data %>%
    mutate(variation = factor(variation_labels[vignette],
                              levels = rev(variation_labels)))
  slide_plot <- plot_ame(slide_plot_data, NULL,
                         baseline_subtitle(slide_plot_data, NULL, group_labels),
                         base_size = base_size)
  ggsave(file_path, slide_plot, width = size[["width"]],
         height = size[["height"]], device = cairo_pdf)
}

#' One beamer frame of the slides
#'
#' @param title   the frame title (LaTeX)
#' @param content the lines inside the frame
#' @param note    a note at the bottom of the frame (LaTeX), or NULL
#' @return character vector, the lines of the frame and an empty line
slide_frame <- function(title, content, note = NULL) {
  lines <- c(paste0("\\begin{frame}{", title, "}"), content)
  if (!is.null(note)) {
    lines <- c(lines, paste0("  \\slidenote{", note, "}"))
  }
  c(lines, "\\end{frame}", "")
}

#' Numbers for the text of the slides as LaTeX macros, e.g.
#' \newcommand{\nExperts}{1,929}
#'
#' @param values named vector of the formatted numbers; the names are the
#'               macro names, letters only (LaTeX command names have no digits)
#' @return character vector, one line per macro
numbers_tex <- function(values) {
  if (!all(grepl("^[A-Za-z]+$", names(values)))) {
    stop("Macro names must be letters only: ", paste(names(values), collapse = ", "))
  }
  paste0("\\newcommand{\\", names(values), "}{", values, "}")
}
