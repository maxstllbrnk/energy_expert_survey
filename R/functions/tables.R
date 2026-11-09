#' Shared table writers: every table in the paper goes through these functions.
#' Core file of the economics-r skill: identical in every project, no project
#' settings. Update it by copying a newer version from the skill.
#'
#' Output is a LaTeX fragment ready for \input{}: a booktabs tabular wrapped in
#' threeparttable with its notes. The document adds the float, caption and label.
#' Labels and notes are written in LaTeX: escape %, & and _ yourself (\%, \&, \_).
#' Needs: dplyr, kableExtra, modelsummary.


# 1. Number formatting ---------------------------------------------------------

#' Format numbers with a fixed number of decimals and a thousands separator.
#' @param x numeric vector
#' @param digits number of decimals
#' @return character vector; NA becomes "--", negative numbers get a LaTeX minus
fmt_num <- function(x, digits = 2) {
  # values that round to zero are set to exactly 0, so they don't print as "-0.00"
  x[!is.na(x) & abs(x) < 0.5 * 10^(-digits)] <- 0
  out <- formatC(x, format = "f", digits = digits, big.mark = ",")
  out <- sub("^-", "$-$", out)
  out[is.na(x)] <- "--"
  out
}

#' Format counts: no decimals, thousands separator.
fmt_int <- function(x) {
  fmt_num(x, digits = 0)
}

#' Format a share as a percentage: 0.506 -> "50.6\%".
#' @param x numeric vector of shares between 0 and 1
#' @param digits number of decimals of the percentage
fmt_pct <- function(x, digits = 1) {
  out <- paste0(fmt_num(100 * x, digits), "\\%")
  out[is.na(x)] <- "--"
  out
}


# 2. Writing -------------------------------------------------------------------

#' Wrap a kableExtra table and its notes in threeparttable and write a .tex file.
#' The notes are one paragraph in footnote size, as wide as the table, starting
#' with "Notes:" and set ragged-right, so narrow tables get no wide word gaps.
#' @param tab kableExtra object (from kbl())
#' @param notes table notes in LaTeX
#' @param file path of the .tex file, e.g. file.path(dir_tables, "tab_x.tex")
#' @return the lines of the .tex file, invisibly
save_table <- function(tab, notes, file) {
  if (!grepl("\\.tex$", file)) stop("Table files must end in .tex: ", file)

  # The notes block is written here rather than with kableExtra's footnote():
  # footnote() puts "Notes:" in a list item of its own, which leaves a gap
  # before the text, and it drops single backslashes such as the one in \%.
  lines <- c(
    "\\begin{threeparttable}",
    trimws(as.character(tab)),
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize\\raggedright",
    paste("\\item \\textit{Notes:}", notes),
    "\\end{tablenotes}",
    "\\end{threeparttable}"
  )
  writeLines(lines, file)
  invisible(lines)
}

#' Write named values as LaTeX macros, so numbers in the text come from code.
#' @param values named character vector, already formatted,
#'   e.g. c(nHouseholds = fmt_int(27867)) -> \newcommand{\nHouseholds}{27,867}
#' @param file path of the .tex file
write_macros <- function(values, file) {
  lines <- paste0("\\newcommand{\\", names(values), "}{", values, "}")
  writeLines(lines, file)
}


# 3. Tables built by hand ------------------------------------------------------

#' Write a table whose cells are already formatted (transition matrices,
#' sample construction, anything that is not a summary or regression table).
#' @param tab data frame of character columns; the first column holds row labels
#' @param file path of the .tex file
#' @param notes table notes in LaTeX
#' @param col_names column headers (default: the names of `tab`)
#' @param align column alignment, e.g. c("l", "r", "r"); default: first "l", rest "r"
#' @param header optional header row above the columns, e.g. c(" " = 1, "Destination" = 2)
#' @param panels optional row panels, e.g. c("Renters" = 3, "Owners" = 3)
table_custom <- function(tab, file, notes, col_names = names(tab), align = NULL,
                         header = NULL, panels = NULL) {
  if (is.null(align)) align <- c("l", rep("r", ncol(tab) - 1))

  out <- kbl(tab, format = "latex", booktabs = TRUE, escape = FALSE,
             linesep = "", align = align, col.names = col_names)
  if (!is.null(header)) out <- add_header_above(out, header, escape = FALSE)
  if (!is.null(panels)) out <- pack_rows(out, index = panels, escape = FALSE)

  save_table(out, notes, file)
}


# 4. Summary statistics --------------------------------------------------------

#' Summary-statistics table, one block of columns per group.
#'
#' Rate rows (kind = "pct") show the share only: a standard deviation or
#' quartile of a 0/1 variable says nothing the mean doesn't. Empty cells mean
#' "not reported"; "--" means "cannot be computed" (no observations).
#' Minimum and maximum are available but off by default: with confidential
#' microdata they report the values of single households.
#'
#' @param data data frame, one row per observation
#' @param vars variable dictionary with columns var, label, digits, kind
#'   ("num" or "pct") and optionally panel (rows of a panel must be adjacent)
#' @param file path of the .tex file
#' @param notes table notes in LaTeX
#' @param by optional name of a grouping column; one column block per value
#' @param stats statistics to report, from n, mean, sd, min, p25, p50, p75, max
#' @param id optional name of the unit identifier; adds a row with distinct units
#' @param id_label row label for the distinct units
#' @return the formatted table as a data frame, invisibly
table_summary <- function(data, vars, file, notes, by = NULL,
                          stats = c("n", "mean", "sd", "p25", "p50", "p75"),
                          id = NULL, id_label = "Households") {
  stat_names <- c(n = "N", mean = "Mean", sd = "SD", min = "Min", p25 = "P25",
                  p50 = "Median", p75 = "P75", max = "Max")

  # One data set per column block: the full sample, or one per value of `by`
  if (is.null(by)) {
    groups <- list(All = data)
  } else {
    if (any(is.na(data[[by]]))) stop("The grouping variable ", by, " has missing values")
    groups <- split(data, data[[by]])
  }

  # Footer rows: observations and, optionally, distinct units
  footer_labels <- "Observations"
  if (!is.null(id)) footer_labels <- c(footer_labels, id_label)

  blocks <- list()
  for (g in names(groups)) {
    d <- groups[[g]]
    block <- matrix("", nrow = nrow(vars) + length(footer_labels), ncol = length(stats))

    for (i in seq_len(nrow(vars))) {
      x <- d[[vars$var[i]]]
      x <- x[!is.na(x)]
      if (length(x) == 0) {
        block[i, ] <- "--"
        next
      }
      values <- c(
        n = length(x), mean = mean(x), sd = sd(x), min = min(x),
        p25 = unname(quantile(x, 0.25)), p50 = median(x),
        p75 = unname(quantile(x, 0.75)), max = max(x)
      )
      for (j in seq_along(stats)) {
        s <- stats[j]
        if (s == "n") {
          block[i, j] <- fmt_int(values[["n"]])
        } else if (vars$kind[i] == "pct") {
          if (s == "mean") block[i, j] <- fmt_pct(values[["mean"]], vars$digits[i])
        } else {
          block[i, j] <- fmt_num(values[[s]], vars$digits[i])
        }
      }
    }

    # Footer counts go in the first statistic column
    block[nrow(vars) + 1, 1] <- fmt_int(nrow(d))
    if (!is.null(id)) block[nrow(vars) + 2, 1] <- fmt_int(n_distinct(d[[id]]))

    blocks[[g]] <- block
  }

  tab <- data.frame(Variable = c(vars$label, footer_labels), do.call(cbind, blocks))
  col_names <- c("Variable", rep(stat_names[stats], length(groups)))
  # Readable names for the returned data frame, e.g. "Owners: Mean"
  names(tab) <- c("Variable", paste0(rep(names(groups), each = length(stats)), ": ",
                                     stat_names[stats]))

  out <- kbl(tab, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
             align = c("l", rep("r", ncol(tab) - 1)), col.names = col_names)

  if (!is.null(by)) {
    header <- c(1, rep(length(stats), length(groups)))
    names(header) <- c(" ", names(groups))
    out <- add_header_above(out, header, escape = FALSE)
  }

  if ("panel" %in% names(vars)) {
    # rle() counts the adjacent rows of each panel: c("A", "A", "B") -> A = 2, B = 1
    runs <- rle(vars$panel)
    if (any(duplicated(runs$values))) stop("Rows of a panel must be adjacent in `vars`")
    panels <- c(setNames(runs$lengths, runs$values), Sample = length(footer_labels))
    out <- pack_rows(out, index = panels, escape = FALSE)
  } else {
    out <- row_spec(out, nrow(vars), extra_latex_after = "\\midrule")
  }

  save_table(out, notes, file)
  invisible(tab)
}


# 5. Regression tables ---------------------------------------------------------

#' Regression table from fixest models, glm models or marginaleffects output.
#'
#' Coefficients with standard errors in parentheses below; stars at 10, 5 and
#' 1 percent unless the project's journal forbids them. Fixed-effect rows are read from the models (absorbed after `|`),
#' so the table cannot claim fixed effects a model doesn't have. For fixed
#' effects entered as factor() terms, as in probits whose marginal effects need
#' standard errors, pass the rows through `extra_rows` instead of `fe_labels`.
#'
#' @param models named list of models; names become column headers, e.g. "(1)"
#' @param file path of the .tex file
#' @param coef_labels named vector: coefficient name -> label; sets row order and
#'   drops every coefficient not listed
#' @param notes table notes in LaTeX: sample, standard errors, clustering; the
#'   significance legend is added automatically
#' @param gof_labels statistics below the coefficients: modelsummary name -> label
#' @param fe_labels named vector: fixed-effect variable -> label, e.g. c(state = "State")
#' @param extra_rows optional data frame of extra rows below the statistics: a
#'   label column followed by one character column per model
#' @param header optional header row, e.g. c(" " = 1, "Log price" = 2, "Log rent" = 2)
#' @param digits decimals for coefficients and statistics
#' @param stars TRUE for significance stars and their legend; FALSE where the
#'   project profile says the journal doesn't allow them
#' @return the formatted table as a data frame, invisibly
table_regression <- function(models, file, coef_labels, notes,
                             gof_labels = c(nobs = "Observations",
                                            r.squared = "R$^2$",
                                            r2.within = "Within R$^2$"),
                             fe_labels = NULL, extra_rows = NULL, header = NULL,
                             digits = 3, stars = TRUE) {
  if (is.null(names(models))) stop("Name the models, e.g. list(\"(1)\" = m1)")

  # modelsummary builds the cells; we keep full control of the LaTeX below.
  # gof_map as a data frame: raw name, label in the table, decimals.
  fe_raw <- character(0)
  fe_clean <- character(0)
  if (!is.null(fe_labels)) {
    fe_raw <- paste0("FE: ", names(fe_labels))
    fe_clean <- paste(fe_labels, "FE")
  }
  gof_map <- data.frame(
    raw = c(names(gof_labels), fe_raw),
    clean = c(unname(gof_labels), fe_clean),
    fmt = digits
  )

  star_levels <- FALSE
  if (stars) star_levels <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)

  cells <- modelsummary(
    models,
    output = "data.frame",
    fmt = digits,
    coef_map = coef_labels,
    gof_map = gof_map,
    stars = star_levels
  )

  model_cols <- names(models)
  is_estimate <- cells$part == "estimates"
  is_fe <- cells$term %in% fe_clean

  for (col in model_cols) {
    x <- cells[[col]]
    # an estimate that rounds to zero loses its sign: -0.000 -> 0.000
    x[is_estimate] <- sub("^-(0\\.0+)(\\**)$", "\\1\\2", x[is_estimate])
    # stars as superscripts and a proper minus sign: -0.391*** -> $-$0.391$^{***}$
    x[is_estimate] <- sub("^-", "$-$", x[is_estimate])
    x[is_estimate] <- sub("(\\*+)$", "$^{\\1}$", x[is_estimate])
    # fixed effects: modelsummary marks them "X"
    x[is_fe] <- ifelse(x[is_fe] == "X", "Yes", "No")
    cells[[col]] <- x
  }

  # Observations with a thousands separator
  if ("nobs" %in% names(gof_labels)) {
    is_nobs <- cells$term == gof_labels[["nobs"]]
    for (col in model_cols) {
      cells[is_nobs, col] <- fmt_int(as.numeric(cells[is_nobs, col]))
    }
  }

  # Standard-error rows have no label
  cells$term[cells$statistic == "std.error"] <- ""

  tab <- cells[, c("term", model_cols)]
  if (!is.null(extra_rows)) {
    names(extra_rows) <- names(tab)
    tab <- rbind(tab, extra_rows)
  }

  out <- kbl(tab, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
             align = c("l", rep("c", length(model_cols))),
             col.names = c("", model_cols))
  out <- row_spec(out, sum(is_estimate), extra_latex_after = "\\midrule")
  if (!is.null(header)) out <- add_header_above(out, header, escape = FALSE)

  if (stars) {
    # braces around p<0.10 stop LaTeX from breaking the line after "<"
    notes <- paste(notes, "$^{*}\\,{p<0.10}$, $^{**}\\,{p<0.05}$, $^{***}\\,{p<0.01}$.")
  }
  save_table(out, notes, file)
  invisible(tab)
}
