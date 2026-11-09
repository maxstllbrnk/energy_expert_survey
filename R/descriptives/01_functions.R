################################################################################
# 01_functions.R — statistics, tables and figures of the English descriptives
# Inputs : none; sourced by R/run_descriptives.R
# Outputs: none
# Checks : inside the functions: no statistic rests on fewer than MIN_N
#          answers; the answer options of a table are answers of the data;
#          a multiple-choice answer is "Ja" or "Nicht Gewählt"
################################################################################
# Tables are complete LaTeX floats from table_float() and figures vector PDFs
# from save_figure_pdf(), both in R/analysis/00_functions_output.R, so that
# these exhibits look like those of the mixed logit models in the same deck.
# Numbers become LaTeX through latex_number(), labels through latex_text().

# 1. Statistics ----------------------------------------------------------------

#' Summary statistics, one row per row of a table (rows of section 3 of
#' 00_settings.R). A statistic resting on fewer than MIN_N answers is NA.
#'
#' @param data one row per expert
#' @param rows panel, variable, kind, level, label, digits
#' @return rows, plus n (answers), mean (for a share: the share in percent),
#'   sd, p25, median, p75; NA where the kind has no such statistic
summary_statistics <- function(data, rows) {
  stats <- tibble()
  for (k in seq_len(nrow(rows))) {
    row_stats <- tibble(n = NA_real_, mean = NA_real_, sd = NA_real_,
                        p25 = NA_real_, median = NA_real_, p75 = NA_real_)
    if (rows$kind[k] != "heading") {
      if (!(rows$variable[k] %in% names(data))) {
        stop("Not in the data: ", rows$variable[k])
      }
      values <- data[[rows$variable[k]]]
      answered <- values[!is.na(values)]
      row_stats$n <- length(answered)
      if (length(answered) >= MIN_N) {
        if (rows$kind[k] %in% c("share", "level")) {
          row_stats$mean <- 100 * mean(as.character(answered) == rows$level[k])
        } else {
          row_stats$mean <- mean(answered)
          row_stats$sd <- sd(answered)
          row_stats$p25 <- quantile(answered, 0.25, names = FALSE)
          row_stats$median <- median(answered)
          row_stats$p75 <- quantile(answered, 0.75, names = FALSE)
        }
      }
    }
    stats <- bind_rows(stats, row_stats)
  }
  bind_cols(rows, stats)
}

#' Stops if an answer option of a table is not an answer of the data: a
#' misspelt option would show 0 percent. With complete = TRUE, also if the
#' data have answers that the table leaves out, so that the shares of a
#' question with several options sum to 100.
#'
#' @param data     one row per expert, the pooled sample
#' @param rows     rows of a table (section 3 of 00_settings.R)
#' @param complete TRUE if the table shows every answer of its questions
check_share_rows <- function(data, rows, complete) {
  share_rows <- rows %>%
    filter(kind %in% c("share", "level"))
  for (variable_name in unique(share_rows$variable)) {
    values <- data[[variable_name]]
    answers <- unique(as.character(values[!is.na(values)]))
    # A factor knows its answers even if nobody gave one of them
    if (is.factor(values)) {
      possible_answers <- levels(values)
    } else {
      possible_answers <- answers
    }
    listed <- share_rows$level[share_rows$variable == variable_name]
    unknown <- setdiff(listed, possible_answers)
    if (length(unknown) > 0) {
      stop(variable_name, ": the table shows answers that are not in the ",
           "data: ", paste(unknown, collapse = ", "))
    }
    not_listed <- setdiff(answers, listed)
    if (complete && length(listed) > 1 && length(not_listed) > 0) {
      stop(variable_name, ": answers in the data that the table leaves out: ",
           paste(not_listed, collapse = ", "))
    }
  }
}

#' Share of experts who ticked each option of a "tick all that apply"
#' question, by group and for all experts, among those who answered it
#'
#' @param data      one row per expert, with group
#' @param variables the options, one column each: "Ja" if ticked,
#'                  "Nicht Gewählt" if not, NA if not answered
#' @return one row per group x variable: group (the three and ALL_EXPERTS),
#'   variable, n (experts who answered), share (0 to 1)
multiple_choice_shares <- function(data, variables) {
  answers <- data %>%
    select(resp_uid, group, all_of(variables))
  for (variable in variables) {
    answers[[variable]] <- as.character(answers[[variable]])
  }
  answers <- answers %>%
    mutate(group = as.character(group)) %>%
    pivot_longer(all_of(variables), names_to = "variable",
                 values_to = "answer") %>%
    filter(!is.na(answer))
  unknown_answers <- setdiff(unique(answers$answer), c("Ja", "Nicht Gewählt"))
  if (length(unknown_answers) > 0) {
    stop("Answers other than Ja and Nicht Gewählt: ",
         paste(unknown_answers, collapse = ", "))
  }
  answers_all_experts <- answers %>%
    mutate(group = ALL_EXPERTS)
  bind_rows(answers, answers_all_experts) %>%
    group_by(group, variable) %>%
    summarise(n = n(), share = mean(answer == "Ja"), .groups = "drop")
}

# 2. Formatting ----------------------------------------------------------------

#' Statistics as cells of a table: latex_number(), and years without a
#' thousands separator; "--" for a statistic that rests on fewer than MIN_N
#' answers
#'
#' @param x      numbers
#' @param digits decimals
#' @param kind   "year" for years, anything else for other numbers
statistic_text <- function(x, digits, kind = "number") {
  if (kind == "year") {
    text <- formatC(round(x), format = "f", digits = 0, big.mark = "")
  } else {
    text <- latex_number(x, digits)
  }
  text[is.na(x)] <- "--"
  text
}

#' The note on "--", for a table with a cell "--"; nothing otherwise
#'
#' @param cells the cells of the table: a data frame of LaTeX strings, or a
#'              vector of them
#' @return " --: fewer than MIN_N answers." or ""
dash_note <- function(cells) {
  # unlist(): all cells of the table as one vector
  if (any(unlist(cells) == "--", na.rm = TRUE)) {
    paste0(" --: fewer than ", MIN_N, " answers.")
  } else {
    ""
  }
}

#' A share (0 to 1) as text in a figure, e.g. "45%"
percent_label <- function(share, digits = 0) {
  paste0(formatC(100 * share, format = "f", digits = digits), "%")
}

#' One line of a table body: its cells, LaTeX, the row label first
table_line <- function(cells) {
  paste0(paste(cells, collapse = " & "), " \\\\")
}

#' Lines of a table body, one per row of a data frame of LaTeX strings whose
#' first column holds the row labels
cell_rows <- function(cells) {
  lines <- character(nrow(cells))
  for (k in seq_len(nrow(cells))) {
    lines[k] <- table_line(unlist(cells[k, ]))
  }
  lines
}

# 3. Tables --------------------------------------------------------------------

#' The rows of a table of summary statistics, in panels: N, mean, SD and
#' quartiles of a continuous answer, N and the share of an answer option
#'
#' @param stats from summary_statistics()
#' @return list: body (the lines of the table) and cells (all of their cells,
#'   for dash_note())
summary_rows_tex <- function(stats) {
  body <- character(0)
  all_cells <- character(0)
  panels <- unique(stats$panel)
  for (p in seq_along(panels)) {
    if (p > 1) body <- c(body, PANEL_SPACE)
    # A panel "" has no title line, only the space above it
    if (panels[p] != "") {
      body <- c(body, panel_row(latex_text(panels[p]), 7))
    }
    panel_stats <- stats %>%
      filter(panel == panels[p])
    for (k in seq_len(nrow(panel_stats))) {
      row <- panel_stats[k, ]
      label_text <- latex_text(row$label)
      if (row$kind == "heading") {
        cells <- c(label_text, rep("", 6))
      } else if (row$kind %in% c("share", "level")) {
        if (row$kind == "level") label_text <- paste0("\\quad ", label_text)
        cells <- c(label_text, latex_number(row$n, 0),
                   statistic_text(row$mean, row$digits), rep("", 4))
      } else {
        cells <- c(label_text, latex_number(row$n, 0),
                   statistic_text(c(row$mean, row$sd, row$p25, row$median,
                                    row$p75), row$digits, row$kind))
      }
      body <- c(body, table_line(cells))
      all_cells <- c(all_cells, cells)
    }
  }
  list(body = body, cells = all_cells)
}

#' The answer options of two questions with a heading, side by side, each
#' with its N in the heading: a table of its own across the seven columns of
#' a summary table
#'
#' @param stats from summary_statistics(): two questions with a heading and
#'              answer options (level), the left one first
#' @return list: lines (rows of the summary table) and cells (the shares,
#'   for dash_note())
option_lists_tex <- function(stats) {
  # Each question: its heading row, and the rows of its answer options below
  questions <- unique(stats$variable[stats$kind == "level"])
  blocks <- list()
  for (question in questions) {
    option_rows <- which(stats$variable == question & stats$kind == "level")
    heading_row <- option_rows[1] - 1
    if (stats$kind[heading_row] != "heading") {
      stop("No heading above the answer options of ", question)
    }
    blocks[[question]] <- list(
      heading = paste0("\\textit{", latex_text(stats$label[heading_row]),
                       ", N = ", latex_number(stats$n[option_rows[1]], 0),
                       "}"),
      labels = paste0("\\quad ", latex_text(stats$label[option_rows])),
      shares = statistic_text(stats$mean[option_rows],
                              stats$digits[option_rows[1]])
    )
  }
  # The shorter list of answer options is filled up with empty cells
  n_lines <- max(length(blocks[[1]]$labels), length(blocks[[2]]$labels))
  for (b in 1:2) {
    n_missing <- n_lines - length(blocks[[b]]$labels)
    blocks[[b]]$labels <- c(blocks[[b]]$labels, rep("", n_missing))
    blocks[[b]]$shares <- c(blocks[[b]]$shares, rep("", n_missing))
  }

  # The two lists in a table of their own, across all columns
  option_lines <- table_line(c(blocks[[1]]$heading, "",
                               blocks[[2]]$heading, ""))
  for (k in seq_len(n_lines)) {
    option_lines <- c(option_lines, table_line(c(
      blocks[[1]]$labels[k], blocks[[1]]$shares[k],
      blocks[[2]]$labels[k], blocks[[2]]$shares[k]
    )))
  }
  option_table <- c(
    paste0("\\multicolumn{7}{l}{\\begin{tabular}{@{}p{",
           OPTION_LABEL_WIDTHS[["left"]], "}r@{\\hspace{2em}}p{",
           OPTION_LABEL_WIDTHS[["right"]], "}r@{}}"),
    option_lines,
    "\\end{tabular}} \\\\"
  )
  list(lines = option_table,
       cells = c(blocks[[1]]$shares, blocks[[2]]$shares))
}

#' A table of summary statistics made to fit a slide: the rows of one line
#' (mean, year, share) in panels, with N, mean, SD and quartiles; below them,
#' if the table has any, the answer options of two questions side by side
#' (option_lists_tex()); the number of experts at the end
#'
#' @param stats     from summary_statistics(): rows of one line, and none or
#'                  two questions with a heading and answer options (level)
#' @param n_experts experts in the sample, for the last row
#' @param caption   plain text
#' @param label     e.g. "tab:desc_experts"
#' @param notes     LaTeX; the note on "--" is added where a statistic is
#'                  left out
#' @return lines of the .tex file
summary_table_tex <- function(stats, n_experts, caption, label, notes) {
  upper <- summary_rows_tex(stats %>%
                              filter(!(kind %in% c("heading", "level"))))
  body <- upper$body
  all_cells <- upper$cells
  n_questions <- length(unique(stats$variable[stats$kind == "level"]))
  if (n_questions == 2) {
    options <- option_lists_tex(stats)
    body <- c(body, PANEL_SPACE, options$lines)
    all_cells <- c(all_cells, options$cells)
  } else if (n_questions != 0) {
    stop("summary_table_tex() shows the answer options of two questions ",
         "or of none")
  }
  body <- c(body, "\\midrule",
            table_line(c("Experts", latex_number(n_experts, 0), rep("", 5))))
  table_float(
    column_spec = paste0("p{", LABEL_WIDTH_SUMMARY, "}cccccc"),
    header = " & N & Mean & SD & P25 & Median & P75 \\\\",
    body = body,
    caption = latex_text(caption),
    label = label,
    notes = paste0(notes, dash_note(all_cells)),
    font_size = TABLE_FONT_SIZE
  )
}

#' A table with one column per group and all experts: the row labels and the
#' cells as LaTeX, optionally in panels, and optionally rows below a rule
#' (the number of experts)
#'
#' @param cells       data frame of LaTeX strings: panel (or NA), label, then
#'                    one column per group in the order of the header
#' @param header      lines of the header
#' @param caption     plain text
#' @param label,notes as in table_float()
#' @param cells_below rows below a rule, as `cells`, or NULL
#' @return lines of the .tex file
group_table_tex <- function(cells, header, caption, label, notes,
                            cells_below = NULL) {
  n_columns <- ncol(cells) - 1
  body <- character(0)
  panels <- unique(cells$panel)
  for (p in seq_along(panels)) {
    if (!is.na(panels[p])) {
      if (p > 1) body <- c(body, PANEL_SPACE)
      # A panel "" has no title line, only the space above it
      if (panels[p] != "") {
        body <- c(body, panel_row(latex_text(panels[p]), n_columns))
      }
      panel_cells <- cells %>%
        filter(panel == panels[p])
    } else {
      panel_cells <- cells %>%
        filter(is.na(panel))
    }
    body <- c(body, cell_rows(panel_cells %>% select(-panel)))
  }
  if (!is.null(cells_below)) {
    body <- c(body, "\\midrule", cell_rows(cells_below %>% select(-panel)))
  }
  # strrep(): the text repeated, e.g. "cccc"
  table_float(
    column_spec = paste0("p{", LABEL_WIDTH_GROUPS, "}",
                         strrep("c", n_columns - 1)),
    header = header,
    body = body,
    caption = latex_text(caption),
    label = label,
    notes = notes,
    font_size = TABLE_FONT_SIZE
  )
}

#' Cells of a table with one column per group and all experts
#'
#' @param data   one row per item x group: item, group, text (the cell, LaTeX)
#' @param items  the items in the order of the rows
#' @param labels the row labels, LaTeX, one per item
#' @param panel  the panel of the rows, or NA
#' @return data frame: panel, label, one column per group and ALL_EXPERTS
group_cells <- function(data, items, labels, panel = NA_character_) {
  cells <- tibble(panel = rep(panel, length(items)), label = labels)
  for (group_name in GROUP_COLUMNS) {
    texts <- character(length(items))
    for (i in seq_along(items)) {
      text <- data$text[data$item == items[i] & data$group == group_name]
      if (length(text) != 1) {
        stop("Not one cell for ", items[i], " and ", group_name)
      }
      texts[i] <- text
    }
    cells[[group_name]] <- texts
  }
  cells
}

#' The header of a table with a column per group and all experts, the group
#' names over two lines
group_header <- function(first_cell = "") {
  cells <- character(length(GROUP_COLUMNS))
  for (g in seq_along(GROUP_COLUMNS)) {
    cells[g] <- header_cell(GROUP_HEADERS[[GROUP_COLUMNS[g]]])
  }
  paste0(first_cell, " & ", paste(cells, collapse = " & "), " \\\\")
}

# 4. Figures -------------------------------------------------------------------

#' Shares as horizontal bars: a panel per group and a row per item, in the
#' same order in every panel; with a series (the arm), a bar per series in
#' each row. The barriers, the criteria of the customers and the
#' recommendations.
#'
#' @param plot_data      one row per item x group (x series), each of them:
#'                       item (factor; its first level at the top), group
#'                       (factor of group names), share (0 to 1; NA where it
#'                       is left out, which draws no bar), and series
#'                       (factor) for several bars per row
#' @param x_title        title of the axis of the shares, with "(%)": the
#'                       axis shows 25, 50 and 75
#' @param series_colours colours named by the levels of series; NULL for one
#'                       bar per row, in the first series colour
#' @return ggplot
plot_group_shares <- function(plot_data, x_title, series_colours = NULL) {
  # Every combination exactly once: without one, the other bar of its row
  # would move into its place. nlevels(): the number of levels of a factor;
  # anyDuplicated(): the position of the first repeated row, 0 if none.
  keys <- c("item", "group")
  n_combinations <- nlevels(plot_data$item) * nlevels(plot_data$group)
  if (!is.null(series_colours)) {
    keys <- c(keys, "series")
    n_combinations <- n_combinations * nlevels(plot_data$series)
  }
  if (nrow(plot_data) != n_combinations ||
      anyDuplicated(plot_data[keys]) > 0) {
    stop("The data of the figure need exactly one row per ",
         paste(keys, collapse = " x "))
  }
  # A group without a single share would show an empty panel
  has_share <- !is.na(plot_data$share)
  shown_groups <- unique(as.character(plot_data$group[has_share]))
  empty_groups <- setdiff(levels(plot_data$group), shown_groups)
  if (length(empty_groups) > 0) {
    stop("No share of at least ", MIN_N, " answers for: ",
         paste(empty_groups, collapse = ", "))
  }

  # The vertical axis puts the first level at the bottom: reversed, so that
  # the first item is at the top
  plot_data <- plot_data %>%
    mutate(item = factor(item, levels = rev(levels(item))))
  # na.rm = TRUE: a share left out (NA) draws no bar, without a warning, and
  # keeps its place in the row
  if (is.null(series_colours)) {
    plot <- ggplot(plot_data, aes(x = share, y = item)) +
      geom_col(width = 0.6, fill = ZEW_LIME, na.rm = TRUE)
  } else {
    series_levels <- levels(plot_data$series)
    missing_series <- setdiff(series_levels, names(series_colours))
    if (length(missing_series) > 0) {
      stop("No colour for: ", paste(missing_series, collapse = ", "))
    }
    # position_dodge(): the bars of a row one above the other (orientation =
    # "y"), the first series at the top (reverse = TRUE)
    plot <- ggplot(plot_data, aes(x = share, y = item, fill = series)) +
      geom_col(width = 0.7, na.rm = TRUE,
               position = position_dodge(width = 0.8, orientation = "y",
                                         reverse = TRUE)) +
      scale_fill_manual(values = series_colours[series_levels],
                        breaks = series_levels)
  }
  plot +
    facet_wrap(~group, nrow = 1) +
    # The same axis in every panel, from 0 to 100%, its labels without "%",
    # which is in the title: three labels with "%" do not fit a narrow
    # panel. No breaks at 0 and 100%, where the labels of two panels would
    # touch.
    scale_x_continuous(limits = c(0, 1), breaks = c(0.25, 0.5, 0.75),
                       labels = c("25", "50", "75"), expand = c(0, 0)) +
    labs(x = x_title, y = NULL) +
    theme_rcl(FONT_SIZE) +
    # legend.location = "plot": the legend is centred under the whole figure,
    # not under the panels, which long labels push to the right
    theme(panel.grid.major.y = element_blank(),
          panel.spacing.x = unit(10, "pt"),
          legend.location = "plot",
          legend.key.size = unit(LEGEND_KEY_SIZE, "pt"))
}
