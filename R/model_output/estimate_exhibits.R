################################################################################
# estimate_exhibits.R — what the exhibits of the models share: tables of
#                       estimates with their standard errors below, figures of
#                       estimates with their intervals, and the text of notes
# Inputs : the cells of collect_results(), already labelled, with the minimum
#          cell applied
# Outputs: none; the sections write the files
# Checks : a cell's status decides what it shows
################################################################################
# Every cell has a status, set once in collect_results():
#   "shown"           its numbers are shown
#   "too few"         it rests on fewer than MIN_N experts: "--", no numbers
#   "not applicable"  the effect leaves out every unit there: blank
# so that no table or figure can show a cell that the minimum cell hides.

# 1. Tables of estimates -------------------------------------------------------

#' Stars for p-values below 0.10, 0.05 and 0.01, as table_regression() sets them;
#' none for NA
star_text <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.10 ~ "$^{*}$",
    TRUE ~ ""
  )
}

#' The legend of the stars, for the end of a table's notes
STARS_NOTE <- "$^{*}\\,{p<0.10}$, $^{**}\\,{p<0.05}$, $^{***}\\,{p<0.01}$."

#' The panels of the tables of coefficients, in this order; the panels of the
#' terms are those of TERM_LABELS
COEFFICIENT_PANELS <- c("Constants", "The household", "The expert",
                        "Prices and expectations",
                        "One coefficient for all technologies",
                        "Standard deviation of the random constant")

#' A table of estimates: per row a line of estimates and a line below with their
#' standard errors in parentheses, one column per column. Through table_custom(),
#' so that it looks like every other table. A row whose cells all do not apply is
#' left out.
#'
#' @param cells        tibble, one row per cell: row_key (factor: the rows in
#'                     their order), row_label, panel (plain text; the rows of a
#'                     panel adjacent), column (factor: the columns in their
#'                     order), estimate, se, p (NA: no star), status
#' @param file         path of the .tex file
#' @param notes        table notes, plain text
#' @param digits       decimals of the estimates and standard errors
#' @param stars        TRUE: stars from p, and their legend in the notes
#' @param column_names the headers of the columns, plain text, in the order of
#'                     the levels of column; NULL: the levels
#' @param header       optional header above the columns, as in table_custom(),
#'                     plain text names
#' @param row_info     optional tibble: row_key, value (text), shown in a last
#'                     column on the line of the estimates
#' @param row_info_title the header of that column
#' @return TRUE if the table was written, FALSE if no cell is shown
table_estimates <- function(cells, file, notes, digits = 1, stars = TRUE,
                            column_names = NULL, header = NULL, row_info = NULL,
                            row_info_title = "") {
  if (!any(cells$status == "shown")) {
    return(FALSE)
  }
  # The rows with at least one cell that applies, in their order
  applying <- cells %>%
    filter(status != "not applicable")
  row_keys <- levels(cells$row_key)
  row_keys <- row_keys[row_keys %in% as.character(applying$row_key)]
  column_keys <- levels(cells$column)
  if (is.null(column_names)) {
    column_names <- column_keys
  }

  # (a) The text of every cell: the estimate with its stars, and its standard
  # error, or "--" and blank
  estimate_lines <- matrix("", length(row_keys), length(column_keys))
  se_lines <- matrix("", length(row_keys), length(column_keys))
  for (i in seq_len(nrow(cells))) {
    r <- match(as.character(cells$row_key[i]), row_keys)
    k <- match(as.character(cells$column[i]), column_keys)
    if (is.na(r) || is.na(k)) {
      next
    }
    if (cells$status[i] == "too few") {
      estimate_lines[r, k] <- "--"
    } else if (cells$status[i] == "shown") {
      estimate_lines[r, k] <- fmt_num(cells$estimate[i], digits)
      if (stars) {
        estimate_lines[r, k] <- paste0(estimate_lines[r, k], star_text(cells$p[i]))
      }
      if (!is.na(cells$se[i])) {
        se_lines[r, k] <- paste0("(", fmt_num(cells$se[i], digits), ")")
      }
    }
  }

  # (b) Two lines per row: the label with the estimates, then the standard errors.
  # match(): the position of each row's first cell, for its label and panel.
  first_cell <- match(row_keys, as.character(cells$row_key))
  labels <- latex_text(cells$row_label[first_cell])
  panels <- cells$panel[first_cell]
  n_rows <- length(row_keys)
  line_labels <- character(2 * n_rows)
  line_labels[seq(1, 2 * n_rows, by = 2)] <- labels
  tab <- data.frame(label = line_labels, stringsAsFactors = FALSE)
  for (k in seq_along(column_keys)) {
    values <- character(2 * n_rows)
    values[seq(1, 2 * n_rows, by = 2)] <- estimate_lines[, k]
    values[seq(2, 2 * n_rows, by = 2)] <- se_lines[, k]
    tab[[paste0("column_", k)]] <- values
  }
  # A long header over several lines (multiline_header(), exhibits.R)
  header_names <- c("", multiline_header(column_names, 14))
  if (!is.null(row_info)) {
    values <- character(2 * n_rows)
    values[seq(1, 2 * n_rows, by = 2)] <-
      row_info$value[match(row_keys, as.character(row_info$row_key))]
    values[is.na(values)] <- ""
    tab$row_info <- values
    header_names <- c(header_names, latex_text(row_info_title))
  }

  # (c) The panels: two lines per row. rle(): the lengths of runs of equal values,
  # so that each panel counts its adjacent rows.
  runs <- rle(panels)
  if (any(duplicated(runs$values))) {
    stop("The rows of a panel must be adjacent: ", file)
  }
  # setNames(): the values, with these names
  panel_sizes <- setNames(2 * runs$lengths, latex_text(runs$values))
  table_header <- NULL
  if (!is.null(header)) {
    table_header <- setNames(header, latex_text(names(header)))
  }
  table_notes <- latex_text(notes)
  if (stars && any(!is.na(cells$p[cells$status == "shown"]))) {
    table_notes <- paste(table_notes, STARS_NOTE)
  }
  table_custom(tab, file, table_notes, col_names = header_names,
               align = c("l", rep("c", ncol(tab) - 1)), header = table_header,
               panels = panel_sizes)
  TRUE
}

#' The cells of a table of effects: the probabilities at the baseline in
#' percent, then the effects in percentage points, by block, in the order of
#' EFFECT_LABELS
#'
#' @param effects   rows of collected$effects, with the column `column` set
#' @param baselines rows of collected$baselines, with `column` set; may be empty
#' @return cells for table_estimates(); the caller sets the levels of column
effect_cells <- function(effects, baselines) {
  baseline_cells <- baselines %>%
    transmute(row_key = "baseline", row_label = "Probability (%)",
              panel = "At the baseline", column, estimate = 100 * probability,
              se = 100 * se, p = NA_real_, status)
  change_cells <- effects %>%
    transmute(row_key = effect, row_label = effect_label, panel = block, column,
              estimate = 100 * ame, se = 100 * se, p, status)
  # order(): stable, so the effects keep their order within a block
  effect_order <- EFFECT_LABELS$effect[order(match(EFFECT_LABELS$block,
                                                    EFFECT_BLOCKS))]
  row_order <- c("baseline", effect_order)
  cells <- bind_rows(baseline_cells, change_cells)
  cells$row_key <- factor(cells$row_key,
                          levels = row_order[row_order %in% cells$row_key])
  cells
}

#' What a set of effects is, for the notes of its exhibits, plain text
#'
#' @param set_row    one row of collected$sets
#' @param for_figure TRUE: about intervals, not standard errors and baselines
#' @return text
set_note <- function(set_row, for_figure = FALSE) {
  who <- paste0("Over the observed experts, each with their own characteristics, ",
                "all at this household: ", set_row$household, ".")
  if (set_row$kind == "reference expert") {
    who <- paste0("At one fictitious reference expert per group, with the ",
                  "reference values of the estimation script, at this household: ",
                  set_row$household, ".")
  } else if (set_row$kind == "answers") {
    who <- paste0("Over the vignettes the experts answered, each expert with ",
                  "their own characteristics.")
  }
  values <- ""
  if (set_row$values_text != "") {
    values <- paste0(" The changes of continuous terms move between these values: ",
                     set_row$values_text, "; every other change switches one ",
                     "answer or attribute on.")
  }
  draws <- paste0(fmt_int(set_row$n_draws_kr), " Krinsky-Robb draws at ",
                  fmt_int(set_row$n_draws), " Halton draws over the random ",
                  "constants")
  # A figure keeps its note short: the household and the values are in the notes
  # of the table of the same set
  if (for_figure) {
    return(paste0("The average change in the probability of each ",
                  "recommendation, in percentage points, with its 95% ",
                  "percentile interval from ", draws, ". The household and the ",
                  "values of the changes: the notes of the table of this set."))
  }
  # The stars take the effect over its standard error as normal, while the
  # figures show the percentile interval of the same draws, which can differ
  # from it where the draws are skewed: the note says so (Claude's proposal,
  # agreed by Max, 2026-11-23)
  paste0(who, " The average change in the probability of each recommendation, ",
         "in percentage points; the probability at the baseline in percent.",
         values, " Standard errors in parentheses, from ", draws, "; the stars ",
         "take the effect over its standard error as normal, whereas the figures ",
         "show the 95% percentile interval of the draws, which can differ where ",
         "the draws are skewed. The effects of one change sum to zero over the ",
         "outcomes.")
}

#' The sentence that the estimates of the expert's terms are associations, for
#' the notes of every exhibit that shows one of them (Max, 2026-11-17): only the
#' household's attributes are assigned at random (project profile, "Known
#' issues"). A block or panel not listed here counts as the expert's, so that a
#' new one carries the sentence.
#'
#' @param blocks the blocks of the effects (EFFECT_BLOCKS) or the panels of the
#'               coefficients (TERM_LABELS, COEFFICIENT_PANELS) the exhibit shows
#' @return the sentence with a space before it; "" if the exhibit shows none of
#'         the expert's terms
association_note <- function(blocks) {
  not_expert <- c("The household", "Constants", "At the baseline",
                  "Standard deviation of the random constant")
  if (all(blocks %in% not_expert)) {
    return("")
  }
  paste0(" Only the household's attributes are assigned at random: the expert's ",
         "characteristics, reported prices and price expectations vary only ",
         "between experts, so their estimates are associations, not causal ",
         "effects.")
}

#' The households of the effects over the observed experts, for a note: the
#' reference household in full, the others by what differs from it
#'
#' @param household_sets rows of collected$sets of kind "experts", the main set
#'                       first
#' @return text
households_text <- function(household_sets) {
  short_labels <- SET_LABELS$short[match(household_sets$set, SET_LABELS$set)]
  text <- paste0(short_labels[1], ": ", household_sets$household[1], ".")
  if (nrow(household_sets) > 1) {
    text <- paste0(text, " The others differ from it in: ",
                   paste0(short_labels[-1], ": ",
                          household_sets$household_change[-1], collapse = " | "),
                   ".")
  }
  text
}

#' A number for a note, with a minus sign that LaTeX sets as one (latex_text()
#' turns "−" into $-$)
signed_text <- function(x, digits) {
  # formatC(): the number with exactly `digits` decimals and a thousands comma;
  # sub(): its hyphen replaced by a minus sign
  sub("^-", "−", formatC(x, format = "f", digits = digits, big.mark = ","))
}

# 2. Figures of estimates ------------------------------------------------------

#' Estimates as points with their intervals: a row per estimate, its first level
#' at the top; a panel per facet, all on one axis; a colour and symbol per series,
#' the series side by side within a row
#'
#' @param points    tibble: row (factor), facet (factor), series (factor),
#'                  estimate, ci_low, ci_high; only cells that are shown
#' @param x_title   title of the horizontal axis
#' @param colours   colours named by the levels of series
#' @param shapes    filled symbols (21 to 25) named by the levels of series
#' @return ggplot
plot_estimates <- function(points, x_title, colours, shapes) {
  # orientation = "y": side by side along the vertical axis; reverse = TRUE: the
  # first series at the top of its row
  beside <- position_dodge(width = 0.7, orientation = "y", reverse = TRUE)
  plot <- ggplot(points, aes(x = estimate, y = row, colour = series,
                             fill = series, shape = series)) +
    geom_vline(xintercept = 0, colour = COLOUR_INK, linewidth = 0.3) +
    # geom_linerange(): a line from xmin to xmax, here the interval
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), position = beside,
                   linewidth = 0.4) +
    geom_point(position = beside, size = 1.5, stroke = 0.25,
               colour = COLOUR_INK) +
    scale_colour_manual(values = colours) +
    scale_fill_manual(values = colours) +
    scale_shape_manual(values = shapes) +
    # limits = rev: the first level of row at the top
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = label_number()) +
    labs(x = x_title, y = NULL) +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_text(size = figure_font_size - 1),
          legend.key.size = unit(8, "pt"))
  # Three panels in a row, so that six outcomes take two rows
  if (n_distinct(points$facet) > 1) {
    plot <- plot + facet_wrap(~facet, ncol = 3)
  }
  if (n_distinct(points$series) == 1) {
    plot <- plot + theme(legend.position = "none")
  }
  plot
}

#' The figures of one block of effects: as many as the rows need, each at a
#' size of figure_sizes, each saved, with its row of the list of exhibits. A
#' figure holds as many rows as fit its panels: more series side by side need
#' more height per row.
#'
#' @param points    as in plot_estimates()
#' @param file_stem the file name without ".pdf"; a second part adds "_2"
#' @param dirs      the folders of the exhibits
#' @param exhibit   list: block, id, group, caption, placement, note, for
#'                  exhibit_row()
#' @param colours, shapes, x_title as in plot_estimates()
#' @return the exhibit_row()s of the figures written
effect_figures <- function(points, file_stem, dirs, exhibit, colours, shapes,
                           x_title = "Change in probability (pp)") {
  if (nrow(points) == 0) {
    return(tibble())
  }
  # The height a row takes, in inches, and the height left for rows in a panel of
  # each size: a measure, checked on the decks, not a rule of the skill. Three
  # panels in a row, as in plot_estimates().
  n_series <- n_distinct(points$series)
  row_height <- 0.12 + 0.05 * (n_series - 1)
  n_facet_rows <- ceiling(n_distinct(points$facet) / 3)
  size <- "full"
  room <- figure_sizes$full[["height"]] - 0.75
  # droplevels(): only the rows that have points; nlevels(): how many
  if (n_facet_rows > 1 || nlevels(droplevels(points$row)) * row_height > room) {
    size <- "tall"
    room <- (figure_sizes$tall[["height"]] - 0.75) / n_facet_rows
  }
  rows_per_figure <- max(3, floor(room / row_height))
  row_levels <- levels(droplevels(points$row))
  # ceiling(): the parts needed for all rows; then the rows spread evenly over
  # them, so that no part is left with one or two rows
  n_parts <- ceiling(length(row_levels) / rows_per_figure)
  rows_per_figure <- ceiling(length(row_levels) / n_parts)
  exhibits <- tibble()
  for (part in seq_len(n_parts)) {
    part_rows <- row_levels[((part - 1) * rows_per_figure + 1):
                              min(part * rows_per_figure, length(row_levels))]
    part_points <- points %>%
      filter(row %in% part_rows) %>%
      mutate(row = factor(as.character(row), levels = part_rows))
    file_name <- paste0(file_stem, if (part > 1) paste0("_", part) else "", ".pdf")
    plot <- plot_estimates(part_points, x_title, colours, shapes)
    save_figure(plot, file.path(dirs$figures, file_name), size = size)
    caption <- exhibit$caption
    if (n_parts > 1) {
      caption <- paste0(caption, " (", part, " of ", n_parts, ")")
    }
    exhibits <- bind_rows(exhibits, exhibit_row(
      exhibit$block, exhibit$id, exhibit$group, "figure",
      file.path("figures", file_name), caption, exhibit$placement, exhibit$note
    ))
  }
  exhibits
}

# 3. Text ----------------------------------------------------------------------

#' A name for files: lower case, everything but letters and digits as "_", e.g.
#' "Gas or oil heating" -> "gas_or_oil_heating"
slug_of <- function(text) {
  slug <- gsub("[^a-z0-9]+", "_", str_to_lower(text))
  # gsub() with "^_|_$": no "_" at either end
  gsub("^_|_$", "", slug)
}

#' The short name of a block of effects in file names (the names of
#' EFFECT_BLOCKS, labels.R), e.g. "Expected price change" -> "expectations"
block_slug <- function(block_name) {
  # match(): the position of the block among EFFECT_BLOCKS
  slug <- names(EFFECT_BLOCKS)[match(block_name, EFFECT_BLOCKS)]
  if (is.na(slug)) {
    slug <- slug_of(block_name)
  }
  slug
}

#' A model as the text of the estimation scripts: "m4pe_3" -> "(4pe_3)"
model_text <- function(model_name) {
  # sub(): replaces the first match of a pattern, here the leading "m"
  paste0("(", sub("^m", "", model_name), ")")
}

#' What a probability is the probability of, inside a sentence: "recommending
#' the heat pump", or "giving no recommendation"
#'
#' @param outcome_label a label of OUTCOME_LABELS
outcome_phrase <- function(outcome_label) {
  if_else(outcome_label == OUTCOME_LABELS[["Keine Empfehlung"]],
          "giving no recommendation",
          paste0("recommending the ", str_to_lower(outcome_label)))
}

#' Text in a table cell over several lines, flush left: LaTeX, in \makecell[l]
#'
#' @param text  plain text
#' @param width characters per line
wrapped_cell <- function(text, width) {
  # str_wrap(): line breaks after at most `width` characters; each one becomes
  # the line break of a cell
  wrapped <- str_wrap(latex_text(text), width)
  paste0("\\makecell[l]{", gsub("\n", "\\\\", wrapped, fixed = TRUE), "}")
}

#' The household of a set of effects in words, from its `at` and the labels of
#' the design; without attributes, the experts' own vignettes
#'
#' @param at the `at` of compute_ames(), a named list
#' @return text
household_text <- function(at) {
  design_attributes <- unique(DESIGN_LEVELS$attribute)
  set_attributes <- design_attributes[design_attributes %in% names(at)]
  if (length(set_attributes) == 0) {
    return("each expert's own vignettes")
  }
  parts <- character(0)
  for (attribute_name in set_attributes) {
    is_level <- DESIGN_LEVELS$attribute == attribute_name &
      DESIGN_LEVELS$level == at[[attribute_name]]
    if (sum(is_level) != 1) {
      stop("No label for the level ", at[[attribute_name]], " of ",
           attribute_name, " (add it to DESIGN_LEVELS in R/functions/labels.R)")
    }
    parts <- c(parts, paste0(DESIGN_LEVELS$attribute_label[is_level], ": ",
                             DESIGN_LEVELS$level_label[is_level]))
  }
  paste(parts, collapse = "; ")
}

#' A value of a continuous change, for the notes: no decimals from 100 up, else
#' one
value_text <- function(x) {
  if (abs(x) >= 100) {
    return(formatC(x, format = "f", digits = 0, big.mark = ","))
  }
  formatC(x, format = "f", digits = 1)
}

#' The values that the continuous changes of a set move between, for the notes,
#' e.g. "gas price 10.7 to 13.2 ct/kWh; heating degree days 3,100 to 3,600
#' degree days". A change counts if it sets one variable, a number at both ends.
#'
#' @param changes the `changes` of compute_ames()
#' @param effects the effects to describe, in their order
#' @return text, "" if none
change_values_text <- function(changes, effects) {
  parts <- character(0)
  for (effect in effects) {
    change <- changes[[effect]]
    if (length(change$from) != 1 || length(change$to) != 1) {
      next
    }
    value_from <- change$from[[1]]
    value_to <- change$to[[1]]
    if (!is.numeric(value_from) || !is.numeric(value_to) ||
        is.na(value_from) || is.na(value_to)) {
      next
    }
    effect_row <- EFFECT_LABELS[EFFECT_LABELS$effect == effect, ]
    unit_text <- ""
    if (effect_row$unit != "") {
      unit_text <- paste0(" ", effect_row$unit)
    }
    parts <- c(parts, paste0(effect_row$label, " ", value_text(value_from), " to ",
                             value_text(value_to), unit_text))
  }
  paste(parts, collapse = "; ")
}
