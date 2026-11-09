################################################################################
# exhibits.R — what the tables, figures and maps of the summary statistics share:
#              one list of exhibits, the minimum cell, the state maps, the kinds
#              of figure that several sections draw
# Inputs : labels.R; tables.R and figures.R (core files of the economics-r skill)
# Outputs: none
# Checks : the minimum cell in summary_table(); unknown labels and answers stop
################################################################################
# A section of the summary statistics is one function that writes its files into
# dirs$tables, dirs$figures and dirs$maps and returns one exhibit_row() per file.
# The deck and exhibits.csv are built from these rows. Files are named
#   tab_<id>_<group slug>.tex, fig_<id>_<group slug>.pdf, map_<id>_<group slug>.pdf

# 1. Text ----------------------------------------------------------------------

#' Plain text as LaTeX: the reserved characters are escaped, and symbols that
#' pdflatex does not read as they are become LaTeX, so that one label serves the
#' tables, the figures and Excel. The order matters: the symbols write $ and ^.
#' Backslashes and braces are not expected in plain text.
latex_text <- function(text) {
  text <- gsub("&", "\\&", text, fixed = TRUE)
  text <- gsub("%", "\\%", text, fixed = TRUE)
  text <- gsub("_", "\\_", text, fixed = TRUE)
  text <- gsub("#", "\\#", text, fixed = TRUE)
  text <- gsub("$", "\\$", text, fixed = TRUE)
  text <- gsub("~", "\\textasciitilde{}", text, fixed = TRUE)
  text <- gsub("^", "\\textasciicircum{}", text, fixed = TRUE)
  text <- gsub("²", "$^2$", text, fixed = TRUE)
  text <- gsub("₂", "$_2$", text, fixed = TRUE)
  text <- gsub("×", "$\\times$", text, fixed = TRUE)
  text <- gsub("≥", "$\\geq$", text, fixed = TRUE)
  text <- gsub("≤", "$\\leq$", text, fixed = TRUE)
  text <- gsub("−", "$-$", text, fixed = TRUE)
  text <- gsub("χ", "$\\chi$", text, fixed = TRUE)
  text <- gsub("→", "$\\rightarrow$", text, fixed = TRUE)
  gsub("€", "\\texteuro{}", text, fixed = TRUE)
}

#' The label of each value; stops if a value has none, so that no raw name or
#' German answer reaches a table or figure
#'
#' @param values character vector or factor
#' @param labels named character vector: value -> label
#' @param what   what the values are, for the error message
label_of <- function(values, labels, what) {
  values <- as.character(values)
  missing_values <- setdiff(unique(values[!is.na(values)]), names(labels))
  if (length(missing_values) > 0) {
    stop("No label for the ", what, ": ", paste(missing_values, collapse = ", "),
         " (add it in R/functions/labels.R)")
  }
  unname(labels[values])
}

#' The name of a group in file names, e.g. "HVAC installers" -> "hvac_installers"
group_slug <- function(group_name) {
  label_of(group_name, GROUP_SLUGS, "group")
}

# 2. The list of exhibits ------------------------------------------------------

#' One row of the list of exhibits, from which the deck and exhibits.csv are built
#'
#' @param block     the block of the summary statistics, e.g. "D. Experts and firms"
#' @param id        name of the exhibit without group, e.g. "experts"
#' @param group     "All experts" or a group of EXPERT_GROUPS
#' @param type      "table", "figure" or "map"
#' @param file      path of the file, relative to the summary statistics folder,
#'                  e.g. "tables/tab_experts_all.tex"
#' @param caption   the caption, plain text
#' @param placement "main" or "appendix" of the paper
#' @param note      figures and maps: the note below them, plain text
#' @return tibble, one row
exhibit_row <- function(block, id, group, type, file, caption, placement,
                        note = "") {
  tibble(block = block, id = id, group = group, type = type, file = file,
         caption = caption, placement = placement, note = note)
}

#' The experts or vignettes of one version of an exhibit: all, or one group
#'
#' @param data  data with the column expert_group
#' @param group "All experts" or a group of EXPERT_GROUPS
subset_group <- function(data, group) {
  if (group == ALL_EXPERTS) {
    data
  } else {
    data %>%
      filter(expert_group == group)
  }
}

#' The sections of the summary statistics skip what the data do not have: whether
#' all of these columns are in the data
has_columns <- function(data, columns) {
  all(columns %in% names(data))
}

# 3. Summary tables ------------------------------------------------------------

#' Summary table of one sample, or of several groups side by side: N, mean, SD and
#' quartiles of numeric variables, N and the share of each answer of categorical
#' ones. A wrapper around table_summary() (tables.R) that enforces the minimum
#' cell: a row with fewer than MIN_N answers shows "--" in every column.
#'
#' @param data  one row per expert or vignette
#' @param rows  one row per line of the table:
#'   panel     panel of the table (rows of a panel adjacent)
#'   variable  the column of `data`
#'   kind      "num": a number; "year": a number without a thousands separator;
#'             "level": the share of answers equal to `level`; "dummy": a 0/1
#'             column, shown as the share of ones
#'   level     for "level": the answer, exactly as in the data; else NA
#'   label     the row label, plain text with units
#'   digits    decimals
#' @param file  path of the .tex file
#' @param notes table notes, plain text (latex_text() is applied)
#' @param by    optional grouping column: one block of columns per value. Rows with
#'              NA in it are left out, and so is a value with fewer than MIN_N
#'              rows, as its number of rows would stand in the footer
#' @param stats statistics to report, as in table_summary(); N first
#' @return TRUE if the table was written; FALSE if none of its variables is in
#'   the data, or no value of `by` has MIN_N rows
summary_table <- function(data, rows, file, notes, by = NULL,
                          stats = c("n", "mean", "sd", "p25", "p50", "p75")) {
  rows <- rows %>%
    filter(variable %in% names(data))
  if (nrow(rows) == 0) {
    return(FALSE)
  }
  if (!is.null(by)) {
    data <- data[!is.na(data[[by]]), ]
    # table(): the number of rows per value
    group_sizes <- table(as.character(data[[by]]))
    large_groups <- names(group_sizes)[group_sizes >= MIN_N]
    data <- data[as.character(data[[by]]) %in% large_groups, ]
    if (nrow(data) == 0) {
      return(FALSE)
    }
    # levels in their order: split() in table_summary() sorts a factor by them
    data[[by]] <- droplevels(as.factor(data[[by]]))
  }

  # One numeric column per row of the table
  table_data <- tibble(.rows = nrow(data))
  if (!is.null(by)) {
    table_data[[by]] <- data[[by]]
  }
  row_names <- paste0("row_", seq_len(nrow(rows)))
  for (k in seq_len(nrow(rows))) {
    values <- data[[rows$variable[k]]]
    if (rows$kind[k] == "level") {
      # NA stays NA: the share is among those who answered
      values <- as.numeric(as.character(values) == rows$level[k])
    } else {
      values <- as.numeric(values)
    }
    table_data[[row_names[k]]] <- values
  }

  # The minimum cell, per block of columns
  group_values <- rep("all", nrow(table_data))
  if (!is.null(by)) {
    group_values <- as.character(table_data[[by]])
  }
  for (row_name in row_names) {
    for (group_value in unique(group_values)) {
      in_group <- group_values == group_value
      if (sum(!is.na(table_data[[row_name]][in_group])) < MIN_N) {
        table_data[[row_name]][in_group] <- NA
      }
    }
  }

  vars <- tibble(
    var = row_names,
    label = latex_text(rows$label),
    digits = rows$digits,
    kind = if_else(rows$kind %in% c("num", "year"), "num", "pct")
  )
  if ("panel" %in% names(rows)) {
    vars$panel <- latex_text(rows$panel)
  }
  table_summary(table_data, vars, file, notes = latex_text(notes), by = by,
                stats = stats)

  # table_summary() writes a year like any number, 1,995: the statistics of these
  # rows lose the separator. The first cell of a row is its label, and every N keeps
  # the separator.
  year_labels <- vars$label[rows$kind == "year"]
  if (length(year_labels) > 0) {
    # readLines(), writeLines(): a text file as a vector of lines, and back
    lines <- readLines(file, encoding = "UTF-8")
    for (year_label in year_labels) {
      # grepl(fixed = TRUE): does the line contain this text?
      for (k in which(grepl(paste0(year_label, " &"), lines, fixed = TRUE))) {
        # strsplit(): the cells of the line; [[1]]: of this one line
        cells <- strsplit(lines[k], " & ", fixed = TRUE)[[1]]
        is_statistic <- seq_along(cells) > 1 &
          (seq_along(cells) - 2) %% length(stats) != 0
        cells[is_statistic] <- gsub(",", "", cells[is_statistic], fixed = TRUE)
        lines[k] <- paste(cells, collapse = " & ")
      }
    }
    writeLines(lines, file)
  }
  TRUE
}

#' The standard end of a table's notes
cell_note <- function() {
  paste0("-- means fewer than ", MIN_N, " answers.")
}

#' A count of answers with the minimum cell: NA below MIN_N answers, or below
#' MIN_CONTACTS contacted persons, as the rate could be computed from it
#'
#' @param count     the answers
#' @param contacted the persons behind a rate; leave out if the table has no rate
shown_count <- function(count, contacted = MIN_CONTACTS) {
  if_else(contacted >= MIN_CONTACTS & count >= MIN_N, as.numeric(count), NA_real_)
}

# The note of the versions by group of the exhibits on who stopped
GROUP_KNOWN_NOTE <- paste0(
  "The group comes from Q2: persons who left before it have no group, and within ",
  "a group nobody can stop before it."
)

# 4. Maps ----------------------------------------------------------------------

# The labels of the small states would cover their neighbours' labels: they move
# outside the map by these kilometres, with a line to the state. Brandenburg's
# label would cover Berlin: it moves within Brandenburg, without a line.
MAP_LABEL_NUDGES <- tribble(
  ~state,        ~dx_km, ~dy_km, ~with_line,
  "Berlin",      110,    70,     TRUE,
  "Hamburg",     -140,   60,     TRUE,
  "Bremen",      -150,   0,      TRUE,
  "Saarland",    -75,    -40,    TRUE,
  "Brandenburg", 80,     -60,    FALSE
)

#' Map of the federal states with one number per state, coloured and written on
#' the map. A state without a value (below the minimum cell) is left grey with "–".
#'
#' @param state_values tibble: state (as in bundesland), value (numeric), label
#'                     (the text on the map, e.g. "12.3%")
#' @param legend_title title of the colour legend, plain text
#' @param low,high     ends of the colour ramp
#' @param legend_labels how the legend shows the values, e.g. label_percent()
#' @return ggplot; save it with save_figure(plot, file, size = "map")
plot_state_map <- function(state_values, legend_title, low = MAP_LOW, high = MAP_HIGH,
                           legend_labels = label_comma()) {
  # GF 9: the land area of the states, one polygon each
  map_states <- st_read(BUNDESLAND_SHP, quiet = TRUE) %>%
    filter(GF == 9)
  unknown_states <- setdiff(state_values$state, map_states$GEN)
  if (length(unknown_states) > 0) {
    stop("States that are no polygon of the map: ",
         paste(unknown_states, collapse = ", "))
  }
  map_data <- map_states %>%
    left_join(state_values, by = c("GEN" = "state")) %>%
    mutate(label = if_else(is.na(value), "–", label))

  # Where the labels go: a point inside each state, in metres of the map's
  # projection (st_point_on_surface(), st_coordinates()), moved by MAP_LABEL_NUDGES
  state_points <- st_coordinates(st_point_on_surface(st_geometry(map_data)))
  map_labels <- tibble(state = map_data$GEN, label = map_data$label,
                       x = state_points[, "X"], y = state_points[, "Y"]) %>%
    left_join(MAP_LABEL_NUDGES, by = "state", relationship = "one-to-one") %>%
    mutate(is_nudged = !is.na(dx_km),
           label_x = if_else(is_nudged, x + 1000 * dx_km, x),
           label_y = if_else(is_nudged, y + 1000 * dy_km, y),
           # the line stops short of the label
           line_x = x + 0.75 * (label_x - x),
           line_y = y + 0.75 * (label_y - y))
  map_box <- st_bbox(map_data)
  map_xlim <- range(c(map_box[["xmin"]], map_box[["xmax"]], map_labels$label_x)) +
    c(-50000, 50000)
  map_ylim <- range(c(map_box[["ymin"]], map_box[["ymax"]], map_labels$label_y)) +
    c(-20000, 20000)

  ggplot() +
    geom_sf(data = map_data, aes(fill = value), colour = "white", linewidth = 0.3) +
    geom_segment(data = map_labels %>% filter(is_nudged, with_line),
                 aes(x = x, y = y, xend = line_x, yend = line_y),
                 colour = COLOUR_INK, linewidth = 0.25) +
    geom_label(data = map_labels, aes(x = label_x, y = label_y, label = label),
               size = (figure_font_size - 2) / .pt, colour = COLOUR_INK,
               fill = "white", alpha = 0.85, linewidth = 0,
               label.padding = unit(0.1, "lines")) +
    scale_fill_gradient(low = low, high = high, labels = legend_labels,
                        na.value = COLOUR_NOT_SHOWN, name = legend_title) +
    # datum = NA: no graticule. The x and y of the labels and the limits are in the
    # map's projection, as coord_sf() expects of layers that are not sf.
    coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE, datum = NA) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "right",
          legend.title = element_text(size = figure_font_size - 1),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
}

#' A number per federal state from the answers of the experts, with the minimum
#' cell applied, ready for plot_state_map()
#'
#' @param data    one row per expert, with bundesland
#' @param values  numeric vector, one per row of data; the mean per state is taken
#' @param digits  decimals of the label
#' @param percent TRUE: the values are shares, shown as percent
#' @return tibble: state, n, value, label
state_means <- function(data, values, digits = 1, percent = FALSE) {
  by_state <- tibble(state = as.character(data$bundesland), value = values) %>%
    filter(!is.na(state), !is.na(value)) %>%
    group_by(state) %>%
    summarise(n = n(), value = mean(value)) %>%
    ungroup() %>%
    mutate(value = if_else(n >= MIN_N, value, NA_real_))
  if (percent) {
    by_state$label <- paste0(formatC(100 * by_state$value, format = "f", digits = digits), "%")
  } else {
    by_state$label <- formatC(by_state$value, format = "f", digits = digits, big.mark = ",")
  }
  by_state
}

# 5. What the sections of the questions share ----------------------------------

#' The number of answers of each row of a table in this sample; 0 if its column
#' is not in the sample
#'
#' @param rows rows of a table, as in summary_table()
#' @param data one row per expert
#' @return integer vector, one number per row
answers_per_row <- function(rows, data) {
  n_answers <- integer(nrow(rows))
  for (k in seq_len(nrow(rows))) {
    if (rows$variable[k] %in% names(data)) {
      n_answers[k] <- sum(!is.na(data[[rows$variable[k]]]))
    }
  }
  n_answers
}

#' Stops if a table does not fit the answers: an answer of a question it shows
#' with several answers is not listed, so that the shares would not sum to 100;
#' or a listed answer is not an answer of the question (a factor knows its
#' answers), so that the share would be 0
#'
#' @param rows rows of a table, as in summary_table()
#' @param data one row per expert
check_listed_answers <- function(rows, data) {
  level_rows <- rows %>%
    filter(kind == "level", variable %in% names(data))
  for (variable_name in unique(level_rows$variable)) {
    values <- data[[variable_name]]
    answers <- unique(as.character(values[!is.na(values)]))
    listed <- level_rows$level[level_rows$variable == variable_name]
    # setdiff(): what is in the first vector and not in the second
    not_listed <- setdiff(answers, listed)
    if (length(listed) > 1 && length(not_listed) > 0) {
      stop(variable_name, " has answers that the table does not list: ",
           paste(not_listed, collapse = ", "))
    }
    if (is.factor(values) && !all(listed %in% levels(values))) {
      stop("The table lists answers that ", variable_name, " does not have: ",
           paste(setdiff(listed, levels(values)), collapse = ", "))
    }
  }
}

#' A table of numbers in all its versions: all experts, each group and, for a
#' table of the main part, the groups side by side (id <id>_groups). A question
#' that nobody in a version answered (routing) loses its rows there: "--" would
#' say that too few answered, not that it was not asked. In the groups side by
#' side its row stays, with "--" for the group, and the note says who was asked.
#' A version without a row of MIN_N answers is not written.
#'
#' @param data      one row per expert, with expert_group
#' @param rows      as in summary_table()
#' @param id        name of the exhibit without group
#' @param caption   the caption, plain text
#' @param notes     table notes, plain text; cell_note() is added
#' @param block     the block, as in exhibit_row()
#' @param placement of the all-experts version; the groups are always "appendix"
#' @param dirs      the folders of the section
#' @return the exhibit_row()s of the tables written
summary_exhibits <- function(data, rows, id, caption, notes, block, placement,
                             dirs) {
  check_listed_answers(rows, data)
  table_notes <- paste(notes, cell_note())
  exhibits <- tibble()

  # All experts and each group
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_data <- subset_group(data, group)
    n_answers <- answers_per_row(rows, group_data)
    if (max(n_answers) < MIN_N) {
      next
    }
    file_name <- paste0("tab_", id, "_", group_slug(group), ".tex")
    summary_table(group_data, rows[n_answers > 0, ],
                  file.path(dirs$tables, file_name), table_notes)
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, id, group, "table", file.path("tables", file_name), caption,
      if_else(group == ALL_EXPERTS, placement, "appendix")
    ))
  }

  # The groups side by side; a group below the minimum cell is no column, as its
  # number of experts would stand in the table
  answers_by_group <- data %>%
    filter(!is.na(expert_group)) %>%
    group_by(expert_group) %>%
    filter(n() >= MIN_N) %>%
    ungroup()
  n_answers <- answers_per_row(rows, answers_by_group)
  has_groups <- n_distinct(answers_by_group$expert_group) > 1
  if (placement == "main" && has_groups && max(n_answers) >= MIN_N) {
    id_groups <- paste0(id, "_groups")
    file_name <- paste0("tab_", id_groups, "_", group_slug(ALL_EXPERTS), ".tex")
    summary_table(answers_by_group, rows[n_answers > 0, ],
                  file.path(dirs$tables, file_name), table_notes,
                  by = "expert_group", stats = c("n", "mean", "sd"))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, id_groups, ALL_EXPERTS, "table", file.path("tables", file_name),
      paste0(caption, ", by group"), "main"
    ))
  }
  exhibits
}

#' Text as the header of a table column over several lines: LaTeX, in \makecell
#'
#' @param text  plain text
#' @param width characters per line
multiline_header <- function(text, width = 12) {
  # str_wrap(): line breaks after at most `width` characters; each one becomes
  # the line break of a cell
  wrapped <- str_wrap(latex_text(text), width)
  paste0("\\makecell{", gsub("\n", "\\\\", wrapped, fixed = TRUE), "}")
}

#' The shares of the segments of stacked bars, from one row per answer. A bar of
#' fewer than MIN_N experts is left out: six vignettes can come from one expert.
#'
#' @param answers tibble, one row per answer, no NA: resp_uid, panel (the panel of
#'                the figure, "" for one), bar, segment (factors in their order)
#' @return tibble: panel, bar, segment, n, n_bar, share; no rows if no bar is left
stack_answers <- function(answers) {
  answers %>%
    group_by(panel, bar) %>%
    mutate(n_bar = n(),
           n_experts = n_distinct(resp_uid)) %>%
    group_by(panel, bar, segment, n_bar, n_experts) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(share = n / n_bar) %>%
    filter(n_experts >= MIN_N)
}

#' Stacked bars of shares, one bar per row of the figure: the first level of bar
#' at the top, the first level of segment at the left. The number of answers
#' stands to the right of each bar.
#'
#' @param bars        tibble: panel, bar, segment, share, n_bar (stack_answers())
#' @param colours     colours named by the levels of segment
#' @param x_title     title of the horizontal axis
#' @param legend_rows rows of the legend
#' @return ggplot
plot_stacked_shares <- function(bars, colours, x_title, legend_rows = 1) {
  n_panels <- n_distinct(bars$panel)
  # .pt: points per mm, as text in a plot has its size in mm
  label_size <- (figure_font_size - 2) / .pt
  # A percentage in a segment only where it fits
  smallest_labelled <- if (n_panels > 1) 0.13 else 0.06
  percent_text <- label_percent(accuracy = 1)
  comma_text <- label_comma()
  # White text on the dark segments. col2rgb(): the red, green and blue of a
  # colour, one column per colour
  channels <- col2rgb(colours[as.character(bars$segment)])
  brightness <- 0.299 * channels["red", ] + 0.587 * channels["green", ] +
    0.114 * channels["blue", ]
  # droplevels(): the legend shows only the segments of these bars
  bars <- bars %>%
    mutate(segment = droplevels(segment),
           segment_label = if_else(share >= smallest_labelled,
                                   percent_text(share), ""),
           label_colour = if_else(brightness < 150, "white", COLOUR_INK))
  totals <- bars %>%
    group_by(panel, bar) %>%
    summarise(total = sum(share),
              n_bar = first(n_bar)) %>%
    ungroup() %>%
    mutate(n_label = paste0("n = ", comma_text(n_bar)))

  # position_stack(reverse = TRUE): the first level at the left
  plot <- ggplot(bars, aes(x = share, y = bar)) +
    geom_col(aes(fill = segment), width = 0.7, colour = "white", linewidth = 0.2,
             position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = segment_label, colour = label_colour, group = segment),
              size = label_size,
              position = position_stack(vjust = 0.5, reverse = TRUE)) +
    geom_text(data = totals, aes(x = total + 0.015, label = n_label), hjust = 0,
              size = label_size, colour = COLOUR_INK) +
    scale_fill_manual(values = colours, breaks = levels(bars$segment)) +
    # scale_colour_identity(): the colours in label_colour as they are
    scale_colour_identity() +
    # limits = rev: the first level of bar at the top
    scale_y_discrete(limits = rev) +
    scale_x_continuous(
      labels = percent_text,
      breaks = if (n_panels > 1) c(0, 0.5, 1) else seq(0, 1, 0.25),
      expand = expansion(mult = c(0, if (n_panels > 1) 0.34 else 0.15))
    ) +
    guides(fill = guide_legend(nrow = legend_rows, byrow = TRUE)) +
    labs(x = x_title, y = NULL) +
    # Seven technologies have to fit one row of the width of the figure, and the
    # long names of the arms a narrow panel. legend.location = "plot": the legend
    # is centered under the figure, not under the panels, which long labels push
    # to the right.
    theme(panel.grid.major.y = element_blank(),
          panel.spacing.x = unit(8, "pt"),
          strip.text = element_text(size = figure_font_size - 1.5, hjust = 0),
          strip.clip = "off",
          legend.location = "plot",
          legend.text = element_text(size = figure_font_size - 2,
                                     margin = margin(l = 2)),
          legend.key.size = unit(7, "pt"),
          legend.key.spacing.x = unit(3, "pt"))
  if (n_panels > 1) {
    plot <- plot + facet_wrap(~panel, nrow = 1)
  }
  plot
}

#' Horizontal bars of shares with the percentage at their end: a bar per item, or
#' several per item side by side (series)
#'
#' @param bars        tibble: item (factor; its first level at the top), series
#'                    (factor, the colour of the bar), share (0 to 1; NA draws no
#'                    bar and keeps its place)
#' @param colours     colours named by the levels of series
#' @param x_title     title of the horizontal axis
#' @param show_legend whether to show the legend of the series
#' @return ggplot
plot_share_bars <- function(bars, colours, x_title, show_legend = FALSE) {
  percent_text <- label_percent(accuracy = 1)
  # orientation = "y": side by side along the vertical axis; reverse = TRUE: the
  # first series at the top
  beside <- position_dodge(width = 0.8, orientation = "y", reverse = TRUE)
  bars <- bars %>%
    mutate(share_label = percent_text(share))
  ggplot(bars, aes(x = share, y = item, fill = series)) +
    geom_col(width = 0.7, position = beside, na.rm = TRUE) +
    geom_text(aes(label = share_label, group = series), position = beside,
              hjust = -0.15, size = (figure_font_size - 2) / .pt,
              colour = COLOUR_INK, na.rm = TRUE) +
    scale_fill_manual(values = colours,
                      guide = if (show_legend) "legend" else "none") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = percent_text, limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.1))) +
    labs(x = x_title, y = NULL) +
    theme(panel.grid.major.y = element_blank(),
          legend.location = "plot",
          legend.key.size = unit(8, "pt"))
}

#' Histograms in panels, with the mean and the median of all answers of a panel.
#' A bin of fewer than MIN_N answers is left out, as the tails of a distribution
#' would be single experts, and so is a panel without any bin left.
#'
#' @param values    tibble: panel (factor), value (numeric, no NA)
#' @param binwidth  width of the bins, which start at 0
#' @param x_title   title of the horizontal axis
#' @param n_columns columns of panels
#' @return ggplot; NULL if no panel is left
plot_binned_histogram <- function(values, binwidth, x_title, n_columns) {
  bins <- values %>%
    # floor(): the bin of a value, counted from 0
    mutate(bin_middle = (floor(value / binwidth) + 0.5) * binwidth) %>%
    count(panel, bin_middle) %>%
    filter(n >= MIN_N)
  if (nrow(bins) == 0) {
    return(NULL)
  }
  comma_text <- label_comma()
  panel_stats <- values %>%
    filter(panel %in% bins$panel) %>%
    group_by(panel) %>%
    summarise(n_panel = n(),
              Mean = mean(value),
              Median = median(value)) %>%
    ungroup() %>%
    mutate(n_label = paste0("n = ", comma_text(n_panel)))
  lines <- panel_stats %>%
    select(panel, Mean, Median) %>%
    pivot_longer(c(Mean, Median), names_to = "statistic",
                 values_to = "line_at")

  # x = Inf, y = Inf: the top right corner of each panel
  ggplot() +
    geom_col(data = bins, aes(x = bin_middle, y = n), width = binwidth,
             fill = COLOUR_BAR, colour = "white", linewidth = 0.1) +
    geom_vline(data = lines, aes(xintercept = line_at, linetype = statistic),
               colour = COLOUR_INK, linewidth = 0.4) +
    geom_text(data = panel_stats, aes(x = Inf, y = Inf, label = n_label),
              hjust = 1.1, vjust = 1.4, size = (figure_font_size - 2) / .pt,
              colour = COLOUR_INK) +
    facet_wrap(~panel, ncol = n_columns) +
    scale_linetype_manual(values = c(Mean = "solid", Median = "22")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = x_title, y = "Answers") +
    theme(legend.key.width = unit(18, "pt"))
}
