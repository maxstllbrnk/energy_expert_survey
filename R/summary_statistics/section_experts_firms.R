################################################################################
# section_experts_firms.R — Block D: who the experts are and what their firms are
#                           like. First, the helpers that sections D to J share.
# Inputs : experts (docs/data_contract.md)
# Outputs: tables/tab_experts_<group>.tex, tab_experts_groups_all.tex;
#          maps/map_experts_state_all.pdf
# Checks : unknown answers and the minimum cell, in the helpers
################################################################################

# 1. What sections D to J share ------------------------------------------------

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

#' summary_table() writes a year like any number, 1,995: the statistics of these
#' rows lose the separator, their N keeps it
#'
#' @param file         path of the .tex file
#' @param year_labels  the labels of the rows, as LaTeX
#' @param n_statistics the statistics per block of columns
remove_year_separators <- function(file, year_labels, n_statistics) {
  # readLines(), writeLines(): a text file as a vector of lines, and back
  lines <- readLines(file, encoding = "UTF-8")
  for (year_label in year_labels) {
    # grepl(fixed = TRUE): does the line contain this text?
    for (k in which(grepl(paste0(year_label, " &"), lines, fixed = TRUE))) {
      # strsplit(): the cells of the line; [[1]]: of this one line
      cells <- strsplit(lines[k], " & ", fixed = TRUE)[[1]]
      # The second cell is the first N, the first cell the label
      is_n <- (seq_along(cells) - 2) %% n_statistics == 0
      # gsub(): replaces every comma
      cells[!is_n] <- gsub(",", "", cells[!is_n], fixed = TRUE)
      lines[k] <- paste(cells, collapse = " & ")
    }
  }
  writeLines(lines, file)
}

#' A table of numbers in all its versions: all experts, each group and, for a
#' table of the main part, the groups side by side (id <id>_groups). A question
#' that nobody in a version answered (routing) loses its rows there: "--" would
#' say that too few answered, not that it was not asked. In the groups side by
#' side its row stays, with "--" for the group, and the note says who was asked.
#' A version without a row of MIN_N answers is not written.
#'
#' @param data      one row per expert, with expert_group
#' @param rows      as in summary_table(); also kind "year", a number without a
#'                  thousands separator
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
  year_labels <- latex_text(rows$label[rows$kind == "year"])
  rows <- rows %>%
    mutate(kind = if_else(kind == "year", "num", kind))
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
    # Six statistics: N, mean, SD and the three quartiles
    remove_year_separators(file.path(dirs$tables, file_name), year_labels, 6)
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
    group_stats <- c("n", "mean", "sd")
    summary_table(answers_by_group, rows[n_answers > 0, ],
                  file.path(dirs$tables, file_name), table_notes,
                  by = "expert_group", stats = group_stats)
    remove_year_separators(file.path(dirs$tables, file_name), year_labels,
                           length(group_stats))
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

# 2. Block D -------------------------------------------------------------------

#' The experts and their firms: a table in its versions, and the map of the states
#'
#' @param experts one row per expert of the sample, as in experts.rds
#' @param dirs    the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_experts_firms <- function(experts, dirs) {
  block <- "D. Experts and firms"

  # 2.1 The table: the expert, then the firm
  rows <- bind_rows(
    tribble(
      ~panel,       ~variable,                 ~kind,   ~level, ~label,                        ~digits,
      "The expert", "age",                     "num",   NA,     "Age (years)",                 1,
      "The expert", "dem_training_year_clean", "year",  NA,     "Year training was completed", 0,
      "The expert", "resp_is_owner",           "level", "Ja",   "Owner or managing director",  1
    ),
    tibble(panel = "The expert: highest degree (Q22)",
           variable = "dem_education_recoded", kind = "level",
           level = names(EDUCATION_LABELS), label = unname(EDUCATION_LABELS),
           digits = 1),
    tibble(panel = "The expert: continuing education, last three years (Q24)",
           variable = names(TRAINING_LABELS), kind = "level", level = "Ja",
           label = unname(TRAINING_LABELS), digits = 1),
    tibble(panel = "The firm", variable = "firm_employees_clean", kind = "num",
           level = NA_character_, label = "Employees", digits = 1),
    tibble(panel = "The firm: revenue, last fiscal year (Q4)",
           variable = "firm_revenue", kind = "level",
           level = names(REVENUE_LABELS), label = unname(REVENUE_LABELS),
           digits = 1),
    tibble(panel = "The firm: service area (Q6)",
           variable = "firm_radius_recoded", kind = "level",
           level = names(RADIUS_LABELS), label = unname(RADIUS_LABELS),
           digits = 1),
    tibble(panel = "The firm: category, several ticks possible (Q2)",
           variable = names(FIRM_CATEGORY_LABELS), kind = "level", level = "Ja",
           label = unname(FIRM_CATEGORY_LABELS), digits = 1)
  )
  notes <- paste0(
    "Age: ", SURVEY_YEAR, " minus the year of birth. Years and employees without ",
    "impossible answers and without the 0.5% lowest and highest answers of all ",
    "experts. Shares among the experts who answered. Q6: the soft launch had no ",
    "answer \"nationwide\". Q2 sets the group: HVAC before chimney sweep before ",
    "energy advice, so the shares of Q2 within a group are in part mechanical."
  )
  exhibits <- summary_exhibits(experts, rows, "experts",
                               "The experts and their firms", notes, block,
                               "main", dirs)

  # 2.2 Experts per state (all experts only). A state below the minimum cell has
  # no number: its count would describe too few experts.
  if ("bundesland" %in% names(experts)) {
    comma_text <- label_comma()
    state_counts <- experts %>%
      filter(!is.na(bundesland)) %>%
      count(bundesland) %>%
      transmute(state = as.character(bundesland),
                value = if_else(n >= MIN_N, as.numeric(n), NA_real_),
                label = comma_text(n))
    if (any(!is.na(state_counts$value))) {
      plot <- plot_state_map(state_counts, "Experts")
      file_name <- paste0("map_experts_state_", group_slug(ALL_EXPERTS), ".pdf")
      save_figure(plot, file.path(dirs$maps, file_name), size = "map")
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "experts_state", ALL_EXPERTS, "map",
        file.path("maps", file_name), "Experts per federal state", "appendix",
        note = paste0("The state follows from the postcode of the firm (Q0). A ",
                      "state with fewer than ", MIN_N,
                      " experts is gray, without a number.")
      ))
    }
  }
  exhibits
}
