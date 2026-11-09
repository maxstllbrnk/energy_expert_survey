################################################################################
# 07_response_times.R — how long the experts took (appendix)
# Inputs : experts, vignettes (02_data.R); question_times() and
#          QUESTIONNAIRE of R/prepare_analysis_data.R
# Outputs: tables/tab_desc_times_sections.tex, tab_desc_times_vignettes.tex;
#          figures/fig_desc_vignette_times.pdf
# Checks : every section has a label; no statistic rests on fewer than MIN_N
#          answers
################################################################################
# Medians and quartiles, not trimmed: a questionnaire left open over lunch
# moves the mean, not the median. A section's time is the sum of the times of
# its questions, among the experts who answered at least one of them; the
# whole questionnaire is LimeSurvey's total time.

#' N, quartiles and median of the times of each item, for all experts, and the
#' median of each group
#'
#' @param times one row per expert x item: group, item, minutes (or seconds)
#' @param items the items in the order of the rows
#' @return one row per item: n, p25, median, p75 of all experts, and
#'   median_<group> for each group; NA below MIN_N answers
time_statistics <- function(times, items) {
  statistics <- tibble()
  for (item_name in items) {
    item_times <- times %>%
      filter(item == item_name)
    row <- tibble(item = item_name, n = nrow(item_times), p25 = NA_real_,
                  median = NA_real_, p75 = NA_real_)
    if (nrow(item_times) >= MIN_N) {
      row$p25 <- quantile(item_times$time, 0.25, names = FALSE)
      row$median <- median(item_times$time)
      row$p75 <- quantile(item_times$time, 0.75, names = FALSE)
    }
    for (group_name in GROUP_LABELS) {
      group_times <- item_times$time[item_times$group == group_name]
      if (length(group_times) >= MIN_N) {
        row[[paste0("median_", group_name)]] <- median(group_times)
      } else {
        row[[paste0("median_", group_name)]] <- NA_real_
      }
    }
    statistics <- bind_rows(statistics, row)
  }
  statistics
}

#' The cells of a table of time_statistics(), with the row labels
time_cells <- function(statistics, labels, digits) {
  cells <- tibble(label = labels,
                  n = latex_number(statistics$n, 0),
                  p25 = statistic_text(statistics$p25, digits),
                  median = statistic_text(statistics$median, digits),
                  p75 = statistic_text(statistics$p75, digits))
  for (group_name in GROUP_LABELS) {
    cells[[group_name]] <- statistic_text(
      statistics[[paste0("median_", group_name)]], digits
    )
  }
  cells
}

time_header <- c(
  paste0(" & \\multicolumn{4}{c}{All experts} & ",
         "\\multicolumn{3}{c}{Median by group} \\\\"),
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-8}",
  paste0(" & N & P25 & Median & P75 & ",
         paste(unname(GROUP_HEADERS_SHORT[GROUP_LABELS]), collapse = " & "),
         " \\\\")
)

# 1. The questionnaire and its sections ----------------------------------------
section_times <- question_times(experts, vignettes) %>%
  mutate(section = as.character(section)) %>%
  group_by(resp_uid, section) %>%
  summarise(time = sum(seconds) / 60, .groups = "drop") %>%
  rename(item = section)
total_times <- experts %>%
  filter(!is.na(time_interview)) %>%
  transmute(resp_uid, item = "Whole questionnaire",
            time = time_interview / 60)
times <- bind_rows(total_times, section_times) %>%
  left_join(experts %>%
              transmute(resp_uid, group = as.character(group)),
            by = "resp_uid", relationship = "many-to-one")

# The sections in the order of the questionnaire, those of the data only
# (intersect(): the values in both, in the order of the first)
sections <- intersect(levels(QUESTIONNAIRE$section), unique(section_times$item))
section_statistics <- time_statistics(times, c("Whole questionnaire",
                                               sections))
section_labels <- c("Whole questionnaire",
                    label_of(sections, SECTION_LABELS_ENGLISH, "section"))
section_cells <- time_cells(section_statistics, latex_text(section_labels),
                            digits = 1)
section_body <- cell_rows(section_cells)
section_body <- c(section_body[1], "\\midrule", section_body[-1])
sections_tex <- table_float(
  column_spec = "lccccccc",
  header = time_header,
  body = section_body,
  caption = "Response times: the questionnaire and its sections (minutes)",
  label = "tab:desc_times_sections",
  notes = paste0(latex_text(paste0(
    "Completed questionnaires of energy advisers (Advisers), HVAC installers ",
    "(HVAC) and chimney sweeps (Sweeps). Whole questionnaire: LimeSurvey's ",
    "total time. A section: the sum of the times of its questions, among the ",
    "experts who answered at least one; some sections were shown to some ",
    "groups only. Not trimmed."
  )), dash_note(section_cells)),
  font_size = TABLE_FONT_SIZE
)
write_tex(sections_tex, file.path(tables_dir, "tab_desc_times_sections.tex"))
excel_sheets$times_sections <- section_statistics %>%
  mutate(label = section_labels, .after = item)
numbers[["descMedianMinutes"]] <- latex_number(section_statistics$median[1], 0)

# 2. The vignettes, by position ------------------------------------------------
vignette_times <- bind_rows(
  vignettes %>%
    filter(!is.na(vig_time)) %>%
    transmute(resp_uid, group = as.character(group),
              item = paste0("Vignette ", vig_num), time = vig_time),
  vignettes %>%
    filter(!is.na(vig_cost_time)) %>%
    transmute(resp_uid, group = as.character(group),
              item = "Cost of the last vignette", time = vig_cost_time)
)
vignette_items <- c(paste0("Vignette ", 1:N_VIGNETTES),
                    "Cost of the last vignette")
vignette_statistics <- time_statistics(vignette_times, vignette_items)
vignette_cells <- time_cells(vignette_statistics, latex_text(vignette_items),
                             digits = 0)
vignettes_tex <- table_float(
  column_spec = "lccccccc",
  header = time_header,
  body = cell_rows(vignette_cells),
  caption = "Response times: the vignettes (seconds)",
  label = "tab:desc_times_vignettes",
  notes = paste0(latex_text(paste0(
    "Completed questionnaires of energy advisers (Advisers), HVAC installers ",
    "(HVAC) and chimney sweeps (Sweeps), both arms. The time of the ",
    "question of a vignette, by its position in the questionnaire; the ",
    "question on the cost was asked after the last vignette. Not trimmed."
  )), dash_note(vignette_cells)),
  font_size = TABLE_FONT_SIZE
)
write_tex(vignettes_tex, file.path(tables_dir, "tab_desc_times_vignettes.tex"))
excel_sheets$times_vignettes <- vignette_statistics

# The figure: the median by position, a line per group and all experts
position_medians <- tibble()
for (position in seq_len(N_VIGNETTES)) {
  position_times <- vignette_times %>%
    filter(item == paste0("Vignette ", position))
  for (group_name in GROUP_COLUMNS) {
    if (group_name == ALL_EXPERTS) {
      group_times <- position_times$time
    } else {
      group_times <- position_times$time[position_times$group == group_name]
    }
    if (length(group_times) >= MIN_N) {
      position_medians <- bind_rows(position_medians, tibble(
        position = position, group = group_name, seconds = median(group_times)
      ))
    }
  }
}
position_medians <- position_medians %>%
  mutate(group = factor(group, levels = GROUP_COLUMNS))
plot_times <- ggplot(position_medians,
                     aes(x = position, y = seconds, colour = group,
                         fill = group, shape = group)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.9, stroke = 0.35, colour = ZEW_DARK) +
  scale_colour_manual(values = GROUP_COLOURS[GROUP_COLUMNS]) +
  scale_fill_manual(values = GROUP_COLOURS[GROUP_COLUMNS]) +
  scale_shape_manual(values = GROUP_SHAPES[GROUP_COLUMNS]) +
  scale_x_continuous(breaks = seq_len(N_VIGNETTES),
                     labels = paste0("Vignette ", seq_len(N_VIGNETTES))) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Seconds (median)") +
  theme_rcl(FONT_SIZE) +
  theme(panel.grid.major.x = element_blank())
save_figure_pdf(plot_times,
                file.path(figures_dir, "fig_desc_vignette_times.pdf"),
                FIGURE_SIZES$full)
numbers[["descNoteVignetteTimes"]] <- latex_text(paste0(
  "Median time of the question of a vignette, by its position in the ",
  "questionnaire. Completed questionnaires of energy advisers, HVAC ",
  "installers and chimney sweeps, both arms; ",
  latex_number(vignette_statistics$n[1], 0), " experts answered the first ",
  "vignette."
))
