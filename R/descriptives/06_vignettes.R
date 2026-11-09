################################################################################
# 06_vignettes.R — the vignettes: their design, the recommendations by arm and
#                  group, and who is in each arm
# Inputs : experts, vignettes (02_data.R)
# Outputs: figures/fig_desc_recommendations.pdf;
#          tables/tab_desc_design.tex, tab_desc_recommendations.tex,
#          tab_desc_arms.tex
# Checks : every attribute value of a vignette is a level of DESIGN_LEVELS;
#          heat demand and renovation status agree; district heating is
#          recommended only where it is an option; every recommendation has a
#          label
################################################################################

answered_vignettes <- vignettes %>%
  filter(!is.na(vig_rec)) %>%
  mutate(arm = as.character(vig_arm),
         technology = as.character(vig_rec),
         group = as.character(group))

# 1. The design (V0) -----------------------------------------------------------
# A value that is not a level of DESIGN_LEVELS would be missing from the
# table; a heat demand with the wrong renovation status would be mislabelled
for (attribute_name in unique(DESIGN_LEVELS$attribute)) {
  known_levels <- DESIGN_LEVELS$level[DESIGN_LEVELS$attribute == attribute_name]
  unknown_levels <- setdiff(unique(answered_vignettes[[attribute_name]]),
                            known_levels)
  if (length(unknown_levels) > 0) {
    stop(attribute_name, " has values that are not in DESIGN_LEVELS: ",
         paste(unknown_levels, collapse = ", "))
  }
}
for (demand in names(RENOVATION_OF_DEMAND)) {
  renovation <- answered_vignettes$att_renovation[
    answered_vignettes$att_heat_demand == demand]
  # grepl(fixed = TRUE): does the text contain these words?
  if (!all(grepl(RENOVATION_OF_DEMAND[[demand]], renovation, fixed = TRUE))) {
    stop("Vignettes with a heat demand of ", demand, " whose renovation ",
         "status is not '", RENOVATION_OF_DEMAND[[demand]], "'")
  }
}
# The note of the table says that no house built in 2000 is unrenovated
if (any(answered_vignettes$att_build_year == "2000" &
        answered_vignettes$att_heat_demand == "200")) {
  stop("Vignettes of a house built in 2000 with 200 kWh/m2: the note of ",
       "tab_desc_design.tex is wrong")
}

design_cells <- tibble()
for (attribute_name in unique(DESIGN_LEVELS$attribute)) {
  attribute_levels <- DESIGN_LEVELS %>%
    filter(attribute == attribute_name)
  reference_level <- attribute_levels$level_label[attribute_levels$is_reference]
  other_levels <- attribute_levels$level_label[!attribute_levels$is_reference]
  design_cells <- bind_rows(design_cells, tibble(
    label = latex_text(attribute_levels$attribute_label[1]),
    reference = latex_text(reference_level),
    other = latex_text(paste(other_levels, collapse = "; "))
  ))
}
design_tex <- table_float(
  column_spec = "lll",
  header = " & Reference vignette & Other levels \\\\",
  body = cell_rows(design_cells),
  caption = "The attributes of the vignettes",
  label = "tab:desc_design",
  notes = latex_text(paste0(
    "Each expert recommends a heating technology for six vignettes of a ",
    "couple and their house. The reference vignette is the one of the ",
    "marginal effects of the models. No vignette has a house built in 2000 ",
    "that was never renovated (200 kWh/m²)."
  )),
  font_size = TABLE_FONT_SIZE
)
write_tex(design_tex, file.path(tables_dir, "tab_desc_design.tex"))

# The share of the answered vignettes with each level, by arm (Excel only):
# how balanced the design is in the data
design_balance <- tibble()
for (attribute_name in unique(DESIGN_LEVELS$attribute)) {
  level_counts <- tibble(arm = answered_vignettes$arm,
                         level = answered_vignettes[[attribute_name]]) %>%
    count(arm, level) %>%
    group_by(arm) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    mutate(attribute = attribute_name, .before = 1)
  design_balance <- bind_rows(design_balance, level_counts)
}
excel_sheets$design <- DESIGN_LEVELS
excel_sheets$design_balance <- design_balance

# 2. Recommendations by arm and group (V1) -------------------------------------
unknown_technologies <- setdiff(unique(answered_vignettes$technology),
                                names(TECHNOLOGY_LABELS))
if (length(unknown_technologies) > 0) {
  stop("Recommendations without a label: ",
       paste(unknown_technologies, collapse = ", "))
}
# District heating is a choice in the fw arm only (between-subject design)
if (any(answered_vignettes$arm == "nofw" &
        answered_vignettes$technology == "Fernwärme")) {
  stop("District heating recommended in the arm without it")
}

answered_vignettes_all_experts <- answered_vignettes %>%
  mutate(group = ALL_EXPERTS)
recommendation_counts <- bind_rows(answered_vignettes,
                                   answered_vignettes_all_experts) %>%
  count(arm, group, technology)
arm_group_totals <- bind_rows(answered_vignettes,
                              answered_vignettes_all_experts) %>%
  group_by(arm, group) %>%
  summarise(vignettes = n(), experts = n_distinct(resp_uid),
            .groups = "drop")

# Every technology in every arm and group, also those never recommended
# there; district heating is no option without it (NA)
recommendations <- tibble()
for (arm_code in names(ARMS)) {
  for (group_name in GROUP_COLUMNS) {
    recommendations <- bind_rows(recommendations, tibble(
      arm = arm_code, group = group_name,
      technology = names(TECHNOLOGY_LABELS)
    ))
  }
}
recommendations <- recommendations %>%
  left_join(recommendation_counts, by = c("arm", "group", "technology")) %>%
  left_join(arm_group_totals, by = c("arm", "group")) %>%
  mutate(n = coalesce(n, 0L),
         vignettes = coalesce(vignettes, 0L),
         experts = coalesce(experts, 0L),
         is_option = !(arm == "nofw" & technology == "Fernwärme"),
         share = if_else(is_option & vignettes >= MIN_N,
                         n / vignettes, NA_real_))

# The figure: a panel per group, the two arms side by side in each row. Every
# technology in every group and arm; no option, or a share left out (NA),
# draws no bar.
plot_data <- recommendations %>%
  filter(group != ALL_EXPERTS) %>%
  mutate(item = factor(unname(TECHNOLOGY_LABELS[technology]),
                       levels = unname(TECHNOLOGY_LABELS)),
         group = factor(group, levels = GROUP_LABELS),
         series = factor(unname(ARMS[arm]), levels = ARMS))
plot_recommendations <- plot_group_shares(
  plot_data, x_title = "Share of the answered vignettes (%)",
  series_colours = ARM_COLOURS
)
save_figure_pdf(plot_recommendations,
                file.path(figures_dir, "fig_desc_recommendations.pdf"),
                FIGURE_SIZES$full)

# The table: a column per group in each arm
recommendation_cells <- tibble(
  label = latex_text(c(unname(TECHNOLOGY_LABELS), "Vignettes", "Experts"))
)
for (arm_code in names(ARMS)) {
  for (group_name in GROUP_COLUMNS) {
    cell_data <- recommendations %>%
      filter(arm == arm_code, group == group_name)
    stopifnot(identical(cell_data$technology, names(TECHNOLOGY_LABELS)))
    share_text <- statistic_text(100 * cell_data$share, 1)
    share_text[!cell_data$is_option] <- ""
    recommendation_cells[[paste0(arm_code, "_", group_name)]] <- c(
      share_text,
      latex_number(cell_data$vignettes[1], 0),
      latex_number(cell_data$experts[1], 0)
    )
  }
}
n_technologies <- length(TECHNOLOGY_LABELS)
recommendation_body <- cell_rows(recommendation_cells)
recommendation_body <- c(recommendation_body[seq_len(n_technologies)],
                         "\\midrule",
                         recommendation_body[-seq_len(n_technologies)])
short_headers <- unname(GROUP_HEADERS_SHORT[GROUP_COLUMNS])
n_group_columns <- length(GROUP_COLUMNS)
recommendations_tex <- table_float(
  column_spec = paste0("l", strrep("c", 2 * n_group_columns)),
  header = c(
    paste0(" & \\multicolumn{", n_group_columns, "}{c}{", ARMS[["fw"]],
           "} & \\multicolumn{", n_group_columns, "}{c}{", ARMS[["nofw"]],
           "} \\\\"),
    paste0("\\cmidrule(lr){2-", 1 + n_group_columns, "} \\cmidrule(lr){",
           2 + n_group_columns, "-", 1 + 2 * n_group_columns, "}"),
    paste0(" & ", paste(c(short_headers, short_headers), collapse = " & "),
           " \\\\")
  ),
  body = recommendation_body,
  caption = "Which technology do the experts recommend?",
  label = "tab:desc_recommendations",
  notes = paste0(latex_text(paste0(
    "Answered vignettes of the completed questionnaires of energy advisers ",
    "(Advisers), HVAC installers (HVAC) and chimney sweeps (Sweeps); share ",
    "of the vignettes, in percent. The arm follows from Q6a: experts with ",
    "district heating available or planned in their service area could ",
    "recommend it. The arms differ in the experts as well as in the choice."
  )), dash_note(recommendation_cells)),
  font_size = TABLE_FONT_SIZE
)
write_tex(recommendations_tex,
          file.path(tables_dir, "tab_desc_recommendations.tex"))

totals_fw <- arm_group_totals %>%
  filter(arm == "fw", group == ALL_EXPERTS)
totals_nofw <- arm_group_totals %>%
  filter(arm == "nofw", group == ALL_EXPERTS)
numbers[["descNoteRecommendations"]] <- latex_text(paste0(
  "Share of the answered vignettes: ", latex_number(totals_fw$vignettes, 0),
  " vignettes of ", latex_number(totals_fw$experts, 0), " experts with the ",
  "district-heating option, ", latex_number(totals_nofw$vignettes, 0),
  " of ", latex_number(totals_nofw$experts, 0), " experts without it. The ",
  "arm follows from Q6a, so the experts of the two arms differ too."
))
# The workbook leaves out what the table and the figure leave out: the count
# behind a share that rests on fewer than MIN_N vignettes
excel_sheets$recommendations <- recommendations %>%
  mutate(n = if_else(vignettes >= MIN_N, n, NA_integer_))

# 3. Who is in each arm (appendix) ---------------------------------------------
# The arm follows from Q6a, so the experts of the two arms can differ in more
# than the choice they had
check_share_rows(experts, ARM_ROWS, complete = FALSE)
arm_cells <- tibble(panel = ARM_ROWS$panel,
                    label = latex_text(ARM_ROWS$label))
arm_results <- tibble()
for (arm_code in names(ARMS)) {
  arm_experts <- experts %>%
    filter(arm == arm_code)
  arm_stats <- summary_statistics(arm_experts, ARM_ROWS)
  # Each row with its own decimals
  arm_text <- character(nrow(arm_stats))
  for (k in seq_len(nrow(arm_stats))) {
    arm_text[k] <- statistic_text(arm_stats$mean[k], arm_stats$digits[k])
  }
  arm_cells[[arm_code]] <- arm_text
  arm_results <- bind_rows(arm_results, arm_stats %>%
                             mutate(arm = arm_code, experts = nrow(arm_experts),
                                    .before = 1))
}
n_experts_by_arm <- character(length(ARMS))
for (a in seq_along(ARMS)) {
  n_experts_by_arm[a] <- latex_number(sum(experts$arm == names(ARMS)[a],
                                          na.rm = TRUE), 0)
}
arm_body <- character(0)
arm_panels <- unique(arm_cells$panel)
for (p in seq_along(arm_panels)) {
  if (p > 1) arm_body <- c(arm_body, PANEL_SPACE)
  arm_body <- c(arm_body, panel_row(latex_text(arm_panels[p]), 3),
                cell_rows(arm_cells %>%
                            filter(panel == arm_panels[p]) %>%
                            select(-panel)))
}
arm_body <- c(arm_body, "\\midrule",
              table_line(c("Experts", n_experts_by_arm)))
arms_tex <- table_float(
  column_spec = paste0("p{", LABEL_WIDTH_GROUPS, "}cc"),
  header = paste0(" & ", header_cell("With the\\\\district-heating option"),
                  " & ", header_cell("Without the\\\\district-heating option"),
                  " \\\\"),
  body = arm_body,
  caption = "Who is in each arm?",
  label = "tab:desc_arms",
  notes = paste0(latex_text(paste0(
    "Completed questionnaires of the three groups, by the arm of their ",
    "vignettes (from Q6a). Prices per kWh of the energy carrier (district ",
    "heating: of heat), converted and cleaned as in the table of the prices."
  )), dash_note(arm_cells)),
  font_size = TABLE_FONT_SIZE
)
write_tex(arms_tex, file.path(tables_dir, "tab_desc_arms.tex"))
excel_sheets$arms <- arm_results
