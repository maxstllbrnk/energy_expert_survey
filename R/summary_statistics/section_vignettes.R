################################################################################
# section_vignettes.R — Block J: the design of the experiment, the recommendations
#                       and who is in each arm
# Inputs : experts, vignettes (docs/data_contract.md)
# Outputs: tables/tab_design_all.tex, tab_balance_all.tex,
#          tab_recommendations_<group>.tex, tab_arms_all.tex,
#          tab_within_expert_<group>.tex, tab_within_expert_groups_all.tex;
#          figures/fig_recommendations_<group>.pdf,
#          fig_recommendations_groups_all.pdf,
#          fig_recommendations_<attribute>_<group>.pdf, fig_position_all.pdf,
#          fig_investment_cost_<group>.pdf
# Checks : every level of the design and every answer has a label (label_of());
#          the minimum cell, in experts: six vignettes can come from one expert
################################################################################
# own_heating_of_asked(): in section_heating_context.R

#' The design, the recommendations and the arms
#'
#' @param experts   one row per expert of the sample, as in experts.rds
#' @param vignettes one row per expert x vignette of the sample, as in vignettes.rds
#' @param dirs      the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_vignettes <- function(experts, vignettes, dirs) {
  block <- "J. The vignettes"
  exhibits <- tibble()
  design_attributes <- unique(DESIGN_LEVELS$attribute)
  if (!has_columns(vignettes, c("vig_rec", "vig_arm", "expert_group",
                                design_attributes))) {
    return(exhibits)
  }

  # 1. The answered vignettes --------------------------------------------------
  answered <- vignettes %>%
    filter(!is.na(vig_rec), !is.na(vig_arm)) %>%
    mutate(arm = factor(label_of(vig_arm, ARM_LABELS, "arm"), levels = ARM_LABELS),
           technology = label_of(vig_rec, TECHNOLOGY_LABELS, "recommendation"))
  if (n_distinct(answered$resp_uid) < MIN_N) {
    return(exhibits)
  }
  arms_present <- levels(droplevels(answered$arm))
  comma_text <- label_comma()
  # The segments of stacked bars in the order of the colours, which keeps
  # neighbours apart
  technology_order <- intersect(names(TECHNOLOGY_COLOURS), TECHNOLOGY_LABELS)
  vignette_note <- paste(
    "Answered vignettes. The arm follows from Q6a, so the arms differ in the",
    "experts as well as in the choice."
  )
  # The design does not cross these attributes: a share by one of them also shows
  # the others. The note of the table of the design says the same; tests/ checks it.
  uncrossed_attributes <- c("att_current_heating", "att_build_year",
                            "att_heat_demand")

  # 2. The design (all experts only) -------------------------------------------
  design_cells <- tibble()
  attribute_sizes <- integer(0)
  for (attribute_name in design_attributes) {
    attribute_levels <- DESIGN_LEVELS %>%
      filter(attribute == attribute_name)
    level_labels <- attribute_levels$level_label
    names(level_labels) <- attribute_levels$level
    # Stops if the data have a level that the design does not list, or none: the
    # design is known for every vignette (tests/selftest.R)
    label_of(answered[[attribute_name]], level_labels, attribute_name)
    if (anyNA(answered[[attribute_name]])) {
      stop("Answered vignettes without a level of ", attribute_name)
    }
    for (k in seq_len(nrow(attribute_levels))) {
      in_level <- answered[[attribute_name]] == attribute_levels$level[k]
      has_enough <- n_distinct(answered$resp_uid[in_level]) >= MIN_N
      design_cells <- bind_rows(design_cells, tibble(
        level = latex_text(attribute_levels$level_label[k]),
        reference = if_else(attribute_levels$is_reference[k], "Yes", ""),
        n = if_else(has_enough, fmt_int(sum(in_level)), "--"),
        share = if_else(has_enough, fmt_pct(mean(in_level)), "--")
      ))
    }
    attribute_sizes <- c(attribute_sizes, nrow(attribute_levels))
  }
  # The panels of the two tables of the design. match(): the first row of each
  # attribute
  names(attribute_sizes) <- latex_text(
    DESIGN_LEVELS$attribute_label[match(design_attributes, DESIGN_LEVELS$attribute)]
  )
  design_cells <- bind_rows(design_cells, tibble(
    level = c("Answered vignettes", "Experts"), reference = "",
    n = c(fmt_int(nrow(answered)), fmt_int(n_distinct(answered$resp_uid))),
    share = ""
  ))
  file_name <- paste0("tab_design_", group_slug(ALL_EXPERTS), ".tex")
  table_custom(
    design_cells, file.path(dirs$tables, file_name),
    notes = latex_text(paste(
      "Each expert recommends a heating technology for six vignettes, each a",
      "couple and their house, drawn from 128 combinations. Reference: the level",
      "of the reference vignette of the models. The heat demand is the same split",
      "as the renovation status; a picture illustrates the other attributes. The",
      "design does not cross all attributes: no house built in 2000 is heated with",
      "oil or was never renovated (200 kWh/m²). -- means fewer than", MIN_N,
      "experts."
    )),
    col_names = c("Level", "Reference", "Vignettes", "Share"),
    panels = c(attribute_sizes, "Sample" = 2)
  )
  exhibits <- bind_rows(exhibits, exhibit_row(
    block, "design", ALL_EXPERTS, "table", file.path("tables", file_name),
    "The attributes of the vignettes", "main"
  ))

  # 3. The balance of the design (all experts only) ----------------------------
  # Share of each level in each arm x group. A cell below the minimum shows "--"
  # and stays out of the test. The test: Pearson's chi-squared of level x cell, over
  # vignettes, written out so that a small expected count gives no warning.
  grouped <- answered %>%
    filter(!is.na(expert_group)) %>%
    mutate(cell = paste(arm, expert_group, sep = " | "))
  cell_names <- character(0)
  for (arm_label in arms_present) {
    cell_names <- c(cell_names, paste(arm_label, EXPERT_GROUPS, sep = " | "))
  }
  is_shown <- logical(length(cell_names))
  for (k in seq_along(cell_names)) {
    in_cell <- grouped$cell == cell_names[k]
    is_shown[k] <- n_distinct(grouped$resp_uid[in_cell]) >= MIN_N
  }
  cells_tested <- cell_names[is_shown]
  if (length(cells_tested) > 0) {
    tested <- grouped %>%
      filter(cell %in% cells_tested)
    balance_cells <- tibble()
    # The p-value of an attribute stands in the title of its panel: a column of
    # its own would make the table wider than the page
    balance_panels <- attribute_sizes
    for (a in seq_along(design_attributes)) {
      attribute_name <- design_attributes[a]
      attribute_levels <- DESIGN_LEVELS %>%
        filter(attribute == attribute_name)
      # table(): the vignettes of each level x cell
      counts <- table(tested[[attribute_name]], tested$cell)
      p_text <- "--"
      if (nrow(counts) > 1 && ncol(counts) > 1) {
        # outer(): a matrix of all products of the row and the column totals
        expected <- outer(rowSums(counts), colSums(counts)) / sum(counts)
        statistic <- sum((counts - expected)^2 / expected)
        degrees <- (nrow(counts) - 1) * (ncol(counts) - 1)
        # pchisq(): the probability of a larger statistic under the chi-squared
        p_value <- pchisq(statistic, degrees, lower.tail = FALSE)
        p_text <- if_else(p_value < 0.001, "$<$ 0.001",
                          paste("=", fmt_num(p_value, 3)))
      }
      names(balance_panels)[a] <- paste0(names(attribute_sizes)[a], " (p ", p_text,
                                         ")")
      for (k in seq_len(nrow(attribute_levels))) {
        row_cells <- tibble(level = latex_text(attribute_levels$level_label[k]))
        for (cell_name in cell_names) {
          row_cells[[cell_name]] <- "--"
          if (cell_name %in% cells_tested) {
            levels_in_cell <- tested[[attribute_name]][tested$cell == cell_name]
            row_cells[[cell_name]] <- fmt_pct(
              mean(levels_in_cell == attribute_levels$level[k])
            )
          }
        }
        balance_cells <- bind_rows(balance_cells, row_cells)
      }
    }
    sample_cells <- tibble(level = "Answered vignettes")
    for (k in seq_along(cell_names)) {
      sample_cells[[cell_names[k]]] <- if_else(
        is_shown[k], fmt_int(sum(grouped$cell == cell_names[k])), "--"
      )
    }
    balance_cells <- bind_rows(balance_cells, sample_cells)

    # Over two lines, so that the header does not widen the columns
    arm_header <- rep(length(EXPERT_GROUPS), length(arms_present))
    names(arm_header) <- multiline_header(arms_present, width = 24)
    file_name <- paste0("tab_balance_", group_slug(ALL_EXPERTS), ".tex")
    table_custom(
      balance_cells, file.path(dirs$tables, file_name),
      notes = latex_text(paste(
        "Share of each level among the answered vignettes of each arm and group",
        "(energy advisers, HVAC installers, chimney sweeps). Balanced means the",
        "same shares in every column: the levels are not equally frequent by",
        "design. p: Pearson's chi-squared test of level by column, per attribute,",
        "on the columns with enough experts; it treats the vignettes as",
        "independent, though six come from one expert. Experts without a group are",
        "left out. -- means fewer than", MIN_N, "experts."
      )),
      # The short names of the groups, so that the table fits the page
      col_names = c("Level", rep(unname(GROUP_LABELS_SHORT[EXPERT_GROUPS]),
                                 length(arms_present))),
      # " ": no header above the labels
      header = c(" " = 1, arm_header),
      panels = c(balance_panels, "Sample" = 1)
    )
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "balance", ALL_EXPERTS, "table", file.path("tables", file_name),
      "Is the design balanced across arms and groups?", "appendix"
    ))
  }

  # 4. Recommendations by arm: figure and table --------------------------------
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_answered <- subset_group(answered, group)
    if (n_distinct(group_answered$resp_uid) < MIN_N) {
      next
    }
    arms_here <- levels(droplevels(group_answered$arm))

    # One row per arm x technology, so that a figure keeps the place of a bar left
    # out. District heating is no option in the arm without it. An arm below the
    # minimum cell has no shares.
    shares <- tibble()
    for (arm_label in arms_here) {
      in_arm <- group_answered %>%
        filter(arm == arm_label)
      for (technology_label in unname(TECHNOLOGY_LABELS)) {
        is_technology <- in_arm$technology == technology_label
        shares <- bind_rows(shares, tibble(
          arm = arm_label, technology = technology_label,
          n = sum(is_technology),
          n_recommending = n_distinct(in_arm$resp_uid[is_technology]),
          n_arm = nrow(in_arm), n_experts = n_distinct(in_arm$resp_uid),
          is_option = !(arm_label == ARM_LABELS[["nofw"]] &&
                          technology_label == TECHNOLOGY_LABELS[["Fernwärme"]])
        ))
      }
    }
    # Without the row of district heating if it is an option in no arm here
    shares <- shares %>%
      mutate(share = if_else(is_option & n_experts >= MIN_N, n / n_arm,
                             NA_real_)) %>%
      group_by(technology) %>%
      filter(any(is_option)) %>%
      ungroup()
    if (all(is.na(shares$share))) {
      next
    }

    # The figure
    bars <- shares %>%
      mutate(item = factor(technology, levels = TECHNOLOGY_LABELS),
             series = factor(arm, levels = ARM_LABELS))
    plot <- plot_share_bars(bars, ARM_COLOURS, "Share of the answered vignettes",
                            show_legend = TRUE)
    file_name <- paste0("fig_recommendations_", group_slug(group), ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name))
    # str_to_lower(): the name of the arm in small letters, inside a sentence
    arm_totals <- shares %>%
      distinct(arm, n_arm, n_experts) %>%
      filter(n_experts >= MIN_N) %>%
      mutate(text = paste0(comma_text(n_arm), " vignettes of ",
                           comma_text(n_experts), " experts ",
                           str_to_lower(arm)))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "recommendations", group, "figure", file.path("figures", file_name),
      "Which technology do the experts recommend?",
      if_else(group == ALL_EXPERTS, "main", "appendix"),
      note = paste0(vignette_note, " ", paste(arm_totals$text, collapse = "; "),
                    ". District heating is no option in the arm without it.")
    ))

    # The table: N and share in each arm, blank where it is no option. A count
    # stands alone, so it needs the minimum cell itself.
    technologies_here <- intersect(unname(TECHNOLOGY_LABELS), shares$technology)
    cells <- tibble(technology = latex_text(technologies_here))
    sample_rows <- tibble(technology = c("Vignettes", "Experts"))
    for (arm_label in arms_here) {
      in_arm <- shares %>%
        filter(arm == arm_label)
      n_text <- rep("--", nrow(in_arm))
      share_text <- rep("--", nrow(in_arm))
      sample_text <- rep("--", 2)
      if (in_arm$n_experts[1] >= MIN_N) {
        n_text <- if_else(in_arm$n_recommending >= MIN_N, fmt_int(in_arm$n), "--")
        share_text <- fmt_pct(in_arm$share)
        sample_text <- c(fmt_int(in_arm$n_arm[1]), fmt_int(in_arm$n_experts[1]))
      }
      n_text[!in_arm$is_option] <- ""
      share_text[!in_arm$is_option] <- ""
      cells[[paste0("n_", arm_label)]] <- n_text
      cells[[paste0("share_", arm_label)]] <- share_text
      sample_rows[[paste0("n_", arm_label)]] <- sample_text
      sample_rows[[paste0("share_", arm_label)]] <- ""
    }
    arm_header <- rep(2, length(arms_here))
    names(arm_header) <- multiline_header(arms_here, width = 24)
    file_name <- paste0("tab_recommendations_", group_slug(group), ".tex")
    table_custom(
      bind_rows(cells, sample_rows), file.path(dirs$tables, file_name),
      notes = latex_text(paste(
        vignette_note, "Blank: no option in this arm. -- means fewer than", MIN_N,
        "experts."
      )),
      col_names = c("", rep(c("N", "Share"), length(arms_here))),
      header = c(" " = 1, arm_header),
      panels = c("Recommendation" = length(technologies_here), "Sample" = 2)
    )
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "recommendations", group, "table", file.path("tables", file_name),
      "Which technology do the experts recommend?", "appendix"
    ))
  }

  # 5. Recommendations by group, a panel per arm (all experts only) ------------
  group_answers <- answered %>%
    filter(!is.na(expert_group)) %>%
    transmute(resp_uid,
              panel = arm,
              bar = factor(expert_group, levels = EXPERT_GROUPS),
              segment = factor(technology, levels = technology_order))
  bars <- stack_answers(group_answers)
  if (nrow(bars) > 0) {
    plot <- plot_stacked_shares(bars, TECHNOLOGY_COLOURS,
                                "Share of the answered vignettes")
    file_name <- paste0("fig_recommendations_groups_",
                        group_slug(ALL_EXPERTS), ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "recommendations_groups", ALL_EXPERTS, "figure",
      file.path("figures", file_name),
      "Which technology does each group recommend?", "main",
      note = paste(vignette_note, "n: vignettes. A bar of fewer than", MIN_N,
                   "experts is left out.")
    ))
  }

  # 6. Recommendations by each level of each attribute -------------------------
  for (attribute_name in design_attributes) {
    attribute_levels <- DESIGN_LEVELS %>%
      filter(attribute == attribute_name)
    level_labels <- attribute_levels$level_label
    names(level_labels) <- attribute_levels$level
    # The name of the exhibit, without the prefix of the column. str_remove():
    # the text without the part that matches
    id <- paste0("recommendations_", str_remove(attribute_name, "^att_"))
    attribute_note <- paste(vignette_note, "n: vignettes. A bar of fewer than",
                            MIN_N, "experts is left out.")
    if (attribute_name %in% uncrossed_attributes) {
      attribute_note <- paste(
        attribute_note, "No house built in 2000 is heated with oil or was never",
        "renovated, so these shares also show the other attributes."
      )
    }
    for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
      group_answered <- subset_group(answered, group)
      level_values <- group_answered[[attribute_name]]
      level_answers <- tibble(
        resp_uid = group_answered$resp_uid,
        panel = group_answered$arm,
        bar = factor(label_of(level_values, level_labels, attribute_name),
                     levels = unname(level_labels)),
        segment = factor(group_answered$technology, levels = technology_order)
      )
      bars <- stack_answers(level_answers)
      if (nrow(bars) == 0) {
        next
      }
      plot <- plot_stacked_shares(bars, TECHNOLOGY_COLOURS,
                                  "Share of the answered vignettes")
      file_name <- paste0("fig_", id, "_", group_slug(group), ".pdf")
      save_figure(plot, file.path(dirs$figures, file_name))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, id, group, "figure", file.path("figures", file_name),
        paste0("Recommendations by ",
               str_to_lower(attribute_levels$attribute_label[1])),
        "appendix", note = attribute_note
      ))
    }
  }

  # 7. Who is in each arm (all experts only) -----------------------------------
  # An arm below the minimum cell is no column, as its number of experts would
  # stand in the table
  arm_experts <- tibble(arm = character())
  if ("arm" %in% names(experts)) {
    arm_experts <- experts %>%
      filter(!is.na(arm)) %>%
      group_by(arm) %>%
      filter(n() >= MIN_N) %>%
      ungroup()
  }
  if (n_distinct(arm_experts$arm) > 1) {
    # The names of the arms over two lines, so that they do not widen the columns
    arm_headers <- multiline_header(ARM_LABELS, width = 24)
    names(arm_headers) <- names(ARM_LABELS)
    arm_experts <- arm_experts %>%
      mutate(arm_label = factor(label_of(arm, arm_headers, "arm"),
                                levels = arm_headers))
    if (has_columns(arm_experts, c("own_heating", "last_question_position"))) {
      arm_experts <- arm_experts %>%
        mutate(own_heating_asked = own_heating_of_asked(arm_experts))
    }
    arm_rows <- bind_rows(
      tibble(panel = "Expert group", variable = "expert_group", kind = "level",
             level = EXPERT_GROUPS, label = EXPERT_GROUPS, digits = 1),
      tibble(panel = "Reported price per kWh of the energy carrier (cent)",
             variable = paste0("price_", names(FUEL_LABELS), "_kwh"), kind = "num",
             level = NA_character_, label = unname(FUEL_LABELS), digits = 1),
      tibble(panel = "Climate", variable = "hdd", kind = "num",
             level = NA_character_,
             label = paste0("Heating degree days, ", HDD_YEAR), digits = 0),
      tibble(panel = "Heating of the expert's home (Q20)",
             variable = "own_heating_asked", kind = "level",
             level = names(OWN_HEATING_LABELS), label = unname(OWN_HEATING_LABELS),
             digits = 1)
    )
    check_listed_answers(arm_rows, arm_experts)
    n_answers <- answers_per_row(arm_rows, arm_experts)
    if (max(n_answers) >= MIN_N) {
      file_name <- paste0("tab_arms_", group_slug(ALL_EXPERTS), ".tex")
      summary_table(
        arm_experts, arm_rows[n_answers > 0, ], file.path(dirs$tables, file_name),
        notes = paste(
          "Experts by arm, also those who stopped before the vignettes. The arm",
          "follows from the answer to Q6a and is not randomized: district heating",
          "available or planned leads to the arm with the option. Prices and degree",
          "days as in the tables of the prices and the heating context. Q20: among",
          "the experts who got as far.", cell_note()
        ),
        by = "arm_label", stats = c("n", "mean", "sd")
      )
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "arms", ALL_EXPERTS, "table", file.path("tables", file_name),
        "Who is in each arm?", "main"
      ))
    }
  }

  # 8. Variation within experts ------------------------------------------------
  # Over the experts who answered all six vignettes: one who recommends a single
  # technology throughout says nothing about how the attributes matter, and pushes
  # the random constants
  by_expert <- answered %>%
    group_by(resp_uid, expert_group, vig_arm) %>%
    summarise(n_answered = n(),
              n_technologies = n_distinct(technology)) %>%
    ungroup() %>%
    filter(n_answered == N_VIGNETTES) %>%
    mutate(one_technology = if_else(n_technologies == 1, 1, 0))
  within_panels <- tibble(arm_code = "both", panel = "Both arms")
  if (length(arms_present) > 1) {
    within_panels <- bind_rows(
      within_panels,
      tibble(arm_code = names(ARM_LABELS), panel = unname(ARM_LABELS))
    )
  } else {
    within_panels$panel <- arms_present
  }
  within_rows <- tibble()
  for (k in seq_len(nrow(within_panels))) {
    arm_code <- within_panels$arm_code[k]
    in_panel <- arm_code == "both" | by_expert$vig_arm == arm_code
    column_n <- paste0("n_technologies_", arm_code)
    column_one <- paste0("one_technology_", arm_code)
    by_expert[[column_n]] <- if_else(in_panel, by_expert$n_technologies,
                                     NA_integer_)
    by_expert[[column_one]] <- if_else(in_panel, by_expert$one_technology,
                                       NA_real_)
    within_rows <- bind_rows(within_rows, tibble(
      panel = within_panels$panel[k],
      variable = c(column_n, column_one), kind = c("num", "dummy"),
      level = NA_character_,
      label = c("Technologies recommended", "One technology throughout"),
      digits = c(2, 1)
    ))
  }
  within_notes <- paste(
    "Experts who answered all six vignettes. Technologies recommended: how many",
    "different ones across the six vignettes. An expert with one technology",
    "throughout has no variation in the choice, which matters for the random",
    "constants of the models."
  )
  exhibits <- bind_rows(exhibits, summary_exhibits(
    by_expert, within_rows, "within_expert", "Variation within experts",
    within_notes, block, "main", dirs
  ))

  # 9. Recommendations by the position of the vignette (all experts only) ------
  position_labels <- paste("Vignette", seq_len(N_VIGNETTES))
  position_answers <- answered %>%
    transmute(resp_uid,
              panel = arm,
              bar = factor(position_labels[vig_num], levels = position_labels),
              segment = factor(technology, levels = technology_order))
  bars <- stack_answers(position_answers)
  if (nrow(bars) > 0) {
    plot <- plot_stacked_shares(bars, TECHNOLOGY_COLOURS,
                                "Share of the answered vignettes")
    file_name <- paste0("fig_position_", group_slug(ALL_EXPERTS), ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "position", ALL_EXPERTS, "figure", file.path("figures", file_name),
      "Recommendations by the position of the vignette", "appendix",
      note = paste(vignette_note, "The experts answer the six vignettes in this",
                   "order. If some broke off, the later vignettes rest on other",
                   "experts. n: vignettes. A bar of fewer than", MIN_N,
                   "experts is left out.")
    ))
  }

  # 10. The expected investment cost -------------------------------------------
  if ("vig_cost" %in% names(answered)) {
    cost_answers <- answered %>%
      filter(!is.na(vig_cost)) %>%
      mutate(panel = "",
             bar = factor(technology, levels = TECHNOLOGY_LABELS),
             segment = factor(label_of(vig_cost, COST_LABELS, "answer on the cost"),
                              levels = COST_LABELS))
    for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
      bars <- stack_answers(subset_group(cost_answers, group))
      if (nrow(bars) == 0) {
        next
      }
      plot <- plot_stacked_shares(bars, COST_COLOURS, "Share of the answers",
                                  legend_rows = 2)
      file_name <- paste0("fig_investment_cost_", group_slug(group), ".pdf")
      save_figure(plot, file.path(dirs$figures, file_name))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "investment_cost", group, "figure",
        file.path("figures", file_name),
        "Expected investment cost, by the technology recommended", "appendix",
        note = paste(
          "Asked after the last vignette: the expected cost of buying and",
          "installing the system, without subsidies, by the technology recommended",
          "there, both arms. The houses differ between the bars. A bar of fewer",
          "than", MIN_N, "experts is left out."
        )
      ))
    }
  }
  exhibits
}
