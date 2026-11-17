################################################################################
# section_models.R — the exhibits of one model: its coefficients and random
#                    constants, its effects, and for the main model also its
#                    other sets, the households, the definitions of an effect
#                    and the differences between the groups
# Inputs : collect_results(), for one run and model
# Outputs: in dirs$tables and dirs$figures: tab_coefficients_<model>,
#          tab_constants_<model>, tab_effects_<model>_<set>_<profile>,
#          fig_effects_<model>_<set>_<block>_<all or groups>; for the main model
#          also tab_effects_<model>_<set>_<outcome>, fig_/tab_baseline_<model>,
#          fig_households_<model>_<block>, fig_definitions_<model>_<outcome>,
#          tab_group_differences_<model>_<group>_vs_<group>
# Checks : only cells that collect_results() shows; a difference between groups
#          only where both cells are shown
################################################################################

#' The exhibits of one model of one run
#'
#' @param collected  collect_results()
#' @param run_name   the run, an estimation folder
#' @param model_name the model
#' @param full       TRUE: every exhibit (the main model); FALSE: the core ones
#' @param placement  "main" or "appendix", for the headline exhibits
#' @param dirs       the folders of the run's exhibits: tables, figures
#' @return list: exhibits (exhibit_row()s of the files written), problems (none)
section_models <- function(collected, run_name, model_name, full, placement,
                           dirs) {
  # 1. What the exhibits of the model share ------------------------------------
  block <- if (full) "A. The main model" else "D. Every other model"
  block_robust <- "B. Robustness of the main model's effects"
  run_row <- collected$runs[collected$runs$run == run_name, ]
  model_row <- collected$models[collected$models$run == run_name &
                                  collected$models$model == model_name, ]
  model_outcomes <- collected$outcomes %>%
    filter(run == run_name, model == model_name) %>%
    arrange(position)
  outcome_levels <- model_outcomes$outcome_label
  reference_label <- OUTCOME_LABELS[[model_row$reference]]
  technologies <- model_outcomes %>%
    filter(outcome != model_row$reference)
  model_title <- paste0(model_text(model_name), " ", MODEL_LABELS[[model_name]])
  # str_to_lower(): in small letters, inside a sentence
  model_note <- paste0("Model ", model_text(model_name), ": ", model_row$label,
                       "; a mixed logit with the ", str_to_lower(reference_label),
                       " as reference outcome, standard errors clustered by expert.")
  cell_note <- paste0("--: rests on fewer than ", MIN_N, " experts.")
  set_rows <- collected$sets %>%
    filter(run == run_name, model == model_name)
  exhibits <- tibble()

  # 2. The coefficients --------------------------------------------------------
  # Rows: each group's own constant, the terms of the household and the expert by
  # technology, the terms with one coefficient for all technologies, and the
  # standard deviations of the random constants. A row shows a group's own value.
  term_order <- TERM_LABELS %>%
    mutate(order = row_number()) %>%
    rename(row_label = label)
  # The order of the groups' rows: match() below gives each its position here
  group_order <- c(EXPERT_GROUPS, "Other experts", ALL_EXPERTS)
  technology_labels <- technologies %>%
    select(technology = outcome, column = outcome_label)
  coefficient_rows <- collected$coefficients %>%
    filter(run == run_name, model == model_name) %>%
    rename(term = column)
  by_technology <- coefficient_rows %>%
    filter(!is.na(technology), !term %in% c("(Intercept)", GROUP_COLUMNS)) %>%
    left_join(term_order, by = "term", relationship = "many-to-one") %>%
    left_join(technology_labels, by = "technology", relationship = "many-to-one") %>%
    transmute(row_key = paste0("term|", term), row_label, panel, order, column,
              estimate, se, p, status)
  for_all <- coefficient_rows %>%
    filter(is.na(technology)) %>%
    left_join(term_order, by = "term", relationship = "many-to-one") %>%
    transmute(row_key = paste0("term|", term), row_label, panel, order,
              column = "All technologies", estimate, se, p, status)
  own_constants <- collected$own_constants %>%
    filter(run == run_name, model == model_name) %>%
    left_join(technology_labels, by = "technology", relationship = "many-to-one") %>%
    transmute(row_key = paste0("constant|", group), row_label = group,
              panel = "Constants", order = match(group, group_order) - 100,
              column, estimate, se, p, status)
  sd_rows <- collected$constants %>%
    filter(run == run_name, model == model_name, type == "sd") %>%
    left_join(technology_labels, by = c("technology_1" = "technology"),
              relationship = "many-to-one") %>%
    transmute(row_key = paste0("sd|", constants_of), row_label = constants_of,
              panel = "Standard deviation of the random constant",
              order = 1000 + match(constants_of, group_order), column, estimate,
              se, p = NA_real_, status)
  # By panel first, so that a term added at the end of TERM_LABELS still joins its
  # panel
  cells <- bind_rows(own_constants, by_technology, for_all, sd_rows) %>%
    arrange(match(panel, COEFFICIENT_PANELS), order)
  cells$row_key <- factor(cells$row_key, levels = unique(cells$row_key))
  column_levels <- technology_labels$column
  if (nrow(for_all) > 0) {
    column_levels <- c(column_levels, "All technologies")
  }
  cells$column <- factor(cells$column, levels = column_levels)
  constant_text <- "The constants are each group's own."
  if (any(own_constants$row_label == ALL_EXPERTS)) {
    constant_text <- "The constants are those of all experts."
  } else if (any(coefficient_rows$term == "(Intercept)")) {
    constant_text <- paste0("The constants are each group's own: the constant ",
                            "plus the group's shift.")
  }
  association <- association_note(cells$panel[cells$status != "not applicable"])
  notes <- paste0(
    "Sample: ", run_row$sample_label, ". Coefficients of the mixed logit: the ",
    "utility of each recommendation relative to the ",
    str_to_lower(reference_label), ". ", constant_text, association,
    " Standard errors clustered by expert in parentheses; for the standard deviations of the ",
    "random constants from ", fmt_int(model_row$n_draws_kr), " Krinsky-Robb ",
    "draws. ", fmt_int(model_row$n_experts), " experts, ",
    fmt_int(model_row$n_choices), " recommendations, ",
    model_row$n_parameters, " parameters, log-likelihood ",
    signed_text(model_row$log_likelihood, 1), ", McFadden R² ",
    formatC(model_row$mcfadden_r2, format = "f", digits = 3), "; ",
    fmt_int(model_row$n_draws_model), " Halton draws per expert. ", model_note,
    " ", cell_note
  )
  file_name <- paste0("tab_coefficients_", model_name, ".tex")
  if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                      digits = 2)) {
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "coefficients", ALL_EXPERTS, "table", file.path("tables", file_name),
      paste0("Coefficients, ", model_title), "appendix"
    ))
  }

  # 3. The random constants ----------------------------------------------------
  # Standard deviations, and the correlations as a lower triangle: the row's
  # constant with the column's, both in the order of the outcomes
  constant_rows <- collected$constants %>%
    filter(run == run_name, model == model_name)
  technology_order <- technologies$outcome
  position_1 <- match(constant_rows$technology_1, technology_order)
  position_2 <- match(constant_rows$technology_2, technology_order)
  is_later <- !is.na(position_2) & position_2 > position_1
  row_technology <- if_else(is_later, constant_rows$technology_2,
                            constant_rows$technology_1)
  column_technology <- if_else(is_later, constant_rows$technology_1,
                               constant_rows$technology_2)
  cells <- constant_rows %>%
    mutate(row_technology = row_technology,
           column = if_else(type == "sd", "Standard deviation",
                            unname(OUTCOME_LABELS[column_technology])),
           order = 100 * match(constants_of, group_order) +
             match(row_technology, technology_order)) %>%
    arrange(order) %>%
    transmute(row_key = paste(constants_of, row_technology, sep = "|"),
              row_label = unname(OUTCOME_LABELS[row_technology]),
              panel = paste0("Random constants: ", constants_of), column,
              estimate, se, p = NA_real_, status)
  cells$row_key <- factor(cells$row_key, levels = unique(cells$row_key))
  correlation_columns <- unname(OUTCOME_LABELS[technology_order[-length(technology_order)]])
  cells$column <- factor(cells$column,
                         levels = c("Standard deviation", correlation_columns))
  notes <- paste0(
    "Sample: ", run_row$sample_label, ". Standard deviations of the random ",
    "constants, and the correlations between the constants of the row and of ",
    "the column, with Krinsky-Robb standard errors in parentheses (",
    fmt_int(model_row$n_draws_kr), " draws). ", model_note, " ", cell_note
  )
  file_name <- paste0("tab_constants_", model_name, ".tex")
  if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                      digits = 2, stars = FALSE)) {
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "constants", ALL_EXPERTS, "table", file.path("tables", file_name),
      paste0("Random constants, ", model_title), "appendix"
    ))
  }

  # 4. The effects of the main set ---------------------------------------------
  # A table per profile (the core: all experts), and for the main model one per
  # outcome with the profiles side by side. The last column of a table per
  # profile: the experts each effect averages over.
  main_set <- set_rows[set_rows$set == MAIN_AMES, ]
  if (nrow(main_set) == 0) {
    return(list(exhibits = exhibits, problems = character(0)))
  }
  main_effects <- collected$effects %>%
    filter(run == run_name, model == model_name, set == MAIN_AMES)
  main_baselines <- collected$baselines %>%
    filter(run == run_name, model == model_name, set == MAIN_AMES)
  profiles <- unique(main_baselines$profile)
  # intersect(): all experts, if the set has that profile
  core_profiles <- intersect(ALL_EXPERTS, profiles)
  if (length(core_profiles) == 0) {
    core_profiles <- profiles[1]
  }
  tabled_profiles <- if (full) profiles else core_profiles
  set_label <- SET_LABELS$label[SET_LABELS$set == MAIN_AMES]
  for (profile_name in tabled_profiles) {
    profile_effects <- main_effects %>%
      filter(profile == profile_name) %>%
      mutate(column = outcome_label)
    profile_baselines <- main_baselines %>%
      filter(profile == profile_name) %>%
      mutate(column = outcome_label)
    cells <- effect_cells(profile_effects, profile_baselines)
    cells$column <- factor(cells$column, levels = outcome_levels)
    # The experts behind each row: the same for every outcome of an effect
    experts <- bind_rows(
      profile_baselines %>% transmute(row_key = "baseline", n_experts, status),
      profile_effects %>% transmute(row_key = effect, n_experts, status)
    ) %>%
      filter(status != "not applicable") %>%
      group_by(row_key) %>%
      summarise(n_experts = first(n_experts), status = first(status)) %>%
      ungroup() %>%
      mutate(value = if_else(status == "shown", fmt_int(n_experts), "--"))
    association <- association_note(
      profile_effects$block[profile_effects$status != "not applicable"]
    )
    notes <- paste0("Sample: ", run_row$sample_label, " (", profile_name, "). ",
                    set_label, ". ", set_note(main_set), association,
                    " The last column gives ",
                    "the experts each effect averages over. ", model_note, " ",
                    cell_note, " Blank: the change does not apply to them.")
    file_name <- paste0("tab_effects_", model_name, "_",
                        slug_of(profile_name), ".tex")
    if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                        row_info = experts, row_info_title = "Experts")) {
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "effects", profile_name, "table", file.path("tables", file_name),
        paste0("Effects on the recommendation, ", model_title),
        if (profile_name == core_profiles) placement else "appendix"
      ))
    }
  }
  if (full) {
    for (k in seq_len(nrow(model_outcomes))) {
      outcome_cells <- effect_cells(
        main_effects %>%
          filter(outcome == model_outcomes$outcome[k]) %>%
          mutate(column = profile),
        main_baselines %>%
          filter(outcome == model_outcomes$outcome[k]) %>%
          mutate(column = profile)
      )
      outcome_cells$column <- factor(outcome_cells$column, levels = profiles)
      association <- association_note(
        outcome_cells$panel[outcome_cells$status != "not applicable"]
      )
      notes <- paste0("Sample: ", run_row$sample_label, ". ", set_label, ", ",
                      "on the probability of ",
                      outcome_phrase(model_outcomes$outcome_label[k]), ". ",
                      set_note(main_set), association, " ", model_note, " ",
                      cell_note,
                      " Blank: the change does not apply to them.")
      file_name <- paste0("tab_effects_", model_name, "_",
                          slug_of(model_outcomes$outcome_label[k]), ".tex")
      if (table_estimates(outcome_cells, file.path(dirs$tables, file_name),
                          notes)) {
        exhibits <- bind_rows(exhibits, exhibit_row(
          block, "effects_by_group", ALL_EXPERTS, "table",
          file.path("tables", file_name),
          paste0("Effects on ",
                 outcome_phrase(model_outcomes$outcome_label[k]), ", by group, ",
                 model_title),
          if (model_outcomes$outcome[k] == model_row$reference) placement else
            "appendix"
        ))
      }
    }
  }

  # 5. Figures of the main set -------------------------------------------------
  # One per block of effects: all experts, and for the main model also the groups
  figure_versions <- list(all = core_profiles)
  if (full) {
    figure_versions$groups <- profiles
  }
  for (version in names(figure_versions)) {
    for (block_name in EFFECT_BLOCKS) {
      points <- main_effects %>%
        filter(block == block_name, status == "shown",
               profile %in% figure_versions[[version]]) %>%
        transmute(row = effect_label, facet = outcome_label, series = profile,
                  estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                  ci_high = 100 * ci_high_kr)
      if (nrow(points) == 0) {
        next
      }
      points$row <- factor(points$row, levels = EFFECT_LABELS$label)
      points$facet <- factor(points$facet, levels = outcome_levels)
      points$series <- factor(points$series, levels = figure_versions[[version]])
      exhibits <- bind_rows(exhibits, effect_figures(
        points,
        paste0("fig_effects_", model_name, "_", block_slug(block_name),
               "_", version),
        dirs,
        list(block = block, id = paste0("effects_", block_slug(block_name)),
             group = ALL_EXPERTS,
             caption = paste0(block_name, ": effects on the recommendation, ",
                              model_title,
                              if (version == "groups") ", by group" else ""),
             placement = if (version == "all") placement else "appendix",
             note = paste0(set_label, ". ", set_note(main_set, for_figure = TRUE),
                           association_note(block_name))),
        GROUP_COLOURS, GROUP_SHAPES
      ))
    }
  }
  if (!full) {
    return(list(exhibits = exhibits, problems = character(0)))
  }

  # 6. The main model: over the answers, and the households --------------------
  # Over the answers: the attribute effects over the vignettes the experts saw
  answer_set <- set_rows[set_rows$kind == "answers", ]
  for (k in seq_len(nrow(answer_set))) {
    answer_effects <- collected$effects %>%
      filter(run == run_name, model == model_name, set == answer_set$set[k])
    answer_baselines <- collected$baselines %>%
      filter(run == run_name, model == model_name, set == answer_set$set[k])
    answer_label <- SET_LABELS$label[SET_LABELS$set == answer_set$set[k]]
    cells <- effect_cells(
      answer_effects %>% filter(profile %in% core_profiles) %>%
        mutate(column = outcome_label),
      answer_baselines %>% filter(profile %in% core_profiles) %>%
        mutate(column = outcome_label)
    )
    cells$column <- factor(cells$column, levels = outcome_levels)
    association <- association_note(cells$panel[cells$status != "not applicable"])
    notes <- paste0("Sample: ", run_row$sample_label, " (", core_profiles, "). ",
                    answer_label, ". ", set_note(answer_set[k, ]), association, " ",
                    model_note, " ", cell_note)
    file_name <- paste0("tab_effects_", model_name, "_", answer_set$set[k], "_",
                        slug_of(core_profiles), ".tex")
    if (table_estimates(cells, file.path(dirs$tables, file_name), notes)) {
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "effects_answers", ALL_EXPERTS, "table",
        file.path("tables", file_name),
        paste0("Effects over the vignettes the experts answered, ", model_title),
        "appendix"
      ))
    }
    points <- answer_effects %>%
      filter(status == "shown") %>%
      transmute(row = effect_label, facet = outcome_label, series = profile,
                estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                ci_high = 100 * ci_high_kr)
    if (nrow(points) > 0) {
      points$row <- factor(points$row, levels = EFFECT_LABELS$label)
      points$facet <- factor(points$facet, levels = outcome_levels)
      points$series <- factor(points$series, levels = profiles)
      exhibits <- bind_rows(exhibits, effect_figures(
        points, paste0("fig_effects_", model_name, "_", answer_set$set[k]), dirs,
        list(block = block, id = "effects_answers", group = ALL_EXPERTS,
             caption = paste0("Effects over the vignettes the experts answered, ",
                              model_title),
             placement = "appendix",
             note = paste0(answer_label, ". ", set_note(answer_set[k, ], TRUE),
                           association)),
        GROUP_COLOURS, GROUP_SHAPES
      ))
    }
  }

  # The households: the main set and the other households of the observed experts
  household_sets <- set_rows %>%
    filter(kind == "experts") %>%
    mutate(order = match(set, SET_LABELS$set)) %>%
    arrange(order)
  household_levels <- SET_LABELS$short[match(household_sets$set, SET_LABELS$set)]
  # setNames(): the palette's colours, named by the households
  household_colours <- setNames(paper_colors[seq_along(household_levels)],
                                household_levels)
  household_shapes <- setNames(SERIES_SHAPES[seq_along(household_levels)],
                               household_levels)
  if (nrow(household_sets) > 1) {
    # The expert's terms at each household, for all experts: where the decision
    # is open or settled. Only the effects every household has.
    household_effects <- collected$effects %>%
      filter(run == run_name, model == model_name, set %in% household_sets$set,
             profile %in% core_profiles, block != "The household")
    for (block_name in setdiff(EFFECT_BLOCKS, "The household")) {
      points <- household_effects %>%
        filter(block == block_name, status == "shown") %>%
        transmute(row = effect_label, facet = outcome_label,
                  series = SET_LABELS$short[match(set, SET_LABELS$set)],
                  estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                  ci_high = 100 * ci_high_kr)
      if (nrow(points) == 0) {
        next
      }
      points$row <- factor(points$row, levels = EFFECT_LABELS$label)
      points$facet <- factor(points$facet, levels = outcome_levels)
      points$series <- factor(points$series, levels = household_levels)
      exhibits <- bind_rows(exhibits, effect_figures(
        points, paste0("fig_households_", model_name, "_", block_slug(block_name)),
        dirs,
        list(block = block, id = paste0("households_", block_slug(block_name)),
             group = ALL_EXPERTS,
             caption = paste0(block_name, ": effects at each household, ",
                              model_title),
             placement = "appendix",
             note = paste0("All experts. The average change in the probability ",
                           "of each recommendation, in percentage points, over ",
                           "the observed experts, each with their own ",
                           "characteristics, with 95% Krinsky-Robb percentile ",
                           "intervals. ", households_text(household_sets),
                           association_note(block_name))),
        household_colours, household_shapes
      ))
    }
  }
  # The probabilities at each household, by group: a bar only where every
  # outcome of it is shown
  household_baselines <- collected$baselines %>%
    filter(run == run_name, model == model_name, set %in% household_sets$set) %>%
    group_by(set, profile) %>%
    filter(all(status == "shown")) %>%
    ungroup()
  if (nrow(household_baselines) > 0) {
    bars <- household_baselines %>%
      transmute(panel = factor(SET_LABELS$short[match(set, SET_LABELS$set)],
                               levels = household_levels),
                bar = factor(profile, levels = profiles),
                segment = factor(outcome_label, levels = outcome_levels),
                share = probability, n_bar = n_experts)
    # Two rows of panels, so that the percentages fit their segments
    plot <- plot_stacked_shares(bars, OUTCOME_COLOURS[outcome_levels],
                                "Predicted probability of each recommendation",
                                panel_rows = 2)
    file_name <- paste0("fig_baseline_", model_name, ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name), size = "tall")
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "baseline", ALL_EXPERTS, "figure", file.path("figures", file_name),
      paste0("Predicted recommendations at each household, ", model_title),
      placement,
      paste0("The average predicted probability of each recommendation over the ",
             "observed experts of each group, each with their own ",
             "characteristics; the number of experts to the right of each bar. ",
             households_text(household_sets))
    ))
    cells <- household_baselines %>%
      mutate(order = 100 * match(set, household_sets$set) +
               match(profile, profiles)) %>%
      arrange(order) %>%
      transmute(row_key = paste(set, profile, sep = "|"), row_label = profile,
                panel = SET_LABELS$short[match(set, SET_LABELS$set)],
                column = outcome_label, estimate = 100 * probability,
                se = 100 * se, p = NA_real_, status)
    cells$row_key <- factor(cells$row_key, levels = unique(cells$row_key))
    cells$column <- factor(cells$column, levels = outcome_levels)
    notes <- paste0("Sample: ", run_row$sample_label, ". The average predicted ",
                    "probability of each recommendation in percent, over the ",
                    "observed experts of each group, with Krinsky-Robb standard ",
                    "errors in parentheses. ", households_text(household_sets),
                    " ", model_note, " ", cell_note)
    file_name <- paste0("tab_baseline_", model_name, ".tex")
    if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                        stars = FALSE)) {
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "baseline", ALL_EXPERTS, "table", file.path("tables", file_name),
        paste0("Predicted recommendations at each household, ", model_title),
        "appendix"
      ))
    }
  }

  # 7. Robustness: the definitions of an effect --------------------------------
  # The attribute effects under three definitions: a reference expert, the
  # observed experts at the reference household, and over the answers. By group,
  # as the reference expert has no profile of all experts.
  definition_sets <- intersect(c("neutral", MAIN_AMES, answer_set$set),
                               set_rows$set)
  if (length(definition_sets) > 1) {
    definition_levels <- SET_LABELS$short[match(definition_sets, SET_LABELS$set)]
    definition_effects <- collected$effects %>%
      filter(run == run_name, model == model_name, set %in% definition_sets,
             block == "The household", profile %in% EXPERT_GROUPS,
             status == "shown")
    for (k in seq_len(nrow(model_outcomes))) {
      points <- definition_effects %>%
        filter(outcome == model_outcomes$outcome[k]) %>%
        transmute(row = effect_label, facet = profile,
                  series = SET_LABELS$short[match(set, SET_LABELS$set)],
                  estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                  ci_high = 100 * ci_high_kr)
      if (nrow(points) == 0) {
        next
      }
      points$row <- factor(points$row, levels = EFFECT_LABELS$label)
      points$facet <- factor(points$facet, levels = EXPERT_GROUPS)
      points$series <- factor(points$series, levels = definition_levels)
      exhibits <- bind_rows(exhibits, effect_figures(
        points,
        paste0("fig_definitions_", model_name, "_",
               slug_of(model_outcomes$outcome_label[k])),
        dirs,
        list(block = block_robust, id = "definitions", group = ALL_EXPERTS,
             caption = paste0("Three definitions of an effect on ",
                              outcome_phrase(model_outcomes$outcome_label[k]), ", ",
                              model_title),
             placement = "appendix",
             note = paste0("The same changes of the household under three ",
                           "definitions of the average effect: at one reference ",
                           "expert per group; over the observed experts at the ",
                           "reference household; over the vignettes the experts ",
                           "answered. In percentage points, with 95% Krinsky-Robb ",
                           "percentile intervals. ", model_note)),
        setNames(paper_colors[seq_along(definition_levels)], definition_levels),
        setNames(SERIES_SHAPES[seq_along(definition_levels)], definition_levels)
      ))
    }
  }

  # 8. Robustness: the differences between the groups --------------------------
  # From the Krinsky-Robb draws of the main set: both groups' effects are
  # averaged at the same draws, so the spread of their difference over the draws
  # is its standard error. Only where both cells are shown.
  group_draws <- collected$draws[[paste(run_name, model_name, sep = "|")]]
  pairs <- list(c("HVAC installers", "Energy advisers"),
                c("Chimney sweeps", "Energy advisers"),
                c("Chimney sweeps", "HVAC installers"))
  # One key per cell, to find the two cells of a difference
  main_effects$key <- paste(main_effects$profile, main_effects$effect,
                            main_effects$outcome, sep = "|")
  for (pair in pairs) {
    if (is.null(group_draws) || !all(pair %in% names(group_draws))) {
      next
    }
    pair_rows <- list()
    for (effect_name in unique(main_effects$effect)) {
      for (k in seq_len(nrow(model_outcomes))) {
        outcome_name <- model_outcomes$outcome[k]
        first_cell <- main_effects[main_effects$key ==
                                     paste(pair[1], effect_name, outcome_name,
                                           sep = "|"), ]
        second_cell <- main_effects[main_effects$key ==
                                      paste(pair[2], effect_name, outcome_name,
                                            sep = "|"), ]
        if (nrow(first_cell) != 1 || nrow(second_cell) != 1) {
          next
        }
        status <- "shown"
        if ("not applicable" %in% c(first_cell$status, second_cell$status)) {
          status <- "not applicable"
        } else if ("too few" %in% c(first_cell$status, second_cell$status)) {
          status <- "too few"
        }
        estimate <- NA_real_
        se <- NA_real_
        if (status == "shown") {
          draws_first <- group_draws[[pair[1]]]
          draws_second <- group_draws[[pair[2]]]
          change_first <- draws_first[paste0(effect_name, "|to"), outcome_name, ] -
            draws_first[paste0(effect_name, "|from"), outcome_name, ]
          change_second <- draws_second[paste0(effect_name, "|to"), outcome_name, ] -
            draws_second[paste0(effect_name, "|from"), outcome_name, ]
          estimate <- first_cell$ame - second_cell$ame
          se <- sd(change_first - change_second)
        }
        pair_rows[[length(pair_rows) + 1]] <- tibble(
          effect = effect_name, effect_label = first_cell$effect_label,
          block = first_cell$block, column = model_outcomes$outcome_label[k],
          ame = estimate, se = se, p = 2 * pnorm(-abs(estimate / se)),
          status = status
        )
      }
    }
    if (length(pair_rows) == 0) {
      next
    }
    cells <- effect_cells(bind_rows(pair_rows), tibble(
      column = character(0), probability = numeric(0), se = numeric(0),
      status = character(0)
    ))
    cells$column <- factor(cells$column, levels = outcome_levels)
    association <- association_note(cells$panel[cells$status != "not applicable"])
    notes <- paste0("Sample: ", run_row$sample_label, ". The effect for ",
                    pair[1], " minus that for ", pair[2],
                    ", in percentage points. ", set_label,
                    ". Both effects are averaged at the same Krinsky-Robb draws, ",
                    "so the standard error in parentheses accounts for how the ",
                    "two move together.", association, " ", model_note,
                    " --: one of the two rests ",
                    "on fewer than ", MIN_N, " experts. Blank: the change does ",
                    "not apply to one of the groups.")
    file_name <- paste0("tab_group_differences_", model_name, "_",
                        slug_of(pair[1]), "_vs_", slug_of(pair[2]), ".tex")
    if (table_estimates(cells, file.path(dirs$tables, file_name), notes)) {
      exhibits <- bind_rows(exhibits, exhibit_row(
        block_robust, "group_differences", ALL_EXPERTS, "table",
        file.path("tables", file_name),
        paste0("How ", pair[1], " differ from ", pair[2], ", ", model_title),
        "appendix"
      ))
    }
  }
  list(exhibits = exhibits, problems = character(0))
}
