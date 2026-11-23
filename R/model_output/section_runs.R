################################################################################
# section_runs.R — the runs compared: the two arms side by side, and in each
#                  arm the gas and the oil heating as one outcome against apart
# Inputs : collect_results(); the main runs and the runs with the outcomes
#          apart, of the settings
# Outputs: in dirs$tables and dirs$figures: fig_effects_arms_<block>,
#          tab_effects_arms_<outcome>, tab_models_arms, tab_decision_arms,
#          fig_baseline_arms, fig_choice_set_<arm>_<block>
# Checks : effects side by side only at one household and with one definition;
#          a sum of outcomes only where every part is shown
################################################################################
# The arms are not randomised: Q6a routes each expert, and only the arm with the
# option can recommend district heating. The comparisons are descriptive, so no
# test of a difference between the arms is made (Max, 2026-11-16).

#' The comparisons across runs
#'
#' @param collected       collect_results()
#' @param main_runs       the main run of each arm, named by arm
#' @param separate_runs   the run with the gas and the oil heating apart, named
#'                        by arm
#' @param main_model      the model whose effects are compared
#' @param models_compared the comparisons of the settings; the first is shown
#'                        across the arms
#' @param dirs            the folders of the exhibits across runs
#' @return list: exhibits (exhibit_row()s), problems (text, for "Not shown")
section_runs <- function(collected, main_runs, separate_runs, main_model,
                         models_compared, dirs) {
  # 1. What the comparisons share ----------------------------------------------
  block_arms <- "A. The arms"
  block_choice <- "B. Gas and oil heating as one outcome or apart"
  cell_note <- paste0("--: rests on fewer than ", MIN_N, " experts.")
  arm_note <- paste0("The arms are not randomised: the expert's answer on whether ",
                     "district heating is available routes them, and only the arm with ",
                     "the option can recommend it, so the arms compare descriptively.")
  short_profiles <- c("All experts" = "All", GROUP_LABELS_SHORT)
  exhibits <- tibble()
  problems <- character(0)
  arm_runs <- main_runs[main_runs %in% collected$runs$run]
  if (length(arm_runs) < 2) {
    problems <- c(problems, paste0(
      "The arms are compared only with the main run of both: found ",
      paste(arm_runs, collapse = ", ")
    ))
  }

  # 2. The effects of the main model in both arms ------------------------------
  # At the same household, and only the changes both arms define alike
  arm_levels <- unname(ARM_LABELS[names(arm_runs)])
  # setNames(): the values, with these names: the arm of each run
  arm_of_run <- setNames(arm_levels, arm_runs)
  main_sets <- collected$sets %>%
    filter(run %in% arm_runs, model == main_model, set == MAIN_AMES)
  can_compare <- length(arm_runs) == 2 && nrow(main_sets) == 2 &&
    n_distinct(main_sets$at_hash) == 1
  if (length(arm_runs) == 2 && !can_compare) {
    problems <- c(problems, paste0(
      "The effects of ", main_model, " are not compared across the arms: a run ",
      "lacks the main set, or its household differs"
    ))
  }
  if (can_compare) {
    sample_note <- paste0("Samples: ",
                          paste0(arm_levels, ": ",
                                 collected$runs$sample_label[
                                   match(arm_runs, collected$runs$run)],
                                 collapse = "; "), ".")
    definitions <- collected$definitions %>%
      filter(run %in% arm_runs, model == main_model, set == MAIN_AMES) %>%
      group_by(effect) %>%
      mutate(is_alike = n_distinct(definition_hash) == 1) %>%
      ungroup()
    if (any(!definitions$is_alike)) {
      problems <- c(problems, paste0(
        "Defined otherwise in the two arms, and not compared: ",
        paste(unique(definitions$effect[!definitions$is_alike]), collapse = ", ")
      ))
    }
    arm_effects <- collected$effects %>%
      filter(run %in% arm_runs, model == main_model, set == MAIN_AMES) %>%
      left_join(definitions %>% select(run, effect, is_alike),
                by = c("run", "effect"), relationship = "many-to-one") %>%
      mutate(status = if_else(is_alike %in% FALSE, "not applicable", status),
             arm = unname(arm_of_run[run]))
    arm_baselines <- collected$baselines %>%
      filter(run %in% arm_runs, model == main_model, set == MAIN_AMES) %>%
      mutate(arm = unname(arm_of_run[run]))
    arm_outcomes <- collected$outcomes %>%
      filter(run %in% arm_runs, model == main_model) %>%
      distinct(outcome, position, outcome_label) %>%
      arrange(position)
    # The main model in words, as a reader of the paper does not know the codes
    # (Claude's proposal, agreed by Max, 2026-11-23)
    model_note <- paste0("The main model in both arms, standard errors clustered ",
                         "by expert.")

    # Figures: all experts, the arms side by side
    for (block_name in EFFECT_BLOCKS) {
      points <- arm_effects %>%
        filter(block == block_name, status == "shown", profile == ALL_EXPERTS) %>%
        transmute(row = effect_label, facet = outcome_label, series = arm,
                  estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                  ci_high = 100 * ci_high_kr)
      if (nrow(points) == 0) {
        next
      }
      points$row <- factor(points$row, levels = EFFECT_LABELS$label)
      points$facet <- factor(points$facet, levels = arm_outcomes$outcome_label)
      points$series <- factor(points$series, levels = arm_levels)
      exhibits <- bind_rows(exhibits, effect_figures(
        points, paste0("fig_effects_arms_", block_slug(block_name)), dirs,
        list(block = block_arms, id = paste0("effects_arms_", block_slug(block_name)),
             group = ALL_EXPERTS,
             caption = paste0(block_name, ": effects on the recommendation in ",
                              "both arms, the main model"),
             placement = "main",
             note = paste0("All experts. ", set_note(main_sets[1, ], TRUE),
                           association_note(block_name), " ", arm_note)),
        ARM_COLOURS[arm_levels],
        setNames(SERIES_SHAPES[seq_along(arm_levels)], arm_levels)
      ))
    }

    # Tables: one per outcome, the profiles of each arm that has it
    for (k in seq_len(nrow(arm_outcomes))) {
      outcome_effects <- arm_effects %>%
        filter(outcome == arm_outcomes$outcome[k]) %>%
        mutate(column = paste(arm, profile, sep = "|"))
      outcome_baselines <- arm_baselines %>%
        filter(outcome == arm_outcomes$outcome[k]) %>%
        mutate(column = paste(arm, profile, sep = "|"))
      column_keys <- character(0)
      header <- c(" " = 1)
      for (arm_label in arm_levels) {
        arm_profiles <- unique(outcome_baselines$profile[outcome_baselines$arm ==
                                                           arm_label])
        if (length(arm_profiles) == 0) {
          next
        }
        column_keys <- c(column_keys, paste(arm_label, arm_profiles, sep = "|"))
        header <- c(header, setNames(length(arm_profiles), arm_label))
      }
      cells <- effect_cells(outcome_effects, outcome_baselines)
      cells$column <- factor(cells$column, levels = column_keys)
      # The profile of each column, in short
      # sub(): the key without the arm and its "|"
      column_profiles <- sub("^[^|]*\\|", "", column_keys)
      column_names <- unname(short_profiles[column_profiles])
      column_names[is.na(column_names)] <- column_profiles[is.na(column_names)]
      association <- association_note(cells$panel[cells$status != "not applicable"])
      notes <- paste0(sample_note, " The probability of ",
                      outcome_phrase(arm_outcomes$outcome_label[k]),
                      " in each arm. ", set_note(main_sets[1, ]), association, " ",
                      model_note, " ", arm_note, " ", cell_note,
                      " Blank: the change does not apply to them.")
      file_name <- paste0("tab_effects_arms_",
                          slug_of(arm_outcomes$outcome_label[k]), ".tex")
      if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                          column_names = column_names, header = header)) {
        exhibits <- bind_rows(exhibits, exhibit_row(
          block_arms, "effects_arms", ALL_EXPERTS, "table",
          file.path("tables", file_name),
          paste0("Effects on ",
                 outcome_phrase(arm_outcomes$outcome_label[k]),
                 " in both arms, the main model"),
          if (k == 1) "main" else "appendix"
        ))
      }
    }

    # The probabilities at each household in both arms: a bar only where every
    # outcome is shown
    household_sets <- collected$sets %>%
      filter(run %in% arm_runs, model == main_model, kind == "experts") %>%
      mutate(order = match(set, SET_LABELS$set)) %>%
      arrange(order)
    household_levels <- unique(SET_LABELS$short[match(household_sets$set,
                                                      SET_LABELS$set)])
    bars <- collected$baselines %>%
      filter(run %in% arm_runs, model == main_model,
             set %in% household_sets$set, profile == ALL_EXPERTS) %>%
      group_by(run, set) %>%
      filter(all(status == "shown")) %>%
      ungroup()
    if (nrow(bars) > 0) {
      bars <- bars %>%
        transmute(panel = factor(unname(arm_of_run[run]), levels = arm_levels),
                  bar = factor(SET_LABELS$short[match(set, SET_LABELS$set)],
                               levels = household_levels),
                  segment = factor(outcome_label,
                                   levels = arm_outcomes$outcome_label),
                  share = probability, n_bar = n_experts)
      plot <- plot_stacked_shares(bars,
                                  OUTCOME_COLOURS[arm_outcomes$outcome_label],
                                  "Predicted probability of each recommendation")
      file_name <- "fig_baseline_arms.pdf"
      save_figure(plot, file.path(dirs$figures, file_name), size = "full")
      exhibits <- bind_rows(exhibits, exhibit_row(
        block_arms, "baseline_arms", ALL_EXPERTS, "figure",
        file.path("figures", file_name),
        paste0("Predicted recommendations at each household in both arms, ",
               "the main model"),
        "appendix",
        paste0("All experts. The average predicted probability of each ",
               "recommendation over the observed experts of each arm, each with ",
               "their own characteristics; the number of experts to the right of ",
               "each bar. ",
               households_text(household_sets %>% filter(run == arm_runs[[1]])),
               " ", arm_note)
      ))
    }
  }

  # 3. The models of both arms -------------------------------------------------
  # The first comparison of the settings, a block of columns per arm
  if (length(arm_runs) == 2 && length(models_compared) > 0) {
    wanted <- models_compared[[1]]
    all_rows <- NULL
    header <- c(" " = 1)
    column_names <- character(0)
    for (k in seq_along(arm_runs)) {
      run_models <- collected$models$model[collected$models$run == arm_runs[[k]]]
      arm_models <- wanted[wanted %in% run_models]
      if (length(arm_models) == 0) {
        next
      }
      rows <- model_table_rows(collected, arm_runs[[k]], arm_models)
      # The columns of this arm under names of their own
      names(rows)[-(1:2)] <- paste0(names(arm_runs)[k], "|", arm_models)
      if (is.null(all_rows)) {
        all_rows <- rows
      } else {
        # full_join(): every row of both tables, blank where one arm lacks it
        all_rows <- full_join(all_rows, rows, by = c("panel", "label"),
                              relationship = "one-to-one")
      }
      header <- c(header, setNames(length(arm_models), arm_levels[k]))
      column_names <- c(column_names,
                        paste0("(", seq_along(arm_models), ") ",
                               MODEL_LABELS[arm_models]))
    }
    if (!is.null(all_rows)) {
      all_rows <- all_rows %>%
        mutate(panel_order = match(panel, unique(panel))) %>%
        arrange(panel_order) %>%
        select(-panel_order)
      # A row one arm does not have, e.g. a test it does not run, stays blank
      for (column in names(all_rows)[-(1:2)]) {
        all_rows[[column]][is.na(all_rows[[column]])] <- ""
      }
      runs_of_panels <- rle(all_rows$panel)
      tab <- all_rows %>%
        select(-panel) %>%
        mutate(label = latex_text(label))
      notes <- paste0(
        "Samples: ", paste0(arm_levels, ": ",
                            collected$runs$sample_label[match(arm_runs,
                                                              collected$runs$run)],
                            collapse = "; "),
        ". Each model adds to the one before it what its header says. BIC counts ",
        "experts. LR: the likelihood-ratio statistic against the model named; ",
        "robust p: after Holm's correction over the tests of the arm. The proposal ",
        "is that of the fixed rule; the decision is the researchers'. ", arm_note
      )
      file_name <- "tab_models_arms.tex"
      table_custom(tab, file.path(dirs$tables, file_name), latex_text(notes),
                   col_names = c("", multiline_header(column_names, 14)),
                   align = c("l", rep("c", ncol(tab) - 1)),
                   header = setNames(header, latex_text(names(header))),
                   panels = setNames(runs_of_panels$lengths,
                                     latex_text(runs_of_panels$values)))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block_arms, "models_arms", ALL_EXPERTS, "table",
        file.path("tables", file_name),
        paste0("The models compared in both arms: ", names(models_compared)[1]),
        "main"
      ))
    }

    # Every question of the rule, with its proposal and the decision in each arm
    decisions <- tibble()
    for (k in seq_along(arm_runs)) {
      comparison <- collected$comparisons[[arm_runs[[k]]]]
      if (is.null(comparison) || nrow(comparison$decision) == 0) {
        next
      }
      # coalesce(): the first value that is not missing, "open" for no decision
      decisions <- bind_rows(decisions, comparison$decision %>%
        transmute(question, smaller, larger, arm = names(arm_runs)[k],
                  proposal, final_decision = coalesce(final_decision, "open")))
    }
    if (nrow(decisions) > 0) {
      wide <- decisions %>%
        pivot_wider(names_from = arm, values_from = c(proposal, final_decision))
      tab <- tibble(question = wrapped_cell(wide$question, 28),
                    models = paste0(latex_text(model_text(wide$smaller)), " vs ",
                                    latex_text(model_text(wide$larger))))
      column_names <- c("Question", "Models")
      header <- c(" " = 2)
      for (k in seq_along(arm_runs)) {
        proposal_column <- paste0("proposal_", names(arm_runs)[k])
        decision_column <- paste0("final_decision_", names(arm_runs)[k])
        if (!proposal_column %in% names(wide)) {
          next
        }
        tab[[proposal_column]] <- wrapped_cell(coalesce(wide[[proposal_column]],
                                                            "not tested"), 20)
        tab[[decision_column]] <- wrapped_cell(coalesce(wide[[decision_column]],
                                                            ""), 16)
        column_names <- c(column_names, "The rule proposes", "Decision")
        header <- c(header, setNames(2, arm_levels[k]))
      }
      notes <- paste0("Every question of the fixed rule for comparing models, in each ",
                      "arm; the decision is the researchers'. ", arm_note, " ",
                      MODEL_CODES_NOTE)
      file_name <- "tab_decision_arms.tex"
      table_custom(tab, file.path(dirs$tables, file_name), latex_text(notes),
                   col_names = column_names,
                   align = c("l", "l", rep("l", ncol(tab) - 2)),
                   header = setNames(header, latex_text(names(header))))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block_arms, "decision_arms", ALL_EXPERTS, "table",
        file.path("tables", file_name),
        "Every question of the rule in both arms", "appendix"
      ))
    }
  }

  # 4. The gas and the oil heating as one outcome or apart ---------------------
  # In each arm, the main model's effects for all experts on the outcomes both
  # runs have, and the grouped outcome against the sum of its parts in the run
  # with the outcomes apart. The sum's interval from that run's own draws, as
  # draws of two models cannot be paired (each model's start from one seed).
  for (arm in names(separate_runs)) {
    grouped_run <- unname(main_runs[arm])
    separate_run <- unname(separate_runs[arm])
    if (is.na(grouped_run) || !all(c(grouped_run, separate_run) %in%
                                   collected$runs$run)) {
      problems <- c(problems, paste0(
        "Arm ", arm, ": the outcomes as one or apart are compared only with both ",
        "runs, ", grouped_run, " and ", separate_run
      ))
      next
    }
    both_sets <- collected$sets %>%
      filter(run %in% c(grouped_run, separate_run), model == main_model,
             set == MAIN_AMES)
    if (nrow(both_sets) != 2 || n_distinct(both_sets$at_hash) != 1) {
      problems <- c(problems, paste0(
        "Arm ", arm, ": the main set of ", main_model, " is missing in a run, or ",
        "at another household"
      ))
      next
    }
    grouped_outcomes <- collected$outcomes %>%
      filter(run == grouped_run, model == main_model) %>%
      arrange(position)
    separate_outcomes <- collected$outcomes %>%
      filter(run == separate_run, model == main_model)
    merged <- setdiff(grouped_outcomes$outcome, separate_outcomes$outcome)
    parts <- setdiff(separate_outcomes$outcome, grouped_outcomes$outcome)
    alike <- collected$definitions %>%
      filter(run %in% c(grouped_run, separate_run), model == main_model,
             set == MAIN_AMES) %>%
      group_by(effect) %>%
      filter(n() == 2, n_distinct(definition_hash) == 1) %>%
      ungroup() %>%
      distinct(effect) %>%
      pull(effect)
    grouped_effects <- collected$effects %>%
      filter(run == grouped_run, model == main_model, set == MAIN_AMES,
             profile == ALL_EXPERTS, effect %in% alike)
    separate_effects <- collected$effects %>%
      filter(run == separate_run, model == main_model, set == MAIN_AMES,
             profile == ALL_EXPERTS, effect %in% alike)
    # The parts summed, at every draw of the run with the outcomes apart
    summed <- tibble()
    separate_draws <- collected$draws[[paste(separate_run, main_model, sep = "|")]]
    if (length(merged) == 1 && length(parts) >= 2 && !is.null(separate_draws) &&
        ALL_EXPERTS %in% names(separate_draws)) {
      profile_draws <- separate_draws[[ALL_EXPERTS]]
      for (effect_name in alike) {
        part_cells <- separate_effects %>%
          filter(effect == effect_name, outcome %in% parts)
        if (nrow(part_cells) != length(parts) ||
            any(part_cells$status != "shown")) {
          next
        }
        change <- 0
        for (part in parts) {
          change <- change + profile_draws[paste0(effect_name, "|to"), part, ] -
            profile_draws[paste0(effect_name, "|from"), part, ]
        }
        summed <- bind_rows(summed, tibble(
          effect = effect_name, effect_label = part_cells$effect_label[1],
          block = part_cells$block[1], outcome = merged,
          outcome_label = OUTCOME_LABELS[[merged]],
          ame = sum(part_cells$ame),
          ci_low_kr = quantile(change, 0.025, names = FALSE),
          ci_high_kr = quantile(change, 0.975, names = FALSE),
          status = "shown"
        ))
      }
    } else if (length(merged) > 0) {
      problems <- c(problems, paste0(
        "Arm ", arm, ": the grouped outcome is not set against the sum of its ",
        "parts (the draws of the main set of ", separate_run, " are missing, or ",
        "the outcomes do not group one into several)"
      ))
    }
    variant_levels <- unname(VARIANT_LABELS[c("grouped", "separate")])
    choice_effects <- bind_rows(
      grouped_effects %>%
        mutate(series = variant_levels[1]),
      separate_effects %>%
        filter(!outcome %in% parts) %>%
        mutate(series = variant_levels[2]),
      summed %>%
        mutate(series = variant_levels[2])
    ) %>%
      filter(status == "shown")
    for (block_name in EFFECT_BLOCKS) {
      points <- choice_effects %>%
        filter(block == block_name) %>%
        transmute(row = effect_label, facet = outcome_label, series,
                  estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                  ci_high = 100 * ci_high_kr)
      if (nrow(points) == 0) {
        next
      }
      points$row <- factor(points$row, levels = EFFECT_LABELS$label)
      points$facet <- factor(points$facet, levels = grouped_outcomes$outcome_label)
      points$series <- factor(points$series, levels = variant_levels)
      exhibits <- bind_rows(exhibits, effect_figures(
        points, paste0("fig_choice_set_", arm, "_", block_slug(block_name)), dirs,
        list(block = block_choice,
             id = paste0("choice_set_", arm, "_", block_slug(block_name)),
             group = ALL_EXPERTS,
             caption = paste0(block_name, ": gas and oil heating as one outcome ",
                              "or apart, ", str_to_lower(ARM_LABELS[[arm]]),
                              ", the main model"),
             placement = "appendix",
             note = paste0("All experts. ", set_note(both_sets[1, ], TRUE),
                           association_note(block_name),
                           " With the outcomes apart, the effect on the gas or ",
                           "oil heating is the sum of those on each, its ",
                           "interval from the same draws. Only the changes both ",
                           "runs define alike.")),
        setNames(paper_colors[1:2], variant_levels),
        setNames(SERIES_SHAPES[1:2], variant_levels)
      ))
    }
  }
  list(exhibits = exhibits, problems = problems)
}
