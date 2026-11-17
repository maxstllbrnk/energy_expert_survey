################################################################################
# section_comparison.R — the models of one run compared: their fit and the
#                        rule's verdicts, their coefficients and effects side by
#                        side, how each estimation ended, the fit to the shares
# Inputs : collect_results(), for one run; the comparisons of the settings
# Outputs: in dirs$tables and dirs$figures: tab_models_<comparison>,
#          tab_common_terms_<comparison>, tab_coefficients_<comparison>_<outcome>,
#          fig_effects_<comparison>_<block>, tab_effects_<comparison>_<outcome>,
#          tab_decision, tab_diagnostics, fig_fit_<model>
# Checks : the models of a comparison share their sample; an effect stands next
#          to another model's only if both define it alike, at one household
################################################################################

#' The rows of the table of the models: their fit and the test against the model
#' before, as text, one column per model. Also used across the arms
#' (section_runs()).
#'
#' @param collected  collect_results()
#' @param run_name   the run
#' @param model_names the models, in the order of the columns
#' @return tibble: panel, label, then one column of LaTeX text per model
model_table_rows <- function(collected, run_name, model_names) {
  model_rows <- collected$models %>%
    filter(run == run_name)
  # match(): the position of each model's row, so that the rows follow model_names
  model_rows <- model_rows[match(model_names, model_rows$model), ]
  comparison <- collected$comparisons[[run_name]]
  rows <- tibble(panel = "Fit", label = c("Parameters", "Log-likelihood",
                                          "McFadden R²", "AIC", "BIC"))
  for (k in seq_along(model_names)) {
    rows[[model_names[k]]] <- c(
      fmt_int(model_rows$n_parameters[k]), fmt_num(model_rows$log_likelihood[k], 1),
      fmt_num(model_rows$mcfadden_r2[k], 3), fmt_num(model_rows$aic[k], 1),
      fmt_num(model_rows$bic[k], 1)
    )
  }
  if (is.null(comparison)) {
    return(rows)
  }

  # The held-out log-likelihood of the cross-validation, where it ran
  cv <- comparison$cross_validation
  if ("held_out_per_expert" %in% names(cv)) {
    held_out <- cv %>%
      filter(row_type == "model", item %in% model_names)
    if (nrow(held_out) > 0) {
      cv_row <- tibble(panel = "Fit", label = "Held-out log-likelihood per expert")
      for (k in seq_along(model_names)) {
        value <- held_out$held_out_per_expert[held_out$item == model_names[k]]
        cv_row[[model_names[k]]] <- if (length(value) == 1) fmt_num(value, 3) else ""
      }
      rows <- bind_rows(rows, cv_row)
    }
  }

  # Each model's test against a model of the table it nests: the decision row in
  # which it is the larger model
  test_rows <- tibble(panel = "Against the model before",
                      label = c("Tested against", "LR (df)", "Robust p, Holm",
                                "The rule proposes", "Decision"))
  has_test <- FALSE
  for (k in seq_along(model_names)) {
    test <- comparison$decision %>%
      filter(larger == model_names[k], smaller %in% model_names)
    values <- rep("", nrow(test_rows))
    if (nrow(test) >= 1) {
      test <- test[1, ]
      has_test <- TRUE
      p_value <- if_else(test$p_robust_holm < 0.001, "$<$0.001",
                         fmt_num(test$p_robust_holm, 3), missing = "")
      decision <- test$final_decision
      if (is.na(decision)) {
        decision <- "open"
      }
      values <- c(
        paste0("(", match(test$smaller, model_names), ")"),
        if_else(is.na(test$lr), "",
                paste0(fmt_num(test$lr, 1), " (", fmt_int(test$df), ")")),
        p_value,
        multiline_header(test$proposal, 16),
        multiline_header(decision, 16)
      )
    }
    test_rows[[model_names[k]]] <- values
  }
  if (has_test) {
    rows <- bind_rows(rows, test_rows)
  }
  rows
}

#' The comparisons of the models of one run
#'
#' @param collected       collect_results()
#' @param run_name        the run
#' @param models_compared named list of model names, one comparison each
#' @param main_model      the model whose fit to the shares is drawn
#' @param dirs            the folders of the run's exhibits
#' @return list: exhibits (exhibit_row()s), problems (text, for "Not shown")
section_comparison <- function(collected, run_name, models_compared, main_model,
                               dirs) {
  # 1. What the comparisons share ----------------------------------------------
  block <- "C. The models compared"
  run_row <- collected$runs[collected$runs$run == run_name, ]
  models_of_run <- collected$models %>%
    filter(run == run_name) %>%
    arrange(n_parameters)
  comparison <- collected$comparisons[[run_name]]
  cell_note <- paste0("--: rests on fewer than ", MIN_N, " experts.")
  term_order <- TERM_LABELS %>%
    mutate(order = row_number()) %>%
    rename(row_label = label)
  group_order <- c(EXPERT_GROUPS, "Other experts", ALL_EXPERTS)
  exhibits <- tibble()
  problems <- character(0)

  for (comparison_name in names(models_compared)) {
    wanted <- models_compared[[comparison_name]]
    model_names <- wanted[wanted %in% models_of_run$model]
    # setdiff(): the models wanted that this run does not have
    if (length(setdiff(wanted, model_names)) > 0) {
      problems <- c(problems, paste0(
        "The comparison ", comparison_name, " leaves out models that this run ",
        "does not have: ", paste(setdiff(wanted, model_names), collapse = ", ")
      ))
    }
    if (length(model_names) < 2) {
      next
    }
    hashes <- models_of_run$sample_hash[match(model_names, models_of_run$model)]
    if (n_distinct(hashes) > 1) {
      stop("The models of the comparison ", comparison_name, " in ", run_name,
           " do not share their sample, so their fit does not compare: ",
           paste(model_names, collapse = ", "))
    }
    numbers <- paste0("(", seq_along(model_names), ")")
    headers <- paste0(numbers, " ", MODEL_LABELS[model_names])
    model_key <- paste0(numbers, " ", MODEL_LABELS[model_names],
                        collapse = "; ")
    model_rows <- models_of_run[match(model_names, models_of_run$model), ]
    sample_note <- paste0("Sample: ", run_row$sample_label, ", ",
                          fmt_int(model_rows$n_experts[1]), " experts, ",
                          fmt_int(model_rows$n_choices[1]), " recommendations.")

    # 2. The fit of the models and the tests between them ----------------------
    rows <- model_table_rows(collected, run_name, model_names)
    # rle(): the lengths of runs of equal values, the rows of each panel;
    # setNames() below: these lengths, named by their panel
    runs <- rle(rows$panel)
    tab <- rows %>%
      select(-panel) %>%
      mutate(label = latex_text(label))
    notes <- paste0(
      sample_note, " Each model adds to the one before it what its header says. ",
      "BIC counts experts. Held-out log-likelihood: the mean per expert in a ",
      "5-fold cross-validation by expert. LR: the likelihood-ratio statistic ",
      "against the model named; robust p: its p-value from the weighted ",
      "chi-squared distribution, after Holm's correction over the tests of this ",
      "run. The proposal is that of the fixed rule for comparing models; the ",
      "decision is the researchers'."
    )
    file_name <- paste0("tab_models_", comparison_name, ".tex")
    table_custom(tab, file.path(dirs$tables, file_name), latex_text(notes),
                 col_names = c("", multiline_header(headers, 14)),
                 align = c("l", rep("c", length(model_names))),
                 panels = setNames(runs$lengths, latex_text(runs$values)))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "models", ALL_EXPERTS, "table", file.path("tables", file_name),
      paste0("The models compared: ", comparison_name), "main"
    ))

    # 3. The terms with one coefficient for all technologies, and the spread of
    # the constants ------------------------------------------------------------
    common <- collected$coefficients %>%
      filter(run == run_name, model %in% model_names, is.na(technology)) %>%
      rename(term = column) %>%
      left_join(term_order, by = "term", relationship = "many-to-one") %>%
      transmute(row_key = term, row_label,
                panel = "One coefficient for all technologies", order,
                column = model, estimate, se, p, status)
    spread <- collected$constants %>%
      filter(run == run_name, model %in% model_names, type == "sd") %>%
      mutate(order = 1000 + 10 * match(technology_1, names(OUTCOME_LABELS)) +
               match(constants_of, group_order)) %>%
      transmute(row_key = paste("sd", constants_of, technology_1, sep = "|"),
                row_label = paste0(OUTCOME_LABELS[technology_1],
                                   if_else(constants_of == ALL_EXPERTS, "",
                                           paste0(", ", constants_of))),
                panel = "Standard deviation of the random constant", order,
                column = model, estimate, se, p = NA_real_, status)
    cells <- bind_rows(common, spread) %>%
      arrange(match(panel, COEFFICIENT_PANELS), order)
    cells$row_key <- factor(cells$row_key, levels = unique(cells$row_key))
    cells$column <- factor(cells$column, levels = model_names)
    association <- association_note(cells$panel[cells$status != "not applicable"])
    notes <- paste0(
      sample_note, " Coefficients of the terms with one coefficient for all ",
      "technologies, with standard errors clustered by expert in parentheses, and ",
      "the standard deviations of the random constants, with Krinsky-Robb ",
      "standard errors.", association, " Scales can differ between models. ",
      cell_note
    )
    file_name <- paste0("tab_common_terms_", comparison_name, ".tex")
    if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                        digits = 2, column_names = headers)) {
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "common_terms", ALL_EXPERTS, "table",
        file.path("tables", file_name),
        paste0("Prices, expectations and the spread of the constants: ",
               comparison_name), "appendix"
      ))
    }

    # 4. The coefficients of each technology across models ---------------------
    first_model <- collected$outcomes %>%
      filter(run == run_name, model == model_names[1]) %>%
      arrange(position)
    reference <- model_rows$reference[1]
    for (technology_name in setdiff(first_model$outcome, reference)) {
      technology_label <- OUTCOME_LABELS[[technology_name]]
      by_technology <- collected$coefficients %>%
        filter(run == run_name, model %in% model_names,
               technology == technology_name,
               !column %in% c("(Intercept)", GROUP_COLUMNS)) %>%
        rename(term = column) %>%
        left_join(term_order, by = "term", relationship = "many-to-one") %>%
        transmute(row_key = term, row_label, panel, order, column = model,
                  estimate, se, p, status)
      own_constants <- collected$own_constants %>%
        filter(run == run_name, model %in% model_names,
               technology == technology_name) %>%
        transmute(row_key = paste0("constant|", group), row_label = group,
                  panel = "Constants", order = match(group, group_order) - 100,
                  column = model, estimate, se, p, status)
      cells <- bind_rows(own_constants, by_technology) %>%
        arrange(match(panel, COEFFICIENT_PANELS), order)
      cells$row_key <- factor(cells$row_key, levels = unique(cells$row_key))
      cells$column <- factor(cells$column, levels = model_names)
      association <- association_note(cells$panel[cells$status != "not applicable"])
      notes <- paste0(
        sample_note, " Coefficients of the outcome ", technology_label,
        ": its utility relative to the ",
        str_to_lower(OUTCOME_LABELS[[reference]]), ", with standard errors ",
        "clustered by expert in parentheses. The constants are each group's own.",
        association, " ",
        "Scales can differ between models, and the constants describe another ",
        "expert in each model that adds a term of the expert that is not centred. ",
        cell_note
      )
      file_name <- paste0("tab_coefficients_", comparison_name, "_",
                          slug_of(technology_label), ".tex")
      if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                          digits = 2, column_names = headers)) {
        exhibits <- bind_rows(exhibits, exhibit_row(
          block, "coefficients_models", ALL_EXPERTS, "table",
          file.path("tables", file_name),
          paste0("Coefficients of the outcome ", technology_label, ": ",
                 comparison_name), "appendix"
        ))
      }
    }

    # 5. The effects across models ---------------------------------------------
    # Of the main set, for all experts. Side by side only at the same household
    # and with the same definition of a change as the first model that has it.
    set_rows <- collected$sets %>%
      filter(run == run_name, model %in% model_names, set == MAIN_AMES)
    set_rows <- set_rows[match(model_names, set_rows$model), ]
    effect_models <- model_names[!is.na(set_rows$at_hash) &
                                   set_rows$at_hash == set_rows$at_hash[1]]
    if (length(setdiff(model_names, effect_models)) > 0) {
      problems <- c(problems, paste0(
        "The effects of ", paste(setdiff(model_names, effect_models), collapse = ", "),
        " are left out of the comparison ", comparison_name, ": no main set, or ",
        "at another household"
      ))
    }
    definitions <- collected$definitions %>%
      filter(run == run_name, model %in% effect_models, set == MAIN_AMES) %>%
      mutate(position = match(model, effect_models)) %>%
      arrange(position) %>%
      group_by(effect) %>%
      mutate(is_alike = definition_hash == first(definition_hash)) %>%
      ungroup()
    unlike <- definitions %>%
      filter(!is_alike)
    if (nrow(unlike) > 0) {
      problems <- c(problems, paste0(
        "In the comparison ", comparison_name, ", defined otherwise than in the ",
        "first model that has them, and left out: ",
        paste(unique(paste(unlike$model, unlike$effect)), collapse = ", ")
      ))
    }
    across_effects <- collected$effects %>%
      filter(run == run_name, model %in% effect_models, set == MAIN_AMES,
             profile == ALL_EXPERTS) %>%
      left_join(definitions %>% select(model, effect, is_alike),
                by = c("model", "effect"), relationship = "many-to-one") %>%
      mutate(status = if_else(is_alike %in% FALSE, "not applicable", status))
    across_baselines <- collected$baselines %>%
      filter(run == run_name, model %in% effect_models, set == MAIN_AMES,
             profile == ALL_EXPERTS)
    if (nrow(across_effects) > 0) {
      main_set <- set_rows[set_rows$model == effect_models[1], ]
      outcome_levels <- first_model$outcome_label
      series_levels <- numbers[match(effect_models, model_names)]
      # Up to five models side by side in a row of a figure; more only as tables
      if (length(effect_models) > 5) {
        problems <- c(problems, paste0(
          "The effects of the comparison ", comparison_name, " are tables only: ",
          "a figure shows at most five models side by side, and it has ",
          length(effect_models)
        ))
      } else {
        for (block_name in EFFECT_BLOCKS) {
          points <- across_effects %>%
            filter(block == block_name, status == "shown") %>%
            transmute(row = effect_label, facet = outcome_label,
                      series = numbers[match(model, model_names)],
                      estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                      ci_high = 100 * ci_high_kr)
          if (nrow(points) == 0) {
            next
          }
          points$row <- factor(points$row, levels = EFFECT_LABELS$label)
          points$facet <- factor(points$facet, levels = outcome_levels)
          points$series <- factor(points$series, levels = series_levels)
          exhibits <- bind_rows(exhibits, effect_figures(
            points, paste0("fig_effects_", comparison_name, "_",
                           block_slug(block_name)),
            dirs,
            list(block = block, id = paste0("effects_models_", block_slug(block_name)),
                 group = ALL_EXPERTS,
                 caption = paste0(block_name, ": effects in each model, ",
                                  comparison_name),
                 placement = "appendix",
                 note = paste0("All experts. ", model_key, ". ",
                               set_note(main_set, for_figure = TRUE),
                               association_note(block_name))),
            setNames(paper_colors[seq_along(series_levels)], series_levels),
            setNames(SERIES_SHAPES[seq_along(series_levels)], series_levels)
          ))
        }
      }
      for (outcome_name in first_model$outcome) {
        outcome_label <- OUTCOME_LABELS[[outcome_name]]
        cells <- effect_cells(
          across_effects %>%
            filter(outcome == outcome_name) %>%
            mutate(column = model),
          across_baselines %>%
            filter(outcome == outcome_name) %>%
            mutate(column = model)
        )
        cells$column <- factor(cells$column, levels = effect_models)
        association <- association_note(
          cells$panel[cells$status != "not applicable"]
        )
        notes <- paste0(
          sample_note, " All experts, the probability of ",
          outcome_phrase(outcome_label), " in each model. ", set_note(main_set),
          association, " ", cell_note,
          " Blank: the model does not have the change, or defines ",
          "it otherwise."
        )
        file_name <- paste0("tab_effects_", comparison_name, "_",
                            slug_of(outcome_label), ".tex")
        if (table_estimates(cells, file.path(dirs$tables, file_name), notes,
                            column_names = headers[match(effect_models,
                                                         model_names)])) {
          exhibits <- bind_rows(exhibits, exhibit_row(
            block, "effects_models", ALL_EXPERTS, "table",
            file.path("tables", file_name),
            paste0("Effects on ", outcome_phrase(outcome_label),
                   " in each model: ", comparison_name),
            if (outcome_name == reference) "main" else "appendix"
          ))
        }
      }
    }
  }

  # 6. Every question of the rule ----------------------------------------------
  # Without the rule's largest change of an effect: compare_models() takes that
  # maximum over every cell, also those below the minimum cell, which an exhibit
  # may not show (project profile). Its numbers stay in model_comparison.xlsx.
  if (!is.null(comparison) && nrow(comparison$decision) > 0) {
    decision <- comparison$decision
    out_of_sample <- if_else(
      is.na(decision$cv_gain_per_expert),
      paste0("$\\Delta$BIC ", fmt_num(decision$delta_bic, 1)),
      paste0(fmt_num(decision$cv_gain_per_expert / decision$cv_paired_se, 1), " SE")
    )
    # coalesce(): the first value that is not missing, "open" for no decision
    tab <- tibble(
      question = wrapped_cell(decision$question, 28),
      models = paste0(latex_text(model_text(decision$smaller)), " vs ",
                      latex_text(model_text(decision$larger))),
      p = if_else(decision$p_robust_holm < 0.001, "$<$0.001",
                  fmt_num(decision$p_robust_holm, 3), missing = ""),
      fresh = if_else(decision$lr_fresh_mean > decision$critical_value, "yes",
                      "no", missing = ""),
      out_of_sample = out_of_sample,
      proposal = wrapped_cell(decision$proposal, 22),
      final = wrapped_cell(coalesce(decision$final_decision, "open"), 18)
    )
    notes <- paste0(
      "Sample: ", run_row$sample_label, ". Every question of the fixed rule for ",
      "comparing models in this run. Robust p: the likelihood-ratio test with the ",
      "weighted chi-squared distribution, after Holm's correction. Beyond the ",
      "draws: whether the mean likelihood-ratio statistic on fresh Halton draws ",
      "exceeds the robust test's critical value. Out of sample: the gain in the ",
      "held-out log-likelihood per expert in paired standard errors, or the ",
      "change in BIC where the cross-validation did not run. The decision is the ",
      "researchers'."
    )
    file_name <- "tab_decision.tex"
    table_custom(tab, file.path(dirs$tables, file_name), latex_text(notes),
                 col_names = c("Question", "Models", "Robust p, Holm",
                               "Beyond the draws", "Out of sample",
                               "The rule proposes", "Decision"),
                 align = c("l", "l", rep("c", 3), "l", "l"))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "decision", ALL_EXPERTS, "table", file.path("tables", file_name),
      "Every question of the rule, and its verdict", "appendix"
    ))
  }

  # 7. How each estimation ended -----------------------------------------------
  validity <- NULL
  if (!is.null(comparison)) {
    validity <- comparison$validity
  }
  tab <- tibble(
    model = latex_text(model_text(models_of_run$model)),
    code = fmt_int(models_of_run$code),
    iterations = fmt_int(models_of_run$iterations),
    # format = "e": in scientific notation, e.g. 3.6e-08
    scaled_gradient = formatC(models_of_run$scaled_gradient, format = "e",
                              digits = 1),
    condition = formatC(models_of_run$condition_number, format = "e", digits = 1),
    start = latex_text(if_else(is.na(models_of_run$start_from), "",
                               model_text(models_of_run$start_from))),
    minutes = if_else(is.na(models_of_run$minutes), "",
                      fmt_int(models_of_run$minutes)),
    valid = ""
  )
  if (!is.null(validity) && "valid" %in% names(validity)) {
    tab$valid <- if_else(validity$valid[match(models_of_run$model,
                                              validity$model)],
                         "yes", "no", missing = "")
  }
  notes <- paste0(
    "Sample: ", run_row$sample_label, ". How the estimation of each model ended: ",
    "the convergence code of the optimizer (1 or 2: a stop at a maximum), the iterations, ",
    "the scaled gradient g' H^-1 g, the condition number of the Hessian, the ",
    "model whose estimates were the starting values, and the minutes of the ",
    "estimation. Valid: at a maximum by the rule's criteria."
  )
  file_name <- "tab_diagnostics.tex"
  table_custom(tab, file.path(dirs$tables, file_name), latex_text(notes),
               col_names = c("Model", "Code", "Iterations", "Scaled gradient",
                             "Condition number", "Started from", "Minutes",
                             "Valid"),
               align = c("l", rep("c", 7)))
  exhibits <- bind_rows(exhibits, exhibit_row(
    block, "diagnostics", ALL_EXPERTS, "table", file.path("tables", file_name),
    "How each estimation ended", "appendix"
  ))

  # 8. The fit of the main model to the observed shares ------------------------
  # A cell of at least N_VIGNETTES x MIN_N decisions holds at least MIN_N experts,
  # as compare_models() counts decisions, not experts (minimum cell)
  if (!is.null(comparison) && "decisions" %in% names(comparison$absolute_fit)) {
    fit_cells <- comparison$absolute_fit %>%
      filter(model == main_model, level != "all decisions", !is.na(decisions),
             decisions >= N_VIGNETTES * MIN_N, !is.na(observed_percent)) %>%
      mutate(outcome_label = unname(OUTCOME_LABELS[outcome]))
    if (nrow(fit_cells) > 0) {
      outcome_levels <- names(OUTCOME_COLOURS)[names(OUTCOME_COLOURS) %in%
                                                 fit_cells$outcome_label]
      fit_cells$outcome_label <- factor(fit_cells$outcome_label,
                                        levels = outcome_levels)
      # geom_abline(): the line on which predicted and observed agree
      plot <- ggplot(fit_cells, aes(x = observed_percent, y = predicted_percent,
                                    colour = outcome_label)) +
        geom_abline(intercept = 0, slope = 1, colour = COLOUR_INK,
                    linewidth = 0.3) +
        geom_point(size = 1.2) +
        scale_colour_manual(values = OUTCOME_COLOURS[outcome_levels]) +
        labs(x = "Observed share (%)", y = "Predicted share (%)") +
        theme(legend.position = "right")
      file_name <- paste0("fig_fit_", main_model, ".pdf")
      save_figure(plot, file.path(dirs$figures, file_name), size = "full")
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "fit", ALL_EXPERTS, "figure", file.path("figures", file_name),
        paste0("Predicted against observed recommendations, ",
               model_text(main_model)),
        "appendix",
        paste0("Each point: the share of one recommendation among the decisions ",
               "at one level of a 0/1 term that varies within an expert, observed ",
               "and predicted by ", model_text(main_model), ". Only cells of at ",
               "least ", N_VIGNETTES * MIN_N, " decisions, which hold at least ",
               MIN_N, " experts.")
      ))
    }
  }
  list(exhibits = exhibits, problems = problems)
}
