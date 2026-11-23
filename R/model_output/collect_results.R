################################################################################
# collect_results.R — every estimation run in long tables, with their labels
#                     and the minimum cell applied, for the output script
# Inputs : <output_dir>/<run>/run_info.rds, <run>/<model>/results.rds,
#          <run>/model_comparison/model_comparison.rds and .xlsx
# Outputs: none; the tables of the sections
# Checks : every term, effect, set, model and outcome has a label; the minimum
#          cell, before anything is computed from the cells
################################################################################

#' Reads every run of the estimation scripts, one results.rds at a time (each
#' holds the Krinsky-Robb draws of its main set, about 70 MB in a full run), and
#' keeps the tables the exhibits need.
#'
#' The minimum cell (project profile) is applied here, once: a cell that may not
#' be shown keeps its status and loses its numbers, so that no exhibit can show
#' it or compute anything from it.
#'   effects, baselines  over observed experts or answers: fewer than MIN_N
#'                       experts behind the cell. At a reference expert, an
#'                       effect rests on the whole sample (Max, 2026-11-16).
#'   coefficients        fewer than MIN_N experts with the term in the rows of
#'                       its technology, or fewer than MIN_N of them who
#'                       recommended it (term_support of results.rds); then
#'                       every effect that moves the term is hidden too
#'                       (moved_terms of the set) (Claude's proposal, agreed by
#'                       Max, 2026-11-16)
#'   own constants       the same for the experts of the group
#'   SDs, correlations   hidden with the constants of a technology they involve
#'
#' @param output_dir OUTPUT_DIR
#' @param main_model the model whose main set keeps its Krinsky-Robb draws, for
#'                   the differences between groups and the sums of outcomes
#' @return list of tibbles: runs, models, outcomes, coefficients, own_constants,
#'   constants, sets, definitions, effects, baselines, problems; draws (the
#'   draws of the main set, by "run|model"); comparisons (the sheets of
#'   model_comparison.rds by run, with final_decision from the .xlsx)
collect_results <- function(output_dir, main_model) {
  # 1. The runs ----------------------------------------------------------------
  # Every folder that holds a model, as in check_results.R. list.dirs(): the
  # folders inside a folder.
  folders <- character(0)
  for (folder in list.dirs(output_dir, recursive = FALSE, full.names = FALSE)) {
    model_folders <- list.dirs(file.path(output_dir, folder), recursive = FALSE)
    if (any(file.exists(file.path(model_folders, "results.rds")))) {
      folders <- c(folders, folder)
    }
  }
  if (length(folders) == 0) {
    stop("No estimation folder with a model in ", output_dir)
  }
  # A folder with models but without run_info.rds is left out and named: a test's
  # folder, or a run of a script from before 2026-11-16, which writes it in
  # section 2 since then
  has_info <- file.exists(file.path(output_dir, folders, "run_info.rds"))
  problems <- tibble(run = character(0), what = character(0))
  if (any(!has_info)) {
    without_info <- paste0(
      "Left out, as they have models but no run_info.rds: ",
      paste(folders[!has_info], collapse = ", "), ". An estimation script writes ",
      "it in section 2: run the script again, which loads what it estimated."
    )
    message(without_info)
    problems <- tibble(run = "", what = without_info)
  }
  folders <- folders[has_info]
  if (length(folders) == 0) {
    stop("No estimation folder with run_info.rds in ", output_dir, ": run the ",
         "estimation scripts again, which write it in section 2")
  }
  runs <- tibble()
  for (folder in folders) {
    run_info <- readRDS(file.path(output_dir, folder, "run_info.rds"))
    runs <- bind_rows(runs, tibble(run = folder, arm = run_info$arm,
                                   variant = run_info$variant,
                                   sample_label = run_info$sample_label))
  }
  # str_to_lower(): in small letters, inside a sentence
  runs$run_label <- paste0(
    label_of(runs$arm, ARM_LABELS, "arm (run_info.rds)"), "; ",
    str_to_lower(label_of(runs$variant, VARIANT_LABELS,
                          "variant (run_info.rds)"))
  )
  # The figures order their rows and series by label, so a label used twice would
  # draw two things as one. anyDuplicated(): the position of the first label used
  # twice, 0 if none; duplicated(): which ones repeat an earlier one
  for (label_list in list(TERM_LABELS$label, EFFECT_LABELS$label,
                          SET_LABELS$short)) {
    if (anyDuplicated(label_list) > 0) {
      stop("A label of the models is used twice in R/functions/labels.R: ",
           paste(label_list[duplicated(label_list)], collapse = ", "))
    }
  }

  # 2. Every model -------------------------------------------------------------
  models <- tibble()
  outcomes <- tibble()
  coefficients <- tibble()
  own_constants <- tibble()
  constants <- tibble()
  sets <- tibble()
  definitions <- tibble()
  effects <- tibble()
  baselines <- tibble()
  draws <- list()
  missing_labels <- list(terms = character(0), effects = character(0),
                         sets = character(0), models = character(0),
                         outcomes = character(0))
  group_terms <- c("(Intercept)", unname(GROUP_COLUMNS))
  for (run_name in runs$run) {
    # The models of the run saved before the experts behind each coefficient were
    # counted (2026-11-16), and the effects hidden for a rare term, each named
    # once on the page "Not shown"
    without_support <- character(0)
    rare_rows <- tibble()
    for (model_folder in list.dirs(file.path(output_dir, run_name),
                                   recursive = FALSE)) {
      results_file <- file.path(model_folder, "results.rds")
      if (!file.exists(results_file)) {
        next
      }
      r <- readRDS(results_file)
      model_name <- r$name
      if (!model_name %in% names(MODEL_LABELS)) {
        missing_labels$models <- c(missing_labels$models, model_name)
      }
      # setdiff(): the outcomes without a label
      missing_labels$outcomes <- c(missing_labels$outcomes,
                                   setdiff(r$outcomes, names(OUTCOME_LABELS)))

      # (a) The model, its fit and how it ended
      start_from <- r$diagnostics$start_from
      if (is.null(start_from)) {
        start_from <- NA_character_
      }
      models <- bind_rows(models, tibble(
        run = run_name, model = model_name, label = r$label, formula = r$formula,
        reference = r$reference,
        n_experts = r$fit$n_experts, n_choices = r$fit$n_choices,
        n_parameters = r$fit$n_parameters,
        log_likelihood = r$fit$log_likelihood, mcfadden_r2 = r$fit$mcfadden_r2,
        aic = r$fit$aic, bic = r$fit$bic, tic = r$fit$tic,
        n_draws_model = r$fit$n_draws_per_expert, n_draws_kr = r$n_draws_kr,
        code = r$diagnostics$code, iterations = r$diagnostics$iterations,
        scaled_gradient = r$diagnostics$scaled_gradient,
        condition_number = r$diagnostics$condition_number,
        start_from = as.character(start_from),
        minutes = as.numeric(r$diagnostics$minutes_estimation),
        sample_hash = r$sample_hash,
        # file.mtime(): when the file was last written
        written = file.mtime(results_file)
      ))
      # The outcomes in the order of the tables (labels.R). match(): the position
      # in the vector.
      outcomes <- bind_rows(outcomes, tibble(
        run = run_name, model = model_name, outcome = r$outcomes,
        position = match(r$outcomes, names(OUTCOME_LABELS))
      ))

      # (b) The coefficients, and the experts behind each (post_estimation())
      model_terms <- coefficient_parts(r)$terms %>%
        left_join(r$coefficient_table, by = "parameter",
                  relationship = "one-to-one")
      missing_labels$terms <- c(
        missing_labels$terms,
        setdiff(model_terms$column, c(group_terms, TERM_LABELS$term))
      )
      if (is.null(r$term_support)) {
        without_support <- c(without_support, model_name)
        model_terms$experts_with_term <- NA_real_
        model_terms$experts_choosing <- NA_real_
      } else {
        model_terms <- model_terms %>%
          left_join(r$term_support %>%
                      select(parameter, experts_with_term, experts_choosing),
                    by = "parameter", relationship = "one-to-one")
      }
      # Without the counts, a coefficient counts as resting on few experts: the
      # rule fails closed, it hides rather than shows (2026-11-16)
      model_terms <- model_terms %>%
        mutate(status = if_else(experts_with_term < MIN_N |
                                  experts_choosing < MIN_N, "too few", "shown",
                                missing = "too few"))
      # The columns of make_regressors() with a coefficient that is hidden
      masked_columns <- unique(model_terms$column[model_terms$status == "too few"])
      coefficients <- bind_rows(coefficients, model_terms %>%
        mutate(run = run_name, model = model_name,
               estimate = if_else(status == "shown", estimate, NA_real_),
               se = if_else(status == "shown", se, NA_real_),
               z = if_else(status == "shown", z, NA_real_),
               p = if_else(status == "shown", p, NA_real_)) %>%
        select(run, model, parameter, column, technology, estimate, se, z, p,
               experts_with_term, experts_choosing, status))

      # (c) Each group's own constant: the constant plus the group's shift, or the
      # group's constant in a model with constants by group (the seminar's
      # convention: a row shows a group's own value). Its standard error from the
      # clustered covariance of the sum. The groups as post_estimation() counts
      # them in count_term_support().
      technologies <- setdiff(r$outcomes, r$reference)
      has_constant <- "(Intercept)" %in% model_terms$column
      has_group_column <- GROUP_COLUMNS %in% model_terms$column
      group_parts <- list()
      if (!any(has_group_column)) {
        group_parts[[ALL_EXPERTS]] <- "(Intercept)"
      } else {
        for (group in names(GROUP_COLUMNS)[has_group_column]) {
          group_parts[[group]] <- c(if (has_constant) "(Intercept)",
                                    GROUP_COLUMNS[[group]])
        }
        groups_without_column <- names(GROUP_COLUMNS)[!has_group_column]
        if (has_constant && length(groups_without_column) == 1) {
          group_parts[[groups_without_column]] <- "(Intercept)"
        } else if (has_constant && length(groups_without_column) > 1) {
          group_parts[["Other experts"]] <- "(Intercept)"
        }
      }
      for (group in names(group_parts)) {
        for (technology in technologies) {
          parameters <- paste0(group_parts[[group]], ":", technology)
          if (!all(parameters %in% names(r$coefficients))) {
            next
          }
          estimate <- sum(r$coefficients[parameters])
          # The variance of a sum: the sum of the elements of its covariance
          se <- sqrt(sum(r$vcov[parameters, parameters]))
          # Shown only with a count of MIN_N experts and more behind it: without
          # the counts it fails closed, as the coefficients do
          status <- "too few"
          if (!is.null(r$constant_support)) {
            # The row of this group and technology; base R subsetting, as the
            # loop's names are also the table's column names
            support <- r$constant_support[
              r$constant_support$group == group &
                r$constant_support$technology == technology, ]
            if (nrow(support) == 1 && support$experts >= MIN_N &&
                support$experts_choosing >= MIN_N) {
              status <- "shown"
            }
          }
          own_constants <- bind_rows(own_constants, tibble(
            run = run_name, model = model_name, group = group,
            technology = technology,
            estimate = if_else(status == "shown", estimate, NA_real_),
            se = if_else(status == "shown", se, NA_real_),
            p = if_else(status == "shown", 2 * pnorm(-abs(estimate / se)),
                        NA_real_),
            status = status
          ))
        }
      }

      # (d) The standard deviations and correlations of the random constants, with
      # the names post_estimation() gives them. A set of constants is all experts'
      # or one group's.
      random <- r$random
      constants_of <- if_else(
        random$column == "(Intercept)", ALL_EXPERTS,
        names(GROUP_COLUMNS)[match(random$column, GROUP_COLUMNS)]
      )
      constant_rows <- tibble(parameter = paste0("sd.", random$name), type = "sd",
                              constants_of = constants_of,
                              technology_1 = random$technology,
                              technology_2 = NA_character_)
      for (i in seq_len(nrow(random))) {
        for (j in seq_len(i - 1)) {
          if (random$column[i] == random$column[j]) {
            constant_rows <- bind_rows(constant_rows, tibble(
              parameter = paste0("cor.", random$name[j], ":", random$name[i]),
              type = "cor", constants_of = constants_of[i],
              technology_1 = random$technology[j],
              technology_2 = random$technology[i]
            ))
          }
        }
      }
      # Hidden with the constants of a technology they involve: of the group, or
      # for all experts the constant's own support
      hidden_constants <- character(0)
      for (k in seq_len(nrow(constant_rows))) {
        involved <- c(constant_rows$technology_1[k], constant_rows$technology_2[k])
        involved <- involved[!is.na(involved)]
        if (constant_rows$constants_of[k] == ALL_EXPERTS) {
          hidden <- model_terms %>%
            filter(column == "(Intercept)", technology %in% involved,
                   status == "too few")
        } else {
          hidden <- own_constants %>%
            filter(run == run_name, model == model_name,
                   group == constant_rows$constants_of[k],
                   technology %in% involved, status == "too few")
        }
        if (nrow(hidden) > 0) {
          hidden_constants <- c(hidden_constants, constant_rows$parameter[k])
        }
      }
      constants <- bind_rows(constants, constant_rows %>%
        left_join(r$constant_table %>% select(-type), by = "parameter",
                  relationship = "one-to-one") %>%
        mutate(run = run_name, model = model_name,
               status = if_else(parameter %in% hidden_constants, "too few",
                                "shown"),
               estimate = if_else(status == "shown", estimate, NA_real_),
               se = if_else(status == "shown", se, NA_real_),
               ci_low_kr = if_else(status == "shown", ci_low_kr, NA_real_),
               ci_high_kr = if_else(status == "shown", ci_high_kr, NA_real_)) %>%
        select(run, model, parameter, type, constants_of, technology_1,
               technology_2, estimate, se, ci_low_kr, ci_high_kr, status))

      # (e) Every set of effects: what it is, and its cells with their status. The
      # household of a set also as what differs from the main set's, for notes.
      main_at <- list()
      if (MAIN_AMES %in% names(r$ames)) {
        main_at <- r$ames[[MAIN_AMES]]$at
      }
      for (set_name in names(r$ames)) {
        ames <- r$ames[[set_name]]
        changed_at <- list()
        # intersect(): the attributes of the design among the variables of `at`
        for (variable in intersect(names(ames$at), DESIGN_LEVELS$attribute)) {
          if (!identical(main_at[[variable]], ames$at[[variable]])) {
            changed_at[[variable]] <- ames$at[[variable]]
          }
        }
        household_change <- ""
        if (length(changed_at) > 0) {
          household_change <- household_text(changed_at)
        }
        if (!set_name %in% SET_LABELS$set) {
          missing_labels$sets <- c(missing_labels$sets, set_name)
        }
        effect_names <- names(ames$changes)
        unlabelled <- setdiff(effect_names, EFFECT_LABELS$effect)
        missing_labels$effects <- c(missing_labels$effects, unlabelled)
        set_profiles <- unique(ames$baseline$profile)
        kind <- "experts"
        if (is.null(ames$population)) {
          kind <- "reference expert"
        } else if (length(ames$at) == 0) {
          kind <- "answers"
        } else if (all(set_profiles %in% names(STATE_LABELS))) {
          kind <- "states"
        }
        values_text <- ""
        if (length(unlabelled) == 0) {
          values_text <- change_values_text(ames$changes, effect_names)
        }
        sets <- bind_rows(sets, tibble(
          run = run_name, model = model_name, set = set_name, kind = kind,
          household = household_text(ames$at),
          household_change = household_change,
          # rlang::hash(): a fingerprint of an object, to compare two of them
          at_hash = rlang::hash(ames$at),
          values_text = values_text,
          n_draws = ames$n_draws, n_draws_kr = ames$n_draws_kr,
          has_draws = !is.null(ames$draws)
        ))
        for (effect in effect_names) {
          definitions <- bind_rows(definitions, tibble(
            run = run_name, model = model_name, set = set_name, effect = effect,
            definition_hash = rlang::hash(ames$changes[[effect]])
          ))
        }

        # The effects that move a term whose coefficient is hidden. Without the
        # counts (said once per run, below) or without the moved terms (sets saved
        # before 2026-11-16), every effect of the set is hidden: the rule fails
        # closed, as which effects rest on few experts is not known.
        rare_effects <- character(0)
        if (is.null(r$term_support)) {
          rare_effects <- effect_names
        } else if (!is.null(ames$moved_terms)) {
          for (effect in names(ames$moved_terms)) {
            if (any(ames$moved_terms[[effect]] %in% masked_columns)) {
              rare_effects <- c(rare_effects, effect)
            }
          }
        } else if (length(masked_columns) > 0) {
          rare_effects <- effect_names
          problems <- bind_rows(problems, tibble(run = run_name, what = paste0(
            model_name, ", ", set_name, ": every effect hidden, as the terms each ",
            "moves are not saved and some terms rest on fewer than ", MIN_N,
            " experts. Run the estimation script again: compute_ames() adds them ",
            "without computing again."
          )))
        }
        if (length(rare_effects) > 0 && !is.null(r$term_support) &&
            !is.null(ames$moved_terms)) {
          rare_rows <- bind_rows(rare_rows, tibble(
            model = model_name,
            columns = paste(masked_columns, collapse = ", "),
            effects = paste(intersect(effect_names, rare_effects), collapse = ", ")
          ))
        }

        set_table <- ames$table
        set_baseline <- ames$baseline
        # Effects saved before 12 November 2026 have no counts: over observed
        # units, nothing of them can be shown
        if (!"n_experts" %in% names(set_table)) {
          set_table$n_units <- NA_integer_
          set_table$n_experts <- NA_integer_
          set_baseline$n_units <- NA_integer_
          set_baseline$n_experts <- NA_integer_
        }
        has_population <- !is.null(ames$population)
        effects <- bind_rows(effects, set_table %>%
          mutate(run = run_name, model = model_name, set = set_name,
                 status = case_when(
                   n_units %in% 0 ~ "not applicable",
                   has_population & is.na(n_experts) ~ "too few",
                   n_experts < MIN_N ~ "too few",
                   effect %in% rare_effects ~ "too few",
                   TRUE ~ "shown"
                 ),
                 ame = if_else(status == "shown", ame, NA_real_),
                 se = if_else(status == "shown", se, NA_real_),
                 p = if_else(status == "shown", p, NA_real_),
                 ci_low_kr = if_else(status == "shown", ci_low_kr, NA_real_),
                 ci_high_kr = if_else(status == "shown", ci_high_kr, NA_real_),
                 n_experts = if_else(status == "shown", as.numeric(n_experts),
                                     NA_real_)) %>%
          select(run, model, set, profile, effect, outcome, ame, se, p, ci_low_kr,
                 ci_high_kr, n_experts, status))
        baselines <- bind_rows(baselines, set_baseline %>%
          mutate(run = run_name, model = model_name, set = set_name,
                 status = case_when(
                   has_population & is.na(n_experts) ~ "too few",
                   n_experts < MIN_N ~ "too few",
                   TRUE ~ "shown"
                 ),
                 probability = if_else(status == "shown", probability, NA_real_),
                 se = if_else(status == "shown", se, NA_real_),
                 ci_low_kr = if_else(status == "shown", ci_low_kr, NA_real_),
                 ci_high_kr = if_else(status == "shown", ci_high_kr, NA_real_),
                 n_experts = if_else(status == "shown", as.numeric(n_experts),
                                     NA_real_)) %>%
          select(run, model, set, profile, outcome, probability, se, ci_low_kr,
                 ci_high_kr, n_experts, status))
        # The draws of the main model's main set, for differences and sums that
        # the sections compute only from cells that are shown
        if (model_name == main_model && set_name == MAIN_AMES &&
            !is.null(ames$draws)) {
          draws[[paste(run_name, model_name, sep = "|")]] <- ames$draws
        }
      }
      # Only the tables are kept: the next results.rds can take its memory
      rm(r)
    }
    if (length(without_support) > 0) {
      problems <- bind_rows(problems, tibble(run = run_name, what = paste0(
        "The experts behind each coefficient are not saved for ",
        paste(without_support, collapse = ", "), ", so their coefficients, ",
        "constants and effects are hidden. Run the estimation script again: ",
        "post_estimation() adds the counts without estimating again."
      )))
    }
    # One line per group of effects hidden for the same rare terms, with the
    # models where that happens
    if (nrow(rare_rows) > 0) {
      rare_summary <- rare_rows %>%
        distinct(model, columns, effects) %>%
        group_by(columns, effects) %>%
        summarise(models = paste(model, collapse = ", ")) %>%
        ungroup()
      for (k in seq_len(nrow(rare_summary))) {
        problems <- bind_rows(problems, tibble(run = run_name, what = paste0(
          "Hidden in ", rare_summary$models[k], ", in every set that has them: ",
          rare_summary$effects[k], ", as they move a term that rests on fewer ",
          "than ", MIN_N, " experts (", rare_summary$columns[k], ")"
        )))
      }
    }
  }

  # 3. Labels ------------------------------------------------------------------
  missing_text <- character(0)
  for (kind in names(missing_labels)) {
    if (length(missing_labels[[kind]]) > 0) {
      missing_text <- c(missing_text, paste0(
        kind, ": ", paste(unique(missing_labels[[kind]]), collapse = ", ")
      ))
    }
  }
  if (length(missing_text) > 0) {
    stop("Add these labels to R/functions/labels.R (TERM_LABELS, EFFECT_LABELS, ",
         "SET_LABELS, MODEL_LABELS, OUTCOME_LABELS), so that no raw name reaches ",
         "an exhibit:\n", paste(missing_text, collapse = "\n"))
  }
  outcomes$outcome_label <- unname(OUTCOME_LABELS[outcomes$outcome])
  effects <- effects %>%
    left_join(EFFECT_LABELS %>% select(effect, effect_label = label, block),
              by = "effect", relationship = "many-to-one") %>%
    mutate(outcome_label = unname(OUTCOME_LABELS[outcome]))
  baselines$outcome_label <- unname(OUTCOME_LABELS[baselines$outcome])

  # 4. The model comparisons ---------------------------------------------------
  # Your final decisions are in the workbook, not in the .rds, which a run writes
  # with an empty column (compare_models())
  comparisons <- list()
  for (run_name in runs$run) {
    comparison_dir <- file.path(output_dir, run_name, "model_comparison")
    comparison_file <- file.path(comparison_dir, "model_comparison.rds")
    if (!file.exists(comparison_file)) {
      problems <- bind_rows(problems, tibble(run = run_name, what = paste0(
        "No model_comparison.rds: the comparisons of the models leave out the ",
        "tests and the rule's proposals"
      )))
      next
    }
    comparison <- readRDS(comparison_file)
    comparison$decision$final_decision <- NA_character_
    workbook <- file.path(comparison_dir, "model_comparison.xlsx")
    if (file.exists(workbook)) {
      # col_types = "text": every column as text, as a decision is words
      filled <- read_excel(workbook, sheet = "decision", col_types = "text") %>%
        select(smaller, larger, final_decision_filled = final_decision)
      comparison$decision <- comparison$decision %>%
        left_join(filled, by = c("smaller", "larger"),
                  relationship = "one-to-one") %>%
        mutate(final_decision = final_decision_filled) %>%
        select(-final_decision_filled)
    }
    newest_results <- max(models$written[models$run == run_name])
    if (file.mtime(comparison_file) < newest_results) {
      problems <- bind_rows(problems, tibble(run = run_name, what = paste0(
        "The model comparison is older than a results.rds of this run: run the ",
        "estimation script to its end, so that compare_models() sees every model"
      )))
    }
    comparisons[[run_name]] <- comparison
  }

  list(runs = runs, models = models, outcomes = outcomes,
       coefficients = coefficients, own_constants = own_constants,
       constants = constants, sets = sets, definitions = definitions,
       effects = effects, baselines = baselines, draws = draws,
       comparisons = comparisons, problems = problems)
}
