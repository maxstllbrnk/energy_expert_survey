################################################################################
# check_results.R — a text summary of every estimation run: the models, their
#                   fit, the comparison and every set of effects
# Inputs : OUTPUT_DIR/<folder>/<model>/results.rds and
#          OUTPUT_DIR/<folder>/model_comparison/model_comparison.rds, for every
#          folder that holds a model (QUICK_RUN in config.R picks output_quick/)
# Outputs: OUTPUT_DIR/check_results.txt, the same as the console
# Checks : aggregates only. A number that rests on fewer than MIN_N experts, and
#          a count below MIN_N, print as "<10", and such cells enter no
#          comparison (project profile)
################################################################################
# To have a run checked by someone without access to the secure drive: source
# this script and share check_results.txt. It finds the folders, models, sets of
# effects, profiles and outcomes itself, so it needs no change when the
# estimation scripts change. A section that cannot be shown says why, and the
# others follow; warnings and messages go into the file too. Coefficients, the
# standard deviations and correlations of the constants and effects at a
# reference expert are printed unmasked, as they rest on the whole estimation
# sample: an exception to the minimum cell (project profile, Max, 2026-11-14).
source(file.path("R", "setup.R"))

# 1. Settings ------------------------------------------------------------------
# NULL checks every folder of OUTPUT_DIR that holds a model; or name some, e.g.
# c("district_heating")
folders_to_check <- NULL
check_file <- file.path(OUTPUT_DIR, "check_results.txt")

#' A number with its digits, a star if its 95% Krinsky-Robb interval excludes 0,
#' and "<10" if it rests on fewer than MIN_N experts. n_experts is NA at a
#' reference expert, whose effects rest on the whole sample.
shown <- function(x, n_experts, digits = 1, significant = FALSE) {
  # formatC(format = "f"): the number with exactly `digits` decimals
  text <- paste0(formatC(x, format = "f", digits = digits),
                 if_else(significant %in% TRUE, "*", ""))
  text[is.na(x)] <- "."
  text[!is.na(n_experts) & n_experts < MIN_N] <- "<10"
  text
}

#' A coefficient as "estimate (z)"
estimate_with_z <- function(estimate, z, digits) {
  paste0(formatC(estimate, format = "f", digits = digits), " (",
         formatC(z, format = "f", digits = 1), ")")
}

#' The variables and values of a state, on one line; numbers to four digits
describe_values <- function(values, if_empty) {
  if (length(values) == 0) {
    return(if_empty)
  }
  parts <- character(0)
  for (variable in names(values)) {
    value <- values[[variable]]
    if (is.numeric(value)) {
      # signif(): rounded to four significant digits
      value <- signif(value, 4)
    }
    parts <- c(parts, paste0(variable, " = ", value))
  }
  paste(parts, collapse = ", ")
}

#' A field of results.rds, or NA if an older run did not save it
value_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) NA else x
}

#' A table in full, without row numbers
print_table <- function(x) {
  print(as.data.frame(x), row.names = FALSE, right = TRUE)
}

#' Runs `code`. An error prints `failure` and its message, and the summary goes
#' on; a warning or message is printed where it occurs, so that it reaches the
#' file (sink() catches printed output only).
guarded <- function(code, failure) {
  # withCallingHandlers(): hands each warning or message to the function named
  # for it while the code runs on; invokeRestart() keeps it from printing a
  # second time. tryCatch(): stops the code at an error and runs `error`.
  withCallingHandlers(
    tryCatch(code, error = function(e) {
      cat("  ", failure, conditionMessage(e), "\n")
    }),
    warning = function(w) {
      cat("   Warning:", conditionMessage(w), "\n")
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      cat("   Message:", conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  invisible(NULL)
}

#' One numbered section of the summary
section <- function(title, code) {
  cat("\n--", title, "\n")
  guarded(code, "This section could not be shown:")
}

# 2. The folders that hold a model ---------------------------------------------
# list.dirs(): the folders inside a folder
folders <- character(0)
for (folder in list.dirs(OUTPUT_DIR, recursive = FALSE, full.names = FALSE)) {
  model_folders <- list.dirs(file.path(OUTPUT_DIR, folder), recursive = FALSE)
  if (any(file.exists(file.path(model_folders, "results.rds")))) {
    folders <- c(folders, folder)
  }
}
if (!is.null(folders_to_check)) {
  # intersect(): the elements that are in both
  folders <- intersect(folders, folders_to_check)
}

# 3. One summary per folder ----------------------------------------------------
# sink(split = TRUE): everything printed goes to the file and the console
# max.print: an R session may print 1,000 values only and cut a wide table there
# (2026-11-22, the full run's sections 9, 10 and 12)
old_options <- options(width = 250, max.print = 1e6)
sink(check_file, split = TRUE)
tryCatch({
  # system(intern = TRUE): runs a command and returns what it prints
  git_commit <- tryCatch(
    system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) NA_character_
  )
  # basename(): the last part of a path
  cat("check_results.R,", format(Sys.time(), "%Y-%m-%d %H:%M"),
      "| output:", basename(OUTPUT_DIR), "| QUICK_RUN:", QUICK_RUN,
      "| code now at git", git_commit, "| MIN_N:", MIN_N, "\n")
  cat("Folders with models:", if (length(folders) == 0) "none" else folders,
      "\n")

  for (folder in folders) {
    est_dir <- file.path(OUTPUT_DIR, folder)
    cat("\n\n====================", folder, "====================\n")
    guarded(failure = "This folder could not be summarised:", code = {

      # The models, smallest first, so that a model comes after those it nests
      results <- list()
      for (model_folder in list.dirs(est_dir, recursive = FALSE)) {
        results_file <- file.path(model_folder, "results.rds")
        if (file.exists(results_file)) {
          results[[basename(model_folder)]] <- readRDS(results_file)
        }
      }
      n_parameters <- integer(0)
      for (model_name in names(results)) {
        n_parameters[model_name] <- results[[model_name]]$fit$n_parameters
      }
      # order(): the positions that sort the values
      results <- results[order(n_parameters, names(results))]
      model_names <- names(results)
      # Effects saved before 12 November 2026 have no counts of units and
      # experts; they are all at a reference expert
      for (model_name in model_names) {
        for (set_name in names(results[[model_name]]$ames)) {
          a <- results[[model_name]]$ames[[set_name]]
          for (count in c("n_units", "n_experts")) {
            if (!count %in% names(a$table)) {
              a$table[[count]] <- NA_integer_
              a$baseline[[count]] <- NA_integer_
            }
          }
          results[[model_name]]$ames[[set_name]] <- a
        }
      }

      section("1. The models: formula, fit and convergence", {
        for (model_name in model_names) {
          r <- results[[model_name]]
          # strwrap(): breaks a long text into lines of at most `width`
          cat(strwrap(paste0(model_name, " (", r$label, "), reference outcome ",
                             r$reference, ": ", r$formula),
                      width = 150, indent = 3, exdent = 7), sep = "\n")
        }
        # gain_over_start: the log-likelihood gained over the model it started
        # from, which it nests, so it should be >= 0. trace_ratio: tr(H^-1 B)
        # per parameter, near 1 in a well-specified model.
        fit_table <- tibble()
        for (model_name in model_names) {
          r <- results[[model_name]]
          diagnostics <- r$diagnostics
          log_lik_start <- as.numeric(
            value_or_na(diagnostics$log_likelihood_start)
          )
          minutes <- as.numeric(value_or_na(diagnostics$minutes_estimation))
          fit_table <- bind_rows(fit_table, tibble(
            model = model_name,
            experts = r$fit$n_experts,
            choices = r$fit$n_choices,
            parameters = r$fit$n_parameters,
            draws = r$fit$n_draws_per_expert,
            kr_draws = r$n_draws_kr,
            log_lik = round(r$fit$log_likelihood, 1),
            start_from = as.character(value_or_na(diagnostics$start_from)),
            gain_over_start = round(r$fit$log_likelihood - log_lik_start, 1),
            mcfadden_r2 = round(r$fit$mcfadden_r2, 3),
            bic = round(r$fit$bic, 1),
            trace_ratio = round(r$fit$trace_hinv_b / r$fit$n_parameters, 2),
            code = diagnostics$code,
            iterations = diagnostics$iterations,
            scaled_gradient = signif(diagnostics$scaled_gradient, 2),
            smallest_eigen = signif(diagnostics$smallest_eigenvalue, 2),
            condition = signif(diagnostics$condition_number, 2),
            minutes = round(minutes),
            effect_sets = length(r$ames),
            date = as.character(r$date),
            git = as.character(value_or_na(r$git_commit))
          ))
        }
        print_table(fit_table)
      })

      # Shares only: a count of answers would need 10 distinct experts behind
      # it, which results.rds does not record (project profile)
      section("2. Recommendations in the estimation sample: % of choices", {
        sample_hashes <- character(0)
        for (model_name in model_names) {
          sample_hashes[model_name] <- results[[model_name]]$sample_hash
        }
        one_sample <- n_distinct(sample_hashes) == 1
        cat("   One sample for all models:", one_sample, "\n")
        models_shown <- model_names
        if (one_sample) {
          models_shown <- model_names[1]
        }
        shares_table <- tibble(outcome = results[[model_names[1]]]$outcomes)
        for (model_name in models_shown) {
          counts <- results[[model_name]]$choice_counts
          shares_table[[model_name]] <-
            round(100 * as.numeric(counts[shares_table$outcome]) / sum(counts),
                  1)
        }
        print_table(shares_table)
      })

      # The coefficients, each with its term and technology (NA: one
      # coefficient for all technologies), as compute_ames() reads them
      coefficients <- tibble()
      for (model_name in model_names) {
        r <- results[[model_name]]
        terms <- coefficient_parts(r)$terms
        coefficients <- bind_rows(coefficients, terms %>%
          left_join(r$coefficient_table, by = "parameter",
                    relationship = "one-to-one") %>%
          mutate(model = model_name))
      }

      section("3. Coefficients common to all technologies: estimate (z)", {
        common <- coefficients %>%
          filter(is.na(technology)) %>%
          mutate(value = estimate_with_z(estimate, z, 3)) %>%
          select(column, model, value) %>%
          pivot_wider(names_from = model, values_from = value,
                      values_fill = ".")
        if (nrow(common) == 0) {
          cat("   none\n")
        } else {
          print_table(common)
        }
      })

      section(paste0("4. Coefficients per technology, against the reference ",
                     "outcome: estimate (z)"), {
        for (model_name in model_names) {
          cat("  ", model_name, "\n")
          print_table(coefficients %>%
                        filter(model == model_name, !is.na(technology)) %>%
                        mutate(value = estimate_with_z(estimate, z, 2)) %>%
                        select(column, technology, value) %>%
                        pivot_wider(names_from = technology, values_from = value,
                                    values_fill = "."))
        }
      })

      section(paste0("5. Random constants: standard deviations [95% ",
                     "Krinsky-Robb interval], and correlations"), {
        sds <- tibble()
        correlation_notes <- character(0)
        for (model_name in model_names) {
          constants <- results[[model_name]]$constant_table
          if (is.null(constants) || nrow(constants) == 0) {
            next
          }
          # sub(): replaces the first match of a pattern, here the prefix
          sds <- bind_rows(sds, constants %>%
            filter(type == "sd") %>%
            transmute(constant = sub("^sd\\.", "", parameter),
                      model = model_name,
                      value = paste0(round(estimate, 2), " [",
                                     round(ci_low_kr, 2), ", ",
                                     round(ci_high_kr, 2), "]")))
          correlations <- constants %>% filter(type == "cor")
          if (nrow(correlations) == 0) {
            next
          }
          high <- correlations %>% filter(abs(estimate) > 0.9)
          high_text <- "none"
          if (nrow(high) > 0) {
            high_text <- paste0(sub("^cor\\.", "", high$parameter), " ",
                                round(high$estimate, 2), " [",
                                round(high$ci_low_kr, 2), ", ",
                                round(high$ci_high_kr, 2), "]",
                                collapse = "; ")
          }
          correlation_notes <- c(correlation_notes, paste0(
            model_name, ": ", nrow(correlations), " correlations, from ",
            round(min(correlations$estimate), 2), " to ",
            round(max(correlations$estimate), 2),
            "; above 0.9 in absolute value: ", high_text
          ))
        }
        print_table(sds %>%
                      pivot_wider(names_from = model, values_from = value,
                                  values_fill = "."))
        cat(strwrap(correlation_notes, width = 150, indent = 3, exdent = 7),
            sep = "\n")
      })

      section("6. Model comparison: the rule's proposals", {
        comparison_file <- file.path(est_dir, "model_comparison",
                                     "model_comparison.rds")
        if (!file.exists(comparison_file)) {
          cat("   No model_comparison.rds\n")
        } else {
          # file.mtime(): when a file was last written
          newest_model <- max(file.mtime(file.path(est_dir, model_names,
                                                   "results.rds")))
          cat("   Written",
              format(file.mtime(comparison_file), "%Y-%m-%d %H:%M"),
              "; newest results.rds", format(newest_model, "%Y-%m-%d %H:%M"),
              "\n")
          decision <- readRDS(comparison_file)$decision
          # any_of(): the columns of this list that the sheet has
          decision_shown <- decision %>%
            select(any_of(c("question", "smaller", "larger", "verdict_validity",
                            "proposal", "final_decision", "lr", "df",
                            "p_robust", "p_robust_holm", "lr_fresh_mean",
                            "lr_fresh_sd", "critical_value", "delta_bic",
                            "cv_gain_per_expert", "cv_paired_se",
                            "ame_rows_moved", "ame_largest_change_pp",
                            "ame_added_largest_pp")))
          # Three significant digits, small p-values as 1.2e-05
          for (column in names(decision_shown)) {
            if (is.numeric(decision_shown[[column]])) {
              decision_shown[[column]] <- formatC(decision_shown[[column]],
                                                  format = "g", digits = 3)
            }
          }
          print_table(decision_shown)
          for (i in seq_len(nrow(decision))) {
            cat(strwrap(paste0(decision$question[i], " ", decision$reason[i]),
                        width = 150, indent = 3, exdent = 5), sep = "\n")
          }
        }
      })

      # Every set of effects of every model, in one table each. Only the rows
      # that may be shown enter a comparison between rows (section 12).
      effects <- tibble()
      baselines <- tibble()
      for (model_name in model_names) {
        for (set_name in names(results[[model_name]]$ames)) {
          a <- results[[model_name]]$ames[[set_name]]
          effects <- bind_rows(effects, a$table %>%
                                 mutate(model = model_name, set = set_name))
          baselines <- bind_rows(baselines, a$baseline %>%
                                   mutate(model = model_name, set = set_name))
        }
      }
      if (nrow(effects) == 0) {
        cat("\n-- No sets of effects in this folder\n")
        next
      }
      effects_shown <- effects %>%
        filter(is.na(n_experts) | n_experts >= MIN_N)
      effect_order <- unique(effects$effect)
      profile_order <- unique(c(intersect(ALL_EXPERTS, baselines$profile),
                                baselines$profile))

      section(paste0("7. The sets of effects and their checks. baseline_dev: ",
                     "largest gap of a profile's probabilities from summing ",
                     "to 1; effect_dev: largest sum of an effect over the ",
                     "outcomes; outside_kr: estimates outside their own ",
                     "Krinsky-Robb interval; significant_pct: profile x ",
                     "effect with an outcome whose interval excludes 0; the ",
                     "last three over the cells that may be shown"), {
        overview <- tibble()
        notes <- character(0)
        share_check <- tibble()
        for (model_name in model_names) {
          r <- results[[model_name]]
          for (set_name in names(r$ames)) {
            a <- r$ames[[set_name]]
            table_shown <- a$table %>%
              filter(is.na(n_experts) | n_experts >= MIN_N)
            baseline_shown <- a$baseline %>%
              filter(is.na(n_experts) | n_experts >= MIN_N)
            baseline_sums <- a$baseline %>%
              group_by(profile) %>%
              summarise(total = sum(probability))
            effect_sums <- a$table %>%
              filter(!is.na(ame)) %>%
              group_by(profile, effect) %>%
              summarise(total = sum(ame), .groups = "drop")
            significant <- table_shown %>%
              filter(!is.na(ame)) %>%
              group_by(profile, effect) %>%
              summarise(any = any(ci_low_kr > 0 | ci_high_kr < 0),
                        .groups = "drop")
            population_units <- as.numeric(value_or_na(a$population$units))
            population_experts <- as.numeric(
              value_or_na(a$population$experts)
            )
            overview <- bind_rows(overview, tibble(
              model = model_name,
              set = set_name,
              profiles = n_distinct(a$baseline$profile),
              effects = n_distinct(a$table$effect),
              units = shown(population_units, population_units, 0),
              experts = shown(population_experts, population_experts, 0),
              draws = a$n_draws,
              kr_draws = a$n_draws_kr,
              date = as.character(a$date),
              # format = "e": in scientific notation, e.g. 2e-16
              baseline_dev = formatC(max(abs(baseline_sums$total - 1)),
                                     format = "e", digits = 0),
              effect_dev = formatC(max(abs(effect_sums$total), 0),
                                   format = "e", digits = 0),
              outside_kr = sum(table_shown$ame < table_shown$ci_low_kr |
                                 table_shown$ame > table_shown$ci_high_kr,
                               na.rm = TRUE) +
                sum(baseline_shown$probability < baseline_shown$ci_low_kr |
                      baseline_shown$probability > baseline_shown$ci_high_kr),
              median_ci_width_pp = round(100 * median(
                table_shown$ci_high_kr - table_shown$ci_low_kr, na.rm = TRUE
              ), 1),
              significant_pct = round(100 * mean(significant$any))
            ))
            # Effects that move nothing, that no unit is in, and that leave
            # experts out
            moving <- a$table %>%
              group_by(effect) %>%
              summarise(moves = any(se > 0, na.rm = TRUE),
                        empty = all(n_units %in% 0))
            left_out <- a$table %>%
              distinct(profile, effect, n_experts) %>%
              left_join(distinct(a$baseline, profile, n_all = n_experts),
                        by = "profile", relationship = "many-to-one") %>%
              mutate(n_left_out = n_all - n_experts) %>%
              filter(n_left_out > 0)
            left_out_text <- "none"
            if (nrow(left_out) > 0) {
              left_out <- left_out %>%
                group_by(effect) %>%
                summarise(most = max(n_left_out))
              left_out_text <- paste0(left_out$effect, " (",
                                      shown(left_out$most, left_out$most, 0),
                                      ")", collapse = " ")
            }
            zero_effects <- moving$effect[!moving$moves & !moving$empty]
            empty_effects <- moving$effect[moving$empty]
            notes <- c(notes, paste0(
              model_name, " ", set_name, ": exactly 0: ",
              if (length(zero_effects) == 0) "none" else
                paste(zero_effects, collapse = " "),
              " | no units: ",
              if (length(empty_effects) == 0) "none" else
                paste(empty_effects, collapse = " "),
              " | experts left out, at most: ", left_out_text
            ))
            # Over all the answers of the sample, with nothing set, the average
            # probabilities should be close to the observed shares
            for (profile_name in unique(a$baseline$profile)) {
              profile_baseline <- a$baseline %>%
                filter(profile == profile_name)
              if (length(a$at) == 0 &&
                  isTRUE(profile_baseline$n_units[1] == r$fit$n_choices)) {
                observed <- as.numeric(
                  r$choice_counts[profile_baseline$outcome]
                ) / r$fit$n_choices
                share_check <- bind_rows(share_check, tibble(
                  model = model_name, set = set_name, profile = profile_name,
                  outcome = profile_baseline$outcome,
                  predicted_pct = round(100 * profile_baseline$probability, 1),
                  observed_pct = round(100 * observed, 1),
                  gap_pp = round(100 * (profile_baseline$probability -
                                          observed), 2)
                ))
              }
            }
          }
        }
        print_table(overview)
        cat(strwrap(notes, width = 150, indent = 3, exdent = 7), sep = "\n")
        if (nrow(share_check) > 0) {
          cat("   Average probability over all answers against the observed",
              "share:\n")
          print_table(share_check)
        }
      })

      section("8. What each set sets: who, the reference state, the changes", {
        states <- tibble()
        definitions <- tibble()
        for (model_name in model_names) {
          for (set_name in names(results[[model_name]]$ames)) {
            a <- results[[model_name]]$ames[[set_name]]
            population <- "a reference expert"
            if (!is.null(a$population)) {
              population <- paste0("observed units, exclude_na = ",
                                   a$population$exclude_na)
            }
            cat("  ", paste0(model_name, ":", set_name), "|", population,
                "| profiles:",
                paste(unique(a$baseline$profile), collapse = ", "), "\n")
            states <- bind_rows(states, tibble(
              at = describe_values(a$at, "nothing set, each unit's own values"),
              used_in = paste0(model_name, ":", set_name)
            ))
            for (effect in names(a$changes)) {
              definitions <- bind_rows(definitions, tibble(
                effect = effect,
                from = describe_values(a$changes[[effect]]$from,
                                       "at, or each unit's own"),
                to = describe_values(a$changes[[effect]]$to, "?"),
                used_in = paste0(model_name, ":", set_name)
              ))
            }
          }
        }
        # Each reference state and each change once, with the sets that use
        # it; a change whose name has one definition only says in how many sets
        states <- states %>%
          group_by(at) %>%
          summarise(sets = paste(used_in, collapse = " "), .groups = "drop")
        cat("   The reference states (at):\n")
        for (i in seq_len(nrow(states))) {
          cat(strwrap(paste0(states$sets[i], ": ", states$at[i]), width = 150,
                      indent = 3, exdent = 7), sep = "\n")
        }
        definitions <- definitions %>%
          group_by(effect, from, to) %>%
          summarise(n_sets = n(), sets = paste(used_in, collapse = " "),
                    .groups = "drop") %>%
          group_by(effect) %>%
          mutate(n_definitions = n()) %>%
          ungroup() %>%
          mutate(sets = if_else(n_definitions > 1, sets,
                                paste(n_sets, "sets"))) %>%
          arrange(factor(effect, levels = effect_order))
        cat("   The changes, from -> to:\n")
        for (i in seq_len(nrow(definitions))) {
          cat(strwrap(paste0(definitions$effect[i], ": ", definitions$from[i],
                             " -> ", definitions$to[i], " | in ",
                             definitions$sets[i]),
                      width = 150, indent = 3, exdent = 7), sep = "\n")
        }
      })

      section("9. Probabilities at the baseline (%)", {
        print_table(baselines %>%
                      mutate(value = shown(100 * probability, n_experts),
                             n_experts = shown(n_experts, n_experts, 0)) %>%
                      select(model, set, profile, n_experts, outcome, value) %>%
                      pivot_wider(names_from = outcome, values_from = value,
                                  values_fill = "."))
      })

      section(paste0("10. Effects on the reference outcome (pp), every set, ",
                     "one table per profile; * 95% Krinsky-Robb interval ",
                     "excludes 0"), {
        reference_effects <- tibble()
        for (model_name in model_names) {
          reference_effects <- bind_rows(reference_effects, effects %>%
            filter(model == model_name,
                   outcome == results[[model_name]]$reference))
        }
        reference_wide <- reference_effects %>%
          mutate(column = paste0(model, ":", set),
                 value = shown(100 * ame, n_experts,
                               significant = ci_low_kr > 0 | ci_high_kr < 0)) %>%
          select(profile, effect, column, value) %>%
          pivot_wider(names_from = column, values_from = value,
                      values_fill = ".") %>%
          arrange(factor(profile, levels = profile_order),
                  factor(effect, levels = effect_order))
        # Shorter column names, if they stay distinct
        short_names <- sub(":observed_", ":obs_", names(reference_wide))
        if (!any(duplicated(short_names))) {
          names(reference_wide) <- short_names
        }
        # A profile without effects (a federal state of observed_states) is
        # left out
        for (profile_name in intersect(profile_order, reference_wide$profile)) {
          cat("  ", profile_name, "\n")
          print_table(reference_wide %>%
                        filter(profile == profile_name) %>%
                        select(-profile))
        }
      })

      section(paste0("11. Effects on every outcome (pp), every set, for all ",
                     "experts where a set has them, else its first profile"), {
        for (model_name in model_names) {
          for (set_name in names(results[[model_name]]$ames)) {
            set_table <- effects %>%
              filter(model == model_name, set == set_name)
            # A set without effects (the households of m4, the states) is
            # left out
            if (nrow(set_table) == 0) {
              next
            }
            profile_name <- set_table$profile[1]
            if (ALL_EXPERTS %in% set_table$profile) {
              profile_name <- ALL_EXPERTS
            }
            cat("  ", model_name, set_name, "|", profile_name, "\n")
            print_table(set_table %>%
                          filter(profile == profile_name) %>%
                          mutate(value = shown(100 * ame, n_experts,
                                               significant = ci_low_kr > 0 |
                                                 ci_high_kr < 0),
                                 n_experts = shown(n_experts, n_experts, 0)) %>%
                          select(effect, n_experts, outcome, value) %>%
                          pivot_wider(names_from = outcome, values_from = value,
                                      values_fill = "."))
          }
        }
      })

      # A: the same set in two models next to each other in size, over the
      # effects defined alike and only if `at` is the same; a large gap there
      # is worth a look. B: the sets of one model against each other; they
      # differ in the reference state or the population, so gaps are expected.
      section(paste0("12. How close two sets of effects are, over the rows ",
                     "they share that may be shown. A: one set in two ",
                     "models; B: two sets of one model"), {
        pairs <- tibble()
        for (set_name in unique(effects$set)) {
          having <- unique(effects$model[effects$set == set_name])
          for (k in seq_len(length(having) - 1)) {
            pairs <- bind_rows(pairs, tibble(kind = "A",
                                             model_a = having[k],
                                             set_a = set_name,
                                             model_b = having[k + 1],
                                             set_b = set_name))
          }
        }
        for (model_name in model_names) {
          sets <- names(results[[model_name]]$ames)
          for (i in seq_along(sets)) {
            for (j in seq_along(sets)) {
              if (i < j) {
                pairs <- bind_rows(pairs, tibble(kind = "B",
                                                 model_a = model_name,
                                                 set_a = sets[i],
                                                 model_b = model_name,
                                                 set_b = sets[j]))
              }
            }
          }
        }
        agreement <- tibble()
        for (i in seq_len(nrow(pairs))) {
          ames_a <- results[[pairs$model_a[i]]]$ames[[pairs$set_a[i]]]
          ames_b <- results[[pairs$model_b[i]]]$ames[[pairs$set_b[i]]]
          compared <- intersect(names(ames_a$changes), names(ames_b$changes))
          if (pairs$kind[i] == "A") {
            # identical(): the same object, value for value
            if (!identical(ames_a$at, ames_b$at)) {
              cat("   ", pairs$set_a[i], "of", pairs$model_a[i], "and",
                  pairs$model_b[i], "not compared: different at\n")
              next
            }
            alike <- character(0)
            for (effect in compared) {
              if (identical(ames_a$changes[[effect]],
                            ames_b$changes[[effect]])) {
                alike <- c(alike, effect)
              }
            }
            compared <- alike
          }
          joined <- inner_join(
            effects_shown %>%
              filter(model == pairs$model_a[i], set == pairs$set_a[i],
                     effect %in% compared) %>%
              select(profile, effect, outcome, ame_a = ame),
            effects_shown %>%
              filter(model == pairs$model_b[i], set == pairs$set_b[i],
                     effect %in% compared) %>%
              select(profile, effect, outcome, ame_b = ame),
            by = c("profile", "effect", "outcome")
          ) %>%
            filter(!is.na(ame_a), !is.na(ame_b)) %>%
            mutate(difference = abs(ame_a - ame_b))
          if (nrow(joined) < 2) {
            next
          }
          # which.max(): the position of the largest value
          largest <- which.max(joined$difference)
          # A set whose effects are all equal, e.g. all 0, has no correlation
          correlation <- NA_real_
          if (sd(joined$ame_a) > 0 && sd(joined$ame_b) > 0) {
            correlation <- round(cor(joined$ame_a, joined$ame_b), 3)
          }
          both_moving <- joined %>%
            filter(abs(ame_a) > 0.01, abs(ame_b) > 0.01)
          # sign(): -1, 0 or 1
          agreement <- bind_rows(agreement, tibble(
            kind = pairs$kind[i],
            a = paste(pairs$model_a[i], pairs$set_a[i]),
            b = paste(pairs$model_b[i], pairs$set_b[i]),
            rows = nrow(joined),
            correlation = correlation,
            mean_abs_diff_pp = round(100 * mean(joined$difference), 2),
            max_abs_diff_pp = round(100 * joined$difference[largest], 2),
            largest_at = paste(joined$effect[largest], joined$outcome[largest],
                               joined$profile[largest]),
            same_sign_pct_above_1pp = round(100 * mean(
              sign(both_moving$ame_a) == sign(both_moving$ame_b)
            ))
          ))
        }
        print_table(agreement)
      })
    })
  }
}, finally = {
  sink()
  options(old_options)
})
message("Written to ", check_file)
