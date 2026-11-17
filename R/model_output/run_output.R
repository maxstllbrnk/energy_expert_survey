################################################################################
# run_output.R — the exhibits of the models: every run with its models and
#                their comparison, the comparisons across runs, the numbers
#                for the text, and a deck per folder to check them
# Inputs : every estimation folder of the output folder (collect_results())
# Outputs: <output_dir>/exhibits/<run>/ and exhibits/across_runs/: tables/,
#          figures/, maps/, exhibits.csv, exhibits.pdf; exhibits/numbers.tex,
#          exhibits/log.txt, exhibits/fingerprint.rds
# Checks : the settings name runs, models and macros that can exist; the rest in
#          collect_results() and the sections
################################################################################

# The blocks of the decks, for their page "Not shown"
RUN_DECK_BLOCKS <- tribble(
  ~letter, ~block,                                       ~reason,
  "A",     "A. The main model",                          "The main model is not in this run.",
  "B",     "B. Robustness of the main model's effects",  "The main set of the main model has no saved draws, and no second definition of an effect.",
  "C",     "C. The models compared",                     "No comparison of the settings has two models in this run.",
  "D",     "D. Every other model",                       "The run has no other model.",
  "E",     "E. Maps",                                    "No set of effects by state (observed_states) for the main model."
)
ACROSS_DECK_BLOCKS <- tribble(
  ~letter, ~block,                                             ~reason,
  "A",     "A. The arms",                                      "The main runs of both arms are needed.",
  "B",     "B. Gas and oil heating as one outcome or apart",   "Both runs of an arm, with the outcomes as one and apart, are needed."
)

#' All exhibits of the models. Loads nothing and writes nothing if no input,
#' setting or code has changed since the last run.
#'
#' @param main_runs       the main run of each arm, named by arm
#' @param separate_runs   the run with the gas and the oil heating apart, by arm
#' @param main_model      the main specification, in every run
#' @param models_compared named list: the models of each comparison within a run
#' @param full_models     the models that get every exhibit; the others the core
#' @param text_numbers    the numbers the text quotes: macro (letters only), arm,
#'                        model, set, profile, effect (NA: the probability at the
#'                        baseline), outcome, quantity ("ame", "se", "ci_low",
#'                        "ci_high", "probability")
#' @param output_dir      the output folder, OUTPUT_DIR
#' @param force           TRUE: write everything again
#' @return the list of exhibits of every folder, invisibly
run_output <- function(main_runs, separate_runs, main_model, models_compared,
                       full_models, text_numbers, output_dir = OUTPUT_DIR,
                       force = FALSE) {
  start_time <- Sys.time()
  exhibits_dir <- file.path(output_dir, "exhibits")

  # 1. The settings --------------------------------------------------------------
  if (!all(names(c(main_runs, separate_runs)) %in% names(ARM_LABELS))) {
    stop("main_runs and separate_runs must be named by arm: ",
         paste(names(ARM_LABELS), collapse = ", "))
  }
  # grepl(): whether each name matches the pattern, here letters only
  if (!all(grepl("^[A-Za-z]+$", text_numbers$macro))) {
    stop("The name of a LaTeX macro has letters only: ",
         paste(text_numbers$macro[!grepl("^[A-Za-z]+$", text_numbers$macro)],
               collapse = ", "))
  }

  # 2. Nothing changed? --------------------------------------------------------
  # What the exhibits depend on: every results.rds, comparison and run_info.rds,
  # the code that writes them, the map and the settings. tools::md5sum(): a
  # fingerprint of a file; rlang::hash(): of any object.
  input_files <- list.files(
    output_dir, pattern = "^(results\\.rds|model_comparison\\.(rds|xlsx)|run_info\\.rds)$",
    recursive = TRUE, full.names = TRUE
  )
  code_files <- c(
    list.files(file.path("R", "model_output"), full.names = TRUE),
    file.path("R", "functions", c("compute_ames.R", "exhibits.R", "figures.R",
                                  "labels.R", "tables.R")),
    file.path("R", "summary_statistics", c("presentation.R", "econslides.sty")),
    file.path("R", "setup.R"), "config.R", BUNDESLAND_SHP
  )
  fingerprint <- rlang::hash(list(
    input_files, unname(tools::md5sum(c(input_files, code_files))), main_runs,
    separate_runs, main_model, models_compared, full_models, text_numbers
  ))
  fingerprint_file <- file.path(exhibits_dir, "fingerprint.rds")
  if (!force && file.exists(fingerprint_file) &&
      identical(readRDS(fingerprint_file), fingerprint)) {
    message("The exhibits of the models are current, nothing changed: ",
            exhibits_dir)
    return(invisible(NULL))
  }
  # No fingerprint until the run has ended: a run that stops leaves nothing that
  # counts as current
  dir.create(exhibits_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(fingerprint_file)
  log_file <- file.path(exhibits_dir, "log.txt")
  writeLines(paste0("run_output(), ", format(Sys.time(), "%Y-%m-%d %H:%M"),
                    ", output: ", output_dir), log_file)

  # Every message and warning also goes to log.txt, as the exhibits are meant to
  # be shared (project profile). withCallingHandlers(): runs the function named
  # for a message or warning, and lets the code go on.
  written <- withCallingHandlers({

    # 3. The results -----------------------------------------------------------
    collected <- collect_results(output_dir, main_model)
    # The folder of a run that is gone goes too, so that no old deck looks current
    for (folder in list.dirs(exhibits_dir, recursive = FALSE, full.names = FALSE)) {
      if (!folder %in% c(collected$runs$run, "across_runs")) {
        unlink(file.path(exhibits_dir, folder), recursive = TRUE)
      }
    }

    # 4. Each run ----------------------------------------------------------------
    # decks: the path of each deck, NA for one that did not compile
    all_exhibits <- tibble()
    decks <- character(0)
    for (run_name in collected$runs$run) {
      run_row <- collected$runs[collected$runs$run == run_name, ]
      run_dir <- file.path(exhibits_dir, run_name)
      dirs <- list(tables = file.path(run_dir, "tables"),
                   figures = file.path(run_dir, "figures"),
                   maps = file.path(run_dir, "maps"))
      # unlink(): deletes, so that no file of an earlier run stays among the new
      for (folder in dirs) {
        unlink(folder, recursive = TRUE)
        dir.create(folder, recursive = TRUE, showWarnings = FALSE)
      }
      problems <- collected$problems$what[collected$problems$run == run_name]
      run_models <- collected$models %>%
        filter(run == run_name) %>%
        arrange(n_parameters) %>%
        pull(model)
      if (!main_model %in% run_models) {
        problems <- c(problems, paste0("The main model ", main_model,
                                       " is not in this run."))
      }
      is_main_run <- run_name %in% main_runs
      exhibits <- tibble()
      # The main model first, then every other model by its size. intersect(),
      # setdiff(): the elements in both, in the first only
      for (model_name in c(intersect(main_model, run_models),
                           setdiff(run_models, main_model))) {
        placement <- "appendix"
        if (is_main_run && model_name == main_model) {
          placement <- "main"
        }
        result <- section_models(collected, run_name, model_name,
                                 full = model_name %in% full_models,
                                 placement = placement, dirs = dirs)
        exhibits <- bind_rows(exhibits, result$exhibits)
        problems <- c(problems, result$problems)
      }
      result <- section_comparison(collected, run_name, models_compared,
                                   main_model, dirs)
      exhibits <- bind_rows(exhibits, result$exhibits)
      problems <- c(problems, result$problems)
      result <- section_maps(collected, run_name, main_model,
                             if (is_main_run) "main" else "appendix", dirs)
      exhibits <- bind_rows(exhibits, result$exhibits)
      problems <- c(problems, result$problems)

      # The deck in the order of the blocks, A to E; arrange() keeps the order of
      # the exhibits within a block
      if (nrow(exhibits) > 0) {
        exhibits <- exhibits %>%
          arrange(block)
      }
      write_csv(exhibits, file.path(run_dir, "exhibits.csv"))
      decks <- c(decks, build_presentation(
        exhibits, run_dir, run_row$sample_label,
        title = paste0("The models: ", run_row$run_label),
        deck_file = "exhibits", deck_blocks = RUN_DECK_BLOCKS,
        author = paste0("Folder ", run_name, ", ", length(run_models),
                        " models", if (QUICK_RUN) ", QUICK RUN" else ""),
        not_shown = problems
      ))
      all_exhibits <- bind_rows(all_exhibits, exhibits %>%
                                  mutate(folder = run_name))
    }

    # 5. Across runs -----------------------------------------------------------
    across_dir <- file.path(exhibits_dir, "across_runs")
    dirs <- list(tables = file.path(across_dir, "tables"),
                 figures = file.path(across_dir, "figures"),
                 maps = file.path(across_dir, "maps"))
    for (folder in dirs) {
      unlink(folder, recursive = TRUE)
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    }
    result <- section_runs(collected, main_runs, separate_runs, main_model,
                           models_compared, dirs)
    if (nrow(result$exhibits) > 0) {
      result$exhibits <- result$exhibits %>%
        arrange(block)
    }
    write_csv(result$exhibits, file.path(across_dir, "exhibits.csv"))
    decks <- c(decks, build_presentation(
      result$exhibits, across_dir, "The runs of the estimation scripts compared",
      title = "The models across runs", deck_file = "exhibits",
      deck_blocks = ACROSS_DECK_BLOCKS,
      author = paste0(nrow(collected$runs), " runs",
                      if (QUICK_RUN) ", QUICK RUN" else ""),
      not_shown = c(collected$problems$what[collected$problems$run == ""],
                    result$problems)
    ))
    all_exhibits <- bind_rows(all_exhibits, result$exhibits %>%
                                mutate(folder = "across_runs"))

    # 6. The numbers of the text -------------------------------------------------
    # LaTeX macros (write_macros(), tables.R): the size of the sample and the
    # draws of each run of the settings, then the numbers the settings list. Only
    # the runs of the settings, so that a variant run cannot take a main run's
    # name. A number that cannot be shown prints as a bold ?? in the paper, and the
    # log names it. str_to_title(): the first letter large, "fw" -> "Fw".
    numbers <- character(0)
    named_runs <- c(main_runs, separate_runs)
    run_macros <- c(str_to_title(names(main_runs)),
                    paste0(str_to_title(names(separate_runs)), "Separate"))
    for (k in seq_along(named_runs)) {
      run_name <- named_runs[[k]]
      run_macro <- run_macros[k]
      model_row <- collected$models %>%
        filter(run == run_name, model == main_model)
      if (nrow(model_row) == 1) {
        numbers[paste0("nExperts", run_macro)] <- fmt_int(model_row$n_experts)
        numbers[paste0("nChoices", run_macro)] <- fmt_int(model_row$n_choices)
        numbers[paste0("nDrawsModel", run_macro)] <- fmt_int(model_row$n_draws_model)
        numbers[paste0("nDrawsKr", run_macro)] <- fmt_int(model_row$n_draws_kr)
      }
    }
    for (k in seq_len(nrow(text_numbers))) {
      wanted <- text_numbers[k, ]
      run_name <- unname(main_runs[wanted$arm])
      value <- NA_real_
      if (wanted$quantity == "probability") {
        cell <- collected$baselines %>%
          filter(run == run_name, model == wanted$model, set == wanted$set,
                 profile == wanted$profile, outcome == wanted$outcome,
                 status == "shown")
        if (nrow(cell) == 1) {
          value <- 100 * cell$probability
        }
      } else {
        cell <- collected$effects %>%
          filter(run == run_name, model == wanted$model, set == wanted$set,
                 profile == wanted$profile, effect == wanted$effect,
                 outcome == wanted$outcome, status == "shown")
        if (nrow(cell) == 1) {
          value <- 100 * case_when(wanted$quantity == "ame" ~ cell$ame,
                                   wanted$quantity == "se" ~ cell$se,
                                   wanted$quantity == "ci_low" ~ cell$ci_low_kr,
                                   wanted$quantity == "ci_high" ~ cell$ci_high_kr)
        }
      }
      numbers[wanted$macro] <- fmt_num(value, 1)
      if (is.na(value)) {
        numbers[wanted$macro] <- "\\textbf{??}"
        message("The number ", wanted$macro, " does not exist or may not be ",
                "shown: it prints as ?? in the text")
      }
    }
    write_macros(numbers, file.path(exhibits_dir, "numbers.tex"))
    list(exhibits = all_exhibits, decks = decks)
  },
  # conditionMessage(): the text of a message or warning; cat(file =, append =
  # TRUE): adds it to the end of the log
  message = function(condition) {
    cat("Message:", conditionMessage(condition), file = log_file, append = TRUE)
  },
  warning = function(condition) {
    cat("Warning:", conditionMessage(condition), "\n", file = log_file,
        append = TRUE)
  })
  all_exhibits <- written$exhibits

  # 7. Done ----------------------------------------------------------------------
  # The fingerprint only if every deck compiled: one that did not (pdflatex
  # missing, or the old deck open in a viewer) leaves the run to be written again.
  # anyNA(): whether any is missing.
  if (length(written$decks) > 0 && !anyNA(written$decks)) {
    saveRDS(fingerprint, fingerprint_file)
  }
  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  message("Exhibits of the models: ", nrow(all_exhibits), " in ", exhibits_dir,
          " (", round(minutes, 1), " minutes); messages and warnings in ",
          log_file)
  invisible(all_exhibits)
}
