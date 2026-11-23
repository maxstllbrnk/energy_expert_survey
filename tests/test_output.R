################################################################################
# test_output.R — the output script of the models (R/run_output.R) on the
#                 synthetic results of the estimation scripts
# Inputs : OUTPUT_DIR/<estimation folder>/ of a synthetic run of the estimation
#          scripts: run_info.rds, <model>/results.rds, model_comparison/
# Outputs: in OUTPUT_DIR/test_output/, removed at the start of a run
# Checks : every exhibit of the lists exists, and every file is listed; no raw
#          name of a term or effect in a table; a second run with nothing changed
#          writes nothing; the minimum cell for effects and for the coefficients
#          of rare terms and their effects; a missing label stops; a missing model
#          is left out and named; an own constant's standard error and a
#          difference between groups against a second computation
################################################################################
# From the project root, in a session with CLAUDECODE=1, once the estimation
# scripts ran on the synthetic data (at least the two *_grouped.R):
#   Rscript tests/test_output.R

source(file.path("R", "setup.R"))
if (!USE_SYNTHETIC_DATA) {
  stop("tests/test_output.R is for synthetic data")
}
test_dir <- file.path(OUTPUT_DIR, "test_output")
unlink(test_dir, recursive = TRUE)

# 1. Helpers -------------------------------------------------------------------
n_failed <- 0
report <- function(what, is_ok, detail = "") {
  cat(sprintf("%-72s %s %s\n", what, if (isTRUE(is_ok)) "PASS" else "*** FAIL ***",
              detail))
  if (!isTRUE(is_ok)) {
    # <<-: changes n_failed outside this function
    n_failed <<- n_failed + 1
  }
}

#' The message of the error that code stops with, or "no error"
error_of <- function(code) {
  tryCatch({
    code
    "no error"
  }, error = function(e) conditionMessage(e))
}

#' A copy of the runs, the kept models only, into a folder of the test
#'
#' @param name        the folder of the copy
#' @param model_names the models to copy
#' @return the folder
copy_runs <- function(name, model_names) {
  copy_dir <- file.path(test_dir, name)
  for (run_name in run_names) {
    run_dir <- file.path(OUTPUT_DIR, run_name)
    dir.create(file.path(copy_dir, run_name, "model_comparison"),
               recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(run_dir, "run_info.rds"), file.path(copy_dir, run_name))
    file.copy(list.files(file.path(run_dir, "model_comparison"), full.names = TRUE,
                         pattern = "^model_comparison\\.(rds|xlsx)$"),
              file.path(copy_dir, run_name, "model_comparison"))
    for (model_name in model_names) {
      results_file <- file.path(run_dir, model_name, "results.rds")
      if (file.exists(results_file)) {
        dir.create(file.path(copy_dir, run_name, model_name), showWarnings = FALSE)
        file.copy(results_file, file.path(copy_dir, run_name, model_name))
      }
    }
  }
  copy_dir
}

# 2. The runs to test on -------------------------------------------------------
# Every estimation folder with run_info.rds; the main model and three others,
# so that the test takes minutes
run_names <- character(0)
for (folder in list.dirs(OUTPUT_DIR, recursive = FALSE, full.names = FALSE)) {
  if (file.exists(file.path(OUTPUT_DIR, folder, "run_info.rds")) &&
      file.exists(file.path(OUTPUT_DIR, folder, "m4pe", "results.rds"))) {
    run_names <- c(run_names, folder)
  }
}
if (!all(c("district_heating_grouped", "no_district_heating_grouped") %in%
         run_names)) {
  stop("Run R/estimation/district_heating_grouped.R and ",
       "no_district_heating_grouped.R on the synthetic data first")
}
kept_models <- c("m4", "m4p", "m4pe", "m6pe")
main_runs <- c(fw = "district_heating_grouped", nofw = "no_district_heating_grouped")
separate_runs <- c(fw = "district_heating", nofw = "no_district_heating")
models_compared <- list(specifications = kept_models)
text_numbers <- tribble(
  ~macro,          ~arm, ~model, ~set,                 ~profile,      ~effect,              ~outcome,     ~quantity,
  "hpBaselineFw",  "fw", "m4pe", "observed_reference", "All experts", NA,                   "Wärmepumpe", "probability",
  "hpUrgentFw",    "fw", "m4pe", "observed_reference", "All experts", "replacement_urgent", "Wärmepumpe", "ame",
  "notThereFw",    "fw", "m4pe", "observed_reference", "All experts", "no_such_effect",     "Wärmepumpe", "ame"
)
cat("Runs:", paste(run_names, collapse = ", "), "\n")

# 3. The whole output on a copy ------------------------------------------------
base_dir <- copy_runs("base", kept_models)
seconds <- system.time(
  run_output(main_runs, separate_runs, "m4pe", models_compared, "m4pe",
             text_numbers, output_dir = base_dir, force = TRUE)
)[["elapsed"]]
cat("The output:", round(seconds), "seconds\n")
exhibits_dir <- file.path(base_dir, "exhibits")
folders <- c(run_names, "across_runs")
for (folder in folders) {
  listed <- read_csv(file.path(exhibits_dir, folder, "exhibits.csv"),
                     show_col_types = FALSE)
  on_disk <- list.files(file.path(exhibits_dir, folder),
                        pattern = "\\.(tex|pdf)$", recursive = TRUE)
  on_disk <- on_disk[on_disk != "exhibits.tex" & on_disk != "exhibits.pdf"]
  report(paste0(folder, ": every listed exhibit exists"),
         all(file.exists(file.path(exhibits_dir, folder, listed$file))))
  report(paste0(folder, ": every file is listed"),
         setequal(on_disk, listed$file),
         paste(setdiff(on_disk, listed$file), collapse = " "))
}
report("the main run has the main model's tables, figures and comparison",
       all(file.exists(file.path(
         exhibits_dir, "district_heating_grouped",
         c("tables/tab_coefficients_m4pe.tex",
           "tables/tab_effects_m4pe_all_experts.tex",
           "figures/fig_effects_m4pe_household_all.pdf",
           "tables/tab_models_specifications.tex", "tables/tab_diagnostics.tex")
       ))))
report("across runs: the arms side by side",
       file.exists(file.path(exhibits_dir, "across_runs", "tables",
                             "tab_effects_arms_heat_pump.tex")))

# No raw name of a term or an effect in a table: latex_text() of a name is how it
# would stand there
table_files <- list.files(exhibits_dir, pattern = "\\.tex$", recursive = TRUE,
                          full.names = TRUE)
table_files <- table_files[basename(table_files) != "exhibits.tex"]
table_text <- character(0)
for (table_file in table_files) {
  table_text <- c(table_text, readLines(table_file, encoding = "UTF-8"))
}
raw_names <- c(TERM_LABELS$term[grepl("_", TERM_LABELS$term)],
               EFFECT_LABELS$effect[grepl("_", EFFECT_LABELS$effect)])
found <- character(0)
for (raw_name in raw_names) {
  if (any(grepl(latex_text(raw_name), table_text, fixed = TRUE))) {
    found <- c(found, raw_name)
  }
}
report("no raw name of a term or effect in a table", length(found) == 0,
       paste(found, collapse = " "))
numbers <- readLines(file.path(exhibits_dir, "numbers.tex"))
report("numbers.tex: the macros, and ?? for a number that does not exist",
       any(grepl("\\\\newcommand\\{\\\\hpUrgentFw\\}\\{[^?]", numbers)) &&
         any(grepl("\\\\newcommand\\{\\\\notThereFw\\}\\{\\\\textbf\\{\\?\\?\\}\\}",
                   numbers)) &&
         any(grepl("nExpertsFw", numbers)))

# 4. Nothing changed: nothing is written ---------------------------------------
listed_file <- file.path(exhibits_dir, "district_heating_grouped", "exhibits.csv")
written_before <- file.mtime(listed_file)
run_output(main_runs, separate_runs, "m4pe", models_compared, "m4pe",
           text_numbers, output_dir = base_dir)
report("a second run with nothing changed writes nothing",
       identical(file.mtime(listed_file), written_before))

# 5. The minimum cell ----------------------------------------------------------
# (a) An effect over too few experts: chimney sweeps, one change of the main set
stressed_dir <- copy_runs("few_experts", "m4pe")
results_file <- file.path(stressed_dir, "district_heating_grouped", "m4pe",
                          "results.rds")
results <- readRDS(results_file)
table <- results$ames[[MAIN_AMES]]$table
is_stressed <- table$profile == "Chimney sweeps" & table$effect == "age_60"
table$n_experts[is_stressed] <- 5
results$ames[[MAIN_AMES]]$table <- table
saveRDS(results, results_file)
collected <- collect_results(stressed_dir, "m4pe")
stressed <- collected$effects %>%
  filter(run == "district_heating_grouped", set == MAIN_AMES,
         profile == "Chimney sweeps", effect == "age_60")
others <- collected$effects %>%
  filter(run == "district_heating_grouped", set == MAIN_AMES,
         profile == "Energy advisers", effect == "age_60")
report("fewer than 10 experts: too few, and no number left",
       all(stressed$status == "too few") && all(is.na(stressed$ame)) &&
         all(is.na(stressed$se)) && all(others$status == "shown"))

# (b) A rare term: one coefficient that few experts recommend. Every effect that
# moves its term is hidden, in every set; without the moved terms, every effect
# of the model
results <- readRDS(file.path(OUTPUT_DIR, "district_heating_grouped", "m4pe",
                             "results.rds"))
rare_dir <- copy_runs("rare_term", "m4pe")
results_file <- file.path(rare_dir, "district_heating_grouped", "m4pe",
                          "results.rds")
expert_terms <- coefficient_parts(results)$terms %>%
  filter(column %in% TERM_LABELS$term[TERM_LABELS$panel == "The expert"],
         !is.na(technology))
rare_parameter <- expert_terms$parameter[1]
rare_column <- expert_terms$column[1]
if (is.null(results$term_support)) {
  results$term_support <- coefficient_parts(results)$terms %>%
    mutate(experts_with_term = 1000, experts_choosing = 1000)
}
results$term_support$experts_choosing[results$term_support$parameter ==
                                        rare_parameter] <- 3
saveRDS(results, results_file)
collected <- collect_results(rare_dir, "m4pe")
rare_coefficient <- collected$coefficients %>%
  filter(run == "district_heating_grouped", parameter == rare_parameter)
report(paste0("a rare term (", rare_parameter, "): its coefficient is hidden"),
       rare_coefficient$status == "too few" && is.na(rare_coefficient$estimate))
rare_effects <- character(0)
has_moved_terms <- !is.null(results$ames[[MAIN_AMES]]$moved_terms)
if (has_moved_terms) {
  moved_terms <- results$ames[[MAIN_AMES]]$moved_terms
  for (effect in names(moved_terms)) {
    if (rare_column %in% moved_terms[[effect]]) {
      rare_effects <- c(rare_effects, effect)
    }
  }
} else {
  rare_effects <- names(results$ames[[MAIN_AMES]]$changes)
}
main_cells <- collected$effects %>%
  filter(run == "district_heating_grouped", set == MAIN_AMES)
report(paste0("a rare term: every effect that moves it is hidden",
              if (has_moved_terms) "" else " (no moved terms: all)"),
       length(rare_effects) > 0 &&
         all(main_cells$status[main_cells$effect %in% rare_effects] %in%
               c("too few", "not applicable")) &&
         (!has_moved_terms ||
            any(main_cells$status[!main_cells$effect %in% rare_effects] == "shown")))
report("a rare term: named on the page Not shown",
       any(grepl(rare_column, collected$problems$what, fixed = TRUE)) ||
         !has_moved_terms)

# (c) A missing label stops, and names what to add
unlabelled_dir <- copy_runs("unlabelled", "m4pe")
results_file <- file.path(unlabelled_dir, "district_heating_grouped", "m4pe",
                          "results.rds")
results <- readRDS(results_file)
set_ames <- results$ames[[MAIN_AMES]]
names(set_ames$changes)[names(set_ames$changes) == "age_60"] <- "age_sixty"
set_ames$table$effect[set_ames$table$effect == "age_60"] <- "age_sixty"
results$ames[[MAIN_AMES]] <- set_ames
saveRDS(results, results_file)
message_text <- error_of(collect_results(unlabelled_dir, "m4pe"))
report("an effect without a label stops, and the message names it",
       grepl("age_sixty", message_text, fixed = TRUE))

# (d) A model the run does not have: left out of its comparison, and named
missing_dir <- copy_runs("missing_model", c("m4", "m4p", "m4pe"))
collected <- collect_results(missing_dir, "m4pe")
dirs <- list(tables = file.path(missing_dir, "tables"),
             figures = file.path(missing_dir, "figures"),
             maps = file.path(missing_dir, "maps"))
for (folder in dirs) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}
result <- section_comparison(collected, "district_heating_grouped",
                             models_compared, "m4pe", dirs)
report("a missing model is left out of its comparison and named",
       any(grepl("m6pe", result$problems, fixed = TRUE)) &&
         file.exists(file.path(dirs$tables, "tab_models_specifications.tex")))

# (e) A set by state becomes maps, a state below the minimum cell stays grey. The
# set is made here from the main set's baseline, as observed_states would be.
states_dir <- copy_runs("states", "m4pe")
results_file <- file.path(states_dir, "district_heating_grouped", "m4pe",
                          "results.rds")
results <- readRDS(results_file)
state_set <- results$ames[[MAIN_AMES]]
all_baseline <- state_set$baseline %>%
  filter(profile == ALL_EXPERTS)
state_rows <- list()
for (state in names(STATE_LABELS)) {
  state_rows[[state]] <- all_baseline %>%
    mutate(profile = state,
           n_experts = if_else(state == "Bremen", 5, 50),
           n_units = n_experts)
}
state_set$baseline <- bind_rows(state_rows)
state_set$table <- state_set$table[0, ]
state_set$changes <- list()
state_set$moved_terms <- list()
state_set$draws <- NULL
results$ames$observed_states <- state_set
saveRDS(results, results_file)
collected <- collect_results(states_dir, "m4pe")
dirs <- list(tables = file.path(states_dir, "tables"),
             figures = file.path(states_dir, "figures"),
             maps = file.path(states_dir, "maps"))
for (folder in dirs) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}
result <- section_maps(collected, "district_heating_grouped", "m4pe", "main", dirs)
bremen <- collected$baselines %>%
  filter(run == "district_heating_grouped", set == "observed_states",
         profile == "Bremen")
report("a set by state: a map per outcome, a state below the minimum cell is grey",
       nrow(result$exhibits) == length(results$outcomes) &&
         all(file.exists(file.path(states_dir, result$exhibits$file))) &&
         all(bremen$status == "too few") && all(is.na(bremen$probability)))

# 6. Two numbers against a second computation ----------------------------------
collected <- collect_results(base_dir, "m4pe")
results <- readRDS(file.path(base_dir, "district_heating_grouped", "m4pe",
                             "results.rds"))
technology_name <- setdiff(results$outcomes, results$reference)[1]
parameters <- paste0(c("(Intercept)", "is_hvac"), ":", technology_name)
by_hand <- sqrt(results$vcov[parameters[1], parameters[1]] +
                  results$vcov[parameters[2], parameters[2]] +
                  2 * results$vcov[parameters[1], parameters[2]])
own_constant <- collected$own_constants %>%
  filter(run == "district_heating_grouped", model == "m4pe",
         group == "HVAC installers", technology == technology_name)
report("an own constant: the constant plus the shift, its SE from the covariance",
       abs(own_constant$se - by_hand) < 1e-10 &&
         abs(own_constant$estimate - sum(results$coefficients[parameters])) < 1e-10)

# A difference between the groups, from the effects of the collected tables: the
# point is the difference of the two effects, as the table shows it
main_effects <- collected$effects %>%
  filter(run == "district_heating_grouped", model == "m4pe", set == MAIN_AMES,
         effect == "replacement_urgent", outcome == results$reference)
difference <- 100 * (main_effects$ame[main_effects$profile == "HVAC installers"] -
                       main_effects$ame[main_effects$profile == "Energy advisers"])
difference_table <- readLines(file.path(
  exhibits_dir, "district_heating_grouped", "tables",
  "tab_group_differences_m4pe_hvac_installers_vs_energy_advisers.tex"
), encoding = "UTF-8")
urgent_line <- difference_table[grepl("Broken, urgent replacement",
                                      difference_table, fixed = TRUE)]
report("a difference between groups: the difference of the two effects",
       length(urgent_line) == 1 &&
         grepl(fmt_num(difference, 1), urgent_line, fixed = TRUE))

cat("\n", if (n_failed == 0) "All checks passed" else
  paste(n_failed, "checks failed"), "\n")
if (n_failed > 0) {
  quit(status = 1)
}
