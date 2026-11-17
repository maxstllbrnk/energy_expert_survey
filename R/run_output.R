################################################################################
# run_output.R — the exhibits of the models for the paper: tables, figures and
#                maps of every model, the models of a run compared, and the runs
#                compared, each folder with a deck to check them
# Inputs : OUTPUT_DIR/<estimation folder>/: run_info.rds, <model>/results.rds,
#          model_comparison/ (aggregates only; no model.rds is read)
# Outputs: OUTPUT_DIR/exhibits/<estimation folder>/ and exhibits/across_runs/:
#          tables/, figures/, maps/, exhibits.csv, exhibits.pdf;
#          exhibits/numbers.tex, exhibits/log.txt
# Checks : every term, effect, set, model and outcome has a label; the minimum
#          cell, also for the coefficients of terms that few experts hold; the
#          models of a comparison share their sample; effects side by side only
#          where they are defined alike
################################################################################
# Run after the estimation scripts. It finds every estimation folder, model, set
# of effects, profile and outcome itself, so a new model needs no change here:
# it gets its own exhibits, and enters a comparison once its name is in
# models_compared. A new term or effect needs its label in R/functions/labels.R;
# the script stops and lists every one missing. What is not shown, and why, is on
# the last page of each deck. docs/results_contract.md, "The output script".

source(file.path("R", "setup.R"))

# 1. Settings ------------------------------------------------------------------
# The main run of each arm (Max, 2026-11-16), and the runs with the gas and the
# oil heating apart, compared with them
main_runs <- c(fw = "district_heating_grouped", nofw = "no_district_heating_grouped")
separate_runs <- c(fw = "district_heating", nofw = "no_district_heating")
# The main specification (Max, 2026-10-29), until the model comparison of the full
# run has been read (Max, 2026-11-16): every exhibit in every run
main_model <- "m4pe"
full_models <- main_model
# The comparisons within a run, each a table of models and their effects side by
# side; a model that a run does not have is left out there ((6pe) in arm nofw)
models_compared <- list(
  specifications = c("m4", "m4p", "m4pe", "m6pe"),
  steps = c("m4pe", paste0("m4pe_", 1:8))
)
# The numbers the text quotes, as LaTeX macros in exhibits/numbers.tex: \macro
# prints the number. quantity: "ame", "se", "ci_low", "ci_high" (percentage
# points) or "probability" (percent, with effect NA). Letters only in a macro.
text_numbers <- tribble(
  ~macro,                ~arm,   ~model, ~set,                 ~profile,      ~effect,              ~outcome,     ~quantity,
  "hpBaselineFw",        "fw",   "m4pe", "observed_reference", "All experts", NA,                   "Wärmepumpe", "probability",
  "hpBaselineNofw",      "nofw", "m4pe", "observed_reference", "All experts", NA,                   "Wärmepumpe", "probability",
  "hpUrgentFw",          "fw",   "m4pe", "observed_reference", "All experts", "replacement_urgent", "Wärmepumpe", "ame",
  "hpUrgentNofw",        "nofw", "m4pe", "observed_reference", "All experts", "replacement_urgent", "Wärmepumpe", "ame"
)

# 2. The exhibits --------------------------------------------------------------
run_output(main_runs, separate_runs, main_model, models_compared, full_models,
           text_numbers)
