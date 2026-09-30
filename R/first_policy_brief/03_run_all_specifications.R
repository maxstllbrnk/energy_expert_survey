################################################################################
# 03_run_all_specifications.R — runs 03_rc_logit_with_ame.R for both arms
#                               (with and without Fernwärme) and for two
#                               reference outcomes
################################################################################
# Each run sources the main script in its own environment (run_env). The
# settings of the run are passed in RUN_SETTINGS, and the objects of one run
# (data, models, tables) do not carry over into the next. Each run writes its
# output into its own subfolder of FIRST_POLICY_BRIEF_DIR, e.g.
# rc_logit_fw_ref_waermepumpe. If a run stops with an error, the error is
# recorded and the loop continues with the next run.
#
# Runtime: the sum of the four runs; the correlated mixed logit and its
# numerical Hessian take most of it.

source(file.path("R", "prepare_analysis_data.R"))   # load_analysis_data()

# The main script, relative to the working directory (the project folder)
MAIN_SCRIPT <- "03_rc_logit_with_ame.R"

# The two arms as coded in vig_arm, read from the data
ARMS <- sort(unique(load_analysis_data(completers_only = TRUE)$vignettes$vig_arm))
print(ARMS)
stopifnot(length(ARMS) == 2, "fw" %in% ARMS)

REFERENCES <- c("Keine Empfehlung", "Wärmepumpe")

# One row per run: every arm with every reference outcome
runs <- expand.grid(arm = ARMS, reference = REFERENCES, stringsAsFactors = FALSE)
runs$status <- NA_character_
runs$minutes <- NA_real_

for (r in seq_len(nrow(runs))) {
  message("\n==== Run ", r, " of ", nrow(runs), ": arm ", runs$arm[r],
          ", reference outcome ", runs$reference[r], " ====")
  start_time <- Sys.time()
  
  # Fresh environment for this run; its parent is the global environment, so
  # the packages and load_analysis_data() are available in it
  run_env <- new.env(parent = globalenv())
  run_env$RUN_SETTINGS <- list(arm = runs$arm[r], reference = runs$reference[r])
  
  runs$status[r] <- tryCatch({
    source(MAIN_SCRIPT, local = run_env, encoding = "UTF-8")
    "done"
  }, error = function(e) {
    paste("error:", conditionMessage(e))
  })
  
  runs$minutes[r] <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)
  message("Run ", r, ": ", runs$status[r], " (", runs$minutes[r], " minutes)")
}

print(runs)