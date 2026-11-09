################################################################################
# 02_output.R — tables, figures, tests and an Excel workbook from the saved
#               results of the mixed logit models (01_run_specifications.R)
# Inputs : RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/<model>.rds,
#          and <model>_term_effects.rds where 01_run_term_effects.R made it;
#          aggregates only (the fitted models are not read)
# Outputs: in RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/
#            tables/   tab_rcl_*.tex, complete LaTeX table floats, and
#                      numbers_rcl.tex, sample sizes and draws for the text
#            figures/  fig_rcl_*.pdf
#            excel/    rc_logit_results.xlsx
#          *_slide.pdf, *_slide.tex: versions for the main slides, narrower
#          or shorter, with room for notes beside or below them
# Checks : all results from one arm, reference outcome, reference vignette
#          and sample; every value has a label; the restrictions of each LR
#          test account for the difference in parameters
################################################################################
# The outputs follow the story of a talk:
#   section 4, the models compared   which model: the models and their fit,
#                                    LR tests between nested models; do the
#                                    results depend on it: the probabilities
#                                    at the reference vignette, the attribute
#                                    effects and the AMEs across models; the
#                                    price coefficients; the variants of (4p)
#                                    with their fit and LR tests; versions
#                                    for the main slides across models
#   section 5, each model            the constants (mean and standard
#                                    deviation by group), the attribute
#                                    effects (the regression table), the
#                                    probabilities at the reference vignette,
#                                    the AMEs, the correlations of the
#                                    constants; in the variants of (4p) that
#                                    have them, the price effect by heat
#                                    demand (with a Wald test), the
#                                    interactions of the attributes and the
#                                    expert characteristics; the effects of
#                                    all terms on the probabilities
# A quantity is always shown the same way (top of 00_functions_output.R): a
# row is a group's own value, never a difference to the reference group; the
# groups come in the same order, with the same colour and symbol; tables give
# standard errors in parentheses, figures 95% Krinsky-Robb percentile
# intervals. Everything is sized for the 16:9 slides of the economics-latex
# layout and in English; the tables need booktabs and threeparttable.

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

# config.R: RC_LOGIT_DIR, packages
source(file.path("R", "prepare_analysis_data.R"))
# run_folder(), file_name(), cholesky_factor()
source(file.path("R", "analysis", "00_functions_estimation.R"))
source(file.path("R", "analysis", "00_functions_output.R"))

# 1. Settings ------------------------------------------------------------------

# The run of 01_run_specifications.R to report on
SELECTED_FW_ARM <- "fw"
REFERENCE_OUTCOME <- "Wärmepumpe"

# The models, in the order of the tables, with their number (as in README.md)
# and a short description for legends and subtitles, in the words of the
# columns of the table of the models (4.1): what differs by occupational
# group, and the prices. Models without results are skipped with a message.
MODELS <- tribble(
  ~model, ~number, ~description,
  "m2",   "(2)",   "No groups",
  "m4",   "(4)",   "Group means",
  "m4i",  "(4i)",  "Group means, group × attributes",
  "m6i",  "(6i)",  "Group means and covariances, group × attributes",
  "m4p",  "(4p)",  "Group means, prices",
  "m6p",  "(6p)",  "Group means and covariances, prices",
  "m4ip", "(4ip)", "Group means, group × attributes, prices",
  "m4pa", "(4pa)", "Group means, prices, attribute × attribute",
  "m4pd", "(4pd)", "Group means, prices × heat demand",
  "m4pl", "(4pl)", "Group means, log prices",
  "m4pe", "(4pe)", "Group means, prices, expert characteristics",
  "m6pe", "(6pe)", "Group means and covariances, prices, expert characteristics"
)
# The main specification (Max, 2026-10-29); (6pe) is its robustness check
MAIN_MODEL <- "m4pe"

# The variants of (4p), each with one change, in a few words: they have a
# table of their own with their LR tests against (4p) (4.6), outside the
# table of the models and the table of the LR tests. The main specification
# is in both.
VARIANT_CHANGES <- c(
  m4pa = "+ attribute × attribute",
  m4pd = "+ price × heat demand",
  m4pl = "log prices",
  m4pe = "+ expert characteristics"
)

# Figures across models: the main specification and its robustness check
# first, then (4p) without the expert characteristics and (4) without prices.
# At most four models: the ZEW palette has four colours.
COMPARISON_MODELS <- c("m4pe", "m6pe", "m4p", "m4")

# Tables of the attribute effects across models, one per technology: the
# models whose attribute effects are the same for all groups (those with
# interactions have a table per group in section 5), and not (4pa): with
# interactions of the attributes, its attribute effects hold at the reference
# levels of the attributes they interact with (checked in 4.3)
COEFFICIENT_COMPARISON_MODELS <- c("m2", "m4", "m4p", "m4pe", "m6pe")

# Likelihood-ratio tests between nested models, classical and robust
# (lr_tests()). `restricted` is a special case of `unrestricted`;
# `restrictions` says how (types in lr_restrictions()). The tests of one
# question form a panel of the table, in this order.
LR_TESTS <- tribble(
  ~question,                                   ~restricted, ~unrestricted, ~restrictions,
  "Do the mean constants differ by group?",     "m2",        "m4",          "regressors",
  "Do the covariances differ by group?",        "m4i",       "m6i",         "group_covariances",
  "Do the covariances differ by group?",        "m4p",       "m6p",         "group_covariances",
  "Do the covariances differ by group?",        "m4pe",      "m6pe",        "group_covariances",
  "Do the attribute effects differ by group?",  "m4",        "m4i",         "regressors",
  "Do the attribute effects differ by group?",  "m4p",       "m4ip",        "regressors",
  "Do the prices matter?",                      "m4",        "m4p",         "prices",
  "Do the prices matter?",                      "m4i",       "m4ip",        "prices",
  "Do the attributes interact?",                "m4p",       "m4pa",        "regressors",
  "Does the price effect differ by heat demand?", "m4p",     "m4pd",        "prices",
  "Do the expert characteristics matter?",      "m4p",       "m4pe",        "regressors + prices"
)

# Labels. The order of the names is the order in every table and figure.
TECHNOLOGY_LABELS <- c(
  "Wärmepumpe" = "Heat pump",
  "Pelletheizung" = "Pellet heating",
  "Hybridheizung" = "Hybrid heating",
  "Gasheizung" = "Gas heating",
  "Ölheizung" = "Oil heating",
  "Fernwärme" = "District heating",
  "Keine Empfehlung" = "No recommendation"
)
# One word, for the columns of the tables and heat maps: a header of one line
# leaves room for the rows, so that eight attributes with standard errors and
# the price coefficient fit a frame
TECHNOLOGY_LABELS_SHORT <- c(
  "Wärmepumpe" = "Heat pump",
  "Pelletheizung" = "Pellet",
  "Hybridheizung" = "Hybrid",
  "Gasheizung" = "Gas",
  "Ölheizung" = "Oil",
  "Fernwärme" = "District",
  "Keine Empfehlung" = "No rec."
)

# The Berufsgruppen: the reference group first, as it is the group the others
# are compared with. "All experts": a model without Berufsgruppe, or a
# covariance matrix for all experts.
GROUP_LABELS <- c(
  "Energieberater" = "Energy advisers",
  "SHK-Handwerk" = "HVAC installers",
  "Schornsteinfeger" = "Chimney sweeps",
  "All experts" = "All experts"
)
# The same inside a sentence
GROUP_LABELS_IN_TEXT <- c(
  "Energieberater" = "energy advisers",
  "SHK-Handwerk" = "HVAC installers",
  "Schornsteinfeger" = "chimney sweeps",
  "All experts" = "all experts"
)
REFERENCE_GROUP <- "Energieberater"
# The group dummies of 01_run_specifications.R
GROUP_DUMMIES <- c(
  is_energieberater = "Energieberater",
  is_shk = "SHK-Handwerk",
  is_schornsteinfeger = "Schornsteinfeger"
)

# The vignette attributes (the dummies of 01_run_specifications.R): the
# level that differs from the reference vignette
ATTRIBUTE_LABELS <- c(
  age_60 = "Couple in their mid-60s",
  income_low = "Income below €40,000",
  heating_oil = "Current heating: oil",
  replacement_urgent = "Heating broken",
  built_2000 = "Built in 2000",
  demand_70 = "Heat demand 70 kWh/m²",
  demand_200 = "Heat demand 200 kWh/m²",
  floor_heating = "Underfloor heating"
)

# The reference vignette, as in REFERENCE_LEVELS of 01_run_specifications.R,
# and its description for the slides (a macro, section 7); the check in
# section 2 makes sure the two stay the same
REFERENCE_VIGNETTE <- list(
  att_couple_age = "Mitte 40",
  att_income = "65.000 € brutto im Jahr",
  att_current_heating = "Gasheizung",
  att_replacement_timing = "muss in den nächsten Jahren ersetzt werden",
  att_build_year = "1970",
  att_heat_demand = "120",
  att_heat_distribution = "Heizkörper"
)
reference_vignette_text <- paste(
  "a couple in their mid-40s with a gross income of \\texteuro{}65,000 a",
  "year and a gas heating system that must be replaced in the next few",
  "years, in a house built in 1970 with a heat demand of 120 kWh/m$^2$ and",
  "radiators"
)

# The price terms: the price, the log price instead of it (4pl) and the price
# times the heat demand (4pd)
PRICE_LABELS <- c(
  price = "Price (cent per kWh)",
  log_price = "Log price",
  price_x_demand_70 = "Price × 70 kWh/m²",
  price_x_demand_200 = "Price × 200 kWh/m²"
)
# The dummies for a price not reported, <name>_price_na, and for a blank in
# Q17, <name>_rise_na, with the names of PRICE_VARIABLES and
# EXPECTATION_VARIABLES of 01_run_specifications.R. Those for Q17 are in Excel
# only, without labels.
PRICE_NA_LABELS <- c(
  hpelec_price_na = "Heat pump (electricity)",
  pellet_price_na = "Pellet heating",
  hybrid_price_na = "Hybrid heating",
  gas_price_na = "Gas heating",
  oil_price_na = "Oil heating",
  dh_price_na = "District heating"
)
RISE_NA_TERMS <- paste0(c("gas", "oil", "pellet", "hpelec", "hybrid", "dh"),
                        "_rise_na")

# The further terms of the variants of (4p). The interactions of two
# attributes, with one coefficient per technology:
INTERACTION_LABELS <- c(
  demand_70_x_built_2000 = "70 kWh/m² × built in 2000",
  demand_70_x_floor_heating = "70 kWh/m² × underfloor",
  demand_200_x_floor_heating = "200 kWh/m² × underfloor",
  floor_heating_x_built_2000 = "Underfloor × built in 2000"
)
# The expert characteristics: the degree days and the heat pump at home
# ((4pe), (6pe) since 2026-11-01) with one coefficient per technology, the
# others with one coefficient for all technologies, like the price: the match
# of every heating at home and the expected rise. The dummies for a blank in
# Q17 (RISE_NA_TERMS) are in Excel only.
EXPERT_LABELS <- c(
  hdd_c = "Heating degree days",
  heat_pump_at_home = "Heat pump at home",
  own_heating_match = "Heats the expert's home",
  expects_rise = "Expects its price to rise"
)
# The expert characteristics that are answers, centred at their shares,
# among the regressors or the price terms: with any of them, a model's
# probabilities and AMEs depend on the expert they describe (section 2)
EXPERT_ANSWER_TERMS <- c("heat_pump_at_home", "own_heating_match",
                         "expects_rise")

# The effects of all terms on the probabilities (01_run_term_effects.R): the
# fuels of the prices and expected rises (FUEL_COMPOUND_LABELS: the fuel
# before "prices", "Expects higher pellet prices"), and the models with a
# figure per outcome (5.9). The changes start where the model's AMEs are
# evaluated (section 2), and the figures show them next to the attribute
# AMEs. The other models' effects are in Excel.
FUEL_LABELS <- c(
  gas = "Gas",
  hpelec = "Electricity",
  dh = "District heating",
  oil = "Heating oil",
  pellet = "Pellets"
)
FUEL_COMPOUND_LABELS <- c(
  gas = "gas",
  hpelec = "electricity",
  dh = "district heating",
  oil = "heating oil",
  pellet = "pellet"
)
TERM_FIGURE_MODELS <- c("m4pe", "m6pe", "m4pa", "m4pd")

# Versions for the main slides, which leave room for notes beside or below
# them (Max, 2026-10-30), all on the reference outcome: narrow, the
# baseline probabilities of the benchmark and the AMEs of MAIN_MODEL and of
# the benchmark; short,
# the effects of all terms of MAIN_MODEL; the baseline probability by group
# across the models of BASELINE_SLIDE_MODELS (narrow); and the spread of the
# constants across the models of GAP_MODELS (a small table). Each model has
# its short legend in GAP_MODELS.
BENCHMARK_MODEL <- "m4"
GAP_MODELS <- c(
  m4 = "(4) Benchmark",
  m4p = "(4p) + prices",
  m4pe = "(4pe) + expert characteristics",
  m6pe = "(6pe) + covariances by group"
)
# The baseline probability by group on the slide: the benchmark and the main
# specification only (Max, 2026-10-31); each must be one of GAP_MODELS
BASELINE_SLIDE_MODELS <- c("m4", "m4pe")

# Colours and symbols: the ZEW chart colours for talks, the same values and
# groups as in R/descriptives/00_settings.R (Max, 2026-10-28; here 2026-10-29).
# Series in this order: lime, gray, ice, slate. The symbols have a fill (21 to
# 25) in the series' colour, and they and their intervals are drawn in
# ZEW_DARK, so that the light colours stand out on white.
ZEW_LIME <- "#C8D202"    # zewchartlime, first series
ZEW_GRAY <- "#9C9E9F"    # zewchartgray, second series
ZEW_ICE <- "#ABDADC"     # zewchartice, third series
ZEW_SLATE <- "#35484F"   # zewchartslate, fourth series
ZEW_DARK <- "#4C4C4C"    # zewdark: lines, outlines and text
GROUP_COLOURS <- c(
  "Energy advisers" = ZEW_LIME,
  "HVAC installers" = ZEW_GRAY,
  "Chimney sweeps" = ZEW_ICE,
  "All experts" = ZEW_SLATE
)
GROUP_SHAPES <- c(
  "Energy advisers" = 21,
  "HVAC installers" = 24,
  "Chimney sweeps" = 22,
  "All experts" = 23
)
# The models of the comparison figures, in the order of COMPARISON_MODELS
MODEL_COLOURS <- c(ZEW_LIME, ZEW_GRAY, ZEW_ICE, ZEW_SLATE)
MODEL_SHAPES <- c(21, 22, 24, 23)
# The heat maps of the correlations: from ZEW_LIME (-1) over light grey (0)
# to ZEW_SLATE (1), the diagonal in grey; white numbers on the dark tiles
HEATMAP_COLOURS <- c(low = ZEW_LIME, mid = "#F2F2F2", high = ZEW_SLATE,
                     text = ZEW_DARK, diagonal = "grey85")

# Sizes for the 16:9 slides of the economics-latex layout (text width 5.5
# in): a figure across a frame, and a heat map with room for text beside it.
# The figures keep their titles inside (profile), so they are taller than the
# skill's 2.3 in. The versions for the main slides: narrow, with notes or a
# table beside it; short, with notes below it.
FONT_SIZE <- 9
FIGURE_SIZES <- list(
  full = c(width = 5.5, height = 2.8),
  heatmap = c(width = 3.2, height = 2.8),
  narrow = c(width = 3.2, height = 2.8),
  short = c(width = 5.5, height = 2.3)
)
HEATMAP_TEXT_SIZE <- 2.3
# Font size of the tables: \scriptsize lets eight attributes with standard
# errors and six technology columns fit a frame
TABLE_FONT_SIZE <- "\\scriptsize"

# 2. Results -------------------------------------------------------------------
run_dir <- file.path(RC_LOGIT_DIR, run_folder(SELECTED_FW_ARM, REFERENCE_OUTCOME))
results_dir <- file.path(run_dir, "results")

results_list <- list()
for (model_name in MODELS$model) {
  results_file <- file.path(results_dir, paste0(model_name, ".rds"))
  if (file.exists(results_file)) {
    results_list[[model_name]] <- readRDS(results_file)
  } else {
    message("No results for ", model_name, ": run 01_run_specifications.R for it")
  }
}
if (length(results_list) == 0) stop("No results in ", results_dir)
MODELS <- MODELS %>%
  filter(model %in% names(results_list))
COMPARISON_MODELS <- intersect(COMPARISON_MODELS, MODELS$model)
COEFFICIENT_COMPARISON_MODELS <- intersect(COEFFICIENT_COMPARISON_MODELS,
                                           MODELS$model)
if (length(COMPARISON_MODELS) > length(MODEL_COLOURS)) {
  stop("COMPARISON_MODELS: at most ", length(MODEL_COLOURS), " models, one ",
       "per colour of MODEL_COLOURS")
}
# One colour and symbol per comparison model, in the order of the palette
MODEL_COLOURS <- setNames(MODEL_COLOURS[seq_along(COMPARISON_MODELS)],
                          COMPARISON_MODELS)
MODEL_SHAPES <- setNames(MODEL_SHAPES[seq_along(COMPARISON_MODELS)],
                         COMPARISON_MODELS)
if (!(MAIN_MODEL %in% MODELS$model)) {
  stop("No results for the main specification ", MAIN_MODEL, " (MAIN_MODEL)")
}
# The models of the slide versions across models keep the colour and symbol
# they have in the comparison figures: each must be one of COMPARISON_MODELS
GAP_MODELS <- GAP_MODELS[names(GAP_MODELS) %in% MODELS$model]
if (!all(names(GAP_MODELS) %in% COMPARISON_MODELS)) {
  stop("GAP_MODELS: ", paste(setdiff(names(GAP_MODELS), COMPARISON_MODELS),
                             collapse = ", "),
       " not in COMPARISON_MODELS, which sets the colours of the models")
}
BASELINE_SLIDE_MODELS <- intersect(BASELINE_SLIDE_MODELS, MODELS$model)
if (!all(BASELINE_SLIDE_MODELS %in% names(GAP_MODELS))) {
  stop("BASELINE_SLIDE_MODELS: ",
       paste(setdiff(BASELINE_SLIDE_MODELS, names(GAP_MODELS)), collapse = ", "),
       " not in GAP_MODELS, which sets the legends of the models")
}

# All results must come from the current version of 01_run_specifications.R
# and from the same arm, reference outcome, reference vignette and sample, so
# that they can be compared. The date and draws of each are printed, so that
# results of an older run stand out.
first_results <- results_list[[1]]
for (model_name in names(results_list)) {
  results <- results_list[[model_name]]
  if (is.null(results$probability_draws) || is.null(results$fit)) {
    stop("The results of ", model_name, " come from an older version of ",
         "01_run_specifications.R: rerun it")
  }
  stopifnot(results$arm == SELECTED_FW_ARM,
            results$reference_outcome == REFERENCE_OUTCOME,
            identical(results$outcomes, first_results$outcomes),
            identical(results$reference_levels, REFERENCE_VIGNETTE),
            identical(results$attribute_dummies, first_results$attribute_dummies))
  cat(model_name, ": estimated", as.character(results$date),
      "| draws: model", results$draws$n_draws_model,
      ", AMEs", results$draws$n_draws_ame,
      ", Krinsky-Robb", results$draws$n_draws_kr,
      "| log-likelihood", round(results$fit$log_likelihood, 2), "\n")
  # saved since 29 October 2026 (01_run_specifications.R, 4.6)
  if (!is.null(results$convergence)) {
    cat("   convergence code", results$convergence$code,
        "| iterations", results$convergence$iterations,
        "| largest gradient element", signif(results$convergence$max_gradient, 3),
        "| started from", results$convergence$start_from, "\n")
  }
  if (results$fit$n_choices != first_results$fit$n_choices ||
      results$fit$n_experts != first_results$fit$n_experts) {
    stop("The results come from different samples: rerun ",
         "01_run_specifications.R for all models")
  }
  # The notes and figures compare the groups with REFERENCE_GROUP, so it must
  # be the reference group of the Berufsgruppe shifts and interactions
  reference_dummy <- model_structure(results$specification,
                                     results$attribute_dummies,
                                     GROUP_DUMMIES)$reference_dummy
  if (!is.na(reference_dummy) && GROUP_DUMMIES[[reference_dummy]] != REFERENCE_GROUP) {
    stop("The reference group of ", model_name, " is ",
         GROUP_DUMMIES[[reference_dummy]], ", not REFERENCE_GROUP")
  }
  # The notes quote one number of each kind of draws, and the LR tests need
  # the same draws in both models
  if (!identical(results$draws, first_results$draws)) {
    stop("The models differ in their numbers of draws (printed above): rerun ",
         "01_run_specifications.R with the same draws for all, or report ",
         "only the models that share them (MODELS)")
  }
}

# The heat demand of the reference vignette and the dummies of the other
# levels, which the price times the heat demand is named after, e.g.
# price_x_demand_70 (as.numeric(): "120" -> 120)
reference_demand <- as.numeric(first_results$reference_levels$att_heat_demand)
demand_dummies <- first_results$attribute_dummies %>%
  filter(attribute == "att_heat_demand")
demand_terms <- paste0("price_x_", demand_dummies$dummy)

# Every regressor and price term of every model has a label, so that none is
# left out of the tables unnoticed: the attribute dummies and the Berufsgruppe
# above, the further regressors in INTERACTION_LABELS or EXPERT_LABELS, the
# price terms in PRICE_LABELS, PRICE_NA_LABELS or EXPERT_LABELS. label_of()
# stops if one has none. The dummies for a blank in Q17 (RISE_NA_TERMS) are in
# Excel only. A model with the price times the heat demand has it for every
# demand but the reference demand, as the tables of 4.4 and 5.6 assume.
for (model_name in names(results_list)) {
  results <- results_list[[model_name]]
  further <- further_regressors(results$specification,
                                results$attribute_dummies, GROUP_DUMMIES)
  label_of(further, c(INTERACTION_LABELS, EXPERT_LABELS), "regressors")
  price_terms <- results$specification$prices
  label_of(price_terms[!(price_terms %in% RISE_NA_TERMS)],
           c(PRICE_LABELS, PRICE_NA_LABELS, EXPERT_LABELS), "price terms")
  n_demand_terms <- sum(demand_terms %in% price_terms)
  if (n_demand_terms > 0 && n_demand_terms < length(demand_terms)) {
    stop(model_name, " has the price times some heat demands but not all: ",
         paste(intersect(demand_terms, price_terms), collapse = ", "))
  }
}

# The effects of all terms on the probabilities (01_run_term_effects.R), of
# the models that have them. They must rest on the model's current
# coefficients: the effects of an earlier estimation are left out.
term_effects_list <- list()
for (model_name in names(results_list)) {
  term_file <- file.path(results_dir, paste0(model_name, "_term_effects.rds"))
  if (file.exists(term_file)) {
    term_effects <- readRDS(term_file)
    if (identical(term_effects$coefficients,
                  results_list[[model_name]]$coefficients)) {
      term_effects_list[[model_name]] <- term_effects
    } else {
      message("The term effects of ", model_name, " rest on an earlier ",
              "estimation and are left out: rerun 01_run_term_effects.R for it")
    }
  }
}

# The expert at whom each model's probabilities, AMEs and mean constants are
# evaluated (expert_states). "average": the reference state of
# 01_run_specifications.R, the mean prices and degree days and, in (4pe) and
# (6pe), the shares of the expected rises and heating at home. "neutral": in
# (4pe) and (6pe), the mean prices and degree days, no expected price rise
# and no heat pump at home (Max, 2026-10-30 and 2026-11-01), from the term
# effects of 01_run_term_effects.R with TERM_EFFECTS_BASE "neutral": their
# AMEs and probabilities at every draw replace those of 01, so that the
# baseline probabilities, AMEs, group differences and comparison figures all
# describe that expert, and their base_constant_shift moves the mean
# constants there (constant_shifts). A neutral file without them (30 October
# 2026, before the evening) is left out, so that no figure mixes the two.
expert_states <- setNames(rep("average", length(results_list)),
                          names(results_list))
constant_shifts <- list()
for (model_name in names(term_effects_list)) {
  term_effects <- term_effects_list[[model_name]]
  if (!identical(term_effects$base_state, "neutral")) next
  if (is.null(term_effects$base_ame_table)) {
    message("The term effects of ", model_name, " start from the neutral base ",
            "state but have no AMEs there and are left out: rerun ",
            "01_run_term_effects.R for it")
    term_effects_list[[model_name]] <- NULL
    next
  }
  results <- results_list[[model_name]]
  # laid out as 01's: the same columns, profiles, attributes and outcomes
  same_layout <-
    identical(names(term_effects$base_ame_table), names(results$ame_table)) &&
    identical(names(term_effects$base_probability_draws),
              names(results$probability_draws)) &&
    setequal(term_effects$base_ame_table$vignette, results$ame_table$vignette) &&
    setequal(term_effects$base_ame_table$outcome, results$ame_table$outcome)
  if (!same_layout) {
    stop("The AMEs of the term effects of ", model_name, " are not laid out ",
         "as those of 01_run_specifications.R")
  }
  results_list[[model_name]]$ame_table <- term_effects$base_ame_table
  results_list[[model_name]]$probability_draws <-
    term_effects$base_probability_draws
  constant_shifts[[model_name]] <- term_effects$base_constant_shift
  expert_states[[model_name]] <- "neutral"
}
# The models with the expected rises or the heating at home must all be at
# the same expert, as the comparison figures show them together: a model
# whose neutral term effects are missing, stale or without AMEs stops the run
expert_models <- character(0)
for (model_name in names(results_list)) {
  model_specification <- results_list[[model_name]]$specification
  has_expert_terms <- any(EXPERT_ANSWER_TERMS %in%
                            c(model_specification$regressors,
                              model_specification$prices))
  if (has_expert_terms) expert_models <- c(expert_models, model_name)
}
average_models <- expert_models[expert_states[expert_models] == "average"]
if (length(average_models) > 0 && length(average_models) < length(expert_models)) {
  stop("The probabilities and AMEs of ", paste(average_models, collapse = ", "),
       " are those of an expert with average characteristics, those of ",
       paste(setdiff(expert_models, average_models), collapse = ", "),
       " of the neutral expert: run 01_run_term_effects.R for ",
       paste(average_models, collapse = ", "),
       " with TERM_EFFECTS_BASE \"neutral\" (messages above)")
}
if (length(average_models) > 0) {
  message(paste(average_models, collapse = ", "), ": the probabilities and ",
          "AMEs are those of an expert with average characteristics; for the ",
          "neutral expert, run 01_run_term_effects.R for them with ",
          "TERM_EFFECTS_BASE \"neutral\"")
}
cat("Expert of the probabilities, AMEs and mean constants:\n")
print(expert_states)
# The text for that expert in the notes and captions, from the terms of the
# models at the neutral expert: they must describe the same expert, as the
# comparison figures show them together
neutral_texts <- character(0)
for (model_name in names(expert_states)[expert_states == "neutral"]) {
  model_prices <- results_list[[model_name]]$specification$prices
  model_regressors <- results_list[[model_name]]$specification$regressors
  items <- character(0)
  if ("expects_rise" %in% model_prices) {
    items <- c(items, "no expected price rise")
  }
  if ("own_heating_match" %in% model_prices) {
    items <- c(items, "none of the technologies at home")
  }
  if ("heat_pump_at_home" %in% model_regressors) {
    items <- c(items, "no heat pump at home")
  }
  neutral_texts[[model_name]] <- paste(items, collapse = ", ")
}
if (length(unique(neutral_texts)) > 1) {
  stop("The models at the neutral expert describe different experts: ",
       paste(names(neutral_texts), neutral_texts, sep = ": ", collapse = "; "))
}
NEUTRAL_TEXT <- unname(neutral_texts[1])

outcomes <- first_results$outcomes
technologies <- outcomes[-1]
n_experts <- first_results$fit$n_experts
n_choices <- first_results$fit$n_choices
n_draws_kr <- first_results$draws$n_draws_kr

output_dirs <- list(
  tables = file.path(run_dir, "tables"),
  figures = file.path(run_dir, "figures"),
  excel = file.path(run_dir, "excel")
)
for (folder in output_dirs) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

# Labels used throughout. setNames(): the values, named by the models.
model_number <- setNames(MODELS$number, MODELS$model)
model_legend <- setNames(paste(MODELS$number, MODELS$description), MODELS$model)
reference_label <- TECHNOLOGY_LABELS[[REFERENCE_OUTCOME]]
# tolower(): "heat pump" inside a sentence
reference_text <- tolower(reference_label)
# The header of the tables with a column per technology, one word each, and
# in the cell above the row labels that the utilities are relative to the
# reference outcome
technology_cells <- latex_text(TECHNOLOGY_LABELS_SHORT[technologies])
technology_header <- paste0("\\textit{Relative to ", reference_text, "} & ",
                            paste(technology_cells, collapse = " & "),
                            " \\\\")
label_prefix <- paste0("tab:rcl_", SELECTED_FW_ARM, "_ref_",
                       file_name(REFERENCE_OUTCOME), "_")

# Notes shared by the tables. They are short, two or three lines, so that a
# table fits a frame: what the cells are, the standard errors, the stars. The
# sample in full in the table of the models (4.1); the reference vignette and
# the draws as macros for the slides (section 7). The experts by Berufsgruppe
# in the order of GROUP_LABELS (as.character(): the Berufsgruppe is a factor).
experts_by_group <- first_results$experts_by_group %>%
  mutate(berufsgruppe = as.character(berufsgruppe))
group_counts <- character(0)
for (group_name in GROUP_DUMMIES) {
  n_group <- experts_by_group$experts[experts_by_group$berufsgruppe == group_name]
  group_counts <- c(group_counts, paste(latex_number(n_group, 0),
                                        GROUP_LABELS_IN_TEXT[[group_name]]))
}
arm_text <- "could recommend district heating"
if (SELECTED_FW_ARM != "fw") arm_text <- "could not recommend district heating"
sample_note <- paste0(
  latex_number(n_choices, 0), " recommendations of ", latex_number(n_experts, 0),
  " experts (", paste(group_counts, collapse = ", "), ") who completed the ",
  "questionnaire and ", arm_text, "."
)
star_note <- "$^{*}$ $p<0.10$, $^{**}$ $p<0.05$, $^{***}$ $p<0.01$."
se_note <- "Standard errors clustered by expert."
kr_note <- "Krinsky-Robb standard errors."

# 3. The quantities of every model ---------------------------------------------
# One long table per quantity, each row with its model (functions in section 2
# of 00_functions_output.R)
constants_all <- tibble()
sds_all <- tibble()
effects_all <- tibble()
correlations_all <- tibble()
baseline_all <- tibble()
ame_all <- tibble()
differences_all <- tibble()
coefficients_all <- tibble()
for (model_name in names(results_list)) {
  results <- results_list[[model_name]]
  # constant_shifts[[model_name]]: NULL for a model at the average expert
  constants_all <- bind_rows(constants_all,
                             mean_constants(results, GROUP_DUMMIES,
                                            constant_shifts[[model_name]]) %>%
                               mutate(model = model_name))
  sds_all <- bind_rows(sds_all,
                       constant_sds(results, GROUP_DUMMIES) %>%
                         mutate(model = model_name))
  effects_all <- bind_rows(effects_all,
                           attribute_effects(results, GROUP_DUMMIES) %>%
                             mutate(model = model_name))
  correlations_all <- bind_rows(correlations_all,
                                correlation_table(results, GROUP_DUMMIES) %>%
                                  mutate(model = model_name))
  baseline_all <- bind_rows(baseline_all,
                            baseline_probabilities(results) %>%
                              mutate(model = model_name))
  ame_all <- bind_rows(ame_all,
                       results$ame_table %>%
                         rename(group = profile, attribute = vignette) %>%
                         mutate(model = model_name))
  differences_all <- bind_rows(differences_all,
                               group_differences(results, REFERENCE_GROUP) %>%
                                 mutate(model = model_name))
  # Every estimated parameter as mlogit names it, without the elements of the
  # Cholesky factor (the standard deviations and correlations show them)
  coefficients_all <- bind_rows(coefficients_all,
                                results$coefficient_table %>%
                                  filter(!startsWith(parameter, "chol.")) %>%
                                  mutate(model = model_name))
}

# Labels: label_of() stops if a value has none
constants_all <- constants_all %>%
  mutate(group_label = label_of(group, GROUP_LABELS, "groups"),
         technology_label = label_of(technology, TECHNOLOGY_LABELS, "outcomes"))
sds_all <- sds_all %>%
  mutate(group_label = label_of(group, GROUP_LABELS, "groups"),
         technology_label = label_of(technology, TECHNOLOGY_LABELS, "outcomes"))
effects_all <- effects_all %>%
  mutate(group_label = label_of(group, GROUP_LABELS, "groups"),
         attribute_label = label_of(attribute, ATTRIBUTE_LABELS, "attributes"),
         technology_label = label_of(technology, TECHNOLOGY_LABELS, "outcomes"))
baseline_all <- baseline_all %>%
  mutate(group_label = label_of(group, GROUP_LABELS, "groups"),
         outcome_label = label_of(outcome, TECHNOLOGY_LABELS, "outcomes"))
ame_all <- ame_all %>%
  mutate(group_label = label_of(group, GROUP_LABELS, "groups"),
         attribute_label = label_of(attribute, ATTRIBUTE_LABELS, "attributes"),
         outcome_label = label_of(outcome, TECHNOLOGY_LABELS, "outcomes"))
# The effects of all terms, with the labels of the changes; the heat demand of
# each demand dummy for the price at another heat demand (4pd)
term_effects_all <- tibble()
for (model_name in names(term_effects_list)) {
  term_effects_all <- bind_rows(term_effects_all,
                                term_effects_list[[model_name]]$table %>%
                                  mutate(model = model_name))
}
if (nrow(term_effects_all) > 0) {
  demand_levels <- setNames(demand_dummies$level, demand_dummies$dummy)
  change_labels <- term_effect_labels(term_effects_all, FUEL_LABELS,
                                      FUEL_COMPOUND_LABELS, TECHNOLOGY_LABELS,
                                      ATTRIBUTE_LABELS, demand_levels)
  term_effects_all <- term_effects_all %>%
    mutate(group_label = label_of(profile, GROUP_LABELS, "groups"),
           outcome_label = label_of(outcome, TECHNOLOGY_LABELS, "outcomes"),
           change_label = change_labels)
}

# The differences: vignette "reference" is the probability at the reference
# vignette, any other an attribute's AME
if (nrow(differences_all) > 0) {
  differences_all <- differences_all %>%
    mutate(group_label = label_of(group, GROUP_LABELS, "groups"),
           compared_with_label = label_of(compared_with, GROUP_LABELS, "groups"),
           attribute_label = label_of(vignette, c(ATTRIBUTE_LABELS,
                                                  reference = "Reference vignette"),
                                      "attributes"),
           outcome_label = label_of(outcome, TECHNOLOGY_LABELS, "outcomes"))
}

# 4. The models compared -------------------------------------------------------

# 4.1 The models and their fit ------------------------------------------------
# Which parts of each model depend on the Berufsgruppe, read from its
# specification
fit_all <- tibble()
for (model_name in MODELS$model) {
  results <- results_list[[model_name]]
  setup <- model_structure(results$specification, results$attribute_dummies,
                           GROUP_DUMMIES)
  further <- further_regressors(results$specification,
                                results$attribute_dummies, GROUP_DUMMIES)
  has_experts <- length(intersect(names(EXPERT_LABELS),
                                  c(further, results$specification$prices))) > 0
  # The convergence of the estimation, for Excel; saved with the results
  # since 29 October 2026
  convergence <- results$convergence
  if (is.null(convergence)) {
    convergence <- list(code = NA_real_, iterations = NA_real_,
                        max_gradient = NA_real_, start_from = NA_character_,
                        log_likelihood_start = NA_real_)
  }
  fit_all <- bind_rows(fit_all, results$fit %>%
                         mutate(model = model_name,
                                group_means = setup$group_constants |
                                  length(setup$group_shifts) > 0,
                                group_covariances = setup$group_constants,
                                interactions = setup$interactions,
                                prices = setup$prices,
                                expert_characteristics = has_experts,
                                convergence_code = convergence$code,
                                iterations = convergence$iterations,
                                max_gradient = convergence$max_gradient,
                                start_from = convergence$start_from,
                                log_likelihood_start =
                                  convergence$log_likelihood_start))
}
print(fit_all %>%
        select(model, n_parameters, log_likelihood, aic, bic, mcfadden_r2,
               convergence_code))

# The table of the models: all but the variants of (4p), which have a table
# of their own with their LR tests (4.6); with them, it would not fit a frame.
# The main specification is in both.
fit_models <- fit_all %>%
  filter(!(model %in% names(VARIANT_CHANGES)) | model == MAIN_MODEL)
aic_text <- latex_number(fit_models$aic, 1)
bic_text <- latex_number(fit_models$bic, 1)
is_lowest_aic <- fit_models$aic == min(fit_models$aic)
is_lowest_bic <- fit_models$bic == min(fit_models$bic)
aic_text[is_lowest_aic] <- paste0("\\textbf{", aic_text[is_lowest_aic], "}")
bic_text[is_lowest_bic] <- paste0("\\textbf{", bic_text[is_lowest_bic], "}")
fit_body <- paste0(
  model_number[fit_models$model], " & ",
  if_else(fit_models$group_means, "Yes", "No"), " & ",
  if_else(fit_models$group_covariances, "Yes", "No"), " & ",
  if_else(fit_models$interactions, "Yes", "No"), " & ",
  if_else(fit_models$prices, "Yes", "No"), " & ",
  if_else(fit_models$expert_characteristics, "Yes", "No"), " & ",
  latex_number(fit_models$n_parameters, 0), " & ",
  latex_number(fit_models$log_likelihood, 1), " & ",
  aic_text, " & ", bic_text, " & ",
  latex_number(fit_models$mcfadden_r2, 3), " \\\\"
)
fit_header <- paste0(
  "Model & ", header_cell("Group\\\\means"), " & ",
  header_cell("Group\\\\covariances"), " & ",
  header_cell("Group $\\times$\\\\attributes"), " & Prices & ",
  header_cell("Expert\\\\characteristics"), " & ",
  header_cell("Para-\\\\meters"), " & ", header_cell("Log-\\\\likelihood"),
  " & AIC & BIC & ", header_cell("McFadden\\\\R$^2$"), " \\\\"
)
fit_notes <- paste0(
  "Mixed logits with constants normal and correlated across experts; ",
  "``Yes'': this part differs by occupational group, or the model has the ",
  "prices or the expert characteristics. ", model_number[[MAIN_MODEL]],
  " is the main specification. Lowest AIC and BIC in bold, among these ",
  "models (the other variants of (4p) are in a table of their own); BIC with ",
  "$N$ = experts. ", sample_note
)
write_tex(table_float("lcccccrrrrr", fit_header, fit_body,
                      "Models and Their Fit", paste0(label_prefix, "fit"),
                      fit_notes, TABLE_FONT_SIZE),
          file.path(output_dirs$tables, "tab_rcl_fit.tex"))

# 4.2 Likelihood-ratio tests ---------------------------------------------------
# lr_tests() simulates the robust p-values with 1,000,000 draws per test and
# sets its own seed before each test
lr <- lr_tests(results_list, LR_TESTS)
lr <- lr %>%
  left_join(LR_TESTS %>% select(restricted, unrestricted, question),
            by = c("restricted", "unrestricted"), relationship = "one-to-one")
print(lr %>% select(restricted, unrestricted, lr, df, p, p_robust, mean_weight))

# A larger model whose log-likelihood is below that of the smaller one has
# stopped short of its maximum, although mlogit reported convergence: its LR
# statistic is negative and no test. The tables mark it with a dagger.
is_below <- !is.na(lr$lr) & lr$lr < 0
if (any(is_below)) {
  message("Below the model they nest, so short of their maximum: ",
          paste(model_number[lr$unrestricted[is_below]], collapse = ", "),
          ". Rerun them starting from it (start_from, 01_run_specifications.R)")
}
lr <- lr %>%
  mutate(lr_text = latex_number(lr, 1),
         lr_text = if_else(!is.na(lr) & lr < 0,
                           paste0(lr_text, "$^{\\dagger}$"), lr_text))
below_note <- paste0("$^{\\dagger}$ Negative: the larger model ended below ",
                     "the smaller one, short of its maximum.")

# One block of rows per question, the question in its first row. The tests of
# the variants of (4p) are in their own table (4.6).
lr_models <- lr %>%
  filter(!(unrestricted %in% names(VARIANT_CHANGES)))
lr_body <- character(0)
for (question_text in unique(lr_models$question)) {
  question_tests <- lr_models %>%
    filter(question == question_text)
  question_cells <- c(question_text, rep("", nrow(question_tests) - 1))
  if (length(lr_body) > 0) lr_body <- c(lr_body, "\\addlinespace")
  lr_body <- c(lr_body, paste0(
    question_cells, " & ", model_number[question_tests$restricted], " vs ",
    model_number[question_tests$unrestricted], " & ",
    latex_number(question_tests$df, 0), " & ",
    question_tests$lr_text, " & ",
    latex_p(question_tests$p), " & ", latex_p(question_tests$p_robust), " \\\\"
  ))
}
lr_header <- paste0("Question & Models & df & LR & $p$-value & ",
                    header_cell("Robust\\\\$p$-value"), " \\\\")
lr_notes <- paste0(
  "LR $=2(\\log L_1-\\log L_0)$ of the larger against the smaller model. ",
  "$p$-value from $\\chi^2_{\\mathrm{df}}$; robust $p$-value from a weighted ",
  "sum of $\\chi^2_1$ (Kent 1982; White 1982), which does not assume that the ",
  "larger model is correctly specified."
)
if (any(!is.na(lr_models$lr) & lr_models$lr < 0)) {
  lr_notes <- paste(lr_notes, below_note)
}
write_tex(table_float("llrrrr", lr_header, lr_body,
                      "Likelihood-Ratio Tests between Nested Models",
                      paste0(label_prefix, "lr_tests"), lr_notes,
                      TABLE_FONT_SIZE),
          file.path(output_dirs$tables, "tab_rcl_lr_tests.tex"))

# 4.3 Attribute effects across models -----------------------------------------
# One table per technology. Only models whose attribute effects are the same
# for all groups fit one column.
for (model_name in COEFFICIENT_COMPARISON_MODELS) {
  results <- results_list[[model_name]]
  setup <- model_structure(results$specification, results$attribute_dummies,
                           GROUP_DUMMIES)
  if (setup$interactions) {
    stop("COEFFICIENT_COMPARISON_MODELS: ", model_name, " has attribute ",
         "effects by group, which do not fit one column")
  }
  further <- further_regressors(results$specification,
                                results$attribute_dummies, GROUP_DUMMIES)
  if (any(further %in% names(INTERACTION_LABELS))) {
    stop("COEFFICIENT_COMPARISON_MODELS: ", model_name, " has interactions ",
         "of the attributes, so its attribute effects hold at the reference ",
         "levels of the attributes they interact with, unlike the others'")
  }
}
comparison_numbers <- model_number[COEFFICIENT_COMPARISON_MODELS]
for (technology_name in technologies) {
  technology_effects <- effects_all %>%
    filter(technology == technology_name)
  body <- character(0)
  for (attribute_name in names(ATTRIBUTE_LABELS)) {
    # one cell per model, in the order of the columns
    cells <- tibble()
    for (model_name in COEFFICIENT_COMPARISON_MODELS) {
      is_row <- technology_effects$model == model_name &
        technology_effects$attribute == attribute_name
      if (sum(is_row) != 1) stop("No effect of ", attribute_name, " in ", model_name)
      cells <- bind_rows(cells, technology_effects[is_row, ])
    }
    body <- c(body, estimate_rows(latex_text(ATTRIBUTE_LABELS[[attribute_name]]),
                                  cells$estimate, cells$se, cells$p))
  }
  header <- paste0(" & ", paste(comparison_numbers, collapse = " & "), " \\\\")
  technology_label <- TECHNOLOGY_LABELS[[technology_name]]
  # The coefficients are relative to the scale of the unexplained utility,
  # which can differ between models; the AMEs compare directly
  # the caption names the technology
  notes <- paste0(
    "Utility relative to ", reference_text, "; scales can differ between ",
    "models. ", se_note, " ", star_note
  )
  # tools::toTitleCase(): the caption in title case ("Gas Heating")
  # The columns are the models, so the caption names the technology only and
  # stays on one line in this narrow table
  caption <- paste0("Attribute Effects on ", tools::toTitleCase(technology_label))
  # strrep("c", 5): "ccccc", one centred column per model
  write_tex(table_float(paste0("l", strrep("c", length(comparison_numbers))),
                        header, body, latex_text(caption),
                        paste0(label_prefix, "coefficients_",
                               file_name(technology_label)),
                        notes, TABLE_FONT_SIZE),
            file.path(output_dirs$tables,
                      paste0("tab_rcl_coefficients_",
                             file_name(technology_label), ".tex")))
}

# 4.4 The price coefficients --------------------------------------------------
# One column per model with prices. They share the treatment of the reported
# prices (checked), which the notes describe. One row per price term that any
# of them has, in the order of the labels, the cell empty where a model
# does not have it: the price, or the log price instead (4pl), and the price
# times the heat demand (4pd). The dummies for a price not reported are in a
# second table: with the rows of the variants of (4p), one table no longer
# fits a frame. The expert characteristics of (4pe) are in a table of their
# own (5.8).
price_models <- character(0)
for (model_name in MODELS$model) {
  if (length(results_list[[model_name]]$specification$prices) > 0) {
    price_models <- c(price_models, model_name)
  }
}
if (length(price_models) > 0) {
  price_results <- results_list[[price_models[1]]]
  all_price_terms <- character(0)
  for (model_name in price_models) {
    stopifnot(identical(results_list[[model_name]]$price_reference,
                        price_results$price_reference),
              identical(results_list[[model_name]]$price_settings,
                        price_results$price_settings))
    all_price_terms <- c(all_price_terms,
                         results_list[[model_name]]$specification$prices)
  }
  # in the order of the labels (section 2 checked that every price term has
  # one): the price terms, then the dummies for a price not reported
  main_terms <- intersect(names(PRICE_LABELS), all_price_terms)
  dummy_terms <- intersect(names(PRICE_NA_LABELS), all_price_terms)
  term_labels <- c(PRICE_LABELS, PRICE_NA_LABELS)
  price_body <- character(0)
  not_reported_body <- character(0)
  for (term in c(main_terms, dummy_terms)) {
    # one cell per model, NA (an empty cell) if the model does not have it
    estimates <- rep(NA_real_, length(price_models))
    ses <- rep(NA_real_, length(price_models))
    p_values <- rep(NA_real_, length(price_models))
    for (k in seq_along(price_models)) {
      model_prices <- results_list[[price_models[k]]]$specification$prices
      is_row <- coefficients_all$model == price_models[k] &
        coefficients_all$parameter == term
      if (sum(is_row) == 1) {
        estimates[k] <- coefficients_all$estimate[is_row]
        ses[k] <- coefficients_all$se[is_row]
        p_values[k] <- coefficients_all$p[is_row]
      } else if (term %in% model_prices) {
        stop("No coefficient ", term, " in ", price_models[k])
      }
    }
    rows <- estimate_rows(latex_text(term_labels[[term]]), estimates, ses,
                          p_values, digits = 3)
    if (term %in% dummy_terms) {
      not_reported_body <- c(not_reported_body, rows)
    } else {
      price_body <- c(price_body, rows)
    }
  }

  # The mean prices, the COP and the weight of gas in the hybrid heating's
  # price are macros for the slides (section 7). The models with the log
  # price, and with the price times the heat demand, say so.
  log_price_models <- character(0)
  demand_models <- character(0)
  for (model_name in price_models) {
    model_prices <- results_list[[model_name]]$specification$prices
    if ("log_price" %in% model_prices) {
      log_price_models <- c(log_price_models, model_name)
    }
    if (all(demand_terms %in% model_prices)) {
      demand_models <- c(demand_models, model_name)
    }
  }
  price_notes <- paste0(
    "Own price per kWh, centred at the mean over the experts of both arms; ",
    "one coefficient for all technologies. "
  )
  if (length(log_price_models) > 0) {
    log_numbers <- paste(model_number[log_price_models], collapse = ", ")
    price_notes <- paste0(price_notes, log_numbers,
                          ": the log of the price minus the log of the mean. ")
  }
  if (length(demand_models) > 0) {
    demand_numbers <- paste(model_number[demand_models], collapse = ", ")
    price_notes <- paste0(price_notes, demand_numbers,
                          ": the price coefficient at ", reference_demand,
                          " kWh/m$^2$. ")
  }
  # as in the tables of the attribute effects across models (4.3); the
  # README says why the scales of (4p) and (4pe) are about the same
  price_notes <- paste0(price_notes, "With dummies for a price not reported. ",
                        "Scales can differ between models. ", se_note, " ",
                        star_note)
  price_header <- paste0(" & ", paste(model_number[price_models], collapse = " & "),
                         " \\\\")
  write_tex(table_float(paste0("l", strrep("c", length(price_models))),
                        price_header, price_body, "Price Coefficients",
                        paste0(label_prefix, "prices"), price_notes,
                        TABLE_FONT_SIZE),
            file.path(output_dirs$tables, "tab_rcl_prices.tex"))

  # The dummies for a price not reported, one row per technology
  not_reported_notes <- paste0(
    "Dummy for a price blank or not usable, in the rows of the technology; ",
    "its price is then set to the mean. Scales can differ between models. ",
    se_note, " ", star_note
  )
  write_tex(table_float(paste0("l", strrep("c", length(price_models))),
                        price_header, not_reported_body,
                        "Dummies for a Price Not Reported",
                        paste0(label_prefix, "prices_not_reported"),
                        not_reported_notes, TABLE_FONT_SIZE),
            file.path(output_dirs$tables, "tab_rcl_prices_not_reported.tex"))
}

# 4.5 Probabilities and AMEs across models (figures) --------------------------
# One panel per Berufsgruppe. A model without Berufsgruppe ((2)) has one set
# of probabilities for all experts: it appears in every panel. A model with
# prices: at the mean prices.
groups_in_panels <- unname(GROUP_DUMMIES)
comparison_series <- model_legend[COMPARISON_MODELS]
comparison_colours <- setNames(MODEL_COLOURS[COMPARISON_MODELS], comparison_series)
comparison_shapes <- setNames(MODEL_SHAPES[COMPARISON_MODELS], comparison_series)
# The caption: the models with one profile for all experts, which appear in
# every panel, and that the models with prices are at the mean prices
comparison_caption <- "95% Krinsky-Robb intervals."
for (model_name in COMPARISON_MODELS) {
  if (identical(names(results_list[[model_name]]$probability_draws), "All experts")) {
    comparison_caption <- paste0(comparison_caption, " ", model_number[[model_name]],
                                 " is the same for all groups.")
  }
}
if (any(COMPARISON_MODELS %in% price_models)) {
  comparison_caption <- paste0(comparison_caption,
                               " Models with prices: at the mean prices.")
}
# The models at the neutral expert (section 2)
comparison_neutral <- COMPARISON_MODELS[expert_states[COMPARISON_MODELS] == "neutral"]
if (length(comparison_neutral) > 0) {
  comparison_caption <- paste0(comparison_caption, " ",
                               paste(model_number[comparison_neutral],
                                     collapse = ", "),
                               ": mean degree days, ", NEUTRAL_TEXT, ".")
}
# strwrap(): lines of at most 115 characters, which fit the width
comparison_caption <- paste(strwrap(comparison_caption, width = 115),
                            collapse = "\n")

# rows_by_panel(): a model's rows for all experts go into every panel
plot_data <- rows_by_panel(baseline_all, COMPARISON_MODELS, groups_in_panels) %>%
  mutate(panel = factor(GROUP_LABELS[panel], levels = GROUP_LABELS[groups_in_panels]),
         series = factor(model_legend[model], levels = comparison_series),
         item = factor(outcome_label, levels = rev(TECHNOLOGY_LABELS[outcomes])),
         estimate = 100 * estimate,
         ci_low = 100 * ci_low_kr,
         ci_high = 100 * ci_high_kr)
plot <- plot_estimates(
  plot_data, comparison_colours, comparison_shapes,
  title = "Baseline probabilities across models",
  subtitle = "Probability of each recommendation at the reference vignette, by occupational group",
  caption = comparison_caption,
  x_title = "Probability (%)", base_size = FONT_SIZE,
  legend_rows = length(COMPARISON_MODELS),
  line_colour = ZEW_DARK
)
save_figure_pdf(plot, file.path(output_dirs$figures,
                                "fig_rcl_comparison_baseline.pdf"),
                FIGURE_SIZES$full)

# The AMEs: one figure per outcome and Berufsgruppe, as eight attributes times
# five models do not fit three panels of a slide
for (outcome_name in outcomes) {
  outcome_label <- TECHNOLOGY_LABELS[[outcome_name]]
  outcome_ames <- ame_all %>%
    filter(outcome == outcome_name)
  for (group_name in groups_in_panels) {
    plot_data <- rows_by_panel(outcome_ames, COMPARISON_MODELS, group_name) %>%
      mutate(series = factor(model_legend[model], levels = comparison_series),
             item = factor(attribute_label, levels = rev(ATTRIBUTE_LABELS)),
             estimate = 100 * ame,
             ci_low = 100 * ci_low_kr,
             ci_high = 100 * ci_high_kr) %>%
      select(-panel)
    plot <- plot_estimates(
      plot_data, comparison_colours, comparison_shapes,
      title = paste0(outcome_label, ": average marginal effects across models"),
      subtitle = paste0(GROUP_LABELS[[group_name]], ". Change in the probability ",
                        "when one attribute of the reference vignette changes"),
      caption = comparison_caption,
      x_title = "Change in probability (percentage points)", base_size = FONT_SIZE,
      legend_rows = length(COMPARISON_MODELS), line_colour = ZEW_DARK
    )
    save_figure_pdf(plot, file.path(output_dirs$figures,
                                    paste0("fig_rcl_comparison_ame_",
                                           file_name(outcome_label), "_",
                                           file_name(GROUP_LABELS[[group_name]]),
                                           ".pdf")),
                    FIGURE_SIZES$full)
  }
}

# 4.6 The variants of (4p) -----------------------------------------------------
# (4p) and its variants, each with one change: the change, the fit, and the
# LR test against (4p) (4.2). (4pl) has no test: it is not nested in (4p) and
# has as many parameters, so the log-likelihoods compare directly. Lowest AIC
# and BIC in bold among these models.
variant_models <- intersect(c("m4p", names(VARIANT_CHANGES)), MODELS$model)
if (length(variant_models) > 1) {
  # in the order of variant_models; match(): the row of each model
  variant_fit <- fit_all[match(variant_models, fit_all$model), ]
  aic_text <- latex_number(variant_fit$aic, 1)
  bic_text <- latex_number(variant_fit$bic, 1)
  is_lowest_aic <- variant_fit$aic == min(variant_fit$aic)
  is_lowest_bic <- variant_fit$bic == min(variant_fit$bic)
  aic_text[is_lowest_aic] <- paste0("\\textbf{", aic_text[is_lowest_aic], "}")
  bic_text[is_lowest_bic] <- paste0("\\textbf{", bic_text[is_lowest_bic], "}")
  variant_body <- character(0)
  for (k in seq_along(variant_models)) {
    change <- "--"
    if (variant_models[k] %in% names(VARIANT_CHANGES)) {
      change <- latex_text(VARIANT_CHANGES[[variant_models[k]]])
    }
    # the LR test against (4p); four empty cells if there is none
    test <- lr %>%
      filter(restricted == "m4p", unrestricted == variant_models[k])
    test_cells <- " &  &  & "
    if (nrow(test) == 1) {
      test_cells <- paste0(test$lr_text, " & ",
                           latex_number(test$df, 0), " & ",
                           latex_p(test$p), " & ", latex_p(test$p_robust))
    }
    variant_body <- c(variant_body, paste0(
      model_number[[variant_models[k]]], " & ", change, " & ",
      latex_number(variant_fit$n_parameters[k], 0), " & ",
      latex_number(variant_fit$log_likelihood[k], 1), " & ",
      aic_text[k], " & ", bic_text[k], " & ", test_cells, " \\\\"
    ))
  }
  variant_header <- paste0(
    "Model & Change & ", header_cell("Para-\\\\meters"), " & ",
    header_cell("Log-\\\\likelihood"), " & AIC & BIC & LR & df & $p$-value & ",
    header_cell("Robust\\\\$p$-value"), " \\\\"
  )
  variant_notes <- paste0(
    "(4p) and its variants, each with one change. LR test against (4p), robust ",
    "$p$-value as in the table of the LR tests; (4pl) is not nested in (4p) ",
    "and has as many parameters, so the log-likelihoods compare directly. ",
    "Lowest AIC and BIC in bold; BIC with $N$ = experts."
  )
  variant_tests <- lr %>%
    filter(restricted == "m4p", unrestricted %in% variant_models)
  if (any(!is.na(variant_tests$lr) & variant_tests$lr < 0)) {
    variant_notes <- paste(variant_notes, below_note)
  }
  write_tex(table_float("llrrrrrrrr", variant_header, variant_body,
                        "The Variants of (4p)", paste0(label_prefix, "variants"),
                        variant_notes, TABLE_FONT_SIZE),
            file.path(output_dirs$tables, "tab_rcl_variants.tex"))
}

# 4.7 Versions for the main slides across models ------------------------------
# With room for notes or a figure beside them (Max, 2026-10-30), on the
# reference outcome: (a) its probability at the reference vignette by group
# across the models of BASELINE_SLIDE_MODELS, how the gaps between the groups
# change with the prices and expert characteristics; (b) the mean and spread
# of the constants of BENCHMARK_MODEL; (c) the spread across the models of
# GAP_MODELS with one covariance matrix for all groups. The tables have the
# short labels of the technologies, so that they fit beside a figure.

# (a) The probability by group across models, with each model's colour and
#     symbol of the comparison figures and its legend of GAP_MODELS
slide_legends <- GAP_MODELS[BASELINE_SLIDE_MODELS]
gap_series <- unname(slide_legends)
gap_colours <- setNames(MODEL_COLOURS[names(slide_legends)], gap_series)
gap_shapes <- setNames(MODEL_SHAPES[names(slide_legends)], gap_series)
gap_groups <- unname(GROUP_DUMMIES)
plot_data <- baseline_all %>%
  filter(model %in% names(slide_legends), outcome == REFERENCE_OUTCOME,
         group %in% gap_groups) %>%
  mutate(item = factor(GROUP_LABELS[group],
                       levels = rev(GROUP_LABELS[gap_groups])),
         series = factor(slide_legends[model], levels = gap_series),
         estimate = 100 * estimate,
         ci_low = 100 * ci_low_kr,
         ci_high = 100 * ci_high_kr)
if (nrow(plot_data) > 0) {
  # where the models are evaluated: at the mean prices, and the models at the
  # neutral expert (section 2) also at the mean degree days, as NEUTRAL_TEXT
  # says. Named model by model, as
  # the figure may show only one model with prices.
  gap_caption <- paste("95% Krinsky-Robb intervals. Models with prices or",
                       "expert characteristics: at their means.")
  slide_states <- expert_states[BASELINE_SLIDE_MODELS]
  gap_neutral <- BASELINE_SLIDE_MODELS[slide_states == "neutral"]
  if (length(gap_neutral) > 0) {
    # the other models with prices have no expert characteristics (section 2
    # stops if models with them are at different experts)
    gap_prices_only <- setdiff(intersect(BASELINE_SLIDE_MODELS, price_models),
                               gap_neutral)
    gap_caption <- "95% Krinsky-Robb intervals."
    if (length(gap_prices_only) > 0) {
      gap_caption <- paste0(gap_caption, " ",
                            paste(model_number[gap_prices_only], collapse = ", "),
                            ": at the mean prices.")
    }
    gap_caption <- paste0(gap_caption, " ",
                          paste(model_number[gap_neutral], collapse = ", "),
                          ": at the mean prices and degree days, ",
                          NEUTRAL_TEXT, ".")
  }
  plot <- plot_estimates(
    plot_data, gap_colours, gap_shapes,
    title = paste0(reference_label, ": baseline probability"),
    subtitle = paste0("By occupational group and model, at the reference\n",
                      "vignette"),
    # strwrap(): lines of at most 60 characters, which fit the narrow width
    caption = paste(strwrap(gap_caption, width = 60), collapse = "\n"),
    x_title = "Probability (%)", base_size = FONT_SIZE,
    legend_rows = length(slide_legends), line_colour = ZEW_DARK
  ) +
    # the legend centred on the whole figure, as in 5.3
    theme(legend.location = "plot")
  save_figure_pdf(plot, file.path(output_dirs$figures,
                                  "fig_rcl_comparison_baseline_slide.pdf"),
                  FIGURE_SIZES$narrow)
}

# (b) The constants of the benchmark: one row per technology, the means of
#     the groups and the standard deviation across experts (one covariance
#     matrix for all groups), with standard errors
benchmark_constants <- constants_all %>%
  filter(model == BENCHMARK_MODEL)
benchmark_sds <- sds_all %>%
  filter(model == BENCHMARK_MODEL)
benchmark_groups <- intersect(names(GROUP_LABELS), benchmark_constants$group)
has_one_block <- all(benchmark_sds$group == "All experts")
if (nrow(benchmark_constants) > 0 && has_one_block) {
  body <- character(0)
  for (technology_name in technologies) {
    mean_rows <- benchmark_constants %>%
      filter(technology == technology_name)
    mean_rows <- mean_rows[match(benchmark_groups, mean_rows$group), ]
    sd_row <- benchmark_sds %>%
      filter(technology == technology_name)
    body <- c(body, estimate_rows(
      latex_text(TECHNOLOGY_LABELS_SHORT[[technology_name]]),
      c(mean_rows$estimate, sd_row$estimate),
      c(mean_rows$se, sd_row$se)
    ))
  }
  group_cells <- header_cell(gsub(" ", "\\\\", GROUP_LABELS[benchmark_groups],
                                  fixed = TRUE))
  header <- paste0(" & ", paste(group_cells, collapse = " & "), " & SD \\\\")
  # Short notes, two or three lines in this narrow table
  notes <- paste0("Utility relative to the ", reference_text, ": mean by ",
                  "group; SD across experts. Standard errors clustered by ",
                  "expert, Krinsky-Robb for the SD.")
  table_name <- paste0(BENCHMARK_MODEL, "_constants_slide")
  write_tex(table_float(paste0("l", strrep("c", length(benchmark_groups) + 1)),
                        header, body,
                        paste0("Constants, Model ",
                               model_number[[BENCHMARK_MODEL]]),
                        paste0(label_prefix, table_name), notes,
                        TABLE_FONT_SIZE),
            file.path(output_dirs$tables, paste0("tab_rcl_", table_name, ".tex")))
}

# (c) The spread of the constants across the models of GAP_MODELS with one
#     covariance matrix for all groups: one column per model
spread_models <- character(0)
for (model_name in names(GAP_MODELS)) {
  model_blocks <- sds_all$group[sds_all$model == model_name]
  if (all(model_blocks == "All experts")) {
    spread_models <- c(spread_models, model_name)
  }
}
if (length(spread_models) > 0) {
  body <- character(0)
  for (technology_name in technologies) {
    rows <- sds_all %>%
      filter(model %in% spread_models, technology == technology_name)
    rows <- rows[match(spread_models, rows$model), ]
    body <- c(body, estimate_rows(
      latex_text(TECHNOLOGY_LABELS_SHORT[[technology_name]]),
      rows$estimate, rows$se
    ))
  }
  header <- paste0(" & ", paste(model_number[spread_models], collapse = " & "),
                   " \\\\")
  # Short notes, two or three lines in this narrow table
  notes <- paste0("SD across experts of the utility relative to the ",
                  reference_text, ". Scales can differ between models. ",
                  kr_note)
  write_tex(table_float(paste0("l", strrep("c", length(spread_models))),
                        header, body, "Spread of the Constants",
                        paste0(label_prefix, "spread_slide"), notes,
                        TABLE_FONT_SIZE),
            file.path(output_dirs$tables, "tab_rcl_spread_slide.tex"))
}

# 5. Each model ----------------------------------------------------------------
# The price effect by heat demand and its Wald test, of every model that has
# them, for Excel (5.6)
price_demand_all <- tibble()
wald_all <- tibble()
for (model_name in MODELS$model) {
  results <- results_list[[model_name]]
  setup <- model_structure(results$specification, results$attribute_dummies,
                           GROUP_DUMMIES)
  number <- model_number[[model_name]]
  # The model in the subtitles of the figures and the notes of the tables
  model_title <- paste0("Model ", model_legend[[model_name]])
  model_note <- paste0("Model ", number, ": ",
                       latex_text(results$specification$label), ".")
  # The further terms of the variants of (4p): interactions of the
  # attributes, the price times the heat demand, expert characteristics
  further <- further_regressors(results$specification,
                                results$attribute_dummies, GROUP_DUMMIES)
  interaction_terms <- intersect(names(INTERACTION_LABELS), further)
  expert_terms <- intersect(names(EXPERT_LABELS),
                            c(further, results$specification$prices))
  has_demand_terms <- all(demand_terms %in% results$specification$prices)
  # The probabilities and constants of a model with prices are those of an
  # expert who reported the mean prices, and with expert characteristics, of
  # an expert with their mean (01_run_specifications.R centres them), or of
  # the neutral expert (section 2)
  prices_text <- ""
  prices_figure_text <- ""
  if (setup$prices) {
    prices_text <- ", at the mean prices"
    prices_figure_text <- "; at the mean prices"
  }
  if (length(expert_terms) > 0) {
    prices_text <- ", at the mean prices and expert characteristics"
    prices_figure_text <- "; at the mean prices and expert characteristics"
  }
  is_neutral_model <- expert_states[[model_name]] == "neutral"
  if (is_neutral_model) {
    prices_text <- paste0(", at the mean prices and degree days, ",
                          NEUTRAL_TEXT)
    prices_figure_text <- paste0("; at the mean prices and degree days, ",
                                 NEUTRAL_TEXT)
  }
  # The first line of the subtitles of its figures: strwrap() breaks a long
  # text into lines of at most 104 characters, which fit the width of a figure
  # (the description of (6pe) does not fit one line)
  model_subtitle <- paste(strwrap(paste0(model_title, prices_figure_text),
                                  width = 104), collapse = "\n")

  # 5.1 The constants: mean and standard deviation ----------------------------
  model_constants <- constants_all %>%
    filter(model == model_name)
  model_sds <- sds_all %>%
    filter(model == model_name)
  body <- panel_row("A. Mean", length(technologies) + 1)
  for (group_name in intersect(names(GROUP_LABELS), model_constants$group)) {
    rows <- model_constants %>%
      filter(group == group_name)
    # in the order of the technology columns; match(): the position of each
    # technology among the rows
    rows <- rows[match(technologies, rows$technology), ]
    body <- c(body, estimate_rows(latex_text(GROUP_LABELS[[group_name]]),
                                  rows$estimate, rows$se, rows$p))
  }
  body <- c(body, panel_row("B. Standard deviation across experts",
                            length(technologies) + 1))
  for (group_name in intersect(names(GROUP_LABELS), model_sds$group)) {
    rows <- model_sds %>%
      filter(group == group_name)
    rows <- rows[match(technologies, rows$technology), ]
    body <- c(body, estimate_rows(latex_text(GROUP_LABELS[[group_name]]),
                                  rows$estimate, rows$se))
  }
  if (setup$group_constants) {
    mean_text <- "each group's own constants"
    sd_text <- "a covariance matrix per group"
  } else if (length(setup$group_shifts) > 0) {
    mean_text <- "the constant plus the group's shift"
    sd_text <- "one covariance matrix for all groups"
  } else {
    mean_text <- "the same for all experts"
    sd_text <- "one covariance matrix"
  }
  notes <- paste0(
    model_note, " Utilities at the reference vignette", prices_text, ". A: ",
    mean_text, "; standard errors clustered ",
    "by expert. B: ", sd_text, "; Krinsky-Robb standard errors. ", star_note
  )
  write_tex(table_float(paste0("l", strrep("c", length(technologies))),
                        technology_header, body,
                        paste0("Constants of the Technologies, Model ", number),
                        paste0(label_prefix, model_name, "_constants"),
                        notes, TABLE_FONT_SIZE),
            file.path(output_dirs$tables,
                      paste0("tab_rcl_", model_name, "_constants.tex")))

  # 5.2 The attribute effects: the regression table ---------------------------
  # One table for all experts, or one per group if the effects differ by
  # group
  model_effects <- effects_all %>%
    filter(model == model_name)
  for (group_name in intersect(names(GROUP_LABELS), model_effects$group)) {
    group_effects <- model_effects %>%
      filter(group == group_name)
    body <- character(0)
    for (attribute_name in names(ATTRIBUTE_LABELS)) {
      rows <- group_effects %>%
        filter(attribute == attribute_name)
      rows <- rows[match(technologies, rows$technology), ]
      body <- c(body, estimate_rows(latex_text(ATTRIBUTE_LABELS[[attribute_name]]),
                                    rows$estimate, rows$se, rows$p))
    }
    # A model with prices: its price coefficient, one for all technologies
    # and groups, below the attributes. The dummies for a price not reported
    # are in a table of their own (4.4): a row of them would not fit the
    # frame, and the heat pump's has no column.
    prices_note <- ""
    if (setup$prices) {
      # the price, or the log price instead of it (4pl)
      price_term <- intersect(c("price", "log_price"),
                              results$specification$prices)
      if (length(price_term) != 1) {
        stop(model_name, " must have either the price or the log price")
      }
      is_price <- coefficients_all$model == model_name &
        coefficients_all$parameter == price_term
      if (sum(is_price) != 1) stop("No price coefficient in ", model_name)
      # no rule above it, so that the table still fits a frame: the cell
      # across the technologies sets it apart
      body <- c(body,
                spanning_row(latex_text(PRICE_LABELS[[price_term]]),
                             coefficients_all$estimate[is_price],
                             coefficients_all$se[is_price],
                             coefficients_all$p[is_price],
                             length(technologies), digits = 3))
      prices_note <- "With dummies for a price not reported. "
    }
    # The further terms are in tables of their own (5.6 to 5.8)
    further_note <- ""
    # short, so that the notes stay on two lines and the table fits a frame:
    # the attribute effects (main effects) hold at the reference levels of
    # the attributes they interact with
    if (length(interaction_terms) > 0) {
      further_note <- paste0(further_note,
                             "Main effects at the reference levels. ")
    }
    if (has_demand_terms) {
      further_note <- paste0(further_note, "Price coefficient at ",
                             reference_demand, " kWh/m$^2$. ")
    }
    if (length(expert_terms) > 0) {
      further_note <- paste0(further_note, "Expert characteristics in a table ",
                             "of their own. ")
    }
    caption <- paste0("Attribute Effects, Model ", number)
    table_name <- paste0("tab_rcl_", model_name, "_coefficients")
    group_text <- ""
    if (group_name != "All experts") {
      caption <- paste0("Attribute Effects for ",
                        tools::toTitleCase(GROUP_LABELS[[group_name]]),
                        ", Model ", number)
      table_name <- paste0(table_name, "_", file_name(GROUP_LABELS[[group_name]]))
      if (group_name != REFERENCE_GROUP) {
        group_text <- "Coefficient plus the group's interaction. "
      }
    }
    # The caption names the model and the group, the header that the
    # utilities are relative to the reference outcome; the notes say what
    # else the rows are
    notes <- paste0(group_text, prices_note, further_note, se_note, " ",
                    star_note)
    write_tex(table_float(paste0("l", strrep("c", length(technologies))),
                          technology_header, body, latex_text(caption),
                          paste0(label_prefix, sub("^tab_rcl_", "", table_name)),
                          notes, TABLE_FONT_SIZE),
              file.path(output_dirs$tables, paste0(table_name, ".tex")))
  }

  # 5.3 Probabilities at the reference vignette -------------------------------
  model_baseline <- baseline_all %>%
    filter(model == model_name)
  model_groups <- intersect(names(GROUP_LABELS), model_baseline$group)
  body <- character(0)
  for (outcome_name in outcomes) {
    rows <- model_baseline %>%
      filter(outcome == outcome_name)
    rows <- rows[match(model_groups, rows$group), ]
    body <- c(body, estimate_rows(latex_text(TECHNOLOGY_LABELS[[outcome_name]]),
                                  100 * rows$estimate, 100 * rows$se,
                                  digits = 1))
  }
  header <- paste0(" & ", paste(header_cell(gsub(" ", "\\\\", GROUP_LABELS[model_groups],
                                                 fixed = TRUE)),
                                collapse = " & "), " \\\\")
  # The table is narrow, (2) has one column: the caption names the model,
  # the notes say no more
  notes <- paste0("In \\%, at the reference vignette", prices_text, ". ",
                  kr_note)
  write_tex(table_float(paste0("l", strrep("c", length(model_groups))),
                        header, body,
                        paste0("Baseline Probabilities, Model ", number),
                        paste0(label_prefix, model_name, "_baseline"),
                        notes, TABLE_FONT_SIZE),
            file.path(output_dirs$tables,
                      paste0("tab_rcl_", model_name, "_baseline.tex")))

  # Figures of this model: the groups in the order of GROUP_LABELS. Whether
  # two groups differ is not marked: their probabilities and AMEs come from
  # the same draws of shared coefficients, so the intervals of the groups say
  # little about their difference. The tests of the differences are in Excel.
  series_levels <- GROUP_LABELS[model_groups]
  group_colours <- GROUP_COLOURS[series_levels]
  group_shapes <- GROUP_SHAPES[series_levels]

  plot_data <- model_baseline %>%
    mutate(item = factor(outcome_label, levels = rev(TECHNOLOGY_LABELS[outcomes])),
           series = factor(group_label, levels = series_levels),
           estimate = 100 * estimate,
           ci_low = 100 * ci_low_kr,
           ci_high = 100 * ci_high_kr)
  plot <- plot_estimates(
    plot_data, group_colours, group_shapes,
    title = "Baseline probabilities",
    subtitle = paste0(model_subtitle, "\nProbability of each ",
                      "recommendation at the reference vignette"),
    caption = "95% Krinsky-Robb intervals.",
    x_title = "Probability (%)", base_size = FONT_SIZE, line_colour = ZEW_DARK
  )
  save_figure_pdf(plot, file.path(output_dirs$figures,
                                  paste0("fig_rcl_", model_name, "_baseline.pdf")),
                  FIGURE_SIZES$full)
  # The version for the main slides, with a table beside it: the texts
  # wrapped to the narrow width, the legend in two rows and centred on the
  # whole figure (legend.location = "plot"), not on the panel, which the
  # labels of the rows push to the right
  if (model_name == BENCHMARK_MODEL) {
    plot <- plot_estimates(
      plot_data, group_colours, group_shapes,
      title = "Baseline probabilities",
      subtitle = paste(c(strwrap(paste0(model_title, prices_figure_text),
                                 width = 60),
                         "Probability of each recommendation at the",
                         "reference vignette"), collapse = "\n"),
      caption = "95% Krinsky-Robb intervals.",
      x_title = "Probability (%)", base_size = FONT_SIZE, legend_rows = 2,
      line_colour = ZEW_DARK
    ) +
      theme(legend.location = "plot")
    save_figure_pdf(plot, file.path(output_dirs$figures,
                                    paste0("fig_rcl_", model_name,
                                           "_baseline_slide.pdf")),
                    FIGURE_SIZES$narrow)
  }

  # 5.4 AMEs, one figure per outcome ------------------------------------------
  for (outcome_name in outcomes) {
    outcome_label <- TECHNOLOGY_LABELS[[outcome_name]]
    baseline_rows <- model_baseline %>%
      filter(outcome == outcome_name)
    baseline_rows <- baseline_rows[match(model_groups, baseline_rows$group), ]
    baseline_text <- paste0(GROUP_LABELS_IN_TEXT[model_groups], " ",
                            round(100 * baseline_rows$estimate), "%")
    plot_data <- ame_all %>%
      filter(model == model_name, outcome == outcome_name) %>%
      mutate(item = factor(attribute_label, levels = rev(ATTRIBUTE_LABELS)),
             series = factor(group_label, levels = series_levels),
             estimate = 100 * ame,
             ci_low = 100 * ci_low_kr,
             ci_high = 100 * ci_high_kr)
    plot <- plot_estimates(
      plot_data, group_colours, group_shapes,
      title = paste0(outcome_label, ": average marginal effects"),
      subtitle = paste0(model_subtitle,
                        "\nBaseline probability: ",
                        paste(baseline_text, collapse = ", ")),
      caption = paste0("Change in the probability when one attribute of the ",
                       "reference vignette changes; 95% Krinsky-Robb intervals."),
      x_title = "Change in probability (percentage points)", base_size = FONT_SIZE,
      line_colour = ZEW_DARK
    )
    save_figure_pdf(plot, file.path(output_dirs$figures,
                                    paste0("fig_rcl_", model_name, "_ame_",
                                           file_name(outcome_label), ".pdf")),
                    FIGURE_SIZES$full)
    # The version for the main slides, for the main specification and the
    # benchmark, with notes beside it: the texts wrapped to the narrow width
    # (where the probabilities are evaluated moves from the subtitle to the
    # caption), a shorter axis title, the legend in two rows and centred on
    # the whole figure, as in 5.3
    if (model_name %in% c(MAIN_MODEL, BENCHMARK_MODEL) &&
        outcome_name == REFERENCE_OUTCOME) {
      baseline_line <- paste0("Baseline probability: ",
                              paste(baseline_text, collapse = ", "))
      plot <- plot_estimates(
        plot_data, group_colours, group_shapes,
        title = paste0(outcome_label, ": average marginal effects"),
        subtitle = paste(c(strwrap(model_title, width = 62),
                           strwrap(baseline_line, width = 62)),
                         collapse = "\n"),
        caption = paste(strwrap(paste0("Change in the probability when one ",
                                       "attribute of the reference vignette ",
                                       "changes", prices_text, "; 95% ",
                                       "Krinsky-Robb intervals."),
                                width = 68), collapse = "\n"),
        x_title = "Change (percentage points)",
        base_size = FONT_SIZE, legend_rows = 2, line_colour = ZEW_DARK
      ) +
        theme(legend.location = "plot")
      save_figure_pdf(plot, file.path(output_dirs$figures,
                                      paste0("fig_rcl_", model_name, "_ame_",
                                             file_name(outcome_label),
                                             "_slide.pdf")),
                      FIGURE_SIZES$narrow)
    }
  }

  # 5.5 Correlations of the constants: one heat map per covariance block -----
  model_correlations <- correlations_all %>%
    filter(model == model_name)
  for (group_name in intersect(names(GROUP_LABELS), model_correlations$group)) {
    heatmap_data <- model_correlations %>%
      filter(group == group_name) %>%
      mutate(row = factor(TECHNOLOGY_LABELS[technology_row],
                          levels = rev(TECHNOLOGY_LABELS[technologies])),
             column = factor(TECHNOLOGY_LABELS_SHORT[technology_column],
                             levels = TECHNOLOGY_LABELS_SHORT[technologies]),
             is_diagonal = technology_row == technology_column,
             is_significant = ci_low_kr > 0 | ci_high_kr < 0)
    # Print the matrix: one row per technology (pivot_wider(): the columns of
    # the pairs side by side)
    cat("\n", model_name, ", ", group_name, "\n", sep = "")
    heatmap_data %>%
      select(technology_row, technology_column, estimate) %>%
      mutate(estimate = round(estimate, 2)) %>%
      pivot_wider(names_from = technology_column, values_from = estimate) %>%
      print(width = Inf)
    # The heat map is narrow: strwrap() breaks a long model description into
    # lines of at most 50 characters
    heatmap_subtitle <- paste(c(strwrap(model_title, width = 50),
                                GROUP_LABELS[[group_name]]), collapse = "\n")
    plot <- plot_correlations(
      heatmap_data,
      title = "Correlations of the constants",
      subtitle = heatmap_subtitle,
      caption = paste0("Constant: utility relative to ", reference_text,
                       ".\nBold: 95% Krinsky-Robb interval excludes 0."),
      base_size = FONT_SIZE, text_size = HEATMAP_TEXT_SIZE,
      colours = HEATMAP_COLOURS
    )
    file_suffix <- ""
    if (setup$group_constants) {
      file_suffix <- paste0("_", file_name(GROUP_LABELS[[group_name]]))
    }
    save_figure_pdf(plot, file.path(output_dirs$figures,
                                    paste0("fig_rcl_", model_name,
                                           "_correlations", file_suffix, ".pdf")),
                    FIGURE_SIZES$heatmap)
  }

  # 5.6 The price effect by heat demand ---------------------------------------
  # In a model with the price times the heat demand: the price coefficient at
  # each heat demand (at the reference demand, plus the interaction at the
  # others), and the one a price effect proportional to the heat demand would
  # have, as if the price mattered through the running cost, price times heat
  # demand: the coefficient at the reference demand times demand / reference
  # demand. The Wald test of proportionality, for all demands at once: each
  # interaction is (demand / reference demand - 1) times the price
  # coefficient.
  if (has_demand_terms) {
    demand_rows <- tibble()
    restrictions <- matrix(0, nrow = length(demand_terms),
                           ncol = 1 + length(demand_terms),
                           dimnames = list(NULL, c("price", demand_terms)))
    for (k in seq_along(demand_terms)) {
      demand <- as.numeric(demand_dummies$level[k])
      ratio <- demand / reference_demand
      # not named like the columns: inside tibble(), a column created first
      # would hide an object of the same name
      estimated_effect <- sum_of_coefficients(results,
                                              c("price", demand_terms[k]))
      proportional_effect <- sum_of_coefficients(results, "price",
                                                 weights = ratio)
      demand_rows <- bind_rows(demand_rows, tibble(
        demand = demand,
        estimate = estimated_effect$estimate, se = estimated_effect$se,
        p = estimated_effect$p, proportional = proportional_effect$estimate,
        se_proportional = proportional_effect$se,
        p_proportional = proportional_effect$p
      ))
      restrictions[k, "price"] <- -(ratio - 1)
      restrictions[k, demand_terms[k]] <- 1
    }
    at_reference <- sum_of_coefficients(results, "price")
    demand_rows <- bind_rows(demand_rows, tibble(
      demand = reference_demand,
      estimate = at_reference$estimate, se = at_reference$se,
      p = at_reference$p, proportional = at_reference$estimate,
      se_proportional = at_reference$se, p_proportional = at_reference$p
    )) %>%
      arrange(demand)
    wald <- wald_test(results, restrictions)
    price_demand_all <- bind_rows(price_demand_all, demand_rows %>%
                                    mutate(model = model_name))
    wald_all <- bind_rows(wald_all, wald %>%
                            mutate(model = model_name,
                                   test = "Price proportional to heat demand"))

    body <- character(0)
    for (k in seq_len(nrow(demand_rows))) {
      body <- c(body, estimate_rows(
        paste0(demand_rows$demand[k], " kWh/m$^2$"),
        c(demand_rows$estimate[k], demand_rows$proportional[k]),
        c(demand_rows$se[k], demand_rows$se_proportional[k]),
        c(demand_rows$p[k], demand_rows$p_proportional[k]),
        digits = 3
      ))
    }
    header <- paste0("Heat demand & Estimated & ",
                     header_cell("Proportional\\\\to demand"), " \\\\")
    notes <- paste0(
      "Price coefficient (cent per kWh) at each heat demand: at ",
      reference_demand, " kWh/m$^2$ plus the interaction. Proportional: the ",
      "coefficient at ", reference_demand, " kWh/m$^2$ times demand/",
      reference_demand, ", as if the price mattered through the running ",
      "cost. Wald test of proportionality: $\\chi^2_{", wald$df, "}$ = ",
      latex_number(wald$statistic, 2), ", $p$ = ", latex_p(wald$p), ". ",
      se_note, " ", star_note
    )
    write_tex(table_float("lcc", header, body,
                          paste0("Price Effect by Heat Demand, Model ", number),
                          paste0(label_prefix, model_name, "_price_by_demand"),
                          notes, TABLE_FONT_SIZE),
              file.path(output_dirs$tables,
                        paste0("tab_rcl_", model_name, "_price_by_demand.tex")))
  }

  # 5.7 The interactions of the attributes ------------------------------------
  # One coefficient per technology, as the attribute effects: the utility
  # beyond the effects of the two attributes when both apply
  if (length(interaction_terms) > 0) {
    body <- character(0)
    for (term in interaction_terms) {
      rows <- tibble()
      for (technology_name in technologies) {
        rows <- bind_rows(rows, sum_of_coefficients(
          results, paste0(term, ":", technology_name)))
      }
      body <- c(body, estimate_rows(latex_text(INTERACTION_LABELS[[term]]),
                                    rows$estimate, rows$se, rows$p))
    }
    notes <- paste0(
      "Utility beyond the effects of the two attributes when both apply. No ",
      "vignette has a house built in 2000 with 200 kWh/m$^2$. ", se_note, " ",
      star_note
    )
    write_tex(table_float(paste0("l", strrep("c", length(technologies))),
                          technology_header, body,
                          paste0("Attribute Interactions, Model ", number),
                          paste0(label_prefix, model_name, "_interactions"),
                          notes, TABLE_FONT_SIZE),
              file.path(output_dirs$tables,
                        paste0("tab_rcl_", model_name, "_interactions.tex")))
  }

  # 5.8 The expert characteristics --------------------------------------------
  # A regressor (the degree days, the heat pump at home) with one coefficient
  # per technology, as the attribute effects; a term with one coefficient
  # (the expected price rise), as the price, in a cell across the columns.
  # The dummies for a blank in Q17 are in Excel.
  if (length(expert_terms) > 0) {
    body <- character(0)
    for (term in expert_terms) {
      if (term %in% results$specification$regressors) {
        rows <- tibble()
        for (technology_name in technologies) {
          rows <- bind_rows(rows, sum_of_coefficients(
            results, paste0(term, ":", technology_name)))
        }
        body <- c(body, estimate_rows(latex_text(EXPERT_LABELS[[term]]),
                                      rows$estimate, rows$se, rows$p))
      } else {
        coefficient <- sum_of_coefficients(results, term)
        body <- c(body, spanning_row(latex_text(EXPERT_LABELS[[term]]),
                                     coefficient$estimate, coefficient$se,
                                     coefficient$p, length(technologies)))
      }
    }
    notes <- ""
    if ("hdd_c" %in% expert_terms) {
      reference <- results$expert_reference
      notes <- paste0(notes, "Degree days of the expert's NUTS3 region in ",
                      reference$hdd_year, ", in ",
                      latex_number(reference$hdd_unit, 0),
                      ", centred at the mean; the mean for experts without ",
                      "a region. ")
    }
    if ("heat_pump_at_home" %in% expert_terms) {
      notes <- paste0(notes, "Heat pump at home: 0/1 minus its share among ",
                      "the experts; no heat pump for other heatings and no ",
                      "answer. ")
    }
    if ("own_heating_match" %in% expert_terms) {
      notes <- paste0(notes, "Heating at home: 0/1 minus its share among the ",
                      "experts, one coefficient for all technologies. ")
    }
    if ("expects_rise" %in% expert_terms) {
      notes <- paste0(notes, "Expected rise: 0/1 minus its share among the ",
                      "experts, one coefficient for all technologies; with ",
                      "dummies for a blank in Q17 (in Excel). ")
    }
    notes <- paste0(notes, se_note, " ", star_note)
    write_tex(table_float(paste0("l", strrep("c", length(technologies))),
                          technology_header, body,
                          paste0("Expert Characteristics, Model ", number),
                          paste0(label_prefix, model_name, "_expert"),
                          notes, TABLE_FONT_SIZE),
              file.path(output_dirs$tables,
                        paste0("tab_rcl_", model_name, "_expert.tex")))
  }

  # 5.9 The effects of all terms on the probabilities -------------------------
  # For the models of TERM_FIGURE_MODELS with term effects
  # (01_run_term_effects.R): the changes that move the outcome's own utility
  # (the prices and expected rises of its fuels, its heating at home), and
  # those that move every utility (the degree days, the heat pump at home, the
  # interactions of the attributes); the dummies for a price or an
  # expectation not reported are in
  # Excel only. The changes start where the model's AMEs and baseline
  # probabilities are evaluated (section 2: the average or the neutral
  # expert), so they are shown next to the attribute AMEs of 5.4, on the same
  # scale, with the same baseline probabilities (checked) (Max, 2026-10-30).
  # The subtitle gives the probabilities where the changes start, the
  # caption where that is.
  if (model_name %in% TERM_FIGURE_MODELS &&
      model_name %in% names(term_effects_list)) {
    term_effects <- term_effects_list[[model_name]]
    # A file of 01_run_term_effects.R from before 30 October 2026 has no
    # base_state: its changes start from the reference state of the AMEs
    is_neutral <- identical(term_effects$base_state, "neutral")
    if (is_neutral != is_neutral_model) {
      stop(model_name, ": the term effects and the AMEs start from different ",
           "experts")
    }
    # The expert characteristics that are answers: with them, the caption
    # says from which expert the changes start, also for an outcome without
    # rows of them
    model_has_experts <- any(EXPERT_ANSWER_TERMS %in%
                               c(results$specification$regressors,
                                 results$specification$prices))
    model_terms <- term_effects_all %>%
      filter(model == model_name, is_affected,
             !(type %in% c("price_na", "rise_na")))
    # Check: the probabilities where the changes start are the baseline
    # probabilities of the model's other figures
    if (!is.null(term_effects$base_probabilities)) {
      base_check <- term_effects$base_probabilities %>%
        rename(group = profile, estimate_base = estimate) %>%
        select(group, outcome, estimate_base) %>%
        left_join(model_baseline %>% select(group, outcome, estimate),
                  by = c("group", "outcome"), relationship = "one-to-one")
      if (anyNA(base_check$estimate) ||
          max(abs(base_check$estimate_base - base_check$estimate)) > 1e-10) {
        stop(model_name, ": the term effects start from other probabilities ",
             "than the baseline probabilities of its other figures")
      }
    }
    for (outcome_name in outcomes) {
      outcome_label <- TECHNOLOGY_LABELS[[outcome_name]]
      outcome_terms <- model_terms %>%
        filter(outcome == outcome_name)
      term_rows <- outcome_terms %>%
        transmute(group_label, item_label = change_label,
                  estimate = 100 * effect, ci_low = 100 * ci_low_kr,
                  ci_high = 100 * ci_high_kr)
      # the changes in the order of 01_run_term_effects.R, below the
      # attributes
      term_levels <- unique(term_rows$item_label)
      attribute_rows <- ame_all %>%
        filter(model == model_name, outcome == outcome_name) %>%
        transmute(group_label, item_label = attribute_label,
                  estimate = 100 * ame, ci_low = 100 * ci_low_kr,
                  ci_high = 100 * ci_high_kr)
      item_levels <- c(ATTRIBUTE_LABELS, term_levels)
      plot_data <- bind_rows(attribute_rows, term_rows) %>%
        mutate(item = factor(item_label, levels = rev(item_levels)),
               series = factor(group_label, levels = series_levels))
      term_data <- term_rows %>%
        mutate(item = factor(item_label, levels = rev(term_levels)),
               series = factor(group_label, levels = series_levels))

      # The subtitle: the model and the probability of the outcome where the
      # changes start, by group; the caption says where they start, so that
      # the many rows keep their room
      outcome_base <- model_baseline %>%
        filter(outcome == outcome_name)
      outcome_base <- outcome_base[match(model_groups, outcome_base$group), ]
      baseline_text <- paste0(GROUP_LABELS_IN_TEXT[model_groups], " ",
                              round(100 * outcome_base$estimate), "%")
      term_subtitle <- paste0(paste(strwrap(model_title, width = 104),
                                    collapse = "\n"),
                              "\nBaseline probability: ",
                              paste(baseline_text, collapse = ", "))

      # The caption: where the changes start, at which means everything else
      # stays (the prices of the rows' fuels, in cent per kWh, and the degree
      # days), and how each kind of row changes
      # formatC(): the number as text, with fixed decimals and a thousands
      # comma
      price_fuels <- unique(outcome_terms$fuel[outcome_terms$type == "price"])
      mean_items <- character(0)
      for (fuel in price_fuels) {
        is_fuel <- results$price_reference$fuel == fuel
        mean_price <- results$price_reference$mean[is_fuel]
        mean_items <- c(mean_items,
                        paste0(tolower(FUEL_LABELS[[fuel]]), " ",
                               formatC(mean_price, format = "f", digits = 1),
                               " ct/kWh"))
      }
      if (any(outcome_terms$type == "hdd")) {
        hdd_mean <- results$expert_reference$hdd_mean
        mean_items <- c(mean_items,
                        paste(formatC(hdd_mean, format = "f", digits = 0,
                                      big.mark = ","),
                              "degree days"))
      }
      # the expected rises, the heat pump at home and the heating at home
      # switched on, and a heating at home that replaces another (condition,
      # e.g. the heat pump instead of a gas heating, OWN_HEATING_BEFORE of
      # 01_run_term_effects.R)
      is_own_heating <- outcome_terms$type == "own_heating"
      switched_on <- character(0)
      if (any(outcome_terms$type == "expects_rise")) {
        switched_on <- c(switched_on, "an expected rise")
      }
      is_heat_pump_term <- outcome_terms$change == "heat_pump_at_home"
      if (any(is_heat_pump_term)) {
        switched_on <- c(switched_on, "a heat pump at home")
      }
      if (any(is_own_heating & is.na(outcome_terms$condition) &
                !is_heat_pump_term)) {
        switched_on <- c(switched_on, "own heating")
      }
      replacing_rows <- outcome_terms %>%
        filter(type == "own_heating", !is.na(condition)) %>%
        distinct(technology, condition)
      if (is_neutral) {
        start_text <- paste("One change at a time, from the reference vignette",
                            "and an expert with", NEUTRAL_TEXT)
      } else if (model_has_experts) {
        start_text <- paste("One change at a time, from the reference vignette",
                            "and an expert with average characteristics")
      } else {
        start_text <- "One change at a time, from the reference vignette"
      }
      # the prices and degree days of the model at their means, with the
      # values of those in the rows
      mean_terms <- character(0)
      if (setup$prices) mean_terms <- c(mean_terms, "prices")
      if ("hdd_c" %in% results$specification$regressors) {
        mean_terms <- c(mean_terms, "degree days")
      }
      if (length(mean_terms) > 0) {
        start_text <- paste0(start_text, ", with ",
                             paste(mean_terms, collapse = " and "),
                             " at their means")
      }
      if (length(mean_items) > 0) {
        start_text <- paste0(start_text, " (",
                             paste(mean_items, collapse = "; "), ")")
      }
      # the changes of the terms (the slide version), then all changes with
      # the attributes
      term_items <- character(0)
      range_items <- character(0)
      if (any(outcome_terms$type == "price")) {
        range_items <- c(range_items, "a price")
      }
      if (any(outcome_terms$type == "hdd")) {
        range_items <- c(range_items, "degree days")
      }
      if (length(range_items) > 0) {
        term_items <- c(term_items,
                        paste(paste(range_items, collapse = " or "),
                              "from the 25th to the 75th percentile"))
      }
      if (length(switched_on) > 0 && is_neutral) {
        term_items <- c(term_items,
                        paste(paste(switched_on, collapse = " or "),
                              "switched on"))
      }
      if (length(switched_on) > 0 && !is_neutral) {
        term_items <- c(term_items,
                        paste(paste(switched_on, collapse = " or "),
                              "from no to yes, the other fuels and",
                              "technologies at their shares"))
      }
      # a heating that replaces another starts from an expert with that
      # other heating at home, not from the expert above
      for (k in seq_len(nrow(replacing_rows))) {
        technology_label <- TECHNOLOGY_LABELS[[replacing_rows$technology[k]]]
        replaced_label <- TECHNOLOGY_LABELS[[replacing_rows$condition[k]]]
        # tolower(): the labels inside a sentence
        technology_text <- tolower(technology_label)
        replaced_text <- tolower(replaced_label)
        term_items <- c(term_items,
                        paste0(technology_text, " instead of ", replaced_text,
                               " at home (from an expert with ", replaced_text,
                               " at home)"))
      }
      if (any(outcome_terms$type == "interaction")) {
        term_items <- c(term_items,
                        "A | B: attribute A where attribute B applies")
      }
      change_items <- c("an attribute of the reference vignette", term_items)
      # strwrap(): lines of at most 115 characters, which fit the width
      term_caption <- paste(strwrap(paste0(start_text, ": ",
                                           paste(change_items, collapse = "; "),
                                           ". 95% Krinsky-Robb intervals."),
                                    width = 115), collapse = "\n")
      slide_caption <- paste(strwrap(paste0(start_text, ": ",
                                            paste(term_items, collapse = "; "),
                                            ". 95% Krinsky-Robb intervals."),
                                     width = 115), collapse = "\n")

      # the legend centred on the whole figure, as in 5.3: the long labels of
      # the price rows push the panel to the right
      plot <- plot_estimates(
        plot_data, group_colours, group_shapes,
        title = paste0(outcome_label, ": the effects of all terms"),
        subtitle = term_subtitle,
        caption = term_caption,
        x_title = "Change in probability (percentage points)",
        base_size = FONT_SIZE, line_colour = ZEW_DARK
      ) +
        theme(legend.location = "plot")
      figure_file <- paste0("fig_rcl_", model_name, "_term_effects_",
                            file_name(outcome_label))
      save_figure_pdf(plot, file.path(output_dirs$figures,
                                      paste0(figure_file, ".pdf")),
                      FIGURE_SIZES$full)
      # The version for the main slides, with room for notes below it: the
      # changes of the terms only, as the attribute AMEs have their own slide
      # version (5.4)
      if (model_name == MAIN_MODEL && outcome_name == REFERENCE_OUTCOME) {
        plot <- plot_estimates(
          term_data, group_colours, group_shapes,
          title = paste0(outcome_label, ": the effects of all terms"),
          subtitle = term_subtitle,
          caption = slide_caption,
          x_title = "Change in probability (percentage points)",
          base_size = FONT_SIZE, line_colour = ZEW_DARK
        ) +
          theme(legend.location = "plot")
        save_figure_pdf(plot, file.path(output_dirs$figures,
                                        paste0(figure_file, "_slide.pdf")),
                        FIGURE_SIZES$short)
      }
    }
  }
}

# 6. Excel ---------------------------------------------------------------------
# Every quantity of every model, with labels; the numbers unrounded
# models: expert_state, the expert of each model's probabilities, AMEs and
# mean constants (section 2)
sheets <- list(
  models = MODELS %>%
    left_join(fit_all, by = "model", relationship = "one-to-one") %>%
    mutate(expert_state = expert_states[model]),
  lr_tests = lr %>%
    mutate(restricted = model_number[restricted],
           unrestricted = model_number[unrestricted]),
  mean_constants = constants_all %>%
    select(model, group_label, technology_label, estimate, se, z, p),
  sd_constants = sds_all %>%
    select(model, group_label, technology_label, estimate, se, ci_low_kr,
           ci_high_kr),
  correlations = correlations_all %>%
    filter(technology_row != technology_column) %>%
    mutate(group_label = label_of(group, GROUP_LABELS, "groups"),
           technology_row = label_of(technology_row, TECHNOLOGY_LABELS, "outcomes"),
           technology_column = label_of(technology_column, TECHNOLOGY_LABELS,
                                        "outcomes")) %>%
    select(model, group_label, technology_row, technology_column, estimate, se,
           ci_low_kr, ci_high_kr),
  attribute_effects = effects_all %>%
    select(model, group_label, attribute_label, technology_label, estimate, se,
           z, p),
  coefficients_as_estimated = coefficients_all %>%
    select(model, parameter, estimate, se, z, p),
  baseline = baseline_all %>%
    select(model, group_label, outcome_label, estimate, se, ci_low_kr,
           ci_high_kr),
  ame = ame_all %>%
    select(model, group_label, attribute_label, outcome_label, ame, se, z, p,
           ci_low, ci_high, ci_low_kr, ci_high_kr)
)
# none if every model has one profile, e.g. only (2)
if (nrow(differences_all) > 0) {
  sheets$group_differences <- differences_all %>%
    select(model, quantity, group_label, compared_with_label, attribute_label,
           outcome_label, difference, se, z, p, ci_low_kr, ci_high_kr,
           significant)
}
# none without a model with the price times the heat demand
if (nrow(price_demand_all) > 0) {
  sheets$price_by_heat_demand <- price_demand_all %>%
    select(model, demand, estimate, se, p, proportional, se_proportional,
           p_proportional)
  sheets$wald_tests <- wald_all %>%
    select(model, test, statistic, df, p)
}
# none without the results of 01_run_term_effects.R. base_state: where the
# model's changes start ("neutral": no expected price rise and no heat pump
# at home; "reference": the reference state of the AMEs, also
# for a file from before 30 October 2026, which has no base_state);
# is_affected: the change moves the outcome's own utility (a heating at home
# instead of another: the utilities of both; its p_before is with that other
# heating at home, not at the base state), TRUE for all outcomes with a
# regressor with one coefficient per technology (the degree days, the heat
# pump at home); value_before, value_after: the
# prices (cent per kWh) and degree days of the change
if (nrow(term_effects_all) > 0) {
  base_states <- tibble()
  for (model_name in names(term_effects_list)) {
    base_state <- "reference"
    if (!is.null(term_effects_list[[model_name]]$base_state)) {
      base_state <- term_effects_list[[model_name]]$base_state
    }
    base_states <- bind_rows(base_states,
                             tibble(model = model_name, base_state = base_state))
  }
  sheets$term_effects <- term_effects_all %>%
    left_join(base_states, by = "model", relationship = "many-to-one") %>%
    select(model, base_state, group_label, change_label, outcome_label,
           is_affected, value_before, value_after, p_before, p_after, effect,
           se, z, p, ci_low, ci_high, ci_low_kr, ci_high_kr)
}
# The probabilities where the changes of each model start (base_state:
# "neutral", no expected price rise and no heat pump at home, or
# "reference", the reference state of the AMEs); none in a file of
# 01_run_term_effects.R from before 30 October 2026
term_base_all <- tibble()
for (model_name in names(term_effects_list)) {
  term_effects <- term_effects_list[[model_name]]
  if (!is.null(term_effects$base_probabilities)) {
    term_base_all <- bind_rows(term_base_all,
                               term_effects$base_probabilities %>%
                                 mutate(model = model_name,
                                        base_state = term_effects$base_state))
  }
}
if (nrow(term_base_all) > 0) {
  sheets$term_effects_base <- term_base_all %>%
    mutate(group_label = label_of(profile, GROUP_LABELS, "groups"),
           outcome_label = label_of(outcome, TECHNOLOGY_LABELS, "outcomes")) %>%
    select(model, base_state, group_label, outcome_label, estimate, se,
           ci_low_kr, ci_high_kr)
}
write_xlsx(sheets, path = file.path(output_dirs$excel, "rc_logit_results.xlsx"))

# 7. Numbers for the text of the slides ----------------------------------------
# What the table notes leave out, for the slides that explain the data and
# the model: the sample, the draws, the reference vignette and the prices.
# Macro names: letters only (no digits in LaTeX command names).
numbers <- c(
  rclExperts = latex_number(n_experts, 0),
  rclDecisions = latex_number(n_choices, 0),
  rclDrawsModel = latex_number(first_results$draws$n_draws_model, 0),
  rclDrawsAme = latex_number(first_results$draws$n_draws_ame, 0),
  rclDrawsKrinskyRobb = latex_number(n_draws_kr, 0),
  rclReferenceVignette = reference_vignette_text
)
for (group_name in GROUP_DUMMIES) {
  # the label in words without spaces: rclExpertsEnergyAdvisers,
  # rclExpertsHvacInstallers, rclExpertsChimneySweeps
  label_words <- tools::toTitleCase(tolower(GROUP_LABELS[[group_name]]))
  macro <- paste0("rclExperts", gsub(" ", "", label_words))
  numbers[[macro]] <- latex_number(
    experts_by_group$experts[experts_by_group$berufsgruppe == group_name], 0)
}
# The mean reported prices (cent per kWh), at which the price terms are
# centred, and the settings that turn them into the heat pump's and the
# hybrid heating's price
if (length(price_models) > 0) {
  price_reference <- price_results$price_reference
  price_macros <- c(gas = "rclPriceGas", oil = "rclPriceOil",
                    pellet = "rclPricePellets", dh = "rclPriceDistrictHeating",
                    hpelec = "rclPriceElectricity")
  for (fuel in names(price_macros)) {
    numbers[[price_macros[[fuel]]]] <- latex_number(
      price_reference$mean[price_reference$fuel == fuel], 1)
  }
  cop <- price_results$price_settings$cop
  numbers[["rclCop"]] <- latex_number(cop[["radiators"]], 1)
  if (cop[["floor_heating"]] != cop[["radiators"]]) {
    numbers[["rclCop"]] <- paste0(latex_number(cop[["radiators"]], 1),
                                  " (radiators) and ",
                                  latex_number(cop[["floor_heating"]], 1),
                                  " (underfloor heating)")
  }
  numbers[["rclHybridGasShare"]] <- latex_number(
    price_results$price_settings$hybrid_gas_share, 2)
}
write_tex(numbers_tex(numbers), file.path(output_dirs$tables, "numbers_rcl.tex"))

cat("Output written to", run_dir, "\n")
