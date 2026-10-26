################################################################################
# 02_output.R — tables, figures, tests and an Excel workbook from the saved
#               results of the mixed logit models (01_run_specifications.R)
# Inputs : RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/<model>.rds,
#          aggregates only (the fitted models are not read)
# Outputs: in RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/
#            tables/   tab_rcl_*.tex, complete LaTeX table floats, and
#                      numbers_rcl.tex, sample sizes and draws for the text
#            figures/  fig_rcl_*.pdf
#            excel/    rc_logit_results.xlsx
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
#                                    price coefficients
#   section 5, each model            the constants (mean and standard
#                                    deviation by group), the attribute
#                                    effects (the regression table), the
#                                    probabilities at the reference vignette,
#                                    the AMEs, the correlations of the
#                                    constants
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
  "m6",   "(6)",   "Group means and covariances",
  "m4i",  "(4i)",  "Group means, group × attributes",
  "m6i",  "(6i)",  "Group means and covariances, group × attributes",
  "m4p",  "(4p)",  "Group means, prices",
  "m6p",  "(6p)",  "Group means and covariances, prices",
  "m4ip", "(4ip)", "Group means, group × attributes, prices"
)
MAIN_MODEL <- "m4"

# Figures across models: the main model and one alternative per dimension,
# (2) without Berufsgruppe, (6) covariances by group, (4i) interactions, (4p)
# prices. More than about five models no longer fit a slide.
COMPARISON_MODELS <- c("m2", "m4", "m6", "m4i", "m4p")

# Tables of the attribute effects across models, one per technology: the
# models whose attribute effects are the same for all groups (those with
# interactions have a table per group in section 5)
COEFFICIENT_COMPARISON_MODELS <- c("m2", "m4", "m6", "m4p", "m6p")

# Likelihood-ratio tests between nested models, classical and robust
# (lr_tests()). `restricted` is a special case of `unrestricted`;
# `restrictions` says how (types in lr_restrictions()). The tests of one
# question form a panel of the table, in this order.
LR_TESTS <- tribble(
  ~question,                                   ~restricted, ~unrestricted, ~restrictions,
  "Do the mean constants differ by group?",     "m2",        "m4",          "regressors",
  "Do the covariances differ by group?",        "m4",        "m6",          "group_covariances",
  "Do the covariances differ by group?",        "m4i",       "m6i",         "group_covariances",
  "Do the covariances differ by group?",        "m4p",       "m6p",         "group_covariances",
  "Do the attribute effects differ by group?",  "m4",        "m4i",         "regressors",
  "Do the attribute effects differ by group?",  "m6",        "m6i",         "regressors",
  "Do the attribute effects differ by group?",  "m4p",       "m4ip",        "regressors",
  "Do the prices matter?",                      "m4",        "m4p",         "prices",
  "Do the prices matter?",                      "m6",        "m6p",         "prices",
  "Do the prices matter?",                      "m4i",       "m4ip",        "prices"
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

# The price terms: the price, then the dummies for a price not reported
PRICE_LABELS <- c(
  price = "Price (cent per kWh)",
  hpelec_price_na = "Heat pump (electricity)",
  pellet_price_na = "Pellet heating",
  hybrid_price_na = "Hybrid heating",
  gas_price_na = "Gas heating",
  oil_price_na = "Oil heating",
  dh_price_na = "District heating"
)

# Colours and symbols. The groups: Okabe-Ito, as in the policy brief. The
# models: the other Okabe-Ito colours, black for the main model. Symbols with
# a fill (21 to 25), so that the figures can leave a symbol open.
GROUP_COLOURS <- c(
  "Energy advisers" = "#0072B2",
  "HVAC installers" = "#D55E00",
  "Chimney sweeps" = "#009E73",
  "All experts" = "grey35"
)
GROUP_SHAPES <- c(
  "Energy advisers" = 21,
  "HVAC installers" = 24,
  "Chimney sweeps" = 22,
  "All experts" = 23
)
MODEL_COLOURS <- c(m2 = "grey55", m4 = "black", m6 = "#E69F00",
                   m4i = "#56B4E9", m6i = "#F0E442", m4p = "#CC79A7",
                   m6p = "#999933", m4ip = "#882255")
MODEL_SHAPES <- c(m2 = 23, m4 = 21, m6 = 22, m4i = 24, m6i = 25, m4p = 21,
                  m6p = 22, m4ip = 24)

# Sizes for the 16:9 slides of the economics-latex layout (text width 5.5
# in): a figure across a frame, and a heat map with room for text beside it.
# The figures keep their titles inside (profile), so they are taller than the
# skill's 2.3 in.
FONT_SIZE <- 9
FIGURE_SIZES <- list(
  full = c(width = 5.5, height = 2.8),
  heatmap = c(width = 3.2, height = 2.8)
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
if (!(MAIN_MODEL %in% MODELS$model)) {
  stop("No results for the main specification ", MAIN_MODEL, " (MAIN_MODEL)")
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
  if (results$fit$n_choices != first_results$fit$n_choices ||
      results$fit$n_experts != first_results$fit$n_experts) {
    stop("The results come from different samples: rerun ",
         "01_run_specifications.R for all models")
  }
  # The notes and figures compare the groups with REFERENCE_GROUP, so it must
  # be the reference group of the Berufsgruppe shifts and interactions
  reference_dummy <- model_structure(results$specification,
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
  constants_all <- bind_rows(constants_all,
                             mean_constants(results, GROUP_DUMMIES) %>%
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
  setup <- model_structure(results$specification, GROUP_DUMMIES)
  fit_all <- bind_rows(fit_all, results$fit %>%
                         mutate(model = model_name,
                                group_means = setup$group_constants |
                                  length(setup$group_shifts) > 0,
                                group_covariances = setup$group_constants,
                                interactions = setup$interactions,
                                prices = setup$prices))
}
print(fit_all %>%
        select(model, n_parameters, log_likelihood, aic, bic, mcfadden_r2))

aic_text <- latex_number(fit_all$aic, 1)
bic_text <- latex_number(fit_all$bic, 1)
is_lowest_aic <- fit_all$aic == min(fit_all$aic)
is_lowest_bic <- fit_all$bic == min(fit_all$bic)
aic_text[is_lowest_aic] <- paste0("\\textbf{", aic_text[is_lowest_aic], "}")
bic_text[is_lowest_bic] <- paste0("\\textbf{", bic_text[is_lowest_bic], "}")
fit_body <- paste0(
  model_number[fit_all$model], " & ",
  if_else(fit_all$group_means, "Yes", "No"), " & ",
  if_else(fit_all$group_covariances, "Yes", "No"), " & ",
  if_else(fit_all$interactions, "Yes", "No"), " & ",
  if_else(fit_all$prices, "Yes", "No"), " & ",
  latex_number(fit_all$n_parameters, 0), " & ",
  latex_number(fit_all$log_likelihood, 1), " & ",
  aic_text, " & ", bic_text, " & ",
  latex_number(fit_all$mcfadden_r2, 3), " \\\\"
)
fit_header <- paste0(
  "Model & ", header_cell("Group\\\\means"), " & ",
  header_cell("Group\\\\covariances"), " & ",
  header_cell("Group $\\times$\\\\attributes"), " & Prices & ",
  header_cell("Para-\\\\meters"), " & ", header_cell("Log-\\\\likelihood"),
  " & AIC & BIC & ", header_cell("McFadden\\\\R$^2$"), " \\\\"
)
fit_notes <- paste0(
  "Mixed logits with constants normal and correlated across experts; ",
  "``Yes'': this part differs by occupational group, or the model has the ",
  "prices. ", model_number[[MAIN_MODEL]], " is the main specification. ",
  "Lowest AIC and BIC in bold; BIC with $N$ = experts. ", sample_note
)
write_tex(table_float("lccccrrrrr", fit_header, fit_body,
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

# One block of rows per question, the question in its first row
lr_body <- character(0)
for (question_text in unique(lr$question)) {
  question_tests <- lr %>%
    filter(question == question_text)
  question_cells <- c(question_text, rep("", nrow(question_tests) - 1))
  if (length(lr_body) > 0) lr_body <- c(lr_body, "\\addlinespace")
  lr_body <- c(lr_body, paste0(
    question_cells, " & ", model_number[question_tests$restricted], " vs ",
    model_number[question_tests$unrestricted], " & ",
    latex_number(question_tests$df, 0), " & ",
    latex_number(question_tests$lr, 1), " & ",
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
write_tex(table_float("llrrrr", lr_header, lr_body,
                      "Likelihood-Ratio Tests between Nested Models",
                      paste0(label_prefix, "lr_tests"), lr_notes,
                      TABLE_FONT_SIZE),
          file.path(output_dirs$tables, "tab_rcl_lr_tests.tex"))

# 4.3 Attribute effects across models -----------------------------------------
# One table per technology. Only models whose attribute effects are the same
# for all groups fit one column.
for (model_name in COEFFICIENT_COMPARISON_MODELS) {
  if (model_structure(results_list[[model_name]]$specification,
                      GROUP_DUMMIES)$interactions) {
    stop("COEFFICIENT_COMPARISON_MODELS: ", model_name, " has attribute ",
         "effects by group, which do not fit one column")
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
# prices (checked), which the notes describe.
price_models <- character(0)
for (model_name in MODELS$model) {
  if (length(results_list[[model_name]]$specification$prices) > 0) {
    price_models <- c(price_models, model_name)
  }
}
if (length(price_models) > 0) {
  price_results <- results_list[[price_models[1]]]
  for (model_name in price_models) {
    stopifnot(identical(results_list[[model_name]]$price_reference,
                        price_results$price_reference),
              identical(results_list[[model_name]]$price_settings,
                        price_results$price_settings))
  }
  # label_of() stops if a price term has no label
  price_terms <- price_results$specification$prices
  label_of(price_terms, PRICE_LABELS, "price terms")
  # the price first, then the dummies for a price not reported under a
  # heading, in the order of PRICE_LABELS
  dummy_terms <- intersect(names(PRICE_LABELS), setdiff(price_terms, "price"))
  price_body <- character(0)
  for (term in c("price", dummy_terms)) {
    cells <- tibble()
    for (model_name in price_models) {
      is_row <- coefficients_all$model == model_name &
        coefficients_all$parameter == term
      if (sum(is_row) != 1) stop("No coefficient ", term, " in ", model_name)
      cells <- bind_rows(cells, coefficients_all[is_row, ])
    }
    if (term == dummy_terms[1]) {
      price_body <- c(price_body, panel_row("Price not reported",
                                            length(price_models) + 1))
    }
    row_label <- latex_text(PRICE_LABELS[[term]])
    if (term != "price") row_label <- paste0("\\quad ", row_label)
    price_body <- c(price_body, estimate_rows(row_label, cells$estimate,
                                              cells$se, cells$p, digits = 3))
  }

  # The mean prices, the COP and the weight of gas in the hybrid heating's
  # price are macros for the slides (section 7)
  price_notes <- paste0(
    "Own price per kWh, centred at the mean over the experts of both arms; ",
    "one coefficient for all technologies. ", se_note, " ", star_note
  )
  price_header <- paste0(" & ", paste(model_number[price_models], collapse = " & "),
                         " \\\\")
  write_tex(table_float(paste0("l", strrep("c", length(price_models))),
                        price_header, price_body, "Price Coefficients",
                        paste0(label_prefix, "prices"), price_notes,
                        TABLE_FONT_SIZE),
            file.path(output_dirs$tables, "tab_rcl_prices.tex"))
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
  x_title = "Probability (%)", base_size = FONT_SIZE, legend_rows = 3
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
      legend_rows = 3
    )
    save_figure_pdf(plot, file.path(output_dirs$figures,
                                    paste0("fig_rcl_comparison_ame_",
                                           file_name(outcome_label), "_",
                                           file_name(GROUP_LABELS[[group_name]]),
                                           ".pdf")),
                    FIGURE_SIZES$full)
  }
}

# 5. Each model ----------------------------------------------------------------
for (model_name in MODELS$model) {
  results <- results_list[[model_name]]
  setup <- model_structure(results$specification, GROUP_DUMMIES)
  number <- model_number[[model_name]]
  # The model in the subtitles of the figures and the notes of the tables
  model_title <- paste0("Model ", model_legend[[model_name]])
  model_note <- paste0("Model ", number, ": ",
                       latex_text(results$specification$label), ".")
  # The probabilities and constants of a model with prices are those of an
  # expert who reported the mean prices
  prices_text <- ""
  prices_figure_text <- ""
  if (setup$prices) {
    prices_text <- ", at the mean prices"
    prices_figure_text <- "; at the mean prices"
  }

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
    # are in the table of the price coefficients (4.4): a row of them would
    # not fit the frame, and the heat pump's has no column.
    prices_note <- ""
    if (setup$prices) {
      is_price <- coefficients_all$model == model_name &
        coefficients_all$parameter == "price"
      if (sum(is_price) != 1) stop("No price coefficient in ", model_name)
      # no rule above it, so that the table still fits a frame: the cell
      # across the technologies sets it apart
      body <- c(body,
                spanning_row(latex_text(PRICE_LABELS[["price"]]),
                             coefficients_all$estimate[is_price],
                             coefficients_all$se[is_price],
                             coefficients_all$p[is_price],
                             length(technologies), digits = 3))
      prices_note <- "With dummies for a price not reported. "
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
    notes <- paste0(group_text, prices_note, se_note, " ", star_note)
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
    subtitle = paste0(model_title, prices_figure_text, "\nProbability of each ",
                      "recommendation at the reference vignette"),
    caption = "95% Krinsky-Robb intervals.",
    x_title = "Probability (%)", base_size = FONT_SIZE
  )
  save_figure_pdf(plot, file.path(output_dirs$figures,
                                  paste0("fig_rcl_", model_name, "_baseline.pdf")),
                  FIGURE_SIZES$full)

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
      subtitle = paste0(model_title, prices_figure_text,
                        "\nBaseline probability: ",
                        paste(baseline_text, collapse = ", ")),
      caption = paste0("Change in the probability when one attribute of the ",
                       "reference vignette changes; 95% Krinsky-Robb intervals."),
      x_title = "Change in probability (percentage points)", base_size = FONT_SIZE
    )
    save_figure_pdf(plot, file.path(output_dirs$figures,
                                    paste0("fig_rcl_", model_name, "_ame_",
                                           file_name(outcome_label), ".pdf")),
                    FIGURE_SIZES$full)
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
      base_size = FONT_SIZE, text_size = HEATMAP_TEXT_SIZE
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
}

# 6. Excel ---------------------------------------------------------------------
# Every quantity of every model, with labels; the numbers unrounded
sheets <- list(
  models = MODELS %>%
    left_join(fit_all, by = "model", relationship = "one-to-one"),
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
