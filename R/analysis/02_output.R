################################################################################
# 02_output.R — tables, figures and an Excel workbook of the mixed logit
#               specifications estimated by 01_run_specifications.R
# Inputs : RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/*.rds, the
#          aggregate results (the fitted models and the data are not loaded)
# Outputs: in the same folder rc_logit_<arm>_ref_<outcome>/:
#            tables/   tab_<specification>.tex: one table per mixed logit
#                      tab_<comparison>.tex: specifications side by side
#            figures/  fig_ame_<specification>_<outcome>.pdf: AMEs by profile
#                      fig_ame_<comparison>_<profile>_<outcome>.pdf: AMEs by
#                      specification
#                      fig_correlation_<specification>[_<block>].pdf
#            excel/    rc_logit_results.xlsx
#            tables/slides/, figures/slides/: slide versions of the tables
#                      and figures, the frames of the appendix of the slides
#                      (frames_*.tex) and numbers for their text
#                      (numbers_slides.tex), for slides/ in the same folder
# Checks : every value has a label; all results come from one arm, reference
#          outcome and sample, and from the current version of
#          01_run_specifications.R; LR tests only between results on that
#          sample with the same draws; the slides quote the numbers of draws
#          only if all specifications share them
################################################################################
# All outputs are in English; every label is set in section 1. Numbers have a
# decimal point (0.123) and a thousands comma (2,400). The LaTeX tables need
# \usepackage[T1]{fontenc}, \usepackage{booktabs} and
# \usepackage{threeparttable}. The functions are in 00_functions_output.R.

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

# config.R: RC_LOGIT_DIR, packages
source(file.path("R", "prepare_analysis_data.R"))
# run_folder(), lr_tests()
source(file.path("R", "analysis", "00_functions_estimation.R"))
source(file.path("R", "analysis", "00_functions_output.R"))

# 1. Settings ------------------------------------------------------------------

# The run of 01_run_specifications.R to report on
SELECTED_FW_ARM <- "fw"
REFERENCE_OUTCOME <- "Wärmepumpe"
if (exists("RUN_SETTINGS", inherits = FALSE)) {
  SELECTED_FW_ARM <- RUN_SETTINGS$arm
  REFERENCE_OUTCOME <- RUN_SETTINGS$reference
}

# The specifications to report, in the order of the table columns and figure
# legends, with their number in tables and figures. Those without results are
# skipped with a message.
SPECIFICATION_NUMBERS <- c(
  asc_uncorrelated = "(1)",
  asc_correlated = "(2)",
  asc_uncorrelated_berufsgruppe = "(3)",
  asc_correlated_berufsgruppe = "(4)",
  asc_by_group_uncorrelated = "(5)",
  asc_by_group_correlated = "(6)",
  asc_uncorrelated_berufsgruppe_interactions = "(3i)",
  asc_correlated_berufsgruppe_interactions = "(4i)",
  asc_by_group_uncorrelated_interactions = "(5i)",
  asc_by_group_correlated_interactions = "(6i)",
  asc_uncorrelated_prices = "(1p)",
  asc_correlated_prices = "(2p)",
  asc_uncorrelated_berufsgruppe_prices = "(3p)",
  asc_correlated_berufsgruppe_prices = "(4p)",
  asc_by_group_uncorrelated_prices = "(5p)",
  asc_by_group_correlated_prices = "(6p)",
  asc_uncorrelated_berufsgruppe_prices_by_group = "(3pg)",
  asc_correlated_berufsgruppe_prices_by_group = "(4pg)",
  asc_by_group_uncorrelated_prices_by_group = "(5pg)",
  asc_by_group_correlated_prices_by_group = "(6pg)"
)

# Likelihood-ratio tests, classical and robust (lr_tests()). In each pair, the
# restricted specification must be a special case of the unrestricted one;
# `restrictions` says how, in the unrestricted specification's coefficients
# (types in lr_restrictions(), several joined by " + "; together they must
# account for the difference in parameters). Each test takes two lines: the
# restricted specification, then the unrestricted one and the restrictions.
# What each test asks:
LR_TEST_PAIRS <- tribble(
  ~restricted,
  ~unrestricted,                                 ~restrictions,
  # are the constants correlated?
  "asc_uncorrelated",
  "asc_correlated",                              "correlations",
  # do the means of the constants differ by Berufsgruppe?
  "asc_uncorrelated",
  "asc_uncorrelated_berufsgruppe",               "regressors",
  "asc_correlated",
  "asc_correlated_berufsgruppe",                 "regressors",
  # ... and their standard deviations?
  "asc_uncorrelated_berufsgruppe",
  "asc_by_group_uncorrelated",                   "group_covariances",
  # ... and their covariance matrices?
  "asc_correlated_berufsgruppe",
  "asc_by_group_correlated",                     "group_covariances",
  # are the constants correlated within the Berufsgruppen?
  "asc_by_group_uncorrelated",
  "asc_by_group_correlated",                     "correlations",
  # do the effects of the attributes differ by Berufsgruppe?
  "asc_uncorrelated_berufsgruppe",
  "asc_uncorrelated_berufsgruppe_interactions",  "regressors",
  "asc_correlated_berufsgruppe",
  "asc_correlated_berufsgruppe_interactions",    "regressors",
  "asc_by_group_uncorrelated",
  "asc_by_group_uncorrelated_interactions",      "regressors",
  "asc_by_group_correlated",
  "asc_by_group_correlated_interactions",        "regressors",
  # the tests of the constants, with the interactions
  "asc_uncorrelated_berufsgruppe_interactions",
  "asc_by_group_uncorrelated_interactions",      "group_covariances",
  "asc_correlated_berufsgruppe_interactions",
  "asc_by_group_correlated_interactions",        "group_covariances",
  "asc_by_group_uncorrelated_interactions",
  "asc_by_group_correlated_interactions",        "correlations",
  # do the prices matter?
  "asc_uncorrelated",
  "asc_uncorrelated_prices",                     "prices",
  "asc_correlated",
  "asc_correlated_prices",                       "prices",
  "asc_uncorrelated_berufsgruppe",
  "asc_uncorrelated_berufsgruppe_prices",        "prices",
  "asc_correlated_berufsgruppe",
  "asc_correlated_berufsgruppe_prices",          "prices",
  "asc_by_group_uncorrelated",
  "asc_by_group_uncorrelated_prices",            "prices",
  "asc_by_group_correlated",
  "asc_by_group_correlated_prices",              "prices",
  # does the price coefficient differ by Berufsgruppe?
  "asc_uncorrelated_berufsgruppe_prices",
  "asc_uncorrelated_berufsgruppe_prices_by_group", "price_groups",
  "asc_correlated_berufsgruppe_prices",
  "asc_correlated_berufsgruppe_prices_by_group", "price_groups",
  "asc_by_group_uncorrelated_prices",
  "asc_by_group_uncorrelated_prices_by_group",   "price_groups",
  "asc_by_group_correlated_prices",
  "asc_by_group_correlated_prices_by_group",     "price_groups"
)

# Comparisons: the specifications side by side in one table, and in one AME
# figure per profile and outcome. A table lists the LR tests that involve at
# least one of its specifications. At most six specifications per comparison
# (the colours of the figures).
COMPARISONS <- list(
  comparison = c("asc_uncorrelated", "asc_correlated",
                 "asc_uncorrelated_berufsgruppe", "asc_correlated_berufsgruppe",
                 "asc_by_group_uncorrelated", "asc_by_group_correlated"),
  comparison_interactions = c("asc_uncorrelated_berufsgruppe_interactions",
                              "asc_correlated_berufsgruppe_interactions",
                              "asc_by_group_uncorrelated_interactions",
                              "asc_by_group_correlated_interactions"),
  comparison_prices = c("asc_uncorrelated_prices", "asc_correlated_prices",
                        "asc_uncorrelated_berufsgruppe_prices",
                        "asc_correlated_berufsgruppe_prices",
                        "asc_by_group_uncorrelated_prices",
                        "asc_by_group_correlated_prices"),
  comparison_prices_by_group = c("asc_uncorrelated_berufsgruppe_prices_by_group",
                                 "asc_correlated_berufsgruppe_prices_by_group",
                                 "asc_by_group_uncorrelated_prices_by_group",
                                 "asc_by_group_correlated_prices_by_group")
)

# Slides: the price coefficients side by side, one row per specification
# without prices: its variant with one price coefficient, and the one with a
# price coefficient per occupational group (NA: none)
PRICE_COMPARISON <- tribble(
  ~prices,                                ~prices_by_group,
  "asc_uncorrelated_prices",              NA_character_,
  "asc_correlated_prices",                NA_character_,
  "asc_uncorrelated_berufsgruppe_prices", "asc_uncorrelated_berufsgruppe_prices_by_group",
  "asc_correlated_berufsgruppe_prices",   "asc_correlated_berufsgruppe_prices_by_group",
  "asc_by_group_uncorrelated_prices",     "asc_by_group_uncorrelated_prices_by_group",
  "asc_by_group_correlated_prices",       "asc_by_group_correlated_prices_by_group"
)

# Labels, in plain text; latex_text() writes them as LaTeX in the tables. The
# names are the values as they appear in the results.
MODEL_LABELS <- c(
  mnl = "Multinomial logit",
  mixed = "Mixed logit"
)

# Outcomes in figure titles, table rows and Excel; the order of the names is
# the order of the rows and columns of the heat maps
TECHNOLOGY_LABELS <- c(
  "Wärmepumpe" = "Heat pump",
  "Gasheizung" = "Gas heating",
  "Pelletheizung" = "Pellet heating",
  "Hybridheizung" = "Hybrid heating",
  "Ölheizung" = "Oil heating",
  "Fernwärme" = "District heating",
  "Keine Empfehlung" = "No recommendation"
)

# Table column headers over two lines (\\ is a line break in LaTeX), so that
# up to six columns fit the text width
TECHNOLOGY_COLUMN_HEADERS <- c(
  "Keine Empfehlung" = "No recom-\\\\mendation",
  "Wärmepumpe" = "Heat\\\\pump",
  "Pelletheizung" = "Pellet\\\\heating",
  "Hybridheizung" = "Hybrid\\\\heating",
  "Gasheizung" = "Gas\\\\heating",
  "Ölheizung" = "Oil\\\\heating",
  "Fernwärme" = "District\\\\heating"
)

# Table rows and Excel: the columns of the design matrices of the variables,
# named as in colnames(model.matrix()). A column of an interaction gets the
# labels of its parts, e.g. "Chimney sweep × Income below €40,000".
TERM_LABELS <- c(
  "(Intercept)" = "Constant",
  "is_shk" = "Constant, HVAC installers",
  "is_schornsteinfeger" = "Constant, chimney sweeps",
  "is_energieberater" = "Constant, energy advisers",
  "berufsgruppeSchornsteinfeger" = "Chimney sweep",
  "berufsgruppeEnergieberater" = "Energy adviser",
  "att_couple_ageMitte 60" = "Couple in their mid-60s",
  "att_incomeunter 40.000 € brutto im Jahr" = "Income below €40,000",
  "att_current_heatingÖlheizung" = "Current heating: oil",
  "att_replacement_timingist defekt und muss kurzfristig ersetzt werden" =
    "Heating broken, urgent replacement",
  "att_build_year2000" = "Built in 2000",
  "att_heat_demand70" = "Heat demand 70 kWh/m²",
  "att_heat_demand200" = "Heat demand 200 kWh/m²",
  "att_heat_distributioneine Fußbodenheizung" = "Underfloor heating"
)

# Figure rows and Excel: the variations of the reference vignette, named as in
# ame_vignettes(). Like the umlauts, € and ² need the file to be read as UTF-8.
VARIATION_LABELS <- c(
  "att_couple_age: Mitte 60" = "Couple in their mid-60s (instead of mid-40s)",
  "att_income: unter 40.000 € brutto im Jahr" =
    "Income below €40,000 (instead of €65,000)",
  "att_current_heating: Ölheizung" = "Current heating: oil (instead of gas)",
  "att_replacement_timing: ist defekt und muss kurzfristig ersetzt werden" =
    "Heating broken (instead of replacement in the next few years)",
  "att_build_year: 2000" = "Built in 2000 (instead of 1970)",
  "att_heat_demand: 70" = "Heat demand 70 kWh/m² (instead of 120)",
  "att_heat_demand: 200" = "Heat demand 200 kWh/m² (instead of 120)",
  "att_heat_distribution: eine Fußbodenheizung" =
    "Underfloor heating (instead of radiators)"
)

# The profiles of the AMEs (AME_PROFILES in 01_run_specifications.R)
PROFILE_LABELS <- c(
  "All experts" = "All experts",
  "SHK-Handwerk" = "HVAC installers",
  "Schornsteinfeger" = "Chimney sweeps",
  "Energieberater" = "Energy advisers"
)

# The groups of the group constants (the dummies of 01_run_specifications.R),
# for the titles of the slides
GROUP_LABELS <- c(
  is_shk = "HVAC installers",
  is_schornsteinfeger = "Chimney sweeps",
  is_energieberater = "Energy advisers"
)

# Regressors other than the vignette attributes, for the comparison table and
# the notes; an interaction with an attribute reads "<label> × attributes".
# "prices" stands for the prices of a specification, "prices_by_group" for
# prices with one coefficient per occupational group.
VARIABLE_LABELS <- c(
  berufsgruppe = "Occupational group",
  prices = "Energy prices",
  prices_by_group = "Energy prices by occupational group"
)

# The price terms by their kind (column kind of PRICE_TERMS in
# 01_run_specifications.R), and the row of a price with one coefficient for
# all technologies: tables and Excel. Per kWh of fuel or of heat, depending on
# the technology: the table notes say which.
PRICE_LABELS <- c(
  price = "Price per kWh (cent)",
  not_reported = "Price not reported",
  all_technologies = "All technologies"
)

DISTRIBUTION_LABELS <- c(
  n = "normal",
  ln = "log-normal",
  cn = "censored normal",
  u = "uniform",
  t = "triangular"
)

labels <- list(
  models = MODEL_LABELS,
  technologies = TECHNOLOGY_LABELS,
  technology_headers = TECHNOLOGY_COLUMN_HEADERS,
  terms = TERM_LABELS,
  variations = VARIATION_LABELS,
  profiles = PROFILE_LABELS,
  groups = GROUP_LABELS,
  variables = VARIABLE_LABELS,
  prices = PRICE_LABELS,
  distributions = DISTRIBUTION_LABELS,
  group_name = "occupational group"
)

# Notes. The sample is that of 01_run_specifications.R, section 2.2; the
# reference vignette is LaTeX.
arm_text <- "recommend district heating."
if (SELECTED_FW_ARM != "fw") arm_text <- "not recommend district heating."
sample_text <- paste(
  "Sample: experts who completed the questionnaire, belong to one of the",
  "three occupational groups and could", arm_text
)
reference_note <- paste(
  "Reference vignette: a couple in their mid-40s with a gross income of",
  "\\texteuro{}65,000 a year and a gas heating system that must be replaced",
  "in the next few years, in a house built in 1970 with a heat demand of",
  "120 kWh/m$^2$ and radiators."
)
# Further notes for single tables, by specification
reference_group_note <- "Reference category of the occupational group: HVAC installers."
TABLE_NOTES <- c(
  asc_uncorrelated_berufsgruppe = reference_group_note,
  asc_correlated_berufsgruppe = reference_group_note,
  asc_uncorrelated_berufsgruppe_interactions = reference_group_note,
  asc_correlated_berufsgruppe_interactions = reference_group_note,
  asc_uncorrelated_berufsgruppe_prices = reference_group_note,
  asc_correlated_berufsgruppe_prices = reference_group_note,
  asc_uncorrelated_berufsgruppe_prices_by_group = reference_group_note,
  asc_correlated_berufsgruppe_prices_by_group = reference_group_note
)
# The slide tables show the interactions, also those of (5i) and (6i), whose
# paper tables leave them out: their notes need the reference group as well
SLIDE_TABLE_NOTES <- c(
  TABLE_NOTES,
  asc_by_group_uncorrelated_interactions = reference_group_note,
  asc_by_group_correlated_interactions = reference_group_note
)

# Tables without the correlations: with one block of correlations per
# Berufsgruppe, the table runs over a page. The correlations are in the heat
# maps and in Excel.
TABLES_WITHOUT_CORRELATIONS <- c("asc_by_group_correlated",
                                 "asc_by_group_correlated_interactions",
                                 "asc_by_group_correlated_prices",
                                 "asc_by_group_correlated_prices_by_group")

# The coefficient tables leave out the rows of the interactions (regressors
# "a:b"): the 16 interactions of the Berufsgruppe with the attributes would
# run a table over a page. They are in Excel, and the AMEs by Berufsgruppe
# show their effect. FALSE shows them.
HIDE_INTERACTIONS <- TRUE

# Slides: the slide versions are for the 16:9 slides of the economics-latex
# layout (text width 5.5 in). Their figures have a smaller font and shorter
# labels of the variations than the paper figures, and no title (the frame
# title says it). Sizes in inches: an AME figure across the frame, with up to
# three lines of notes below it; a heat map next to text. They are saved with
# cairo_pdf (R's PDF device through the Cairo library), which embeds the fonts
# and prints a hyphen as a hyphen; R's default pdf() prints it as a minus
# sign ("mid−60s").
SLIDE_FONT_SIZE <- 9
SLIDE_FIGURE_SIZES <- list(
  ame = c(width = 5.5, height = 2.4),
  heatmap = c(width = 2.7, height = 2.55)
)
# Size of the numbers in the tiles of a slide heat map (mm, as in
# geom_text()): small enough that "-0.21" fits a tile of a 6 x 6 block
SLIDE_HEATMAP_TEXT_SIZE <- 2.1
SLIDE_VARIATION_LABELS <- c(
  "att_couple_age: Mitte 60" = "Couple in their mid-60s",
  "att_income: unter 40.000 € brutto im Jahr" = "Income below €40,000",
  "att_current_heating: Ölheizung" = "Current heating: oil",
  "att_replacement_timing: ist defekt und muss kurzfristig ersetzt werden" =
    "Heating broken",
  "att_build_year: 2000" = "Built in 2000",
  "att_heat_demand: 70" = "Heat demand 70 kWh/m²",
  "att_heat_demand: 200" = "Heat demand 200 kWh/m²",
  "att_heat_distribution: eine Fußbodenheizung" = "Underfloor heating"
)

# 2. Results -------------------------------------------------------------------
run_dir <- file.path(RC_LOGIT_DIR,
                     run_folder(SELECTED_FW_ARM, REFERENCE_OUTCOME))
results_dir <- file.path(run_dir, "results")

results_list <- list()
for (spec_name in names(SPECIFICATION_NUMBERS)) {
  results_file <- file.path(results_dir, paste0(spec_name, ".rds"))
  if (file.exists(results_file)) {
    results_list[[spec_name]] <- readRDS(results_file)
  } else {
    message("No results for ", spec_name, ": run 01_run_specifications.R first")
  }
}
if (length(results_list) == 0) stop("No results in ", results_dir)

# Results of a specification without a number in SPECIFICATION_NUMBERS are
# not reported: say which. list.files(): the files in a folder whose names
# end in .rds.
results_files <- list.files(results_dir, pattern = "\\.rds$")
unlisted <- setdiff(sub("\\.rds$", "", results_files), names(SPECIFICATION_NUMBERS))
if (length(unlisted) > 0) {
  message("Not reported, as they have no number in SPECIFICATION_NUMBERS: ",
          paste(unlisted, collapse = ", "))
}

# All results must come from the current version of 01_run_specifications.R
# (which saves the random coefficients) and from the same arm, reference
# outcome and sample, so that they can be compared. The date and draws of each
# are printed, so that results from an older run stand out.
fit_all <- tibble()
for (spec_name in names(results_list)) {
  results <- results_list[[spec_name]]
  if (is.null(results$random_coefficients)) {
    stop("The results of ", spec_name, " come from an older version of ",
         "01_run_specifications.R: rerun it")
  }
  stopifnot(results$arm == SELECTED_FW_ARM,
            results$reference_outcome == REFERENCE_OUTCOME)
  cat(SPECIFICATION_NUMBERS[[spec_name]], spec_name, ": estimated",
      as.character(results$date), "| draws: model", results$draws$n_draws_model,
      ", AMEs", results$draws$n_draws_ame, ", Krinsky-Robb",
      results$draws$n_draws_kr, "\n")
  fit_all <- bind_rows(fit_all, results$fit_statistics %>%
                         mutate(specification = spec_name))
}
if (n_distinct(fit_all$n_choices) != 1 || n_distinct(fit_all$n_experts) != 1) {
  stop("The results come from different samples: rerun 01_run_specifications.R ",
       "for all specifications")
}

output_dirs <- list(
  tables = file.path(run_dir, "tables"),
  figures = file.path(run_dir, "figures"),
  excel = file.path(run_dir, "excel"),
  slide_tables = file.path(run_dir, "tables", "slides"),
  slide_figures = file.path(run_dir, "figures", "slides")
)
for (folder in output_dirs) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}
reference_label <- TECHNOLOGY_LABELS[[REFERENCE_OUTCOME]]

# The frames of the appendix of the slides, filled in sections 4 to 6 and
# written in section 7
table_frames <- character(0)
ame_group_frames <- character(0)
ame_comparison_frames <- character(0)
heatmap_frames <- character(0)

# Likelihood-ratio tests between nested specifications, classical and robust
lr <- lr_tests(results_list, LR_TEST_PAIRS)
print(lr)

# 3. Excel ---------------------------------------------------------------------
write_xlsx(
  results_excel_sheets(results_list, lr, labels, SPECIFICATION_NUMBERS),
  path = file.path(output_dirs$excel, "rc_logit_results.xlsx")
)

# 4. LaTeX tables --------------------------------------------------------------
for (spec_name in names(results_list)) {
  results <- results_list[[spec_name]]
  extra_note <- NULL
  if (spec_name %in% names(TABLE_NOTES)) extra_note <- TABLE_NOTES[[spec_name]]
  show_correlations <- !(spec_name %in% TABLES_WITHOUT_CORRELATIONS)
  # the interactions: the regressors with a ":"
  hidden_terms <- NULL
  if (HIDE_INTERACTIONS) {
    regressors <- results$specification$regressors
    hidden_terms <- regressors[grepl(":", regressors, fixed = TRUE)]
  }
  lines <- coefficient_table_tex(results, labels, reference_note, sample_text,
                                 extra_note, show_correlations, hidden_terms)
  write_tex(lines, file.path(output_dirs$tables,
                             paste0("tab_", spec_name, ".tex")))

  # Slide versions: the table split into tables that fit a slide, all rows
  # (also the interactions and correlations), plus the probabilities at the
  # reference vignette; one frame each
  number <- SPECIFICATION_NUMBERS[[spec_name]]
  spec_note <- paste0(number, " ", latex_text(results$specification$label),
                      ". Mixed logit; reference category: ",
                      latex_text(reference_label), ".")
  if (spec_name %in% names(SLIDE_TABLE_NOTES)) {
    spec_note <- paste(spec_note, SLIDE_TABLE_NOTES[[spec_name]])
  }
  slide_tables <- slide_coefficient_tables(results, labels)
  slide_tables[["baseline"]] <- slide_baseline_table(results, labels)
  for (slide_table in slide_tables) {
    table_file <- paste0("tab_", spec_name, "_", slide_table$name, ".tex")
    write_tex(slide_table$lines, file.path(output_dirs$slide_tables, table_file))
    table_frames <- c(table_frames, slide_frame(
      paste0(number, " ", latex_text(slide_table$title)),
      paste0("  \\slidetable{\\tablepath/", table_file, "}"),
      paste(spec_note, slide_table$note)
    ))
  }
}

attributes <- names(results_list[[1]]$attribute_levels)
for (comparison_name in names(COMPARISONS)) {
  specs <- intersect(COMPARISONS[[comparison_name]], names(results_list))
  if (length(specs) == 0) next
  lr_comparison <- lr %>%
    filter(restricted %in% specs | unrestricted %in% specs)
  lines <- comparison_table_tex(results_list[specs], lr_comparison, labels,
                                attributes, sample_text, SPECIFICATION_NUMBERS,
                                comparison_name)
  write_tex(lines, file.path(output_dirs$tables,
                             paste0("tab_", comparison_name, ".tex")))
}

# Slide versions of the comparisons: the fit of the specifications, and the
# LR tests, each with a row for those without results. Each comes in two
# tables, so that each fits a slide: the specifications without prices, and
# those with prices (their names end in "_prices" or "_prices_by_group"); the
# LR tests of the prices, and the others. The lowest AIC and BIC are in bold
# within each fit table.
is_price_specification <- grepl("_prices", names(SPECIFICATION_NUMBERS))
numbers_by_table <- list(
  fit = SPECIFICATION_NUMBERS[!is_price_specification],
  fit_prices = SPECIFICATION_NUMBERS[is_price_specification]
)
for (table_name in names(numbers_by_table)) {
  numbers_table <- numbers_by_table[[table_name]]
  results_table <- results_list[intersect(names(numbers_table),
                                          names(results_list))]
  write_tex(slide_fit_table(results_table, numbers_table),
            file.path(output_dirs$slide_tables, paste0("tab_", table_name, ".tex")))
}
is_price_test <- LR_TEST_PAIRS$restrictions %in% c("prices", "price_groups")
write_tex(slide_lr_table(LR_TEST_PAIRS[!is_price_test, ], lr, results_list,
                         labels, SPECIFICATION_NUMBERS),
          file.path(output_dirs$slide_tables, "tab_lr_tests.tex"))
write_tex(slide_lr_table(LR_TEST_PAIRS[is_price_test, ], lr, results_list,
                         labels, SPECIFICATION_NUMBERS),
          file.path(output_dirs$slide_tables, "tab_lr_tests_prices.tex"))

# The price coefficients across the specifications with prices
write_tex(slide_price_comparison_table(results_list, PRICE_COMPARISON, labels,
                                       SPECIFICATION_NUMBERS),
          file.path(output_dirs$slide_tables, "tab_price_coefficients.tex"))

# 5. Figures: AMEs at the reference vignette -----------------------------------
# The AMEs of the mixed logits in percentage points, with the 95% Krinsky-Robb
# percentile intervals. For the intervals estimate +- 1.96 se instead, use
# ci_low and ci_high.
ame_all <- tibble()
for (spec_name in names(results_list)) {
  ame_all <- bind_rows(ame_all, results_list[[spec_name]]$ame_table %>%
                         mutate(specification = spec_name))
}
figure_data <- ame_all %>%
  filter(model == "mixed") %>%
  mutate(
    variation = factor(VARIATION_LABELS[vignette],
                       levels = rev(VARIATION_LABELS)),
    ame = 100 * ame,
    ci_low = 100 * ci_low_kr,
    ci_high = 100 * ci_high_kr,
    p_reference = 100 * p_reference
  )
if (anyNA(figure_data$variation)) {
  unlabelled <- unique(figure_data$vignette[is.na(figure_data$variation)])
  stop("Variations without a label: ", paste(unlabelled, collapse = ", "))
}
missing_profiles <- setdiff(figure_data$profile, names(PROFILE_LABELS))
if (length(missing_profiles) > 0) {
  stop("Profiles without a label: ", paste(missing_profiles, collapse = ", "))
}
missing_slide_labels <- setdiff(figure_data$vignette, names(SLIDE_VARIATION_LABELS))
if (length(missing_slide_labels) > 0) {
  stop("Variations without a slide label: ",
       paste(missing_slide_labels, collapse = ", "))
}

# The slide versions have the short labels of the variations, and this note;
# the frames of (b), whose note lists the specifications, the short one
ame_note <- paste("Change in the probability of the recommendation, in",
                  "percentage points, when one attribute of the reference",
                  "vignette changes; 95\\% Krinsky-Robb percentile intervals.")
ame_note_short <- paste("AMEs in percentage points; 95\\% Krinsky-Robb",
                        "percentile intervals.")

# (a) One figure per specification and outcome comparing the profiles, for the
#     specifications with several profiles (e.g. the Berufsgruppen)
for (spec_name in names(results_list)) {
  spec_data <- figure_data %>%
    filter(specification == spec_name)
  profiles <- unique(spec_data$profile)
  if (length(profiles) < 2) next
  spec_title <- paste(SPECIFICATION_NUMBERS[[spec_name]],
                      results_list[[spec_name]]$specification$label)
  if (length(results_list[[spec_name]]$specification$prices) > 0) {
    spec_title <- paste0(spec_title, "; at the mean prices")
  }
  for (outcome_name in results_list[[spec_name]]$outcomes) {
    plot_data <- spec_data %>%
      filter(outcome == outcome_name) %>%
      mutate(group = factor(PROFILE_LABELS[profile],
                            levels = PROFILE_LABELS[profiles]))
    subtitle <- baseline_subtitle(plot_data, spec_title)
    plot <- plot_ame(plot_data, TECHNOLOGY_LABELS[[outcome_name]], subtitle)
    file_path <- file.path(output_dirs$figures,
                           paste0("fig_ame_", spec_name, "_",
                                  file_name(TECHNOLOGY_LABELS[[outcome_name]]),
                                  ".pdf"))
    ggsave(file_path, plot, width = 8, height = 5)

    # Slide version and its frame. basename(): the file name without its
    # folder
    figure_file <- basename(file_path)
    save_slide_ame_figure(plot_data,
                          file.path(output_dirs$slide_figures, figure_file),
                          SLIDE_VARIATION_LABELS, SLIDE_FIGURE_SIZES$ame,
                          SLIDE_FONT_SIZE)
    ame_group_frames <- c(ame_group_frames, slide_frame(
      paste0(latex_text(TECHNOLOGY_LABELS[[outcome_name]]), ": AMEs by ",
             labels$group_name, ", ", SPECIFICATION_NUMBERS[[spec_name]]),
      c("  \\centering", paste0("  \\includegraphics{", figure_file, "}")),
      paste0(latex_text(spec_title), ". ", ame_note)
    ))
  }
}

# (b) One figure per comparison, profile and outcome comparing the
#     specifications of the comparison that have this profile. The legend
#     numbers the specifications as the comparison tables do.
for (comparison_name in names(COMPARISONS)) {
  comparison_data <- figure_data %>%
    filter(specification %in% COMPARISONS[[comparison_name]])
  for (profile_name in unique(comparison_data$profile)) {
    profile_data <- comparison_data %>%
      filter(profile == profile_name)
    has_profile <- names(results_list) %in% profile_data$specification
    specs <- names(results_list)[has_profile]
    if (length(specs) < 2) next
    # legend: number and label of each specification; subtitle: the number only
    spec_legend <- character(0)
    for (spec_name in specs) {
      spec_legend[spec_name] <- paste(SPECIFICATION_NUMBERS[[spec_name]],
                                      results_list[[spec_name]]$specification$label)
    }
    # gsub() removes the parentheses: "3i" for (3i)
    short_labels <- gsub("[()]", "", SPECIFICATION_NUMBERS[specs])
    names(short_labels) <- spec_legend[specs]
    outcomes <- results_list[[specs[1]]]$outcomes
    # The specifications with prices: their AMEs are at the mean prices
    profile_title <- PROFILE_LABELS[[profile_name]]
    prices_text <- ""
    for (spec_name in specs) {
      if (length(results_list[[spec_name]]$specification$prices) > 0) {
        prices_text <- " Specifications with prices: at the mean prices."
      }
    }
    if (prices_text != "") {
      profile_title <- paste0(profile_title, ".", prices_text)
    }
    for (outcome_name in outcomes) {
      plot_data <- profile_data %>%
        filter(outcome == outcome_name) %>%
        mutate(group = factor(spec_legend[specification], levels = spec_legend))
      subtitle <- baseline_subtitle(plot_data, profile_title, short_labels)
      # The labels of the specifications are long: one per line
      plot <- plot_ame(plot_data, TECHNOLOGY_LABELS[[outcome_name]], subtitle) +
        guides(colour = guide_legend(ncol = 1), shape = guide_legend(ncol = 1))
      file_path <- file.path(output_dirs$figures,
                             paste0("fig_ame_", comparison_name, "_",
                                    file_name(PROFILE_LABELS[[profile_name]]), "_",
                                    file_name(TECHNOLOGY_LABELS[[outcome_name]]),
                                    ".pdf"))
      ggsave(file_path, plot, width = 8, height = 6)

      # Slide version and its frame: the legend has the numbers of the
      # specifications, the note their labels
      slide_numbers <- unname(SPECIFICATION_NUMBERS[specs])
      slide_short_labels <- gsub("[()]", "", slide_numbers)
      names(slide_short_labels) <- slide_numbers
      slide_plot_data <- plot_data %>%
        mutate(group = factor(unname(SPECIFICATION_NUMBERS[specification]),
                              levels = slide_numbers))
      figure_file <- basename(file_path)
      save_slide_ame_figure(slide_plot_data,
                            file.path(output_dirs$slide_figures, figure_file),
                            SLIDE_VARIATION_LABELS, SLIDE_FIGURE_SIZES$ame,
                            SLIDE_FONT_SIZE, slide_short_labels)
      ame_comparison_frames <- c(ame_comparison_frames, slide_frame(
        paste0(latex_text(TECHNOLOGY_LABELS[[outcome_name]]),
               ": AMEs across specifications, ", PROFILE_LABELS[[profile_name]]),
        c("  \\centering", paste0("  \\includegraphics{", figure_file, "}")),
        paste0(latex_text(paste(spec_legend[specs], collapse = "; ")), ".",
               prices_text, " ", ame_note_short)
      ))
    }
  }
}

# 6. Figures: correlations of the random coefficients --------------------------
# One heat map per correlated specification and correlation block: the
# standard deviations on the diagonal, the correlations off it. strwrap()
# breaks the sample note into lines of at most 100 characters, so that the
# caption fits the width of the figure.
heatmap_caption <- paste(strwrap(sample_text, width = 100), collapse = "\n")
for (spec_name in names(results_list)) {
  results <- results_list[[spec_name]]
  if (identical(results$specification$correlation, "none")) next
  coefficients_mixed <- results$coefficient_table %>%
    filter(model == "mixed")
  random <- results$random_coefficients
  column_label <- column_labels(results$columns, labels)
  blocks <- unique(random$block)
  for (b in blocks) {
    # The random coefficients of the block: by column, the technologies in the
    # order of TECHNOLOGY_LABELS
    block_coefficients <- random %>%
      filter(block == b) %>%
      mutate(technology_order = match(technology, names(TECHNOLOGY_LABELS)),
             column_order = match(column, results$columns$column)) %>%
      arrange(column_order, technology_order)
    block_columns <- unique(block_coefficients$column)
    # setNames(): the labels, named by the random coefficients
    technology_labels <- unname(TECHNOLOGY_LABELS[block_coefficients$technology])
    if (length(block_columns) == 1) {
      row_labels <- setNames(technology_labels, block_coefficients$name)
    } else {
      row_labels <- setNames(paste0(column_label[block_coefficients$column], ": ",
                                    technology_labels),
                             block_coefficients$name)
    }
    sd_cor <- sd_cor_matrix(coefficients_mixed, block_coefficients$name)
    cat("\n", spec_name, ", block", b, "\n")
    print(round(sd_cor, 2))

    # The constants measure preferences for technologies; other random
    # coefficients the effect of their regressor
    is_constant <- block_coefficients$term == "(Intercept)" |
      block_coefficients$term %in% results$specification$constants
    block_line <- ""
    if (length(blocks) > 1) {
      block_line <- paste0(paste(column_label[block_columns], collapse = "; "),
                           ". ")
    }
    if (all(is_constant)) {
      title <- "Correlation of preferences for heating technologies"
      meaning <- paste("each constant measures the preference for a technology",
                       "relative to this category.\n")
    } else {
      title <- "Correlation of the random coefficients"
      meaning <- paste("each coefficient is the effect on a technology relative",
                       "to this category.\n")
    }
    subtitle <- paste0(
      block_line, SPECIFICATION_NUMBERS[[spec_name]], " ",
      results$specification$label, ".\n",
      "Reference category: ", TECHNOLOGY_LABELS[[REFERENCE_OUTCOME]], "; ",
      meaning,
      "Off the diagonal: correlation across experts\n",
      "(blue = liked together, red = traded off).\n",
      "Diagonal (bold): standard deviation across experts."
    )
    plot <- plot_correlation_heatmap(sd_cor, row_labels, title, subtitle,
                                     heatmap_caption)
    # one file per block, e.g. ..._constant_energy_advisers.pdf
    file_suffix <- ""
    if (length(blocks) > 1) {
      file_suffix <- paste0("_", file_name(column_label[[block_columns[1]]]))
    }
    figure_file <- paste0("fig_correlation_", spec_name, file_suffix, ".pdf")
    ggsave(file.path(output_dirs$figures, figure_file),
           plot, width = 8, height = 8)

    # Slide version, without title, subtitle and caption: the frame has them,
    # with the explanation next to the heat map
    slide_plot <- plot_correlation_heatmap(sd_cor, row_labels, NULL, NULL, NULL,
                                           base_size = SLIDE_FONT_SIZE,
                                           text_size = SLIDE_HEATMAP_TEXT_SIZE)
    ggsave(file.path(output_dirs$slide_figures, figure_file), slide_plot,
           width = SLIDE_FIGURE_SIZES$heatmap[["width"]],
           height = SLIDE_FIGURE_SIZES$heatmap[["height"]], device = cairo_pdf)
    # tolower(): the label in lower case, inside a sentence
    if (all(is_constant)) {
      frame_title <- "Correlations of the constants"
      meaning_item <- paste("Each constant: the preference for a technology",
                            "relative to", latex_text(tolower(reference_label)))
    } else {
      frame_title <- "Correlations of the random coefficients"
      meaning_item <- paste("Each coefficient: the effect on a technology",
                            "relative to", latex_text(tolower(reference_label)))
    }
    frame_title <- paste0(SPECIFICATION_NUMBERS[[spec_name]], " ", frame_title)
    if (length(blocks) > 1) {
      if (block_columns[1] %in% names(GROUP_LABELS)) {
        block_label <- GROUP_LABELS[[block_columns[1]]]
      } else {
        block_label <- paste(column_label[block_columns], collapse = "; ")
      }
      frame_title <- paste0(frame_title, ": ", latex_text(block_label))
    }
    heatmap_frames <- c(heatmap_frames, slide_frame(
      frame_title,
      c("  \\begin{columns}[T]",
        "    \\column{0.52\\textwidth}",
        paste0("      \\includegraphics{", figure_file, "}"),
        "    \\column{0.44\\textwidth}",
        "      \\begin{itemize}",
        "        \\item Diagonal (bold): standard deviation across experts",
        paste("        \\item Off the diagonal: correlation across experts;",
              "blue = liked together, red = traded off"),
        paste0("        \\item ", meaning_item),
        "      \\end{itemize}",
        "  \\end{columns}"),
      paste0(SPECIFICATION_NUMBERS[[spec_name]], " ",
             latex_text(results$specification$label),
             ". Implied by the estimated Cholesky factor of the mixed logit.")
    ))
  }
}

# 7. Slides --------------------------------------------------------------------
# The frames of the appendix of the slides (sections 4 to 6), and the numbers
# that their text quotes. slides/<date>_<topic>/slides.tex in this folder
# reads them with \input.
write_tex(table_frames,
          file.path(output_dirs$slide_tables, "frames_tables.tex"))
write_tex(ame_group_frames,
          file.path(output_dirs$slide_tables, "frames_ame_by_group.tex"))
write_tex(ame_comparison_frames,
          file.path(output_dirs$slide_tables, "frames_ame_comparisons.tex"))
write_tex(heatmap_frames,
          file.path(output_dirs$slide_tables, "frames_heatmaps.tex"))

# The numbers of draws: one number each if all specifications share them, as
# they share their sample (section 2); otherwise "differ" (e.g. after a rerun
# of one specification with other draws), and the cat() of section 2 has them
# by specification. as_tibble(): the list of three numbers as a one-row table.
draws_all <- tibble()
for (spec_name in names(results_list)) {
  draws_all <- bind_rows(draws_all, as_tibble(results_list[[spec_name]]$draws))
}
if (nrow(distinct(draws_all)) == 1) {
  draws_macros <- c(
    nDrawsModel = latex_number(draws_all$n_draws_model[1], 0),
    nDrawsAme = latex_number(draws_all$n_draws_ame[1], 0),
    nDrawsKrinskyRobb = latex_number(draws_all$n_draws_kr[1], 0)
  )
} else {
  message("The specifications differ in their numbers of draws: the slides ",
          "say \"differ\" instead of a number")
  draws_macros <- c(nDrawsModel = "differ", nDrawsAme = "differ",
                    nDrawsKrinskyRobb = "differ")
}
slide_macros <- c(
  nDecisions = latex_number(fit_all$n_choices[1], 0),
  nExperts = latex_number(fit_all$n_experts[1], 0),
  draws_macros,
  nSpecificationsReported = length(results_list),
  nSpecificationsListed = length(SPECIFICATION_NUMBERS)
)
write_tex(numbers_tex(slide_macros),
          file.path(output_dirs$slide_tables, "numbers_slides.tex"))

cat("Output written to", run_dir, "\n")
