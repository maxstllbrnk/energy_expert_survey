################################################################################
# test_sections.R — the sections of the questions and the vignettes (Blocks D to
#                   J) on the synthetic data: on the full sample and on sub-samples
# Inputs : experts.rds, vignettes.rds of a synthetic run (tests/simulate_data.R,
#          R/run_cleaning.R)
# Outputs: in OUTPUT_DIR/test_sections/, removed at the start of a run
# Checks : every exhibit file exists and has its row in the list, and the other way
#          round; no German answer and no unescaped percent sign in a table; the
#          minimum cell; a few numbers against a second computation
################################################################################
# From the project root, in a session with CLAUDECODE=1:
#   Rscript tests/test_sections.R

source(file.path("R", "setup.R"))
if (!USE_SYNTHETIC_DATA) {
  stop("tests/test_sections.R is for synthetic data")
}
experts_all <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds"))
vignettes_all <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds"))
test_dir <- file.path(OUTPUT_DIR, "test_sections")
unlink(test_dir, recursive = TRUE)

# 1. Helpers -------------------------------------------------------------------
n_failed <- 0
report <- function(what, is_ok, detail = "") {
  cat(sprintf("%-68s %s %s\n", what, if (is_ok) "PASS" else "*** FAIL ***", detail))
  if (!is_ok) {
    # <<-: changes n_failed outside this function
    n_failed <<- n_failed + 1
  }
}

#' The folders of a run, created
make_dirs <- function(name) {
  output_dir <- file.path(test_dir, name)
  dirs <- list(output_dir = output_dir,
               tables = file.path(output_dir, "tables"),
               figures = file.path(output_dir, "figures"),
               maps = file.path(output_dir, "maps"))
  for (folder in dirs) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  dirs
}

#' Runs all sections on one sample into a folder of its own
run_sections <- function(experts, vignettes, name) {
  dirs <- make_dirs(name)
  exhibits <- bind_rows(
    section_experts_firms(experts, dirs),
    section_market(experts, dirs),
    section_prices_beliefs(experts, dirs),
    section_barriers_criteria(experts, dirs),
    section_preferences(experts, dirs),
    section_heating_context(experts, dirs),
    section_vignettes(experts, vignettes, dirs)
  )
  list(exhibits = exhibits, output_dir = dirs$output_dir)
}

#' What every run must satisfy: the list and the folders agree, tables are clean
check_run <- function(run, name) {
  exhibits <- run$exhibits
  on_disk <- list.files(run$output_dir, recursive = TRUE)
  report(paste0(name, ": every exhibit of the list exists"),
         all(exhibits$file %in% on_disk),
         paste(setdiff(exhibits$file, on_disk), collapse = ", "))
  report(paste0(name, ": every file is in the list"),
         all(on_disk %in% exhibits$file),
         paste(setdiff(on_disk, exhibits$file), collapse = ", "))
  report(paste0(name, ": no file twice in the list"),
         !any(duplicated(exhibits$file)))
  report(paste0(name, ": every row names a block, a caption and a placement"),
         all(exhibits$block != "" & exhibits$caption != "" &
               exhibits$placement %in% c("main", "appendix")))

  # The file name follows the convention
  prefix <- c(table = "tables/tab_", figure = "figures/fig_", map = "maps/map_")
  ending <- c(table = ".tex", figure = ".pdf", map = ".pdf")
  expected <- paste0(prefix[exhibits$type], exhibits$id, "_",
                     group_slug(exhibits$group), ending[exhibits$type])
  report(paste0(name, ": file names follow the convention"),
         all(expected == exhibits$file),
         paste(exhibits$file[expected != exhibits$file], collapse = ", "))

  table_files <- file.path(run$output_dir,
                           exhibits$file[exhibits$type == "table"])
  n_percent <- 0
  german_found <- character(0)
  smallest_count <- Inf
  for (table_file in table_files) {
    lines <- readLines(table_file, encoding = "UTF-8")
    n_percent <- n_percent + sum(grepl("(^|[^\\\\])%", lines))
    for (word in GERMAN_ANSWERS) {
      if (any(grepl(word, lines, fixed = TRUE))) {
        german_found <- c(german_found, paste0(basename(table_file), ": ", word))
      }
    }
    # A whole number that stands alone in a cell is a count, or a statistic with no
    # decimals: none of 1 to 9. The test of Q17 has its degrees of freedom, and the
    # year of training an SD without decimals.
    if (basename(table_file) != "tab_price_tests_all.tex") {
      for (line in lines[grepl("&", lines, fixed = TRUE) &
                           !grepl("Year training", lines, fixed = TRUE)]) {
        line <- sub("\\\\\\\\$", "", line)
        row_cells <- trimws(strsplit(line, "&", fixed = TRUE)[[1]])
        counts <- number_of(row_cells[-1][grepl("^[0-9,]+$", row_cells[-1])])
        smallest_count <- min(c(smallest_count, counts[counts > 0]))
      }
    }
  }
  report(paste0(name, ": percent signs escaped in all tables"), n_percent == 0)
  report(paste0(name, ": no German answer in a table"), length(german_found) == 0,
         paste(german_found, collapse = "; "))
  report(paste0(name, ": no count below the minimum cell in a table"),
         smallest_count >= MIN_N, paste("smallest", smallest_count))
}

# Every German answer of labels.R that is not also an English word or a name
GERMAN_ANSWERS <- setdiff(
  c(names(REVENUE_LABELS), names(RADIUS_LABELS), names(DISTRICT_HEATING_LABELS),
    names(EDUCATION_LABELS), names(CUSTOMER_LABELS), names(TECHNOLOGY_LABELS),
    names(OWN_HEATING_LABELS), names(EXPECTATION_LABELS), names(COST_LABELS),
    names(CRITERION_LABELS), "Nicht Gewählt", "Sonstige", DESIGN_LEVELS$level),
  c("1970", "2000", "120", "70", "200")
)

#' The table of a run as lines of cells: list of character vectors, by row label
table_row <- function(run, file, row_label) {
  lines <- readLines(file.path(run$output_dir, "tables", file), encoding = "UTF-8")
  line <- lines[grepl(row_label, lines, fixed = TRUE)][1]
  line <- sub("\\\\\\\\$", "", line)
  trimws(strsplit(line, "&", fixed = TRUE)[[1]])
}
number_of <- function(cell) {
  as.numeric(gsub("[^0-9.-]", "", cell))
}

# 2. The design the labels rely on ---------------------------------------------
# These checks stand here, and not in the section, as the design is a fixed file
words_of_demand <- c("120" = "teilsaniert", "70" = "vollständig saniert",
                     "200" = "nicht saniert")
is_matching <- logical(length(words_of_demand))
for (k in seq_along(words_of_demand)) {
  demand <- names(words_of_demand)[k]
  renovation <- vignettes_all$att_renovation[vignettes_all$att_heat_demand == demand]
  is_matching[k] <- length(renovation) > 0 &&
    all(grepl(words_of_demand[k], renovation, fixed = TRUE))
}
report("heat demand and renovation are the same split", all(is_matching))
report("no house built in 2000 that was never renovated",
       !any(vignettes_all$att_build_year == "2000" &
              vignettes_all$att_heat_demand == "200"))
report("no house built in 2000 with oil heating",
       !any(vignettes_all$att_build_year == "2000" &
              vignettes_all$att_current_heating == "Ölheizung"))

# 3. The full sample -----------------------------------------------------------
full_run <- run_sections(experts_all, vignettes_all, "full")
check_run(full_run, "full sample")
full <- full_run$exhibits
cat("\nExhibits of the full sample:\n")
print(count(full, block, type))
print(count(full, placement))

# The text of the figures and maps, if pdftotext is there. Sys.which(): the path of
# a program, "" if it is not installed; nzchar(): is the text not empty?
if (nzchar(Sys.which("pdftotext"))) {
  german_in_figures <- character(0)
  figure_files <- file.path(full_run$output_dir, full$file[full$type != "table"])
  for (figure_file in figure_files) {
    # system2(stdout = TRUE): the output of the command as lines of text;
    # shQuote(): quotes a path, which can hold spaces
    figure_text <- system2("pdftotext",
                           c("-enc", "UTF-8", shQuote(figure_file), "-"),
                           stdout = TRUE)
    figure_text <- paste(figure_text, collapse = " ")
    for (word in GERMAN_ANSWERS) {
      if (grepl(word, figure_text, fixed = TRUE)) {
        german_in_figures <- c(german_in_figures,
                               paste0(basename(figure_file), ": ", word))
      }
    }
  }
  report("full sample: no German answer in a figure or map",
         length(german_in_figures) == 0, paste(german_in_figures, collapse = "; "))
}

expected_ids <- c(
  "experts", "experts_groups", "experts_state", "market", "technology_shares",
  "technology_shares_groups", "prices", "prices_groups", "price_distribution",
  "price_expectations", "price_tests", "carbon_price", "price_state_gas",
  "price_state_hpelec", "barriers_renewable", "barriers_district_heating",
  "barriers_conventional", "barriers", "customer_criteria", "customer_weights",
  "preferences", "preference_distribution", "own_heating", "heating_context",
  "heating_context_groups", "district_heating_state", "degree_days", "design",
  "balance", "recommendations", "recommendations_groups",
  "recommendations_couple_age", "recommendations_income",
  "recommendations_current_heating", "recommendations_replacement_timing",
  "recommendations_build_year", "recommendations_heat_demand",
  "recommendations_heat_distribution", "arms", "within_expert",
  "within_expert_groups", "position", "investment_cost"
)
report("all exhibits of the brief are there", all(expected_ids %in% full$id),
       paste(setdiff(expected_ids, full$id), collapse = ", "))

# Four versions, unless the brief says all experts only
one_version <- c(
  "experts_groups", "experts_state", "technology_shares_groups", "prices_groups",
  "price_tests", "price_state_gas", "price_state_hpelec", "heating_context_groups",
  "district_heating_state", "degree_days", "design", "balance",
  "recommendations_groups", "arms", "within_expert_groups", "position"
)
versions <- full %>%
  distinct(id, group) %>%
  count(id)
report("four versions, except those for all experts only",
       all(versions$n[versions$id %in% one_version] == 1) &&
         all(versions$n[!(versions$id %in% one_version)] == 4))

# Placement: a group version is never in the main part, the comparison is
is_group_version <- full$group != ALL_EXPERTS
report("group versions are in the appendix",
       all(full$placement[is_group_version] == "appendix"))
main_ids <- c("experts", "experts_groups", "technology_shares",
              "technology_shares_groups", "prices", "prices_groups",
              "price_expectations", "barriers_renewable", "customer_criteria",
              "own_heating", "heating_context", "heating_context_groups",
              "district_heating_state", "design", "recommendations",
              "recommendations_groups", "arms", "within_expert",
              "within_expert_groups")
placed_main <- full %>%
  filter(placement == "main") %>%
  distinct(id) %>%
  pull(id)
# setequal(): the same elements, in any order
report("the main part is what the brief says", setequal(placed_main, main_ids),
       paste(c(setdiff(placed_main, main_ids), setdiff(main_ids, placed_main)),
             collapse = ", "))

# 4. A few numbers against a second computation --------------------------------
cells <- table_row(full_run, "tab_prices_all.tex", "Gas")
mean_gas_price <- mean(experts_all$price_gas_kwh, na.rm = TRUE)
report("mean gas price", abs(number_of(cells[3]) - mean_gas_price) < 0.051)
report("N of the gas price",
       number_of(cells[2]) == sum(!is.na(experts_all$price_gas_kwh)))

cells <- table_row(full_run, "tab_experts_all.tex", "Year training was completed")
report("a year has no thousands separator in its statistics",
       !grepl(",", cells[3], fixed = TRUE) && !grepl(",", cells[6], fixed = TRUE))

answered_q19a <- experts_all$barr_ren_funds[!is.na(experts_all$barr_ren_funds)]
cells <- table_row(full_run, "tab_barriers_all.tex", "Customers lack the funds")
report("share of a barrier",
       abs(number_of(cells[2]) - 100 * mean(answered_q19a == "Ja")) < 0.051)

answered <- vignettes_all %>%
  filter(!is.na(vig_rec), !is.na(vig_arm))
cells <- table_row(full_run, "tab_recommendations_all.tex", "Heat pump")
n_hp_fw <- sum(answered$vig_rec == "Wärmepumpe" & answered$vig_arm == "fw")
report("heat pumps recommended in the arm with district heating",
       number_of(cells[2]) == n_hp_fw)

six_answered <- answered %>%
  group_by(resp_uid) %>%
  summarise(n_answered = n(), n_technologies = n_distinct(vig_rec)) %>%
  filter(n_answered == N_VIGNETTES)
cells <- table_row(full_run, "tab_within_expert_all.tex",
                   "Technologies recommended")
report("experts with six vignettes, and their technologies",
       number_of(cells[2]) == nrow(six_answered) &&
         abs(number_of(cells[3]) - mean(six_answered$n_technologies)) < 0.0051)

# The weights of Q28 sum to 100 within a table, up to rounding
weight_means <- numeric(0)
for (criterion_label in CRITERION_LABELS) {
  cells <- table_row(full_run, "tab_customer_weights_all.tex", criterion_label)
  weight_means <- c(weight_means, number_of(cells[3]))
}
report("the mean weights of Q28 sum to 100", abs(sum(weight_means) - 100) < 0.5,
       paste("sum", sum(weight_means)))

# The G-test against the same statistic from a second formula. chisq.test(): R's own
# Pearson test, with the expected counts
test_answers <- experts_all %>%
  filter(!is.na(expert_group), !is.na(pexp_gas))
counts <- table(droplevels(test_answers$expert_group),
                droplevels(test_answers$pexp_gas))
cells <- table_row(full_run, "tab_price_tests_all.tex", "Gas")
pearson_test <- suppressWarnings(chisq.test(counts))
observed <- as.vector(counts)
expected <- as.vector(pearson_test$expected)
has_count <- observed > 0
g_by_hand <- 2 * sum(observed[has_count] *
                       log(observed[has_count] / expected[has_count]))
report("G statistic of gas", abs(number_of(cells[3]) - g_by_hand) < 0.051)
cramers_v <- sqrt(unname(pearson_test$statistic) /
                    (sum(counts) * (min(dim(counts)) - 1)))
report("Cramér's V of gas", abs(number_of(cells[5]) - cramers_v) < 0.00051)

# The balance of the design against chisq.test(), in the title of the panel
balance_data <- answered %>%
  filter(!is.na(expert_group)) %>%
  mutate(cell = paste(vig_arm, expert_group))
balance_test <- suppressWarnings(
  chisq.test(table(balance_data$att_couple_age, balance_data$cell))
)
cells <- table_row(full_run, "tab_balance_all.tex", "Age of the couple")
p_in_table <- number_of(sub(".*p = ([0-9.]+).*", "\\1", cells[1]))
report("p-value of the balance of an attribute",
       abs(p_in_table - unname(balance_test$p.value)) < 0.00051,
       paste("table", p_in_table, "chisq.test", round(balance_test$p.value, 3)))

# 5. What the synthetic data do not have ---------------------------------------
# Q28 with fewer than three criteria and with a skipped rank. Of 12 experts, 6 name
# investment cost first and quick installation second, 5 only investment cost, and
# 1 only investment cost, on the second rank. Investment cost: (6 x 2/3 + 6) / 12.
criteria <- names(CRITERION_LABELS)
investment_cost <- criteria[CRITERION_LABELS == "Investment cost"]
quick_installation <- criteria[CRITERION_LABELS == "Quick installation"]
few_criteria <- tibble(
  resp_uid = paste0("expert", 1:12),
  expert_group = factor("Energy advisers", levels = EXPERT_GROUPS),
  crit_rank1 = factor(c(rep(investment_cost, 11), NA), levels = criteria),
  crit_rank2 = factor(c(rep(quick_installation, 6), rep(NA, 5), investment_cost),
                      levels = criteria),
  crit_rank3 = factor(NA, levels = criteria)
)
dirs_criteria <- make_dirs("few_criteria")
run_criteria <- list(
  exhibits = section_barriers_criteria(few_criteria, dirs_criteria),
  output_dir = dirs_criteria$output_dir
)
check_run(run_criteria, "few criteria")
cells <- table_row(run_criteria, "tab_customer_weights_all.tex",
                   "Investment cost")
report("Q28: the weight of a criterion with fewer ranks",
       abs(number_of(cells[3]) - 100 * (6 * 2 / 3 + 6) / 12) < 0.051, cells[3])
cells <- table_row(run_criteria, "tab_customer_weights_all.tex",
                   "Quick installation")
report("Q28: the weight of the other criterion",
       abs(number_of(cells[3]) - 100 * (6 / 3) / 12) < 0.051, cells[3])

# Q11c: free text in different spellings, an answer that names no manufacturer, a
# manufacturer below the minimum cell. Vaillant: 15 of 36; Other: 12 + 9 of 36.
brand_answers <- tibble(
  resp_uid = paste0("expert", 1:40),
  expert_group = factor("HVAC installers", levels = EXPERT_GROUPS),
  brand_top1 = c(rep("vaillant ", 12), rep("VAILLANT", 3), rep("Keine", 12),
                 rep("Buderus", 9), rep(NA, 4))
)
dirs_brands <- make_dirs("brands")
run_brands <- list(exhibits = section_market(brand_answers, dirs_brands),
                   output_dir = dirs_brands$output_dir)
check_run(run_brands, "brands")
cells <- table_row(run_brands, "tab_market_all.tex", "Vaillant")
report("Q11c: spellings of one manufacturer come together",
       number_of(cells[2]) == 36 && abs(number_of(cells[3]) - 100 * 15 / 36) < 0.051,
       cells[3])
cells <- table_row(run_brands, "tab_market_all.tex", "Other")
report("Q11c: no manufacturer, and below the minimum cell, go into Other",
       abs(number_of(cells[3]) - 100 * 21 / 36) < 0.051, cells[3])
lines <- readLines(file.path(run_brands$output_dir, "tables", "tab_market_all.tex"),
                   encoding = "UTF-8")
report("Q11c: neither is named",
       !any(grepl("Keine", lines, fixed = TRUE)) &&
         !any(grepl("Buderus", lines, fixed = TRUE)))

# A histogram draws no bin and no panel below the minimum cell: panel A has one bin
# of 20 answers, panel B 12 answers, each in a bin of its own
histogram_values <- tibble(
  panel = factor(c(rep("A", 20), rep("B", 12))),
  value = c(rep(7, 20), seq(1, 60, length.out = 12))
)
histogram_plot <- plot_binned_histogram(histogram_values, 5, "Value", n_columns = 1)
# ggplot_build(): what the figure will draw
histogram_panels <- ggplot_build(histogram_plot)$layout$layout
report("histogram: a panel without a bin is left out",
       nrow(histogram_panels) == 1)
report("histogram: no figure if no panel is left",
       is.null(plot_binned_histogram(histogram_values %>% filter(panel == "B"), 5,
                                     "Value", n_columns = 1)))

# 6. Sub-samples ---------------------------------------------------------------
# Completers of one arm, as in the estimation scripts
for (arm_code in c("fw", "nofw")) {
  sample_experts <- experts_all %>%
    filter(is_complete, arm == arm_code)
  sample_vignettes <- vignettes_all %>%
    filter(resp_uid %in% sample_experts$resp_uid)
  arm_run <- run_sections(sample_experts, sample_vignettes,
                          paste0("completers_", arm_code))
  check_run(arm_run, paste("completers", arm_code))
  report(paste("completers", arm_code, ": no table of the arms, a single arm"),
         !("arms" %in% arm_run$exhibits$id))
  report(paste("completers", arm_code, ": recommendations are there"),
         "recommendations" %in% arm_run$exhibits$id)
}

# Without some columns. Only the section they belong to, as figures take time.
no_shares <- experts_all %>%
  select(-starts_with("mix25_"))
dirs_no_shares <- make_dirs("no_mix25")
run_no_shares <- list(exhibits = section_market(no_shares, dirs_no_shares),
                      output_dir = dirs_no_shares$output_dir)
check_run(run_no_shares, "no mix25")
lines <- readLines(file.path(run_no_shares$output_dir, "tables",
                             "tab_technology_shares_all.tex"), encoding = "UTF-8")
report("no mix25: the panel of Q11b is gone",
       !any(grepl("Q11b", lines[!grepl("Notes", lines)], fixed = TRUE)))

no_answers <- experts_all %>%
  select(-starts_with("price_"), -starts_with("pexp_"), -starts_with("barr_"),
         -starts_with("crit_"), -starts_with("cust_"), -starts_with("adv_"),
         -starts_with("mix25_"), -starts_with("co2_"), -starts_with("cpd_"),
         -starts_with("svc_"), -starts_with("tp_"), -starts_with("risk_"),
         -brand_top1, -hdd, -own_heating, -firm_dh_in_area, -bundesland)
run_no_answers <- run_sections(no_answers, vignettes_all, "only_some_columns")
check_run(run_no_answers, "only some columns")
report("only some columns: the blocks without columns write nothing",
       !any(c("prices", "barriers", "customer_criteria", "preferences",
              "own_heating") %in% run_no_answers$exhibits$id))

no_cost <- vignettes_all %>%
  select(-vig_cost)
exhibits_no_cost <- section_vignettes(experts_all, no_cost, make_dirs("no_cost"))
report("no cost question: its figure is gone",
       !("investment_cost" %in% exhibits_no_cost$id))

# Small samples: the minimum cell has to hold everywhere, and nothing may break
set.seed(20261109)
for (n_experts in c(5, 12, 40)) {
  # slice_sample(): n rows at random
  small_experts <- experts_all %>%
    filter(is_complete) %>%
    slice_sample(n = n_experts)
  small_vignettes <- vignettes_all %>%
    filter(resp_uid %in% small_experts$resp_uid)
  small_run <- run_sections(small_experts, small_vignettes,
                            paste0("small_", n_experts))
  check_run(small_run, paste("sample of", n_experts))
  group_sizes <- small_experts %>%
    filter(!is.na(expert_group)) %>%
    count(expert_group)
  groups_too_small <- as.character(group_sizes$expert_group[group_sizes$n < MIN_N])
  report(paste("sample of", n_experts, ": nothing for a group below the minimum"),
         !any(small_run$exhibits$group %in% groups_too_small))
}
# One group only: no comparison of the groups, no test
one_group <- experts_all %>%
  filter(expert_group == "Chimney sweeps")
run_one_group <- run_sections(
  one_group, vignettes_all %>% filter(resp_uid %in% one_group$resp_uid), "one_group"
)
check_run(run_one_group, "one group")
report("one group: no comparison of the groups and no test",
       !any(c("experts_groups", "price_tests") %in% run_one_group$exhibits$id))

# 7. The result ----------------------------------------------------------------
cat("\n", if (n_failed == 0) "ALL CHECKS PASSED" else paste(n_failed, "CHECKS FAILED"),
    "\n",
    sep = "")
if (!interactive()) {
  quit(status = if (n_failed == 0) 0 else 1)
}
