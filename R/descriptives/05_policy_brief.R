################################################################################
# 05_policy_brief.R — the topics of the policy brief, by group
# Inputs : experts (02_data.R); the tests of the price expectations
#          (PRICE_TESTS_FILE), from
#          R/first_policy_brief/02_kl_divergence_prices.R
# Outputs: figures/fig_desc_price_expectations.pdf,
#          fig_desc_barriers_<question>.pdf, fig_desc_customer_criteria.pdf;
#          tables/tab_desc_price_expectations.tex, tab_desc_price_tests.tex,
#          tab_desc_price_pairs.tex, tab_desc_barriers_<question>.tex,
#          tab_desc_customer_criteria.tex, tab_desc_customer_weights.tex
# Checks : the tests rest on the answers of the descriptives; every answer has
#          a label; no share rests on fewer than MIN_N answers
################################################################################

sample_text <- paste0("Completed questionnaires of energy advisers, HVAC ",
                      "installers and chimney sweeps.")

# 1. Expected price developments (Q17) -----------------------------------------
# One row per expert x fuel answered: a blank drops out for that fuel only
expectations <- tibble()
for (fuel in PRICE_FUELS) {
  # before tibble(), in which `fuel` would already be the new column
  answer <- as.character(experts[[paste0("pexp_", fuel)]])
  expectations <- bind_rows(expectations, tibble(
    group = as.character(experts$group), fuel = fuel, answer = answer
  ))
}
expectations <- expectations %>%
  filter(!is.na(answer))
expectations_all_experts <- expectations %>%
  mutate(group = ALL_EXPERTS)
expectations <- bind_rows(expectations, expectations_all_experts)

expectation_shares <- expectations %>%
  count(group, fuel, answer) %>%
  group_by(group, fuel) %>%
  mutate(n_answered = sum(n), share = n / n_answered) %>%
  ungroup()

# 1.1 The figure: the answers of each fuel as one bar, a panel per group; a
# bar of fewer than MIN_N answers is left out
expectation_plot_data <- expectation_shares %>%
  filter(group != ALL_EXPERTS, n_answered >= MIN_N) %>%
  mutate(
    group = factor(group, levels = GROUP_LABELS),
    # the first fuel at the top
    fuel = factor(label_of(fuel, FUEL_LABELS, "fuel"),
                  levels = rev(FUEL_LABELS)),
    answer = factor(label_of(answer, EXPECTATION_LABELS, "answer of Q17"),
                    levels = EXPECTATION_LABELS),
    # a label only where it fits: "14%" is wider than a segment of 14%
    label = if_else(share >= 0.2, percent_label(share), ""),
    text_colour = if_else(answer == "More than 10% lower", "white", ZEW_DARK)
  )
n_bars_left_out <- expectation_shares %>%
  filter(group != ALL_EXPERTS, n_answered < MIN_N) %>%
  distinct(group, fuel) %>%
  nrow()
cat("Bars of Q17 with fewer than", MIN_N, "answers:", n_bars_left_out, "\n")

# position_stack(reverse = TRUE): the answers from left to right in their
# order
plot_expectations <- ggplot(expectation_plot_data,
                            aes(x = share, y = fuel, fill = answer)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.2,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = label, colour = text_colour, group = answer),
            size = TEXT_SIZE,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  facet_wrap(~group, nrow = 1) +
  scale_fill_manual(values = EXPECTATION_COLOURS) +
  # scale_colour_identity(): the colours in text_colour as they are
  scale_colour_identity() +
  # no breaks at 0 and 100%, where the labels of two panels would touch
  scale_x_continuous(labels = scales::label_percent(),
                     breaks = c(0.25, 0.5, 0.75), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Share of the experts who answered for the fuel", y = NULL) +
  theme_rcl(FONT_SIZE) +
  theme(panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(10, "pt"),
        legend.key.size = unit(LEGEND_KEY_SIZE, "pt"))
save_figure_pdf(plot_expectations,
                file.path(figures_dir, "fig_desc_price_expectations.pdf"),
                FIGURE_SIZES$full)

answers_per_fuel <- expectation_shares %>%
  filter(group == ALL_EXPERTS) %>%
  distinct(fuel, n_answered)
numbers[["descNotePriceExpectations"]] <- latex_text(paste0(
  "Q17: the price of the fuel in five years, compared with the start of ",
  "2026. ", sample_text, " Between ",
  latex_number(min(answers_per_fuel$n_answered), 0), " and ",
  latex_number(max(answers_per_fuel$n_answered), 0), " answers per fuel; ",
  "a fuel not available in the expert's area was to be left blank."
))

# 1.2 The table: the share expecting a rise, and the experts who answered,
# side by side for each group
rise_shares <- expectations %>%
  group_by(group, fuel) %>%
  summarise(n = n(), share_rise = mean(answer %in% PEXP_RISE),
            .groups = "drop") %>%
  mutate(share_rise = if_else(n >= MIN_N, share_rise, NA_real_))
rise_cells <- tibble(label = latex_text(unname(FUEL_LABELS[PRICE_FUELS])))
for (group_name in GROUP_COLUMNS) {
  group_rise <- tibble(fuel = PRICE_FUELS) %>%
    left_join(rise_shares %>%
                filter(group == group_name),
              by = "fuel", relationship = "one-to-one")
  rise_cells[[paste0("rise_", group_name)]] <-
    statistic_text(100 * group_rise$share_rise, 1)
  rise_cells[[paste0("n_", group_name)]] <- latex_number(group_rise$n, 0)
}
group_spans <- character(length(GROUP_COLUMNS))
group_rules <- character(length(GROUP_COLUMNS))
for (g in seq_along(GROUP_COLUMNS)) {
  group_spans[g] <- paste0("\\multicolumn{2}{c}{", GROUP_COLUMNS[g], "}")
  group_rules[g] <- paste0("\\cmidrule(lr){", 2 * g, "-", 2 * g + 1, "}")
}
rise_tex <- table_float(
  column_spec = paste0("l", strrep("c", 2 * length(GROUP_COLUMNS))),
  header = c(
    paste0(" & ", paste(group_spans, collapse = " & "), " \\\\"),
    paste(group_rules, collapse = " "),
    paste0(" & ", paste(rep("Rise (\\%) & N", length(GROUP_COLUMNS)),
                        collapse = " & "), " \\\\")
  ),
  body = cell_rows(rise_cells),
  caption = "Who expects energy prices to rise?",
  label = "tab:desc_price_expectations",
  notes = paste0(
    latex_text(paste0(sample_text, " Q17: the price of the fuel in five ",
                      "years, compared with the start of 2026. Rise: ")),
    "``up to 10\\% higher'' or ``more than 10\\% higher'', among the N ",
    "experts who answered for the fuel.", dash_note(rise_cells)
  ),
  font_size = TABLE_FONT_SIZE
)
write_tex(rise_tex, file.path(tables_dir, "tab_desc_price_expectations.tex"))
# The workbook leaves out what the figure and the table leave out
excel_sheets$price_expectations <- expectation_shares %>%
  mutate(n = if_else(n_answered >= MIN_N, n, NA_integer_),
         share = if_else(n_answered >= MIN_N, share, NA_real_))
excel_sheets$price_rise <- rise_shares

# 2. Tests of the price expectations (appendix) --------------------------------
# The tests are those of the policy brief, not computed again here
if (!file.exists(PRICE_TESTS_FILE)) {
  stop("The tests of the price expectations are missing: run ",
       "R/first_policy_brief/02_kl_divergence_prices.R first (",
       PRICE_TESTS_FILE, ")")
}
g_test <- read_excel(PRICE_TESTS_FILE, sheet = "g_test") %>%
  mutate(fuel = label_of(fuel, PRICE_TESTS_FUELS, "fuel of the tests"))
kruskal_wallis <- read_excel(PRICE_TESTS_FILE, sheet = "kruskal_wallis") %>%
  mutate(fuel = label_of(fuel, PRICE_TESTS_FUELS, "fuel of the tests"))
pairwise <- read_excel(PRICE_TESTS_FILE, sheet = "paarvergleiche") %>%
  mutate(fuel = label_of(fuel, PRICE_TESTS_FUELS, "fuel of the tests"))

# The tests must rest on the answers of the descriptives; otherwise they are
# from an older run or another sample
answers_compared <- kruskal_wallis %>%
  select(fuel, n_tests = n) %>%
  left_join(rise_shares %>%
              filter(group == ALL_EXPERTS) %>%
              select(fuel, n),
            by = "fuel")
if (nrow(answers_compared) != length(PRICE_FUELS) ||
    any(answers_compared$n_tests != answers_compared$n)) {
  print(answers_compared)
  stop("The tests in ", PRICE_TESTS_FILE, " rest on other answers than the ",
       "descriptives (above): rerun ",
       "R/first_policy_brief/02_kl_divergence_prices.R")
}

tests <- tibble(fuel = PRICE_FUELS) %>%
  left_join(g_test %>% select(fuel, cramers_v, p_sim, p_sim_holm),
            by = "fuel") %>%
  left_join(kruskal_wallis %>% select(fuel, n, epsilon_sq, p, p_holm),
            by = "fuel")
test_cells <- tests %>%
  transmute(label = latex_text(unname(FUEL_LABELS[fuel])),
            n = latex_number(n, 0),
            cramers_v = latex_number(cramers_v, 3),
            p_sim = latex_p(p_sim),
            p_sim_holm = latex_p(p_sim_holm),
            epsilon_sq = latex_number(epsilon_sq, 3),
            p = latex_p(p),
            p_holm = latex_p(p_holm))
tests_tex <- table_float(
  column_spec = "p{3.6cm}ccccccc",
  header = c(
    paste0(" & & \\multicolumn{3}{c}{G-test} & ",
           "\\multicolumn{3}{c}{Kruskal--Wallis} \\\\"),
    "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8}",
    " & N & $V$ & $p$ & $p$ Holm & $\\varepsilon^2$ & $p$ & $p$ Holm \\\\"
  ),
  body = cell_rows(test_cells),
  caption = "Do the groups expect different price developments?",
  label = "tab:desc_price_tests",
  notes = paste0(
    latex_text(paste0(
      sample_text, " Q17. G-test: do the three groups give the five answers ",
      "in the same shares? p from simulated tables; V: Cramér's V. ",
      "Kruskal-Wallis: does a group answer higher or lower? "
    )),
    "$\\varepsilon^2 = H / (N - 1)$. ",
    "Holm: corrected across the five fuels."
  ),
  font_size = TABLE_FONT_SIZE
)
write_tex(tests_tex, file.path(tables_dir, "tab_desc_price_tests.tex"))

# The pairs of groups, in the order of the tests
pair_keys <- pairwise %>%
  distinct(group_1, group_2)
pair_headers <- character(nrow(pair_keys))
pair_cells <- tibble(label = latex_text(unname(FUEL_LABELS[PRICE_FUELS])))
for (k in seq_len(nrow(pair_keys))) {
  first_group <- label_of(pair_keys$group_1[k], GROUP_LABELS, "Berufsgruppe")
  second_group <- label_of(pair_keys$group_2[k], GROUP_LABELS, "Berufsgruppe")
  pair_headers[k] <- paste0(
    "\\multicolumn{2}{c}{",
    header_cell(paste0(first_group, "\\\\vs ",
                       GROUP_LABELS_IN_TEXT[[second_group]])), "}"
  )
  pair_tests <- tibble(fuel = PRICE_FUELS) %>%
    left_join(pairwise %>%
                filter(group_1 == pair_keys$group_1[k],
                       group_2 == pair_keys$group_2[k]),
              by = "fuel", relationship = "one-to-one")
  pair_cells[[paste0("higher_", k)]] <-
    latex_number(pair_tests$prob_1_higher, 2)
  pair_cells[[paste0("p_", k)]] <- latex_p(pair_tests$p_holm)
}
pair_rules <- character(nrow(pair_keys))
for (k in seq_len(nrow(pair_keys))) {
  pair_rules[k] <- paste0("\\cmidrule(lr){", 2 * k, "-", 2 * k + 1, "}")
}
pairs_tex <- table_float(
  column_spec = paste0("p{3.6cm}", strrep("c", 2 * nrow(pair_keys))),
  header = c(
    paste0(" & ", paste(pair_headers, collapse = " & "), " \\\\"),
    paste(pair_rules, collapse = " "),
    paste0(" & ", paste(rep("P(higher) & $p$ Holm", nrow(pair_keys)),
                        collapse = " & "), " \\\\")
  ),
  body = cell_rows(pair_cells),
  caption = "Which groups expect different price developments?",
  label = "tab:desc_price_pairs",
  notes = latex_text(paste0(
    sample_text, " Q17, Wilcoxon rank-sum test of each pair. P(higher): the ",
    "probability that an expert of the first group answers higher than one ",
    "of the second, plus half that of the same answer (0.5: no ",
    "difference). Holm: corrected across the ", nrow(pairwise), " tests."
  )),
  font_size = TABLE_FONT_SIZE
)
write_tex(pairs_tex, file.path(tables_dir, "tab_desc_price_pairs.tex"))
excel_sheets$price_tests <- tests
excel_sheets$price_pairs <- pairwise

# 3. Barriers to the demand (Q19a, b and c) ------------------------------------
barrier_results <- tibble()
for (q in seq_len(nrow(BARRIER_QUESTIONS))) {
  barrier_question <- BARRIER_QUESTIONS[q, ]
  # The option of each column, from the parts its name was built from
  barrier_variables <- paste0(barrier_question$prefix, names(BARRIER_LABELS))
  option_of_variable <- names(BARRIER_LABELS)
  names(option_of_variable) <- barrier_variables

  barrier_shares <- multiple_choice_shares(experts, barrier_variables) %>%
    mutate(option = unname(option_of_variable[variable]),
           share = if_else(n >= MIN_N, share, NA_real_))
  # Experts shown the question who ticked no option, not even "I see no
  # barriers", count as seeing none of the barriers. If the question was not
  # mandatory, they may have skipped it.
  ticked <- rowSums(experts[barrier_variables] == "Ja", na.rm = TRUE)
  shown <- rowSums(!is.na(experts[barrier_variables])) > 0
  cat(barrier_question$question, "- shown, but no option ticked:",
      sum(shown & ticked == 0), "of", sum(shown), "experts\n")

  # The options by their share among all experts, "Other" and "I see no
  # barriers" at the end
  options_by_share <- barrier_shares %>%
    filter(group == ALL_EXPERTS, !(option %in% c("other", "none"))) %>%
    arrange(desc(share))
  option_order <- c(options_by_share$option, "other", "none")

  # Every option in every group; a share left out (NA) draws no bar
  plot_data <- barrier_shares %>%
    filter(group != ALL_EXPERTS) %>%
    mutate(item = factor(unname(BARRIER_LABELS[option]),
                         levels = unname(BARRIER_LABELS[option_order])),
           group = factor(group, levels = GROUP_LABELS))
  plot_barriers <- plot_group_shares(
    plot_data, x_title = "Share of the experts who answered (%)"
  )
  save_figure_pdf(plot_barriers,
                  file.path(figures_dir, paste0("fig_desc_",
                                                barrier_question$file, ".pdf")),
                  FIGURE_SIZES$full)

  # The experts who answered the question, per group: the same for every
  # option
  answered <- barrier_shares %>%
    group_by(group) %>%
    summarise(n = max(n), .groups = "drop") %>%
    mutate(item = "answered", text = latex_number(n, 0))
  barrier_cells <- group_cells(
    barrier_shares %>%
      mutate(item = option, text = statistic_text(100 * share, 1)),
    items = option_order,
    labels = latex_text(unname(BARRIER_LABELS[option_order]))
  )
  answered_cells <- group_cells(answered, items = "answered",
                                labels = "Experts who answered")
  question_text <- paste0(
    barrier_question$question, ": thinking of the demand for ",
    barrier_question$demand_for, " in existing buildings among your ",
    "customers, which barriers do you see for a positive development? ",
    "Several answers possible."
  )
  barrier_tex <- group_table_tex(
    barrier_cells, header = group_header(),
    caption = barrier_question$caption,
    label = paste0("tab:desc_", barrier_question$file),
    notes = paste0(latex_text(paste0(
      sample_text, " ", question_text, " Share of the experts who ",
      "answered, in percent."
    )), dash_note(barrier_cells)),
    cells_below = answered_cells
  )
  write_tex(barrier_tex, file.path(tables_dir, paste0("tab_desc_",
                                                      barrier_question$file,
                                                      ".tex")))
  numbers[[barrier_question$note]] <- latex_text(paste0(
    question_text, " ", sample_text, " ",
    latex_number(answered$n[answered$group == ALL_EXPERTS], 0), " answered."
  ))
  barrier_results <- bind_rows(barrier_results, barrier_shares %>%
                                 mutate(question = barrier_question$question,
                                        .before = 1))
}
excel_sheets$barriers <- barrier_results

# 4. What matters most to the customers (Q28) ----------------------------------
# Q28 asks for three criteria: ranks 4 to 8 should be empty
more_ranks <- paste0("crit_rank", 4:8)
n_more_ranks <- sum(rowSums(!is.na(experts[more_ranks])) > 0)
cat("Experts with a criterion of Q28 on rank 4 to 8:", n_more_ranks, "\n")

# One row per expert x criterion named, with its weight (ranking_weights() of
# the summary statistics); the share naming it and its mean weight are taken
# over the experts who named at least one
criteria_named <- ranking_weights(experts, CRITERIA_RANKS, by = "group") %>%
  mutate(group = as.character(group))
criteria_named_all_experts <- criteria_named %>%
  mutate(group = ALL_EXPERTS)
criteria_named <- bind_rows(criteria_named, criteria_named_all_experts)
unknown_criteria <- setdiff(unique(criteria_named$option),
                            names(CRITERION_LABELS))
if (length(unknown_criteria) > 0) {
  stop("Criteria of Q28 without a label: ",
       paste(unknown_criteria, collapse = ", "))
}

experts_ranking <- criteria_named %>%
  group_by(group) %>%
  summarise(n = n_distinct(resp_uid), .groups = "drop")
criteria_counts <- criteria_named %>%
  group_by(group, option) %>%
  summarise(n_named = n_distinct(resp_uid), weight_sum = sum(weight),
            .groups = "drop")
# Every criterion in every group, also those nobody named there
criteria <- tibble()
for (group_name in GROUP_COLUMNS) {
  criteria <- bind_rows(criteria, tibble(group = group_name,
                                         option = names(CRITERION_LABELS)))
}
criteria <- criteria %>%
  left_join(criteria_counts, by = c("group", "option")) %>%
  left_join(experts_ranking, by = "group") %>%
  mutate(n_named = coalesce(n_named, 0L),
         weight_sum = coalesce(weight_sum, 0),
         n = coalesce(n, 0L),
         share = if_else(n >= MIN_N, n_named / n, NA_real_),
         weight = if_else(n >= MIN_N, weight_sum / n, NA_real_))

criteria_by_share <- criteria %>%
  filter(group == ALL_EXPERTS) %>%
  arrange(desc(share))
criterion_order <- criteria_by_share$option

# Every criterion in every group; a share left out (NA) draws no bar
plot_data <- criteria %>%
  filter(group != ALL_EXPERTS) %>%
  mutate(item = factor(unname(CRITERION_LABELS[option]),
                       levels = unname(CRITERION_LABELS[criterion_order])),
         group = factor(group, levels = GROUP_LABELS))
plot_criteria <- plot_group_shares(
  plot_data, x_title = "Share of the experts naming it (%)"
)
save_figure_pdf(plot_criteria,
                file.path(figures_dir, "fig_desc_customer_criteria.pdf"),
                FIGURE_SIZES$full)

criteria_question <- paste0("Q28: which three of the following criteria ",
                            "matter most to your customers?")
criterion_labels <- latex_text(unname(CRITERION_LABELS[criterion_order]))
experts_ranking_cells <- group_cells(
  experts_ranking %>%
    mutate(item = "ranking", text = latex_number(n, 0)),
  items = "ranking", labels = "Experts who named a criterion"
)

# The share of experts naming each criterion (main part)
share_cells <- group_cells(
  criteria %>%
    mutate(item = option, text = statistic_text(100 * share, 1)),
  items = criterion_order, labels = criterion_labels
)
criteria_tex <- group_table_tex(
  share_cells, header = group_header(),
  caption = "What matters most to the customers",
  label = "tab:desc_customer_criteria",
  notes = paste0(latex_text(paste0(
    sample_text, " ", criteria_question, " Share of the experts naming the ",
    "criterion, in percent, among those who named at least one."
  )), dash_note(share_cells)),
  cells_below = experts_ranking_cells
)
write_tex(criteria_tex, file.path(tables_dir, "tab_desc_customer_criteria.tex"))

# The mean weight of each criterion (appendix): the ranking of an expert in
# weights that sum to 1, as in the summary statistics
weight_cells <- group_cells(
  criteria %>%
    mutate(item = option, text = statistic_text(100 * weight, 1)),
  items = criterion_order, labels = criterion_labels
)
weights_tex <- group_table_tex(
  weight_cells, header = group_header(),
  caption = "What matters most to the customers: weights of the ranking",
  label = "tab:desc_customer_weights",
  notes = paste0(latex_text(paste0(
    sample_text, " ", criteria_question, " The ranking of each expert in ",
    "weights that sum to 1: of k criteria named, rank r gets ",
    "(k + 1 - r) / (k (k + 1) / 2), that is 3/6, 2/6 and 1/6 for three. Mean ",
    "weight, in percent, over the experts who named at least one; each ",
    "column sums to 100."
  )), dash_note(weight_cells)),
  cells_below = experts_ranking_cells
)
write_tex(weights_tex, file.path(tables_dir, "tab_desc_customer_weights.tex"))

numbers[["descNoteCustomerCriteria"]] <- latex_text(paste0(
  criteria_question, " ", sample_text, " Share of the experts naming the ",
  "criterion among the ",
  latex_number(experts_ranking$n[experts_ranking$group == ALL_EXPERTS], 0),
  " who named at least one."
))
# The workbook leaves out what the tables leave out: the counts behind a
# share that rests on fewer than MIN_N experts
excel_sheets$customer_criteria <- criteria %>%
  mutate(n_named = if_else(n >= MIN_N, n_named, NA_integer_),
         weight_sum = if_else(n >= MIN_N, weight_sum, NA_real_))
