################################################################################
# section_prices_beliefs.R — Block F: the energy prices the experts report, the
#                            changes they expect, and the carbon price they know
# Inputs : experts (docs/data_contract.md)
# Outputs: tables/tab_prices_<group>.tex, tab_prices_groups_all.tex,
#          tab_price_expectations_<group>.tex, tab_price_tests_all.tex,
#          tab_carbon_price_<group>.tex;
#          figures/fig_price_distribution_<group>.pdf,
#          fig_price_expectations_<group>.pdf;
#          maps/map_price_state_gas_all.pdf, map_price_state_hpelec_all.pdf
# Checks : unknown answers (label_of()); the minimum cell
################################################################################
# summary_exhibits(), multiline_header(), stack_answers(), plot_stacked_shares(),
# plot_binned_histogram(): in section_experts_firms.R

# The p-value of the G-test comes from this many simulated tables, because some
# expected counts are small (old R/first_policy_brief/02_kl_divergence_prices.R).
# Fewer in a synthetic run, which only tests the code: on a busy machine r2dtable()
# takes half a minute per fuel, as it asks Windows for messages after each table.
N_SIMULATED_TABLES <- if (USE_SYNTHETIC_DATA) 500 else 10000
# Width of the bins of the reported prices, in cent per kWh
PRICE_BINWIDTH <- 1

#' G statistic of a table of counts, 2 x sum(O x log(O / E)), with E the expected
#' counts under independence; empty cells count 0
g_statistic <- function(counts) {
  # outer(): a matrix of all products of the row and the column totals
  expected <- outer(rowSums(counts), colSums(counts)) / sum(counts)
  has_count <- counts > 0
  2 * sum(counts[has_count] * log(counts[has_count] / expected[has_count]))
}

#' The energy prices and the beliefs about them
#'
#' @param experts one row per expert of the sample, as in experts.rds
#' @param dirs    the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_prices_beliefs <- function(experts, dirs) {
  block <- "F. Prices and beliefs"
  exhibits <- tibble()

  # 1. The reported prices, table ----------------------------------------------
  price_rows <- tibble(
    panel = "Price per kWh of the energy carrier, first two months of 2026 (cent)",
    variable = paste0("price_", names(FUEL_LABELS), "_kwh"), kind = "num",
    level = NA_character_, label = unname(FUEL_LABELS), digits = 1
  )
  price_notes <- paste(
    "Q15: what households among the expert's customers paid on average per kWh,",
    "left blank if none of them uses the energy carrier. Answers in euro, per",
    "liter of oil or per kg of pellets are converted; answers that cannot be read",
    "as a price per kWh, far from the median answer, are left out (cleaning",
    "report). These are beliefs of the experts, not trimmed further."
  )
  exhibits <- bind_rows(exhibits, summary_exhibits(
    experts, price_rows, "prices", "Reported energy prices", price_notes, block,
    "main", dirs
  ))

  # 2. Their distribution ------------------------------------------------------
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_experts <- subset_group(experts, group)
    values <- tibble(panel = character(), value = numeric())
    for (fuel in names(FUEL_LABELS)) {
      column <- paste0("price_", fuel, "_kwh")
      if (column %in% names(group_experts)) {
        values <- bind_rows(values, tibble(panel = FUEL_LABELS[[fuel]],
                                           value = group_experts[[column]]))
      }
    }
    values <- values %>%
      filter(!is.na(value)) %>%
      mutate(panel = factor(panel, levels = FUEL_LABELS))
    plot <- plot_binned_histogram(values, PRICE_BINWIDTH,
                                  "Reported price (cent per kWh)", n_columns = 1)
    if (is.null(plot)) {
      next
    }
    file_name <- paste0("fig_price_distribution_", group_slug(group), ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name), size = "tall")
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "price_distribution", group, "figure",
      file.path("figures", file_name), "Distribution of the reported energy prices",
      "appendix",
      note = paste(
        "Q15: reported prices, cleaned as in the table of the prices. Bins of",
        PRICE_BINWIDTH, "cent. A bin of fewer than", MIN_N, "answers is not drawn;",
        "the mean and the median are over all answers."
      )
    ))
  }

  # 3. The expected changes (Q17) ----------------------------------------------
  # One row per expert x fuel answered: a blank drops out for that fuel only
  expectation_answers <- tibble(resp_uid = character(), expert_group = factor(),
                                fuel = character(), answer = character())
  for (fuel in names(FUEL_LABELS)) {
    column <- paste0("pexp_", fuel)
    if (column %in% names(experts)) {
      answer <- as.character(experts[[column]])
      expectation_answers <- bind_rows(expectation_answers, tibble(
        resp_uid = experts$resp_uid, expert_group = experts$expert_group,
        fuel = fuel, answer = answer
      ))
    }
  }
  expectation_answers <- expectation_answers %>%
    filter(!is.na(answer)) %>%
    mutate(panel = "",
           bar = factor(label_of(fuel, FUEL_LABELS, "fuel"),
                        levels = unname(FUEL_LABELS)),
           segment = factor(label_of(answer, EXPECTATION_LABELS, "answer of Q17"),
                            levels = unname(EXPECTATION_LABELS)))
  expectation_notes <- paste(
    "Q17: the price of the energy carrier in five years, compared with the start",
    "of 2026. Shares among the experts who answered for the energy carrier, which",
    "they were to leave blank if it is not available in their area."
  )

  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_answers <- subset_group(expectation_answers, group)

    # Stacked bars
    bars <- stack_answers(group_answers)
    if (nrow(bars) > 0) {
      plot <- plot_stacked_shares(bars, EXPECTATION_COLOURS,
                                  "Share of the experts who answered")
      file_name <- paste0("fig_price_expectations_", group_slug(group), ".pdf")
      save_figure(plot, file.path(dirs$figures, file_name))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "price_expectations", group, "figure",
        file.path("figures", file_name), "Expected changes of the energy prices",
        if_else(group == ALL_EXPERTS, "main", "appendix"),
        note = paste(expectation_notes, "A bar of fewer than", MIN_N,
                     "answers is left out.")
      ))
    }

    # The same in numbers: one row per fuel, one column per answer
    n_answered <- integer(length(FUEL_LABELS))
    for (f in seq_along(FUEL_LABELS)) {
      n_answered[f] <- sum(group_answers$bar == FUEL_LABELS[f])
    }
    if (max(n_answered) < MIN_N) {
      next
    }
    cells <- tibble(fuel = latex_text(unname(FUEL_LABELS)),
                    n = if_else(n_answered >= MIN_N, fmt_int(n_answered), "--"))
    for (answer_label in EXPECTATION_LABELS) {
      shares <- rep("--", length(FUEL_LABELS))
      for (f in seq_along(FUEL_LABELS)) {
        if (n_answered[f] >= MIN_N) {
          in_bar <- group_answers$segment[group_answers$bar == FUEL_LABELS[f]]
          shares[f] <- fmt_pct(mean(in_bar == answer_label))
        }
      }
      cells[[answer_label]] <- shares
    }
    file_name <- paste0("tab_price_expectations_", group_slug(group), ".tex")
    table_custom(
      cells, file.path(dirs$tables, file_name),
      notes = latex_text(paste(expectation_notes, cell_note())),
      col_names = c("Energy carrier", "N", multiline_header(EXPECTATION_LABELS))
    )
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "price_expectations", group, "table",
      file.path("tables", file_name), "Expected changes of the energy prices",
      "appendix"
    ))
  }

  # 4. Do the groups differ in Q17? --------------------------------------------
  # G-test: do the three groups give the five answers in the same shares? It treats
  # the answers as unordered. A fuel for which a group has fewer than MIN_N answers
  # has no test: the comparison would describe too few experts.
  tested_answers <- expectation_answers %>%
    filter(!is.na(expert_group))
  test_rows <- tibble()
  # Reproducible p-values, with the seed of the test of the policy brief
  set.seed(20260921)
  for (fuel_name in names(FUEL_LABELS)) {
    fuel_answers <- tested_answers %>%
      filter(fuel == fuel_name)
    # table(): the counts of each group x answer; droplevels(): without the answers
    # and groups nobody gave or is in
    counts <- table(droplevels(fuel_answers$expert_group),
                    droplevels(fuel_answers$segment))
    if (nrow(counts) < length(EXPERT_GROUPS) || min(rowSums(counts)) < MIN_N) {
      test_rows <- bind_rows(test_rows, tibble(fuel = fuel_name))
      next
    }
    # r2dtable(): random tables with the same totals of rows and columns
    simulated_tables <- r2dtable(N_SIMULATED_TABLES, rowSums(counts),
                                 colSums(counts))
    simulated_g <- numeric(N_SIMULATED_TABLES)
    for (s in seq_len(N_SIMULATED_TABLES)) {
      simulated_g[s] <- g_statistic(simulated_tables[[s]])
    }
    observed_g <- g_statistic(counts)
    expected <- outer(rowSums(counts), colSums(counts)) / sum(counts)
    pearson <- sum((counts - expected)^2 / expected)
    test_rows <- bind_rows(test_rows, tibble(
      fuel = fuel_name, n = sum(counts), g = observed_g,
      df = (nrow(counts) - 1) * (ncol(counts) - 1),
      cramers_v = sqrt(pearson / (sum(counts) * (min(dim(counts)) - 1))),
      # The observed table counts as one of the tables, so that p is never 0
      p = (1 + sum(simulated_g >= observed_g)) / (1 + N_SIMULATED_TABLES)
    ))
  }
  if ("p" %in% names(test_rows)) {
    # p.adjust(): the p-values corrected for testing the five fuels;
    # coalesce(): "--" where a fuel has no test
    test_rows <- test_rows %>%
      mutate(p_holm = p.adjust(p, method = "holm"),
             p_text = if_else(p < 0.001, "$<$0.001", fmt_num(p, 3)),
             p_holm_text = if_else(p_holm < 0.001, "$<$0.001", fmt_num(p_holm, 3)))
    cells <- test_rows %>%
      transmute(fuel = latex_text(label_of(fuel, FUEL_LABELS, "fuel")),
                n = fmt_int(n), g = fmt_num(g, 1), df = fmt_int(df),
                cramers_v = fmt_num(cramers_v, 3),
                p = coalesce(p_text, "--"), p_holm = coalesce(p_holm_text, "--"))
    file_name <- paste0("tab_price_tests_", group_slug(ALL_EXPERTS), ".tex")
    table_custom(
      cells, file.path(dirs$tables, file_name),
      notes = latex_text(paste(
        "Q17, the experts of the three groups who answered for the energy carrier.",
        "G-test of independence: do the three groups give the five answers in the",
        "same shares? It ignores the order of the answers. p from",
        fmt_int(N_SIMULATED_TABLES), "simulated tables with the same totals of",
        "rows and columns. Holm: corrected across the energy carriers. Cramér's V:",
        "strength of the association between group and answer, from 0 (none) to 1.",
        "-- means fewer than", MIN_N, "answers in a group."
      )),
      col_names = c("Energy carrier", "N", "G", "df", "Cramér's V", "p", "p (Holm)")
    )
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "price_tests", ALL_EXPERTS, "table", file.path("tables", file_name),
      "Do the groups expect different price changes?", "appendix"
    ))
  }

  # 5. The carbon price --------------------------------------------------------
  carbon_rows <- tribble(
    ~panel,                   ~variable,            ~kind,   ~level, ~label,                                           ~digits,
    "Known (Q18)",            "co2_heard",          "level", "Ja",   "Has heard of the carbon price on heating fuels", 1,
    "Estimated price (Q18b)", "co2_est_low_clean",  "num",   NA,     "Lower end of the range (euro per tonne of CO₂)", 1,
    "Estimated price (Q18b)", "co2_est_high_clean", "num",   NA,     "Upper end of the range (euro per tonne of CO₂)", 1
  )
  carbon_notes <- paste(
    "Q18 asks whether the expert has heard of the carbon price on heating fuels",
    "in Germany; Q18b, only if so, for a rough range of its current level. The",
    "two ends without the 0.5% lowest and highest answers of all experts."
  )
  exhibits <- bind_rows(exhibits, summary_exhibits(
    experts, carbon_rows, "carbon_price", "The carbon price: known and estimated",
    carbon_notes, block, "appendix", dirs
  ))

  # 6. Prices by state (all experts only) --------------------------------------
  for (fuel in c("gas", "hpelec")) {
    column <- paste0("price_", fuel, "_kwh")
    if (!has_columns(experts, c(column, "bundesland"))) {
      next
    }
    state_values <- state_means(experts, experts[[column]], digits = 1)
    if (all(is.na(state_values$value))) {
      next
    }
    plot <- plot_state_map(state_values, "Cent per kWh",
                           legend_labels = label_number(accuracy = 0.1))
    id <- paste0("price_state_", fuel)
    file_name <- paste0("map_", id, "_", group_slug(ALL_EXPERTS), ".pdf")
    save_figure(plot, file.path(dirs$maps, file_name), size = "map")
    # str_to_lower(): the name of the fuel in small letters, inside the caption
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, id, ALL_EXPERTS, "map", file.path("maps", file_name),
      paste0("Mean reported price by state: ", str_to_lower(FUEL_LABELS[[fuel]])),
      "appendix",
      note = paste0("Q15, mean of the reported prices in cent per kWh by the ",
                    "state of the firm. A state with fewer than ", MIN_N,
                    " answers is gray, without a number.")
    ))
  }
  exhibits
}
