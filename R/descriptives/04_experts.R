################################################################################
# 04_experts.R — who the experts are, and their characteristics in the models
# Inputs : experts, price_reference, outlier_counts, share_sums (02_data.R)
# Outputs: tables/tab_desc_experts.tex (who answered: the expert and the
#          firm); tab_desc_prices_heating.tex (the prices, the climate and
#          the heating, as they enter the models); tab_desc_advice.tex (the
#          households advised, the heat pumps among the recommendations and
#          the installations); tab_desc_technologies.tex (all technologies
#          among them); the same four per group, tab_desc_<table>_<group>.tex;
#          tab_desc_prices_groups.tex, tab_desc_heating_groups.tex (the means
#          and shares of tab_desc_prices_heating, a column per group);
#          tab_desc_price_cleaning.tex, tab_desc_other_cleaning.tex
# Checks : every answer option of a table is an answer of the data, and the
#          options of a question are all shown; every price answer has
#          exactly one treatment
################################################################################

# 1. Summary statistics, all experts and each group ----------------------------
# Continuous answers: N, mean, SD and quartiles; answer options: N and the
# share, those of two questions side by side below the other rows. The same
# four tables for all experts (the main tables) and for each group (the
# appendix). The notes are LaTeX.
# Who was asked Q10 and Q11b, and how a blank share counts: in the notes of
# both tables of the advice and the installations
routing_text <- paste0(
  "Q10: firms offering energy advice or chimney sweeping; Q11b: HVAC firms. ",
  "A blank share is 0 if other shares were given."
)
summary_tables <- list(
  experts = list(
    rows = EXPERT_ROWS,
    caption = "The experts and their firms",
    notes = paste0(
      "Age: ", SURVEY_YEAR, " minus the year of birth. Years without ",
      "impossible values (birth outside ", VALID_RANGE$dem_birthyear[1], "--",
      VALID_RANGE$dem_birthyear[2], ", training outside ",
      VALID_RANGE$dem_training_year[1], "--", VALID_RANGE$dem_training_year[2],
      "); years and employees without the ", 100 * TRIM_QUANTILES[1],
      "\\% lowest and highest answers of all experts. Shares among the ",
      "experts who answered."
    )
  ),
  prices_heating = list(
    rows = bind_rows(PRICE_ROWS, HEATING_ROWS),
    caption = "Energy prices, climate and heating",
    notes = paste0(
      "Price (Q15): per kWh of the energy carrier, for district heating per ",
      "kWh of heat; answers per litre of heating oil count as 10 kWh and per ",
      "kg of pellets as 5 kWh, the questionnaire's rules of thumb; cleaned as ",
      "in the models. Degree days of the firm's NUTS3 region."
    )
  ),
  advice = list(
    rows = ADVICE_ROWS,
    caption = "Advice and installations",
    notes = paste0(
      routing_text, " Shares used as reported, also where they do not sum to ",
      "100, and not trimmed; households advised without the ",
      100 * TRIM_QUANTILES[1], "\\% lowest and highest answers of all ",
      "experts. The questionnaire does not ask how many heating systems a ",
      "firm installed."
    )
  ),
  technologies = list(
    rows = TECHNOLOGY_ROWS,
    caption = "Recommended and installed technologies",
    notes = paste0(routing_text, " Shares as reported.")
  )
)

# A misspelt answer option would show 0 percent: checked once, on all experts
for (table_name in names(summary_tables)) {
  check_share_rows(experts, summary_tables[[table_name]]$rows, complete = TRUE)
}

table_samples <- c(ALL_EXPERTS, unname(GROUP_LABELS))
summary_results <- tibble()
for (sample_name in table_samples) {
  if (sample_name == ALL_EXPERTS) {
    sample_experts <- experts
    file_suffix <- ""
    caption_suffix <- ""
    sample_text <- paste0("Completed questionnaires of energy advisers, ",
                          "HVAC installers and chimney sweeps.")
  } else {
    sample_experts <- experts %>%
      filter(group == sample_name)
    # folder_name(): "Energy advisers" -> "energy_advisers"
    file_suffix <- paste0("_", folder_name(sample_name))
    caption_suffix <- paste0(": ", GROUP_LABELS_IN_TEXT[[sample_name]])
    sample_text <- paste0("Completed questionnaires of ",
                          GROUP_LABELS_IN_TEXT[[sample_name]], ".")
  }

  for (table_name in names(summary_tables)) {
    table_spec <- summary_tables[[table_name]]
    stats <- summary_statistics(sample_experts, table_spec$rows)
    # A row of kind mean, year or share without a single answer in the group
    # (a question the group was not asked) is left out of its table; the
    # workbook keeps it. The answer options below a heading (level) keep
    # their rows, so that no heading is left without them; a panel without
    # rows is left out with its title.
    table_stats <- stats %>%
      filter(!(kind %in% c("mean", "year", "share") & n == 0))
    notes <- paste0(latex_text(sample_text), " ", table_spec$notes)
    lines <- summary_table_tex(
      table_stats, n_experts = nrow(sample_experts),
      caption = paste0(table_spec$caption, caption_suffix),
      label = paste0("tab:desc_", table_name, file_suffix),
      notes = notes
    )
    write_tex(lines, file.path(tables_dir, paste0("tab_desc_", table_name,
                                                  file_suffix, ".tex")))
    summary_results <- bind_rows(summary_results, stats %>%
                                   mutate(table = table_name,
                                          sample = sample_name,
                                          experts = nrow(sample_experts),
                                          .before = 1))
  }
}
excel_sheets$summary_statistics <- summary_results

# 1.2 Energy prices, climate and heating by group ------------------------------
# The rows of tab_desc_prices_heating with a column per group and all experts:
# the means of the prices and the degree days, the shares of the answers. N,
# SD and quartiles are in the tables of each group. An answer option goes into
# a panel titled by its question. Two tables, so that each fits a frame: the
# prices and the degree days, and the answers to Q6a and Q20.
question_headings <- summary_tables$prices_heating$rows %>%
  filter(kind == "heading") %>%
  select(panel, heading = label)
by_group_rows <- summary_tables$prices_heating$rows %>%
  filter(kind != "heading") %>%
  left_join(question_headings, by = "panel", relationship = "many-to-one") %>%
  # coalesce(): the first value that is not NA, here the question's heading
  # for an answer option and the panel as it is for the other rows
  mutate(panel = coalesce(heading, panel)) %>%
  select(-heading)

by_group_cells <- tibble(panel = by_group_rows$panel,
                         label = latex_text(by_group_rows$label))
by_group_experts <- tibble(panel = NA_character_, label = "Experts")
by_group_results <- tibble()
for (group_name in GROUP_COLUMNS) {
  if (group_name == ALL_EXPERTS) {
    group_experts <- experts
  } else {
    group_experts <- experts %>%
      filter(group == group_name)
  }
  group_stats <- summary_statistics(group_experts, by_group_rows)
  # Each row with its own decimals
  group_text <- character(nrow(group_stats))
  for (k in seq_len(nrow(group_stats))) {
    group_text[k] <- statistic_text(group_stats$mean[k], group_stats$digits[k])
  }
  by_group_cells[[group_name]] <- group_text
  by_group_experts[[group_name]] <- latex_number(nrow(group_experts), 0)
  by_group_results <- bind_rows(by_group_results, group_stats %>%
                                  mutate(group = group_name, .before = 1))
}
by_group_tables <- list(
  prices_groups = list(
    cells = by_group_cells %>%
      filter(panel %in% c(PRICE_PANEL, "")),
    caption = "Energy prices and climate by group",
    notes = paste0(
      "Means. Prices per kWh of the energy carrier, for district heating per ",
      "kWh of heat, cleaned as in the models; degree days of the firm's NUTS3 ",
      "region."
    )
  ),
  heating_groups = list(
    cells = by_group_cells %>%
      filter(!(panel %in% c(PRICE_PANEL, ""))),
    caption = "District heating and heating at home by group",
    notes = paste0(
      "Shares of the experts who answered, in percent. District heating ",
      "available or planned (Q6a) leads to the vignettes with district ",
      "heating as an option."
    )
  )
)
for (table_name in names(by_group_tables)) {
  table_spec <- by_group_tables[[table_name]]
  by_group_tex <- group_table_tex(
    table_spec$cells, header = group_header(),
    caption = table_spec$caption,
    label = paste0("tab:desc_", table_name),
    notes = paste0(latex_text(paste0(
      "Completed questionnaires of energy advisers, HVAC installers and ",
      "chimney sweeps. ", table_spec$notes, " N, SD and quartiles in the ",
      "tables of each group."
    )), dash_note(table_spec$cells)),
    cells_below = by_group_experts
  )
  write_tex(by_group_tex,
            file.path(tables_dir, paste0("tab_desc_", table_name, ".tex")))
}
excel_sheets$prices_heating_groups <- by_group_results

numbers[["descExpertsAdvisers"]] <-
  latex_number(sum(experts$group == "Energy advisers"), 0)
numbers[["descExpertsHvac"]] <-
  latex_number(sum(experts$group == "HVAC installers"), 0)
numbers[["descExpertsSweeps"]] <-
  latex_number(sum(experts$group == "Chimney sweeps"), 0)

# 2. Cleaning of the prices ----------------------------------------------------
# The treatments of clean_reported_prices(), and their columns here
TREATMENT_COLUMNS <- c(
  "blank" = "blank",
  "as reported" = "as_reported",
  "euro" = "euro",
  "per litre or kg" = "per_unit",
  "euro and per litre or kg" = "euro_and_per_unit",
  "not usable" = "not_usable"
)
treatment_counts <- tibble()
for (fuel in PRICE_FUELS) {
  treatment <- experts[[paste0("price_", fuel, "_treatment")]]
  unknown_treatments <- setdiff(unique(treatment), names(TREATMENT_COLUMNS))
  if (length(unknown_treatments) > 0) {
    stop("Unknown treatment of the price of ", fuel, ": ",
         paste(unknown_treatments, collapse = ", "))
  }
  counts <- tibble(fuel = fuel, experts = length(treatment))
  for (treatment_level in names(TREATMENT_COLUMNS)) {
    counts[[TREATMENT_COLUMNS[[treatment_level]]]] <-
      sum(treatment == treatment_level)
  }
  # Each answer has exactly one treatment
  stopifnot(sum(counts[unname(TREATMENT_COLUMNS)]) == counts$experts)
  treatment_counts <- bind_rows(treatment_counts, counts)
}
price_cleaning_summary <- treatment_counts %>%
  left_join(price_reference, by = "fuel", relationship = "one-to-one") %>%
  mutate(answered = experts - blank,
         usable = answered - not_usable)
stopifnot(identical(price_cleaning_summary$fuel, PRICE_FUELS))

# Rules (2) and (3) apply to oil and pellets only: an empty cell elsewhere
is_per_unit_fuel <- PRICE_FUELS %in% c("oil", "pellet")
per_unit_text <- function(counts) {
  text <- latex_number(counts, 0)
  text[!is_per_unit_fuel] <- ""
  text
}
cleaning <- price_cleaning_summary
price_cleaning_body <- c(
  table_line(c("Left blank", latex_number(cleaning$blank, 0))),
  table_line(c("Answered", latex_number(cleaning$answered, 0))),
  table_line(c("\\quad used as reported",
               latex_number(cleaning$as_reported, 0))),
  table_line(c(paste0("\\quad below ", PRICE_EURO_BELOW, ": euro, times 100"),
               latex_number(cleaning$euro, 0))),
  table_line(c("\\quad per litre or kg, divided by 10 or 5",
               per_unit_text(cleaning$per_unit))),
  table_line(c("\\quad both of the above",
               per_unit_text(cleaning$euro_and_per_unit))),
  table_line(c(paste0("\\quad more than ", PRICE_VALID_FACTOR,
                      " times from the median: not usable"),
               latex_number(cleaning$not_usable, 0))),
  table_line(c("Usable", latex_number(cleaning$usable, 0))),
  PANEL_SPACE,
  table_line(c("Median after conversion", latex_number(cleaning$median, 1))),
  table_line(c("Usable range", paste0(latex_number(cleaning$lower, 1), "--",
                                      latex_number(cleaning$upper, 1)))),
  table_line(c("Mean of the usable answers", latex_number(cleaning$mean, 1)))
)
fuel_header_cells <- character(length(PRICE_FUELS))
for (f in seq_along(PRICE_FUELS)) {
  fuel_header_cells[f] <- header_cell(FUEL_HEADERS[[PRICE_FUELS[f]]])
}

price_cleaning_tex <- table_float(
  column_spec = paste0("p{", LABEL_WIDTH_CLEANING, "}ccccc"),
  header = paste0(" & ", paste(fuel_header_cells, collapse = " & "), " \\\\"),
  body = price_cleaning_body,
  caption = "Cleaning of the reported energy prices",
  label = "tab:desc_price_cleaning",
  notes = latex_text(paste0(
    "Completed questionnaires of energy advisers, HVAC installers and ",
    "chimney sweeps (", latex_number(nrow(experts), 0), "). Q15 asks for ",
    "the price per kWh of the energy carrier, in cent, with the rules of ",
    "thumb 1 litre of heating oil is about 10 kWh and 1 kg of pellets about ",
    "5 kWh. The rules of the models, in this order: below ",
    PRICE_EURO_BELOW, " read as euro; oil from ", OIL_PER_LITRE[1], " to ",
    OIL_PER_LITRE[2], " as cent per litre (10 kWh), pellets from ",
    PELLETS_PER_KG[1], " to ", PELLETS_PER_KG[2], " as cent per kg (5 kWh); ",
    "more than ", PRICE_VALID_FACTOR, " times from the median: not ",
    "reported. Median, range and mean in cent per kWh."
  )),
  font_size = TABLE_FONT_SIZE
)
write_tex(price_cleaning_tex,
          file.path(tables_dir, "tab_desc_price_cleaning.tex"))
excel_sheets$price_cleaning <- price_cleaning_summary

# 3. Cleaning of the other continuous answers ----------------------------------
other_cleaning_cells <- outlier_counts %>%
  transmute(label = latex_text(unname(OUTLIER_VARIABLES[variable])),
            answered = latex_number(answered, 0),
            impossible = latex_number(impossible, 0),
            trimmed = latex_number(trimmed, 0),
            used = latex_number(used, 0))
# Only a variable with a VALID_RANGE can have impossible values
has_range <- outlier_counts$variable %in% names(VALID_RANGE)
other_cleaning_cells$impossible[!has_range] <- ""

share_sum_text <- character(nrow(share_sums))
for (k in seq_len(nrow(share_sums))) {
  share_sum_text[k] <- paste0(
    latex_number(share_sums$not_100[k], 0), " of the ",
    latex_number(share_sums$answered[k], 0), " experts who gave shares in ",
    share_sums$question[k]
  )
}
other_cleaning_tex <- table_float(
  column_spec = paste0("p{", LABEL_WIDTH_CLEANING, "}cccc"),
  header = " & Answered & Impossible & Trimmed & Used \\\\",
  body = cell_rows(other_cleaning_cells),
  caption = "Cleaning of the other continuous answers",
  label = "tab:desc_other_cleaning",
  notes = latex_text(paste0(
    "Completed questionnaires of energy advisers, HVAC installers and ",
    "chimney sweeps. Impossible: year of birth outside ",
    VALID_RANGE$dem_birthyear[1], " to ", VALID_RANGE$dem_birthyear[2],
    ", year of training outside ", VALID_RANGE$dem_training_year[1], " to ",
    VALID_RANGE$dem_training_year[2], ". Trimmed: below the ",
    100 * TRIM_QUANTILES[1], "% or above the ", 100 * TRIM_QUANTILES[2],
    "% quantile of the remaining answers of all experts. The shares of the ",
    "technologies (Q10b, Q11b) should sum to 100 but do not for ",
    paste(share_sum_text, collapse = ", and for "), "; they are used as ",
    "reported."
  )),
  font_size = TABLE_FONT_SIZE
)
write_tex(other_cleaning_tex,
          file.path(tables_dir, "tab_desc_other_cleaning.tex"))
excel_sheets$other_cleaning <- outlier_counts
excel_sheets$shares_not_100 <- share_sums
