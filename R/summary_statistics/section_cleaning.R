################################################################################
# section_cleaning.R — Block 0. Data cleaning, for the data appendix
# Inputs : REPORT_DIR/cleaning_report.rds (R/cleaning/07_report.R)
# Outputs: in dirs$tables: tab_clean_sample.tex, tab_clean_groups.tex,
#          tab_clean_prices.tex, tab_clean_outliers.tex
# Checks : none; the report checks its tables
################################################################################

#' The tables of the data appendix on what the cleaning did. Counts may be small
#' (counts of what a rule did); the means respect the minimum cell in the report.
#' tab_clean_prices.tex needs makecell in the preamble, besides booktabs and
#' threeparttable.
#'
#' @param dirs list with the folder tables
#' @return one exhibit_row() per table; tibble() if the report does not exist
section_cleaning <- function(dirs) {
  report_file <- file.path(REPORT_DIR, "cleaning_report.rds")
  if (!file.exists(report_file)) {
    return(tibble())
  }
  report <- readRDS(report_file)
  # The rules of the run, as they are in the report
  settings <- report$settings
  setting_value <- function(setting_name) {
    settings$value[settings$setting == setting_name]
  }

  # 1. From the questionnaires to the sample -----------------------------------
  overview <- report$overview
  sample_cells <- tibble(
    step = latex_text(overview$step),
    all = fmt_int(overview$all),
    arm_fw = fmt_int(overview$arm_fw),
    arm_nofw = fmt_int(overview$arm_nofw),
    arm_unknown = fmt_int(overview$arm_unknown),
    arm_both = fmt_int(overview$arm_both)
  )
  # rle(): the runs of equal values, here the rows of each panel
  unit_runs <- rle(overview$unit)
  unit_panels <- unit_runs$lengths
  names(unit_panels) <- latex_text(unit_runs$values)
  table_custom(
    sample_cells,
    file = file.path(dirs$tables, "tab_clean_sample.tex"),
    notes = latex_text(paste0(
      "A person is one invitation: a person who started twice keeps the ",
      "furthest attempt, completed before not completed, then the highest ",
      "page. A token is per survey, so someone invited in two waves counts ",
      "twice. The arm is observed from the vignettes answered or, without any, ",
      "routed from Q6a. Unknown: no vignette and no answer to Q6a. Both: an ",
      "expert who answered in both arms, left out of the vignettes."
    )),
    col_names = c("", "All", "With", "Without", "Unknown", "Both"),
    header = c(" " = 2, "District-heating option" = 4),
    panels = unit_panels
  )

  # 2. The expert groups -------------------------------------------------------
  # The combinations of categories in Q2 are many: in the report only
  groups <- report$expert_groups %>%
    filter(part != "Every combination of categories in Q2")
  group_cells <- tibble(
    category = latex_text(groups$category),
    persons = fmt_int(groups$persons),
    completed = fmt_int(groups$completed)
  )
  part_runs <- rle(groups$part)
  part_panels <- part_runs$lengths
  names(part_panels) <- latex_text(part_runs$values)
  table_custom(
    group_cells,
    file = file.path(dirs$tables, "tab_clean_groups.tex"),
    notes = latex_text(paste0(
      "One row per person. Q2 asks for the categories of the firm and allows ",
      "several ticks. Every person gets one group, by this priority: HVAC ",
      "installers if ticked ``sanitary, heating, air conditioning''; then ",
      "chimney sweeps if ticked ``chimney sweep''; then energy advisers if ",
      "ticked ``energy advice office'' or ``architecture or engineering ",
      "office'', or if the free text of ``other'' is filled in. A person who ",
      "did not answer Q2, or submitted it without any category, has no group ",
      "(NA). Set by the project team, September 2026."
    )),
    col_names = c("", "Persons", "Completed"),
    panels = part_panels
  )

  # 3. The prices --------------------------------------------------------------
  prices <- report$prices
  fuels <- unique(prices$fuel)
  # A row of the table: its label, and a value for each fuel
  price_row <- function(label, values) {
    row <- tibble(label = latex_text(label))
    for (k in seq_along(fuels)) {
      row[[fuels[k]]] <- values[k]
    }
    row
  }
  price_cells <- tibble()
  panel_sizes <- integer(0)
  for (sample_name in unique(prices$sample)) {
    in_sample <- prices[prices$sample == sample_name, ]
    # Rules (2) and (3) apply to oil and pellets only: an empty cell elsewhere
    is_per_unit_fuel <- in_sample$fuel %in% c("oil", "pellet")
    per_unit <- fmt_int(in_sample$per_litre_or_kg)
    per_unit[!is_per_unit_fuel] <- ""
    euro_and_per_unit <- fmt_int(in_sample$euro_and_per_litre_or_kg)
    euro_and_per_unit[!is_per_unit_fuel] <- ""
    sample_rows <- bind_rows(
      price_row("Left blank", fmt_int(in_sample$blank)),
      price_row("Answered", fmt_int(in_sample$answered)),
      price_row("\\quad used as reported", fmt_int(in_sample$as_reported)),
      price_row("\\quad read as euro", fmt_int(in_sample$euro)),
      price_row("\\quad read per litre or kg", per_unit),
      price_row("\\quad both of the above", euro_and_per_unit),
      price_row("\\quad not usable", fmt_int(in_sample$not_usable)),
      price_row("Usable", fmt_int(in_sample$usable)),
      price_row("Mean of the answers as reported",
                fmt_num(in_sample$mean_before, 1)),
      price_row("Mean of the usable answers", fmt_num(in_sample$mean_after, 1))
    )
    price_cells <- bind_rows(price_cells, sample_rows)
    panel_sizes <- c(panel_sizes, nrow(sample_rows))
  }
  # The same in both samples: the median of those who completed with a group
  in_sample <- prices[prices$sample == unique(prices$sample)[1], ]
  rule_rows <- bind_rows(
    price_row("Median after conversion", fmt_num(in_sample$median, 1)),
    price_row("Usable range", paste0(fmt_num(in_sample$lower, 1), "--",
                                     fmt_num(in_sample$upper, 1)))
  )
  price_cells <- bind_rows(price_cells, rule_rows)
  names(panel_sizes) <- latex_text(unique(prices$sample))
  price_panels <- c(panel_sizes, "The rule" = nrow(rule_rows))
  # gsub(): a header of two words on two lines
  fuel_headers <- paste0("\\makecell{", gsub(" ", "\\\\\\\\", in_sample$label), "}")
  table_custom(
    price_cells,
    file = file.path(dirs$tables, "tab_clean_prices.tex"),
    notes = latex_text(paste0(
      "Q15 asks for the price per kWh of the energy carrier in January and ",
      "February 2026, in cent, with the rules of thumb that 1 litre of heating ",
      "oil is about 10 kWh and 1 kg of pellets about 5 kWh. The rules, in this ",
      "order: below ", setting_value("price_euro_below"), " read as euro, times ",
      "100; oil from ", setting_value("oil_per_litre"), " read as cent per ",
      "litre, divided by 10; pellets from ", setting_value("pellets_per_kg"),
      " read as cent per kg, divided by 5; more than ",
      setting_value("price_valid_factor"), " times above or below the median: ",
      "not usable, counts as not reported. The median is that of the persons ",
      "who completed with an expert group, after the conversions, and applies ",
      "to everyone. Means, median and range in cent per kWh. ", cell_note()
    )),
    col_names = c("", fuel_headers),
    panels = price_panels
  )

  # 4. The other continuous answers --------------------------------------------
  outliers <- report$outliers
  # Only a variable with a valid range can have impossible values
  impossible <- fmt_int(outliers$impossible)
  impossible[is.na(outliers$valid_range)] <- ""
  outlier_cells <- tibble(
    label = latex_text(outliers$label),
    answered = fmt_int(outliers$answered),
    impossible = impossible,
    trimmed = fmt_int(outliers$trimmed),
    used = fmt_int(outliers$used)
  )
  ranges <- outliers[!is.na(outliers$valid_range), ]
  # tolower(): the label inside a sentence
  range_text <- paste(paste0(tolower(ranges$label), " outside ",
                             ranges$valid_range),
                      collapse = ", ")
  share_sums <- report$shares %>%
    distinct(question, persons_gave_shares, sum_not_100)
  share_text <- paste(paste0(fmt_int(share_sums$sum_not_100), " of the ",
                             fmt_int(share_sums$persons_gave_shares),
                             " persons who gave shares in ", share_sums$question),
                      collapse = ", and for ")
  table_custom(
    outlier_cells,
    file = file.path(dirs$tables, "tab_clean_outliers.tex"),
    notes = latex_text(paste0(
      "Everyone who answered, also those who did not complete, so that an ",
      "answer is kept or removed in every table alike. Impossible: ", range_text,
      ". Trimmed: below the ", setting_value("trim_lower_percent"),
      "% or above the ", setting_value("trim_upper_percent"), "% quantile of ",
      "the remaining answers. The shares of the technologies (Q10b, Q11b) ",
      "should sum to 100 but do not for ", share_text,
      "; they are used as reported."
    )),
    col_names = c("", "Answered", "Impossible", "Trimmed", "Used")
  )

  # 5. The list of exhibits ----------------------------------------------------
  block <- "0. Data cleaning"
  bind_rows(
    exhibit_row(block, "clean_sample", ALL_EXPERTS, "table",
                file.path("tables", "tab_clean_sample.tex"),
                "From the questionnaires started to the sample, by arm",
                "appendix"),
    exhibit_row(block, "clean_groups", ALL_EXPERTS, "table",
                file.path("tables", "tab_clean_groups.tex"),
                "Assignment of the expert groups", "appendix"),
    exhibit_row(block, "clean_prices", ALL_EXPERTS, "table",
                file.path("tables", "tab_clean_prices.tex"),
                "Cleaning of the reported energy prices", "appendix"),
    exhibit_row(block, "clean_outliers", ALL_EXPERTS, "table",
                file.path("tables", "tab_clean_outliers.tex"),
                "Cleaning of the other continuous answers", "appendix")
  )
}
