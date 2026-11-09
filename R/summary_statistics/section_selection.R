################################################################################
# section_selection.R — Block C. Selection: do those who never completed differ, on
#                       what they did answer, from those who completed?
# Inputs : experts and vignettes of the sample
# Outputs: in dirs$tables: tab_selection_<group>, tab_selection_state_<group>,
#          tab_selection_later_<group>, tab_selection_barriers_<group>,
#          tab_selection_vignettes_<group>
# Checks : unknown answers of a question with several levels stop; the minimum cell
#          in both columns of a row
################################################################################
# Only experts who reached a question count: its position in QUESTIONNAIRE at most
# last_question_position. The later a question, the fewer break-offs reached it,
# and those are selected themselves: the comparison is among those who got that far.
#
# The rows of all blocks together are too many for one table, in a paper or on a
# slide: the first table holds the firm, the invitation and the prices, the next
# ones the federal state, the later blocks and the barriers (`part` in
# selection_rows()).

#' The rows of one categorical question, one per answer
#'
#' @param part,panel the table and the panel the rows go into
#' @param item       the question, as in QUESTIONNAIRE
#' @param variable   the column
#' @param labels     named vector: answer as in the data -> label
#' @param prefix     text in front of each label
#' @return tibble with the columns of selection_rows()
level_rows <- function(part, panel, item, variable, labels, prefix = "") {
  tibble(part = part, panel = panel, item = item, variable = variable,
         kind = "level", level = names(labels), label = paste0(prefix, labels),
         digits = 1)
}

#' The rows of columns that are one line each: an option ticked ("Ja"), by default;
#' or a number or a 0/1 column, by `kind`
#'
#' @param part,panel,item as in level_rows()
#' @param variables the columns
#' @param labels    the label of each column
#' @param kind      "level", "num" or "dummy"
#' @param digits    decimals
option_rows <- function(part, panel, item, variables, labels, kind = "level",
                        digits = 1) {
  tibble(part = part, panel = panel, item = item, variable = variables, kind = kind,
         level = if_else(kind == "level", "Ja", NA_character_),
         label = unname(labels), digits = digits)
}

#' Every variable of the comparison, in the order of the questionnaire
#'
#' @return tibble, one row per line of a table:
#'   part      the table: "selection", "selection_state", "selection_later",
#'             "selection_barriers"
#'   panel     the panel of the table
#'   item      the question, as in QUESTIONNAIRE, whose position the expert must
#'             have reached; NA: known of everyone who started
#'   variable  the column of experts
#'   kind      "num": a number; "level": the share of answers equal to `level`;
#'             "dummy": a 0/1 column, as the share of ones
#'   level     for "level": the answer, exactly as in the data; else NA
#'   label     the row label, plain text with units
#'   digits    decimals
selection_rows <- function() {
  fuels <- names(FUEL_LABELS)
  technologies <- names(SHARE_TECHNOLOGY_LABELS)
  group_labels <- EXPERT_GROUPS
  names(group_labels) <- EXPERT_GROUPS
  is_answer <- names(OWN_HEATING_LABELS) != "Keine Angabe"

  first <- "selection"
  state <- "selection_state"
  later <- "selection_later"
  firm <- "The firm (Q0 to Q6a)"
  invitation <- "The invitation and the arm"
  prices <- "Energy and carbon prices (Q15 to Q18b)"
  installed <- "Heating systems installed in 2025 (Q11b, HVAC firms)"
  preferences <- "Preferences (scores from 0 to 1, at the end of the staircase)"
  advice <- "Services and advice (Q8, Q10, firms with advice)"
  customers <- "Main customers and their criteria (Q7, Q28, first rank)"
  person <- "The person (Q20 to Q24)"

  barrier_rows <- tibble()
  for (k in seq_len(nrow(BARRIER_QUESTIONS))) {
    barrier_rows <- bind_rows(barrier_rows, option_rows(
      "selection_barriers",
      paste0(BARRIER_QUESTIONS$question[k], ": ", BARRIER_QUESTIONS$caption[k]),
      paste0("time_", str_remove(BARRIER_QUESTIONS$prefix[k], "_$")),
      paste0(BARRIER_QUESTIONS$prefix[k], names(BARRIER_LABELS)),
      BARRIER_LABELS
    ))
  }

  bind_rows(
    option_rows(first, firm, "time_firm_zip", "hdd",
                "Heating degree days of the region", kind = "num", digits = 0),
    option_rows(first, firm, "time_resp_is_owner", "resp_is_owner",
                "Owner or managing director"),
    option_rows(first, firm, "time_firm_cat", names(FIRM_CATEGORY_LABELS),
                paste0("Q2: ", FIRM_CATEGORY_LABELS)),
    level_rows(first, firm, "time_firm_cat", "expert_group", group_labels,
               "Group: "),
    option_rows(first, firm, "time_firm_employees", "firm_employees_clean",
                "Employees", kind = "num"),
    level_rows(first, firm, "time_firm_revenue", "firm_revenue", REVENUE_LABELS,
               "Revenue: "),
    level_rows(first, firm, "time_firm_radius", "firm_radius_recoded",
               RADIUS_LABELS, "Service area: "),
    level_rows(first, firm, "time_firm_dh_in_area", "firm_dh_in_area",
               DISTRICT_HEATING_LABELS, "District heating in the area: "),

    level_rows(first, invitation, "time_firm_dh_in_area", "arm", ARM_LABELS,
               "Arm: "),
    option_rows(first, invitation, NA_character_, "is_softlaunch",
                "Invited in the soft launch", kind = "dummy"),
    level_rows(first, invitation, NA_character_, "target_group",
               TARGET_GROUP_LABELS, "Target group of the wave: "),

    option_rows(first, prices, "time_price", paste0("price_", fuels, "_kwh"),
                paste0("Price: ", FUEL_LABELS, " (cent/kWh)"), kind = "num"),
    option_rows(first, prices, "time_pexp", paste0("pexp_", fuels, "_rise"),
                paste0("Expects a rise: ", FUEL_LABELS), kind = "dummy"),
    option_rows(first, prices, "time_co2_heard", "co2_heard", "Carbon price known"),
    option_rows(first, prices, "time_co2_est", "co2_est_low_clean",
                "Carbon price estimated, low (euro/t)", kind = "num"),
    option_rows(first, prices, "time_co2_est", "co2_est_high_clean",
                "Carbon price estimated, high (euro/t)", kind = "num"),

    level_rows(state, "Federal state of the postcode (Q0)", "time_firm_zip",
               "bundesland", STATE_LABELS),

    option_rows(later, installed, "time_mix25",
                paste0("mix25_", technologies, "_filled"),
                paste0(SHARE_TECHNOLOGY_LABELS, " (share, %)"), kind = "num"),
    # The score of an expert who stops early is coarser: compared at the end of the
    # staircase
    option_rows(later, preferences, "time_tp_5", "tp_score", "Patience",
                kind = "num", digits = 2),
    option_rows(later, preferences, "time_risk_5", "risk_score", "Risk tolerance",
                kind = "num", digits = 2),
    option_rows(later, advice, "time_svc", names(SERVICE_LABELS),
                paste0("Service: ", SERVICE_LABELS)),
    option_rows(later, advice, "time_adv_households", "adv_households_clean",
                "Households advised", kind = "num"),
    option_rows(later, advice, "time_adv_share",
                paste0("adv_share_", technologies, "_filled"),
                paste0("Recommended: ", SHARE_TECHNOLOGY_LABELS, " (share, %)"),
                kind = "num"),
    level_rows(later, customers, "time_cust_rank", "cust_rank1", CUSTOMER_LABELS,
               "Customers: "),
    level_rows(later, customers, "time_crit_rank", "crit_rank1", CRITERION_LABELS,
               "Criterion: "),
    option_rows(later, person, "time_dem_birthyear", "age", "Age (years)",
                kind = "num"),
    level_rows(later, person, "time_dem_education", "dem_education_recoded",
               EDUCATION_LABELS, "Degree: "),
    option_rows(later, person, "time_dem_training_year", "dem_training_year_clean",
                "Year training was completed", kind = "num", digits = 0),
    option_rows(later, person, "time_cpd", names(TRAINING_LABELS),
                paste0("Continuing education: ", TRAINING_LABELS)),
    level_rows(later, person, "time_dem_own_heating", "dem_own_heating",
               OWN_HEATING_LABELS[is_answer], "Heating at home: "),

    barrier_rows
  )
}

#' The table that compares completers and break-offs on a list of rows
#'
#' @param experts one row per expert of this version of the exhibit
#' @param rows    rows of selection_rows()
#' @param file    path of the .tex file
#' @param note    the first part of the note, plain text
#' @return TRUE if the table was written; FALSE if no row has enough answers in
#'   both groups
write_selection_table <- function(experts, rows, file, note) {
  cells <- tibble()
  for (k in seq_len(nrow(rows))) {
    values <- experts[[rows$variable[k]]]
    if (rows$kind[k] == "level") {
      # NA stays NA: the share is among those who answered
      values <- as.numeric(as.character(values) == rows$level[k])
    } else {
      values <- as.numeric(values)
    }
    position <- 0
    if (!is.na(rows$item[k])) {
      position <- QUESTIONNAIRE$position[QUESTIONNAIRE$item == rows$item[k]]
    }
    has_reached <- experts$is_complete | experts$last_question_position >= position
    values[!has_reached] <- NA
    completers <- values[experts$is_complete & !is.na(values)]
    breakoffs <- values[!experts$is_complete & !is.na(values)]

    is_share <- rows$kind[k] != "num"
    digits <- rows$digits[k]
    row_cells <- tibble(label = latex_text(rows$label[k]), is_shown = FALSE)
    if (length(completers) >= MIN_N && length(breakoffs) >= MIN_N) {
      row_cells$is_shown <- TRUE
      column_values <- list(c = completers, b = breakoffs)
      for (column in names(column_values)) {
        x <- column_values[[column]]
        p25 <- quantile(x, 0.25, names = FALSE)
        p75 <- quantile(x, 0.75, names = FALSE)
        row_cells[[paste0("n_", column)]] <- fmt_int(length(x))
        if (is_share) {
          # A share says it all: no standard deviation or quartiles, as in
          # table_summary()
          row_cells[[paste0("mean_", column)]] <- fmt_pct(mean(x), digits)
          row_cells[[paste0("sd_", column)]] <- ""
          row_cells[[paste0("p25_", column)]] <- ""
          row_cells[[paste0("p50_", column)]] <- ""
          row_cells[[paste0("p75_", column)]] <- ""
        } else {
          row_cells[[paste0("mean_", column)]] <- fmt_num(mean(x), digits)
          row_cells[[paste0("sd_", column)]] <- fmt_num(sd(x), digits)
          row_cells[[paste0("p25_", column)]] <- fmt_num(p25, digits)
          row_cells[[paste0("p50_", column)]] <- fmt_num(median(x), digits)
          row_cells[[paste0("p75_", column)]] <- fmt_num(p75, digits)
        }
      }
      # The normalised difference (Imbens and Rubin): the difference of the means
      # over the root of the mean of the two variances, so that it does not grow
      # with N as a test statistic does
      pooled_sd <- sqrt((var(completers) + var(breakoffs)) / 2)
      normalised_difference <- if_else(
        pooled_sd > 0, (mean(completers) - mean(breakoffs)) / pooled_sd, NA_real_
      )
      if (is_share) {
        # Pearson, the square of the z test of two shares: no continuity correction.
        # chisq.test() warns of a rough approximation where an answer is rare, which
        # is expected here, and a p-value that is only descriptive can bear it.
        answer_counts <- matrix(c(sum(completers == 1), sum(completers == 0),
                                  sum(breakoffs == 1), sum(breakoffs == 0)),
                                nrow = 2)
        p_value <- suppressWarnings(
          chisq.test(answer_counts, correct = FALSE)$p.value
        )
      } else if (pooled_sd > 0) {
        # Welch: t.test() does not assume equal variances by default
        p_value <- t.test(completers, breakoffs)$p.value
      } else {
        p_value <- NA_real_
      }
      row_cells$difference <- fmt_num(normalised_difference, 2)
      row_cells$p <- if_else(!is.na(p_value) & p_value < 0.001, "$<$0.001",
                             fmt_num(p_value, 3))
    }
    cells <- bind_rows(cells, row_cells)
  }

  # A panel in which no row can be shown: too few break-offs reached it
  panels_shown <- unique(rows$panel[cells$is_shown])
  if (length(panels_shown) == 0) {
    return(FALSE)
  }
  is_kept <- rows$panel %in% panels_shown
  # A table of shares only needs no columns for numbers
  if (any(rows$kind[is_kept] == "num")) {
    statistics <- c("N", "Mean", "SD", "P25", "Median", "P75")
    table_cells <- cells[is_kept, ] %>%
      select(label, n_c, mean_c, sd_c, p25_c, p50_c, p75_c,
             n_b, mean_b, sd_b, p25_b, p50_b, p75_b, difference, p)
  } else {
    statistics <- c("N", "Share")
    table_cells <- cells[is_kept, ] %>%
      select(label, n_c, mean_c, n_b, mean_b, difference, p)
  }
  # A row below the minimum cell shows -- in every column
  for (column in names(table_cells)[-1]) {
    table_cells[[column]] <- coalesce(table_cells[[column]], "--")
  }
  # rle() counts the adjacent rows of each panel
  panel_runs <- rle(rows$panel[is_kept])
  panels <- panel_runs$lengths
  names(panels) <- latex_text(panel_runs$values)
  statistics_header <- c(1, length(statistics), length(statistics), 2)
  names(statistics_header) <- c(" ", "Completers", "Break-offs", " ")

  full_note <- paste0(
    note, " Completers: submitted the questionnaire; break-offs: never did. Only ",
    "persons who reached a question count, by the last question they answered; N: ",
    "those of them who answered it. Answers as shares of those who answered. Norm. ",
    "diff.: the normalised difference (Imbens and Rubin), completers minus break-offs, ",
    "over the root of the mean of the two variances. p: Welch t test for numbers, ",
    "Pearson chi-squared test without continuity correction for shares (one answer ",
    "against the others). The p-values are not adjusted for multiple testing, and ",
    "they are descriptive. -- means fewer than ", MIN_N, " answers in either group."
  )
  table_custom(
    table_cells, file, latex_text(full_note),
    col_names = c("", statistics, statistics, "\\makecell{Norm.\\\\diff.}", "p"),
    header = statistics_header,
    panels = panels
  )
  TRUE
}

#' Block C of the summary statistics. Writes nothing without at least MIN_N
#' completers and MIN_N break-offs.
#'
#' @param experts   one row per expert, as in experts.rds
#' @param vignettes one row per expert x vignette, as in vignettes.rds
#' @param dirs      list: the folders tables, figures, maps
#' @return tibble of exhibit_row()s
section_selection <- function(experts, vignettes, dirs) {
  columns_needed <- c("is_complete", "last_question_position", "expert_group")
  vignette_columns_needed <- c("resp_uid", "vig_rec", "vig_arm", "is_complete",
                               "expert_group")
  if (!has_columns(experts, columns_needed) ||
      !has_columns(vignettes, vignette_columns_needed)) {
    return(tibble())
  }
  block <- "C. Selection"
  exhibits <- tibble()
  rows <- selection_rows() %>%
    filter(variable %in% names(experts))
  # An answer without a row would drop out of the question's shares unnoticed
  for (variable_name in unique(rows$variable[rows$kind == "level"])) {
    levels_of_rows <- rows$level[rows$variable == variable_name]
    answers <- unique(as.character(experts[[variable_name]]))
    # setdiff(): the values of the first that are not in the second
    unknown_answers <- setdiff(answers[!is.na(answers)], levels_of_rows)
    if (length(levels_of_rows) > 1 && length(unknown_answers) > 0) {
      stop("No row of selection_rows() for the answers of ", variable_name, ": ",
           paste(unknown_answers, collapse = ", "))
    }
  }
  # An answer nobody gave says nothing, and has no test
  has_answers <- rep(TRUE, nrow(rows))
  for (k in which(rows$kind == "level")) {
    has_answers[k] <- rows$level[k] %in% as.character(experts[[rows$variable[k]]])
  }
  rows <- rows[has_answers, ]
  label_of(vignettes$vig_rec, TECHNOLOGY_LABELS, "recommendation")

  parts <- tribble(
    ~part,                ~caption,                                                             ~placement_all, ~part_note,
    "selection",          "Completers and break-offs: the firm, the invitation and the prices", "main",         "The federal state and the later blocks are in tables of their own. In the soft launch Q6 had no nationwide answer.",
    "selection_state",    "Completers and break-offs: the federal state",                       "appendix",     "Shares per state among those with a usable postcode.",
    "selection_later",    "Completers and break-offs: the later blocks",                        "appendix",     "The blocks that enough break-offs reached; a question asked of some firms only among those. In the soft launch Q7 was a different ranking task.",
    "selection_barriers", "Completers and break-offs: the barriers",                            "appendix",     "Q19: the share ticking each option, as far as enough break-offs reached it."
  )
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_experts <- subset_group(experts, group)
    if (sum(group_experts$is_complete) < MIN_N ||
        sum(!group_experts$is_complete) < MIN_N) {
      next
    }
    group_note <- if_else(group == ALL_EXPERTS, "", paste0(" ", GROUP_KNOWN_NOTE))

    # 1. The questions ---------------------------------------------------------
    for (k in seq_len(nrow(parts))) {
      part_rows <- rows %>%
        filter(part == parts$part[k])
      # Within one group, its own share says nothing
      if (group != ALL_EXPERTS) {
        part_rows <- part_rows %>%
          filter(variable != "expert_group")
      }
      if (nrow(part_rows) == 0) {
        next
      }
      table_file <- paste0("tab_", parts$part[k], "_", group_slug(group), ".tex")
      is_written <- write_selection_table(
        group_experts, part_rows, file.path(dirs$tables, table_file),
        paste0(parts$part_note[k], group_note)
      )
      if (is_written) {
        exhibits <- bind_rows(exhibits, exhibit_row(
          block, parts$part[k], group, "table", file.path("tables", table_file),
          parts$caption[k],
          if_else(group == ALL_EXPERTS, parts$placement_all[k], "appendix")
        ))
      }
    }

    # 2. The recommendations in the answered vignettes -------------------------
    answered <- subset_group(vignettes, group) %>%
      filter(!is.na(vig_rec), !is.na(vig_arm))
    vignette_columns <- tribble(
      ~arm_name, ~is_completer, ~column,
      "fw",      TRUE,          "fw_completers",
      "fw",      FALSE,         "fw_breakoffs",
      "nofw",    TRUE,          "nofw_completers",
      "nofw",    FALSE,         "nofw_breakoffs"
    )
    technologies <- names(TECHNOLOGY_LABELS)
    recommendation_cells <- tibble(label = latex_text(unname(TECHNOLOGY_LABELS)))
    experts_row <- tibble(label = "Experts")
    vignettes_row <- tibble(label = "Answered vignettes")
    is_any_column_shown <- FALSE
    for (k in seq_len(nrow(vignette_columns))) {
      column_rows <- answered %>%
        filter(vig_arm == vignette_columns$arm_name[k],
               is_complete == vignette_columns$is_completer[k])
      recommendations <- as.character(column_rows$vig_rec)
      n_column_experts <- n_distinct(column_rows$resp_uid)
      # The answers of one expert hang together: the minimum cell counts experts
      column_cells <- rep("--", length(technologies))
      if (n_column_experts >= MIN_N) {
        is_any_column_shown <- TRUE
        for (j in seq_along(technologies)) {
          column_cells[j] <- fmt_pct(mean(recommendations == technologies[j]))
        }
      }
      # Not an answer in this arm
      if (vignette_columns$arm_name[k] == "nofw") {
        column_cells[technologies == "Fernwärme"] <- ""
      }
      recommendation_cells[[vignette_columns$column[k]]] <- column_cells
      experts_row[[vignette_columns$column[k]]] <- fmt_int(n_column_experts)
      vignettes_row[[vignette_columns$column[k]]] <- fmt_int(nrow(column_rows))
    }
    if (is_any_column_shown) {
      arm_header <- c(1, 2, 2)
      names(arm_header) <- c(" ", latex_text(unname(ARM_LABELS[c("fw", "nofw")])))
      vignettes_note <- paste0(
        "Shares of the recommendations in the answered vignettes. Completers: ",
        "submitted the questionnaire; break-offs: never did, so they answered fewer ",
        "vignettes, and the early positions only. No test, as the answers of one ",
        "expert are not independent. Q6a sets the arm, it is not randomized. -- means ",
        "fewer than ", MIN_N, " experts.", group_note
      )
      vignettes_file <- paste0("tab_selection_vignettes_", group_slug(group), ".tex")
      table_custom(
        bind_rows(recommendation_cells, experts_row, vignettes_row),
        file.path(dirs$tables, vignettes_file), latex_text(vignettes_note),
        col_names = c("", "Completers", "Break-offs", "Completers", "Break-offs"),
        header = arm_header,
        panels = c("Recommendation" = length(technologies), "Sample" = 2)
      )
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "selection_vignettes", group, "table",
        file.path("tables", vignettes_file),
        "Completers and break-offs: the recommendations in the answered vignettes",
        if_else(group == ALL_EXPERTS, "main", "appendix")
      ))
    }
  }
  exhibits
}
