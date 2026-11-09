################################################################################
# section_completion.R — Block B. Survey metadata: who did not complete and where
#                        they stopped, how long the questionnaire took, which
#                        questions were answered
# Inputs : experts and vignettes of the sample; QUESTIONNAIRE, question_times()
# Outputs: in dirs$figures: fig_survival_<group>, fig_breakoff_section_<group>,
#          fig_vignette_times_<group>
#          in dirs$tables: tab_breakoff_question_<group>, tab_duration_<group>,
#          tab_item_response_<group>, tab_meta_<group>
#          in dirs$maps: map_breakoff_state_<group>
# Checks : one arm source per person; the minimum cell of docs/data_contract.md
################################################################################
# A person stops where the last question they answered lies
# (last_question_position): the page after it is the one they left on. A group comes
# from Q2, so a person who left before Q2 is in the version for all experts only,
# and within a group nobody can stop before Q2.
#
# A sample of completers has no break-offs: then only duration and vignette_times
# are written.

GROUP_KNOWN_NOTE <- paste0(
  "The group comes from Q2: persons who left before it have no group, and within ",
  "a group nobody can stop before it."
)

# With four lines a figure must also work in greyscale
GROUP_LINETYPES <- c(
  "All experts" = "solid",
  "Energy advisers" = "longdash",
  "HVAC installers" = "dotdash",
  "Chimney sweeps" = "dotted"
)

#' The short name of a question on the axis of a figure: its number in the
#' questionnaire; V, T and R with the number of a vignette or a decision
#'
#' @param label the label in QUESTIONNAIRE
question_code <- function(label) {
  # word(): the first word of the text, the second below
  case_when(
    str_detect(label, "^Q[0-9]") ~ word(label, 1),
    str_detect(label, "^Vignette") ~ paste0("V", word(label, 2)),
    str_detect(label, "^Cost") ~ "Cost",
    str_detect(label, "^Time") ~ paste0("T", str_sub(label, -1)),
    str_detect(label, "^Risk") ~ paste0("R", str_sub(label, -1)),
    label == "Feedback" ~ "FB"
  )
}

#' The answer columns of each question of QUESTIONNAIRE, except the vignettes,
#' whose answers are in the vignettes data
#'
#' @param data one row per expert, with the staircase columns: one column per node
#'   of a staircase, tp_5bbba being level 5 of the time preferences
#' @return named list: item of QUESTIONNAIRE -> names of its columns
question_columns <- function(data) {
  columns <- list(
    time_firm_zip = "firm_zip",
    time_resp_is_owner = "resp_is_owner",
    time_resp_role = "resp_role",
    time_firm_cat = c(names(FIRM_CATEGORY_LABELS), "firm_cat_other"),
    time_firm_employees = "firm_employees",
    time_firm_revenue = "firm_revenue",
    time_firm_radius = "firm_radius_recoded",
    time_firm_dh_in_area = "firm_dh_in_area",
    time_price = paste0("price_", names(FUEL_LABELS)),
    time_pexp = paste0("pexp_", names(FUEL_LABELS)),
    time_co2_heard = "co2_heard",
    time_co2_est = c("co2_est_low", "co2_est_high"),
    time_mix25 = paste0("mix25_", names(SHARE_TECHNOLOGY_LABELS)),
    time_brand_top1 = "brand_top1",
    time_svc = names(SERVICE_LABELS),
    time_adv_households = "adv_households",
    time_adv_share = paste0("adv_share_", names(SHARE_TECHNOLOGY_LABELS)),
    time_barr_ren = paste0("barr_ren_", names(BARRIER_LABELS)),
    time_barr_dh = paste0("barr_dh_", names(BARRIER_LABELS)),
    time_barr_conv = paste0("barr_conv_", names(BARRIER_LABELS)),
    time_cust_rank = paste0("cust_rank", 1:6),
    time_crit_rank = paste0("crit_rank", 1:8),
    time_dem_birthyear = "dem_birthyear",
    time_dem_education = "dem_education_recoded",
    time_dem_training_year = "dem_training_year",
    time_cpd = names(TRAINING_LABELS),
    time_cpd_other_text = "cpd_other_text",
    time_dem_own_heating = "dem_own_heating",
    time_fb_comment = "fb_comment"
  )
  # str_subset(): the names that match. tp_score and the like have no digit here.
  for (level in 1:5) {
    columns[[paste0("time_tp_", level)]] <-
      str_subset(names(data), paste0("^tp_", level))
    columns[[paste0("time_risk_", level)]] <-
      str_subset(names(data), paste0("^risk_", level))
  }
  columns
}

#' Whether each expert answered a question: at least one answer in its columns. A
#' tick box that was not ticked is no answer.
#'
#' @param data    one row per expert
#' @param columns the columns of the question; those not in the data are ignored
#' @return logical vector; NA if none of the columns is in the data
has_answer <- function(data, columns) {
  # intersect(): the values in both
  columns <- intersect(columns, names(data))
  if (length(columns) == 0) {
    return(rep(NA, nrow(data)))
  }
  answered <- rep(FALSE, nrow(data))
  for (column in columns) {
    answer <- as.character(data[[column]])
    answered <- answered | (!is.na(answer) & !(answer %in% c("", "Nicht Gewählt")))
  }
  answered
}

#' A table by question in two halves side by side, so that its 46 questions fit a
#' page and a frame. A section keeps its questions together under a heading row;
#' the halves split at the end of the section closest to the middle.
#'
#' @param cells    data frame of formatted cells, one row per question, label first
#' @param sections the section of each row
#' @return data frame: the columns of the left half, an empty column, the right half
two_halves <- function(cells, sections) {
  rows <- tibble()
  section_ends <- integer(0)
  for (section_name in unique(sections)) {
    heading <- cells[1, ]
    heading[1, ] <- ""
    heading[[1]] <- paste0("\\textbf{", latex_text(section_name), "}")
    section_rows <- cells[sections == section_name, ]
    section_rows[[1]] <- paste0("\\quad ", section_rows[[1]])
    rows <- bind_rows(rows, heading, section_rows)
    section_ends <- c(section_ends, nrow(rows))
  }
  # which.min(): the position of the smallest value
  n_left <- section_ends[which.min(abs(section_ends - nrow(rows) / 2))]
  left <- rows[seq_len(n_left), ]
  right <- rows[-seq_len(n_left), ]
  # The shorter half gets empty rows at its end
  n_rows <- max(nrow(left), nrow(right))
  blank <- cells[rep(1, n_rows), ]
  blank[seq_len(n_rows), ] <- ""
  left <- bind_rows(left, blank[seq_len(n_rows - nrow(left)), ])
  right <- bind_rows(right, blank[seq_len(n_rows - nrow(right)), ])
  names(left) <- paste0("left_", names(left))
  names(right) <- paste0("right_", names(right))
  # bind_cols(): the columns next to each other, row by row
  bind_cols(left, tibble(gap = rep("", n_rows)), right)
}

#' Block B of the summary statistics
#'
#' @param experts   one row per expert, as in experts.rds
#' @param vignettes one row per expert x vignette, as in vignettes.rds
#' @param dirs      list: the folders tables, figures, maps
#' @return tibble of exhibit_row()s
section_completion <- function(experts, vignettes, dirs) {
  columns_needed <- c("resp_uid", "is_complete", "expert_group",
                      "last_question_position", "last_section", "time_firm_zip",
                      "interview_minutes")
  vignette_columns_needed <- c("resp_uid", "vig_num", "vig_rec", "vig_time",
                               "vig_cost", "vig_cost_time", "arm_source",
                               "is_complete", "expert_group")
  if (!has_columns(experts, columns_needed) ||
      !has_columns(vignettes, vignette_columns_needed)) {
    return(tibble())
  }
  block <- "B. Survey metadata"
  versions <- c(ALL_EXPERTS, EXPERT_GROUPS)
  section_names <- levels(QUESTIONNAIRE$section)
  # Where a new section starts on the axis of the survival curves
  section_starts <- QUESTIONNAIRE %>%
    group_by(section) %>%
    summarise(position = min(position)) %>%
    pull(position)
  q2_position <- QUESTIONNAIRE$position[QUESTIONNAIRE$item == "time_firm_cat"]
  answer_columns <- question_columns(experts)
  label_size <- (figure_font_size - 1.5) / .pt
  exhibits <- tibble()

  for (group in versions) {
    group_experts <- subset_group(experts, group)
    group_vignettes <- subset_group(vignettes, group)
    breakoffs <- group_experts %>%
      filter(!is_complete)
    completers <- group_experts %>%
      filter(is_complete)
    has_breakoffs <- nrow(breakoffs) >= MIN_N
    main_if_all <- if_else(group == ALL_EXPERTS, "main", "appendix")
    slug <- group_slug(group)
    group_note <- if_else(group == ALL_EXPERTS, "", paste0(" ", GROUP_KNOWN_NOTE))
    lines <- if (group == ALL_EXPERTS) versions else group

    # 1. Who is still in the survey at each question ---------------------------
    if (has_breakoffs) {
      survival <- tibble()
      for (line in lines) {
        line_experts <- subset_group(experts, line)
        if (nrow(line_experts) < MIN_N) {
          next
        }
        # A completer counts for every question, whichever page came last for them
        furthest <- if_else(line_experts$is_complete, nrow(QUESTIONNAIRE),
                            as.integer(line_experts$last_question_position))
        positions <- c(0, QUESTIONNAIRE$position)
        # The line of a group starts where the group is known
        if (line != ALL_EXPERTS) {
          positions <- positions[positions >= q2_position]
        }
        for (position in positions) {
          survival <- bind_rows(survival, tibble(
            line = line, position = position, share = mean(furthest >= position)
          ))
        }
      }
      survival <- survival %>%
        mutate(line = factor(line, levels = versions))
      plot_survival <- ggplot(survival, aes(x = position, y = share, colour = line,
                                            linetype = line)) +
        # Where a section of the questionnaire starts
        geom_vline(xintercept = section_starts - 0.5, colour = COLOUR_GRID,
                   linewidth = 0.3) +
        geom_step(linewidth = 0.5) +
        scale_colour_manual(values = GROUP_COLOURS[versions]) +
        scale_linetype_manual(values = GROUP_LINETYPES[versions]) +
        scale_x_continuous(breaks = c(0, QUESTIONNAIRE$position),
                           labels = c("Start", question_code(QUESTIONNAIRE$label)),
                           expand = expansion(add = 0.6)) +
        scale_y_continuous(labels = label_percent(accuracy = 1),
                           limits = c(0, 1)) +
        labs(x = NULL, y = "Share of those who started") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                                         size = figure_font_size - 3),
              panel.grid.major.x = element_blank())
      if (group != ALL_EXPERTS) {
        plot_survival <- plot_survival +
          theme(legend.position = "none")
      }
      survival_file <- paste0("fig_survival_", slug, ".pdf")
      save_figure(plot_survival, file.path(dirs$figures, survival_file),
                  size = "full")
      survival_note <- paste0(
        "Persons who started: ", fmt_int(nrow(group_experts)), ", of whom ",
        fmt_int(nrow(breakoffs)), " never completed. Still in the survey: the last ",
        "question answered lies at this question or later, also if routing hid it. ",
        "Questions as in the questionnaire: V vignettes, T and R the decisions on ",
        "time and risk, FB feedback; grey lines separate its sections.", group_note
      )
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "survival", group, "figure", file.path("figures", survival_file),
        "Share of the persons who started still in the survey at each question",
        main_if_all, survival_note
      ))
    }

    # 2. In which section those who never completed stopped --------------------
    if (has_breakoffs) {
      # coalesce(): the first value that is not NA
      section_stops <- breakoffs %>%
        mutate(section = coalesce(as.character(last_section),
                                  "No question answered")) %>%
        count(section)
      stops_by_section <- tibble(section = c("No question answered",
                                             section_names)) %>%
        left_join(section_stops, by = "section", relationship = "one-to-one") %>%
        mutate(n = coalesce(n, 0L),
               share = n / sum(n),
               label = paste0(fmt_num(100 * share, 1), "% (", fmt_int(n), ")"),
               section = factor(section, levels = rev(section)))
      plot_sections <- ggplot(stops_by_section, aes(x = share, y = section)) +
        geom_col(fill = COLOUR_BAR, width = 0.7) +
        geom_text(aes(label = label), hjust = -0.1, size = label_size,
                  colour = COLOUR_INK) +
        scale_x_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA),
                           expand = expansion(mult = c(0, 0.3))) +
        labs(x = "Share of those who never completed", y = NULL) +
        theme(panel.grid.major.y = element_blank())
      sections_file <- paste0("fig_breakoff_section_", slug, ".pdf")
      save_figure(plot_sections, file.path(dirs$figures, sections_file),
                  size = "full")
      sections_note <- paste0(
        "Persons who started and never completed: ", fmt_int(nrow(breakoffs)),
        "; in brackets their number. A person stops in a section if the last ",
        "question answered lies in it, so they left on the page after it: those who ",
        "answered Q6a last left when the vignettes began.", group_note
      )
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "breakoff_section", group, "figure",
        file.path("figures", sections_file),
        "In which section those who never completed stopped", main_if_all,
        sections_note
      ))
    }

    # 3. At which question they stopped ----------------------------------------
    if (has_breakoffs) {
      question_stops <- breakoffs %>%
        count(position = as.integer(last_question_position))
      stops_by_question <- tibble(
        position = c(0L, QUESTIONNAIRE$position),
        question = c("No question answered", QUESTIONNAIRE$label),
        section = c("Before the first question",
                    as.character(QUESTIONNAIRE$section))
      ) %>%
        left_join(question_stops, by = "position", relationship = "one-to-one") %>%
        mutate(n = coalesce(n, 0L),
               share = n / sum(n))
      question_cells <- stops_by_question %>%
        transmute(question = latex_text(question),
                  n = fmt_int(n),
                  share = fmt_pct(share))
      question_note <- paste0(
        "Persons who started and never completed, by the last question they ",
        "answered: they left on the page after it. A count of who stopped may be ",
        "small; the share is of all ", fmt_int(nrow(breakoffs)),
        " who never completed.", group_note
      )
      question_column_names <- c("Last question answered", "N", "Share")
      question_file <- paste0("tab_breakoff_question_", slug, ".tex")
      table_custom(two_halves(question_cells, stops_by_question$section),
                   file.path(dirs$tables, question_file), latex_text(question_note),
                   col_names = c(question_column_names, "", question_column_names),
                   align = c("l", "r", "r", "l", "l", "r", "r"))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "breakoff_question", group, "table",
        file.path("tables", question_file),
        "At which question those who never completed stopped", "appendix"
      ))
    }

    # 4. The break-off rate by state -------------------------------------------
    if (has_breakoffs && has_columns(group_experts, "bundesland")) {
      state_values <- state_means(group_experts,
                                  as.numeric(!group_experts$is_complete),
                                  percent = TRUE) %>%
        mutate(value = if_else(n >= MIN_CONTACTS, value, NA_real_))
      if (any(!is.na(state_values$value))) {
        plot_map <- plot_state_map(state_values, "Break-off\nrate",
                                   low = MAP_BREAKOFF_LOW, high = MAP_BREAKOFF_HIGH,
                                   legend_labels = label_percent(accuracy = 0.1))
        breakoff_map_file <- paste0("map_breakoff_state_", slug, ".pdf")
        save_figure(plot_map, file.path(dirs$maps, breakoff_map_file), size = "map")
        map_note <- paste0(
          "Share of the persons who started and never completed, by the federal ",
          "state of the postcode. Persons who left before the postcode have no state."
        )
        # Grey is explained only where a state is grey
        if (any(is.na(state_values$value)) ||
            nrow(state_values) < length(STATE_LABELS)) {
          map_note <- paste0(map_note, " Grey and \"–\": fewer than ", MIN_CONTACTS,
                             " started.")
        }
        exhibits <- bind_rows(exhibits, exhibit_row(
          block, "breakoff_state", group, "map",
          file.path("maps", breakoff_map_file),
          "Break-off rate by state", "appendix", paste0(map_note, group_note)
        ))
      }
    }

    # 5. How long the questionnaire took ---------------------------------------
    if (nrow(completers) >= MIN_N) {
      completer_vignettes <- group_vignettes %>%
        filter(resp_uid %in% completers$resp_uid)
      section_minutes <- question_times(completers, completer_vignettes) %>%
        group_by(resp_uid, section = paste0("section_", as.integer(section))) %>%
        summarise(minutes = sum(seconds) / 60) %>%
        ungroup() %>%
        pivot_wider(names_from = section, values_from = minutes)
      duration_data <- completers %>%
        select(resp_uid, interview_minutes) %>%
        left_join(section_minutes, by = "resp_uid", relationship = "one-to-one")
      duration_rows <- tibble(
        panel = c("Whole questionnaire", rep("By section", length(section_names))),
        variable = c("interview_minutes",
                     paste0("section_", seq_along(section_names))),
        kind = "num",
        level = NA_character_,
        label = c("Total time (LimeSurvey)", section_names),
        digits = 1
      )
      duration_note <- paste0(
        "Minutes, completed questionnaires, not trimmed: a questionnaire left open ",
        "moves the mean and the standard deviation, not the quartiles. A section: the ",
        "sum of the times of its questions, among the experts who answered at least ",
        "one; some sections were shown to some firms only. ", cell_note()
      )
      duration_file <- paste0("tab_duration_", slug, ".tex")
      summary_table(duration_data, duration_rows,
                    file.path(dirs$tables, duration_file), duration_note)
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "duration", group, "table", file.path("tables", duration_file),
        "Time taken for the questionnaire and its sections (minutes)", "appendix"
      ))
    }

    # 6. The median time per vignette position ---------------------------------
    position_times <- tibble()
    for (line in lines) {
      line_vignettes <- subset_group(vignettes, line) %>%
        filter(is_complete, !is.na(vig_time))
      for (position in seq_len(N_VIGNETTES)) {
        seconds <- line_vignettes$vig_time[line_vignettes$vig_num == position]
        if (length(seconds) >= MIN_N) {
          position_times <- bind_rows(position_times, tibble(
            line = line, position = position, seconds = median(seconds)
          ))
        }
      }
    }
    if (nrow(position_times) > 0) {
      position_times <- position_times %>%
        mutate(line = factor(line, levels = versions))
      plot_times <- ggplot(position_times, aes(x = position, y = seconds,
                                               colour = line, linetype = line)) +
        geom_line(linewidth = 0.5) +
        geom_point(size = 1.5) +
        scale_colour_manual(values = GROUP_COLOURS[versions]) +
        scale_linetype_manual(values = GROUP_LINETYPES[versions]) +
        scale_x_continuous(breaks = seq_len(N_VIGNETTES),
                           labels = paste0("Vignette ", seq_len(N_VIGNETTES))) +
        scale_y_continuous(limits = c(0, NA),
                           expand = expansion(mult = c(0, 0.08))) +
        labs(x = NULL, y = "Median time (seconds)") +
        theme(panel.grid.major.x = element_blank())
      if (group != ALL_EXPERTS) {
        plot_times <- plot_times +
          theme(legend.position = "none")
      }
      times_file <- paste0("fig_vignette_times_", slug, ".pdf")
      save_figure(plot_times, file.path(dirs$figures, times_file), size = "full")
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "vignette_times", group, "figure", file.path("figures", times_file),
        "Median time per vignette position", "appendix",
        paste0(
          "Median seconds on the question of a vignette, by its position in the ",
          "questionnaire, for the persons who completed, so the same persons at ",
          "every position; a point needs at least ", MIN_N, " answers. Not trimmed."
        )
      ))
    }

    # 7. Item response: who reached, was asked and answered each question ------
    if (has_breakoffs) {
      asked_pages <- question_times(group_experts, group_vignettes) %>%
        distinct(resp_uid, item)
      item_response <- tibble()
      for (k in seq_len(nrow(QUESTIONNAIRE))) {
        item_name <- QUESTIONNAIRE$item[k]
        is_reached <- group_experts$is_complete |
          group_experts$last_question_position >= QUESTIONNAIRE$position[k]
        is_asked <- group_experts$resp_uid %in%
          asked_pages$resp_uid[asked_pages$item == item_name]
        if (item_name %in% names(answer_columns)) {
          is_answered <- has_answer(group_experts, answer_columns[[item_name]])
        } else if (item_name == "vignette_cost") {
          is_answered <- group_experts$resp_uid %in%
            group_vignettes$resp_uid[!is.na(group_vignettes$vig_cost)]
        } else {
          vignette_number <- as.integer(str_remove(item_name, "vignette_"))
          is_answered <- group_experts$resp_uid %in%
            group_vignettes$resp_uid[!is.na(group_vignettes$vig_rec) &
                                       group_vignettes$vig_num == vignette_number]
        }
        item_response <- bind_rows(item_response, tibble(
          question = QUESTIONNAIRE$label[k],
          reached = sum(is_reached),
          asked = sum(is_asked),
          answered = sum(is_answered & is_asked)
        ))
      }
      item_cells <- item_response %>%
        mutate(share = if_else(asked >= MIN_N, answered / asked, NA_real_)) %>%
        transmute(question = latex_text(question),
                  reached = fmt_int(reached),
                  asked = fmt_int(asked),
                  answered = fmt_int(answered),
                  share = fmt_pct(share))
      item_note <- paste0(
        "Reached: the last question answered lies at this question or later, or the ",
        "person completed. Asked: submitted its page, which has a response time; the ",
        "others among those who reached it were routed past it. Routing: Q0b only if ",
        "not the owner; Q8 and Q10 only for firms with energy advice, engineering or ",
        "chimney sweeping; Q11b and Q11c only for HVAC firms; Q18b only if the carbon ",
        "price is known; Q24a only with other training, and not in the soft launch; a ",
        "staircase may end before its fifth decision. Answered: at least one answer ",
        "on the page, for tick boxes at least one tick; -- in the share means fewer ",
        "than ", MIN_N, " asked.", group_note
      )
      item_column_names <- c("Question", "Reached", "Asked", "Answered",
                             "\\makecell{Answered\\\\among asked}")
      item_file <- paste0("tab_item_response_", slug, ".tex")
      table_custom(two_halves(item_cells, as.character(QUESTIONNAIRE$section)),
                   file.path(dirs$tables, item_file), latex_text(item_note),
                   col_names = c(item_column_names, "", item_column_names),
                   align = c("l", "r", "r", "r", "r", "l",
                             "l", "r", "r", "r", "r"))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "item_response", group, "table", file.path("tables", item_file),
        "Who reached, was asked and answered each question", "appendix"
      ))
    }

    # 8. The meta data of the survey -------------------------------------------
    meta_columns <- c("n_attempts", "is_softlaunch", "arm_conflict")
    if (has_breakoffs && has_columns(group_experts, meta_columns)) {
      arm_sources <- group_vignettes %>%
        distinct(resp_uid, arm_source)
      check_unique(arm_sources, "resp_uid")
      meta_experts <- group_experts %>%
        select(resp_uid, is_complete, n_attempts, is_softlaunch, arm_conflict) %>%
        left_join(arm_sources, by = "resp_uid", relationship = "one-to-one")
      arm_source <- meta_experts$arm_source

      # A row of persons: their number and share, and how many of them completed
      meta_row <- function(label, is_in_row) {
        n_row <- sum(is_in_row)
        share_completed <- mean(meta_experts$is_complete[is_in_row])
        tibble(label = label,
               persons = fmt_int(n_row),
               share = fmt_pct(n_row / nrow(meta_experts)),
               completed = fmt_pct(if_else(n_row >= MIN_N, share_completed,
                                           NA_real_)))
      }
      meta_cells <- bind_rows(
        meta_row("All persons who started", rep(TRUE, nrow(meta_experts))),
        meta_row("Started more than once", meta_experts$n_attempts > 1),
        tibble(label = "Further attempts, left out",
               persons = fmt_int(sum(meta_experts$n_attempts - 1)),
               share = "", completed = ""),
        meta_row("Soft launch", meta_experts$is_softlaunch),
        meta_row("Main waves", !meta_experts$is_softlaunch),
        meta_row("Answered in both arms (no arm, vignettes left out)",
                 meta_experts$arm_conflict),
        meta_row("Arm from the vignettes answered", arm_source %in% "answered"),
        meta_row("Arm routed from Q6a, no vignette answered",
                 arm_source %in% "routed from Q6a"),
        meta_row("Arm unknown", arm_source %in% "unknown")
      )
      meta_note <- paste0(
        "One row per person: of those who started more than once the furthest attempt ",
        "stays. Completed: share among the persons of the row; -- means fewer than ",
        MIN_N, " persons. The soft launch differs in Q6 (no nationwide answer) and Q7 ",
        "(a different ranking task).", group_note
      )
      meta_file <- paste0("tab_meta_", slug, ".tex")
      table_custom(meta_cells, file.path(dirs$tables, meta_file),
                   latex_text(meta_note),
                   col_names = c("", "N", "Share", "Completed"),
                   panels = c("Attempts" = 3, "Waves" = 2,
                              "Arm of the vignettes" = 4))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "meta", group, "table", file.path("tables", meta_file),
        "Attempts, waves and arms", "appendix"
      ))
    }
  }
  exhibits
}
