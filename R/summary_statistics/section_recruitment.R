################################################################################
# section_recruitment.R — Block A. Who was invited and who answered: the sample
#                         flow, and the response rates by state, group and wave
# Inputs : experts and vignettes of the full sample; the sampling frame
#          (load_frame()); the dispatch log (load_versand_waves())
# Outputs: in dirs$tables: tab_sample_flow_<group>, tab_response_group_all,
#          tab_response_state_table_<group>, tab_response_wave_all
#          in dirs$maps: map_response_state_<group>,
#          map_response_state_started_<group>
#          in dirs$figures: fig_composition_all
# Checks : one row per state, group and wave in the joins; unknown states stop;
#          the minimum cell of docs/data_contract.md in response_rate() and
#          shown_count()
################################################################################
# A person: an e-mail address of the lists once, an invitation once. By group, the
# denominator is the mailing list and the numerator the answer to Q2 (frame.R). The
# exhibits that are by group already come once, the others for all experts and per
# group.

# The columns of the tables of rates: all contacts, then without t-online addresses
RATE_COLUMN_NAMES <- c(
  "Invited", "Started", "Completed",
  "\\makecell{Started\\\\per invited}", "\\makecell{Completed\\\\per invited}",
  "Invited",
  "\\makecell{Started\\\\per invited}", "\\makecell{Completed\\\\per invited}"
)
RATE_HEADER <- c(" " = 1, "All contacts" = 5, "Without t-online addresses" = 3)

#' A response rate with the minimum cell: NA below MIN_CONTACTS contacted persons
#' or MIN_N answers
response_rate <- function(answers, contacted) {
  if_else(contacted >= MIN_CONTACTS & answers >= MIN_N,
          answers / contacted, NA_real_)
}

#' A count of answers with the minimum cell: NA below MIN_N answers, or below
#' MIN_CONTACTS contacted persons, as the rate could be computed from it
#'
#' @param count     the answers
#' @param contacted the persons behind a rate; leave out if the table has no rate
shown_count <- function(count, contacted = MIN_CONTACTS) {
  if_else(contacted >= MIN_CONTACTS & count >= MIN_N, as.numeric(count), NA_real_)
}

#' The formatted cells of rows of a table of rates. The denominator without t-online
#' addresses is hidden below MIN_CONTACTS too: next to the answers it would give the
#' rate, and next to all the invited the t-online addresses of a small cell.
rate_cells <- function(label, n_invited, n_invited_no_tonline, n_started,
                       n_completed) {
  tibble(
    label = label,
    invited = fmt_int(n_invited),
    started = fmt_int(shown_count(n_started, n_invited)),
    completed = fmt_int(shown_count(n_completed, n_invited)),
    rate_started = fmt_pct(response_rate(n_started, n_invited)),
    rate_completed = fmt_pct(response_rate(n_completed, n_invited)),
    invited_no_tonline = fmt_int(if_else(n_invited_no_tonline >= MIN_CONTACTS,
                                         as.numeric(n_invited_no_tonline),
                                         NA_real_)),
    rate_started_no_tonline = fmt_pct(response_rate(n_started,
                                                    n_invited_no_tonline)),
    rate_completed_no_tonline = fmt_pct(response_rate(n_completed,
                                                      n_invited_no_tonline))
  )
}

#' A row of a table of rates with counts only: persons whose list or state is not
#' known have no denominator. A count of what the data lack may be small.
count_cells <- function(label, n_invited, n_started, n_completed) {
  tibble(label = label, invited = n_invited, started = fmt_int(n_started),
         completed = fmt_int(n_completed), rate_started = "", rate_completed = "",
         invited_no_tonline = "", rate_started_no_tonline = "",
         rate_completed_no_tonline = "")
}

#' The note on the rates without t-online addresses
#'
#' @param is_tonline whether each invited person of the table has such an address
tonline_note <- function(is_tonline) {
  paste0(
    "Without t-online addresses: Telekom may have blocked part of the mailing, so ",
    "the ", fmt_num(100 * mean(is_tonline), 1), "% of the invited with such an ",
    "address leave the denominator. The numerator stays, as the data do not say ",
    "which address an answer came from, so these rates are upper bounds."
  )
}

#' Invited, started and completed per federal state. A person without a usable
#' postcode carries no state, on either side of the fraction.
#'
#' @param frame   the invited of this version of the exhibit
#' @param experts the persons who started
#' @return tibble, one row per state of STATE_LABELS
count_by_state <- function(frame, experts) {
  invited <- frame %>%
    filter(!is.na(bundesland)) %>%
    group_by(state = as.character(bundesland)) %>%
    summarise(invited = n(), invited_no_tonline = sum(!is_tonline)) %>%
    ungroup()
  answers <- experts %>%
    filter(!is.na(bundesland)) %>%
    group_by(state = as.character(bundesland)) %>%
    summarise(started = n(), completed = sum(is_complete)) %>%
    ungroup()
  # coalesce(): the first value that is not NA, here 0 for a state without anyone
  tibble(state = names(STATE_LABELS)) %>%
    left_join(invited, by = "state", relationship = "one-to-one") %>%
    left_join(answers, by = "state", relationship = "one-to-one") %>%
    mutate(invited = coalesce(invited, 0L),
           invited_no_tonline = coalesce(invited_no_tonline, 0L),
           started = coalesce(started, 0L),
           completed = coalesce(completed, 0L))
}

#' Block A of the summary statistics. Needs the sampling frame, so it applies to
#' the full sample only.
#'
#' @param experts   one row per expert, as in experts.rds
#' @param vignettes one row per expert x vignette, as in vignettes.rds
#' @param frame     the sampling frame (load_frame()), or NULL
#' @param dirs      list: the folders tables, figures, maps
#' @return tibble of exhibit_row()s; empty without a frame
section_recruitment <- function(experts, vignettes, frame, dirs) {
  columns_needed <- c("resp_uid", "wave", "target_group", "is_softlaunch",
                      "is_complete", "reached_vignettes", "expert_group",
                      "bundesland")
  vignette_columns_needed <- c("resp_uid", "vig_rec", "vig_arm", "is_complete",
                               "expert_group")
  if (is.null(frame) || !has_columns(experts, columns_needed) ||
      !has_columns(vignettes, vignette_columns_needed)) {
    return(tibble())
  }
  block <- "A. Recruitment"
  versions <- c(ALL_EXPERTS, EXPERT_GROUPS)
  exhibits <- tibble()
  answered_vignettes <- vignettes %>%
    filter(!is.na(vig_rec))
  # Stops on a state without a label, which would drop out of the tables below
  label_of(c(as.character(frame$bundesland), as.character(experts$bundesland)),
           STATE_LABELS, "federal state")

  rate_note <- paste0(
    "Persons: an e-mail address of the lists once, an invitation once, so someone ",
    "who started in two waves counts twice among the answers. Rates: per invited ",
    "person; -- means fewer than ", MIN_CONTACTS, " invited or ", MIN_N, " answers."
  )

  # 1. Sample flow -------------------------------------------------------------
  for (group in versions) {
    group_answered <- subset_group(answered_vignettes, group)
    group_experts <- subset_group(experts, group)
    completed_experts <- group_experts %>%
      filter(is_complete)
    completed_answered <- group_answered %>%
      filter(is_complete)
    # The experts of the models of an arm, if they keep the three groups
    with_group_answered <- completed_answered %>%
      filter(!is.na(expert_group))

    flow <- tribble(
      ~step,                            ~n_experts,                            ~n_vignettes,
      "Invited",                        nrow(subset_group(frame, group)),      NA,
      "Started the questionnaire",      nrow(group_experts),                   nrow(group_answered),
      "Reached the vignettes",          sum(group_experts$reached_vignettes),  nrow(group_answered),
      "Completed the questionnaire",    nrow(completed_experts),               nrow(completed_answered)
    )
    if (group == ALL_EXPERTS) {
      flow <- bind_rows(flow, tibble(
        step = "Completed, with a group",
        n_experts = sum(!is.na(completed_experts$expert_group)),
        n_vignettes = nrow(with_group_answered)
      ))
    }
    for (arm_name in names(ARM_LABELS)) {
      arm_answered <- with_group_answered %>%
        filter(vig_arm == arm_name)
      flow <- bind_rows(flow, tibble(
        step = paste0("\\quad ", ARM_LABELS[[arm_name]]),
        n_experts = n_distinct(arm_answered$resp_uid),
        n_vignettes = nrow(arm_answered)
      ))
    }
    # The minimum cell counts experts: the answers of one expert hang together
    flow_cells <- flow %>%
      mutate(n_vignettes = if_else(n_experts >= MIN_N, n_vignettes, NA)) %>%
      transmute(step = latex_text(step),
                n_experts = fmt_int(shown_count(n_experts)),
                n_vignettes = fmt_int(n_vignettes))
    # The invited have answered nothing yet
    flow_cells$n_vignettes[1] <- ""

    flow_note <- paste0(
      "Persons: an e-mail address of the lists once, an invitation once, however ",
      "often it was used to start the questionnaire. Reached the vignettes: ",
      "answered at least one. Answered vignettes: those of the persons of the row, ",
      "without those of an expert who answered in both arms. By arm: persons who ",
      "completed and answered at least one vignette; Q6a sets the arm, it is not ",
      "randomized. -- means fewer than ", MIN_N, " experts."
    )
    if (group == ALL_EXPERTS) {
      flow_note <- paste0(flow_note, " Group from Q2.")
    } else {
      flow_note <- paste(flow_note, GROUP_RATE_NOTE)
    }
    flow_file <- paste0("tab_sample_flow_", group_slug(group), ".tex")
    table_custom(flow_cells, file.path(dirs$tables, flow_file),
                 latex_text(flow_note),
                 col_names = c("", "Experts", "\\makecell{Answered\\\\vignettes}"))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "sample_flow", group, "table", file.path("tables", flow_file),
      "From the invitation to the answered vignettes",
      if_else(group == ALL_EXPERTS, "main", "appendix")
    ))
  }

  # 2. Response rates by state: maps and table ---------------------------------
  map_kinds <- tribble(
    ~answers,    ~id,                      ~legend,                  ~what,                      ~caption,
    "completed", "response_state",         "Completed\nper invited", "Completed questionnaires", "Response rate by state",
    "started",   "response_state_started", "Started\nper invited",   "Started questionnaires",   "Started questionnaires per invited person, by state"
  )
  for (group in versions) {
    group_frame <- subset_group(frame, group)
    group_experts <- subset_group(experts, group)
    state_counts <- count_by_state(group_frame, group_experts)
    group_note <- if_else(group == ALL_EXPERTS, "", paste0(" ", GROUP_RATE_NOTE))

    for (k in seq_len(nrow(map_kinds))) {
      answers <- state_counts[[map_kinds$answers[k]]]
      state_values <- state_counts %>%
        transmute(state,
                  value = response_rate(answers, invited),
                  label = paste0(fmt_num(100 * value, 1), "%"))
      if (all(is.na(state_values$value))) {
        next
      }
      map_file <- paste0("map_", map_kinds$id[k], "_", group_slug(group), ".pdf")
      plot_map <- plot_state_map(state_values, map_kinds$legend[k],
                                 legend_labels = label_percent(accuracy = 0.1))
      save_figure(plot_map, file.path(dirs$maps, map_file), size = "map")

      map_note <- paste0(
        map_kinds$what[k], " per invited person, by the federal state of the ",
        "postcode: ", fmt_int(sum(answers)), " of ",
        fmt_int(sum(state_counts$invited)), " invited with a postcode."
      )
      # Grey is explained only where a state is grey
      if (any(is.na(state_values$value))) {
        map_note <- paste0(map_note, " Grey and \"–\": fewer than ", MIN_CONTACTS,
                           " invited or ", MIN_N, " ", map_kinds$answers[k], ".")
      }
      is_main <- group == ALL_EXPERTS && map_kinds$answers[k] == "completed"
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, map_kinds$id[k], group, "map", file.path("maps", map_file),
        map_kinds$caption[k], if_else(is_main, "main", "appendix"),
        paste0(map_note, group_note)
      ))
    }

    state_cells <- bind_rows(
      rate_cells(label_of(state_counts$state, STATE_LABELS, "federal state"),
                 state_counts$invited, state_counts$invited_no_tonline,
                 state_counts$started, state_counts$completed),
      count_cells("No usable postcode",
                  fmt_int(sum(is.na(group_frame$bundesland))),
                  sum(is.na(group_experts$bundesland)),
                  sum(is.na(group_experts$bundesland) & group_experts$is_complete)),
      rate_cells("Total", nrow(group_frame), sum(!group_frame$is_tonline),
                 nrow(group_experts), sum(group_experts$is_complete))
    )
    state_note <- paste0(
      rate_note, " Persons who left before the postcode (Q0) have no state, and are ",
      "in the row without one, so the rates of the states are low by that much. ",
      tonline_note(group_frame$is_tonline), group_note
    )
    state_file <- paste0("tab_response_state_table_", group_slug(group), ".tex")
    table_custom(state_cells, file.path(dirs$tables, state_file),
                 latex_text(state_note),
                 col_names = c("", RATE_COLUMN_NAMES), header = RATE_HEADER,
                 panels = c("Federal state of the postcode" = nrow(state_counts),
                            "All invited" = 2))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "response_state_table", group, "table",
      file.path("tables", state_file),
      "Invited, started and completed by federal state", "appendix"
    ))
  }

  # 3. Response rates by group -------------------------------------------------
  invited_by_group <- frame %>%
    group_by(group = as.character(expert_group)) %>%
    summarise(invited = n(), invited_no_tonline = sum(!is_tonline)) %>%
    ungroup()
  answers_by_group <- experts %>%
    filter(!is.na(expert_group)) %>%
    group_by(group = as.character(expert_group)) %>%
    summarise(started = n(), completed = sum(is_complete)) %>%
    ungroup()
  group_counts <- tibble(group = EXPERT_GROUPS) %>%
    left_join(invited_by_group, by = "group", relationship = "one-to-one") %>%
    left_join(answers_by_group, by = "group", relationship = "one-to-one") %>%
    mutate(invited = coalesce(invited, 0L),
           invited_no_tonline = coalesce(invited_no_tonline, 0L),
           started = coalesce(started, 0L),
           completed = coalesce(completed, 0L))
  is_without_group <- is.na(experts$expert_group)

  group_cells <- bind_rows(
    rate_cells(group_counts$group, group_counts$invited,
               group_counts$invited_no_tonline, group_counts$started,
               group_counts$completed),
    count_cells("Without a group (Q2 not answered, or nothing ticked)", "",
                sum(is_without_group), sum(is_without_group & experts$is_complete)),
    rate_cells("Total", nrow(frame), sum(!frame$is_tonline),
               nrow(experts), sum(experts$is_complete))
  )
  group_note <- paste(
    rate_note, GROUP_RATE_NOTE, "A person who ticks several categories in Q2 gets",
    "one group: HVAC before chimney sweeps before energy advice. Persons who left",
    "before Q2 or ticked nothing have no group, and their list is not known, so the",
    "rates of the groups are low by that much and do not add up to the total.",
    tonline_note(frame$is_tonline)
  )
  group_file <- "tab_response_group_all.tex"
  table_custom(group_cells, file.path(dirs$tables, group_file),
               latex_text(group_note),
               col_names = c("", RATE_COLUMN_NAMES), header = RATE_HEADER)
  exhibits <- bind_rows(exhibits, exhibit_row(
    block, "response_group", ALL_EXPERTS, "table", file.path("tables", group_file),
    "Invited, started and completed by group", "main"
  ))

  # 4. The groups among the invited and the completed --------------------------
  composition <- bind_rows(
    group_counts %>%
      transmute(group, series = "Invited", n = invited,
                share = invited / sum(invited)),
    group_counts %>%
      transmute(group, series = "Completed", n = completed,
                share = completed / sum(completed))
  ) %>%
    # Bars of fewer answers than the minimum cell are left out
    filter(n >= MIN_N) %>%
    # The first level is drawn at the bottom: reversed, so that the first group and
    # the invited are on top
    mutate(label = paste0(fmt_num(100 * share, 1), "%"),
           group = factor(group, levels = rev(EXPERT_GROUPS)),
           series = factor(series, levels = c("Completed", "Invited")))
  plot_composition <- ggplot(composition,
                             aes(x = share, y = group, fill = series)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = label), position = position_dodge(width = 0.75),
              hjust = -0.15, size = (figure_font_size - 1.5) / .pt,
              colour = COLOUR_INK) +
    scale_fill_manual(values = c("Invited" = COLOUR_BAR_LIGHT,
                                 "Completed" = COLOUR_BAR),
                      breaks = c("Invited", "Completed")) +
    scale_x_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Share of the three groups", y = NULL) +
    theme(panel.grid.major.y = element_blank())
  composition_file <- "fig_composition_all.pdf"
  save_figure(plot_composition, file.path(dirs$figures, composition_file),
              size = "full")
  composition_note <- paste0(
    "Invited: ", fmt_int(sum(group_counts$invited)),
    " persons, by the list they were drawn from. Completed: ",
    fmt_int(sum(group_counts$completed)), " persons with a group, by Q2. ",
    GROUP_RATE_NOTE
  )
  # A missing bar is explained only where one is missing
  if (nrow(composition) < 2 * length(EXPERT_GROUPS)) {
    composition_note <- paste0(composition_note, " A bar of fewer than ", MIN_N,
                               " persons is left out.")
  }
  exhibits <- bind_rows(exhibits, exhibit_row(
    block, "composition", ALL_EXPERTS, "figure",
    file.path("figures", composition_file),
    "The groups among the invited and the completed", "main", composition_note
  ))

  # 5. Response rates by wave, and the cross-check against the dispatch log ----
  dispatch <- load_versand_waves()
  wave_counts <- experts %>%
    group_by(wave, target_group, is_softlaunch) %>%
    summarise(started = n(), completed = sum(is_complete)) %>%
    ungroup() %>%
    left_join(dispatch, by = "wave", relationship = "many-to-one") %>%
    arrange(desc(is_softlaunch), first_sent, wave)
  # A wave has one target group
  check_unique(wave_counts, "wave")
  n_waves_without_log <- sum(is.na(wave_counts$invitations))
  if (n_waves_without_log > 0) {
    message(n_waves_without_log, " of ", nrow(wave_counts),
            " waves are not in the dispatch log")
  }

  # format(): the date as text. A wave that is not in the log has no rate that its
  # counts could give, so they stand
  wave_cells <- wave_counts %>%
    mutate(rate_started = response_rate(started, invitations),
           rate_completed = response_rate(completed, invitations),
           contacted = coalesce(invitations, MIN_CONTACTS)) %>%
    transmute(wave,
              target_group = label_of(target_group, TARGET_GROUP_LABELS,
                                      "target group"),
              first_sent = if_else(is.na(first_sent), "--",
                                   format(first_sent, "%Y-%m-%d")),
              invitations = fmt_int(invitations),
              started = fmt_int(shown_count(started, contacted)),
              completed = fmt_int(shown_count(completed, contacted)),
              rate_started = fmt_pct(rate_started),
              rate_completed = fmt_pct(rate_completed))
  # A wave has one target group, so here a response does go with its list type
  target_totals <- wave_counts %>%
    group_by(target_group) %>%
    summarise(invitations = sum(invitations),
              started = sum(started),
              completed = sum(completed)) %>%
    ungroup()
  # From the log itself: a wave without any starter counts among the invitations
  all_waves <- tibble(wave = "All waves", target_group = "",
                      invitations = sum(dispatch$invitations),
                      started = nrow(experts),
                      completed = sum(experts$is_complete))
  total_cells <- bind_rows(
    target_totals %>%
      transmute(wave = label_of(target_group, TARGET_GROUP_LABELS, "target group"),
                target_group = "", invitations, started, completed),
    all_waves
  ) %>%
    mutate(rate_started = response_rate(started, invitations),
           rate_completed = response_rate(completed, invitations)) %>%
    transmute(wave, target_group, first_sent = "",
              invitations = fmt_int(invitations),
              started = fmt_int(started),
              completed = fmt_int(completed),
              rate_started = fmt_pct(rate_started),
              rate_completed = fmt_pct(rate_completed))
  check_cells <- tibble(
    wave = c("Invitations in the dispatch log", "Rows of the sampling frame",
             "Persons on the lists (one row per e-mail address)",
             "Addresses on more than one list (counted on the first)"),
    target_group = "", first_sent = "",
    invitations = fmt_int(c(sum(dispatch$invitations), attr(frame, "n_raw"),
                            nrow(frame), attr(frame, "n_type_conflicts"))),
    started = "", completed = "", rate_started = "", rate_completed = ""
  )
  n_soft_launch <- sum(wave_counts$is_softlaunch)
  wave_panels <- c(nrow(wave_counts) - n_soft_launch, nrow(total_cells),
                   nrow(check_cells))
  names(wave_panels) <- c("Main waves", "By target group, and all waves",
                          "Cross-check against the dispatch log")
  if (n_soft_launch > 0) {
    wave_panels <- c("Soft launch" = n_soft_launch, wave_panels)
  }
  wave_note <- paste0(
    "Started and completed: persons, an invitation once. Rates here per invitation ",
    "of the dispatch log, as the frame does not say who was invited in which wave, ",
    "so they do not compare with the rates per person of the other tables. A wave ",
    "has one target group, so that its rate needs no answer to Q2. -- means fewer ",
    "than ", MIN_CONTACTS, " invitations or ", MIN_N, " answers, or a wave that is ",
    "not in the log. An invitation is not a person: someone invited in two waves ",
    "received two, so the log may exceed the persons on the lists, but a large gap ",
    "would mean that the frame is not what was mailed. ",
    sum(dispatch$n_partly_sent), " batches of the log are marked as only partly ",
    "sent, so its count is an upper bound. Reminders went out on different days and ",
    "not always to t-online addresses (dispatch log), so the rates of the waves ",
    "differ for that reason too."
  )
  wave_file <- "tab_response_wave_all.tex"
  table_custom(bind_rows(wave_cells, total_cells, check_cells),
               file.path(dirs$tables, wave_file), latex_text(wave_note),
               col_names = c("Wave", "Target group", "First sent", "Invitations",
                             "Started", "Completed",
                             "\\makecell{Started\\\\per invitation}",
                             "\\makecell{Completed\\\\per invitation}"),
               align = c("l", "l", "l", "r", "r", "r", "r", "r"),
               panels = wave_panels)
  exhibits <- bind_rows(exhibits, exhibit_row(
    block, "response_wave", ALL_EXPERTS, "table", file.path("tables", wave_file),
    "Invitations, started and completed by wave", "appendix"
  ))

  exhibits
}
