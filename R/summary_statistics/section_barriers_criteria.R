################################################################################
# section_barriers_criteria.R — Block G: the barriers the experts see (Q19a to
#                               Q19c) and what matters to their customers (Q28)
# Inputs : experts (docs/data_contract.md)
# Outputs: tables/tab_barriers_<group>.tex, tab_customer_weights_<group>.tex;
#          figures/fig_barriers_<file>_<group>.pdf,
#          fig_customer_criteria_<group>.pdf
# Checks : unknown answers (label_of(), barrier_shares()); the minimum cell
################################################################################
# summary_exhibits(), multiline_header(), plot_share_bars(): in
# section_experts_firms.R

# Q28 asks for the three criteria that matter most
CRITERION_RANKS <- c("crit_rank1", "crit_rank2", "crit_rank3")
# The heads of the columns of the barriers table: the captions of BARRIER_QUESTIONS
# are too long for them
BARRIER_HEADERS <- c(Q19a = "Renewable heating", Q19b = "District heating",
                     Q19c = "Conventional heating")

#' The share ticking each option of one barrier question, among those who answered
#'
#' @param data   one row per expert
#' @param prefix the question: barr_ren_, barr_dh_ or barr_conv_
#' @return tibble, one row per option: option, n, share
barrier_shares <- function(data, prefix) {
  shares <- tibble()
  for (option in names(BARRIER_LABELS)) {
    answers <- as.character(data[[paste0(prefix, option)]])
    unknown_answers <- setdiff(answers[!is.na(answers)], c("Ja", "Nicht Gewählt"))
    if (length(unknown_answers) > 0) {
      stop("Answers other than Ja and Nicht Gewählt in ", prefix, option, ": ",
           paste(unknown_answers, collapse = ", "))
    }
    shares <- bind_rows(shares, tibble(
      option = option, n = sum(!is.na(answers)),
      share = mean(answers == "Ja", na.rm = TRUE)
    ))
  }
  shares
}

#' The barriers and the criteria of the customers
#'
#' @param experts one row per expert of the sample, as in experts.rds
#' @param dirs    the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_barriers_criteria <- function(experts, dirs) {
  block <- "G. Barriers and criteria"
  exhibits <- tibble()
  comma_text <- label_comma()
  percent_text <- label_percent(accuracy = 1)

  # 1. The barriers, one figure per question -----------------------------------
  has_question <- logical(nrow(BARRIER_QUESTIONS))
  for (q in seq_len(nrow(BARRIER_QUESTIONS))) {
    has_question[q] <- has_columns(
      experts, paste0(BARRIER_QUESTIONS$prefix[q], names(BARRIER_LABELS))
    )
  }
  questions <- BARRIER_QUESTIONS[has_question, ]

  # The options by their share among all experts, in every version of the
  # question; "Other" and "I see no barriers" at the end. desc(): from the largest
  option_orders <- list()
  for (q in seq_len(nrow(questions))) {
    by_share <- barrier_shares(experts, questions$prefix[q]) %>%
      filter(!(option %in% c("other", "none"))) %>%
      arrange(desc(share))
    option_orders[[questions$question[q]]] <- c(by_share$option, "other", "none")

    # An expert who submits the page without any tick counts among those who
    # answered: not even "I see no barriers". If the question was not mandatory,
    # they may have skipped it, and the shares are too low.
    option_columns <- paste0(questions$prefix[q], names(BARRIER_LABELS))
    is_shown <- rowSums(!is.na(experts[option_columns])) > 0
    n_ticks <- rowSums(experts[option_columns] == "Ja", na.rm = TRUE)
    message(questions$question[q], ": ", sum(is_shown & n_ticks == 0), " of ",
            sum(is_shown), " experts shown the question ticked no option")
  }

  for (q in seq_len(nrow(questions))) {
    question <- questions[q, ]
    option_order <- option_orders[[question$question]]
    id <- paste0("barriers_", question$file)
    for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
      shares <- barrier_shares(subset_group(experts, group), question$prefix)
      if (shares$n[1] < MIN_N) {
        next
      }
      bars <- shares %>%
        mutate(item = factor(label_of(option, BARRIER_LABELS, "barrier"),
                             levels = unname(BARRIER_LABELS[option_order])),
               series = "all")
      plot <- plot_share_bars(bars, c(all = COLOUR_BAR),
                              "Share of the experts who answered")
      file_name <- paste0("fig_", id, "_", group_slug(group), ".pdf")
      save_figure(plot, file.path(dirs$figures, file_name))
      is_main <- question$question == "Q19a" && group == ALL_EXPERTS
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, id, group, "figure", file.path("figures", file_name),
        question$caption, if_else(is_main, "main", "appendix"),
        note = paste0(
          question$question, ": thinking of the development of the demand in ",
          "existing buildings among your customers, which barriers do you see for ",
          "a positive development? Several answers possible. ",
          comma_text(shares$n[1]), " experts answered."
        )
      ))
    }
  }

  # 2. The three questions side by side ----------------------------------------
  # The rows in the order of the first question
  table_order <- option_orders[[questions$question[1]]]
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_experts <- subset_group(experts, group)
    cells <- tibble(option = c(latex_text(unname(BARRIER_LABELS[table_order])),
                               "Experts who answered"))
    is_any_shown <- FALSE
    for (q in seq_len(nrow(questions))) {
      # match(): the rows in the order of table_order
      shares <- barrier_shares(group_experts, questions$prefix[q])
      shares <- shares[match(table_order, shares$option), ]
      if (shares$n[1] >= MIN_N) {
        cells[[questions$question[q]]] <- c(fmt_pct(shares$share),
                                            fmt_int(shares$n[1]))
        is_any_shown <- TRUE
      } else {
        cells[[questions$question[q]]] <- "--"
      }
    }
    if (!is_any_shown) {
      next
    }
    headers <- paste0(BARRIER_HEADERS[questions$question], " (",
                      questions$question, ")")
    file_name <- paste0("tab_barriers_", group_slug(group), ".tex")
    table_custom(
      cells, file.path(dirs$tables, file_name),
      notes = latex_text(paste(
        "Q19a to Q19c: thinking of the development of the demand for renewable",
        "heating (including heat pumps), district heating and conventional",
        "heating in existing buildings among your customers, which barriers do",
        "you see for a positive development? Several answers possible. Share of",
        "the experts who answered the question.", cell_note()
      )),
      col_names = c("Option", multiline_header(headers, width = 10)),
      panels = c("Share ticking the option" = length(table_order), "Sample" = 1)
    )
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "barriers", group, "table", file.path("tables", file_name),
      "Barriers to the demand for the three kinds of heating", "appendix"
    ))
  }

  # 3. Q28: one row per expert x criterion named --------------------------------
  if (!has_columns(experts, CRITERION_RANKS)) {
    return(exhibits)
  }
  # Q28 asks for three criteria: ranks 4 to 8 should be empty
  further_ranks <- intersect(paste0("crit_rank", 4:8), names(experts))
  n_further_ranks <- sum(rowSums(!is.na(experts[further_ranks])) > 0)
  if (n_further_ranks > 0) {
    message("Q28: ", n_further_ranks, " experts ranked more than three criteria; ",
            "the ranks from 4 on are left out")
  }
  # all_of(): the columns named in this vector
  criteria_named <- experts %>%
    select(resp_uid, expert_group, all_of(CRITERION_RANKS)) %>%
    pivot_longer(all_of(CRITERION_RANKS), names_to = "rank_column",
                 values_to = "criterion", values_drop_na = TRUE) %>%
    mutate(rank_number = match(rank_column, CRITERION_RANKS),
           criterion = label_of(criterion, CRITERION_LABELS, "criterion")) %>%
    group_by(resp_uid) %>%
    # The ranking in weights that sum to 1: of k criteria named, the one on place r
    # among them gets (k + 1 - r) / (k (k + 1) / 2), that is 3/6, 2/6, 1/6 for
    # three, so that an expert who named fewer counts as much. min_rank(): the
    # place, so that a skipped rank leaves no gap
    mutate(n_named = n(),
           place = min_rank(rank_number),
           weight = (n_named + 1 - place) / (n_named * (n_named + 1) / 2)) %>%
    ungroup()
  if (nrow(criteria_named) == 0) {
    return(exhibits)
  }

  # In every version the order of all experts; a criterion nobody named at the end
  criterion_order <- criteria_named %>%
    count(criterion) %>%
    arrange(desc(n)) %>%
    pull(criterion)
  criterion_order <- c(criterion_order,
                       setdiff(unname(CRITERION_LABELS), criterion_order))

  # 4. Q28: mentions, stacked by rank -------------------------------------------
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_named <- subset_group(criteria_named, group)
    n_experts <- n_distinct(group_named$resp_uid)
    if (n_experts < MIN_N) {
      next
    }
    bars <- group_named %>%
      count(criterion, rank_number) %>%
      mutate(share = n / n_experts,
             rank_label = factor(paste("Rank", rank_number),
                                 levels = names(RANK_COLOURS)))
    totals <- bars %>%
      group_by(criterion) %>%
      summarise(share = sum(share)) %>%
      ungroup() %>%
      mutate(share_label = percent_text(share))
    plot <- ggplot(bars, aes(x = share, y = criterion)) +
      geom_col(aes(fill = rank_label), width = 0.7, colour = "white",
               linewidth = 0.2, position = position_stack(reverse = TRUE)) +
      geom_text(data = totals, aes(label = share_label), hjust = -0.15,
                size = (figure_font_size - 2) / .pt, colour = COLOUR_INK) +
      scale_fill_manual(values = RANK_COLOURS) +
      # Every criterion has its row, also one nobody named in this version
      scale_y_discrete(limits = rev(criterion_order)) +
      scale_x_continuous(labels = percent_text, limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Share of the experts naming it", y = NULL) +
      theme(panel.grid.major.y = element_blank(),
            legend.key.size = unit(8, "pt"))
    file_name <- paste0("fig_customer_criteria_", group_slug(group), ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "customer_criteria", group, "figure",
      file.path("figures", file_name), "What matters most to the customers",
      if_else(group == ALL_EXPERTS, "main", "appendix"),
      note = paste0(
        "Q28: which three of the following criteria matter most to your ",
        "customers? Share of the ", comma_text(n_experts), " experts who named at ",
        "least one, by the rank they gave it."
      )
    ))
  }

  # 5. Q28: the weights of the ranking ------------------------------------------
  # One row per expert who named a criterion, one column per criterion: its weight
  # in percent, 0 if not named
  weights <- criteria_named %>%
    distinct(resp_uid, expert_group)
  weight_rows <- tibble()
  for (k in seq_along(criterion_order)) {
    named_here <- criteria_named %>%
      filter(criterion == criterion_order[k])
    column <- paste0("weight_", k)
    # coalesce(): 0 for an expert who did not name this criterion
    weights[[column]] <- 100 * coalesce(
      named_here$weight[match(weights$resp_uid, named_here$resp_uid)], 0
    )
    weight_rows <- bind_rows(weight_rows, tibble(
      panel = "Weight of the criterion (%)", variable = column, kind = "num",
      level = NA_character_, label = criterion_order[k], digits = 1
    ))
  }
  weight_notes <- paste(
    "Q28: which three of the following criteria matter most to your customers?",
    "The ranking of each expert in weights that sum to 100: of k criteria named,",
    "the one on place r among them gets (k + 1 - r) / (k (k + 1) / 2), that is",
    "3/6, 2/6 and 1/6 for three; a criterion not named gets 0. Over the experts",
    "who named at least one; the means of a table sum to 100."
  )
  exhibits <- bind_rows(exhibits, summary_exhibits(
    weights, weight_rows, "customer_weights",
    "What matters most to the customers: weights of the ranking", weight_notes,
    block, "appendix", dirs
  ))
  exhibits
}
