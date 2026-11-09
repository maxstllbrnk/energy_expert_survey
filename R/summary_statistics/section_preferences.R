################################################################################
# section_preferences.R — Block H: the time and risk preferences of the experts
# Inputs : experts (docs/data_contract.md)
# Outputs: tables/tab_preferences_<group>.tex;
#          figures/fig_preference_distribution_<group>.pdf
# Checks : the minimum cell (summary_table(), plot_binned_histogram())
################################################################################
# summary_exhibits(), plot_binned_histogram(): in section_experts_firms.R

# The width of the bins of the scores. The scores are the midpoints of 32 cells,
# or of wider ones for those with a shorter path, whose midpoints would fall on the
# edges of bins of 1/32: 1/16 puts every path of four or five answers inside a bin.
SCORE_BINWIDTH <- 1 / 16

#' The scores of the two staircases
#'
#' @param experts one row per expert of the sample, as in experts.rds
#' @param dirs    the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_preferences <- function(experts, dirs) {
  block <- "H. Preferences"
  exhibits <- tibble()

  # 1. The table ---------------------------------------------------------------
  preference_notes <- paste(
    "The staircases of the Global Preference Survey (Falk et al. 2018), without",
    "its self-assessment, so a valid ordering of the experts but not comparable",
    "with its published means: up to five binary choices, each setting the terms",
    "of the next. Time: 100 euro today or a higher amount in 12 months. Risk: a",
    "lottery with a 50% chance of 300 euro or a sure payment. The score is the",
    "midpoint of the cell the answers lead to: 0 the least, 1 the most patient or",
    "willing to take risks. Whole staircase: as deep as the expert's branch goes,",
    "among the experts who made at least one choice."
  )
  rows <- tribble(
    ~panel,                             ~variable,    ~kind,   ~level, ~label,                                        ~digits,
    "Score (0 to 1)",                   "tp_score",   "num",   NA,     "Time preference (patience)",                  2,
    "Score (0 to 1)",                   "risk_score", "num",   NA,     "Risk preference (willingness to take risks)", 2,
    "Went through the whole staircase", "tp_whole",   "dummy", NA,     "Time preference",                             1,
    "Went through the whole staircase", "risk_whole", "dummy", NA,     "Risk preference",                             1
  )
  if (has_columns(experts, c("tp_levels", "tp_complete"))) {
    experts <- experts %>%
      mutate(tp_whole = if_else(tp_levels > 0, as.numeric(tp_complete), NA_real_))
  }
  if (has_columns(experts, c("risk_levels", "risk_complete"))) {
    experts <- experts %>%
      mutate(risk_whole = if_else(risk_levels > 0, as.numeric(risk_complete),
                                  NA_real_))
  }
  exhibits <- bind_rows(exhibits, summary_exhibits(
    experts, rows, "preferences", "Time and risk preferences", preference_notes,
    block, "appendix", dirs
  ))

  # 2. The distribution of the two scores --------------------------------------
  scores <- tribble(
    ~column,      ~panel,
    "tp_score",   "Time preference (patience)",
    "risk_score", "Risk preference (willingness to take risks)"
  )
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_experts <- subset_group(experts, group)
    values <- tibble()
    for (k in seq_len(nrow(scores))) {
      if (scores$column[k] %in% names(group_experts)) {
        values <- bind_rows(values, tibble(
          panel = scores$panel[k], value = group_experts[[scores$column[k]]]
        ))
      }
    }
    if (nrow(values) == 0) {
      next
    }
    values <- values %>%
      filter(!is.na(value)) %>%
      mutate(panel = factor(panel, levels = scores$panel))
    plot <- plot_binned_histogram(values, SCORE_BINWIDTH, "Score", n_columns = 2)
    if (is.null(plot)) {
      next
    }
    file_name <- paste0("fig_preference_distribution_", group_slug(group), ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "preference_distribution", group, "figure",
      file.path("figures", file_name),
      "Distribution of the time and risk preferences", "appendix",
      note = paste(
        "Staircases of the Global Preference Survey: 0 the least, 1 the most",
        "patient or willing to take risks (see the table of the preferences). Bins",
        "of 1/16. A bin of fewer than", MIN_N, "answers is not drawn; the mean and",
        "the median are over all answers."
      )
    ))
  }
  exhibits
}
