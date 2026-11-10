################################################################################
# section_experts_firms.R — Block D: who the experts are and what their firms are
#                           like
# Inputs : experts (docs/data_contract.md)
# Outputs: tables/tab_experts_<group>.tex, tab_experts_groups_all.tex;
#          maps/map_experts_state_all.pdf
# Checks : unknown answers and the minimum cell, in summary_exhibits()
################################################################################

#' The experts and their firms: a table in its versions, and the map of the states
#'
#' @param experts one row per expert of the sample, as in experts.rds
#' @param dirs    the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_experts_firms <- function(experts, dirs) {
  block <- "D. Experts and firms"

  # 2.1 The table: the expert, then the firm
  rows <- bind_rows(
    tribble(
      ~panel,       ~variable,                 ~kind,   ~level, ~label,                        ~digits,
      "The expert", "age",                     "num",   NA,     "Age (years)",                 1,
      "The expert", "dem_training_year_clean", "year",  NA,     "Year training was completed", 0,
      "The expert", "resp_is_owner",           "level", "Ja",   "Owner or managing director",  1
    ),
    tibble(panel = "The expert: highest degree (Q22)",
           variable = "dem_education_recoded", kind = "level",
           level = names(EDUCATION_LABELS), label = unname(EDUCATION_LABELS),
           digits = 1),
    tibble(panel = "The expert: continuing education, last three years (Q24)",
           variable = names(TRAINING_LABELS), kind = "level", level = "Ja",
           label = unname(TRAINING_LABELS), digits = 1),
    tibble(panel = "The firm", variable = "firm_employees_clean", kind = "num",
           level = NA_character_, label = "Employees", digits = 1),
    tibble(panel = "The firm: revenue, last fiscal year (Q4)",
           variable = "firm_revenue", kind = "level",
           level = names(REVENUE_LABELS), label = unname(REVENUE_LABELS),
           digits = 1),
    tibble(panel = "The firm: service area (Q6)",
           variable = "firm_radius_recoded", kind = "level",
           level = names(RADIUS_LABELS), label = unname(RADIUS_LABELS),
           digits = 1),
    tibble(panel = "The firm: category, several ticks possible (Q2)",
           variable = names(FIRM_CATEGORY_LABELS), kind = "level", level = "Ja",
           label = unname(FIRM_CATEGORY_LABELS), digits = 1)
  )
  notes <- paste0(
    "Age: ", SURVEY_YEAR, " minus the year of birth. Years and employees without ",
    "impossible answers; employees also without the ", 100 * TRIM_QUANTILES[1],
    "% lowest and highest answers of all experts. Shares among the experts who ",
    "answered. Q6: the soft launch had no answer \"nationwide\". Q2 sets the ",
    "group: HVAC before chimney sweep before energy advice, so the shares of Q2 ",
    "within a group are in part mechanical."
  )
  exhibits <- summary_exhibits(experts, rows, "experts",
                               "The experts and their firms", notes, block,
                               "main", dirs)

  # 2.2 Experts per state (all experts only). A state below the minimum cell has
  # no number: its count would describe too few experts.
  if ("bundesland" %in% names(experts)) {
    comma_text <- label_comma()
    state_counts <- experts %>%
      filter(!is.na(bundesland)) %>%
      count(bundesland) %>%
      transmute(state = as.character(bundesland),
                value = if_else(n >= MIN_N, as.numeric(n), NA_real_),
                label = comma_text(n))
    if (any(!is.na(state_counts$value))) {
      plot <- plot_state_map(state_counts, "Experts")
      file_name <- paste0("map_experts_state_", group_slug(ALL_EXPERTS), ".pdf")
      save_figure(plot, file.path(dirs$maps, file_name), size = "map")
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "experts_state", ALL_EXPERTS, "map",
        file.path("maps", file_name), "Experts per federal state", "appendix",
        note = paste0("The state follows from the postcode of the firm (Q0). A ",
                      "state with fewer than ", MIN_N,
                      " experts is gray, without a number.")
      ))
    }
  }
  exhibits
}
