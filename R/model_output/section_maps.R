################################################################################
# section_maps.R — maps of the predicted recommendations by federal state
# Inputs : collect_results(), for one run: a set of effects of the main model
#          whose profiles are federal states (observed_states in the estimation
#          scripts)
# Outputs: in dirs$maps: map_probability_<set>_<outcome>.pdf
# Checks : a state below the minimum cell is grey (collect_results())
################################################################################

#' Maps of the probability of each recommendation by state, from every set of the
#' main model whose profiles are federal states
#'
#' @param collected  collect_results()
#' @param run_name   the run
#' @param main_model the model
#' @param placement  "main" or "appendix", of the heat pump's map
#' @param dirs       the folders of the run's exhibits
#' @return list: exhibits (exhibit_row()s), problems (text, for "Not shown")
section_maps <- function(collected, run_name, main_model, placement, dirs) {
  block <- "E. Maps"
  exhibits <- tibble()
  run_row <- collected$runs[collected$runs$run == run_name, ]
  model_row <- collected$models[collected$models$run == run_name &
                                  collected$models$model == main_model, ]
  state_sets <- collected$sets %>%
    filter(run == run_name, model == main_model, kind == "states")
  if (nrow(model_row) == 0 || nrow(state_sets) == 0) {
    return(list(exhibits = exhibits, problems = character(0)))
  }
  model_outcomes <- collected$outcomes %>%
    filter(run == run_name, model == main_model) %>%
    arrange(position)
  for (set_name in state_sets$set) {
    set_row <- state_sets[state_sets$set == set_name, ]
    for (k in seq_len(nrow(model_outcomes))) {
      # The states as they are named in the data and on the map; a state below
      # the minimum cell has no value and stays grey with "–". formatC(): the
      # number with exactly `digits` decimals
      state_values <- collected$baselines %>%
        filter(run == run_name, model == main_model, set == set_name,
               outcome == model_outcomes$outcome[k]) %>%
        transmute(state = profile, value = probability,
                  label = paste0(formatC(100 * probability, format = "f",
                                         digits = 0), "%"))
      if (all(is.na(state_values$value))) {
        next
      }
      # One decimal in the legend, as the states may differ by little
      plot <- plot_state_map(state_values, "Probability",
                             legend_labels = label_percent(accuracy = 0.1))
      file_name <- paste0("map_probability_", set_name, "_",
                          slug_of(model_outcomes$outcome_label[k]), ".pdf")
      save_figure(plot, file.path(dirs$maps, file_name), size = "map")
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, paste0("map_", set_name), ALL_EXPERTS, "map",
        file.path("maps", file_name),
        paste0("Predicted probability of ",
               outcome_phrase(model_outcomes$outcome_label[k]),
               " by state, the main model"),
        if (model_outcomes$outcome[k] == model_row$reference) placement else
          "appendix",
        paste0("Sample: ", run_row$sample_label, ". The average predicted ",
               "probability over the experts of each state, each with their own ",
               "characteristics, at this household: ", set_row$household, ". ",
               "The random constants are averaged over, so the map varies only ",
               "with the experts' measured characteristics. Grey and \"–\": ",
               "fewer than ", MIN_N, " experts. ", fmt_int(set_row$n_draws),
               " Halton draws over the random constants.")
      ))
    }
  }
  list(exhibits = exhibits, problems = character(0))
}
