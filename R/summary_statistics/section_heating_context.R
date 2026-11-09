################################################################################
# section_heating_context.R — Block I: the heating at home, district heating in
#                             the service area, and the climate of the region
# Inputs : experts (docs/data_contract.md); degree days (HDD_FILE) and the map of
#          the districts (KREIS_SHP), public data
# Outputs: tables/tab_heating_context_<group>.tex,
#          tab_heating_context_groups_all.tex;
#          figures/fig_own_heating_<group>.pdf;
#          maps/map_district_heating_state_all.pdf, map_degree_days_all.pdf
# Checks : unknown answers (label_of(), summary_exhibits()); the join of the map
################################################################################

#' The answer to Q20 of the experts who were asked it. own_heating says "Keine
#' Angabe" also for an expert who broke off before Q20 and so never saw it: those
#' are NA here, so that "No answer" means the expert left it blank.
#'
#' @param experts one row per expert, with own_heating, last_question_position
#' @return character vector
own_heating_of_asked <- function(experts) {
  is_q20 <- QUESTIONNAIRE$item == "time_dem_own_heating"
  if_else(experts$last_question_position >= QUESTIONNAIRE$position[is_q20],
          experts$own_heating, NA_character_)
}

#' The heating context of the experts
#'
#' @param experts one row per expert of the sample, as in experts.rds
#' @param dirs    the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_heating_context <- function(experts, dirs) {
  block <- "I. Heating context"
  exhibits <- tibble()
  comma_text <- label_comma()
  if (has_columns(experts, c("own_heating", "last_question_position"))) {
    experts <- experts %>%
      mutate(own_heating_asked = own_heating_of_asked(experts))
  }

  # 1. The table ---------------------------------------------------------------
  rows <- bind_rows(
    tibble(panel = "District heating in the service area (Q6a)",
           variable = "firm_dh_in_area", kind = "level",
           level = names(DISTRICT_HEATING_LABELS),
           label = unname(DISTRICT_HEATING_LABELS), digits = 1),
    tibble(panel = "Heating of the expert's home (Q20)",
           variable = "own_heating_asked", kind = "level",
           level = names(OWN_HEATING_LABELS), label = unname(OWN_HEATING_LABELS),
           digits = 1),
    tibble(panel = "Climate", variable = "hdd", kind = "num",
           level = NA_character_,
           label = paste0("Heating degree days, ", HDD_YEAR), digits = 0)
  )
  context_notes <- paste(
    "Q6a sets the arm of the vignettes: experts with district heating available",
    "or planned in their service area can recommend it. Q20: how is the building",
    "you live in heated? Among the experts who got as far; No answer: left blank.",
    "Degree days: Eurostat, of the NUTS3 region of the firm's postcode."
  )
  exhibits <- bind_rows(exhibits, summary_exhibits(
    experts, rows, "heating_context",
    "District heating, the heating at home and the climate", context_notes, block,
    "main", dirs
  ))

  # 2. The heating at home -----------------------------------------------------
  if ("own_heating_asked" %in% names(experts)) {
    for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
      answers <- subset_group(experts, group)$own_heating_asked
      answers <- answers[!is.na(answers)]
      if (length(answers) < MIN_N) {
        next
      }
      # Every answer keeps its row, also one nobody in this version gave
      answer_labels <- label_of(answers, OWN_HEATING_LABELS, "answer of Q20")
      bars <- tibble(item = unname(OWN_HEATING_LABELS))
      bars$share <- 0
      for (k in seq_len(nrow(bars))) {
        bars$share[k] <- mean(answer_labels == bars$item[k])
      }
      bars <- bars %>%
        mutate(item = factor(item, levels = OWN_HEATING_LABELS),
               series = item)
      plot <- plot_share_bars(bars, TECHNOLOGY_COLOURS,
                              "Share of the experts who were asked")
      file_name <- paste0("fig_own_heating_", group_slug(group), ".pdf")
      save_figure(plot, file.path(dirs$figures, file_name))
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "own_heating", group, "figure", file.path("figures", file_name),
        "The heating of the experts' homes",
        if_else(group == ALL_EXPERTS, "main", "appendix"),
        note = paste0(
          "Q20: how is the building you live in heated? The ",
          comma_text(length(answers)), " experts who got as far as the question; ",
          "No answer: left blank."
        )
      ))
    }
  }

  # 3. District heating by state (all experts only) ----------------------------
  if (has_columns(experts, c("firm_dh_in_area", "bundesland"))) {
    answer <- as.character(experts$firm_dh_in_area)
    # Stops on an answer without a label
    label_of(answer, DISTRICT_HEATING_LABELS, "answer of Q6a")
    answer_available <- names(DISTRICT_HEATING_LABELS)[
      DISTRICT_HEATING_LABELS == "Available"]
    is_available <- if_else(is.na(answer), NA_real_,
                            as.numeric(answer == answer_available))
    state_values <- state_means(experts, is_available, digits = 0, percent = TRUE)
    if (any(!is.na(state_values$value))) {
      plot <- plot_state_map(state_values, "Available",
                             legend_labels = label_percent(accuracy = 1))
      file_name <- paste0("map_district_heating_state_",
                          group_slug(ALL_EXPERTS), ".pdf")
      save_figure(plot, file.path(dirs$maps, file_name), size = "map")
      exhibits <- bind_rows(exhibits, exhibit_row(
        block, "district_heating_state", ALL_EXPERTS, "map",
        file.path("maps", file_name), "District heating in the service area",
        "main",
        note = paste0(
          "Q6a: share of the experts of a state, by the postcode of the firm, who ",
          "answer that district heating is available in their service area, among ",
          "all who answered, also those who do not know. A state with fewer than ",
          MIN_N, " answers is gray, without a number."
        )
      ))
    }
  }

  # 4. Degree days by region (public data, no answers) -------------------------
  # st_read(): reads the map; GF 9: the land area of the regions, one polygon each
  regions <- st_read(KREIS_SHP, quiet = TRUE) %>%
    filter(GF == 9)
  states <- st_read(BUNDESLAND_SHP, quiet = TRUE) %>%
    filter(GF == 9)
  degree_days <- read.csv(HDD_FILE) %>%
    filter(indic_nrg == "HDD", TIME_PERIOD == HDD_YEAR) %>%
    transmute(NUTS = geo, hdd = OBS_VALUE)
  # Two districts can share a NUTS3 region (Hanau and Main-Kinzig-Kreis)
  regions <- regions %>%
    left_join(degree_days, by = "NUTS", relationship = "many-to-one")
  # geom_sf(): draws the polygons of a map
  plot <- ggplot() +
    geom_sf(data = regions, aes(fill = hdd), colour = "white", linewidth = 0.05) +
    # The states on top: without their borders the districts are hard to place
    geom_sf(data = states, fill = NA, colour = "white", linewidth = 0.35) +
    scale_fill_gradient(low = MAP_LOW, high = MAP_HIGH, labels = label_comma(),
                        na.value = COLOUR_NOT_SHOWN,
                        name = "Heating\ndegree days") +
    coord_sf(datum = NA, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "right",
          legend.title = element_text(size = figure_font_size - 1),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  file_name <- paste0("map_degree_days_", group_slug(ALL_EXPERTS), ".pdf")
  save_figure(plot, file.path(dirs$maps, file_name), size = "map")
  exhibits <- bind_rows(exhibits, exhibit_row(
    block, "degree_days", ALL_EXPERTS, "map", file.path("maps", file_name),
    paste0("Heating degree days by region, ", HDD_YEAR), "appendix",
    note = paste0("Eurostat (nrg_chddr2_a), heating degree days of each NUTS3 ",
                  "region in ", HDD_YEAR, ". Public data, no answers of the ",
                  "survey.")
  ))
  exhibits
}
