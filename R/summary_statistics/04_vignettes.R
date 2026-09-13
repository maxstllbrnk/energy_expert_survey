# =============================================================================
# 04_vignettes.R  -  recommendations, response times and costs in the vignettes
# =============================================================================
# The Fernwärme branch is between-subject: experts whose service area has or
# plans district heating (Q6a) saw vignettes WITH Fernwärme as an option, all
# others WITHOUT it. The arms offer different choices, so every figure shows
# them separately. Figures in 05_vignetten/.
#
# Uses vl (one row per expert x vignette) of the current subset.
# =============================================================================

# Answered vignettes only, with readable and ordered attribute levels.
vig <- vl %>%
  filter(!is.na(vig_rec), !is.na(vig_arm)) %>%
  mutate(
    arm      = factor(unname(ARM_LABELS[as.character(vig_arm)]), levels = ARM_LABELS),
    vig_rec  = fct_relevel(vig_rec, names(TECH_COLORS)),
    position = str_c("Vignette ", vig_num),
    att_income = fct_relevel(att_income, "unter 40.000 € brutto im Jahr"),
    att_replacement_timing = fct_recode(att_replacement_timing,
      "Defekt, muss kurzfristig ersetzt werden"    = "ist defekt und muss kurzfristig ersetzt werden",
      "Muss in den nächsten Jahren ersetzt werden" = "muss in den nächsten Jahren ersetzt werden"),
    att_heat_distribution = fct_recode(att_heat_distribution,
      "Fußbodenheizung" = "eine Fußbodenheizung"),
    att_renovation = case_when(
      str_detect(att_renovation, "nicht saniert")       ~ "Nicht saniert (200 kWh/m²)",
      str_detect(att_renovation, "teilsaniert")         ~ "Teilsaniert (120 kWh/m²)",
      str_detect(att_renovation, "vollständig saniert") ~ "Vollständig saniert (70 kWh/m²)"),
    att_renovation = factor(att_renovation, levels = c("Nicht saniert (200 kWh/m²)",
                                                       "Teilsaniert (120 kWh/m²)",
                                                       "Vollständig saniert (70 kWh/m²)"))
  )


# =============================================================================
# recommendations
# =============================================================================
plot_stacked(vig, y = "arm", fill = "vig_rec", colors = TECH_COLORS,
             title = question_text("vig_rec"),
             subtitle = "Anteil an allen beantworteten Vignetten, je Fernwärme-Arm") %>%
  save_figure("05_vignetten", "empfehlungen", height = 8)

plot_stacked(vig, y = "position", fill = "vig_rec", colors = TECH_COLORS, facet = "arm",
             title = question_text("vig_rec"),
             subtitle = "Nach Position der Vignette im Fragebogen") %>%
  save_figure("05_vignetten", "empfehlungen_nach_position", height = 15)

pwalk(VIGNETTE_ATTRIBUTES, function(variable, file, title) {
  plot_stacked(vig, y = variable, fill = "vig_rec", colors = TECH_COLORS, facet = "arm",
               title = question_text("vig_rec"),
               subtitle = str_c("Nach Vignettenmerkmal: ", title)) %>%
    save_figure("05_vignetten", str_c("empfehlungen_nach_", file), height = 11)
})

if (group_label == ALL_GROUP) {
  vig %>%
    filter(berufsgruppe %in% BERUFSGRUPPEN) %>%
    plot_stacked(y = "berufsgruppe", fill = "vig_rec", colors = TECH_COLORS, facet = "arm",
                 title = question_text("vig_rec"), subtitle = "Nach Berufsgruppe") %>%
    save_figure("05_vignetten", "empfehlungen_nach_berufsgruppe", height = 11)
}


# =============================================================================
# response time per vignette
# =============================================================================
for (arm_code in names(ARM_LABELS)) {
  vig %>%
    filter(vig_arm == arm_code) %>%
    plot_histogram("vig_time",
                   title = "Wie lange haben die Befragten für eine Vignette gebraucht?",
                   subtitle = str_c("Antwortzeit je Vignette · ", ARM_LABELS[[arm_code]]),
                   x_label = "Sekunden") %>%
    save_figure("05_vignetten", str_c("bearbeitungszeit_vignette_", arm_code))
}

median_times <- vig %>%
  filter(!is.na(vig_time)) %>%
  summarise(seconds = median(vig_time), n = n(), .by = c(arm, vig_num)) %>%
  filter(n >= MIN_N)

if (nrow(median_times) > 0) {
  p <- ggplot(median_times, aes(x = vig_num, y = seconds, color = arm)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    scale_color_manual(values = ARM_COLORS) +
    scale_x_continuous(breaks = 1:6, labels = str_c("Vignette ", 1:6)) +
    scale_y_continuous(labels = number_de, limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.1))) +
    labs(title = "Wie lange haben die Befragten für eine Vignette gebraucht?",
         subtitle = "Median der Antwortzeit nach Position der Vignette im Fragebogen",
         x = NULL, y = "Sekunden (Median)", caption = make_caption(sum(median_times$n))) +
    theme(panel.grid.major.x = element_blank())
  save_figure(p, "05_vignetten", "bearbeitungszeit_nach_position")
}


# =============================================================================
# expected investment cost (asked about the last vignette only)
# =============================================================================
costs <- vig %>% filter(!is.na(vig_cost))

plot_stacked(costs, y = "arm", fill = "vig_cost", colors = COST_COLORS,
             title = question_text("vig_cost"),
             subtitle = "Zur letzten Vignette, je Fernwärme-Arm") %>%
  save_figure("05_vignetten", "kosten", height = 8)

plot_stacked(costs, y = "vig_rec", fill = "vig_cost", colors = COST_COLORS, facet = "arm",
             title = question_text("vig_cost"),
             subtitle = "Zur letzten Vignette, nach empfohlener Heiztechnologie und Fernwärme-Arm") %>%
  save_figure("05_vignetten", "kosten_nach_technologie", height = 14)
