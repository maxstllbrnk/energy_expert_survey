# =============================================================================
# 04_berufsgruppen.R  -  response rate by the sample the lists were drawn from
# =============================================================================
#   ruecklauf_nach_berufsgruppe          the rate per sample
#   kontakte_nach_berufsgruppe           how the frame splits across the three
#   ruecklauf_berufsgruppe_bundesland    the rate per sample and state, as tiles
#   abbruch_nach_berufsgruppe            break-offs per sample (all answers only)
#
# The three samples are the registers the mailing lists were built from
# (DENA / Schornsteinfeger / SHK), not the self-report in Q2 - see
# FRAME_TYPE_LABELS in 00_settings.R. While the numerator is still classified
# from Q2, every caption says so.
#
# Uses frame_v and resp_v, set by the runner.
# =============================================================================

gruppen <- ruecklauf_by(stack_gruppen(frame_v), stack_gruppen(resp_v), by = "gruppe") %>%
  filter(n_kontakte >= MIN_KONTAKTE)


# --- the rate per sample -----------------------------------------------------
if (nrow(gruppen) > 0) {
  p <- gruppen %>%
    ggplot(aes(x = fct_rev(gruppe), y = ruecklaufquote)) +
    geom_col(fill = COL_BAR, width = 0.7) +
    geom_text(aes(label = percent_rate(ruecklaufquote)), hjust = -0.2,
              size = LABEL_SIZE, color = INK_SECONDARY) +
    scale_y_continuous(labels = percent_rate,
                       expand = expansion(mult = c(0, 0.18))) +
    coord_flip() +
    labs(title = "Wie hoch ist der Rücklauf je Berufsgruppe?",
         subtitle = wrap_subtitle(
           str_c("Anteil der kontaktierten Personen, die geantwortet haben. ",
                 "Die Gruppen sind die drei Stichproben, aus denen die ",
                 "Versandlisten gezogen wurden")),
         x = NULL, y = "Rücklaufquote",
         caption = rl_caption(sum(gruppen$n_antworten[gruppen$gruppe == ALL_TYPES]),
                              sum(gruppen$n_kontakte[gruppen$gruppe == ALL_TYPES]),
                              str_c("Befragte ohne zuordenbare Berufsgruppe zählen nur in ",
                                    "\"", ALL_TYPES, "\""),
                              gruppiert = TRUE)) +
    theme(panel.grid.major.y = element_blank())
  save_figure(p, "03_berufsgruppen", "ruecklauf_nach_berufsgruppe")
}


# --- the frame itself --------------------------------------------------------
# How many people each list held, so the rates above can be read against the
# size of the group they come from.
kontakte <- gruppen %>% filter(gruppe != ALL_TYPES)

if (nrow(kontakte) > 0) {
  p <- kontakte %>%
    mutate(anteil = n_kontakte / sum(n_kontakte)) %>%
    ggplot(aes(x = fct_rev(gruppe), y = n_kontakte)) +
    geom_col(fill = COL_BAR_LIGHT, width = 0.7) +
    geom_text(aes(label = str_c(fmt_n(n_kontakte), " (", percent_rate(anteil), ")")),
              hjust = -0.1, size = LABEL_SIZE, color = INK_SECONDARY) +
    scale_y_continuous(labels = number_de, expand = expansion(mult = c(0, 0.25))) +
    coord_flip() +
    labs(title = "Wie viele Experten wurden je Berufsgruppe kontaktiert?",
         subtitle = "Der Nenner der Rücklaufquote, je Stichprobe",
         x = NULL, y = "Kontaktierte Personen",
         caption = rl_caption(sum(kontakte$n_antworten), sum(kontakte$n_kontakte))) +
    theme(panel.grid.major.y = element_blank())
  save_figure(p, "03_berufsgruppen", "kontakte_nach_berufsgruppe")
}


# --- sample x federal state --------------------------------------------------
# A heatmap rather than sixteen small maps: it shows at a glance whether a
# group's response rate is regionally even, which is what "regionale Muster"
# means once the group is held constant.
tiles <- ruecklauf_by(
  frame_v %>% filter(!is.na(bundesland)) %>% stack_gruppen(),
  resp_v  %>% filter(!is.na(bundesland)) %>% stack_gruppen(),
  by = c("gruppe", "bundesland")
) %>%
  filter(gruppe != ALL_TYPES, n_kontakte >= MIN_KONTAKTE)

if (nrow(tiles) > 0) {
  p <- tiles %>%
    ggplot(aes(x = gruppe, y = fct_rev(bundesland), fill = ruecklaufquote)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = percent_rate(ruecklaufquote)), size = LABEL_SIZE,
              color = INK_PRIMARY) +
    scale_fill_gradient(low = MAP_RATE_LOW, high = MAP_RATE_HIGH,
                        labels = percent_rate, name = "Rücklauf") +
    scale_x_discrete(position = "top") +
    labs(title = "Rücklauf nach Berufsgruppe und Bundesland",
         subtitle = str_c("Zellen mit weniger als ", MIN_KONTAKTE,
                          " Kontakten sind nicht dargestellt"),
         x = NULL, y = NULL,
         caption = rl_caption(sum(tiles$n_antworten), sum(tiles$n_kontakte),
                              gruppiert = TRUE)) +
    theme(panel.grid = element_blank(),
          legend.position = "right",
          legend.title = element_text(color = INK_SECONDARY))
  save_figure(p, "03_berufsgruppen", "ruecklauf_berufsgruppe_bundesland", height = 16)
}


# --- break-offs per sample ---------------------------------------------------
if (!rl_sample$completers_only) {
  abbruch <- abbruch_by(stack_gruppen(resp_v), by = "gruppe") %>%
    filter(n_begonnen >= MIN_KONTAKTE)

  if (nrow(abbruch) > 0) {
    p <- abbruch %>%
      ggplot(aes(x = fct_rev(gruppe), y = abbruchquote)) +
      geom_col(fill = MAP_ABBR_HIGH, width = 0.7) +
      geom_text(aes(label = percent_rate(abbruchquote)), hjust = -0.2,
                size = LABEL_SIZE, color = INK_SECONDARY) +
      scale_y_continuous(labels = percent_rate, expand = expansion(mult = c(0, 0.18))) +
      coord_flip() +
      labs(title = "Wer bricht den Fragebogen ab?",
           subtitle = wrap_subtitle(
             str_c("Anteil der Befragten, die den Fragebogen begonnen, ",
                   "aber nicht abgeschickt haben")),
           x = NULL, y = "Abbruchquote",
           caption = rl_caption(sum(abbruch$n_abbruch[abbruch$gruppe == ALL_TYPES]),
                                sum(abbruch$n_begonnen[abbruch$gruppe == ALL_TYPES]),
                                "Nenner hier: Personen, die den Fragebogen geöffnet haben",
                                gruppiert = TRUE)) +
      theme(panel.grid.major.y = element_blank())
    save_figure(p, "03_berufsgruppen", "abbruch_nach_berufsgruppe")
  }
}
