# =============================================================================
# 02_sample.R  -  who is in the sample, where they dropped out, how long it took
# =============================================================================
# Figures in 01_stichprobe/ and 02_bearbeitungszeit/.
# Uses sm (one row per expert) and vl (one row per expert x vignette) of the
# current subset, set by R/run_summary_statistics.R.
# =============================================================================


# =============================================================================
# composition
# =============================================================================
if (group_label == ALL_GROUP) {
  plot_bar(sm, "berufsgruppe",
           subtitle = str_c("Q2 erlaubt Mehrfachauswahl. Zuordnung nach Priorität: SHK-Handwerk vor ",
                            "Schornsteinfeger vor Energieberater (Energieberatungs- sowie ",
                            "Architektur-/Ingenieurbüros). \"Keine Zuordnung\": nur \"Sonstiges\" gewählt.")) %>%
    save_figure("01_stichprobe", "berufsgruppe", height = 8)
}

if (!sample_spec$completers_only) {
  sm %>%
    mutate(status = factor(if_else(is_complete, "Abgeschlossen", "Abgebrochen"),
                           levels = c("Abgeschlossen", "Abgebrochen"))) %>%
    plot_bar("status", title = "Wie viele Befragte haben den Fragebogen abgeschlossen?",
             subtitle = "Abgeschlossen = Fragebogen bis zum Ende ausgefüllt und abgeschickt") %>%
    save_figure("01_stichprobe", "fragebogen_abgeschlossen", height = 7)
}

sm %>%
  mutate(n_vignettes = factor(n_vignettes_answered, levels = 0:6)) %>%
  plot_bar("n_vignettes", title = "Wie viele der sechs Vignetten haben die Befragten beantwortet?") %>%
  save_figure("01_stichprobe", "anzahl_beantworteter_vignetten", height = 9)


# =============================================================================
# drop-out (only meaningful when break-offs are in the sample)
# =============================================================================
dropouts <- sm %>% filter(!is_complete)

if (!sample_spec$completers_only && nrow(dropouts) >= MIN_N) {

  # --- LimeSurvey's page counter, as recorded -----------------------------------
  p <- dropouts %>%
    filter(!is.na(meta_lastpage)) %>%
    count(meta_lastpage) %>%
    ggplot(aes(x = meta_lastpage, y = n)) +
    geom_col(fill = COL_BAR, width = 0.8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Auf welcher Seite haben die Befragten den Fragebogen verlassen?",
         subtitle = wrap_subtitle(str_c(
           "LimeSurvey-Variable lastpage, nur Abbrecher. Wegen der Filterführung steht dieselbe ",
           "Seitenzahl nicht bei allen Befragten für dieselbe Frage - siehe abbruch_nach_frage.png.")),
         x = "Letzte Seite (lastpage)", y = "Anzahl Abbrecher",
         caption = make_caption(sum(!is.na(dropouts$meta_lastpage))))
  save_figure(p, "01_stichprobe", "abbruch_lastpage")

  # --- the last question answered ---------------------------------------------------
  # In questionnaire order from top to bottom. Questions after which at least 5 %
  # of all break-offs happened are highlighted.
  by_question <- dropouts %>%
    count(last_question_position, last_question) %>%
    mutate(share         = n / sum(n),
           highlight     = share >= 0.05,
           last_question = fct_reorder(last_question, -last_question_position))

  p <- ggplot(by_question, aes(x = n, y = last_question, fill = highlight)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = str_c(fmt_n(n), " (", percent_de(share), ")")),
              hjust = -0.1, size = LABEL_SIZE, color = INK_SECONDARY) +
    scale_fill_manual(values = c("TRUE" = COL_BAR, "FALSE" = COL_BAR_LIGHT), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = "Nach welcher Frage haben die Befragten den Fragebogen abgebrochen?",
         subtitle = wrap_subtitle(str_c(
           "Zuletzt beantwortete Frage (letzte Frage mit erfasster Antwortzeit), in der Reihenfolge ",
           "des Fragebogens. Dunkel: mindestens 5 % aller Abbrüche.")),
         x = "Anzahl Abbrecher", y = NULL, caption = make_caption(nrow(dropouts))) +
    theme(panel.grid.major.y = element_blank())
  save_figure(p, "01_stichprobe", "abbruch_nach_frage", height = 6 + 0.42 * nrow(by_question))

  # --- how far experts got, by section ----------------------------------------------
  # An expert has reached a section when their last answered question is at or
  # after its first question - also if routing hid that section from them.
  reached <- QUESTIONNAIRE %>%
    summarise(first_position = min(position), .by = section) %>%
    mutate(section = as.character(section),
           share   = map_dbl(first_position, ~ mean(sm$last_question_position >= .x))) %>%
    add_row(section = "Fragebogen abgeschickt", share = mean(sm$is_complete))

  p <- reached %>%
    mutate(section = fct_rev(fct_inorder(section))) %>%
    ggplot(aes(x = share, y = section)) +
    geom_col(fill = COL_BAR, width = 0.7) +
    geom_text(aes(label = percent_de(share)), hjust = -0.2, size = LABEL_SIZE,
              color = INK_SECONDARY) +
    scale_x_continuous(labels = percent_de, expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Wie weit sind die Befragten im Fragebogen gekommen?",
         subtitle = wrap_subtitle(str_c(
           "Anteil aller Befragten, die mindestens bis zu diesem Abschnitt gekommen sind ",
           "(auch wenn der Abschnitt für sie ausgeblendet war)")),
         x = "Anteil der Befragten", y = NULL, caption = make_caption(nrow(sm))) +
    theme(panel.grid.major.y = element_blank())
  save_figure(p, "01_stichprobe", "verbleib_nach_abschnitt", height = 10)
}


# =============================================================================
# how long it took
# =============================================================================
# time_interview is LimeSurvey's total time. For an expert who broke off it is
# the time until they left, so completers and break-offs are shown apart.
durations <- sm %>% mutate(bearbeitungszeit_min = time_interview / 60)

durations %>%
  filter(is_complete) %>%
  plot_histogram("bearbeitungszeit_min",
                 title = "Wie lange haben die Befragten für den Fragebogen gebraucht?",
                 subtitle = "Gesamte Bearbeitungszeit, abgeschlossene Fragebögen",
                 x_label = "Minuten") %>%
  save_figure("02_bearbeitungszeit", "bearbeitungszeit_abgeschlossen")

if (!sample_spec$completers_only) {
  durations %>%
    filter(!is_complete) %>%
    plot_histogram("bearbeitungszeit_min",
                   title = "Wie lange waren Abbrecher im Fragebogen, bevor sie ihn verlassen haben?",
                   subtitle = "Bearbeitungszeit bis zum Abbruch, abgebrochene Fragebögen",
                   x_label = "Minuten") %>%
    save_figure("02_bearbeitungszeit", "bearbeitungszeit_bis_abbruch")
}

# --- per section -----------------------------------------------------------------
# Per expert, the response times of all questions in a section are added up;
# the figure shows the median over experts who answered at least one of them.
section_times <- question_times(sm, vl) %>%
  summarise(seconds = sum(seconds), .by = c(resp_uid, section)) %>%
  summarise(minutes = median(seconds) / 60, n = n(), .by = section) %>%
  filter(n >= MIN_N)

if (nrow(section_times) > 0) {
  p <- ggplot(section_times, aes(x = minutes, y = fct_rev(section))) +
    geom_col(fill = COL_BAR, width = 0.7) +
    geom_text(aes(label = str_c(fmt(minutes), " min")), hjust = -0.15, size = LABEL_SIZE,
              color = INK_SECONDARY) +
    scale_x_continuous(labels = number_de, expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Wie lange haben die Befragten für die einzelnen Abschnitte gebraucht?",
         subtitle = wrap_subtitle(str_c(
           "Median der Bearbeitungszeit je Abschnitt (Summe der Antwortzeiten seiner Fragen), ",
           "unter allen, die im Abschnitt mindestens eine Frage beantwortet haben")),
         x = "Minuten (Median)", y = NULL, caption = make_caption(nrow(sm))) +
    theme(panel.grid.major.y = element_blank())
  save_figure(p, "02_bearbeitungszeit", "bearbeitungszeit_nach_abschnitt", height = 10)
}
