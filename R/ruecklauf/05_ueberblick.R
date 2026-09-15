# =============================================================================
# 05_ueberblick.R  -  the headline numbers
# =============================================================================
#   ruecklauf_ueberblick        contacted -> started -> completed, as a funnel
#   ruecklauf_tonline_vergleich the rate with and without t-online contacts
#
# The second figure is the direct answer to "was the mailing hurt by Telekom
# blocking it?". It is drawn from the whole frame rather than the current
# variant, so it is the same figure in both variant folders on purpose: it is
# the comparison between them.
#
# Uses frame (the whole frame), frame_v and resp_v, set by the runner.
# =============================================================================

# --- contacted -> started -> completed ---------------------------------------
stufen <- tibble(
  stufe = factor(c("Kontaktiert", "Fragebogen begonnen", "Fragebogen abgeschlossen"),
                 levels = c("Kontaktiert", "Fragebogen begonnen", "Fragebogen abgeschlossen")),
  n     = c(nrow(frame_v), nrow(resp_v), sum(resp_v$is_complete))
) %>%
  mutate(anteil = n / first(n))

if (nrow(frame_v) >= MIN_KONTAKTE) {
  p <- stufen %>%
    ggplot(aes(x = fct_rev(stufe), y = n)) +
    geom_col(fill = c(COL_BAR_LIGHT, COL_BAR, COL_BAR)[3:1], width = 0.7) +
    geom_text(aes(label = str_c(fmt_n(n), "  (", percent_rate(anteil), ")")),
              hjust = -0.1, size = LABEL_SIZE, color = INK_SECONDARY) +
    scale_y_continuous(labels = number_de, expand = expansion(mult = c(0, 0.25))) +
    coord_flip() +
    labs(title = "Vom Kontakt zur abgeschlossenen Antwort",
         subtitle = "Anteile bezogen auf die Zahl der kontaktierten Personen",
         x = NULL, y = "Personen",
         caption = rl_caption(nrow(resp_v), nrow(frame_v),
                              str_c("Mehrfach begonnene Fragebögen zählen als eine Person"))) +
    theme(panel.grid.major.y = element_blank())
  save_figure(p, "01_ueberblick", "ruecklauf_ueberblick")
}


# --- with and without t-online ----------------------------------------------
# Same numerator, two denominators: the whole frame, and the frame without the
# contacts Telekom may never have delivered to. The gap between the two bars is
# how much the blocked addresses depress the measured response rate.
vergleich <- TONLINE_VARIANTS %>%
  pmap(function(folder, drop_tonline, label) {
    # Only the denominator changes. The numerator stays resp_v in both bars:
    # the response data does not say which address an answer came from, so the
    # respondents cannot be split into t-online and the rest.
    f <- if (drop_tonline) frame %>% filter(!is_tonline) else frame
    ruecklauf_by(stack_gruppen(f), stack_gruppen(resp_v), by = "gruppe") %>%
      mutate(variante = label)
  }) %>%
  list_rbind() %>%
  filter(n_kontakte >= MIN_KONTAKTE) %>%
  mutate(variante = factor(variante, levels = TONLINE_VARIANTS$label))

if (nrow(vergleich) > 0) {
  p <- vergleich %>%
    ggplot(aes(x = fct_rev(gruppe), y = ruecklaufquote, fill = variante)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = percent_rate(ruecklaufquote)),
              position = position_dodge(width = 0.75), hjust = -0.2,
              size = LABEL_SIZE, color = INK_SECONDARY) +
    scale_y_continuous(labels = percent_rate, expand = expansion(mult = c(0, 0.2))) +
    scale_fill_manual(values = set_names(c(COL_BAR_LIGHT, COL_BAR), TONLINE_VARIANTS$label),
                      name = NULL) +
    coord_flip() +
    labs(title = "Wie stark drücken die t-online-Adressen den Rücklauf?",
         subtitle = wrap_subtitle(
           str_c("Gleiche Antworten, zwei Nenner: alle Kontakte, und ohne die ",
                 "t-online-Adressen, die Telekom möglicherweise blockiert hat")),
         x = NULL, y = "Rücklaufquote",
         caption = rl_caption(
           sum(vergleich$n_antworten[vergleich$gruppe == ALL_TYPES &
                                       vergleich$variante == TONLINE_VARIANTS$label[1]]),
           sum(vergleich$n_kontakte[vergleich$gruppe == ALL_TYPES &
                                      vergleich$variante == TONLINE_VARIANTS$label[1]]),
           str_c(fmt_n(sum(frame$is_tonline)), " der ", fmt_n(nrow(frame)),
                 " Kontakte haben eine t-online-Adresse. Der Zähler ist in beiden ",
                 "Balken derselbe - welche Antwort von einer t-online-Adresse kam, ",
                 "ist in den Antwortdaten nicht erkennbar"),
           gruppiert = TRUE)) +
    theme(panel.grid.major.y = element_blank())
  save_figure(p, "01_ueberblick", "ruecklauf_tonline_vergleich", height = 12)
}
