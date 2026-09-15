# =============================================================================
# 03_karten.R  -  response rate on the map
# =============================================================================
# Choropleths on the official BKG state boundaries (map_states, loaded once by
# the runner), drawn exactly like experten_nach_bundesland.png in the
# descriptive report - same palette, same white borders, same boxed labels - so
# the two can sit next to each other in a slide deck.
#
#   ruecklauf_nach_bundesland            the rate per state, all contacts
#   ruecklauf_nach_bundesland_<gruppe>   the same per Berufsgruppe
#   kontakte_nach_bundesland             the denominator itself
#   abbruch_nach_bundesland              break-offs, only in the "all answers" sample
#
# Uses frame_v and resp_v (the current t-online variant and response
# definition), set by the runner.
# =============================================================================

# Rate per state and group. Contacts without a usable postcode carry no state
# and are dropped from the map, on both sides of the fraction.
karte_daten <- ruecklauf_by(
  frame_v %>% filter(!is.na(bundesland)) %>% stack_gruppen(),
  resp_v  %>% filter(!is.na(bundesland)) %>% stack_gruppen(),
  by = c("gruppe", "bundesland")
) %>%
  mutate(bundesland = as.character(bundesland))


# --- one choropleth ----------------------------------------------------------
# `values` is the column to fill by, already a share. Cells built on fewer than
# MIN_KONTAKTE contacts are drawn grey rather than coloured: a rate over 12
# contacts is noise and would otherwise dominate the colour scale.
karte_quote <- function(data, value_col, title, subtitle, legend, caption,
                        low = MAP_RATE_LOW, high = MAP_RATE_HIGH) {
  plot_data <- map_states %>%
    left_join(data, by = c("GEN" = "bundesland")) %>%
    mutate(
      genug = !is.na(.data[[value_col]]) & coalesce(n_kontakte, 0L) >= MIN_KONTAKTE,
      wert  = if_else(genug, .data[[value_col]], NA_real_),
      label = if_else(genug, percent_rate(.data[[value_col]]), "–")
    )

  ggplot(plot_data) +
    geom_sf(aes(fill = wert), color = "white", linewidth = 0.3) +
    geom_sf_label(aes(label = label), size = LABEL_SIZE, color = INK_PRIMARY,
                  fill = "white", alpha = 0.85, linewidth = 0,
                  label.padding = unit(0.12, "lines")) +
    scale_fill_gradient(low = low, high = high, labels = percent_rate,
                        name = legend, na.value = MAP_LAND) +
    coord_sf(datum = NA) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, caption = caption) +
    theme(legend.position = "right",
          legend.title = element_text(color = INK_SECONDARY))
}


# --- the rate, overall and per Berufsgruppe ----------------------------------
for (g in RUECKLAUF_GRUPPEN) {
  d <- karte_daten %>% filter(gruppe == g)
  if (nrow(d) == 0 || sum(d$n_kontakte) < MIN_KONTAKTE) next

  gruppen_zusatz <- if (g == ALL_TYPES) "" else str_c(" – ", g)
  name <- if (g == ALL_TYPES) "ruecklauf_nach_bundesland"
          else str_c("ruecklauf_nach_bundesland_", folder_name(g))

  p <- karte_quote(
    d, "ruecklaufquote",
    title    = str_c("Wie hoch ist der Rücklauf je Bundesland?", gruppen_zusatz),
    subtitle = wrap_subtitle(str_c("Anteil der kontaktierten Personen, die geantwortet haben, ",
                     "je Bundesland aus der Postleitzahl")),
    legend   = "Rücklauf",
    caption  = rl_caption(sum(d$n_antworten), sum(d$n_kontakte),
                          str_c("Bundesländer mit weniger als ", MIN_KONTAKTE,
                                " Kontakten sind nicht eingefärbt"),
                          gruppiert = g != ALL_TYPES)
  )
  save_figure(p, "02_karten", name, height = 19)
}


# --- the denominator ---------------------------------------------------------
# A rate alone invites the wrong reading: a state can look weak simply because
# very few experts were contacted there. This map shows the frame itself.
kontakte_karte <- karte_daten %>% filter(gruppe == ALL_TYPES)

if (sum(kontakte_karte$n_kontakte) >= MIN_KONTAKTE) {
  p <- map_states %>%
    left_join(kontakte_karte, by = c("GEN" = "bundesland")) %>%
    mutate(n_kontakte = coalesce(n_kontakte, 0L)) %>%
    ggplot() +
    geom_sf(aes(fill = n_kontakte), color = "white", linewidth = 0.3) +
    geom_sf_label(aes(label = fmt_n(n_kontakte)), size = LABEL_SIZE, color = INK_PRIMARY,
                  fill = "white", alpha = 0.85, linewidth = 0,
                  label.padding = unit(0.12, "lines")) +
    scale_fill_gradient(low = MAP_LOW, high = MAP_HIGH, labels = number_de,
                        name = "Kontakte") +
    coord_sf(datum = NA) +
    labs(title = "Wie viele Experten wurden je Bundesland kontaktiert?",
         subtitle = "Der Nenner der Rücklaufquote: Anzahl der angeschriebenen Personen",
         x = NULL, y = NULL,
         caption = rl_caption(sum(kontakte_karte$n_antworten),
                              sum(kontakte_karte$n_kontakte))) +
    theme(legend.position = "right",
          legend.title = element_text(color = INK_SECONDARY))
  save_figure(p, "02_karten", "kontakte_nach_bundesland", height = 19)
}


# --- break-offs --------------------------------------------------------------
# Only meaningful where break-offs are still in the data; in the completer
# sample the rate is zero everywhere by construction.
if (!rl_sample$completers_only) {
  abbruch <- abbruch_by(resp_v %>% filter(!is.na(bundesland)), by = "bundesland") %>%
    mutate(bundesland = as.character(bundesland),
           n_kontakte = n_begonnen)   # the "enough observations" rule applies to starters

  if (sum(abbruch$n_begonnen) >= MIN_KONTAKTE) {
    p <- karte_quote(
      abbruch, "abbruchquote",
      title    = "Wo brechen die Befragten ab?",
      subtitle = wrap_subtitle(str_c("Anteil der Befragten, die den Fragebogen begonnen, ",
                       "aber nicht abgeschickt haben")),
      legend   = "Abbruch",
      caption  = rl_caption(sum(abbruch$n_abbruch), sum(abbruch$n_begonnen),
                            str_c("Nenner hier: Personen, die den Fragebogen geöffnet haben. ",
                                  "Bundesländer mit weniger als ", MIN_KONTAKTE,
                                  " Befragten sind nicht eingefärbt")),
      low = MAP_ABBR_LOW, high = MAP_ABBR_HIGH
    )
    save_figure(p, "02_karten", "abbruch_nach_bundesland", height = 19)
  }
}
