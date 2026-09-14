# =============================================================================
# 03_questions.R  -  one figure per survey question outside the vignettes
# =============================================================================
# Which questions are plotted, and into which folder, is set in 00_settings.R
# (HISTOGRAMS, BARS, MULTIPLE_CHOICE, RANKINGS). Two questions need a figure of
# their own and come at the end: the price expectations (Q17), shown for all
# energy carriers at once, and the manufacturer (Q11c), which is free text.
#
# Uses sm (one row per expert) of the current subset.
# =============================================================================

# --- continuous variables: histograms ----------------------------------------
pwalk(HISTOGRAMS, function(variable, section, x_label, binwidth, log_scale) {
  plot_histogram(sm, variable, x_label = x_label, binwidth = binwidth, log_scale = log_scale) %>%
    save_figure(section, file_name(variable))
})

# --- categorical variables: bar charts ---------------------------------------
pwalk(BARS, function(variable, section, height) {
  plot_bar(sm, variable) %>%
    save_figure(section, file_name(variable), height = height)
})

# --- "tick all that apply" -----------------------------------------------------
pwalk(MULTIPLE_CHOICE, function(file, section, variables) {
  plot_multiple_choice(sm, variables) %>%
    save_figure(section, file, height = 4 + 0.8 * length(variables))
})

# --- rankings ----------------------------------------------------------------------
# Mentions by rank, then the mean weights - written in this order, so that the
# report shows the two figures of a question next to each other.
pwalk(RANKINGS, function(file, section, variables) {
  plot_ranking(sm, variables) %>%
    save_figure(section, file, height = 11)
  plot_ranking_weights(sm, variables) %>%
    save_figure(section, str_c(file, "_gewichte"), height = 11)
})

# --- Q17: expected price development, all energy carriers in one figure --------
expectation_vars <- str_subset(names(sm), "^pexp_")

sm %>%
  select(resp_uid, all_of(expectation_vars)) %>%
  pivot_longer(-resp_uid, names_to = "variable", values_to = "expectation",
               values_drop_na = TRUE) %>%
  mutate(carrier = factor(question_item(variable), levels = question_item(expectation_vars))) %>%
  plot_stacked(y = "carrier", fill = "expectation", colors = EXPECTATION_COLORS,
               title = question_text("pexp_gas"),
               subtitle = "Preis in fünf Jahren im Vergleich zu Anfang 2026, je Energieträger") %>%
  save_figure("06_energiepreise", "Q17_preisentwicklung", height = 11)

# --- Q11c: manufacturer sold most, free text -----------------------------------------
# Spellings are harmonised only in case and spacing. The 15 most frequent
# manufacturers are shown, all others are pooled.
sm %>%
  filter(!is.na(brand_top1)) %>%
  mutate(brand = str_to_title(str_squish(brand_top1)),
         brand = fct_lump_n(brand, n = 15, other_level = "Andere Hersteller", ties.method = "first"),
         brand = fct_infreq(brand),
         brand = fct_relevel(brand, intersect("Andere Hersteller", levels(brand)), after = Inf)) %>%
  plot_bar("brand", title = question_text("brand_top1"),
           subtitle = "Freitext, vereinheitlicht in Groß- und Kleinschreibung; die 15 häufigsten Hersteller") %>%
  save_figure("07_heiztechnologien", "Q11c1b_hersteller", height = 13)
