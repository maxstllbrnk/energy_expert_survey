# =============================================================================
# 05_preferences.R  -  time and risk preferences
# =============================================================================
# Both were elicited with the staircase of the Global Preference Survey (Falk et
# al. 2018): up to five binary choices, each setting the terms of the next. The
# first choice is shown as it was asked; the score summarises all of an expert's
# choices (midpoint of their cell, see R/add_preferences.R). Figures in
# 08_praeferenzen/.
#
# Uses sm (one row per expert) of the current subset.
# =============================================================================

# --- the first choice, as asked -----------------------------------------------
plot_bar(sm, "tp_1", subtitle = "Zeitpräferenz: erste von bis zu fünf Entscheidungen") %>%
  save_figure("08_praeferenzen", "zeitpraeferenz_erste_entscheidung", height = 7)

plot_bar(sm, "risk_1", subtitle = "Risikopräferenz: erste von bis zu fünf Entscheidungen") %>%
  save_figure("08_praeferenzen", "risikopraeferenz_erste_entscheidung", height = 7)

# --- the scores ----------------------------------------------------------------
# One bin per cell of the five-step staircase (32 cells).
plot_histogram(sm, "tp_score",
               title = "Zeitpräferenz: Wie geduldig sind die Befragten?",
               subtitle = str_c("Score aus bis zu fünf Entscheidungen zwischen 100 Euro heute und ",
                                "einem höheren Betrag in 12 Monaten. 0 = am ungeduldigsten, ",
                                "1 = am geduldigsten."),
               x_label = "Geduld (Score)", binwidth = 1 / 32) %>%
  save_figure("08_praeferenzen", "zeitpraeferenz_score")

plot_histogram(sm, "risk_score",
               title = "Risikopräferenz: Wie risikobereit sind die Befragten?",
               subtitle = str_c("Score aus bis zu fünf Entscheidungen zwischen einer Verlosung ",
                                "(50 % Chance auf 300 Euro) und einer sicheren Zahlung. ",
                                "0 = am risikoscheuesten, 1 = am risikofreudigsten."),
               x_label = "Risikobereitschaft (Score)", binwidth = 1 / 32) %>%
  save_figure("08_praeferenzen", "risikopraeferenz_score")
