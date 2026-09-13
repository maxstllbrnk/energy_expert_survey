# =============================================================================
# 07_tables.R  -  the numbers behind the figures, one workbook per sample
# =============================================================================
# Writes tabellen.xlsx into the sample folder, with all groups in one long
# table each (column `gruppe`). Continuous variables go through the same outlier
# rule as the histograms, so a mean in the table is the mean in the figure.
#
# Uses dat (the whole sample, all groups) and sample_spec, set by the runner.
# =============================================================================

# One copy of the data per group, stacked, so that every statistic can be
# computed with .by = gruppe.
stack_groups <- function(data) {
  bind_rows(
    data %>% mutate(gruppe = ALL_GROUP),
    data %>% filter(berufsgruppe %in% BERUFSGRUPPEN) %>% mutate(gruppe = as.character(berufsgruppe))
  ) %>%
    mutate(gruppe = factor(gruppe, levels = GROUPS))
}

main_by_group <- dat$main %>%
  mutate(bearbeitungszeit_min = if_else(is_complete, time_interview / 60, NA_real_)) %>%
  stack_groups()

vignettes_by_group <- dat$vignettes %>%
  filter(!is.na(vig_rec), !is.na(vig_arm)) %>%
  mutate(arm     = factor(unname(ARM_LABELS[as.character(vig_arm)]), levels = ARM_LABELS),
         vig_rec = fct_relevel(vig_rec, names(TECH_COLORS))) %>%
  stack_groups()

# Summary statistics of the values that survive the outlier rule.
describe <- function(values, variable) {
  kept <- remove_outliers(values, variable)
  tibble(n          = length(kept),
         n_entfernt = sum(!is.na(values)) - length(kept),
         mittelwert = mean(kept),
         sd         = sd(kept),
         min        = quantile(kept, 0,    names = FALSE),
         p25        = quantile(kept, 0.25, names = FALSE),
         median     = median(kept),
         p75        = quantile(kept, 0.75, names = FALSE),
         max        = quantile(kept, 1,    names = FALSE))
}

# Counts and shares of one categorical variable, per group.
count_answers <- function(data, variable) {
  frage <- question_text(variable)
  data %>%
    filter(!is.na(.data[[variable]])) %>%
    count(gruppe, antwort = .data[[variable]]) %>%
    mutate(anteil = n / sum(n), .by = gruppe) %>%
    arrange(antwort, gruppe) %>%
    transmute(variable = variable, frage = frage, antwortoption = question_item(variable),
              antwort = as.character(antwort), gruppe, n, anteil)
}


# --- sample ----------------------------------------------------------------------
stichprobe <- main_by_group %>%
  summarise(befragte                  = n(),
            abgeschlossen             = sum(is_complete),
            vignetten_erreicht        = sum(reached_vignettes),
            mittlere_anzahl_vignetten = mean(n_vignettes_answered),
            .by = gruppe)

# How the Q2 ticks map onto the Berufsgruppen - makes the priority rule checkable.
berufsgruppen_zuordnung <- dat$main %>%
  count(berufsgruppe,
        shk              = firm_cat_hvac,
        schornsteinfeger = firm_cat_chimney,
        energieberatung  = firm_cat_energyadvice,
        ingenieurbuero   = firm_cat_engineering)

# --- continuous variables ------------------------------------------------------------
continuous_vars <- c(HISTOGRAMS$variable, "tp_score", "risk_score", "bearbeitungszeit_min")

stetige_variablen <- main_by_group %>%
  select(gruppe, all_of(continuous_vars)) %>%
  pivot_longer(-gruppe, names_to = "variable", values_to = "wert", values_drop_na = TRUE) %>%
  summarise(describe(wert, first(variable)), .by = c(variable, gruppe)) %>%
  mutate(frage         = map_chr(variable, question_text),
         antwortoption = question_item(variable),
         .after = variable) %>%
  arrange(factor(variable, levels = continuous_vars), gruppe)

# --- categorical variables -----------------------------------------------------------
categorical_vars <- c("berufsgruppe", BARS$variable, unlist(MULTIPLE_CHOICE$variables),
                      str_subset(names(dat$main), "^pexp_"), "tp_1", "risk_1")

kategoriale_variablen <- map(categorical_vars, ~ count_answers(main_by_group, .x)) %>%
  list_rbind()

# --- vignettes -----------------------------------------------------------------------
vignetten_empfehlungen <- vignettes_by_group %>%
  count(arm, gruppe, empfehlung = vig_rec) %>%
  mutate(anteil = n / sum(n), .by = c(arm, gruppe))

vignetten_kosten <- vignettes_by_group %>%
  filter(!is.na(vig_cost)) %>%
  count(arm, gruppe, empfehlung = vig_rec, kosten = vig_cost) %>%
  mutate(anteil = n / sum(n), .by = c(arm, gruppe, empfehlung))

vignetten_zeit <- vignettes_by_group %>%
  filter(!is.na(vig_time)) %>%
  summarise(describe(vig_time, "vig_time"), .by = c(arm, gruppe))

# --- notes -----------------------------------------------------------------------------
hinweise <- tribble(
  ~thema,              ~hinweis,
  "Stichprobe",        sample_spec$label,
  "Gruppen",           str_c("Alle Berufsgruppen sowie je Berufsgruppe. Zuordnung aus Q2 (Mehrfachauswahl) ",
                             "nach Priorität: SHK-Handwerk vor Schornsteinfeger vor Energieberater ",
                             "(Energieberatungs- sowie Architektur-/Ingenieurbüros). ",
                             "Siehe add_berufsgruppe() in R/prepare_analysis_data.R."),
  "Stetige Variablen", str_c("Zuerst werden unmögliche Werte entfernt (VALID_RANGE), dann Werte unter dem ",
                             fmt(100 * TRIM_QUANTILES[1]), "-%- und über dem ",
                             fmt(100 * TRIM_QUANTILES[2]), "-%-Quantil (TRIM_QUANTILES). ",
                             "n_entfernt zählt beides; alle Kennzahlen beziehen sich auf die übrigen Werte. ",
                             "Nicht getrimmt: ", str_c(NOT_TRIMMED, collapse = ", "), ". ",
                             "Einstellungen in R/summary_statistics/00_settings.R."),
  "Bearbeitungszeit",  "bearbeitungszeit_min: Gesamtzeit laut LimeSurvey, nur abgeschlossene Fragebögen.",
  "Vignetten",         "Getrennt nach Fernwärme-Arm (Q6a). Die Kostenfrage bezieht sich nur auf die letzte Vignette.",
  "Erstellt",          str_c(format(Sys.time(), "%Y-%m-%d %H:%M"), " mit R/run_summary_statistics.R")
)

write_xlsx(
  list(hinweise                = hinweise,
       stichprobe              = stichprobe,
       berufsgruppen_zuordnung = berufsgruppen_zuordnung,
       stetige_variablen       = stetige_variablen,
       kategoriale_variablen   = kategoriale_variablen,
       vignetten_empfehlungen  = vignetten_empfehlungen,
       vignetten_kosten        = vignetten_kosten,
       vignetten_zeit          = vignetten_zeit),
  file.path(SUMMARY_DIR, sample_spec$folder, "tabellen.xlsx")
)
