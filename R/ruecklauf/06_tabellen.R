# =============================================================================
# 06_tabellen.R  -  the numbers behind the figures
# =============================================================================
# Writes tabellen_ruecklauf.xlsx into RUECKLAUF_DIR. One workbook for the whole
# analysis rather than one per folder: every sheet carries `kontakte` (the
# t-online variant) and `antworten` (the response definition) as columns, so all
# four combinations can be compared in a pivot table instead of by opening four
# files.
#
# Runs once, after the figures, and recomputes everything from `frame` and
# `resp_all` so it does not depend on what the loop left behind.
# =============================================================================

# Every combination of t-online variant and response definition, as one long
# table per breakdown.
ueber_alle_varianten <- function(f) {
  TONLINE_VARIANTS %>%
    pmap(function(folder, drop_tonline, label) {
      frame_x <- if (drop_tonline) frame %>% filter(!is_tonline) else frame

      RUECKLAUF_SAMPLES %>%
        pmap(function(folder, completers_only, label) {
          resp_x <- if (completers_only) resp_all %>% filter(is_complete) else resp_all
          f(frame_x, resp_x) %>% mutate(antworten = label)
        }) %>%
        list_rbind() %>%
        mutate(kontakte = label)
    }) %>%
    list_rbind() %>%
    relocate(kontakte, antworten) %>%
    mutate(kontakte  = factor(kontakte,  levels = TONLINE_VARIANTS$label),
           antworten = factor(antworten, levels = RUECKLAUF_SAMPLES$label)) %>%
    arrange(kontakte, antworten)
}


# --- the sheets --------------------------------------------------------------
ueberblick <- ueber_alle_varianten(function(f, r) {
  tibble(n_kontakte     = nrow(f),
         n_antworten    = nrow(r),
         ruecklaufquote = nrow(r) / nrow(f))
})

nach_berufsgruppe <- ueber_alle_varianten(function(f, r) {
  ruecklauf_by(stack_gruppen(f), stack_gruppen(r), by = "gruppe")
})

nach_bundesland <- ueber_alle_varianten(function(f, r) {
  ruecklauf_by(
    f %>% filter(!is.na(bundesland)) %>% stack_gruppen(),
    r %>% filter(!is.na(bundesland)) %>% stack_gruppen(),
    by = c("gruppe", "bundesland")
  )
}) %>%
  arrange(kontakte, antworten, gruppe, bundesland)

# Break-offs depend only on the respondents, so the t-online variant does not
# change them; only the full-answer definition is meaningful here.
abbruch <- bind_rows(
  abbruch_by(stack_gruppen(resp_all), by = "gruppe") %>%
    mutate(ebene = "Berufsgruppe", auspraegung = as.character(gruppe), .keep = "unused"),
  abbruch_by(resp_all %>% filter(!is.na(bundesland)), by = "bundesland") %>%
    mutate(ebene = "Bundesland", auspraegung = as.character(bundesland), .keep = "unused")
) %>%
  relocate(ebene, auspraegung)

# What the frame looks like before any rate is computed - the diagnostics that
# explain every denominator in this workbook.
rahmen <- tibble(
  kennzahl = c("Zeilen in der Rohdatei",
               "davon doppelte E-Mail-Adressen (zusammengefasst)",
               "davon mit widersprüchlicher Stichprobe",
               "Kontaktierte Personen (Nenner)",
               "davon mit t-online-Adresse",
               "davon ohne verwertbare Postleitzahl",
               "Versendete Einladungen laut Versandübersicht"),
  wert = c(attr(frame, "n_raw"),
           attr(frame, "n_duplicate_contacts"),
           attr(frame, "n_type_conflicts"),
           nrow(frame),
           sum(frame$is_tonline),
           sum(is.na(frame$bundesland)),
           versand_gesamt)
)

rahmen_nach_gruppe <- frame %>%
  summarise(n_kontakte  = n(),
            n_tonline   = sum(is_tonline),
            anteil_tonline = mean(is_tonline),
            ohne_plz    = sum(is.na(bundesland)),
            .by = berufsgruppe) %>%
  arrange(berufsgruppe)

# How the postcode was resolved, on both sides - a check that frame and survey
# were treated identically.
plz_herkunft <- bind_rows(
  frame    %>% count(quelle = bundesland_source) %>% mutate(seite = "Stichprobenrahmen"),
  resp_all %>% count(quelle = bundesland_source) %>% mutate(seite = "Befragte")
) %>%
  mutate(anteil = n / sum(n), .by = seite) %>%
  relocate(seite)


# --- notes -------------------------------------------------------------------
hinweise <- tribble(
  ~thema, ~hinweis,
  "Fragestellung",
  "Rücklaufquote = Personen, die geantwortet haben, geteilt durch Personen, die angeschrieben wurden.",

  "Nenner",
  str_c("Der Stichprobenrahmen (", basename(FRAME_FILE), "): eine Zeile je kontaktierter Person. ",
        "Mehrfach vorkommende E-Mail-Adressen werden zu einer Person zusammengefasst, damit niemand ",
        "doppelt gezählt wird. Personen, deren Einladung an eine t-online-Adresse ging, sind in der ",
        "Variante \"", TONLINE_VARIANTS$label[2], "\" aus dem Nenner entfernt."),

  "Zähler",
  str_c("Eine Zeile je Person, die den Fragebogen geöffnet hat. Mehrfach begonnene Fragebögen ",
        "desselben Tokens werden zusammengefasst; behalten wird der am weitesten fortgeschrittene ",
        "Versuch. In survey_main stehen ", fmt_n(nrow(load_analysis_data(FALSE)$main)),
        " Zeilen, aber nur ", fmt_n(nrow(resp_all)), " Personen."),

  "Berufsgruppe",
  unname(NUMERATOR_SOURCE_LABELS[NUMERATOR_SOURCE]),

  "Nicht zuordenbare Befragte",
  str_c("Befragte ohne beantwortete Frage Q2 zählen in \"", ALL_TYPES, "\", aber in keiner ",
        "einzelnen Berufsgruppe. Die drei Gruppenquoten summieren sich daher nicht zur Gesamtquote."),

  "Bundesland",
  str_c("Aus der Postleitzahl abgeleitet, für Rahmen und Befragte mit derselben Regel ",
        "(add_bundesland_from_plz() in R/ruecklauf/01_frame.R, identisch zu R/cleaning/03_recode.R). ",
        "Kontakte ohne verwertbare Postleitzahl fehlen auf beiden Seiten des Bruchs."),

  "Mindestbesetzung",
  str_c("Zellen mit weniger als ", MIN_KONTAKTE, " Kontakten werden nicht abgebildet ",
        "(MIN_KONTAKTE in R/ruecklauf/00_settings.R)."),

  "Wellenübergreifende Doppelungen",
  str_c("Token werden je Umfrage vergeben. Eine Person, die in zwei Wellen angeschrieben wurde, ",
        "hat zwei Token und wird im Zähler zweimal gezählt, im Nenner aber nur einmal. ",
        "Ohne eine Token-E-Mail-Zuordnung ist das nicht erkennbar."),

  "Erinnerungen",
  str_c("Die Wellen wurden unterschiedlich erinnert (Versandübersicht, Spalte1): 196938 inkl. ",
        "t-online, 583464 ausdrücklich ohne t-online, weitere Wellen an anderen Tagen. ",
        "Erinnerungen erhöhen den Rücklauf, Unterschiede zwischen Wellen sind daher nicht ",
        "allein regional zu deuten."),

  "Erstellt",
  str_c(format(Sys.time(), "%Y-%m-%d %H:%M"), " mit R/run_ruecklauf.R")
)

write_xlsx(
  list(hinweise           = hinweise,
       ueberblick         = ueberblick,
       nach_berufsgruppe  = nach_berufsgruppe,
       nach_bundesland    = nach_bundesland,
       abbruch            = abbruch,
       rahmen             = rahmen,
       rahmen_nach_gruppe = rahmen_nach_gruppe,
       plz_herkunft       = plz_herkunft),
  file.path(RUECKLAUF_DIR, "tabellen_ruecklauf.xlsx")
)

message("Tabellen -> ", file.path(RUECKLAUF_DIR, "tabellen_ruecklauf.xlsx"))
