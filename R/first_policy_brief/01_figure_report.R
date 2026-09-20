# =============================================================================
# 01_figure_report.R  -  the figures of the first policy brief, in one report
# =============================================================================
# In RStudio: open energy_expert_survey.Rproj, open this file, click "Source".
# In a terminal, from the project folder:
#   Rscript R/first_policy_brief/01_figure_report.R
#
# Picks the figures the first policy brief uses out of the summary statistics
# and puts them into one report, sorted into the sections of the brief:
#
#   FIRST_POLICY_BRIEF_DIR/          output_dropbox/first_policy_brief/
#     bericht_abbildungen.html       the figures, one chapter per section
#     bericht_abbildungen.pdf        the same page printed to A4, for sharing
#     abbildungen/                   the figures as PNG and SVG, numbered as in
#                                    the report: abb_4_07_*.png/.svg is
#                                    Abbildung 4.7
#
# Nothing is plotted here. The figures are copied from SUMMARY_DIR, so they are
# exactly those of the summary statistics: run R/run_summary_statistics.R first,
# and this script again whenever the summary statistics change.
#
# TO ADD OR REMOVE A FIGURE, edit FIGURES in part 2 below. To rename, add or
# reorder a section, edit SECTIONS in part 1. Nothing else needs changing.
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source("R/prepare_analysis_data.R")            # also loads config.R
source("R/summary_statistics/00_settings.R")   # REPORT_BROWSERS, which print the PDF


# =============================================================================
# 1. the sections of the brief  <- EDIT HERE
# =============================================================================
# In the order they appear in the report. Left: the short name used in FIGURES.
# Right: the heading in the report. A section without figures is left out.
SECTIONS <- c(
  teilnahme   = "Teilnahme, Abbruch und Bearbeitungszeit",
  stichprobe  = "Wer hat teilgenommen? Regionen und Berufsgruppen",
  unternehmen = "Unternehmensstrukturen und Kundengruppen",
  hemmnisse   = "Hemmnisse und Entscheidungskriterien der Kunden",
  heizungen   = "Empfohlene und tatsächlich installierte Heizungen",
  preise      = "Erwartete Entwicklung der Energiepreise",
  vignetten   = "Vignetten: Welche Heizung empfehlen die Experten?"
)


# =============================================================================
# 2. the figures  <- EDIT HERE
# =============================================================================
# One line per figure: its section (a name from SECTIONS) and its path inside
# SUMMARY_DIR, which is always
#
#   <sample>/<group>/<folder>/<file>
#
#   sample  alle_befragten   every expert, including break-offs
#           completer        submitted questionnaires only
#   group   alle_berufsgruppen, shk_handwerk, schornsteinfeger, energieberater
#
# The easiest way to find a path: open bericht.html in SUMMARY_DIR/<sample>,
# where it is printed under every figure. ".png" at the end may be left out.
#
#   remove a figure   delete its line, or put a # in front of it
#   add a figure      copy a line and change the path
#   reorder           move lines; within a section, figures follow this order
FIGURES <- tribble(
  ~section,      ~figure,

  "teilnahme",   "alle_befragten/alle_berufsgruppen/01_stichprobe/fragebogen_abgeschlossen",
  "teilnahme",   "alle_befragten/alle_berufsgruppen/01_stichprobe/abbruch_nach_frage",
  "teilnahme",   "alle_befragten/alle_berufsgruppen/01_stichprobe/verbleib_nach_abschnitt",
  "teilnahme",   "completer/alle_berufsgruppen/02_bearbeitungszeit/bearbeitungszeit_abgeschlossen",
  "teilnahme",   "completer/alle_berufsgruppen/02_bearbeitungszeit/bearbeitungszeit_nach_abschnitt",

  "stichprobe",  "completer/alle_berufsgruppen/03_karten/experten_nach_bundesland",
  "stichprobe",  "completer/alle_berufsgruppen/03_karten/experten_nach_plz",
  "stichprobe",  "completer/alle_berufsgruppen/01_stichprobe/berufsgruppe",
  "stichprobe",  "completer/alle_berufsgruppen/04_unternehmen/Q2_unternehmenskategorie",

  "unternehmen", "completer/alle_berufsgruppen/04_unternehmen/Q3_firm_employees",
  "unternehmen", "completer/alle_berufsgruppen/04_unternehmen/Q0a_resp_is_owner",
  "unternehmen", "completer/alle_berufsgruppen/04_unternehmen/Q4_firm_revenue",
  "unternehmen", "completer/alle_berufsgruppen/04_unternehmen/Q6_firm_radius_recoded",
  "unternehmen", "completer/alle_berufsgruppen/04_unternehmen/Q6a_firm_dh_in_area",
  "unternehmen", "completer/alle_berufsgruppen/09_dienstleistungen_markt/Q7_kundengruppen",
  "unternehmen", "completer/alle_berufsgruppen/09_dienstleistungen_markt/Q7_kundengruppen_gewichte",

  "hemmnisse",   "completer/shk_handwerk/09_dienstleistungen_markt/Q19a_hemmnisse_erneuerbare",
  "hemmnisse",   "completer/schornsteinfeger/09_dienstleistungen_markt/Q19a_hemmnisse_erneuerbare",
  "hemmnisse",   "completer/energieberater/09_dienstleistungen_markt/Q19a_hemmnisse_erneuerbare",
  "hemmnisse",   "completer/shk_handwerk/09_dienstleistungen_markt/Q28_kriterien_der_kunden",
  "hemmnisse",   "completer/schornsteinfeger/09_dienstleistungen_markt/Q28_kriterien_der_kunden",
  "hemmnisse",   "completer/energieberater/09_dienstleistungen_markt/Q28_kriterien_der_kunden",
  "hemmnisse",   "completer/shk_handwerk/09_dienstleistungen_markt/Q28_kriterien_der_kunden_gewichte",
  "hemmnisse",   "completer/schornsteinfeger/09_dienstleistungen_markt/Q28_kriterien_der_kunden_gewichte",
  "hemmnisse",   "completer/energieberater/09_dienstleistungen_markt/Q28_kriterien_der_kunden_gewichte",

  "heizungen",   "completer/alle_berufsgruppen/09_dienstleistungen_markt/Q10a_adv_households",
  "heizungen",   "completer/alle_berufsgruppen/09_dienstleistungen_markt/Q10b_SQ001_adv_share_hp",
  "heizungen",   "completer/shk_handwerk/07_heiztechnologien/Q11b_WP_mix25_hp",

  "preise",      "completer/shk_handwerk/06_energiepreise/Q17_preisentwicklung",
  "preise",      "completer/schornsteinfeger/06_energiepreise/Q17_preisentwicklung",
  "preise",      "completer/energieberater/06_energiepreise/Q17_preisentwicklung",

  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_alter",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_einkommen",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_bestehende_heizung",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_ersatzzeitpunkt",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_baujahr",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_sanierungsstand",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_waermeverteilung",
  "vignetten",   "completer/alle_berufsgruppen/05_vignetten/empfehlungen_nach_berufsgruppe",
)


# =============================================================================
# from here on nothing needs editing
# =============================================================================

# --- 3. check the list before anything is written --------------------------------
unknown_sections <- setdiff(FIGURES$section, names(SECTIONS))
if (length(unknown_sections))
  stop("FIGURES uses sections that are not in SECTIONS: ",
       str_c(unknown_sections, collapse = ", "), call. = FALSE)

figures <- FIGURES %>%
  mutate(figure     = str_c(str_remove(figure, "\\.png$"), ".png"),
         source     = file.path(SUMMARY_DIR, figure),
         svg_source = str_replace(source, "\\.png$", ".svg"))

# A path that does not exist stops the run and names the closest file in the
# same folder, which usually makes the typo obvious.
missing <- figures %>% filter(!file.exists(source))
if (nrow(missing)) {
  closest <- map_chr(missing$source, function(path) {
    candidates <- list.files(dirname(path), "\\.png$")
    if (length(candidates) == 0) return("(no such folder)")
    str_c("(closest: ", candidates[which.min(adist(basename(path), candidates))], ")")
  })
  stop("These figures are not in ", SUMMARY_DIR, ":\n",
       str_c("  ", missing$figure, "  ", closest, collapse = "\n"),
       "\n\nCorrect the path in FIGURES, or run R/run_summary_statistics.R first.",
       call. = FALSE)
}

# The SVG of every figure is written next to its PNG by save_figure(). Summary
# statistics from before that was added have none: run them again.
missing_svg <- figures %>% filter(!file.exists(svg_source))
if (nrow(missing_svg))
  stop("These figures have no SVG in ", SUMMARY_DIR, ":\n",
       str_c("  ", str_replace(missing_svg$figure, "\\.png$", ".svg"), collapse = "\n"),
       "\n\nRun R/run_summary_statistics.R again, which writes every figure as PNG and SVG.",
       call. = FALSE)

# --- 4. number the figures --------------------------------------------------------
# Abbildung <section>.<figure>, counting only sections that have figures. The
# copied file starts with the same number and, unless it shows all experts, ends
# with its Berufsgruppe, so the three versions of a question can be told apart.
figures <- figures %>%
  mutate(section = factor(section, levels = names(SECTIONS)),
         order   = row_number()) %>%
  arrange(section, order) %>%
  mutate(section_no = as.integer(droplevels(section))) %>%
  mutate(figure_no = row_number(), .by = section) %>%
  mutate(number = str_c(section_no, ".", figure_no),
         group  = str_split_i(figure, "/", 2),
         file   = str_c("abb_", section_no, "_", sprintf("%02d", figure_no), "_",
                        str_remove(basename(figure), "\\.png$"),
                        if_else(group == "alle_berufsgruppen", "", str_c("_", group)),
                        ".png"),
         svg_file = str_replace(file, "\\.png$", ".svg"))

# --- 5. copy the figures ----------------------------------------------------------
# abbildungen/ is emptied first, so that a figure taken off the list cannot stay
# behind looking current. Only that folder: other output of the brief can live
# next to it in FIRST_POLICY_BRIEF_DIR.
figure_dir <- file.path(FIRST_POLICY_BRIEF_DIR, "abbildungen")
unlink(figure_dir, recursive = TRUE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

copied <- file.copy(figures$source, file.path(figure_dir, figures$file), overwrite = TRUE)
if (!all(copied))
  stop("Could not copy: ", str_c(figures$figure[!copied], collapse = ", "), call. = FALSE)

copied <- file.copy(figures$svg_source, file.path(figure_dir, figures$svg_file), overwrite = TRUE)
if (!all(copied))
  stop("Could not copy the SVG of: ", str_c(figures$figure[!copied], collapse = ", "), call. = FALSE)

# --- 6. the report page -----------------------------------------------------------
esc   <- htmltools::htmlEscape
stand <- format(max(file.mtime(figures$source)), "%Y-%m-%d")   # newest figure used

# How many experts, from the tables of the summary statistics.
stichprobe <- read_excel(file.path(SUMMARY_DIR, "alle_befragten", "tabellen.xlsx"), sheet = "stichprobe")
count_in   <- function(column, group) stichprobe[[column]][stichprobe$gruppe == group]

key_numbers <- tribble(
  ~value,                                         ~label,
  count_in("befragte",      ALL_GROUP),           "Befragte",
  count_in("abgeschlossen", ALL_GROUP),           "Abgeschlossen",
  count_in("abgeschlossen", "SHK-Handwerk"),      "davon SHK-Handwerk",
  count_in("abgeschlossen", "Schornsteinfeger"),  "davon Schornsteinfeger",
  count_in("abgeschlossen", "Energieberater"),    "davon Energieberater",
  nrow(figures),                                  "Abbildungen"
)

# The rules behind the figures, in the words of the summary statistics.
rules <- read_excel(file.path(SUMMARY_DIR, "completer", "tabellen.xlsx"), sheet = "hinweise") %>%
  filter(thema %in% c("Gruppen", "Stetige Variablen", "Rangfolgen", "Vignetten"))

notes <- bind_rows(
  tibble(thema   = "Abbildungen",
         hinweis = str_c("Ausgewählt aus output_dropbox/summary_statistics, Stand ", stand, ". ",
                         "Unter jeder Abbildung steht, wo sie dort liegt; ihre Fußzeile nennt ",
                         "Stichprobe und Gruppe. Als PNG und SVG im Ordner abbildungen/, benannt nach ihrer Nummer.")),
  rules,
  tibble(thema   = c("Tabellen", "Erstellt"),
         hinweis = c("Die Zahlen hinter den Abbildungen stehen in tabellen.xlsx in output_dropbox/summary_statistics/<Stichprobe>/.",
                     str_c(format(Sys.time(), "%Y-%m-%d %H:%M"), " mit R/first_policy_brief/01_figure_report.R")))
)

# One figure: its number, and its path in SUMMARY_DIR so it can be found there.
figures <- figures %>%
  mutate(html = str_c('<figure><img src="abbildungen/', file, '" alt="Abbildung ', number, '">',
                      '<figcaption><strong>Abbildung ', number, '</strong> · ', esc(figure),
                      '</figcaption></figure>'))

sections <- figures %>%
  summarise(html = str_c(html, collapse = "\n"), n = n(), .by = c(section_no, section)) %>%
  mutate(heading = str_c(section_no, " ", SECTIONS[as.character(section)]),
         anchor  = str_c("abschnitt-", section_no),
         count   = if_else(n == 1, "1 Abbildung", str_c(n, " Abbildungen")))

page <- str_c(
  '<!DOCTYPE html>\n<html lang="de">\n<head>\n<meta charset="utf-8">\n',
  '<title>Erster Policy Brief – Abbildungen</title>\n',
  '<style>\n', read_file("R/summary_statistics/report.css"),
  'figcaption strong { color: var(--ink-2); }\n',
  '</style>\n',
  '</head>\n<body>\n',

  '<header>\n',
  '<h1>Befragung von Energieexperten – Abbildungen für den ersten Policy Brief</h1>\n',
  '<p class="sample">Ausgewählte Abbildungen der deskriptiven Statistik, Stand ', stand, '</p>\n',
  '<div class="numbers">',
  str_c('<div><span class="value">', formatC(key_numbers$value, format = "d", big.mark = ".", decimal.mark = ","), '</span>',
        '<span class="label">', esc(key_numbers$label), '</span></div>', collapse = ""),
  '</div>\n',
  '<nav>', str_c('<a href="#', sections$anchor, '">', esc(sections$heading), '</a>', collapse = ""), '</nav>\n',
  '</header>\n',

  '<section class="notes">\n<h2>Hinweise</h2>\n<dl>',
  str_c('<dt>', esc(notes$thema), '</dt><dd>', esc(notes$hinweis), '</dd>', collapse = ""),
  '</dl>\n</section>\n',

  str_c('<section class="group" id="', sections$anchor, '">\n',
        '<h2>', esc(sections$heading), '<span class="n">', sections$count, '</span></h2>\n',
        '<div class="grid">\n', sections$html, '\n</div>\n</section>', collapse = "\n"),
  '\n</body>\n</html>\n'
)

html_path <- file.path(FIRST_POLICY_BRIEF_DIR, "bericht_abbildungen.html")
pdf_path  <- file.path(FIRST_POLICY_BRIEF_DIR, "bericht_abbildungen.pdf")
write_file(page, html_path)

# --- 7. print it to PDF -----------------------------------------------------------
# As in R/summary_statistics/08_report.R: a throwaway browser profile, so an open
# browser window cannot catch the job, and a timeout, so a hanging browser cannot
# stop the run. The old PDF goes first, so a failed print cannot leave it behind
# looking current.
unlink(pdf_path)
browser <- REPORT_BROWSERS[file.exists(REPORT_BROWSERS)][1]

if (is.na(browser)) {
  message("No Chrome or Edge found: bericht_abbildungen.html written, the PDF skipped. ",
          "Open the HTML and print it to PDF instead.")
} else {
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  html_url   <- str_c("file://", if (.Platform$OS.type == "windows") "/",
                      URLencode(normalizePath(html_path, winslash = "/")))
  system2(browser,
          c("--headless", "--disable-gpu", "--no-pdf-header-footer",
            str_c("--user-data-dir=", shQuote(tempfile("browser_"), type = quote_type)),
            str_c("--print-to-pdf=", shQuote(normalizePath(pdf_path, mustWork = FALSE), type = quote_type)),
            shQuote(html_url, type = quote_type)),
          stdout = FALSE, stderr = FALSE, timeout = 600)

  if (!file.exists(pdf_path)) warning("The browser did not write ", pdf_path, call. = FALSE)
}

message("First policy brief: ", nrow(figures), " figures -> ", normalizePath(FIRST_POLICY_BRIEF_DIR),
        if (file.exists(pdf_path)) str_c(" (PDF ", round(file.size(pdf_path) / 1024^2, 1), " MB)"))
