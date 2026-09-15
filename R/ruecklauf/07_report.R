# =============================================================================
# 07_report.R  -  every figure of the response-rate analysis on one page
# =============================================================================
#   RUECKLAUF_DIR/bericht.html   all four combinations, one chapter each
#   RUECKLAUF_DIR/bericht.pdf    the same page printed to A4, for sharing
#
# One report for the whole analysis rather than one per folder: the point of
# the four folders is the comparison between them, and that only works if they
# are on the same page.
#
# bericht.html shows the PNG files next to it rather than embedding them, so it
# only works inside RUECKLAUF_DIR; bericht.pdf is self-contained. The look is
# R/summary_statistics/report.css, reused unchanged.
#
# Runs once, after the figures. Uses frame, resp_all and versand_gesamt.
# =============================================================================

figures <- tibble(path = list.files(RUECKLAUF_DIR, "\\.png$", recursive = TRUE)) %>%
  mutate(variante = str_split_i(path, "/", 1),
         probe    = str_split_i(path, "/", 2),
         section  = str_split_i(path, "/", 3),
         written  = file.mtime(file.path(RUECKLAUF_DIR, path))) %>%
  arrange(factor(variante, levels = TONLINE_VARIANTS$folder),
          factor(probe,    levels = RUECKLAUF_SAMPLES$folder),
          section, written)

esc <- htmltools::htmlEscape

figure_html <- function(path) {
  str_c('<figure><img src="', path, '" alt="', esc(basename(path)), '">',
        '<figcaption>', esc(path), '</figcaption></figure>')
}

# One chapter: one t-online variant x one response definition.
chapter_html <- function(variante, probe) {
  v <- TONLINE_VARIANTS  %>% filter(folder == variante)
  s <- RUECKLAUF_SAMPLES %>% filter(folder == probe)
  id <- str_c(variante, "-", probe)

  sections <- figures %>%
    filter(variante == !!variante, probe == !!probe) %>%
    summarise(figures = str_c(map_chr(path, figure_html), collapse = "\n"), .by = section) %>%
    mutate(heading = unname(RUECKLAUF_SECTIONS[section]),
           anchor  = str_c(id, "-", section))

  if (nrow(sections) == 0) return("")

  str_c(
    '<section class="group" id="', id, '">\n',
    '<h2>', esc(v$label), ' · ', esc(s$label), '</h2>\n',
    '<nav>', str_c('<a href="#', sections$anchor, '">', esc(sections$heading), '</a>',
                   collapse = ""), '</nav>\n',
    str_c('<h3 id="', sections$anchor, '">', esc(sections$heading), '</h3>\n',
          '<div class="grid">\n', sections$figures, '\n</div>', collapse = "\n"),
    '\n</section>'
  )
}

kapitel <- expand_grid(variante = TONLINE_VARIANTS$folder,
                       probe    = RUECKLAUF_SAMPLES$folder)

key_numbers <- tribble(
  ~value,                      ~label,
  nrow(frame),                 "Kontaktiert",
  nrow(resp_all),              "Geantwortet",
  sum(resp_all$is_complete),   "Abgeschlossen",
  nrow(figures),               "Abbildungen"
)

gesamtquote <- percent_rate(nrow(resp_all) / nrow(frame))

page <- str_c(
  '<!DOCTYPE html>\n<html lang="de">\n<head>\n<meta charset="utf-8">\n',
  '<title>Energieexperten-Befragung – Rücklaufquote</title>\n',
  '<style>\n', read_file("R/summary_statistics/report.css"), '</style>\n',
  '</head>\n<body>\n',

  '<header>\n',
  '<h1>Befragung von Energieexperten – Rücklaufquote</h1>\n',
  '<p class="sample">Rücklauf insgesamt: <strong>', gesamtquote, '</strong> ',
  'der kontaktierten Personen</p>\n',
  '<div class="numbers">',
  str_c('<div><span class="value">', fmt_n(key_numbers$value), '</span>',
        '<span class="label">', key_numbers$label, '</span></div>', collapse = ""),
  '</div>\n',
  '<nav>',
  str_c('<a href="#', kapitel$variante, '-', kapitel$probe, '">',
        esc(TONLINE_VARIANTS$label[match(kapitel$variante, TONLINE_VARIANTS$folder)]), ' · ',
        esc(RUECKLAUF_SAMPLES$label[match(kapitel$probe, RUECKLAUF_SAMPLES$folder)]), '</a>',
        collapse = ""),
  '</nav>\n',
  '</header>\n',

  '<section class="notes">\n<h2>Hinweise</h2>\n<dl>',
  str_c('<dt>', esc(hinweise$thema), '</dt><dd>', esc(hinweise$hinweis), '</dd>', collapse = ""),
  '<dt>Tabellen</dt><dd>Die Zahlen hinter den Abbildungen stehen in ',
  '<a href="tabellen_ruecklauf.xlsx">tabellen_ruecklauf.xlsx</a> im selben Ordner.</dd>',
  '</dl>\n</section>\n',

  str_c(map2_chr(kapitel$variante, kapitel$probe, chapter_html), collapse = "\n"),
  '\n</body>\n</html>\n'
)

html_path <- file.path(RUECKLAUF_DIR, "bericht.html")
pdf_path  <- file.path(RUECKLAUF_DIR, "bericht.pdf")
write_file(page, html_path)

# --- print it to PDF ---------------------------------------------------------
# Same approach as R/summary_statistics/08_report.R: a throwaway browser
# profile so an open window cannot catch the job, and a timeout so a hung
# browser cannot stop the run.
browser <- REPORT_BROWSERS[file.exists(REPORT_BROWSERS)][1]

if (is.na(browser)) {
  message("No Chrome or Edge found: bericht.html written, bericht.pdf skipped. ",
          "Open bericht.html and print it to PDF instead.")
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

message("Bericht -> ", normalizePath(html_path),
        if (file.exists(pdf_path)) str_c(" und bericht.pdf (",
                                         round(file.size(pdf_path) / 1024^2, 1), " MB)"))
