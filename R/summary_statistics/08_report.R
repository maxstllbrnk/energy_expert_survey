# =============================================================================
# 08_report.R  -  all figures of a sample on one page: bericht.html and .pdf
# =============================================================================
# Collects every figure the previous scripts wrote for the current sample into
# one page, so the results can be read without opening the folders:
#
#   <sample>/bericht.html   one chapter per group, figures by questionnaire section
#   <sample>/bericht.pdf    the same page printed to A4, for sharing
#
# bericht.html shows the PNG files next to it rather than embedding them, so it
# stays small but only works inside its folder. bericht.pdf is self-contained.
# It is printed by a headless Chrome or Edge (REPORT_BROWSERS in 00_settings.R);
# where neither is installed, open bericht.html and print it to PDF by hand.
#
# Uses dat, sample_spec and sample_dir (set by the runner) and `hinweise` (from
# 07_tables.R). The look of the page is in report.css.
# =============================================================================

# --- the figures of this sample -------------------------------------------------
# The scripts write figures in questionnaire order, so sorting by the time a
# file was written keeps that order within each section.
figures <- tibble(path = list.files(sample_dir, "\\.png$", recursive = TRUE)) %>%
  mutate(group   = str_split_i(path, "/", 1),
         section = str_split_i(path, "/", 2),
         written = file.mtime(file.path(sample_dir, path))) %>%
  arrange(factor(group, levels = folder_name(GROUPS)), section, written)

esc <- htmltools::htmlEscape

# One figure; the caption is the file's path, so it can be found in the folders.
figure_html <- function(path) {
  str_c('<figure><img src="', path, '" alt="', esc(basename(path)), '">',
        '<figcaption>', esc(path), '</figcaption></figure>')
}

# One group: a heading, links to its sections, then the figures of each section.
group_html <- function(label) {
  id <- folder_name(label)
  n_experts <- if (label == ALL_GROUP) nrow(dat$main) else sum(dat$main$berufsgruppe == label, na.rm = TRUE)

  sections <- figures %>%
    filter(group == id) %>%
    summarise(figures = str_c(map_chr(path, figure_html), collapse = "\n"), .by = section) %>%
    mutate(heading = unname(SECTION_LABELS[section]),
           anchor  = str_c(id, "-", section))

  str_c(
    '<section class="group" id="', id, '">\n',
    '<h2>', esc(label), '<span class="n">', fmt_n(n_experts), ' Experten</span></h2>\n',
    '<nav>', str_c('<a href="#', sections$anchor, '">', esc(sections$heading), '</a>', collapse = ""), '</nav>\n',
    str_c('<h3 id="', sections$anchor, '">', esc(sections$heading), '</h3>\n',
          '<div class="grid">\n', sections$figures, '\n</div>', collapse = "\n"),
    '\n</section>'
  )
}

# --- the page -----------------------------------------------------------------------
key_numbers <- tribble(
  ~value,                               ~label,
  nrow(dat$main),                       "Befragte",
  sum(dat$main$is_complete),            "Abgeschlossen",
  sum(!is.na(dat$vignettes$vig_rec)),   "Vignettenantworten",
  nrow(figures),                        "Abbildungen"
)
# In the completer sample every expert has completed, so that number says nothing.
if (sample_spec$completers_only) key_numbers <- key_numbers %>% filter(label != "Abgeschlossen")

page <- str_c(
  '<!DOCTYPE html>\n<html lang="de">\n<head>\n<meta charset="utf-8">\n',
  '<title>Energieexperten-Befragung – ', esc(sample_spec$label), '</title>\n',
  '<style>\n', read_file("R/summary_statistics/report.css"), '</style>\n',
  '</head>\n<body>\n',

  '<header>\n',
  '<h1>Befragung von Energieexperten – deskriptive Statistik</h1>\n',
  '<p class="sample">Stichprobe: <strong>', esc(sample_spec$label), '</strong></p>\n',
  '<div class="numbers">',
  str_c('<div><span class="value">', fmt_n(key_numbers$value), '</span>',
        '<span class="label">', key_numbers$label, '</span></div>', collapse = ""),
  '</div>\n',
  '<nav>', str_c('<a href="#', folder_name(GROUPS), '">', esc(GROUPS), '</a>', collapse = ""), '</nav>\n',
  '</header>\n',

  '<section class="notes">\n<h2>Hinweise</h2>\n<dl>',
  str_c('<dt>', esc(hinweise$thema), '</dt><dd>', esc(hinweise$hinweis), '</dd>', collapse = ""),
  '<dt>Tabellen</dt><dd>Die Zahlen hinter den Abbildungen stehen in ',
  '<a href="tabellen.xlsx">tabellen.xlsx</a> im selben Ordner.</dd>',
  '</dl>\n</section>\n',

  str_c(map_chr(GROUPS, group_html), collapse = "\n"),
  '\n</body>\n</html>\n'
)

html_path <- file.path(sample_dir, "bericht.html")
pdf_path  <- file.path(sample_dir, "bericht.pdf")
write_file(page, html_path)

# --- print it to PDF --------------------------------------------------------------
# A separate, throwaway browser profile, so an open browser window cannot catch
# the job; a timeout, so a browser that hangs cannot stop the run.
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

message("Report -> ", normalizePath(html_path),
        if (file.exists(pdf_path)) str_c(" and bericht.pdf (",
                                         round(file.size(pdf_path) / 1024^2, 1), " MB)"))
