# =============================================================================
# build_report.R  -  One browsable HTML page with every summary output
# -----------------------------------------------------------------------------
# Collects everything summarize_data.R and map_experts.R produce - the maps, all
# variable plots, and the summary table - into a single self-contained HTML file
# that can be opened in a browser or shared with coauthors.
#
# Every image is embedded in the file itself (base64), so the page works with no
# other files alongside it: one attachment, nothing to break.
#
# HOW TO RUN (after run_pipeline.R, summarize_data.R and map_experts.R):
#   - In RStudio: open this file and click "Source"; or
#   - In a terminal, from the project folder:  Rscript build_report.R
#
# Writes output/summary_report.html
# =============================================================================

# --- 0. Always run from the project folder -----------------------------------
# Locate this script, then walk up to the project root (the folder containing
# the marker file/dir listed in PROJECT_MARKERS). Works under Rscript, source(),
# the RStudio "Source" button, and Ctrl+Enter in the editor.

PROJECT_MARKERS <- c(".Rproj.user", "data")  # adjust: any file/dir unique to the root

find_this_script <- function() {
  # (a) Rscript / R CMD BATCH: read the --file= argument. Rscript encodes
  #     spaces in the path as "~+~", so decode them.
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) >= 1L) {
    return(gsub("~+~", " ", sub("^--file=", "", file_arg[[1L]]), fixed = TRUE))
  }
  # (b) source() / RStudio "Source" button: the file name is recorded in the
  #     call stack. Search inside out so nested source() calls resolve to the
  #     file actually running.
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frames()[[i]]$ofile
    if (!is.null(of) && is.character(of) && nzchar(of)) return(of)
  }
  # (c) open in the RStudio editor and run another way (e.g. Ctrl+Enter).
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getSourceEditorContext()
    if (!is.null(ctx) && nzchar(ctx$path)) return(ctx$path)
  }
  ""  # could not detect
}

find_project_root <- function(start_dir, markers = PROJECT_MARKERS) {
  d <- start_dir
  repeat {
    if (any(file.exists(file.path(d, markers)))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) return(NA_character_)  # reached filesystem root
    d <- parent
  }
}

.script_path <- find_this_script()
if (nzchar(.script_path) && file.exists(.script_path)) {
  .script_dir  <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
  .project_dir <- find_project_root(.script_dir)
  if (is.na(.project_dir)) {
    stop("Found this script at ", .script_dir, ", but no project root above it ",
         "(looked for: ", paste(PROJECT_MARKERS, collapse = ", "), ").")
  }
  setwd(.project_dir)
  message("Working directory: ", getwd())
} else {
  message(
    "NOTE: could not detect this script's location automatically.\n",
    "      Please set your working directory to the project folder before running."
  )
}

source("r-scripts/pipeline/config.R", encoding = "UTF-8")
library(base64enc)

# Optional sample restriction, set by a wrapper before sourcing this file (see
# build_report_completers.R). Run on its own, this script behaves as before.
if (!exists("COMPLETERS_ONLY")) COMPLETERS_ONLY <- FALSE
if (!exists("OUTPUT_TAG"))      OUTPUT_TAG      <- ""

PLOT_DIR   <- file.path("output", paste0("summary", OUTPUT_TAG))
MAP_DIR    <- file.path("output", paste0("maps", OUTPUT_TAG))
REPORT_OUT <- file.path("output", paste0("summary_report", OUTPUT_TAG, ".html"))

summary_csv <- file.path("output",
                         paste0("variable_summary", OUTPUT_TAG, ".csv"))
if (!file.exists(summary_csv)) {
  stop("Could not find '", summary_csv, "'. Run summarize_data.R first.", call. = FALSE)
}
vs_all <- utils::read.csv(summary_csv, stringsAsFactors = FALSE, encoding = "UTF-8")

# --- What the report leaves out --------------------------------------------------
# The report is the curated view; variable_summary.csv and output/summary/ keep
# everything, so nothing here is lost - it is just not shown.
#
# Three groups are dropped, all of them measurements of how the survey ran rather
# than answers to it:
#
#  1. Response times. Every "<question>Time..Fragenzeit.." column, the overall
#     interview time, and the two vignette timings. Useful for the pre-registered
#     speed robustness check, not for reading distributions of answers.
#  2. The 60 individual risk/time staircase items (Time1, Time2A, ... Risk5BBBB).
#     No respondent sees more than five of them, so each one is mostly empty and
#     tells you about the branch of the staircase rather than about preferences.
#     They are represented by the two derived SCORES instead, which is what the
#     staircase exists to produce.
#  3. Pipeline bookkeeping - columns recording how a value was processed
#     (how many staircase levels someone completed, whether an answer came from
#     the free-text recode, whether a postcode parsed).
#
# The patterns are deliberately anchored rather than loose keyword matches: the
# vignette attribute `attr_5_ersatzzeitpunkt` contains "zeit" and a careless
# pattern would silently drop a substantive variable.
EXCLUDE_PATTERNS <- c(
  "Time\\.\\.Fragenzeit",            # per-question response time
  "^interviewtime",                  # total interview duration
  "^vignette_question_time$",
  "^investment_cost_time$",
  "^(Time|Risk)[0-9]",               # the individual staircase items
  "_levels_completed$",              # how far through the staircase someone got
  "_recode_source$",                 # how a Q6 / Q22 answer was recovered
  # "^plz$", "^plz_valid$",            # postcode parsing (the maps use these)
  "^investment_cost_source$",        # which cost question the answer came from
  # LimeSurvey's internal scores for the ranking widget behind Q11b: -1 for
  # 2,084 of the 2,426 respondents who reached it, and an arbitrary 5-digit
  # number for the rest. Not an answer to anything. (They survive the pipeline's
  # empty/constant check only because those few non--1 values exist.)
  "^Q11bScore",
  # The vignette design itself rather than anything a respondent said: the nine
  # attributes describing the scenario shown, and which Fernwaerme branch it was
  # shown in. Their distributions describe the randomisation, not the experts.
  "^attr_[0-9]", "^FW$",
  # LimeSurvey's progress counter, not a question.
  "^lastpage$",
  # Q11bTop1/Q11bTop2 are the highest and second-highest of the Q11b_* shares -
  # the same answers already plotted, just re-expressed as a ranking.
  "^Q11bTop[12]$",
  # Postcode: the two maps show this properly. A histogram of postcode NUMBERS
  # is meaningless anyway - the digits encode region, not quantity.
  "^Q0$",
  # Superseded by the "_recoded" columns or dropped as redundant.
  "^Q6_recoded$", "^Q22_recoded$", "^Q22_other$",
  # Structural columns describing the data layout rather than the respondent.
  "^target_group$", "^vignette_position$", "^vignette_id$",
  # Individual options / ranks now shown as one composite chart per question
  # (Q2_summary, Q8_summary, Q19a_summary, ..., Q28_rank_points, Q7_rank_points).
  "^(Q2|Q8|Q19a|Q19b|Q19c|Q24)_SQ[0-9]+$",
  "^Q28_[0-9]+$", "^Q7_[0-9]+$"
)

excluded <- Reduce(`|`, lapply(EXCLUDE_PATTERNS, function(p) grepl(p, vs_all$variable_name)))
vs <- vs_all[!excluded, ]

n_dropped <- sum(excluded)
message("Report shows ", nrow(vs), " of ", nrow(vs_all),
        " variables (", n_dropped, " timing/derived left out).")

d <- readRDS(file.path(OUTPUT_DIR, "cleaned_survey_data.rds"))
if (COMPLETERS_ONLY) d <- d[!is.na(d$submitdate), , drop = FALSE]
n_respondents <- length(unique(d$unique_id))
n_rows        <- nrow(d)
n_batches     <- length(unique(d$survey_id))

# --- helpers -------------------------------------------------------------------
esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

embed_png <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  paste0("data:image/png;base64,", base64enc::base64encode(path))
}

# Severity of missingness, used for the chip on each card. Three bands rather
# than a continuous scale: the reader is deciding "is this variable usable",
# which is a categorical judgement, not a reading of an exact percentage.
miss_band <- function(pct) {
  ifelse(pct < 5, "ok", ifelse(pct < 25, "some", "high"))
}

fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")

# --- maps ----------------------------------------------------------------------
map_files <- list(
  list(file = file.path(MAP_DIR, "experts_by_plz_points.png"),
       title = "By postcode area",
       note  = "One dot per postcode; dot area is proportional to the number of experts."),
  list(file = file.path(MAP_DIR, "experts_by_bundesland.png"),
       title = "By federal state",
       note  = "Shaded by how many experts are registered in each state.")
)
maps_html <- ""
for (m in map_files) {
  uri <- embed_png(m$file)
  if (is.na(uri)) next
  maps_html <- paste0(maps_html,
    '<figure class="map-card">',
    '<img src="', uri, '" alt="', esc(m$title), '" loading="lazy">',
    '<figcaption><span class="map-title">', esc(m$title), '</span>',
    '<span class="map-note">', esc(m$note), '</span></figcaption>',
    '</figure>')
}
if (identical(maps_html, "")) {
  maps_html <- '<p class="empty-note">No maps found. Run map_experts.R to create them.</p>'
}

# --- variable cards -------------------------------------------------------------
plotted <- vs[vs$type %in% c("continuous", "categorical", "composite"), ]
# Composites first - each one stands for a whole question that would otherwise
# be a dozen separate charts, so they are the most informative thing in the grid.
plotted <- plotted[order(plotted$type != "composite", plotted$variable_name), ]

# The two preference scores get their own section rather than sitting in the
# alphabetical grid: they are the whole point of the 60 staircase items that the
# report leaves out, so showing them on their own makes that substitution visible
# instead of burying it between two unrelated variables.
PREF_VARS <- c("time_preference_score", "risk_preference_score")
pref     <- plotted[plotted$variable_name %in% PREF_VARS, ]
plotted  <- plotted[!(plotted$variable_name %in% PREF_VARS), ]

PREF_NOTE <- list(
  time_preference_score = paste0(
    "Patience. Built from the five-step intertemporal staircase (Time1 &hellip; ",
    "Time5), following the Global Preference Survey. Higher = more willing to ",
    "wait for a larger later payment."),
  risk_preference_score = paste0(
    "Risk tolerance. Built from the five-step lottery staircase (Risk1 &hellip; ",
    "Risk5), following the Global Preference Survey. Higher = more willing to ",
    "take the gamble over a sure payment.")
)

pref_html <- ""
for (v in PREF_VARS) {
  r <- pref[pref$variable_name == v, ]
  if (nrow(r) == 0) next
  uri <- embed_png(file.path(PLOT_DIR, paste0(v, ".png")))
  if (is.na(uri)) next
  pref_html <- paste0(pref_html,
    '<figure class="map-card">',
    '<img src="', uri, '" alt="Distribution of ', esc(v), '" loading="lazy">',
    '<figcaption>',
    '<span class="map-title"><code class="var">', esc(v), '</code></span>',
    '<span class="map-note">', PREF_NOTE[[v]], '</span>',
    '<span class="map-note">', fmt_int(r$n - r$n_missing), ' of ', fmt_int(r$n),
    ' experts scored &middot; mean ', r$mean, ' &middot; sd ', r$sd, '</span>',
    '</figcaption></figure>')
}

# Cards run composites first, then alphabetically. A few variables read better
# beside a related one than in their alphabetical slot, so they are pinned:
#   Q0a ("Sind Sie Eigentümer/Geschäftsführer?") and Q0b ("Was ist Ihre Rolle?")
#   are two halves of one question about the respondent's position in the firm -
#   Q0b is only asked of those who answer "Nein" to Q0a - so they belong
#   together rather than in different blocks of the grid.
PIN_BEFORE <- c(Q0a = "Q0b_top")

for (v in names(PIN_BEFORE)) {
  i <- match(v, plotted$variable_name)
  if (is.na(i)) next
  moved   <- plotted[i, , drop = FALSE]
  plotted <- plotted[-i, , drop = FALSE]
  # Look the target up AFTER the removal - taking the index out shifts
  # everything below it.
  j <- match(PIN_BEFORE[[v]], plotted$variable_name)
  if (is.na(j)) { plotted <- rbind(plotted, moved); next }
  plotted <- rbind(
    plotted[seq_len(j - 1L), , drop = FALSE],
    moved,
    plotted[seq.int(j, nrow(plotted)), , drop = FALSE]
  )
}

cards <- character(0)
missing_plot <- character(0)
for (i in seq_len(nrow(plotted))) {
  row <- plotted[i, ]
  uri <- embed_png(file.path(PLOT_DIR, paste0(row$variable_name, ".png")))
  if (is.na(uri)) { missing_plot <- c(missing_plot, row$variable_name); next }

  label <- if (nzchar(row$question_text) && row$question_text != row$variable_name) {
    row$question_text
  } else {
    "(no question text - timing or derived variable)"
  }

  # A composite covers a whole question rather than one column, so it has no
  # single missingness figure; its own subtitle states who was asked. It gets a
  # type chip instead of a missingness chip.
  chip <- if (row$type == "composite") {
    '<span class="chip chip-composite">combined</span>'
  } else {
    paste0('<span class="chip chip-', miss_band(row$pct_missing), '">',
           row$pct_missing, '% missing</span>')
  }

  stat <- if (row$type == "composite") {
    paste0(row$n_unique, " options &middot; highest: ", esc(row$top_category),
           " (", row$top_category_pct, ")")
  } else if (row$type == "continuous") {
    paste0("mean ", row$mean, " &middot; sd ", row$sd, " &middot; median ", row$median,
           " &middot; range ", row$min, "&ndash;", row$max)
  } else {
    paste0(row$n_unique, " categories &middot; most common: ", esc(row$top_category),
           " (", row$top_category_pct, "% of those who answered)")
  }

  # Where the plot's axis was trimmed, say so on the card as well as on the
  # image, so it is visible when skimming the grid without opening the plot.
  trimmed <- ""
  n_omit_col <- if ("n_omitted_from_plot" %in% names(row)) row$n_omitted_from_plot else 0
  if (!is.na(n_omit_col) && n_omit_col > 0) {
    trimmed <- paste0('<span class="trim">Axis trimmed to ', esc(row$plot_range),
                      ' &mdash; ', fmt_int(n_omit_col), ' value(s) outside it not shown</span>')
  }

  cards <- c(cards, paste0(
    '<article class="card" data-type="', row$type, '" ',
    'data-search="', esc(tolower(paste(row$variable_name, label))), '">',
      '<header class="card-head">',
        '<code class="var">', esc(row$variable_name), '</code>',
        chip,
      '</header>',
      '<p class="qtext" title="', esc(label), '">', esc(label), '</p>',
      '<img src="', uri, '" alt="Distribution of ', esc(row$variable_name), '" loading="lazy">',
      '<footer class="card-foot">',
        '<span class="lvl">', esc(row$level), ' level</span>',
        '<span class="stat">', stat, '</span>',
        trimmed,
      '</footer>',
    '</article>'))
}

# --- variables with no plot ------------------------------------------------------
no_plot <- vs[!(vs$type %in% c("continuous", "categorical", "composite")), ]
rows_html <- ""
if (nrow(no_plot) > 0) {
  no_plot <- no_plot[order(no_plot$variable_name), ]
  for (i in seq_len(nrow(no_plot))) {
    r <- no_plot[i, ]
    why <- if (r$type == "skipped_high_cardinality") {
      paste0("Too many distinct values to plot (", fmt_int(r$n_unique), ")")
    } else if (r$type == "empty") {
      "No values at all"
    } else {
      r$type
    }
    rows_html <- paste0(rows_html,
      "<tr><td><code>", esc(r$variable_name), "</code></td><td>", esc(r$question_text),
      "</td><td class=\"num\">", fmt_int(r$n), "</td><td class=\"num\">", r$pct_missing,
      "%</td><td>", esc(why), "</td></tr>")
  }
}

n_cont <- sum(vs$type == "continuous")
n_cat  <- sum(vs$type == "categorical")
n_skip <- nrow(no_plot)

# --- page ------------------------------------------------------------------------
# Colour is defined once as tokens on :root, then redefined for dark under BOTH
# the media query (viewers on the default "system" setting) and the [data-theme]
# stamp (viewers who picked a theme), so the page resolves correctly in all
# three states rather than only when the viewer has made an explicit choice.
html <- c(
# The title names the artifact in the browser tab and in the gallery, so the two
# versions of this report must not share one - otherwise they are
# indistinguishable in a list.
if (COMPLETERS_ONLY) '<title>HEAT Survey Completers</title>'
else                 '<title>HEAT Survey Explorer</title>',
'<style>',
':root{',
'  --ground:#f5f7f9; --surface:#ffffff; --surface-2:#eef2f6;',
'  --ink:#131922; --ink-2:#44515f; --ink-3:#6b7a8a;',
'  --line:#dde3ea; --accent:#1c5f8b; --accent-soft:#e5eef5; --on-accent:#ffffff;',
'  --ok:#3f6b4a; --ok-bg:#e6efe8;',
'  --some:#8a6410; --some-bg:#f6eeda;',
'  --high:#9a3b2c; --high-bg:#f7e6e2;',
'  --shadow:0 1px 2px rgba(19,25,34,.05),0 4px 14px rgba(19,25,34,.05);',
'}',
'@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){',
'  --ground:#0e1216; --surface:#171c22; --surface-2:#1e242b;',
'  --ink:#e9eef4; --ink-2:#aab6c3; --ink-3:#7d8b99;',
'  --line:#262e37; --accent:#6fb0dc; --accent-soft:#1a2b38; --on-accent:#0e1216;',
'  --ok:#8fc39c; --ok-bg:#1a2820;',
'  --some:#d8ad5c; --some-bg:#2c2415;',
'  --high:#e0907f; --high-bg:#301d19;',
'  --shadow:0 1px 2px rgba(0,0,0,.3),0 4px 14px rgba(0,0,0,.28);',
'}}',
':root[data-theme="dark"]{',
'  --ground:#0e1216; --surface:#171c22; --surface-2:#1e242b;',
'  --ink:#e9eef4; --ink-2:#aab6c3; --ink-3:#7d8b99;',
'  --line:#262e37; --accent:#6fb0dc; --accent-soft:#1a2b38; --on-accent:#0e1216;',
'  --ok:#8fc39c; --ok-bg:#1a2820;',
'  --some:#d8ad5c; --some-bg:#2c2415;',
'  --high:#e0907f; --high-bg:#301d19;',
'  --shadow:0 1px 2px rgba(0,0,0,.3),0 4px 14px rgba(0,0,0,.28);',
'}',
'*{box-sizing:border-box;}',
'body{margin:0;background:var(--ground);color:var(--ink);',
'  font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;',
'  font-size:15px;line-height:1.55;-webkit-font-smoothing:antialiased;}',
'.wrap{max-width:1280px;margin:0 auto;padding:0 24px 72px;}',
'header.top{padding:44px 0 26px;border-bottom:1px solid var(--line);',
'  display:flex;flex-wrap:wrap;gap:28px;align-items:flex-end;justify-content:space-between;}',
'h1{margin:0 0 6px;font-size:31px;line-height:1.15;letter-spacing:-.022em;font-weight:640;',
'  text-wrap:balance;}',
'.sub{margin:0;color:var(--ink-2);max-width:62ch;}',
'.scope{display:inline-block;vertical-align:middle;margin-left:8px;padding:4px 10px;',
'  border-radius:20px;background:var(--accent-soft);color:var(--accent);',
'  font-size:12px;font-weight:640;letter-spacing:.01em;}',
'.stats{display:flex;gap:30px;flex-wrap:wrap;}',
'.stat-b{display:flex;flex-direction:column;gap:1px;}',
'.stat-n{font-size:25px;font-weight:640;letter-spacing:-.02em;font-variant-numeric:tabular-nums;}',
'.stat-l{font-size:11px;text-transform:uppercase;letter-spacing:.085em;color:var(--ink-3);}',
'section{margin-top:44px;}',
'h2{margin:0 0 4px;font-size:19px;font-weight:620;letter-spacing:-.012em;}',
'.lede{margin:0 0 20px;color:var(--ink-2);max-width:70ch;font-size:14px;}',
'.maps{display:grid;grid-template-columns:repeat(auto-fit,minmax(330px,1fr));gap:20px;}',
'.map-card{margin:0;background:var(--surface);border:1px solid var(--line);border-radius:10px;',
'  overflow:hidden;box-shadow:var(--shadow);}',
'.map-card img{display:block;width:100%;height:auto;}',
'.map-card figcaption{padding:13px 16px 15px;border-top:1px solid var(--line);',
'  display:flex;flex-direction:column;gap:3px;}',
'.map-title{font-weight:600;font-size:14px;}',
'.map-note{color:var(--ink-3);font-size:12.5px;}',
'.controls{position:sticky;top:0;z-index:5;background:var(--ground);padding:14px 0 13px;',
'  border-bottom:1px solid var(--line);display:flex;gap:12px;flex-wrap:wrap;align-items:center;}',
'#q{flex:1 1 250px;min-width:190px;padding:9px 12px;border:1px solid var(--line);border-radius:7px;',
'  background:var(--surface);color:var(--ink);font:inherit;font-size:14px;}',
'#q:focus{outline:2px solid var(--accent);outline-offset:1px;border-color:transparent;}',
'.filters{display:flex;gap:7px;flex-wrap:wrap;}',
'.f{padding:8px 13px;border:1px solid var(--line);border-radius:7px;background:var(--surface);',
'  color:var(--ink-2);font:inherit;font-size:13px;cursor:pointer;}',
'.f:hover{border-color:var(--accent);color:var(--ink);}',
'.f:focus-visible{outline:2px solid var(--accent);outline-offset:1px;}',
'.f[aria-pressed="true"]{background:var(--accent);border-color:var(--accent);',
'  color:var(--on-accent);font-weight:560;}',
'#count{color:var(--ink-3);font-size:13px;font-variant-numeric:tabular-nums;margin-left:auto;}',
'.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(348px,1fr));gap:18px;',
'  align-items:start;margin-top:20px;}',
'.card{background:var(--surface);border:1px solid var(--line);border-radius:10px;padding:14px 15px 12px;',
'  display:flex;flex-direction:column;gap:9px;box-shadow:var(--shadow);}',
'.card-head{display:flex;align-items:center;gap:9px;justify-content:space-between;}',
'.var{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:12.5px;',
'  background:var(--surface-2);color:var(--ink);padding:3px 7px;border-radius:5px;',
'  overflow-wrap:anywhere;}',
'.chip{font-size:11px;font-weight:600;padding:3px 8px;border-radius:20px;white-space:nowrap;',
'  font-variant-numeric:tabular-nums;}',
'.chip-ok{background:var(--ok-bg);color:var(--ok);}',
'.chip-some{background:var(--some-bg);color:var(--some);}',
'.chip-high{background:var(--high-bg);color:var(--high);}',
'.chip-composite{background:var(--accent-soft);color:var(--accent);}',
'.qtext{margin:0;font-size:13px;color:var(--ink-2);display:-webkit-box;-webkit-line-clamp:3;',
'  -webkit-box-orient:vertical;overflow:hidden;}',
'.card img{display:block;width:100%;height:auto;border-radius:6px;background:#fff;}',
'.card-foot{display:flex;flex-direction:column;gap:2px;font-size:11.5px;color:var(--ink-3);',
'  border-top:1px solid var(--line);padding-top:9px;}',
'.lvl{text-transform:uppercase;letter-spacing:.06em;font-size:10.5px;}',
'.trim{color:var(--some);font-weight:560;}',
'.tablewrap{overflow-x:auto;border:1px solid var(--line);border-radius:10px;background:var(--surface);}',
'table{border-collapse:collapse;width:100%;font-size:13px;min-width:640px;}',
'th,td{text-align:left;padding:10px 14px;border-bottom:1px solid var(--line);vertical-align:top;}',
'th{font-size:11px;text-transform:uppercase;letter-spacing:.075em;color:var(--ink-3);',
'  font-weight:600;background:var(--surface-2);}',
'tr:last-child td{border-bottom:none;}',
'td.num{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap;}',
'td code{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:12px;}',
'.empty-note{color:var(--ink-3);}',
'.no-results{padding:40px 0;color:var(--ink-3);text-align:center;display:none;}',
'footer.foot{margin-top:52px;padding-top:20px;border-top:1px solid var(--line);',
'  color:var(--ink-3);font-size:12.5px;}',
'@media (prefers-reduced-motion:reduce){*{animation:none!important;transition:none!important;}}',

# Printing / "Save as PDF". The artifact sandbox blocks any download the page
# starts itself, so the browser print dialog is how this becomes a PDF - which
# makes print a real output format for this page, not an afterthought. Whatever
# is filtered out on screen is filtered out of the PDF too, so searching first
# is how you print a subset rather than all 273 plots.
'@media print{',
# Force the light palette on paper whatever theme the screen was in. These
# selectors have to match the specificity of the dark-theme blocks above and
# come after them in source order, or a dark-themed screen prints dark.
'  :root,:root[data-theme="dark"],:root[data-theme="light"],',
'  :root:not([data-theme="light"]){',
'    --ground:#ffffff; --surface:#ffffff; --surface-2:#f2f5f8;',
'    --ink:#131922; --ink-2:#44515f; --ink-3:#6b7a8a;',
'    --line:#c9d2db; --accent:#1c5f8b; --on-accent:#ffffff;',
'    --ok:#3f6b4a; --ok-bg:#e6efe8;',
'    --some:#8a6410; --some-bg:#f6eeda;',
'    --high:#9a3b2c; --high-bg:#f7e6e2;',
'    --shadow:none;',
'  }',
'  @page{margin:14mm;}',
'  body{font-size:11px;}',
'  .wrap{max-width:none;padding:0;}',
# The search box and filter buttons cannot be operated on paper.
'  .controls,.no-results{display:none!important;}',
'  header.top{padding-top:0;}',
'  h1{font-size:22px;}',
'  section{margin-top:22px;}',
# Keep each plot and its caption on one page.
'  .card,.map-card,tr,figure{break-inside:avoid;page-break-inside:avoid;}',
'  h2{break-after:avoid;page-break-after:avoid;}',
'  .grid{grid-template-columns:repeat(2,1fr);gap:10px;}',
'  .maps{grid-template-columns:repeat(2,1fr);}',
'  .card{border:1px solid var(--line);padding:9px 10px 8px;}',
'  .card img,.map-card img{break-inside:avoid;}',
'  .tablewrap{overflow:visible;border:none;}',
'  table{min-width:0;}',
'  thead{display:table-header-group;}',
# Chips and the map fills carry meaning, so keep their colour on paper.
'  *{-webkit-print-color-adjust:exact;print-color-adjust:exact;}',
'}',
'</style>',

'<div class="wrap">',
'<header class="top">',
'<div>',
'<h1>Energy expert survey &mdash; wave 1',
if (COMPLETERS_ONLY) ' <span class="scope">completed questionnaires only</span>' else '',
'</h1>',
'<p class="sub">Every variable in the cleaned data set, summarised and plotted, plus where the ',
'surveyed experts are based. Search or filter to find a variable.',
# Which respondents this page covers has to be on the page itself: the two
# reports are otherwise identical to look at, and a figure read from the wrong
# one would be wrong in a way nothing on screen reveals.
if (COMPLETERS_ONLY)
  paste0(' <strong>This report covers only the ', fmt_int(n_respondents),
         ' experts who reached the end of the questionnaire and submitted it</strong>',
         ' — everyone who broke off part-way is excluded, so missingness that came',
         ' from drop-out is gone while missingness built into the questionnaire',
         ' (filter questions, "leave blank if not applicable") remains.')
else
  ' It covers every expert in the data, including those who broke off part-way.',
'</p>',
'</div>',
'<div class="stats">',
'<div class="stat-b"><span class="stat-n">', fmt_int(n_respondents), '</span><span class="stat-l">Experts</span></div>',
'<div class="stat-b"><span class="stat-n">', fmt_int(n_rows), '</span><span class="stat-l">Vignette rows</span></div>',
'<div class="stat-b"><span class="stat-n">', fmt_int(nrow(vs)), '</span><span class="stat-l">Variables</span></div>',
'<div class="stat-b"><span class="stat-n">', n_batches, '</span><span class="stat-l">Batches</span></div>',
'</div>',
'</header>',

'<section>',
'<h2>Where the experts are</h2>',
'<p class="lede">Based on the postcode of each expert&rsquo;s registered office. ',
# fmt_int(sum(!is.na(d[!duplicated(d$unique_id), "plz"]))), ' of ', fmt_int(n_respondents),
' experts gave a usable postcode.</p>',
'<div class="maps">', maps_html, '</div>',
'</section>',

if (nzchar(pref_html)) paste0(
'<section>',
'<h2>Risk and time preferences</h2>',
'<p class="lede">Each respondent worked through a five-step staircase of paired choices. The ',
'individual steps are not shown &mdash; nobody answers more than five of the sixty, so each one on ',
'its own describes a branch of the staircase rather than a preference. These two scores are what ',
'the staircase exists to produce, z-standardised across respondents.</p>',
'<div class="maps">', pref_html, '</div>',
'</section>') else "",

'<section>',
'<h2>Survey answers</h2>',
'<p class="lede">', fmt_int(length(cards)), ' variables are plotted: histograms for continuous ',
'measures, bar charts for categorical ones. Counts are shown at the level the variable actually ',
'varies &mdash; per expert, or per vignette observation. Response times and pipeline bookkeeping ',
'are left out; they are still in <code class="var">variable_summary.csv</code>.</p>',
'<div class="controls">',
'<input id="q" type="search" placeholder="Search variable name or question text&hellip;" aria-label="Search variables">',
'<div class="filters">',
'<button class="f" data-f="all" aria-pressed="true">All</button>',
'<button class="f" data-f="composite" aria-pressed="false">Combined questions</button>',
'<button class="f" data-f="categorical" aria-pressed="false">Categorical</button>',
'<button class="f" data-f="continuous" aria-pressed="false">Continuous</button>',
'</div>',
'<span id="count"></span>',
'</div>',
'<div class="grid" id="grid">', paste(cards, collapse = ""), '</div>',
'<p class="no-results" id="nores">No variable matches that search.</p>',
'</section>',

if (nrow(no_plot) > 0) paste0(
'<section>',
'<h2>Variables without a plot</h2>',
'<p class="lede">', n_skip, ' variables are listed rather than plotted &mdash; free-text answers and ',
'identifiers have too many distinct values for a chart to say anything useful.</p>',
'<div class="tablewrap"><table><thead><tr><th>Variable</th><th>Question</th>',
'<th class="num">n</th><th class="num">Missing</th><th>Why no plot</th></tr></thead>',
'<tbody>', rows_html, '</tbody></table></div>',
'</section>') else "",

'<footer class="foot">Generated by build_report.R from the cleaned survey data. ',
'Plots come from summarize_data.R, maps from map_experts.R.</footer>',
'</div>',

'<script>',
'(function(){',
'  var grid=document.getElementById("grid");',
'  var cards=Array.prototype.slice.call(grid.querySelectorAll(".card"));',
'  var q=document.getElementById("q");',
'  var count=document.getElementById("count");',
'  var nores=document.getElementById("nores");',
'  var btns=Array.prototype.slice.call(document.querySelectorAll(".f"));',
'  var filter="all";',
'  function apply(){',
'    var term=q.value.trim().toLowerCase();',
'    var shown=0;',
'    cards.forEach(function(c){',
'      var okType=(filter==="all")||(c.dataset.type===filter);',
'      var okTerm=(term==="")||(c.dataset.search.indexOf(term)!==-1);',
'      var vis=okType&&okTerm;',
'      c.style.display=vis?"":"none";',
'      if(vis){shown++;}',
'    });',
'    count.textContent=shown+" of "+cards.length+" shown";',
'    nores.style.display=shown===0?"block":"none";',
'  }',
'  q.addEventListener("input",apply);',
'  btns.forEach(function(b){',
'    b.addEventListener("click",function(){',
'      filter=b.dataset.f;',
'      btns.forEach(function(o){o.setAttribute("aria-pressed",String(o===b));});',
'      apply();',
'    });',
'  });',
'  apply();',
'})();',
'</script>'
)

con <- file(REPORT_OUT, open = "wb")
writeLines(paste(html, collapse = "\n"), con, useBytes = TRUE)
close(con)

message("\n==================== Report built ====================")
message("Variables in data  : ", nrow(vs_all))
message("  left out         : ", n_dropped, " (response times, staircase items, bookkeeping)")
message("Preference scores  : ", nrow(pref))
message("Variables plotted  : ", length(cards))
message("Listed, not plotted: ", n_skip)
if (length(missing_plot) > 0) {
  message("WARNING: no PNG found for ", length(missing_plot), " variable(s): ",
          paste(utils::head(missing_plot, 5), collapse = ", "))
}
message("Maps embedded     : ", sum(vapply(map_files, function(m) file.exists(m$file), logical(1))))
message("Written to        : ", REPORT_OUT,
        "  (", round(file.info(REPORT_OUT)$size / 1024^2, 2), " MB)")
message("======================================================")
