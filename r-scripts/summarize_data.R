# =============================================================================
# summarize_data.R  -  Quick descriptive summary of the cleaned data set
# -----------------------------------------------------------------------------
# For every variable in output/cleaned_survey_data.rds, produces either a
# histogram (continuous variables) or a sorted bar plot (categorical
# variables), plus a companion table of basic statistics. This is meant as a
# first look at the data - a quick way to see what each variable looks like
# and spot anything that needs cleaning - not a publication-ready analysis.
#
# HOW TO RUN (after run_pipeline.R has produced output/cleaned_survey_data.rds):
#   - In RStudio: open this file and click "Source"; or
#   - In a terminal, from the project folder:  Rscript summarize_data.R
#
# WHAT IT PRODUCES (in output/summary/):
#   - one PNG per variable (bar plot or histogram)
# And in output/:
#   - variable_summary.csv - one row per variable with basic statistics
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

# --- 1. Load configuration (packages + paths) ----------------------------------
source("r-scripts/pipeline/config.R", encoding = "UTF-8")

# --- 1b. Optional sample restriction -------------------------------------------
# A wrapper script (build_report_completers.R) can set these BEFORE sourcing this
# file, to summarise a subset of respondents and write the results under a
# different name. Run on its own, this script behaves exactly as before.
#
#   COMPLETERS_ONLY  TRUE keeps only experts who submitted the questionnaire
#   OUTPUT_TAG       appended to every output name, e.g. "_completers"
if (!exists("COMPLETERS_ONLY")) COMPLETERS_ONLY <- FALSE
if (!exists("OUTPUT_TAG"))      OUTPUT_TAG      <- ""

# --- 2. Settings you may want to tweak -----------------------------------------
# Pure record-keeping columns (respondent id, timestamps, tokens) - never
# useful to summarize, so they are skipped outright.
EXCLUDE_VARS <- c(
  "id", "unique_id", "survey_id", "submitdate", "startdate", "datestamp",
  "seed", "token", "startlanguage"
)

# A categorical variable with more distinct values than this is skipped (a bar
# per unique free-text answer is not a useful plot). It still gets a row in
# variable_summary.csv, just no plot.
MAX_CATEGORIES <- 20

# A numeric variable with at most this many distinct values is treated as
# categorical (e.g. vignette_position, which only takes values 1-6) instead of
# being plotted as a histogram.
MAX_NUMERIC_AS_CATEGORICAL <- 10

# Histogram axis trimming (see the continuous branch below). A histogram is
# trimmed to its central 99% only when that central 99% spans LESS than this
# fraction of the full range - i.e. only when a sparse tail is stretching the
# axis and hiding the shape of the data. Raise it to trim more aggressively,
# set it to 0 to always plot the full range.
TRIM_IF_CENTRE_BELOW <- 0.6

# Safety valve on that trimming: never leave out more than this share of the
# observations. If the rule wants to cut more, the variable is genuinely spread
# out rather than outlier-ridden, and it is drawn in full instead.
#
# Set to 10% deliberately. Several substantive variables here - the per-kWh
# energy prices, the number of households advised, company size - are extremely
# right-skewed: the median gas price is 11 while the maximum is 250, so the top
# few per cent of answers squash the entire distribution into one bar. Cutting
# only 2% left those plots as unreadable as before. The trimmed values are real
# answers rather than errors, so every trimmed plot states on its face how many
# were left out and over what range, and the full min/max stay in
# variable_summary.csv.
# 20%: the CO2-price range question needs it. Its lower bound (Q18b_SQ001) has
# 14.1% of answers outside the fence and its upper bound (Q18b_SQ002) 15.3%, so
# a tighter cap would trim one half of a single question and not the other,
# leaving the two halves on incomparable axes.
MAX_TRIM_SHARE <- 0.20

# Hard axis limits for individual variables, where the automatic rule cannot
# know what counts as a possible answer. `NA` leaves that end to the automatic
# rule. Values outside the limit are counted and reported on the plot exactly
# like an automatic trim, never silently dropped.
MANUAL_TRIM <- list(
  # Birth years below 1940 would mean a working expert aged over 86. They are
  # mistyped entries (the raw column runs down to -1957), and the automatic
  # fence only pulls the axis back to 1902.
  Q21 = c(1940, NA)
)

# Descriptions for variables the pipeline derives, which therefore have no
# LimeSurvey question text of their own.
DESCRIPTIONS <- list(
  answer          = "Expertenempfehlung aus Vignette",
  investment_cost = "Geschätzte Investitionssumme"
)

# Some answer options are written out as full sentences, which makes the bar
# labels unreadable - and for Q6a the two "Nein" options are identical for the
# first sixty characters, so truncation alone makes them indistinguishable.
# These replace the label text on the plot only; the data is untouched.
SHORT_LABELS <- list(
  Q6a = c(
    "Ja, eine Fernwärmeversorgung ist vorhanden."                                                                  = "Ja, vorhanden",
    "Nein, eine Fernwärmeversorgung ist nicht vorhanden. Eine Versorgung ist jedoch in den nächsten Jahren geplant." = "Nein, aber geplant",
    "Nein, eine Fernwärmeversorgung ist nicht vorhanden und diese wird in den nächsten Jahren auch nicht geplant."   = "Nein, nicht geplant",
    "Weiß nicht"                                                                                                    = "Weiß nicht"
  )
)

PLOT_DIR <- file.path("output", paste0("summary", OUTPUT_TAG))
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

# Clear plots from a previous run before writing new ones. Without this, a
# variable that has since been renamed or dropped - or that came from an earlier,
# smaller data set - leaves its old PNG sitting in the folder next to the current
# ones, with nothing to indicate it is stale. Anyone browsing the folder would
# read it as a current result.
old_plots <- list.files(PLOT_DIR, pattern = "\\.png$", full.names = TRUE)
if (length(old_plots) > 0L) {
  unlink(old_plots)
  message("Removed ", length(old_plots), " plot(s) from a previous run.")
}

# --- 3. Load the cleaned data ---------------------------------------------------
data_path <- file.path(OUTPUT_DIR, "cleaned_survey_data.rds")
if (!file.exists(data_path)) {
  stop(
    "Could not find '", data_path, "'. Run run_pipeline.R first to produce it.",
    call. = FALSE
  )
}
d <- readRDS(data_path)
question_text <- attr(d, "variable.labels")  # named lookup, may be NULL

message("Loaded ", nrow(d), " rows, ", ncol(d), " columns from '", data_path, "'.")

# Restrict to experts who submitted, if asked. `submitdate` is set only when the
# questionnaire is submitted: all 2,165 submitters end on lastpage 121 (the final
# page) and everyone else stops at 120 or earlier, so this is exactly "finished
# the survey" rather than a proxy for it.
#
# Note the attribute has to be re-attached: subsetting a data frame drops
# variable.labels, which is what supplies the question text on every plot.
if (COMPLETERS_ONLY) {
  keep <- !is.na(d$submitdate)
  d <- d[keep, , drop = FALSE]
  attr(d, "variable.labels") <- question_text
  message("Restricted to completed questionnaires: ", nrow(d), " rows, ",
          length(unique(d$unique_id)), " experts.")
}

# --- 4. Helpers ------------------------------------------------------------------

# A respondent-level variable (e.g. "Q3", company size) is duplicated across
# every row belonging to the same respondent, because the cleaned data has one
# row per respondent x vignette x Fernwaerme branch. Summarizing it on every
# row would still show the right shape, but the wrong N (12x too high) and the
# wrong missing-value count. This detects that case per variable so counts are
# reported at the correct level automatically, without hard-coding which
# columns are respondent-level.
is_respondent_level <- function(x, id) {
  all(tapply(x, id, function(v) length(unique(v))) <= 1L)
}

truncate_label <- function(x, width = 40) {
  x <- as.character(x)
  ifelse(is.na(x), "NA",
         ifelse(nchar(x) > width, paste0(substr(x, 1, width - 1), "…"), x))
}

# Wraps (rather than hard-truncates) a title so long German question text
# stays readable instead of running off the edge of the plot. `width` should
# be narrower for bar plots, whose left margin (for category labels) leaves
# less horizontal room than a histogram has.
# Some questions (e.g. the risk/time-preference lottery items) carry a full
# paragraph of instruction text, not a short question. Capping at max_lines
# keeps the title's footprint bounded no matter how long the source text is -
# otherwise a long enough label makes the margins exceed the image size and
# the plot fails outright.
wrap_title <- function(label, width, max_lines = 3) {
  lines <- strwrap(label, width = width)
  if (length(lines) > max_lines) {
    lines <- lines[seq_len(max_lines)]
    lines[max_lines] <- paste0(sub("\\s+$", "", lines[max_lines]), "…")
  }
  list(
    text       = paste(lines, collapse = "\n"),
    n_lines    = length(lines),
    top_margin = 3 + 1.3 * length(lines)
  )
}

save_plot <- function(var_name, height, plot_fun, width = 900) {
  png(
    file.path(PLOT_DIR, paste0(var_name, ".png")),
    width = width, height = height, res = 120
  )
  on.exit(dev.off())
  plot_fun()
}

# --- 5. Summarize one variable: writes a plot (if applicable) and returns a
#        one-row summary for variable_summary.csv --------------------------------
summarize_variable <- function(var_name) {

  x_full <- d[[var_name]]

  # Timing/system columns have no LimeSurvey question text, stored as NA (not
  # ""). Note nzchar(NA) is TRUE in base R (NA is coerced to the two-character
  # string "NA"), so the NA check has to come first or these would silently
  # display the literal word "NA" as their plot title.
  raw_label <- if (!is.null(question_text) && var_name %in% names(question_text)) {
    question_text[[var_name]]
  } else {
    NA_character_
  }
  label <- if (!is.null(DESCRIPTIONS[[var_name]])) {
    DESCRIPTIONS[[var_name]]
  } else if (!is.na(raw_label) && nzchar(raw_label)) {
    raw_label
  } else {
    var_name
  }

  respondent_level <- is_respondent_level(x_full, d$unique_id)
  level_label <- if (respondent_level) "respondent" else "vignette-observation"

  x <- if (respondent_level) {
    # Column first, then rows. `d[rows, var_name, drop = TRUE]` would rely on a
    # tibble honouring `drop`, which it deprecates; `[[` always gives a vector.
    x_full[!duplicated(d$unique_id)]
  } else {
    x_full
  }

  n_total   <- length(x)
  n_missing <- sum(is.na(x))
  n_unique  <- length(unique(x[!is.na(x)]))

  is_numeric_continuous <- is.numeric(x) && n_unique > MAX_NUMERIC_AS_CATEGORICAL

  row <- data.frame(
    variable_name     = var_name,
    question_text     = label,
    level             = level_label,
    n                 = n_total,
    n_missing         = n_missing,
    pct_missing       = round(100 * n_missing / n_total, 1),
    n_unique          = n_unique,
    type              = NA_character_,
    mean              = NA_real_,
    sd                = NA_real_,
    min               = NA_real_,
    median            = NA_real_,
    max               = NA_real_,
    top_category      = NA_character_,
    top_category_pct  = NA_real_,
    # How many observations the histogram's axis leaves out, so the trimming is
    # recorded in the table as well as drawn on the plot.
    n_omitted_from_plot = 0L,
    plot_range          = NA_character_,
    stringsAsFactors  = FALSE
  )

  if (n_unique == 0L) {
    row$type <- "empty"
    return(row)
  }

  if (is_numeric_continuous) {
    row$type   <- "continuous"
    row$mean   <- round(mean(x, na.rm = TRUE), 2)
    row$sd     <- round(sd(x, na.rm = TRUE), 2)
    row$min    <- round(min(x, na.rm = TRUE), 2)
    row$median <- round(median(x, na.rm = TRUE), 2)
    row$max    <- round(max(x, na.rm = TRUE), 2)

    # A handful of extreme values - a mistyped birth year, an energy price
    # entered per litre instead of per kWh - stretches the x axis so far that the
    # bulk of the data collapses into the first bar and the distribution becomes
    # unreadable. Where that is happening, the axis is trimmed to the central
    # 99% and the number of values left off is stated on the plot, so the tail is
    # never silently hidden. The full range stays in the summary table's
    # min/max columns either way.
    #
    # The trim only applies when it actually buys something: if the central 99%
    # already covers most of the range, the data has no such tail and the plot is
    # drawn in full.
    x_valid <- x[!is.na(x)]
    qs  <- unname(stats::quantile(x_valid, c(0.005, 0.25, 0.75, 0.995)))
    iqr <- qs[3] - qs[2]

    # Two bounds, and the TIGHTER of the two wins.
    #
    # Percentiles alone are not enough: Q21 (birth year) has so many mistyped
    # entries that even its 0.5th percentile is 92, so cutting at percentiles
    # still leaves an axis spanning nineteen centuries. Tukey's "far out" fence
    # (3x the interquartile range beyond the quartiles) is driven by the middle
    # of the distribution instead, so a cluster of nonsense values cannot drag
    # it outwards.
    #
    # Percentiles still matter for the opposite case - a genuinely long-tailed
    # variable where the fence would cut real data - which is why the wider of
    # the two lower bounds and the narrower of the two upper bounds is used.
    lo <- qs[1]
    hi <- qs[4]
    if (is.finite(iqr) && iqr > 0) {
      lo <- max(lo, qs[2] - 3 * iqr)
      hi <- min(hi, qs[3] + 3 * iqr)
    }

    span_full <- diff(range(x_valid))
    do_trim <- span_full > 0 && hi > lo &&
      (hi - lo) < TRIM_IF_CENTRE_BELOW * span_full

    if (do_trim) {
      keep <- x_valid >= lo & x_valid <= hi
      # Never drop more than a sliver. If this rule would cut a real part of the
      # distribution the variable is genuinely spread out, not outlier-ridden,
      # and it belongs on screen in full.
      if (mean(!keep) > MAX_TRIM_SHARE) do_trim <- FALSE
    }

    # A hand-set limit overrides the automatic rule in both directions: it
    # applies even where the automatic rule decided not to trim at all.
    manual <- MANUAL_TRIM[[var_name]]
    if (!is.null(manual)) {
      if (!is.na(manual[1])) lo <- if (do_trim) max(lo, manual[1]) else manual[1]
      if (!is.na(manual[2])) hi <- if (do_trim) min(hi, manual[2]) else manual[2]
      if (is.na(manual[1]) && !do_trim) lo <- min(x_valid)
      if (is.na(manual[2]) && !do_trim) hi <- max(x_valid)
      do_trim <- TRUE
    }

    x_plot <- if (do_trim) x_valid[x_valid >= lo & x_valid <= hi] else x_valid
    n_omit <- length(x_valid) - length(x_plot)

    subtitle <- sprintf(
      "%s level | n=%d, missing=%d (%.0f%%) | mean=%.1f, sd=%.1f, median=%.1f",
      level_label, n_total, n_missing, row$pct_missing, row$mean, row$sd, row$median
    )

    # The trimming note goes INSIDE the plot rather than into the subtitle. It
    # has to be impossible to miss - a reader who does not notice it will read a
    # trimmed axis as the full range of the data - and appending it to the
    # subtitle both buried it and pushed that line past the edge of the image.
    row$n_omitted_from_plot <- as.integer(n_omit)
    if (n_omit > 0) {
      row$plot_range <- paste0(format(round(lo, 2), trim = TRUE), " to ",
                               format(round(hi, 2), trim = TRUE))
    }

    trim_note <- if (n_omit > 0) {
      sprintf("%d value%s outside %s-%s not shown",
              n_omit, if (n_omit == 1) "" else "s",
              format(round(lo, 2), trim = TRUE), format(round(hi, 2), trim = TRUE))
    } else NULL

    t <- wrap_title(label, width = 55)

    save_plot(var_name, height = 600 + 20 * t$n_lines, function() {
      old_mar <- par("mar")
      on.exit(par(mar = old_mar), add = TRUE)
      par(mar = c(5, 4, t$top_margin, 2))
      # Leave headroom above the tallest bar when a note has to go there, so the
      # note sits in empty space instead of on top of the data.
      h <- hist(x_plot, plot = FALSE)
      ylim <- if (is.null(trim_note)) NULL else c(0, max(h$counts) * 1.18)
      hist(
        x_plot, main = t$text, sub = subtitle, cex.sub = 0.72,
        xlab = var_name, col = "#4C78A8", border = "white", ylim = ylim
      )
      if (!is.null(trim_note)) {
        u <- par("usr")
        text(u[2] - diff(u[1:2]) * 0.02, u[4] - diff(u[3:4]) * 0.04,
             # Plain ASCII only: the PNG device's font has no glyph for
             # decorative marks like an arrow, and draws an empty box instead.
             labels = paste("Axis trimmed -", trim_note),
             adj = c(1, 1), cex = 0.72, col = "#8a6410", font = 2)
      }
    })

  } else if (n_unique > MAX_CATEGORIES) {
    row$type <- "skipped_high_cardinality"

  } else {
    row$type <- "categorical"

    # Missing values are NOT drawn as a bar. A bar chart answers "how did people
    # answer this question", and an NA bar answers a different question that is
    # already reported twice - as a count in the subtitle below and as the
    # missingness chip in the report. Worse, where missingness dominates (the
    # vignette-6 cost question is only asked of a small subset, so it is ~95%
    # NA) the NA bar flattens every real category into invisibility.
    counts <- sort(table(x), decreasing = TRUE)
    n_valid <- sum(counts)
    top <- counts[1]
    row$top_category <- names(top)
    # Share of those who ANSWERED, which is what the bars show.
    row$top_category_pct <- if (n_valid > 0) round(100 * top / n_valid, 1) else NA_real_

    # Replace over-long answer texts with readable short forms where one is
    # defined, before truncating whatever is left.
    display_names <- names(counts)
    shorts <- SHORT_LABELS[[var_name]]
    if (!is.null(shorts)) {
      hit <- display_names %in% names(shorts)
      display_names[hit] <- unname(shorts[display_names[hit]])
    }
    plot_labels <- truncate_label(display_names)
    n_cat <- length(counts)

    # Narrower wrap width than the histogram: the left margin reserved for
    # category labels below eats into the width available for the title.
    t <- wrap_title(label, width = 30)

    save_plot(var_name, height = max(350, 120 + 28 * n_cat) + 20 * t$n_lines, function() {
      old_mar <- par("mar")
      on.exit(par(mar = old_mar), add = TRUE)
      par(mar = c(4, max(8, min(30, max(nchar(plot_labels)) * 0.6)), t$top_margin + 1, 2))
      barplot(
        rev(as.numeric(counts)), horiz = TRUE, names.arg = rev(plot_labels),
        las = 1, cex.names = 0.8, col = "#4C78A8", border = NA,
        main = t$text, xlab = "count"
      )
      mtext(
        sprintf(
          "%s level | bars show the %s who answered | %d missing (%.0f%%) not shown | %d categories",
          level_label, format(n_valid, big.mark = ","), n_missing, row$pct_missing, n_cat
        ),
        side = 3, line = 0.3, cex = 0.65
      )
    })
  }

  row
}

# --- 6. Run over every variable except the excluded record-keeping ones --------
vars_to_summarize <- setdiff(names(d), EXCLUDE_VARS)

message("Summarizing ", length(vars_to_summarize), " variables ...")

summary_rows <- lapply(vars_to_summarize, function(v) {
  tryCatch(
    summarize_variable(v),
    error = function(e) {
      warning("Could not summarize '", v, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
})
variable_summary <- do.call(rbind, summary_rows)

# --- 6b. Composite plots -------------------------------------------------------
# Several questions are stored as a family of columns - one per answer option or
# one per rank - and plotting each column on its own says very little: a
# "tick all that apply" question becomes a dozen near-identical Ja/Nein charts
# that cannot be compared, and a ranking becomes three charts none of which
# shows the overall ordering. These build ONE chart per question instead.
#
# Each composite is written as a normal plot plus a row in the summary table, so
# the report picks it up with no special handling. The columns it is built from
# stay in the data and in the table; the report simply stops showing them
# individually.

# Pulls "Energieberatung für Wohngebäude" out of
# "[Energieberatung für Wohngebäude] Welche der folgenden Leistungen ..." and
# strips LimeSurvey's tooltip marker and its explanatory text.
option_label <- function(txt) {
  out <- sub("^\\s*\\[(.*?)\\].*$", "\\1", txt)
  out <- sub("ⓘ.*$", "", out)      # the circled-i tooltip and all after it
  trimws(gsub("\\s+", " ", out))
}

composite_rows <- list()

add_composite <- function(name, description, labels, values, value_label,
                          denom_note, level_label) {
  ord <- order(values, decreasing = TRUE)
  labels <- labels[ord]; values <- values[ord]
  plot_labels <- truncate_label(labels, 44)
  # Composites carry long option texts down the left-hand side, so they get a
  # wider canvas than the standard plots and a title wrapped to match it. The
  # left margin is sized from the longest label but capped well below the canvas
  # width - an uncapped margin squeezes the plot region to nothing and pushes
  # the whole chart off the image.
  canvas_w <- 1250
  t <- wrap_title(description, width = 62)

  save_plot(name, height = max(360, 130 + 30 * length(values)) + 22 * t$n_lines,
            width = canvas_w,
            function() {
    old_mar <- par("mar")
    on.exit(par(mar = old_mar), add = TRUE)
    par(mar = c(4.4, min(24, max(9, max(nchar(plot_labels)) * 0.52)),
                t$top_margin + 1, 3))
    bp <- barplot(rev(values), horiz = TRUE, names.arg = rev(plot_labels),
                  las = 1, cex.names = 0.78, col = "#4C78A8", border = NA,
                  main = t$text, xlab = value_label,
                  xlim = c(0, max(values) * 1.12))
    text(rev(values), bp, labels = paste0(" ", round(rev(values), 1)),
         adj = 0, cex = 0.72, col = "#44515f", xpd = NA)
    mtext(denom_note, side = 3, line = 0.3, cex = 0.65)
  })

  composite_rows[[name]] <<- data.frame(
    variable_name = name, question_text = description, level = level_label,
    n = NA_integer_, n_missing = NA_integer_, pct_missing = NA_real_,
    n_unique = length(values), type = "composite",
    mean = NA_real_, sd = NA_real_, min = NA_real_, median = NA_real_, max = NA_real_,
    top_category = labels[1], top_category_pct = round(values[1], 1),
    n_omitted_from_plot = 0L, plot_range = NA_character_,
    stringsAsFactors = FALSE
  )
}

# (a) "Tick all that apply" questions. Each option is its own Ja / Nicht Gewählt
#     column. The denominator is everyone who reached the question (any option
#     answered), so the percentages read as "x% of those asked picked this".
MULTISELECT <- list(
  Q2   = "Welcher Kategorie ordnen Sie Ihr Unternehmen zu? (Mehrfachauswahl)",
  Q8   = "Welche Leistungen bieten Sie eigenständig auf dem Markt an? (Mehrfachauswahl)",
  Q19a = "Hemmnisse für erneuerbare Heiztechnologien (Mehrfachauswahl)",
  Q19b = "Hemmnisse für Fernwärme (Mehrfachauswahl)",
  Q19c = "Hemmnisse für konventionelle Heiztechnologien (Mehrfachauswahl)",
  Q24  = "Fortbildungen in den letzten 3 Jahren (Mehrfachauswahl)"
)
for (grp in names(MULTISELECT)) {
  cols <- grep(paste0("^", grp, "_SQ[0-9]+$"), names(d), value = TRUE)
  if (length(cols) == 0L) next
  resp <- d[!duplicated(d$unique_id), cols, drop = FALSE]
  reached <- rowSums(!is.na(resp)) > 0
  if (!any(reached)) next
  sub <- resp[reached, , drop = FALSE]
  pct <- vapply(cols, function(cn) 100 * mean(sub[[cn]] == "Ja", na.rm = TRUE), numeric(1))
  labs <- vapply(cols, function(cn) {
    l <- question_text[[cn]]
    if (is.null(l) || is.na(l) || !nzchar(l)) cn else option_label(l)
  }, character(1))
  add_composite(
    name = paste0(grp, "_summary"), description = MULTISELECT[[grp]],
    labels = unname(labs), values = unname(pct),
    value_label = "% of respondents who chose this option",
    denom_note = sprintf("respondent level | %s reached this question | each bar = %% who ticked that option",
                         format(sum(reached), big.mark = ",")),
    level_label = "respondent"
  )
}

# (b) Ranking questions. Respondents name their top few options in order, so the
#     information is in the ORDER, which per-rank charts throw away. Each pick
#     is scored by position - first choice weighted highest - and the scores are
#     summed per option, giving one ordering of what matters most overall.
RANKED <- list(
  Q28 = list(desc = "Wichtigste Kriterien der Kunden (Rangpunkte: Rang 1 = 3, Rang 2 = 2, Rang 3 = 1)",
             cols = c("Q28_1", "Q28_2", "Q28_3"), weights = c(3, 2, 1)),
  Q7  = list(desc = "Wichtigste Kundengruppen (Rangpunkte: Rang 1 = 2, Rang 2 = 1)",
             cols = c("Q7_1", "Q7_2"), weights = c(2, 1))
)
for (grp in names(RANKED)) {
  spec <- RANKED[[grp]]
  cols <- intersect(spec$cols, names(d))
  if (length(cols) == 0L) next
  resp <- d[!duplicated(d$unique_id), cols, drop = FALSE]
  reached <- rowSums(!is.na(resp)) > 0
  if (!any(reached)) next

  pts <- list()
  for (i in seq_along(cols)) {
    v <- as.character(resp[[cols[i]]])
    v <- v[!is.na(v) & nzchar(trimws(v))]
    if (!length(v)) next
    tb <- table(v) * spec$weights[i]
    for (nm in names(tb)) pts[[nm]] <- (pts[[nm]] %||% 0) + as.numeric(tb[[nm]])
  }
  if (!length(pts)) next
  add_composite(
    name = paste0(grp, "_rank_points"), description = spec$desc,
    labels = names(pts), values = unlist(pts, use.names = FALSE),
    value_label = "rank points (summed across respondents)",
    denom_note = sprintf("respondent level | %s ranked at least one option",
                         format(sum(reached), big.mark = ",")),
    level_label = "respondent"
  )
}

# (c) Free-text answers worth a chart. These have too many distinct values for
#     the normal rule, but the distinct values are heavily concentrated - a
#     handful of manufacturers, a handful of job titles - so the most frequent
#     answers are informative once trivial spelling differences are folded
#     together (lower-cased, whitespace collapsed).
FREETEXT_TOP <- list(
  Q0b    = list(desc = "Rolle innerhalb des Unternehmens (häufigste Freitextangaben)", top = 15),
  Q11c1b = list(desc = "Meistverkaufter Hersteller (häufigste Freitextangaben)", top = 15)
)
for (v in names(FREETEXT_TOP)) {
  if (!v %in% names(d)) next
  spec <- FREETEXT_TOP[[v]]
  # `d[[v]][rows]`, not `d[rows, v]`: the cleaned data is a tibble, and a tibble
  # returns a one-column TIBBLE from `[i, j]` rather than the vector a plain
  # data.frame gives. as.character() on that deparses the whole column into a
  # single "c(na, na, ...)" string, which then plots as one bar labelled with
  # the entire column. `[[` always yields the vector.
  x <- as.character(d[[v]][!duplicated(d$unique_id)])
  x <- trimws(gsub("\\s+", " ", tolower(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) next
  tb <- sort(table(x), decreasing = TRUE)
  keep <- head(tb, spec$top)
  add_composite(
    name = paste0(v, "_top"), description = spec$desc,
    labels = names(keep), values = as.numeric(keep),
    value_label = "number of respondents",
    denom_note = sprintf("respondent level | %s gave an answer, %s distinct | top %d shown, lower-cased",
                         format(length(x), big.mark = ","),
                         format(length(tb), big.mark = ","), length(keep)),
    level_label = "respondent"
  )
}

if (length(composite_rows) > 0) {
  variable_summary <- rbind(variable_summary, do.call(rbind, composite_rows))
  message("Composite plots built        : ", length(composite_rows))
}

summary_csv_path <- file.path("output",
                              paste0("variable_summary", OUTPUT_TAG, ".csv"))
write.csv(variable_summary, summary_csv_path, row.names = FALSE, fileEncoding = "UTF-8")

# --- 7. Summary --------------------------------------------------------------
message("\n==================== Summary finished ====================")
message("Variables summarized         : ", nrow(variable_summary))
message("  continuous (histogram)     : ", sum(variable_summary$type == "continuous"))
message("  categorical (bar plot)     : ", sum(variable_summary$type == "categorical"))
message("  skipped (too many values)  : ", sum(variable_summary$type == "skipped_high_cardinality"))
message("  empty                      : ", sum(variable_summary$type == "empty"))
message("Plots written to             : ", PLOT_DIR)
message("Summary table                : ", summary_csv_path)
message("===============================================================")
