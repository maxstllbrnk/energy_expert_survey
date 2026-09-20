# =============================================================================
# 01_plot_functions.R  -  one shared look, and one function per kind of figure
# =============================================================================
# Every figure comes from one of these functions, so all of them share the same
# theme, colours, captions and outlier rule:
#
#   plot_histogram()        a continuous variable, with mean and median
#   plot_bar()              a categorical variable
#   plot_multiple_choice()  "tick all that apply": share choosing each option
#   plot_ranking()          a ranking question: mentions per option and rank
#   plot_ranking_weights()  a ranking question: mean weight per option
#   plot_stacked()          100 % stacked bars: one categorical variable by another
#   save_figure()           writes a figure into the current output folder
#
# A plot function returns NULL instead of a figure when fewer than MIN_N
# experts answered, and save_figure() skips NULL. A question that was not shown
# to a Berufsgruppe therefore produces no file for that group.
#
# Captions and save_figure() read three objects that the runner sets for every
# subset: `sample_spec` (a row of SAMPLES), `group_label` and `out_dir`.
# =============================================================================


# =============================================================================
# question text
# =============================================================================
# Titles are the questionnaire wording, from the codebook the cleaning pipeline
# writes. Instructions to the respondent are left out of titles.
codebook <- read_csv(CODEBOOK_FILE, show_col_types = FALSE)

INSTRUCTIONS <- c(
  "Falls es keine Haushalte.*$",
  "Falls der jeweilige Energieträger.*$",
  "Bitte geben Sie eine geschätzte Spanne.*$",
  "Eine grobe Schätzung ist ausreichend\\."
)

# "[Gas] Wenn Sie schätzen müssten, ...?  Falls es ..."  ->  "Wenn Sie schätzen müssten, ...?"
question_text <- function(variable) {
  if (variable %in% names(TITLES)) return(TITLES[[variable]])
  codebook$question_text[match(str_remove(variable, "_recoded$"), codebook$variable_name)] %>%
    str_remove("^\\[[^\\]]*\\]") %>%
    str_remove_all(str_c(INSTRUCTIONS, collapse = "|")) %>%
    str_squish()
}

# "[Öl  ⓘ Sie können von ... ] Wenn Sie ..."  ->  "Öl"   (NA if there is none)
question_item <- function(variables) {
  item <- codebook$question_text[match(variables, codebook$variable_name)] %>%
    str_extract("^\\[[^\\]]*\\]") %>%
    str_remove_all("^\\[|\\]$") %>%
    str_remove("ⓘ.*$") %>%
    str_squish()
  coalesce(unname(SUBTITLES[variables]), item)
}

# "price_gas" -> "Q15_SQ001_price_gas", so a file can be found in the questionnaire.
file_name <- function(variable) {
  id <- codebook$question_id[match(str_remove(variable, "_recoded$"), codebook$variable_name)]
  if (is.na(id) || id == "-") variable else str_c(id, "_", variable)
}

# "SHK-Handwerk" -> "shk_handwerk"
folder_name <- function(label) {
  label %>%
    str_to_lower() %>%
    str_replace_all(c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss")) %>%
    str_replace_all("[^a-z0-9]+", "_")
}


# =============================================================================
# numbers, text and theme
# =============================================================================
# German number format: decimal comma, and a thousands point only from 10.000
# on, so that years stay "1975".
fmt <- function(x, digits = 1) {
  map_chr(x, ~ formatC(.x, format = "f", digits = digits, decimal.mark = ",",
                       big.mark = if (abs(.x) >= 10000) "." else ""))
}
fmt_n      <- function(n) fmt(n, digits = 0)
percent_de <- scales::label_percent(accuracy = 1, suffix = " %", decimal.mark = ",")
number_de  <- scales::label_number(big.mark = "", decimal.mark = ",")

LABEL_SIZE <- 0.8 * BASE_SIZE / .pt   # text drawn on a plot, in ggplot's mm

# Long question texts are wrapped; missing text becomes no title at all.
wrap_text <- function(text, width) {
  if (is.null(text) || all(is.na(text))) return(NULL)
  str_wrap(text, width)
}
wrap_title    <- function(text) wrap_text(text, 80)
wrap_subtitle <- function(text) wrap_text(text, 95)

# First line: how many answers, plus a note (e.g. removed outliers).
# Second line: which sample and group the figure shows - figures get copied
# into papers and slides, and must say what they are without their folder.
make_caption <- function(n, note = NULL) {
  first_line <- str_c(c(str_c("n = ", fmt_n(n)), note), collapse = " · ")
  str_c(str_wrap(first_line, 120), "\n",
        "Stichprobe: ", sample_spec$label, " · Gruppe: ", group_label)
}

theme_set(
  theme_minimal(base_size = BASE_SIZE) +
    theme(
      plot.title            = element_text(face = "bold", color = INK_PRIMARY, size = rel(1.05)),
      plot.subtitle         = element_text(color = INK_SECONDARY, margin = margin(b = 8)),
      plot.caption          = element_text(color = INK_MUTED, size = rel(0.75), hjust = 0,
                                           margin = margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.background       = element_rect(fill = "white", color = NA),
      plot.margin           = margin(10, 12, 8, 10),
      panel.grid.major      = element_line(color = GRID_COLOR, linewidth = 0.3),
      panel.grid.minor      = element_blank(),
      axis.title            = element_text(color = INK_SECONDARY, size = rel(0.9)),
      axis.text             = element_text(color = INK_SECONDARY),
      legend.position       = "top",
      legend.justification  = "left",
      legend.location       = "plot",      # align the legend with the title
      legend.title          = element_blank(),
      legend.text           = element_text(color = INK_SECONDARY),
      strip.text            = element_text(face = "bold", color = INK_PRIMARY, hjust = 0)
    )
)

# White text on dark fills, dark text on light ones.
is_dark <- function(colors) farver::decode_colour(colors, to = "hcl")[, "l"] < 55


# =============================================================================
# outlier rule (see 00_settings.R)
# =============================================================================
# Returns the non-missing values that survive VALID_RANGE and TRIM_QUANTILES.
remove_outliers <- function(values, variable) {
  kept <- values[!is.na(values)]
  if (variable %in% NOT_TRIMMED) return(kept)

  range <- VALID_RANGE[[variable]]
  if (!is.null(range)) kept <- kept[kept >= range[1] & kept <= range[2]]

  if (length(kept) > 0) {
    limits <- quantile(kept, TRIM_QUANTILES, names = FALSE)
    kept <- kept[kept >= limits[1] & kept <= limits[2]]
  }
  kept
}

# "12 Ausreißer entfernt (außerhalb von 1920–2010 oder unter dem 0,5-%- bzw.
#  über dem 99,5-%-Quantil)"
outlier_note <- function(n_removed, variable) {
  rule <- str_c("unter dem ", fmt(100 * TRIM_QUANTILES[1]), "-%- bzw. über dem ",
                fmt(100 * TRIM_QUANTILES[2]), "-%-Quantil")
  range <- VALID_RANGE[[variable]]
  if (!is.null(range)) rule <- str_c("außerhalb von ", range[1], "–", range[2], " oder ", rule)
  str_c(fmt_n(n_removed), " Ausreißer entfernt (", rule, ")")
}


# =============================================================================
# plot functions
# =============================================================================

# --- a continuous variable ---------------------------------------------------
LOG_BREAKS <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000)

plot_histogram <- function(data, variable, title = question_text(variable),
                           subtitle = question_item(variable), x_label = NULL,
                           binwidth = NA, log_scale = FALSE) {
  values <- data[[variable]]
  kept   <- remove_outliers(values, variable)
  if (length(kept) < MIN_N) return(NULL)

  n_removed <- sum(!is.na(values)) - length(kept)
  note      <- if (n_removed > 0) outlier_note(n_removed, variable)

  # Mean and median as vertical lines; the legend carries their values. Scores
  # between 0 and 1 get two decimals, everything else one.
  digits <- if (max(abs(kept)) <= 1) 2 else 1
  lines <- tibble(
    label = c(str_c("Mittelwert: ", fmt(mean(kept), digits)),
              str_c("Median: ", fmt(median(kept), digits))),
    value = c(mean(kept), median(kept))
  )

  p <- ggplot(tibble(value = kept), aes(x = value)) +
    geom_histogram(bins = 30, binwidth = if (is.na(binwidth)) NULL else binwidth,
                   boundary = 0, fill = COL_BAR, color = "white", linewidth = 0.2) +
    geom_vline(data = lines, aes(xintercept = value, linetype = label),
               color = INK_PRIMARY, linewidth = 0.6) +
    scale_linetype_manual(values = set_names(c("solid", "22"), lines$label),
                          breaks = lines$label) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = wrap_title(title), subtitle = wrap_subtitle(subtitle),
         x = x_label, y = "Anzahl", caption = make_caption(length(kept), note))

  if (log_scale) {
    p + scale_x_continuous(transform = "log1p", breaks = LOG_BREAKS, labels = number_de)
  } else {
    p + scale_x_continuous(labels = number_de)
  }
}

# --- a categorical variable ----------------------------------------------------
# Bars in the order of the answer options (the factor levels), as shares of
# those who answered.
plot_bar <- function(data, variable, title = question_text(variable),
                     subtitle = question_item(variable)) {
  answers <- data %>%
    filter(!is.na(.data[[variable]])) %>%
    count(answer = .data[[variable]]) %>%
    mutate(share = n / sum(n))
  if (sum(answers$n) < MIN_N) return(NULL)

  short <- SHORT_LABELS[[variable]] %||% character()
  relabel <- function(x) {
    hit <- x %in% names(short)
    x[hit] <- short[x[hit]]
    str_wrap(x, 45)
  }

  ggplot(answers, aes(x = share, y = fct_rev(factor(answer)))) +
    geom_col(fill = COL_BAR, width = 0.7) +
    geom_text(aes(label = percent_de(share)), hjust = -0.2, size = LABEL_SIZE,
              color = INK_SECONDARY) +
    scale_x_continuous(labels = percent_de, expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(labels = relabel) +
    labs(title = wrap_title(title), subtitle = wrap_subtitle(subtitle),
         x = "Anteil der Antworten", y = NULL, caption = make_caption(sum(answers$n))) +
    theme(panel.grid.major.y = element_blank())
}

# --- "tick all that apply" -------------------------------------------------------
# Each option is its own Ja / Nicht Gewählt (or Ja / Nein) column. The share is
# taken over everyone who answered the question, largest first.
plot_multiple_choice <- function(data, variables, title = question_text(variables[1]),
                                 subtitle = "Mehrfachauswahl: Anteil der Befragten, die die Option gewählt haben") {
  options <- data %>%
    select(all_of(variables)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "answer",
                 values_drop_na = TRUE) %>%
    summarise(n = n(), share = mean(answer == "Ja"), .by = variable) %>%
    mutate(option = question_item(variable))
  if (nrow(options) == 0 || max(options$n) < MIN_N) return(NULL)

  ggplot(options, aes(x = share, y = fct_reorder(option, share))) +
    geom_col(fill = COL_BAR, width = 0.7) +
    geom_text(aes(label = percent_de(share)), hjust = -0.2, size = LABEL_SIZE,
              color = INK_SECONDARY) +
    scale_x_continuous(labels = percent_de, expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(labels = scales::label_wrap(45)) +
    labs(title = wrap_title(title), subtitle = wrap_subtitle(subtitle),
         x = "Anteil der Befragten", y = NULL, caption = make_caption(max(options$n))) +
    theme(panel.grid.major.y = element_blank())
}

# --- a ranking question ----------------------------------------------------------
# Every figure and table reads a ranking the same way: one row per expert x
# option they named, with the rank they gave it and the weight their ranking
# puts on it. An expert who names k options gives the option on rank r the
# weight (k + 1 - r) / (k (k + 1) / 2) - 3/6, 2/6, 1/6 for three options,
# 2/3, 1/3 for two, 1 for one - so every expert's weights sum to 1, however
# many options they named. Options not named get no row, i.e. weight 0. Ranks
# are counted among the options named, so a skipped rank leaves no gap.
#
# `by` keeps further columns that identify an expert: in 07_tables.R the same
# expert appears once per gruppe.
ranking_weights <- function(data, variables, by = character()) {
  data %>%
    select(all_of(by), resp_uid, all_of(variables)) %>%
    mutate(across(all_of(variables), as.character)) %>%
    pivot_longer(all_of(variables), names_to = "rank", values_to = "option",
                 values_drop_na = TRUE) %>%
    mutate(rank = as.integer(str_extract(rank, "[0-9]+$"))) %>%
    mutate(k      = n(),
           weight = (k + 1 - min_rank(rank)) / (k * (k + 1) / 2),
           .by = all_of(c(by, "resp_uid"))) %>%
    select(-k)
}

# The mean weight of each option over the n experts who named at least one
# option. The means of all options sum to 1.
mean_ranking_weights <- function(data, variables, by = character()) {
  ranking_weights(data, variables, by) %>%
    mutate(n = n_distinct(resp_uid), .by = all_of(by)) %>%
    summarise(weight = sum(weight) / first(n), .by = all_of(c(by, "n", "option")))
}

# "3 Nennungen: 3/6, 2/6, 1/6 · 2 Nennungen: 2/3, 1/3 · 1 Nennung: 1"
weight_rule <- function(n_ranks) {
  map_chr(n_ranks:1, function(k) {
    if (k == 1) return("1 Nennung: 1")
    str_c(k, " Nennungen: ", str_c(k:1, "/", k * (k + 1) / 2, collapse = ", "))
  }) %>%
    str_c(collapse = " · ")
}

# One bar per option: the share of experts naming it, split by the rank they
# gave it. Options named most often overall come first.
plot_ranking <- function(data, variables, title = question_text(variables[1])) {
  mentions <- ranking_weights(data, variables) %>%
    mutate(rank = str_c("Rang ", rank))
  n_experts <- n_distinct(mentions$resp_uid)
  if (n_experts < MIN_N) return(NULL)

  shares <- mentions %>%
    count(option, rank) %>%
    mutate(share = n / n_experts)

  ggplot(shares, aes(x = share, y = fct_reorder(option, share, .fun = sum), fill = rank)) +
    geom_col(width = 0.7, color = "white", linewidth = 0.3,
             position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = RANK_COLORS) +
    scale_x_continuous(labels = percent_de, expand = expansion(mult = c(0, 0.05))) +
    scale_y_discrete(labels = scales::label_wrap(45)) +
    labs(title = wrap_title(title),
         subtitle = "Anteil der Befragten, die die Option nennen, nach vergebenem Rang",
         x = "Anteil der Befragten", y = NULL, caption = make_caption(n_experts)) +
    theme(panel.grid.major.y = element_blank())
}

# One bar per option: the mean weight experts put on it, largest first. The
# bars sum to 100 %, and an expert who named fewer options counts as much as
# one who named all.
plot_ranking_weights <- function(data, variables, title = question_text(variables[1])) {
  weights <- mean_ranking_weights(data, variables)
  if (nrow(weights) == 0 || weights$n[1] < MIN_N) return(NULL)

  subtitle <- str_c("Mittleres Gewicht je Option. Die Rangfolge jedes Befragten ist in Gewichte ",
                    "umgerechnet, die sich zu 100 % summieren (", weight_rule(length(variables)), ")")

  ggplot(weights, aes(x = weight, y = fct_reorder(option, weight))) +
    geom_col(fill = COL_BAR, width = 0.7) +
    geom_text(aes(label = percent_de(weight)), hjust = -0.2, size = LABEL_SIZE,
              color = INK_SECONDARY) +
    scale_x_continuous(labels = percent_de, expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(labels = scales::label_wrap(45)) +
    labs(title = wrap_title(title), subtitle = wrap_subtitle(subtitle),
         x = "Mittleres Gewicht", y = NULL, caption = make_caption(weights$n[1])) +
    theme(panel.grid.major.y = element_blank())
}

# --- one categorical variable by another -----------------------------------------
# One 100 % bar per value of `y`, split by `fill`, optionally one panel per
# value of `facet`. Bars resting on fewer than MIN_N answers are left out.
plot_stacked <- function(data, y, fill, colors, title, subtitle = NULL, facet = NULL) {
  bars <- data %>%
    filter(!is.na(.data[[y]]), !is.na(.data[[fill]])) %>%
    count(across(all_of(c(facet, y, fill)))) %>%
    mutate(n_bar = sum(n), share = n / n_bar, .by = all_of(c(facet, y)))

  n_small <- bars %>% filter(n_bar < MIN_N) %>% distinct(across(all_of(c(facet, y)))) %>% nrow()
  bars    <- bars %>% filter(n_bar >= MIN_N)
  if (nrow(bars) == 0) return(NULL)

  bars <- bars %>%
    mutate(label       = if_else(share >= 0.07, percent_de(share), ""),
           label_color = if_else(is_dark(colors[as.character(.data[[fill]])]),
                                 "white", INK_PRIMARY))
  totals <- bars %>% distinct(across(all_of(c(facet, y))), n_bar)
  note   <- if (n_small > 0) str_c(n_small, " Balken mit weniger als ", MIN_N,
                                   " Antworten nicht dargestellt")

  p <- ggplot(bars, aes(x = share, y = fct_rev(factor(.data[[y]])))) +
    geom_col(aes(fill = .data[[fill]]), width = 0.7, color = "white", linewidth = 0.3,
             position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = label, color = label_color, group = .data[[fill]]),
              size = LABEL_SIZE, position = position_stack(vjust = 0.5, reverse = TRUE)) +
    geom_text(data = totals, aes(x = 1.01, label = str_c("n = ", fmt_n(n_bar))),
              hjust = 0, size = LABEL_SIZE, color = INK_MUTED) +
    scale_fill_manual(values = colors) +
    scale_color_identity() +
    scale_x_continuous(labels = percent_de, breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.14))) +
    scale_y_discrete(labels = scales::label_wrap(35)) +
    guides(fill = guide_legend(nrow = ceiling(n_distinct(bars[[fill]]) / 4), byrow = TRUE)) +
    labs(title = wrap_title(title), subtitle = wrap_subtitle(subtitle), x = "Anteil", y = NULL,
         caption = make_caption(sum(bars$n), note)) +
    theme(panel.grid.major.y = element_blank())

  if (!is.null(facet)) p <- p + facet_wrap(vars(.data[[facet]]), ncol = 1, scales = "free_y")
  p
}


# =============================================================================
# saving
# =============================================================================
# Writes <out_dir>/<section>/<name>.png and <name>.svg. The SVG is for the
# policy briefs: svglite keeps the text as text, so it stays editable. Does
# nothing for NULL, so that
#   plot_bar(...) %>% save_figure(...)
# simply skips a figure that had too few answers.
save_figure <- function(plot, section, name, height = FIG_HEIGHT) {
  if (is.null(plot)) return(invisible(NULL))
  folder <- file.path(out_dir, section)
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(folder, str_c(name, ".png")), plot,
         width = FIG_WIDTH, height = height, units = "cm", dpi = FIG_DPI,
         device = ragg::agg_png, bg = "white")
  ggsave(file.path(folder, str_c(name, ".svg")), plot,
         width = FIG_WIDTH, height = height, units = "cm",
         device = svglite::svglite, bg = "white")
}
