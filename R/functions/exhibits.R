################################################################################
# exhibits.R — what every table, figure and map of the summary statistics shares:
#              one list of exhibits, the minimum cell, the state maps
# Inputs : labels.R; tables.R and figures.R (core files of the economics-r skill)
# Outputs: none
# Checks : the minimum cell in summary_table(); unknown labels stop
################################################################################
# A section of the summary statistics is one function that writes its files into
# dirs$tables, dirs$figures and dirs$maps and returns one exhibit_row() per file.
# The deck and exhibits.csv are built from these rows. Files are named
#   tab_<id>_<group slug>.tex, fig_<id>_<group slug>.pdf, map_<id>_<group slug>.pdf

# 1. Text ----------------------------------------------------------------------

#' Plain text as LaTeX: the reserved characters are escaped, and the superscript
#' two, the multiplication sign and the euro sign become LaTeX, so that one label
#' serves the tables, the figures and Excel
latex_text <- function(text) {
  text <- gsub("&", "\\&", text, fixed = TRUE)
  text <- gsub("%", "\\%", text, fixed = TRUE)
  text <- gsub("_", "\\_", text, fixed = TRUE)
  text <- gsub("#", "\\#", text, fixed = TRUE)
  text <- gsub("$", "\\$", text, fixed = TRUE)
  text <- gsub("²", "$^2$", text, fixed = TRUE)
  text <- gsub("₂", "$_2$", text, fixed = TRUE)
  text <- gsub("×", "$\\times$", text, fixed = TRUE)
  gsub("€", "\\texteuro{}", text, fixed = TRUE)
}

#' The label of each value; stops if a value has none, so that no raw name or
#' German answer reaches a table or figure
#'
#' @param values character vector or factor
#' @param labels named character vector: value -> label
#' @param what   what the values are, for the error message
label_of <- function(values, labels, what) {
  values <- as.character(values)
  missing_values <- setdiff(unique(values[!is.na(values)]), names(labels))
  if (length(missing_values) > 0) {
    stop("No label for the ", what, ": ", paste(missing_values, collapse = ", "),
         " (add it in R/functions/labels.R)")
  }
  unname(labels[values])
}

#' The name of a group in file names, e.g. "HVAC installers" -> "hvac_installers"
group_slug <- function(group_name) {
  label_of(group_name, GROUP_SLUGS, "group")
}

# 2. The list of exhibits ------------------------------------------------------

#' One row of the list of exhibits, from which the deck and exhibits.csv are built
#'
#' @param block     the block of the summary statistics, e.g. "D. Experts and firms"
#' @param id        name of the exhibit without group, e.g. "experts"
#' @param group     "All experts" or a group of EXPERT_GROUPS
#' @param type      "table", "figure" or "map"
#' @param file      path of the file, relative to the summary statistics folder,
#'                  e.g. "tables/tab_experts_all.tex"
#' @param caption   the caption, plain text
#' @param placement "main" or "appendix" of the paper
#' @param note      figures and maps: the note below them, plain text
#' @return tibble, one row
exhibit_row <- function(block, id, group, type, file, caption, placement,
                        note = "") {
  tibble(block = block, id = id, group = group, type = type, file = file,
         caption = caption, placement = placement, note = note)
}

#' The experts or vignettes of one version of an exhibit: all, or one group
#'
#' @param data  data with the column expert_group
#' @param group "All experts" or a group of EXPERT_GROUPS
subset_group <- function(data, group) {
  if (group == ALL_EXPERTS) {
    data
  } else {
    data %>%
      filter(expert_group == group)
  }
}

#' The sections of the summary statistics skip what the data do not have: whether
#' all of these columns are in the data
has_columns <- function(data, columns) {
  all(columns %in% names(data))
}

# 3. Summary tables ------------------------------------------------------------

#' Summary table of one sample, or of several groups side by side: N, mean, SD and
#' quartiles of numeric variables, N and the share of each answer of categorical
#' ones. A wrapper around table_summary() (tables.R) that enforces the minimum
#' cell: a row with fewer than MIN_N answers shows "--" in every column.
#'
#' @param data  one row per expert or vignette
#' @param rows  one row per line of the table:
#'   panel     panel of the table (rows of a panel adjacent)
#'   variable  the column of `data`
#'   kind      "num": a number; "level": the share of answers equal to `level`;
#'             "dummy": a 0/1 column, shown as the share of ones
#'   level     for "level": the answer, exactly as in the data; else NA
#'   label     the row label, plain text with units
#'   digits    decimals
#' @param file  path of the .tex file
#' @param notes table notes, plain text (latex_text() is applied)
#' @param by    optional grouping column: one block of columns per value; rows with
#'              NA in it are left out
#' @param stats statistics to report, as in table_summary()
#' @return TRUE if the table was written; FALSE if none of its variables is in
#'   the data
summary_table <- function(data, rows, file, notes, by = NULL,
                          stats = c("n", "mean", "sd", "p25", "p50", "p75")) {
  rows <- rows %>%
    filter(variable %in% names(data))
  if (nrow(rows) == 0) {
    return(FALSE)
  }
  if (!is.null(by)) {
    data <- data[!is.na(data[[by]]), ]
    # levels in their order: split() in table_summary() sorts a factor by them
    data[[by]] <- droplevels(as.factor(data[[by]]))
  }

  # One numeric column per row of the table
  table_data <- tibble(.rows = nrow(data))
  if (!is.null(by)) {
    table_data[[by]] <- data[[by]]
  }
  row_names <- paste0("row_", seq_len(nrow(rows)))
  for (k in seq_len(nrow(rows))) {
    values <- data[[rows$variable[k]]]
    if (rows$kind[k] == "level") {
      # NA stays NA: the share is among those who answered
      values <- as.numeric(as.character(values) == rows$level[k])
    } else {
      values <- as.numeric(values)
    }
    table_data[[row_names[k]]] <- values
  }

  # The minimum cell, per block of columns
  group_values <- rep("all", nrow(table_data))
  if (!is.null(by)) {
    group_values <- as.character(table_data[[by]])
  }
  for (row_name in row_names) {
    for (group_value in unique(group_values)) {
      in_group <- group_values == group_value
      if (sum(!is.na(table_data[[row_name]][in_group])) < MIN_N) {
        table_data[[row_name]][in_group] <- NA
      }
    }
  }

  vars <- tibble(
    var = row_names,
    label = latex_text(rows$label),
    digits = rows$digits,
    kind = if_else(rows$kind == "num", "num", "pct")
  )
  if ("panel" %in% names(rows)) {
    vars$panel <- latex_text(rows$panel)
  }
  table_summary(table_data, vars, file, notes = latex_text(notes), by = by,
                stats = stats)
  TRUE
}

#' The standard end of a table's notes
cell_note <- function() {
  paste0("-- means fewer than ", MIN_N, " answers.")
}

# 4. Maps ----------------------------------------------------------------------

# The labels of the small states would cover their neighbours' labels: they move
# outside the map by these kilometres, with a line to the state. Brandenburg's
# label would cover Berlin: it moves within Brandenburg, without a line.
MAP_LABEL_NUDGES <- tribble(
  ~state,        ~dx_km, ~dy_km, ~with_line,
  "Berlin",      110,    70,     TRUE,
  "Hamburg",     -140,   60,     TRUE,
  "Bremen",      -150,   0,      TRUE,
  "Saarland",    -75,    -40,    TRUE,
  "Brandenburg", 80,     -60,    FALSE
)

#' Map of the federal states with one number per state, coloured and written on
#' the map. A state without a value (below the minimum cell) is left grey with "–".
#'
#' @param state_values tibble: state (as in bundesland), value (numeric), label
#'                     (the text on the map, e.g. "12.3%")
#' @param legend_title title of the colour legend, plain text
#' @param low,high     ends of the colour ramp
#' @param legend_labels how the legend shows the values, e.g. label_percent()
#' @return ggplot; save it with save_figure(plot, file, size = "map")
plot_state_map <- function(state_values, legend_title, low = MAP_LOW, high = MAP_HIGH,
                           legend_labels = label_comma()) {
  # GF 9: the land area of the states, one polygon each
  map_states <- st_read(BUNDESLAND_SHP, quiet = TRUE) %>%
    filter(GF == 9)
  unknown_states <- setdiff(state_values$state, map_states$GEN)
  if (length(unknown_states) > 0) {
    stop("States that are no polygon of the map: ",
         paste(unknown_states, collapse = ", "))
  }
  map_data <- map_states %>%
    left_join(state_values, by = c("GEN" = "state")) %>%
    mutate(label = if_else(is.na(value), "–", label))

  # Where the labels go: a point inside each state, in metres of the map's
  # projection (st_point_on_surface(), st_coordinates()), moved by MAP_LABEL_NUDGES
  state_points <- st_coordinates(st_point_on_surface(st_geometry(map_data)))
  map_labels <- tibble(state = map_data$GEN, label = map_data$label,
                       x = state_points[, "X"], y = state_points[, "Y"]) %>%
    left_join(MAP_LABEL_NUDGES, by = "state", relationship = "one-to-one") %>%
    mutate(is_nudged = !is.na(dx_km),
           label_x = if_else(is_nudged, x + 1000 * dx_km, x),
           label_y = if_else(is_nudged, y + 1000 * dy_km, y),
           # the line stops short of the label
           line_x = x + 0.75 * (label_x - x),
           line_y = y + 0.75 * (label_y - y))
  map_box <- st_bbox(map_data)
  map_xlim <- range(c(map_box[["xmin"]], map_box[["xmax"]], map_labels$label_x)) +
    c(-50000, 50000)
  map_ylim <- range(c(map_box[["ymin"]], map_box[["ymax"]], map_labels$label_y)) +
    c(-20000, 20000)

  ggplot() +
    geom_sf(data = map_data, aes(fill = value), colour = "white", linewidth = 0.3) +
    geom_segment(data = map_labels %>% filter(is_nudged, with_line),
                 aes(x = x, y = y, xend = line_x, yend = line_y),
                 colour = COLOUR_INK, linewidth = 0.25) +
    geom_label(data = map_labels, aes(x = label_x, y = label_y, label = label),
               size = (figure_font_size - 2) / .pt, colour = COLOUR_INK,
               fill = "white", alpha = 0.85, linewidth = 0,
               label.padding = unit(0.1, "lines")) +
    scale_fill_gradient(low = low, high = high, labels = legend_labels,
                        na.value = COLOUR_NOT_SHOWN, name = legend_title) +
    # datum = NA: no graticule. The x and y of the labels and the limits are in the
    # map's projection, as coord_sf() expects of layers that are not sf.
    coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE, datum = NA) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "right",
          legend.title = element_text(size = figure_font_size - 1),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
}

#' A number per federal state from the answers of the experts, with the minimum
#' cell applied, ready for plot_state_map()
#'
#' @param data    one row per expert, with bundesland
#' @param values  numeric vector, one per row of data; the mean per state is taken
#' @param digits  decimals of the label
#' @param percent TRUE: the values are shares, shown as percent
#' @return tibble: state, n, value, label
state_means <- function(data, values, digits = 1, percent = FALSE) {
  by_state <- tibble(state = as.character(data$bundesland), value = values) %>%
    filter(!is.na(state), !is.na(value)) %>%
    group_by(state) %>%
    summarise(n = n(), value = mean(value)) %>%
    ungroup() %>%
    mutate(value = if_else(n >= MIN_N, value, NA_real_))
  if (percent) {
    by_state$label <- paste0(formatC(100 * by_state$value, format = "f", digits = digits), "%")
  } else {
    by_state$label <- formatC(by_state$value, format = "f", digits = digits, big.mark = ",")
  }
  by_state
}
