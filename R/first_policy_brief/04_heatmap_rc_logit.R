# =============================================================================
# 04_heatmap_rc_logit.R — heat maps of the standard deviations and
#                         correlations of the random constants in the mixed
#                         logit with correlated constants
# Inputs : excel/rc_logit_ergebnisse.xlsx of each run of
#          03_rc_logit_with_ame.R (FIRST_POLICY_BRIEF_DIR/rc_logit_<arm>_ref_<outcome>/)
# Outputs: abbildungen/abb_korrelation_rc_corr.svg in the folder of each run
# Checks : one standard deviation per technology and one correlation per pair;
#          standard deviations positive, correlations between -1 and 1
# =============================================================================
# The figure shows the matrix that mlogit's vcov(model, what = "rpar",
# type = "cor") returns: the standard deviations of the constants across
# experts on the diagonal, their correlations off the diagonal. The numbers are
# read from the workbook that 03_rc_logit_with_ame.R writes (section 7.1 there),
# so they are the same as in panels B and C of tab_rcl_mixed_corr.tex. The
# saved models are not loaded: they contain the respondent-level data.

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source(file.path("R", "prepare_analysis_data.R"))   # config.R: FIRST_POLICY_BRIEF_DIR, packages

# 1. Settings ------------------------------------------------------------------

# The runs of 03_run_all_specifications.R: each arm with each reference
# outcome. A run without results is skipped and reported at the end.
SPECIFICATIONS <- tribble(
  ~arm,   ~reference,
  "fw",   "Keine Empfehlung",
  "fw",   "Wärmepumpe",
  "nofw", "Keine Empfehlung",
  "nofw", "Wärmepumpe"
)

# Folder names of the reference outcomes, as OUTCOME_FILE_NAMES in
# 03_rc_logit_with_ame.R
REFERENCE_FILE_NAMES <- c(
  "Keine Empfehlung" = "keine_empfehlung",
  "Wärmepumpe" = "waermepumpe"
)

# The model in the column Modell of the workbook
MODEL_LABEL <- "Mixed Logit mit korrelierten Konstanten"

# The figure is in English, unlike the other outputs of the logit models

# Rows and columns of the heat map, in this order, with these labels. The
# names are the outcomes as in the data and in the parameter names.
TECHNOLOGY_LABELS <- c(
  "Wärmepumpe" = "Heat pump",
  "Gasheizung" = "Gas",
  "Pelletheizung" = "Pellet",
  "Hybridheizung" = "Hybrid",
  "Ölheizung" = "Oil",
  "Fernwärme" = "District heating",
  "Keine Empfehlung" = "No recommendation"
)

# Colours: correlations from red (-1) over light grey (0) to blue (1). The
# blue of the first draft of this figure, and the red of the same (Tableau)
# palette. The diagonal (standard deviations) is grey, outside this scale.
COLOR_NEGATIVE <- "#E15759"
COLOR_ZERO <- "#F2F2F2"
COLOR_POSITIVE <- "#4E79A7"
COLOR_DIAGONAL <- "grey85"
COLOR_TEXT <- "#2F3B4A"

# White text on dark tiles. A tile is dark by the rule of is_dark() in
# R/summary_statistics/01_plot_functions.R (HCL luminance below 55) from a
# correlation of about 0.88 on; the red end of the scale is never that dark.
WHITE_TEXT_ABOVE <- 0.88

FIGURE_FILE <- "abb_korrelation_rc_corr.pdf"

# Every run folder of 03_rc_logit_with_ame.R must be in SPECIFICATIONS, so that
# a run added to 03_run_all_specifications.R is not left out without notice
expected_folders <- paste0("rc_logit_", SPECIFICATIONS$arm, "_ref_",
                           REFERENCE_FILE_NAMES[SPECIFICATIONS$reference])
run_folders <- list.files(FIRST_POLICY_BRIEF_DIR, pattern = "^rc_logit_")
if (any(!run_folders %in% expected_folders)) {
  stop("Run folders not in SPECIFICATIONS: ",
       paste(setdiff(run_folders, expected_folders), collapse = ", "))
}

# 2. One heat map per run ------------------------------------------------------
run_status <- SPECIFICATIONS
run_status$status <- NA_character_

for (s in seq_len(nrow(SPECIFICATIONS))) {
  arm <- SPECIFICATIONS$arm[s]
  reference <- SPECIFICATIONS$reference[s]
  run_dir <- file.path(FIRST_POLICY_BRIEF_DIR, expected_folders[s])
  results_file <- file.path(run_dir, "excel", "rc_logit_ergebnisse.xlsx")
  cat("\n==== Arm:", arm, "| reference outcome:", reference, "====\n")

  if (!file.exists(results_file)) {
    run_status$status[s] <- "skipped: no results, run 03_rc_logit_with_ame.R first"
    next
  }

  # (a) Estimates of the correlated model. Parameter names, written by
  #     section 7.1 of 03_rc_logit_with_ame.R:
  #       sd.(Intercept):<technology>                          standard deviation
  #       cor.(Intercept):<technology>:(Intercept):<technology>  correlation, each pair once
  estimates <- read_xlsx(results_file, sheet = "Koeffizienten") %>%
    filter(Modell == MODEL_LABEL)
  if (nrow(estimates) == 0) stop("No rows for '", MODEL_LABEL, "' in ", results_file)

  # Look-up table: parameter name -> estimate (setNames() names the values)
  estimate_of <- setNames(estimates$`Schätzwert`, estimates$Parameter)

  # (b) The technologies: one random constant each, i.e. every outcome but the
  #     reference outcome. Put in the order of TECHNOLOGY_LABELS.
  #     startsWith(x, "sd."): TRUE where x begins with "sd."; sub() with
  #     fixed = TRUE cuts the prefix, read as plain text, not as a pattern
  sd_parameters <- estimates$Parameter[startsWith(estimates$Parameter, "sd.")]
  technologies_found <- sub("sd.(Intercept):", "", sd_parameters, fixed = TRUE)
  if (any(!technologies_found %in% names(TECHNOLOGY_LABELS))) {
    stop("Technology without a label: ",
         paste(setdiff(technologies_found, names(TECHNOLOGY_LABELS)), collapse = ", "))
  }
  stopifnot(!reference %in% technologies_found)
  is_found <- names(TECHNOLOGY_LABELS) %in% technologies_found
  technologies <- names(TECHNOLOGY_LABELS)[is_found]
  K <- length(technologies)

  # Every pair of technologies has exactly one correlation
  n_correlations <- sum(startsWith(estimates$Parameter, "cor."))
  stopifnot(n_correlations == K * (K - 1) / 2)

  # (c) Matrix: standard deviations on the diagonal, correlations off it. Each
  #     correlation is stored once, with the two constants in mlogit's order,
  #     so both orders of the pair are tried.
  sd_cor_matrix <- matrix(NA_real_, nrow = K, ncol = K,
                          dimnames = list(technologies, technologies))
  for (i in 1:K) {
    for (j in 1:K) {
      if (i == j) {
        parameter <- paste0("sd.(Intercept):", technologies[i])
      } else {
        name_ij <- paste0("cor.(Intercept):", technologies[i], ":(Intercept):", technologies[j])
        name_ji <- paste0("cor.(Intercept):", technologies[j], ":(Intercept):", technologies[i])
        parameter <- if (name_ij %in% names(estimate_of)) name_ij else name_ji
      }
      if (!parameter %in% names(estimate_of)) stop("Parameter missing: ", parameter)
      sd_cor_matrix[i, j] <- estimate_of[[parameter]]
    }
  }

  # anyNA(): TRUE if any cell is NA
  is_diagonal_cell <- diag(K) == 1
  stopifnot(
    !anyNA(sd_cor_matrix),
    all(diag(sd_cor_matrix) > 0),
    all(abs(sd_cor_matrix[!is_diagonal_cell]) <= 1)
  )
  print(round(sd_cor_matrix, 2))

  # (d) Long format, one row per cell. Off the diagonal, `correlation` sets the
  #     colour; on it, correlation is NA, which the fill scale draws in
  #     COLOR_DIAGONAL. Factor levels: the first technology in the top row and
  #     the left column. formatC() writes the numbers with two decimals.
  technology_labels <- unname(TECHNOLOGY_LABELS[technologies])
  heatmap_data <- as_tibble(sd_cor_matrix, rownames = "row") %>%
    pivot_longer(-row, names_to = "column", values_to = "value") %>%
    mutate(
      is_diagonal = row == column,
      correlation = if_else(is_diagonal, NA_real_, value),
      label = formatC(value, format = "f", digits = 2),
      text_white = !is_diagonal & value > WHITE_TEXT_ABOVE,
      fontface = if_else(is_diagonal, "bold", "plain"),
      row = factor(unname(TECHNOLOGY_LABELS[row]), levels = rev(technology_labels)),
      column = factor(unname(TECHNOLOGY_LABELS[column]), levels = technology_labels)
    )

  # (e) Figure
  sample_note <- if (arm == "fw") {
    "Sample: experts who could recommend district heating."
  } else {
    "Sample: experts who could not recommend district heating."
  }
  # Each constant is the preference for a technology relative to the reference
  # outcome, so the correlations are too: the subtitle says so
  subtitle <- paste0(
    "Mixed logit with correlated constants. Reference category: ",
    TECHNOLOGY_LABELS[[reference]], ";\n",
    "each constant measures the preference for a technology relative to this category.\n",
    "Off the diagonal: correlation of the constants across experts\n",
    "(blue = liked together, red = traded off).\n",
    "Diagonal (bold): standard deviation of the constant across experts."
  )

  plot <- ggplot(heatmap_data, aes(x = column, y = row, fill = correlation)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = label, colour = text_white, fontface = fontface), size = 3.8) +
    scale_fill_gradient2(low = COLOR_NEGATIVE, mid = COLOR_ZERO, high = COLOR_POSITIVE,
                         midpoint = 0, limits = c(-1, 1), na.value = COLOR_DIAGONAL) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = COLOR_TEXT), guide = "none") +
    scale_x_discrete(position = "top") +
    coord_fixed() +
    labs(title = "Correlation of preferences for heating technologies",
         subtitle = subtitle,
         caption = sample_note,
         x = NULL,
         y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
          axis.text = element_text(colour = COLOR_TEXT),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 9),
          plot.caption = element_text(size = 9, hjust = 0),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.position = "none")

  figure_dir <- file.path(run_dir, "abbildungen")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(figure_dir, FIGURE_FILE), plot, width = 8, height = 8)
  run_status$status[s] <- paste("written:", file.path(basename(run_dir), "abbildungen", FIGURE_FILE))
}

# 3. Summary -------------------------------------------------------------------
print(run_status)
