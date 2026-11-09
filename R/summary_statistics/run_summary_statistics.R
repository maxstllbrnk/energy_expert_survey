################################################################################
# run_summary_statistics.R — all tables, figures and maps of one sample, and the
#                            deck that shows them
# Inputs : the experts and vignettes of the sample; the sampling frame, if given
# Outputs: in output_dir: tables/tab_*.tex, figures/fig_*.pdf, maps/map_*.pdf,
#          exhibits.csv (the list of exhibits), summary_statistics.pdf and .tex
# Checks : in the sections
################################################################################

#' All summary statistics of one sample. Output of an earlier run is removed first,
#' so that a file which is no longer produced cannot sit among the current ones.
#'
#' A section that does not apply to the sample writes nothing: response rates need
#' the frame, the comparison of completers and break-offs needs both. An exhibit
#' whose variables were dropped from the sample is skipped too. The deck lists what
#' is not shown.
#'
#' @param experts      one row per expert of the sample, as in experts.rds
#' @param vignettes    one row per expert x vignette of the sample, as in
#'                     vignettes.rds
#' @param output_dir   the summary statistics folder, e.g.
#'                     file.path(OUTPUT_DIR, "district_heating", "summary_statistics")
#' @param sample_label what the sample is, plain text, for the title of the deck
#' @param frame        the sampling frame (load_frame()), or NULL if the sample is
#'                     not the full sample
#' @return the list of exhibits, invisibly
run_summary_statistics <- function(experts, vignettes, output_dir, sample_label,
                                   frame = NULL) {
  dirs <- list(
    tables = file.path(output_dir, "tables"),
    figures = file.path(output_dir, "figures"),
    maps = file.path(output_dir, "maps")
  )
  for (folder in dirs) {
    # unlink(): deletes the folder and everything in it
    unlink(folder, recursive = TRUE)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }

  cleaning_exhibits <- tibble()
  if (!is.null(frame)) {
    cleaning_exhibits <- section_cleaning(dirs)
  }
  exhibits <- bind_rows(
    cleaning_exhibits,
    section_recruitment(experts, vignettes, frame, dirs),
    section_completion(experts, vignettes, dirs),
    section_selection(experts, vignettes, dirs),
    section_experts_firms(experts, dirs),
    section_market(experts, dirs),
    section_prices_beliefs(experts, dirs),
    section_barriers_criteria(experts, dirs),
    section_preferences(experts, dirs),
    section_heating_context(experts, dirs),
    section_vignettes(experts, vignettes, dirs)
  )
  write_csv(exhibits, file.path(output_dir, "exhibits.csv"))

  # Every table note starts with its sample (tables.md). In one place, as a section
  # does not know the label of the sample.
  table_exhibits <- exhibits %>%
    filter(type == "table")
  for (k in seq_len(nrow(table_exhibits))) {
    table_file <- file.path(output_dir, table_exhibits$file[k])
    sample_note <- paste0("\\item \\textit{Notes:} Sample: ",
                          latex_text(sample_label), " (",
                          latex_text(table_exhibits$group[k]), ").")
    # readLines(), writeLines(): a text file as a vector of lines, and back
    lines <- readLines(table_file, encoding = "UTF-8")
    # sub(fixed = TRUE): replaces the text as it stands, backslashes included
    lines <- sub("\\item \\textit{Notes:}", sample_note, lines, fixed = TRUE)
    writeLines(lines, table_file)
  }

  build_presentation(exhibits, output_dir, sample_label, experts, vignettes)
  invisible(exhibits)
}
