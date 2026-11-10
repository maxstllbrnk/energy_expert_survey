################################################################################
# run_summary_statistics.R — all tables, figures and maps of one sample, and the
#                            deck that shows them
# Inputs : the experts and vignettes of the sample; the sampling frame, if given
# Outputs: in output_dir: tables/tab_*.tex, figures/fig_*.pdf, maps/map_*.pdf,
#          exhibits.csv (the list of exhibits), summary_statistics.pdf and .tex
# Checks : in the sections
################################################################################

#' All summary statistics of one sample. Output of an earlier run is removed first,
#' so that a file which is no longer produced cannot sit among the current ones. If
#' neither the sample nor this code has changed, it loads the list of exhibits and
#' writes nothing, like the models: an estimation script can run again at no cost.
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
#' @param sample_label what the sample is, plain text, for the title of the deck and
#'                     the notes of the tables
#' @param frame        the sampling frame (load_frame()), or NULL if the sample is
#'                     not the full sample
#' @param force        TRUE: write everything again
#' @return the list of exhibits, invisibly
run_summary_statistics <- function(experts, vignettes, output_dir, sample_label,
                                   frame = NULL, force = FALSE) {
  # What the output depends on: the sample, the code and settings that write it, and
  # the files it reads; with the frame also the cleaning report, for the appendix
  # tables. rlang::hash(): a fingerprint of any object. tools::md5sum(): one of a file.
  input_files <- c(
    list.files(file.path("R", "summary_statistics"), full.names = TRUE),
    file.path("R", "functions", c("exhibits.R", "figures.R", "frame.R", "labels.R",
                                  "questionnaire.R", "tables.R")),
    file.path("R", "setup.R"), "config.R",
    VERSAND_FILE, HDD_FILE, BUNDESLAND_SHP, KREIS_SHP
  )
  if (!is.null(frame)) {
    input_files <- c(input_files, file.path(REPORT_DIR, "cleaning_report.rds"))
  }
  fingerprint <- rlang::hash(list(experts, vignettes, sample_label, frame,
                                  unname(tools::md5sum(input_files))))
  fingerprint_file <- file.path(output_dir, "fingerprint.rds")
  is_current <- file.exists(fingerprint_file) &&
    file.exists(file.path(output_dir, "exhibits.csv")) &&
    file.exists(file.path(output_dir, "summary_statistics.pdf")) &&
    identical(readRDS(fingerprint_file), fingerprint)
  if (!force && is_current) {
    message("Summary statistics of \"", sample_label, "\" loaded, nothing changed: ",
            output_dir)
    return(invisible(read_csv(file.path(output_dir, "exhibits.csv"),
                              show_col_types = FALSE,
                              col_types = cols(.default = col_character()))))
  }

  # No fingerprint until the run has ended: a run that stops leaves no folder that
  # counts as current
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(fingerprint_file)
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
  # does not know the label of the sample. Not in the blocks of the cleaning and the
  # recruitment: they start before the sample, from the questionnaires or the invited,
  # and their notes say so. str_detect(): whether the pattern occurs.
  table_exhibits <- exhibits %>%
    filter(type == "table", !str_detect(block, "^(0|A)\\."))
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

  # A deck that failed is not current: the next run tries again
  deck <- build_presentation(exhibits, output_dir, sample_label, experts, vignettes)
  if (!is.na(deck)) {
    saveRDS(fingerprint, fingerprint_file)
  }
  invisible(exhibits)
}
