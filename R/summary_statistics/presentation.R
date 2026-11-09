################################################################################
# presentation.R — the deck of the summary statistics: every table, figure and map
#                  of a sample on a frame of its own, rebuilt with every run
# Inputs : the list of exhibits, or exhibits.csv in output_dir; the files it names;
#          econslides.sty next to this code
# Outputs: in output_dir: summary_statistics.tex, .pdf and .log, econslides.sty
# Checks : an exhibit whose file does not exist is listed under "Not shown"
################################################################################

# The blocks of the summary statistics, for the section "Not shown". A block counts
# as shown by its letter, so that a section can change the rest of its name.
DECK_BLOCKS <- tribble(
  ~letter, ~block,                     ~reason,
  "0",     "0. Data cleaning",         "Its tables need the cleaning report, and belong to the full sample.",
  "A",     "A. Recruitment",           "Response rates need the sampling frame, which belongs to the full sample.",
  "B",     "B. Survey metadata",       "The sample does not have the meta data of the survey.",
  "C",     "C. Selection",             paste0("The comparison needs at least ", MIN_N, " completers and ", MIN_N, " break-offs."),
  "D",     "D. Experts and firms",     "None of its variables is in the sample.",
  "E",     "E. Market",                "None of its variables is in the sample.",
  "F",     "F. Prices and beliefs",    "None of its variables is in the sample.",
  "G",     "G. Barriers and criteria", "None of its variables is in the sample.",
  "H",     "H. Preferences",           "None of its variables is in the sample.",
  "I",     "I. Heating context",       "None of its variables is in the sample.",
  "J",     "J. Vignettes",             "None of its variables is in the sample."
)
DECK_FILE <- "summary_statistics"

#' The deck of the summary statistics of one sample, written and compiled in its
#' folder. It always shows the current files: it is rebuilt on every run, and it
#' can run alone on an existing folder, as it needs only exhibits.csv.
#'
#' If pdflatex is missing or fails, this gives a message with the path of the log
#' and does not stop: the .tex stays, and the exhibits are not lost.
#'
#' @param exhibits     the list of exhibits, from the sections; NULL: read
#'                     exhibits.csv in output_dir
#' @param output_dir   the summary statistics folder, with tables/, figures/, maps/
#' @param sample_label what the sample is, plain text, for the first slide
#' @param experts      one row per expert of the sample; NULL: no count on the
#'                     first slide
#' @param vignettes    one row per expert x vignette of the sample; NULL: as above
#' @return the path of the .pdf if it was written, otherwise NA, invisibly
build_presentation <- function(exhibits = NULL, output_dir, sample_label,
                               experts = NULL, vignettes = NULL) {
  # 1. The exhibits in the order of the deck -----------------------------------
  if (is.null(exhibits)) {
    # Everything as text, so that a column without any entry stays a column of text
    exhibits <- read_csv(file.path(output_dir, "exhibits.csv"),
                         show_col_types = FALSE,
                         col_types = cols(.default = col_character()))
  }
  if (!has_columns(exhibits, "block")) {
    # No section wrote anything: no exhibit, but the columns the code below expects
    exhibits <- exhibit_row("", "", "", "", "", "", "")[0, ]
  }
  # A section per block in the order of the exhibits; in a block all experts first,
  # then the groups, each in the order of their exhibits. match(): the position in
  # the vector, so that arrange() follows it
  # A column of text read from a file has NA where a section wrote no note
  exhibits <- exhibits %>%
    mutate(note = coalesce(note, ""),
           block_order = match(block, unique(block)),
           group_order = match(group, c(ALL_EXPERTS, EXPERT_GROUPS)),
           row_order = row_number(),
           has_file = file.exists(file.path(output_dir, file))) %>%
    arrange(block_order, group_order, row_order)
  files_missing <- exhibits$file[!exhibits$has_file]
  exhibits <- exhibits %>%
    filter(has_file)

  # 2. The first slide and the overview ----------------------------------------
  # tryCatch(): an empty answer instead of a stop if git is missing or this is no
  # repository; the question is only which run the deck shows
  commit <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(error_message) character(0)
  )
  if (length(commit) == 0) {
    commit <- "unknown"
  }
  sample_size <- "Number of experts and vignettes not given"
  if (!is.null(experts) && !is.null(vignettes)) {
    sample_size <- paste0(fmt_int(nrow(experts)), " experts, ",
                          fmt_int(nrow(vignettes)), " vignettes, of which ",
                          fmt_int(sum(!is.na(vignettes$vig_rec))), " answered")
  }
  blocks <- unique(exhibits$block)
  lines <- c(
    "% Written by build_presentation() in R/summary_statistics/presentation.R.",
    "% Do not edit: a new run overwrites it.",
    "\\documentclass[10pt, aspectratio=169]{beamer}",
    "\\usepackage{econslides}",
    "\\usepackage[export]{adjustbox}",
    "% Long captions: one line where possible, so that the exhibit keeps the height",
    "\\setbeamerfont{frametitle}{size=\\small}",
    "\\title{Summary statistics}",
    paste0("\\subtitle{", latex_text(sample_label), "}"),
    paste0("\\date{", format(Sys.Date(), "%Y-%m-%d"), "}"),
    paste0("\\author{", sample_size, "}"),
    paste0("\\institute{Commit ", latex_text(commit), "}"),
    "\\begin{document}",
    "\\maketitle",
    "\\begin{frame}{Overview}",
    "\\footnotesize",
    "\\begin{itemize}"
  )
  for (k in seq_along(blocks)) {
    n_exhibits <- sum(exhibits$block == blocks[k])
    lines <- c(lines, paste0(
      "\\item \\hyperlink{block", k, "}{\\textcolor{oiblue}{",
      latex_text(blocks[k]), "}} (", n_exhibits,
      if_else(n_exhibits == 1, " exhibit)", " exhibits)")
    ))
  }
  lines <- c(lines, "\\item \\hyperlink{notshown}{\\textcolor{oiblue}{Not shown}}",
             "\\end{itemize}", "\\end{frame}")

  # 3. One frame per exhibit ---------------------------------------------------
  for (k in seq_along(blocks)) {
    lines <- c(lines, paste0("\\section{", latex_text(blocks[k]), "}"))
    block_exhibits <- exhibits %>%
      filter(block == blocks[k])
    for (i in seq_len(nrow(block_exhibits))) {
      exhibit <- block_exhibits[i, ]
      # The first frame of a block is where the overview links to
      frame_label <- if_else(i == 1, paste0("[label=block", k, "]"), "")
      frame_title <- paste0(latex_text(exhibit$caption), " -- ",
                            latex_text(exhibit$group), " (", exhibit$placement, ")")
      lines <- c(lines,
                 paste0("\\begin{frame}", frame_label, "{", frame_title, "}"))
      # A second line of the title (of 105 characters) takes about 6% of the height
      # of the frame
      title_height <- 0.06 * (ceiling(nchar(frame_title) / 105) - 1)
      if (exhibit$type == "table") {
        # In this deck only: a table shrinks to the frame, however long it is, so
        # that one can check it; the paper takes it at its natural size
        max_height <- 0.82 - title_height
        lines <- c(
          lines,
          paste0("\\begin{adjustbox}{max width=\\textwidth, max totalheight=",
                 sprintf("%.2f", max_height), "\\textheight, center}"),
          "\\footnotesize",
          paste0("\\input{", exhibit$file, "}"),
          "\\end{adjustbox}"
        )
      } else {
        # Natural size whenever it fits: only what is too tall for the frame shrinks.
        # A line of the note (of 120 characters) takes about 4.1% of the height.
        max_height <- 0.88 - title_height - 0.041 * ceiling(nchar(exhibit$note) / 120)
        lines <- c(
          lines,
          "\\centering",
          paste0("\\includegraphics[max width=\\textwidth, max totalheight=",
                 sprintf("%.3f", max_height), "\\textheight]{", exhibit$file, "}")
        )
        if (exhibit$note != "") {
          lines <- c(lines, paste0("\\slidenote{", latex_text(exhibit$note), "}"))
        }
      }
      lines <- c(lines, "\\end{frame}")
    }
  }

  # 4. What the sample does not have -------------------------------------------
  blocks_missing <- DECK_BLOCKS %>%
    filter(!(letter %in% str_extract(blocks, "^[^.]+")))
  lines <- c(lines, "\\section{Not shown}",
             "\\begin{frame}[label=notshown]{Not shown}", "\\footnotesize")
  if (nrow(blocks_missing) == 0 && length(files_missing) == 0) {
    lines <- c(lines,
               "Every block of the summary statistics wrote exhibits for this sample.")
  } else {
    lines <- c(lines, "\\begin{itemize}")
    for (k in seq_len(nrow(blocks_missing))) {
      lines <- c(lines, paste0("\\item \\textbf{",
                               latex_text(blocks_missing$block[k]), "}: ",
                               latex_text(blocks_missing$reason[k])))
    }
    for (file_missing in files_missing) {
      lines <- c(lines, paste0("\\item In the list of exhibits, but the file does ",
                               "not exist: ", latex_text(file_missing)))
    }
    lines <- c(lines, "\\end{itemize}")
  }
  lines <- c(lines, "\\end{frame}", "\\end{document}")

  # 5. Write and compile -------------------------------------------------------
  tex_file <- file.path(output_dir, paste0(DECK_FILE, ".tex"))
  # enc2utf8() and useBytes: the text goes out as it is, in UTF-8 as pdflatex reads
  # it, whatever the locale of the session
  writeLines(enc2utf8(lines), tex_file, useBytes = TRUE)
  file.copy(file.path("R", "summary_statistics", "econslides.sty"), output_dir,
            overwrite = TRUE)

  # pdflatex runs in the folder of the deck, with relative paths, so that its path
  # may have spaces in it, and the deck does not depend on where the folder is.
  # on.exit(): back to the old folder when the function ends, also after an error
  old_directory <- setwd(output_dir)
  on.exit(setwd(old_directory))
  log_path <- file.path(output_dir, paste0(DECK_FILE, ".log"))
  auxiliary_files <- paste0(DECK_FILE, c(".aux", ".nav", ".out", ".snm", ".toc",
                                         ".vrb"))
  # Nothing from an earlier run: no old deck that looks current after a failure,
  # and no auxiliary files that could carry an error over. unlink(): deletes files
  unlink(c(paste0(DECK_FILE, c(".pdf", ".log")), auxiliary_files))
  # Sys.which(): the path of a program, empty if it is not on the PATH
  if (Sys.which("pdflatex") == "") {
    message("The deck was not compiled, pdflatex is missing. Its source: ", tex_file)
    return(invisible(NA_character_))
  }
  # system2(): runs a program and gives its exit status. Two passes, for the links
  # and the numbers of the frames
  for (pass in 1:2) {
    status <- system2("pdflatex",
                      c("-interaction=nonstopmode", paste0(DECK_FILE, ".tex")),
                      stdout = FALSE, stderr = FALSE)
  }
  unlink(auxiliary_files)
  if (status != 0 || !file.exists(paste0(DECK_FILE, ".pdf"))) {
    message("The deck did not compile without errors. See the log: ", log_path,
            "\nIts source: ", tex_file)
    return(invisible(NA_character_))
  }
  pdf_path <- file.path(output_dir, paste0(DECK_FILE, ".pdf"))
  message("Deck: ", pdf_path, " (", nrow(exhibits), " exhibits)")
  invisible(pdf_path)
}
