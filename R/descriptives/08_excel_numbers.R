################################################################################
# 08_excel_numbers.R — all numbers unrounded in Excel, and the numbers and
#                      figure notes of the slides as LaTeX macros
# Inputs : excel_sheets and numbers, filled by 03_recruitment.R to
#          07_response_times.R
# Outputs: excel/descriptives.xlsx; tables/numbers_desc.tex
# Checks : every macro name is letters only; no sheet name is too long
################################################################################

# 1. Excel ---------------------------------------------------------------------
note_sample <- paste0(
  "Completed questionnaires of energy advisers, HVAC installers and chimney ",
  "sweeps (Berufsgruppe from Q2, add_berufsgruppe() in ",
  "R/prepare_analysis_data.R), both arms. The response rates count everyone ",
  "invited and everyone who completed (R/ruecklauf/)."
)
note_prices <- paste0(
  "Cent per kWh of the energy carrier, as Q15 asked (district heating per ",
  "kWh of heat; the questionnaire: 1 litre of heating oil is about 10 kWh, ",
  "1 kg of pellets about 5 kWh). ",
  "By the rules of the mixed logit models, clean_reported_prices() in ",
  "R/prepare_analysis_data.R: euro below ", PRICE_EURO_BELOW, ", oil per ",
  "litre ", OIL_PER_LITRE[1], "-", OIL_PER_LITRE[2], ", pellets per kg ",
  PELLETS_PER_KG[1], "-", PELLETS_PER_KG[2], ", not usable more than ",
  PRICE_VALID_FACTOR, " times above or below the median."
)
note_other <- paste0(
  "Impossible values (VALID_RANGE) and the ", 100 * TRIM_QUANTILES[1],
  "% lowest and highest of the remaining answers of all experts removed ",
  "(R/summary_statistics/00_settings.R): ",
  paste(unname(OUTLIER_VARIABLES), collapse = ", "), "."
)
note_shares <- paste0(
  "Shares of the technologies among the recommendations (Q10b) and the ",
  "installations (Q11b), the variables ending in _filled: an expert gave ",
  "shares if they sum to more than 0; a technology left blank then counts as ",
  "0. Used as reported, also where they do not sum to 100, and not trimmed. ",
  "Means over the firms, not shares of all recommendations or installations."
)
note_cells <- paste0(
  "No statistic of fewer than ", MIN_N, " answers (NA); a state on the map ",
  "needs ", MIN_KONTAKTE, " invited and ", MIN_N, " completed."
)
notes <- tibble(
  topic = c("Sample", "Energy prices", "Other answers", "Technology shares",
            "Small cells", "Created"),
  note = c(note_sample, note_prices, note_other, note_shares, note_cells,
           paste0(format(Sys.time(), "%Y-%m-%d %H:%M"),
                  " with R/run_descriptives.R"))
)
# Excel allows sheet names of up to 31 characters
too_long <- names(excel_sheets)[nchar(names(excel_sheets)) > 31]
if (length(too_long) > 0) {
  stop("Sheet names longer than 31 characters: ",
       paste(too_long, collapse = ", "))
}
write_xlsx(c(list(notes = notes), excel_sheets),
           file.path(excel_dir, "descriptives.xlsx"))

# 2. Numbers and notes of the slides -------------------------------------------
# \input{numbers_desc.tex} once in the slides, then e.g. \descExperts in the
# text and \slidenote{\descNoteResponseMap} below a figure. LaTeX macro names
# have letters only.
numbers[["descMinCell"]] <- latex_number(MIN_N, 0)
bad_names <- names(numbers)[!grepl("^[A-Za-z]+$", names(numbers))]
if (length(bad_names) > 0) {
  stop("Macro names with other characters than letters: ",
       paste(bad_names, collapse = ", "))
}
write_tex(c("% Numbers and figure notes of the descriptives, written by",
            "% R/run_descriptives.R. Do not edit: rerun the script.",
            numbers_tex(numbers)),
          file.path(tables_dir, "numbers_desc.tex"))
